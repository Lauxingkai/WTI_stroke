# ============================================================
# 04_analysis.R — 主分析 + 剂量反应 + 阈值 + 亚组 + 敏感性 + 子分析（v2 适配版）
# 输入: data/analysis/master_cc.RDS（主分析完整案例）
# 输出: results/ 下各模块 CSV + master_results.RDS
# 结构（与方案一致）:
#   主分析 = 总摄入, E-K 周期(2007-2020, 7周期), wt7 = wt_2yr/7
#   子分析 = 仅膳食, C-K 周期(2003-2020, 9周期), wt9 = wt_2yr/9 —— 复现手稿(关卡③)
#   扩展   = L 周期(2021-2022) + 全 10 周期敏感性
# 协变量: m3 不含 alcohol/supp_user（K 无 ALQ、C/D 无补充剂表——二者进敏感性）
# ============================================================

suppressPackageStartupMessages({
  library(survey)
  library(dplyr)
  library(rms)
  library(broom)
  library(EValue)
})

options(survey.lonely.psu = "adjust")
set.seed(2026)

res_dir <- "results"
dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)

mc <- readRDS("data/analysis/master_cc.RDS")

# ---------- 队列定义 ----------
m_total <- mc %>% filter(vitk_total_ok & cycle_sfx != "L") %>%   # E-K 总摄入
  mutate(wt = wt_2yr / 7)
m_diet  <- mc %>% filter(cycle_sfx != "L") %>%                    # C-K 仅膳食
  mutate(wt = wt_2yr / 9)
m_diet_Kexcl <- mc %>% filter(!cycle_sfx %in% c("K","L")) %>%     # C-J（敏感性：排除K）
  mutate(wt = wt_2yr / 8)
m_all10 <- mc %>% mutate(wt = wt_2yr / 10)                        # 全 10 周期

# ---------- 协变量 ----------
covars_m2 <- c("age","sex","race","education","PIR","marital")
covars_m3 <- c(covars_m2, "BMI","smoking","hypertension","diabetes",
               "CHD","TC","HDL","kcal","satfat","chol_int")
m2_form <- paste(covars_m2, collapse = " + ")
m3_form <- paste(covars_m3, collapse = " + ")

# ---------- 工具 ----------
mk_design <- function(dat) {
  svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt,
            data = dat, nest = TRUE)
}
or_out <- function(fit, term, digits = 3) {
  b <- coef(fit); s <- sqrt(diag(vcov(fit)))
  data.frame(term = term,
             OR = round(exp(b[term]), digits),
             LCL = round(exp(b[term] - 1.96 * s[term]), digits),
             UCL = round(exp(b[term] + 1.96 * s[term]), digits),
             p = signif(summary(fit)$coefficients[term, 4], 3))
}
fit_models <- function(dsn, exp_name, exp_label) {
  f1 <- as.formula(paste0("stroke ~ ", exp_name))
  f2 <- as.formula(paste0("stroke ~ ", exp_name, " + ", m2_form))
  f3 <- as.formula(paste0("stroke ~ ", exp_name, " + ", m3_form))
  out <- list()
  for (nm in c("M1","M2","M3")) {
    fit <- svyglm(get(paste0("f", substring(nm, 2))), design = dsn, family = quasibinomial)
    out[[nm]] <- or_out(fit, exp_name); out[[nm]]$model <- nm; out[[nm]]$exposure <- exp_label
  }
  bind_rows(out)
}
quant_cut <- function(x, k, dsn, var) {
  q <- svyquantile(as.formula(paste0("~", var)), dsn,
                   quantiles = seq(0, 1, length.out = k + 1)[-c(1, k + 1)])
  qv <- as.vector(q[[1]][, "quantile"])
  cut(x, breaks = c(-Inf, unique(qv), Inf), labels = FALSE)
}
run_quant <- function(dat, k, exp_var, label, weighted = TRUE) {
  dsn <- mk_design(dat)
  qk <- quant_cut(dat[[exp_var]], k, dsn, exp_var)
  dat$qk <- qk
  med <- dat %>% group_by(qk) %>% summarise(m = median(.data[[exp_var]], na.rm = TRUE))
  dat$qmed <- med$m[qk]
  dat$qk <- factor(qk)
  dsn <- mk_design(dat)
  rows <- list()
  for (nm in c("M1","M2","M3")) {
    f <- if (nm == "M1") as.formula("stroke ~ qk")
         else as.formula(paste0("stroke ~ qk + ", get(paste0("m", substring(nm, 2), "_form"))))
    fit <- svyglm(f, design = dsn, family = quasibinomial)
    tb <- tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>% filter(grepl("^qk", term))
    tb$model <- nm; tb$exposure <- label
    rows[[nm]] <- tb
  }
  tbl <- bind_rows(rows)
  ft <- svyglm(as.formula(paste0("stroke ~ qmed + ", m3_form)), design = dsn,
               family = quasibinomial)
  attr(tbl, "trend_p") <- signif(summary(ft)$coefficients["qmed", 4], 3)
  attr(tbl, "cuts") <- sort(unique(qk))
  tbl
}

# ============================================================
# 1. 主分析：总摄入（E-K）三模型 × 连续每SD / 四分位 / 五分位
# ============================================================
cat("== 主分析队列（总摄入 E-K）: n =", nrow(m_total),
    " 卒中", sum(m_total$stroke == 1), "==\n")
dsn_t <- mk_design(m_total)
sd_vk <- sd(m_total$vitk_total, na.rm = TRUE)
m_total$vitk_sd <- m_total$vitk_total / sd_vk
dsn_t <- update(dsn_t, vitk_sd = m_total$vitk_sd)

res_cont <- fit_models(dsn_t, "vitk_sd", "总摄入每SD")
res_q4 <- run_quant(m_total, 4, "vitk_total", "总摄入四分位")
res_q5 <- run_quant(m_total, 5, "vitk_total", "总摄入五分位")
write.csv(res_cont, file.path(res_dir, "T1_主分析_连续.csv"), row.names = FALSE)
write.csv(res_q4,  file.path(res_dir, "T2_主分析_四分位.csv"), row.names = FALSE)
write.csv(res_q5,  file.path(res_dir, "T3_主分析_五分位.csv"), row.names = FALSE)
cat("Q4 trend P =", attr(res_q4, "trend_p"), "; Q5 trend P =", attr(res_q5, "trend_p"), "\n")
message("== 主分析完成 ==")

# ============================================================
# 2. RCS 剂量反应（4 节点 5/35/65/95；3 节点敏感性）
# ============================================================
p01 <- quantile(m_total$vitk_total, 0.01, na.rm = TRUE)
p99 <- quantile(m_total$vitk_total, 0.99, na.rm = TRUE)
m_rcs <- m_total %>% filter(vitk_total >= p01 & vitk_total <= p99)
writeLines(c(sprintf("p01=%.1f p99=%.1f n=%d(剔除%d)", p01, p99,
                     nrow(m_rcs), nrow(m_total) - nrow(m_rcs))),
           file.path(res_dir, "RCS_extreme_trim.txt"))

knots4 <- quantile(m_rcs$vitk_total, c(0.05, 0.35, 0.65, 0.95), na.rm = TRUE)
b4 <- as.data.frame(unclass(as.matrix(rms::rcs(m_rcs$vitk_total, knots4)))); names(b4) <- paste0("rcs", 1:3)
dsn4 <- mk_design(cbind(m_rcs, b4))
fit_rcs4 <- svyglm(as.formula(paste0("stroke ~ rcs1 + rcs2 + rcs3 + ", m3_form)),
                   design = dsn4, family = quasibinomial)
nl4 <- regTermTest(fit_rcs4, c("rcs2", "rcs3"), method = "Wald")
cat("RCS4 非线性 P =", nl4$p, "\n")

knots3 <- quantile(m_rcs$vitk_total, c(0.10, 0.50, 0.90), na.rm = TRUE)
b3 <- as.data.frame(unclass(as.matrix(rms::rcs(m_rcs$vitk_total, knots3)))); names(b3) <- c("rcs1", "rcs2")
dsn3 <- mk_design(cbind(m_rcs, b3))
fit_rcs3 <- svyglm(as.formula(paste0("stroke ~ rcs1 + rcs2 + ", m3_form)),
                   design = dsn3, family = quasibinomial)
nl3 <- regTermTest(fit_rcs3, "rcs2", method = "Wald")
cat("RCS3 非线性 P =", nl3$p, "\n")
message("== RCS 完成 ==")
saveRDS(list(fit = fit_rcs4, knots = knots4, data = m_rcs, nl_p = nl4$p), file.path(res_dir, "rcs4_model.RDS"))
saveRDS(list(fit = fit_rcs3, knots = knots3, data = m_rcs, nl_p = nl3$p), file.path(res_dir, "rcs3_model.RDS"))

# ============================================================
# 3. 阈值分析：two-piecewise（bootstrap 拐点）
# ============================================================
piecewise_fit <- function(dsn, bp) {
  dsn <- update(dsn, l1 = pmin(vitk_total, bp), l2 = pmax(vitk_total - bp, 0))
  svyglm(as.formula(paste0("stroke ~ l1 + l2 + ", m3_form)),
         design = dsn, family = binomial)
}
find_bp <- function(dsn, step = 0.02) {
  q <- svyquantile(~vitk_total, dsn, quantiles = seq(0.02, 0.98, by = step))
  grid <- as.vector(q[[1]][, "quantile"])
  aic <- rep(NA_real_, length(grid))
  for (i in seq_along(grid)) {
    aic[i] <- tryCatch(AIC(piecewise_fit(dsn, grid[i])), error = function(e) NA_real_)
  }
  ok <- !is.na(aic)
  if (!any(ok)) stop("find_bp: 全部网格点拟合失败")
  grid[ok][which.min(aic[ok])]
}
boot_bp <- function(dat, nrep = 50, step = 0.05) {
  blk <- dat %>% distinct(SDMVSTRA, SDMVPSU)
  bps <- numeric(nrep)
  for (i in seq_len(nrep)) {
    samp <- blk[sample(nrow(blk), nrow(blk), replace = TRUE), ]
    dat_i <- dat %>% semi_join(samp, by = c("SDMVSTRA", "SDMVPSU"))
    bps[i] <- tryCatch(find_bp(mk_design(dat_i), step = step), error = function(e) NA_real_)
    if (i %% 20 == 0) message("  bootstrap ", i, "/", nrep)
  }
  bps
}
bp_main <- find_bp(dsn_t)
message("== 阈值主拐点完成 ==")
fit_bp <- piecewise_fit(dsn_t, bp_main)
if (is.null(fit_bp)) stop("piecewise_fit 失败: bp=", bp_main)
res_bp <- bind_rows(or_out(fit_bp, "l1"), or_out(fit_bp, "l2")) %>% mutate(bp = bp_main)
write.csv(res_bp, file.path(res_dir, "T4_阈值分段.csv"), row.names = FALSE)
bps <- boot_bp(m_total)
message("== 阈值 bootstrap 完成 ==")
writeLines(sprintf("拐点=%.1f (95%%CI %.1f-%.1f)", bp_main,
                   quantile(bps, 0.025, na.rm = TRUE), quantile(bps, 0.975, na.rm = TRUE)),
           file.path(res_dir, "threshold_breakpoint.txt"))
saveRDS(list(bp = bp_main, bps = bps, fit = fit_bp), file.path(res_dir, "threshold_model.RDS"))
cat("阈值拐点 =", round(bp_main, 1), "µg/天\n")

# ============================================================
# 4. 亚组与交互（9 组，森林图数据）
# ============================================================
subgroups <- list(sex = "sex", age = "age_group", BMI = "BMI_group", smoking = "smoking",
                  hypertension = "hypertension", diabetes = "diabetes", alcohol = "alcohol",
                  education = "education", income = "PIR")
sub_rows <- list()
for (nm in names(subgroups)) {
  var <- subgroups[[nm]]
  lv <- sort(unique(m_total[[var]]))
  for (i in seq_along(lv)) {
    d_i <- m_total %>% filter(.data[[var]] == lv[i]) %>% droplevels()
    if (nrow(d_i) < 50) next
    fit <- tryCatch(
      svyglm(as.formula(paste0("stroke ~ vitk_sd + ", m3_form)),
             design = mk_design(d_i), family = quasibinomial),
      error = function(e) NULL)
    if (is.null(fit)) next
    r <- or_out(fit, "vitk_sd"); r$subgroup <- nm; r$level <- as.character(lv[i])
    sub_rows[[paste(nm, i)]] <- r
  }
  fit_int <- tryCatch(
    svyglm(as.formula(paste0("stroke ~ vitk_sd * ", var, " + ",
                             paste(setdiff(covars_m3, var), collapse = " + "))),
           design = dsn_t, family = quasibinomial),
    error = function(e) NULL)
  ip <- if (is.null(fit_int)) NA_real_ else
    regTermTest(fit_int, paste0("vitk_sd:", var), method = "Wald")$p
  sub_rows[[paste(nm, "int")]] <- data.frame(subgroup = nm, level = "P_interaction",
                                             OR = NA, LCL = NA, UCL = NA, p = ip)
}
res_sub <- bind_rows(sub_rows)
write.csv(res_sub, file.path(res_dir, "T5_亚组.csv"), row.names = FALSE)
cat("亚组完成:", nrow(res_sub), "行\n")

# ============================================================
# 5. 敏感性矩阵
# ============================================================
sens_run <- function(dat, exp_var, label, model = m3_form) {
  dsn <- mk_design(dat)
  fit <- svyglm(as.formula(paste0("stroke ~ ", exp_var, " + ", model)),
                design = dsn, family = quasibinomial)
  r <- or_out(fit, exp_var); r$sensitivity <- label; r
}
sens <- bind_rows(
  sens_run(m_total, "vitk_sd", "基线(总摄入每SD,E-K)"),
  # S1 仅膳食（C-K，手稿同定义）
  { d <- m_diet; d$v <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)
    sens_run(d, "v", "仅膳食每SD(C-K)") },
  # S2 nutrient density
  { d <- m_total; d$v <- d$vitk_density / sd(d$vitk_density, na.rm = TRUE)
    sens_run(d, "v", "nutrient density每SD") },
  # S3 能量异常值互换
  sens_run(m_total %>% filter(kcal >= 800 & kcal <= 4000), "vitk_sd", "能量<800/>4000"),
  # S4 排除 K（C-J 仅膳食）
  { d <- m_diet_Kexcl; d$v <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)
    sens_run(d, "v", "排除K周期(仅膳食C-J)") },
  # S5 未加权
  { fit <- glm(as.formula(paste0("stroke ~ vitk_sd + ", m3_form)), data = m_total,
               family = binomial)
    b <- coef(fit)["vitk_sd"]; s <- sqrt(diag(vcov(fit)))["vitk_sd"]
    data.frame(term = "vitk_sd", OR = round(exp(b), 3), LCL = round(exp(b - 1.96 * s), 3),
               UCL = round(exp(b + 1.96 * s), 3),
               p = signif(summary(fit)$coefficients["vitk_sd", 4], 3),
               sensitivity = "未加权") },
  # S6 全 10 周期（含 L）
  { d <- m_all10; d$vitk_sd <- d$vitk_total / sd(d$vitk_total, na.rm = TRUE)
    sens_run(d, "vitk_sd", "全10周期(含L)") },
  # S7 L 周期单独（2021-22）
  { d <- mc %>% filter(cycle_sfx == "L"); if (nrow(d) > 200) {
      d$v <- d$vitk_total / sd(d$vitk_total, na.rm = TRUE); d$wt <- d$wt_2yr
      sens_run(d, "v", "L周期单独(2021-22)") } else NULL },
  # S8 附加调整 alcohol（限 C-J+L，K 无 ALQ）
  { d <- mc %>% filter(cycle_sfx != "K" & !is.na(alcohol)) %>% mutate(wt = wt_2yr / 9)
    d$v <- d$vitk_total / sd(d$vitk_total, na.rm = TRUE)
    sens_run(d, "v", "附加调整饮酒(排除K)", model = paste(c(covars_m3, "alcohol"), collapse = " + ")) },
  # S9 附加调整 supp_user（E-K 可判定）
  { d <- m_total %>% filter(!is.na(supp_user))
    sens_run(d, "vitk_sd", "附加调整补充剂使用", model = paste(c(covars_m3, "supp_user"), collapse = " + ")) }
)
write.csv(sens, file.path(res_dir, "T6_敏感性矩阵.csv"), row.names = FALSE)

# E-value（M3 连续每SD；卒中率≈4%）
fit_main <- svyglm(as.formula(paste0("stroke ~ vitk_sd + ", m3_form)),
                   design = dsn_t, family = quasibinomial)
est <- exp(coef(fit_main)["vitk_sd"]); lo <- exp(confint(fit_main)["vitk_sd", 1])
e <- tryCatch(EValue::evalues.OR(est, lo, rare = FALSE),
              error = function(e) EValue::evalues.OR(est, lo, rare = TRUE))
write.csv(as.data.frame(e), file.path(res_dir, "T7_evalue.csv"), row.names = TRUE)

# ============================================================
# 6. 子分析（关卡③）：仅膳食 C-K，复现手稿
# ============================================================
cat("== 子分析队列（仅膳食 C-K）: n =", nrow(m_diet), " 卒中", sum(m_diet$stroke == 1), "==\n")
d_diet <- m_diet
d_diet$vitk_sd_diet <- d_diet$vitk_diet / sd(d_diet$vitk_diet, na.rm = TRUE)
res_cont9 <- fit_models(mk_design(d_diet), "vitk_sd_diet", "仅膳食每SD(C-K)")
write.csv(res_cont9, file.path(res_dir, "T8_子分析_连续.csv"), row.names = FALSE)

# 五分位切点（未加权，对照手稿 Q1≤29.8…Q5≥137.9）
q9 <- quantile(d_diet$vitk_diet, c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
cat("仅膳食五分位切点:", paste(round(q9, 1), collapse = " / "), "(手稿: 29.8/50.3/78.3/137.9)\n")
writeLines(sprintf("切点: %s", paste(round(q9, 1), collapse = " / ")),
           file.path(res_dir, "sub2003_2020_cuts.txt"))
d_diet$q5 <- cut(d_diet$vitk_diet, breaks = c(-Inf, q9, Inf), labels = FALSE)
fit9 <- svyglm(as.formula(paste0("stroke ~ factor(q5) + ", m3_form)),
               design = mk_design(d_diet), family = quasibinomial)
tb9 <- tidy(fit9, conf.int = TRUE, exponentiate = TRUE) %>% filter(grepl("^factor\\(q5\\)", term))
write.csv(tb9, file.path(res_dir, "T9_子分析_五分位.csv"), row.names = FALSE)

# 阈值拐点复现（对照手稿 117.0 µg/天）
dsn9 <- mk_design(d_diet)
bp9 <- find_bp(dsn9)
writeLines(sprintf("2003-2020仅膳食拐点=%.1f (手稿=117.0)", bp9),
           file.path(res_dir, "threshold_breakpoint_sub2003_2020.txt"))
cat("子分析拐点 =", round(bp9, 1), "µg/天 (手稿 117.0)\n")

# ============================================================
# 汇总
# ============================================================
saveRDS(list(cont = res_cont, q4 = res_q4, q5 = res_q5, sub = res_sub,
             sens = sens, sub9 = res_cont9, tb9 = tb9,
             bp_main = bp_main, bp9 = bp9),
        file.path(res_dir, "master_results.RDS"))
message("== 04_analysis 完成：结果已写入 results/ ==")
