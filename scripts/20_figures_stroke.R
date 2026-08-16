# ============================================================
# 20_figures_stroke.R — 卒中横断面研究图件（A4 阶段）
# 图1 RCS 剂量-反应曲线（OR）| 图2 亚组森林图（9 组，含完整分层）
# 修复: 04_analysis.R 亚组分层模型未排除被分层变量(sex/smoking/
#       hypertension/diabetes/education/PIR 在 m3 中 → 分层内共线导致
#       拟合失败被跳过)。本脚本分层模型用 setdiff(covars_m3, var) 重算,
#       输出完整 T5_亚组_完整.csv。
# 输入: results/rcs4_model.RDS, data/analysis/master_cc.RDS, results/T5_亚组.csv
# 输出: output/figures/Figure1_stroke_RCS.{tiff,png}
#       output/figures/Figure2_stroke_forest.{tiff,png}
#       results/T5_亚组_完整.csv
# ============================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(rms); library(survey)
})
options(survey.lonely.psu = "adjust")
set.seed(2026)
fig_dir <- "output/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

save_fig <- function(p, name) {
  ggsave(file.path(fig_dir, paste0(name, ".tiff")), p, width = 8, height = 6,
         dpi = 300, compression = "lzw")
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = 8, height = 6, dpi = 300)
  cat("已输出", name, "\n")
}

# ============================================================
# 图1 RCS 剂量-反应（OR, 参考点 P25, 4 节点, 模型3）
# ============================================================
rcs_obj <- readRDS("results/rcs4_model.RDS")
fit <- rcs_obj$fit; knots <- rcs_obj$knots; dat <- rcs_obj$data; nl_p <- rcs_obj$nl_p
b <- coef(fit); v <- vcov(fit)
rcs_names <- paste0("rcs", 1:3)
beta <- b[rcs_names]; vbeta <- v[rcs_names, rcs_names]

xgrid <- seq(min(dat$vitk_total), max(dat$vitk_total), length.out = 200)
xref <- as.numeric(quantile(dat$vitk_total, 0.25))
Bg <- as.data.frame(unclass(as.matrix(rms::rcs(xgrid, knots)))); names(Bg) <- rcs_names
Bref <- as.numeric(unclass(as.matrix(rms::rcs(xref, knots)))[1, ])

lp <- as.matrix(Bg) %*% beta
lp_ref <- sum(Bref * beta)
or <- exp(lp - lp_ref)
se_lp <- sqrt(apply(as.matrix(Bg), 1, function(r) t(r) %*% vbeta %*% r))
curve_df <- data.frame(x = xgrid, OR = or,
                       lo = exp(lp - lp_ref - 1.96 * se_lp),
                       hi = exp(lp - lp_ref + 1.96 * se_lp))

bp <- 189.1  # 阈值拐点（threshold_breakpoint.txt）
p_rcs <- ggplot(curve_df, aes(x)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.25) +
  geom_line(aes(y = OR), color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = xref, linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = bp, linetype = "longdash", color = "darkred", alpha = 0.55) +
  annotate("text", x = xref, y = max(curve_df$hi) * 0.99,
           label = sprintf("Reference: P25 = %.0f µg/day", xref),
           size = 3, hjust = 1.1) +
  annotate("text", x = bp, y = min(curve_df$lo) * 1.03,
           label = "Inflection point 189.1 µg/day", size = 3, hjust = 0) +
  labs(x = "Total vitamin K intake (µg/day)", y = "OR (95% CI)",
       title = sprintf(paste0("Dose-response association between total vitamin K intake ",
                              "and prevalent stroke (4-knot RCS, Model 3; ",
                              "P nonlinear = %.3f)"), nl_p)) +
  theme_bw(base_size = 11)
save_fig(p_rcs, "Figure1_stroke_RCS")

# ============================================================
# 图2 亚组森林图（9 组完整分层, 模型3, 分层模型排除被分层变量）
# ============================================================
mc <- readRDS("data/analysis/master_cc.RDS")
m_total <- mc %>% filter(vitk_total_ok & cycle_sfx != "L") %>%
  mutate(wt = wt_2yr / 7)
sd_vk <- sd(m_total$vitk_total, na.rm = TRUE)
m_total$vitk_sd <- m_total$vitk_total / sd_vk

mk_design <- function(dat) {
  svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt,
            data = dat, nest = TRUE)
}
dsn <- mk_design(m_total)

# PIR 加权三分位（与 18_fix_income_subgroup.R 一致）
q <- svyquantile(~PIR, dsn, quantiles = c(1 / 3, 2 / 3))
cuts <- as.vector(q[[1]][, "quantile"])
m_total$PIR_group <- cut(m_total$PIR, breaks = c(-Inf, cuts, Inf),
                         labels = c("T1", "T2", "T3"))
dsn <- mk_design(m_total)

covars_m3 <- c("age", "sex", "race", "education", "PIR", "marital",
               "BMI", "smoking", "hypertension", "diabetes",
               "CHD", "TC", "HDL", "kcal", "satfat", "chol_int")
subgroups <- list(sex = "sex", age = "age_group", BMI = "BMI_group",
                  smoking = "smoking", hypertension = "hypertension",
                  diabetes = "diabetes", alcohol = "alcohol",
                  education = "education", income = "PIR_group")

or_out <- function(fit, term, digits = 3) {
  b <- coef(fit); s <- sqrt(diag(vcov(fit)))
  data.frame(term = term,
             OR = round(exp(b[term]), digits),
             LCL = round(exp(b[term] - 1.96 * s[term]), digits),
             UCL = round(exp(b[term] + 1.96 * s[term]), digits),
             p = signif(summary(fit)$coefficients[term, 4], 3))
}

rows <- list()
for (nm in names(subgroups)) {
  var <- subgroups[[nm]]
  # 分层/交互模型排除被分层变量；income(PIR_group) 需排除连续 PIR（与 18 脚本口径一致）
  excl <- if (var == "PIR_group") setdiff(covars_m3, "PIR")
          else if (var %in% covars_m3) setdiff(covars_m3, var)
          else covars_m3
  m3_excl <- paste(excl, collapse = " + ")
  lv <- levels(m_total[[var]])
  for (i in seq_along(lv)) {
    d_i <- m_total %>% filter(.data[[var]] == lv[i]) %>% droplevels()
    if (nrow(d_i) < 50) next
    fit <- tryCatch(
      svyglm(as.formula(paste0("stroke ~ vitk_sd + ", m3_excl)),
             design = mk_design(d_i), family = quasibinomial),
      error = function(e) { message("[", nm, ":", lv[i], "] ", conditionMessage(e)); NULL })
    if (is.null(fit)) next
    r <- or_out(fit, "vitk_sd"); r$subgroup <- nm; r$level <- as.character(lv[i])
    rows[[paste(nm, i)]] <- r
  }
  int_excl <- if (var == "PIR_group") setdiff(covars_m3, "PIR")
              else setdiff(covars_m3, var)
  fit_int <- tryCatch(
    svyglm(as.formula(paste0("stroke ~ vitk_sd * ", var, " + ",
                             paste(int_excl, collapse = " + "))),
           design = dsn, family = quasibinomial),
    error = function(e) NULL)
  ip <- if (is.null(fit_int)) NA_real_ else
    regTermTest(fit_int, paste0("vitk_sd:", var), method = "Wald")$p
  rows[[paste(nm, "int")]] <- data.frame(subgroup = nm, level = "P_interaction",
                                         OR = NA, LCL = NA, UCL = NA, p = ip)
}
res_sub <- bind_rows(rows)
res_sub <- res_sub[, c("subgroup", "level", "OR", "LCL", "UCL", "p", "term")]
write.csv(res_sub, "results/T5_亚组_完整.csv", row.names = FALSE)
cat("完整亚组结果已输出: ", nrow(res_sub), " 行\n")

# ---------- 森林图 ----------
en_group <- c(sex = "Sex", age = "Age", BMI = "BMI", smoking = "Smoking",
              hypertension = "Hypertension", diabetes = "Diabetes",
              alcohol = "Alcohol", education = "Education",
              income = "Income (PIR tertiles)")
en_level <- c("男性" = "Male", "女性" = "Female",
              "从不" = "Never", "既往" = "Former", "当前" = "Current",
              "否" = "No", "是" = "Yes",
              "<9年级" = "<9th grade", "9-11年级" = "9-11th grade",
              "高中/GED" = "High school/GED", "大学或AA学位" = "Some college/AA",
              "大学及以上" = "College+",
              "T1" = "Tertile 1", "T2" = "Tertile 2", "T3" = "Tertile 3")

strata <- res_sub %>% filter(level != "P_interaction") %>%
  mutate(group_lab = en_group[subgroup],
         level_lab = ifelse(level %in% names(en_level), en_level[level], level),
         y_lab = paste0(group_lab, " \u00b7 ", level_lab))
ints <- res_sub %>% filter(level == "P_interaction") %>%
  mutate(p_int = ifelse(is.na(p), NA, sprintf("%.3f", p))) %>%
  select(subgroup, p_int)

# 组顺序（自上而下）与层内顺序
ord <- c("income", "education", "alcohol", "diabetes", "hypertension",
         "smoking", "BMI", "age", "sex")
strata$subgroup <- factor(strata$subgroup, levels = ord)
strata <- strata %>% arrange(subgroup) %>%
  mutate(y_lab = factor(y_lab, levels = rev(unique(y_lab))))
strata <- strata %>% left_join(ints, by = "subgroup")

xmax <- max(strata$UCL, na.rm = TRUE)
p_forest <- ggplot(strata, aes(x = OR, y = y_lab)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = LCL, xmax = UCL), height = 0.18) +
  geom_point(shape = 21, size = 2.6, fill = "steelblue", color = "black") +
  geom_text(aes(x = xmax * 1.35, label = sprintf("%.2f (%.2f, %.2f)", OR, LCL, UCL)),
            size = 2.8, hjust = 0) +
  geom_text(aes(x = xmax * 2.05, label = p_int), size = 2.8, hjust = 0) +
  scale_x_log10() +
  labs(x = "OR (95% CI) per 1-SD increase in total vitamin K intake",
       y = NULL,
       title = "Subgroup analyses of total vitamin K intake and prevalent stroke (Model 3)") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())
save_fig(p_forest, "Figure2_stroke_forest")

cat("图件完成。\n")
