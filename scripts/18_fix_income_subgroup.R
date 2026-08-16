# ============================================================
# 18_fix_income_subgroup.R — 修复 income 亚组完全分离问题
# 问题: 04_analysis.R §4 亚组循环对连续变量 PIR 按 unique() 逐值分层,
#       产生完全分离(爆炸 OR / 0 值), income 亚组 OR 不可用。
# 修复: PIR 按 survey 加权三分位分组(PIR_group T1/T2/T3);
#       分层 OR 用不含 PIR 的 M3 协变量(避免分层内重复调整);
#       交互检验用 vitk_sd * PIR_group (Wald)。
# 输入: data/analysis/master_cc.RDS
# 输出: results/T5_income_fix.csv (新 income 亚组结果)
#       results/income_pir_cuts.txt (PIR 加权三分位切点)
#       results/T5_亚组.csv (更新: 原 income 行替换为新结果)
# 说明: 仅修改 T5 的 income 部分, 不触碰其他亚组与其他结果文件,
#       保持 04_analysis.R 其余输出哈希基线不变。
# ============================================================

suppressPackageStartupMessages({
  library(survey)
  library(dplyr)
})

options(survey.lonely.psu = "adjust")
set.seed(2026)

res_dir <- "results"
mc <- readRDS("data/analysis/master_cc.RDS")

# ---------- 与 04_analysis.R 相同的队列定义 ----------
m_total <- mc %>% filter(vitk_total_ok & cycle_sfx != "L") %>%
  mutate(wt = wt_2yr / 7)
# vitk_sd 在 04_analysis.R §1 运行时创建（vitk_total 除以 SD），此处复现
sd_vk <- sd(m_total$vitk_total, na.rm = TRUE)
m_total$vitk_sd <- m_total$vitk_total / sd_vk

mk_design <- function(dat) {
  svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt,
            data = dat, nest = TRUE)
}

# ---------- PIR 加权三分位 ----------
q <- svyquantile(~PIR, mk_design(m_total), quantiles = c(1 / 3, 2 / 3))
cuts <- as.vector(q[[1]][, "quantile"])
m_total$PIR_group <- cut(m_total$PIR, breaks = c(-Inf, cuts, Inf),
                         labels = c("T1", "T2", "T3"))
dsn <- mk_design(m_total)  # 在 PIR_group 创建后构建，确保 design 含该变量
writeLines(sprintf("PIR weighted tertile cutoffs: %.2f / %.2f", cuts[1], cuts[2]),
           file.path(res_dir, "income_pir_cuts.txt"))
cat("PIR 三分位切点:", round(cuts, 2), "\n")

# ---------- 分层 OR（M3 不含 PIR，避免分层内重复调整） ----------
covars_m3_noPIR <- c("age", "sex", "race", "education", "marital",
                     "BMI", "smoking", "hypertension", "diabetes",
                     "CHD", "TC", "HDL", "kcal", "satfat", "chol_int")
m3_noPIR_form <- paste(covars_m3_noPIR, collapse = " + ")

or_out <- function(fit, term, digits = 3) {
  b <- coef(fit); s <- sqrt(diag(vcov(fit)))
  data.frame(term = term,
             OR = round(exp(b[term]), digits),
             LCL = round(exp(b[term] - 1.96 * s[term]), digits),
             UCL = round(exp(b[term] + 1.96 * s[term]), digits),
             p = signif(summary(fit)$coefficients[term, 4], 3))
}

rows <- list()
for (lv in levels(m_total$PIR_group)) {
  d_i <- m_total %>% filter(PIR_group == lv)
  fit <- tryCatch(
    svyglm(as.formula(paste0("stroke ~ vitk_sd + ", m3_noPIR_form)),
           design = mk_design(d_i), family = quasibinomial),
    error = function(e) { message("[layer ", lv, "] fit error: ", conditionMessage(e)); NULL })
  if (is.null(fit)) next
  r <- or_out(fit, "vitk_sd"); r$subgroup <- "income"; r$level <- lv
  rows[[lv]] <- r
}

# ---------- 交互检验（分组交互，Wald） ----------
fit_int <- tryCatch(
  svyglm(as.formula(paste0("stroke ~ vitk_sd * PIR_group + ", m3_noPIR_form)),
         design = dsn, family = quasibinomial),
  error = function(e) { message("[interaction] fit error: ", conditionMessage(e)); NULL })
ip <- if (is.null(fit_int)) NA_real_ else
  regTermTest(fit_int, "vitk_sd:PIR_group", method = "Wald")$p
rows[["int"]] <- data.frame(subgroup = "income", level = "P_interaction",
                            OR = NA, LCL = NA, UCL = NA, p = ip)

res <- bind_rows(rows)
message("[debug] res columns: ", paste(colnames(res), collapse = ", "))
message("[debug] res rows: ", nrow(res))
res <- res[, c("subgroup", "level", "OR", "LCL", "UCL", "p", "term")]
write.csv(res, file.path(res_dir, "T5_income_fix.csv"), row.names = FALSE)

# ---------- 更新 T5_亚组.csv：替换 income 行 ----------
t5 <- read.csv(file.path(res_dir, "T5_亚组.csv"), stringsAsFactors = FALSE)
n_old_income <- sum(t5$subgroup == "income")
t5 <- t5 %>% filter(subgroup != "income")
t5 <- bind_rows(t5, res)
write.csv(t5, file.path(res_dir, "T5_亚组.csv"), row.names = FALSE)

cat("income 亚组修复完成: 替换", n_old_income, "行 →", nrow(res), "行\n")
print(res)
