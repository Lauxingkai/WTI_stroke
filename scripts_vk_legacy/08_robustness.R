# ============================================================
# 08_robustness.R — 结论稳健性补检（审稿人必查项）
# 1) P for trend（四分位组中位数连续化, svycoxph）
# 2) 排除基线 CVD（MCQ160B/C/D/E 任一）敏感性
# 3) 排除基线癌症（MCQ220）敏感性
# 4) E-value（全因死亡 M3）
# 5) 比例风险假设（Schoenfeld 残差, coxph 近似）
# 6) 死亡结局 RCS（4 节点 5/35/65/95）
# ============================================================

suppressPackageStartupMessages({
  library(survey); library(survival); library(dplyr); library(EValue)
})
options(survey.lonely.psu = "adjust")
out_dir <- "results"
mort_dir <- "D:/OneDrive - Business/小黑屋/level 1专题课/第19期NHANES/NHANES数据库挖掘专题课课前资料/3.死亡数据"

mc <- readRDS("data/analysis/master_cc.RDS")
d <- mc %>% filter(cycle_sfx %in% c("C","D","E","F","G","H","I","J")) %>% mutate(wt8 = wt_2yr / 8)
mort1 <- read.csv(file.path(mort_dir, "NHANES_MORT_1999_2018.csv"))
mort2 <- read.csv(file.path(mort_dir, "NHANES_specificmortality_1999_2018.csv"))
d <- d %>% left_join(mort1, by = "SEQN") %>% left_join(mort2, by = "SEQN")
d <- d %>% filter(eligstat == 1 & !is.na(time_int) & time_int > 0)

# 癌症病史（MCQ220: 1=是 2=否）——从原始 MCQ 表提取
cancer_df <- list()
for (sfx in c("C","D","E","F","G","H","I","J")) {
  m <- haven::read_xpt(file.path("data/raw", paste0("MCQ_", sfx, ".XPT")))
  if ("MCQ220" %in% names(m)) cancer_df[[sfx]] <- m %>% select(SEQN, cancer = MCQ220)
}
cancer_df <- bind_rows(cancer_df)
d <- d %>% left_join(cancer_df, by = "SEQN")

covars_m2 <- c("age","sex","race","education","PIR","marital")
covars_m3 <- c(covars_m2, "BMI","smoking","hypertension","diabetes","CHD",
               "TC","HDL","kcal","satfat","chol_int")
m3_form <- paste(covars_m3, collapse = " + ")
mk_design <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                                     weights = ~wt8, data = dat, nest = TRUE)
d$vitk_sd_diet <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)

cat("== 死亡分析队列: n =", nrow(d), " 全因死亡:", sum(d$all_cause_mort == 1), "==\n")

# ---------- 1) P for trend ----------
q4 <- quantile(d$vitk_diet, c(0.25, 0.5, 0.75), na.rm = TRUE)
d$q4d <- cut(d$vitk_diet, breaks = c(-Inf, q4, Inf), labels = FALSE)
d$q4med <- ave(d$vitk_diet, d$q4d, FUN = function(x) median(x, na.rm = TRUE))
dsn <- mk_design(d)
fit_tr <- svycoxph(Surv(time_int, all_cause_mort) ~ q4med + age + sex + race + education +
                     PIR + marital + BMI + smoking + hypertension + diabetes + CHD +
                     TC + HDL + kcal + satfat + chol_int, design = dsn)
ct <- summary(fit_tr)$coefficients
p_trend <- ct["q4med", "Pr(>|z|)"]
cat("P for trend (M3) =", signif(p_trend, 3), "\n")
writeLines(sprintf("P for trend = %.4f", p_trend), file.path(out_dir, "mortality_p_trend.txt"))

# ---------- 2) 排除基线 CVD ----------
d$baseline_cvd <- pmax(d$MCQ160B, d$MCQ160C, d$MCQ160D, d$MCQ160E, na.rm = TRUE)
d_nocvd <- d %>% filter(is.na(baseline_cvd) | baseline_cvd != 1)
fit_nocvd <- svycoxph(Surv(time_int, all_cause_mort) ~ vitk_sd_diet + age + sex + race +
                        education + PIR + marital + BMI + smoking + hypertension +
                        diabetes + CHD + TC + HDL + kcal + satfat + chol_int,
                      design = mk_design(d_nocvd))
ct <- summary(fit_nocvd)$coefficients
cat("排除基线CVD: n =", nrow(d_nocvd), " HR =", round(exp(coef(fit_nocvd)["vitk_sd_diet"]), 3),
    " p =", signif(ct["vitk_sd_diet", "Pr(>|z|)"], 3), "\n")

# ---------- 3) 排除基线癌症 ----------
d_noca <- d %>% filter(is.na(cancer) | cancer != 1)
fit_noca <- svycoxph(Surv(time_int, all_cause_mort) ~ vitk_sd_diet + age + sex + race +
                       education + PIR + marital + BMI + smoking + hypertension +
                       diabetes + CHD + TC + HDL + kcal + satfat + chol_int,
                     design = mk_design(d_noca))
ct <- summary(fit_noca)$coefficients
cat("排除基线癌症: n =", nrow(d_noca), " HR =", round(exp(coef(fit_noca)["vitk_sd_diet"]), 3),
    " p =", signif(ct["vitk_sd_diet", "Pr(>|z|)"], 3), "\n")

# ---------- 4) E-value（全因死亡 M3）----------
fit_m3 <- svycoxph(Surv(time_int, all_cause_mort) ~ vitk_sd_diet + age + sex + race +
                     education + PIR + marital + BMI + smoking + hypertension +
                     diabetes + CHD + TC + HDL + kcal + satfat + chol_int, design = dsn)
est <- exp(coef(fit_m3)["vitk_sd_diet"])
lo <- exp(confint(fit_m3)["vitk_sd_diet", 1])
e <- tryCatch(evalues.HR(est, lo, rare = FALSE), error = function(e) NULL)
if (is.null(e)) e <- evalues.HR(est, lo, rare = TRUE)
print(e)
write.csv(as.data.frame(e), file.path(out_dir, "T15_mortality_evalue.csv"), row.names = TRUE)

# ---------- 5) 比例风险假设（Schoenfeld, coxph 未加权近似）----------
fit_ph <- tryCatch(
  coxph(Surv(time_int, all_cause_mort) ~ vitk_sd_diet + age + sex + race + education +
          PIR + marital + BMI + smoking + hypertension + diabetes + CHD + TC + HDL +
          kcal + satfat + chol_int, data = d),
  error = function(e) NULL)
if (!is.null(fit_ph)) {
  ph <- cox.zph(fit_ph)
  cat("Schoenfeld 全局 P =", signif(ph$table[nrow(ph$table), 3], 3), "\n")
  sink(file.path(out_dir, "mortality_ph_test.txt"))
  print(ph)
  sink()
}

# ---------- 6) 死亡结局 RCS（4 节点, svycoxph + rcs 基函数）----------
p01 <- quantile(d$vitk_diet, 0.01, na.rm = TRUE); p99 <- quantile(d$vitk_diet, 0.99, na.rm = TRUE)
d_rcs <- d %>% filter(vitk_diet >= p01 & vitk_diet <= p99)
k4 <- quantile(d_rcs$vitk_diet, c(0.05, 0.35, 0.65, 0.95), na.rm = TRUE)
b4 <- as.data.frame(unclass(as.matrix(rms::rcs(d_rcs$vitk_diet, k4))))
names(b4) <- c("rcs1", "rcs2", "rcs3")
d_rcs <- cbind(d_rcs, b4)
fit_rcs <- svycoxph(Surv(time_int, all_cause_mort) ~ rcs1 + rcs2 + rcs3 + age + sex + race +
                      education + PIR + marital + BMI + smoking + hypertension + diabetes +
                      CHD + TC + HDL + kcal + satfat + chol_int, design = mk_design(d_rcs))
ct <- summary(fit_rcs)$coefficients
p_nl <- 1 - pchisq((ct["rcs2","z"])^2 + (ct["rcs3","z"])^2, df = 2)  # 近似联合检验
cat("死亡结局 RCS 非线性 P（近似 2df χ²）= ", signif(p_nl, 3), "\n")
saveRDS(list(fit = fit_rcs, knots = k4, data = d_rcs), file.path(out_dir, "mortality_rcs.RDS"))

message("== 08 稳健性补检完成 ==")
