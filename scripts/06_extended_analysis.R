# ============================================================
# 06_extended_analysis.R — 方向①②③（扩展分析）
# 方向①: CVD 复合结局（卒中+CHD+心梗+心衰, MCQ160B/C/D/E）横断面
# 方向②: NDI 死亡链接前瞻分析（全因/CVD/脑血管死亡, svycoxph 加权 Cox）
# 方向③: 补充剂使用者 vs 非使用者分层
# 队列: C-J（2003-2018, 8 周期, 与死亡数据覆盖一致）, wt8 = wt_2yr/8
# 方法学对标: Chen W 2026 JHN、Han H 2026 NRP、Chen G 2025 FSN
# ============================================================

suppressPackageStartupMessages({
  library(survey)
  library(survival)
  library(dplyr)
})

options(survey.lonely.psu = "adjust")
set.seed(2026)
out_dir <- "results"
mort_dir <- "D:/OneDrive - Business/小黑屋/level 1专题课/第19期NHANES/NHANES数据库挖掘专题课课前资料/3.死亡数据"

# ---------- 数据 ----------
mc <- readRDS("data/analysis/master_cc.RDS")
d <- mc %>% filter(cycle_sfx %in% c("C","D","E","F","G","H","I","J")) %>%
  mutate(wt8 = wt_2yr / 8)

mort1 <- read.csv(file.path(mort_dir, "NHANES_MORT_1999_2018.csv"))
mort2 <- read.csv(file.path(mort_dir, "NHANES_specificmortality_1999_2018.csv"))
d <- d %>%
  left_join(mort1, by = "SEQN") %>%
  left_join(mort2, by = "SEQN")

# 死亡资格（eligstat==1 可随访）；随访时间用 time_int
d$fup_mo <- d$time_int
cat("== 队列（C-J, 与死亡数据合并）: n =", nrow(d), "==\n")
cat("eligstat==1:", sum(d$eligstat == 1, na.rm = TRUE), "\n")
cat("全因死亡:", sum(d$all_cause_mort == 1, na.rm = TRUE),
    " CVD死亡:", sum(d$cvd_mort == 1, na.rm = TRUE),
    " 脑血管死亡:", sum(d$Cerebrovascular.diseases_mort == 1, na.rm = TRUE), "\n")

# ---------- 变量 ----------
# 方向①: CVD 复合（心梗160B/CHD 160C/心衰160D/卒中160E）
d$cvd_comp <- factor(ifelse(pmax(d$MCQ160B, d$MCQ160C, d$MCQ160D, d$MCQ160E,
                                 na.rm = TRUE) == 1, 1, 0),
                     levels = 0:1, labels = c("否","是"))
# 方向③: 补充剂使用分层（NA/0 = 未使用）
d$supp_grp <- factor(ifelse(is.na(d$vitk_supp) | d$vitk_supp <= 0, 0, 1),
                     levels = 0:1, labels = c("未使用","使用"))

covars_m2 <- c("age","sex","race","education","PIR","marital")
covars_m3 <- c(covars_m2, "BMI","smoking","hypertension","diabetes","CHD",
               "TC","HDL","kcal","satfat","chol_int")
m2_form <- paste(covars_m2, collapse = " + ")
m3_form <- paste(covars_m3, collapse = " + ")

mk_design <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                                     weights = ~wt8, data = dat, nest = TRUE)
hr_out <- function(fit, term) {
  b <- coef(fit); s <- sqrt(diag(vcov(fit)))
  ct <- summary(fit)$coefficients
  data.frame(term = term,
             HR = round(exp(b[term]), 3),
             LCL = round(exp(b[term] - 1.96 * s[term]), 3),
             UCL = round(exp(b[term] + 1.96 * s[term]), 3),
             p = signif(ct[term, ncol(ct)], 3))
}

# 暴露（仅膳食 vitk_diet，全 C-J 可用；每 SD）
d$vitk_sd_diet <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)

# ============================================================
# 方向①: CVD 复合结局横断面（svyglm 三模型）
# ============================================================
cat("\n== 方向①: CVD 复合结局（横断面）==\n")
d1 <- d %>% filter(!is.na(cvd_comp))
dsn1 <- mk_design(d1)
rows1 <- list()
for (nm in c("M1","M2","M3")) {
  f <- if (nm == "M1") "cvd_comp ~ vitk_sd_diet + age + sex"
       else paste0("cvd_comp ~ vitk_sd_diet + ", get(paste0("m", substring(nm, 2), "_form")))
  fit <- svyglm(as.formula(f), design = dsn1, family = quasibinomial)
  r <- hr_out(fit, "vitk_sd_diet"); r$model <- nm; r$outcome <- "CVD复合"
  rows1[[nm]] <- r
}
res_cvd <- bind_rows(rows1)
print(res_cvd)
write.csv(res_cvd, file.path(out_dir, "T10_方向1_CVD复合.csv"), row.names = FALSE)

# ============================================================
# 方向②: NDI 死亡分析（svycoxph 三模型 × 三结局）
# ============================================================
cat("\n== 方向②: 死亡分析（前瞻 Cox）==\n")
d2 <- d %>% filter(eligstat == 1 & !is.na(fup_mo) & fup_mo > 0)
dsn2 <- mk_design(d2)

cox_models <- function(outcome_var, label, dat, dsn) {
  rows <- list()
  for (nm in c("M1","M2","M3")) {
    f <- if (nm == "M1") as.formula(paste0("Surv(fup_mo, ", outcome_var, ") ~ vitk_sd_diet + age + sex"))
         else as.formula(paste0("Surv(fup_mo, ", outcome_var, ") ~ vitk_sd_diet + ",
                                get(paste0("m", substring(nm, 2), "_form"))))
    fit <- tryCatch(svycoxph(f, design = dsn), error = function(e) NULL)
    if (is.null(fit)) { rows[[nm]] <- data.frame(term = "vitk_sd_diet", HR = NA, LCL = NA, UCL = NA, p = NA, model = nm, outcome = label) }
    else { r <- hr_out(fit, "vitk_sd_diet"); r$model <- nm; r$outcome <- label; rows[[nm]] <- r }
  }
  bind_rows(rows)
}

res_mort <- bind_rows(
  cox_models("all_cause_mort", "全因死亡", d2, dsn2),
  cox_models("cvd_mort", "CVD死亡", d2, dsn2),
  cox_models("Cerebrovascular.diseases_mort", "脑血管死亡", d2, dsn2)
)
print(res_mort)
write.csv(res_mort, file.path(out_dir, "T11_方向2_死亡Cox.csv"), row.names = FALSE)

# 死亡分析: 四分位（仅膳食）
q4 <- quantile(d2$vitk_diet, c(0.25, 0.5, 0.75), na.rm = TRUE)
d2$q4d <- cut(d2$vitk_diet, breaks = c(-Inf, q4, Inf), labels = FALSE)
dsn2q <- mk_design(d2)
rowsq <- list()
for (nm in c("M1","M2","M3")) {
  f <- if (nm == "M1") Surv(fup_mo, all_cause_mort) ~ factor(q4d) + age + sex
       else as.formula(paste0("Surv(fup_mo, all_cause_mort) ~ factor(q4d) + ",
                              get(paste0("m", substring(nm, 2), "_form"))))
  fit <- tryCatch(svycoxph(f, design = dsn2q), error = function(e) NULL)
  if (!is.null(fit)) {
    tb <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>% filter(grepl("q4d", term))
    tb$model <- nm; rowsq[[nm]] <- tb
  }
}
res_mort_q4 <- bind_rows(rowsq)
write.csv(res_mort_q4, file.path(out_dir, "T12_方向2_死亡四分位.csv"), row.names = FALSE)

# 敏感性: 排除随访 < 24 个月（反向因果检验）
d3 <- d2 %>% filter(fup_mo >= 24)
dsn3 <- mk_design(d3)
res_mort_sens <- cox_models("all_cause_mort", "全因死亡(排除<24月)", d3, dsn3)
write.csv(res_mort_sens, file.path(out_dir, "T13_方向2_死亡敏感性.csv"), row.names = FALSE)

# ============================================================
# 方向③: 补充剂分层（死亡结局 × supp_grp 交互）
# ============================================================
cat("\n== 方向③: 补充剂分层 ==\n")
d4 <- d2 %>% filter(!is.na(supp_grp))
dsn4 <- mk_design(d4)
# 交互检验（全因死亡）
fit_int <- tryCatch(
  svycoxph(Surv(fup_mo, all_cause_mort) ~ vitk_sd_diet * supp_grp + age + sex + race + education +
             PIR + marital + BMI + smoking + hypertension + diabetes + CHD + TC + HDL +
             kcal + satfat + chol_int, design = dsn4),
  error = function(e) NULL)
p_int <- if (is.null(fit_int)) NA_real_ else {
  ct <- summary(fit_int)$coefficients
  term_int <- grep("vitk_sd_diet.*supp_grp", rownames(ct), value = TRUE)[1]
  ct[term_int, "Pr(>|z|)"]
}
cat("交互 P =", p_int, "\n")

# 分层 HR（每 SD）
rows3 <- list()
for (lv in c("未使用","使用")) {
  d5 <- d4 %>% filter(supp_grp == lv)
  fit <- tryCatch(svycoxph(Surv(fup_mo, all_cause_mort) ~ vitk_sd_diet + age + sex + race +
                             education + PIR + marital + BMI + smoking + hypertension +
                             diabetes + CHD + TC + HDL + kcal + satfat + chol_int,
                           design = mk_design(d5)), error = function(e) NULL)
  if (!is.null(fit)) {
    r <- hr_out(fit, "vitk_sd_diet"); r$supp_grp <- lv; rows3[[lv]] <- r
  }
}
res_supp <- bind_rows(rows3)
res_supp$p_interaction <- p_int
print(res_supp)
write.csv(res_supp, file.path(out_dir, "T14_方向3_补充剂分层.csv"), row.names = FALSE)

message("== 06 扩展分析完成 ==")
