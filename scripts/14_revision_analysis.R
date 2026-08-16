# 14_revision_analysis.R — 审稿修订分析（T1/T2/T3/T5/T10）
# 依据：nature-review-studio 审稿任务表（build/output/review_vitk_mortality_20260811.md）
# 队列：data/analysis/master_cc.RDS（C-K 周期完整案例）+ 死亡队列（同 08_robustness.R 口径）
suppressPackageStartupMessages({
  library(survey); library(survival); library(dplyr); library(EValue)
})
options(survey.lonely.psu = "adjust")

mc  <- readRDS("data/analysis/master_cc.RDS")
cat("master_cc:", nrow(mc), "行\n")

# ---------- 死亡队列构建（与 08_robustness.R 一致：C-J 周期 + LMF 合并） ----------
mort_dir <- "D:/OneDrive - Business/小黑屋/level 1专题课/第19期NHANES/NHANES数据库挖掘专题课课前资料/3.死亡数据"
d <- mc %>% filter(cycle_sfx %in% c("C","D","E","F","G","H","I","J")) %>% mutate(wt8 = wt_2yr / 8)
mort1 <- read.csv(file.path(mort_dir, "NHANES_MORT_1999_2018.csv"))
mort2 <- read.csv(file.path(mort_dir, "NHANES_specificmortality_1999_2018.csv"))
d <- d %>% left_join(mort1, by = "SEQN") %>% left_join(mort2, by = "SEQN")
d <- d %>% filter(eligstat == 1 & !is.na(time_int) & time_int > 0)
d$vitk_sd <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)
cat("死亡队列:", nrow(d), "行；全因死亡:", sum(d$all_cause_mort == 1), "例\n")

# ---------- T1: E-value（点估计 + CI 下界） ----------
# 全调整模型主估计（Model 3，与稿件一致）：HR 0.887 (0.819-0.961)
cat("\n===== T1 E-value（点估计与 CI 下界） =====\n")
ev <- evalues.OR(est = 0.887, lo = 0.819, hi = 0.961, rare = FALSE)
print(ev)
write.csv(as.data.frame(ev), "results/T21_Evalue_CI.csv", row.names = TRUE)

# ---------- T2: 比例风险假设（Schoenfeld 残差） ----------
cat("\n===== T2 比例风险假设检验 =====\n")
# svycoxph 无 cox.zph 方法，采用未加权 coxph 近似检验（报告时注明近似）
covars_m3 <- c("age", "sex", "race", "education", "PIR", "marital", "BMI",
               "smoking", "hypertension", "diabetes", "CHD", "TC", "HDL",
               "kcal", "satfat", "chol_int")
d$vitk_sd <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)
f <- as.formula(paste("Surv(time_int, all_cause_mort) ~ vitk_sd +",
                      paste(covars_m3, collapse = " + ")))
ph_fit <- coxph(f, data = d)
zph <- cox.zph(ph_fit)
print(zph)
write.csv(as.data.frame(zph$table), "results/T22_PH检验.csv", row.names = TRUE)
# 主暴露 PH 随时间变化图（可选输出）
png("output/figures/FigureS_PH_schoenfeld.png", width = 2400, height = 1600, res = 300)
plot(zph[1], main = "Schoenfeld residuals for vitamin K intake (T2 revision)")
abline(h = coef(ph_fit)["vitk_sd"], lty = 2)
dev.off()
cat("PH 图已存 output/figures/FigureS_PH_schoenfeld.png\n")

# ---------- T3: 补充剂敏感性（master_cc 已含 supp_user/vitk_supp） ----------
cat("\n===== T3 补充剂敏感性 =====\n")
cat("队列含补充剂变量: supp_user/vitk_supp\n")
# T3a: 排除补充剂使用者（supp_user: '是'/'否' 因子）
if ("supp_user" %in% names(d) && any(!is.na(d$supp_user))) {
  d_noSUPP <- d %>% filter(!is.na(supp_user) & supp_user == "否")
  cat("排除补充剂使用者后样本:", nrow(d_noSUPP), "死亡:", sum(d_noSUPP$all_cause_mort == 1), "\n")
  if (nrow(d_noSUPP) > 5000) {
    d_noSUPP$wt <- d_noSUPP$wt8
    dsn <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt,
                     data = d_noSUPP, nest = TRUE)
    f3 <- as.formula(paste("Surv(time_int, all_cause_mort) ~ vitk_sd +",
                           paste(covars_m3, collapse = " + ")))
    m <- svycoxph(f3, design = dsn)
    cf <- summary(m)$conf.int["vitk_sd", ]
    cat("T3a 排除补充剂: HR", cf["exp(coef)"], "95%CI", cf["lower .95"], "-", cf["upper .95"],
        "P", summary(m)$coefficients["vitk_sd", "Pr(>|z|)"], "\n")
    t3a <- data.frame(设定 = "排除补充剂使用者", n = nrow(d_noSUPP),
                      死亡 = sum(d_noSUPP$all_cause_mort == 1),
                      HR = cf["exp(coef)"], LCL = cf["lower .95"], UCL = cf["upper .95"],
                      P = summary(m)$coefficients["vitk_sd", "Pr(>|z|)"])
  }
}
# T3b: 调整补充剂使用（主队列）
if ("supp_user" %in% names(d) && any(!is.na(d$supp_user))) {
  d_adj <- d %>% filter(!is.na(supp_user))
  d_adj$wt <- d_adj$wt8
  dsn2 <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt,
                    data = d_adj, nest = TRUE)
  f3b <- as.formula(paste("Surv(time_int, all_cause_mort) ~ vitk_sd +",
                          paste(c(covars_m3, "supp_user"), collapse = " + ")))
  m2 <- svycoxph(f3b, design = dsn2)
  cf2 <- summary(m2)$conf.int["vitk_sd", ]
  cat("T3b 调整补充剂: HR", cf2["exp(coef)"], "95%CI", cf2["lower .95"], "-", cf2["upper .95"],
      "P", summary(m2)$coefficients["vitk_sd", "Pr(>|z|)"], "\n")
  t3b <- data.frame(设定 = "调整补充剂使用", n = nrow(d_adj),
                    死亡 = sum(d_adj$all_cause_mort == 1),
                    HR = cf2["exp(coef)"], LCL = cf2["lower .95"], UCL = cf2["upper .95"],
                    P = summary(m2)$coefficients["vitk_sd", "Pr(>|z|)"])
  t3 <- if (exists("t3a")) rbind(t3a, t3b) else t3b
  write.csv(t3, "results/T23_补充剂敏感性.csv", row.names = FALSE)
  cat("T23_补充剂敏感性.csv 已写\n")
}

# ---------- T5: KM 风险表 ----------
cat("\n===== T5 KM 曲线（含风险表） =====\n")
d$q4 <- cut(d$vitk_diet, breaks = quantile(d$vitk_diet, c(0, .25, .5, .75, 1), na.rm = TRUE),
            labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)
km <- survfit(Surv(time_int/12, all_cause_mort) ~ q4, data = d)
# 风险表：0/2/4/6/8/10 年（年刻度；直接读 km$time/strata 避免 summary 分层顺序问题）
tt <- c(0, 2, 4, 6, 8, 10)
risktab <- sapply(levels(d$q4), function(lv) {
  blocks <- cumsum(km$strata)
  starts <- c(1, head(blocks, -1) + 1)
  pos <- which(names(km$strata) == paste0("q4=", lv))
  idx <- starts[pos]:blocks[pos]
  t <- km$time[idx]; nr <- km$n.risk[idx]
  sapply(tt, function(x) {
    j <- max(which(t <= x))
    if (length(j) == 0 || is.infinite(j)) nr[1] else nr[j]
  })
})
colnames(risktab) <- levels(d$q4)
rownames(risktab) <- paste0(tt, "y")
print(risktab)
write.csv(t(risktab), "results/T24_KM风险表.csv")
cat("T24_KM风险表.csv 已写\n")

cat("\n===== 14_revision_analysis.R 完成 =====\n")
