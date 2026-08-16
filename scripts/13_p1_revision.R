# ============================================================
# 13_p1_revision.R — P1 修订：竞争风险模型
# CVD 死亡/脑血管死亡的 Fine-Gray 子分布风险模型
# 方法: survival::finegray() 生成子分布权重数据 + svycoxph 加权拟合
#       （survey 设计的标准 Fine-Gray 实现）
# 输出: results/T20_竞争风险.csv
# ============================================================

suppressPackageStartupMessages({
  library(survey); library(survival); library(dplyr)
})
options(survey.lonely.psu = "adjust")
set.seed(2026)

mc <- readRDS("data/analysis/master_cc.RDS")
d <- mc %>% filter(cycle_sfx %in% c("C","D","E","F","G","H","I","J")) %>% mutate(wt8 = wt_2yr / 8)
mort <- readRDS("data/mortality/LMF2019_public_CJ.rds")
mort$seqn <- as.numeric(mort$seqn)
d <- d %>% left_join(mort, by = c("SEQN" = "seqn")) %>%
  filter(eligstat == 1 & !is.na(permth_int) & permth_int > 0)
d$vitk_sd_diet <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)
# 死因构造（ucod_leading: 1=心脏病 2=肿瘤 3=慢呼 4=意外 5=脑血管 6=AD 7=DM 8=流肺 9=肾病 10=其他）
d$cvd_mort <- as.integer(d$mortstat == 1 & d$ucod_leading == 1)   # 心脏病死亡
d$cer_mort <- as.integer(d$mortstat == 1 & d$ucod_leading == 5)   # 脑血管死亡
cat("CVD(心脏病)死亡:", sum(d$cvd_mort), "; 脑血管死亡:", sum(d$cer_mort), "\n")

covars_m3 <- c("age","sex","race","education","PIR","marital","BMI","smoking",
               "hypertension","diabetes","CHD","TC","HDL","kcal","satfat","chol_int")
m3 <- paste(covars_m3, collapse = " + ")
mk <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt8, data = dat, nest = TRUE)

# ---------- 竞争风险：CVD 死亡（竞争=非CVD死亡）；脑血管死亡（竞争=非脑血管死亡）----------
fg_analysis <- function(event_var, label, d) {
  # 构造竞争风险状态：0=删失, 1=目标事件, 2=竞争事件（factor 多状态）
  d$fg_ev <- factor(case_when(d[[event_var]] == 1 ~ 1L,
                              d$mortstat == 1 ~ 2L,
                              TRUE ~ 0L),
                    levels = 0:2, labels = c("cens","event","comp"))
  # finegray 生成子分布数据（时间 月→年；公式含 SEQN/vitk_sd_diet 以保留列）
  fg <- finegray(Surv(permth_int / 12, fg_ev) ~ SEQN + vitk_sd_diet, data = d)
  fg$SEQN <- as.numeric(as.character(fg$SEQN))
  fg_dat <- fg %>% select(SEQN, fgstart, fgstop, fgstatus) %>%
    left_join(d %>% select(SEQN, vitk_sd_diet, SDMVPSU, SDMVSTRA, wt8,
                           age, sex, race, education, PIR, marital, BMI,
                           smoking, hypertension, diabetes, CHD, TC, HDL,
                           kcal, satfat, chol_int), by = "SEQN")
  fit <- svycoxph(as.formula(paste0("Surv(fgstart, fgstop, fgstatus) ~ vitk_sd_diet + ", m3)),
                  design = mk(fg_dat))
  ct <- summary(fit)$coefficients; ci <- exp(confint(fit)["vitk_sd_diet", ])
  data.frame(outcome = label, model = "Fine-Gray(子分布)",
             SHR = round(exp(coef(fit)["vitk_sd_diet"]), 3),
             LCL = round(ci[1], 3), UCL = round(ci[2], 3),
             p = signif(ct["vitk_sd_diet", ncol(ct)], 3))
}

res_fg <- bind_rows(
  fg_analysis("cvd_mort", "心脏病死亡", d),
  fg_analysis("cer_mort", "脑血管死亡", d)
)
write.csv(res_fg, "results/T20_竞争风险.csv", row.names = FALSE)
print(res_fg)
message("== P1 竞争风险完成 ==")
