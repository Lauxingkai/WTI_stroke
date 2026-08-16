# 17_cancer_sens_ci.R -- 重跑"排除基线癌症"敏感性并输出完整 95%CI
# 依据: 08_robustness.R 第 3 节（仅打印 HR/P，本脚本补全 CI 供 Table 3 内联）
suppressPackageStartupMessages({
  library(survey); library(survival); library(dplyr)
})
options(survey.lonely.psu = "adjust")

mort_dir <- "D:/OneDrive - Business/小黑屋/level 1专题课/第19期NHANES/NHANES数据库挖掘专题课课前资料/3.死亡数据"
mc <- readRDS("data/analysis/master_cc.RDS")
d <- mc %>% filter(cycle_sfx %in% c("C","D","E","F","G","H","I","J")) %>% mutate(wt8 = wt_2yr / 8)
mort1 <- read.csv(file.path(mort_dir, "NHANES_MORT_1999_2018.csv"))
mort2 <- read.csv(file.path(mort_dir, "NHANES_specificmortality_1999_2018.csv"))
d <- d %>% left_join(mort1, by = "SEQN") %>% left_join(mort2, by = "SEQN")
d <- d %>% filter(eligstat == 1 & !is.na(time_int) & time_int > 0)

cancer_df <- list()
for (sfx in c("C","D","E","F","G","H","I","J")) {
  m <- haven::read_xpt(file.path("data/raw", paste0("MCQ_", sfx, ".XPT")))
  if ("MCQ220" %in% names(m)) cancer_df[[sfx]] <- m %>% select(SEQN, cancer = MCQ220)
}
cancer_df <- bind_rows(cancer_df)
d <- d %>% left_join(cancer_df, by = "SEQN")
d$vitk_sd_diet <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)

mk_design <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                                     weights = ~wt8, data = dat, nest = TRUE)
covars_m3 <- c("age","sex","race","education","PIR","marital","BMI","smoking",
               "hypertension","diabetes","CHD","TC","HDL","kcal","satfat","chol_int")
f3 <- as.formula(paste("Surv(time_int, all_cause_mort) ~ vitk_sd_diet +",
                       paste(covars_m3, collapse = " + ")))

d_noca <- d %>% filter(is.na(cancer) | cancer != 1)
fit_noca <- svycoxph(f3, design = mk_design(d_noca))
ci <- confint(fit_noca)["vitk_sd_diet", ]
out <- data.frame(
  setting = "排除基线癌症",
  n = nrow(d_noca),
  deaths = sum(d_noca$all_cause_mort == 1),
  HR = exp(coef(fit_noca)["vitk_sd_diet"]),
  LCL = exp(ci[1]), UCL = exp(ci[2]),
  P = summary(fit_noca)$coefficients["vitk_sd_diet", "Pr(>|z|)"]
)
print(out)
write.csv(out, "results/T26_癌症敏感性_CI.csv", row.names = FALSE)
cat("\nOK\n")
