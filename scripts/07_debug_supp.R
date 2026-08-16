suppressPackageStartupMessages({library(survey); library(survival); library(dplyr)})
options(survey.lonely.psu = "adjust")
mc <- readRDS("data/analysis/master_cc.RDS")
md <- "D:/OneDrive - Business/小黑屋/level 1专题课/第19期NHANES/NHANES数据库挖掘专题课课前资料/3.死亡数据"
mort1 <- read.csv(file.path(md, "NHANES_MORT_1999_2018.csv"))
mort2 <- read.csv(file.path(md, "NHANES_specificmortality_1999_2018.csv"))
d <- mc %>% filter(cycle_sfx %in% c("C","D","E","F","G","H","I","J")) %>% mutate(wt8 = wt_2yr / 8)
d <- d %>% left_join(mort1, by = "SEQN") %>% left_join(mort2, by = "SEQN")
d <- d %>% filter(eligstat == 1 & !is.na(time_int) & time_int > 0)
d$supp_grp <- factor(ifelse(d$vitk_supp > 0, 1, 0), levels = 0:1, labels = c("未使用","使用"))
cat("未使用组 n:", sum(d$supp_grp == "未使用", na.rm = TRUE),
    " 使用组 n:", sum(d$supp_grp == "使用", na.rm = TRUE), "\n")
d$vitk_sd_diet <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)
mk <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt8, data = dat, nest = TRUE)
covars <- "age + sex + race + education + PIR + marital + BMI + smoking + hypertension + diabetes + CHD + TC + HDL + kcal + satfat + chol_int"
for (lv in c("未使用","使用")) {
  d5 <- d %>% filter(supp_grp == lv)
  fit <- tryCatch(svycoxph(as.formula(paste0("Surv(time_int, all_cause_mort) ~ vitk_sd_diet + ", covars)),
                           design = mk(d5)), error = function(e) NULL)
  if (is.null(fit)) {
    cat(lv, ": 模型失败\n")
  } else {
    ct <- summary(fit)$coefficients
    cat(lv, ": HR=", round(exp(coef(fit)["vitk_sd_diet"]), 3),
        " p=", signif(ct["vitk_sd_diet", ncol(ct)], 3), "\n", sep = "")
  }
}
