suppressPackageStartupMessages({library(survey); library(survival); library(dplyr)})
options(survey.lonely.psu = "adjust")
mort_dir <- "D:/OneDrive - Business/小黑屋/level 1专题课/第19期NHANES/NHANES数据库挖掘专题课课前资料/3.死亡数据"
mc <- readRDS("data/analysis/master_cc.RDS")
d <- mc %>% filter(cycle_sfx %in% c("C","D","E","F","G","H","I","J")) %>% mutate(wt8 = wt_2yr / 8)
mort1 <- read.csv(file.path(mort_dir, "NHANES_MORT_1999_2018.csv"))
mort2 <- read.csv(file.path(mort_dir, "NHANES_specificmortality_1999_2018.csv"))
d <- d %>% left_join(mort1, by = "SEQN") %>% left_join(mort2, by = "SEQN") %>%
  filter(eligstat == 1 & !is.na(time_int) & time_int > 0)
d$vitk_sd_diet <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)
mk <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt8, data = dat, nest = TRUE)
covars_m3 <- c("age","sex","race","education","PIR","marital","BMI","smoking",
               "hypertension","diabetes","CHD","TC","HDL","kcal","satfat","chol_int")
m3 <- paste(covars_m3, collapse = " + ")

for (var in c("sex","smoking")) {
  for (lv in sort(unique(d[[var]]))) {
    d_i <- d %>% filter(.data[[var]] == lv) %>% droplevels()
    fit_i <- tryCatch(svycoxph(as.formula(paste0("Surv(time_int, all_cause_mort) ~ vitk_sd_diet + ", m3)),
                               design = mk(d_i)), error = function(e) e)
    if (inherits(fit_i, "error")) {
      cat(var, "=", as.character(lv), "失败:", conditionMessage(fit_i), "\n")
    } else {
      cat(var, "=", as.character(lv), "OK\n")
    }
  }
}
