suppressPackageStartupMessages({library(survey); library(dplyr); library(rms); library(broom); library(EValue)})
options(survey.lonely.psu = "adjust")
mc <- readRDS("data/analysis/master_cc.RDS")
m_total <- mc %>% filter(vitk_total_ok & cycle_sfx != "L") %>% mutate(wt = wt_2yr / 7)
covars_m3 <- c("age","sex","race","education","PIR","marital","BMI","smoking","hypertension","diabetes","CHD","TC","HDL","kcal","satfat","chol_int")
m3_form <- paste(covars_m3, collapse = " + ")
mk_design <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt, data = dat, nest = TRUE)

# S3: 检查列
d3 <- m_total %>% filter(kcal >= 800 & kcal <= 4000)
cat("S3 列含 vitk_sd:", "vitk_sd" %in% names(d3), " wt:", "wt" %in% names(d3), " n:", nrow(d3), "\n")

# S8: alcohol levels
d8 <- mc %>% filter(cycle_sfx != "K" & !is.na(alcohol)) %>% mutate(wt = wt_2yr / 9)
cat("S8 alcohol levels:", paste(levels(d8$alcohol), collapse = ","), " n:", nrow(d8), "\n")
cat("S8 周期分布:", paste(names(table(d8$cycle_sfx)), table(d8$cycle_sfx), collapse = " "), "\n")

# S9: supp_user levels
d9 <- m_total %>% filter(!is.na(supp_user))
cat("S9 supp_user levels:", paste(levels(d9$supp_user), collapse = ","), " n:", nrow(d9), "\n")
