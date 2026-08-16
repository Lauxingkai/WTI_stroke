# 13g2_fg_debug.R — isolate the Fine-Gray error
suppressPackageStartupMessages({ library(survival); library(dplyr); library(readr) })
nh <- read_csv("D:/NHANES/data/processed/nhanes_fasting_cross_cov.csv", show_col_types = FALSE)
mort <- read_csv("D:/NHANES/data/nhanes_mort2019.csv", show_col_types = FALSE) %>%
  select(seqn, mortstat, ucod_leading, permth_int)
nh <- nh %>% left_join(mort, by = c("SEQN" = "seqn")) %>%
  mutate(time_y = permth_int / 12,
         death = ifelse(mortstat == 1, 1, 0),
         death_stroke = ifelse(mortstat == 1 & ucod_leading == 5, 1, 0)) %>%
  filter(!is.na(permth_int), !is.na(mortstat), !is.na(WTI), !is.na(RIDAGEYR),
         !is.na(RIAGENDR))
fg_dat <- nh %>% filter(!is.na(death_stroke)) %>%
  mutate(fstatus = ifelse(death_stroke == 1, 1, ifelse(death == 1, 2, 0)))
cat("fg_dat n:", nrow(fg_dat), "| fstatus:", paste(names(table(fg_dat$fstatus)), table(fg_dat$fstatus), collapse = ", "), "\n")
X <- model.matrix(~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi + htn + dm + statin + bp_rx + pa_ter, data = fg_dat)[, -1]
cat("X dim:", paste(dim(X), collapse = "x"), "\n")
fit <- cmprsk::crr(fg_dat$time_y, fg_dat$fstatus, X, failcode = 1, cencode = 0)
cat("crr ok. coef names:", paste(rownames(fit$coef)[1:5], collapse = ", "), "\n")
s <- summary(fit)$coef
cat("summary coef class:", class(s), "dim:", paste(dim(s), collapse = "x"), "\n")
print(s[1:3, ])
