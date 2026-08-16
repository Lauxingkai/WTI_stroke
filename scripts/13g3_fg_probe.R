# probe summary.crr structure
suppressPackageStartupMessages({ library(survival); library(dplyr); library(readr) })
nh <- read_csv("D:/NHANES/data/processed/nhanes_fasting_cross_cov.csv", show_col_types = FALSE)
mort <- read_csv("D:/NHANES/data/nhanes_mort2019.csv", show_col_types = FALSE) %>%
  select(seqn, mortstat, ucod_leading, permth_int)
nh <- nh %>% left_join(mort, by = c("SEQN" = "seqn")) %>%
  mutate(time_y = permth_int / 12,
         death = ifelse(mortstat == 1, 1, 0),
         death_stroke = ifelse(mortstat == 1 & ucod_leading == 5, 1, 0),
         WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
         pa_ter = cut(pa_min_day, quantile(pa_min_day, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                      include.lowest = TRUE, labels = c("L", "M", "H"))) %>%
  filter(!is.na(permth_int), !is.na(mortstat), !is.na(WTI), !is.na(RIDAGEYR),
         !is.na(RIAGENDR), !is.na(death_stroke)) %>%
  mutate(fstatus = ifelse(death_stroke == 1, 1, ifelse(death == 1, 2, 0)))
v <- c("time_y", "fstatus", "WTI_sd", "RIDAGEYR", "RIAGENDR", "RIDRETH1", "edu",
       "smoke", "drink", "bmi", "htn", "dm", "statin", "bp_rx", "pa_ter")
nh <- nh %>% filter(complete.cases(select(., all_of(v))))
fit <- cmprsk::crr(nh$time_y, nh$fstatus,
                   model.matrix(~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu +
                                  smoke + drink + bmi + htn + dm + statin + bp_rx +
                                  pa_ter, data = nh)[, -1],
                   failcode = 1, cencode = 0)
s <- summary(fit)$coef
print(head(s, 2))
cat("colnames:", paste(colnames(s), collapse = " | "), "\n")
