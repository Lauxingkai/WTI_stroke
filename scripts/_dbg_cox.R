suppressPackageStartupMessages({library(survival); library(dplyr); library(readr)})
d <- read_csv("D:/NHANES/data/processed/charls_2011_2018_prosp_cov.csv", show_col_types = FALSE)
ev <- read_csv("D:/NHANES/data/processed/charls_events_2011_2018.csv", show_col_types = FALSE)
d <- d %>% left_join(ev, by = "ID_12") %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         sex_m = ifelse(sex == 1, 1, 0),
         w = bloodweight / mean(bloodweight, na.rm=TRUE),
         age = as.numeric(age)) %>%
  filter(!is.na(WTI_sd) & !is.na(age) & !is.na(bmi) & !is.na(stroke))
f <- coxph(Surv(time, stroke) ~ WTI_sd + age + sex_m, data = d,
           weights = w, cluster = communityID)
s <- summary(f)$coefficients["WTI_sd", ]
print(s)
cat("ncol:", length(s), "\n")
