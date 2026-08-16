suppressPackageStartupMessages({ library(dplyr); library(readr) })
ch <- read_csv("D:/NHANES/data/charls_2015_cross_cov.csv", show_col_types = FALSE) %>%
  mutate(sex_m = ifelse(sex == 1, 1, 0),
         pa_ter = cut(pa_days_week, c(-1, 0, 1, 100), labels = c("0d", "1-6d", "7d"))) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age))
m3v <- c("stroke_base", "WTI", "age", "sex_m", "edu", "smoke", "drink", "bmi",
         "htn", "dm", "lipid_rx", "bp_rx", "pa_ter", "bloodweight", "communityID",
         "urban_nbs")
ch2 <- ch %>% filter(complete.cases(select(., all_of(m3v))))
cat("glm-complete n:", nrow(ch2), "events:", sum(ch2$stroke_base), "\n")
cat("NA counts: edu", sum(is.na(ch$edu)), "| lipid_rx", sum(is.na(ch$lipid_rx)),
    "| bp_rx", sum(is.na(ch$bp_rx)), "| smoke", sum(is.na(ch$smoke)),
    "| drink", sum(is.na(ch$drink)), "| htn", sum(is.na(ch$htn)),
    "| dm", sum(is.na(ch$dm)), "| pa", sum(is.na(ch$pa_days_week)),
    "| bmi", sum(is.na(ch$bmi)), "\n")
cat("NA sex:", sum(is.na(ch$sex_m)), "| NA urban:", sum(is.na(ch$urban_nbs)), "\n")
