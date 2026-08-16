# CHARLS sex/age strata M1 (cross-sectional + prospective) for cross-cohort
# comparison with the NHANES stratification (Plan-C completion).
suppressPackageStartupMessages({library(survey); library(dplyr); library(readr)})
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed")
lines <- character(0)
logline <- function(...) { l <- sprintf(...); lines <<- c(lines, l); cat(l, "\n") }

ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         sex_m = ifelse(sex == 1, 1, 0), age = as.numeric(age),
         w_norm = bloodweight / mean(bloodweight, na.rm=TRUE)) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age))
chd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm, data = ch, nest = TRUE)
runm <- function(design, form, tag) {
  m <- svyglm(form, family = quasibinomial(), design = design)
  s <- coef(summary(m))["WTI_sd", ]
  logline("%-24s OR=%.3f (%.3f-%.3f) p=%.4f", tag, exp(s[1]),
          exp(s[1]-1.96*s[2]), exp(s[1]+1.96*s[2]), s[4])
}
logline("=== CHARLS cross-sectional strata ===")
runm(chd, stroke_base ~ WTI_sd + age + sex_m, "all cm1")
runm(subset(chd, age < 60), stroke_base ~ WTI_sd + age + sex_m, "age 45-59")
runm(subset(chd, age >= 60), stroke_base ~ WTI_sd + age + sex_m, "age >=60")
runm(subset(chd, sex_m == 1), stroke_base ~ WTI_sd + age, "male")
runm(subset(chd, sex_m == 0), stroke_base ~ WTI_sd + age, "female")

pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)
d <- pr %>% left_join(ev, by = "ID_12") %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         sex_m = ifelse(sex == 1, 1, 0), age = as.numeric(age),
         w = bloodweight / mean(bloodweight, na.rm=TRUE),
         fstatus = ifelse(stroke, 1, ifelse(death, 2, 0)), ftime = pmin(time, 7.0)) %>%
  filter(!is.na(WTI_sd) & !is.na(age) & !is.na(bmi) & !is.na(stroke))
logline("=== CHARLS prospective strata (weighted logistic, 2018-wave outcome) ===")
pd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w, data = d, nest = TRUE)
runm(pd, stroke_2018 ~ WTI_sd + age + sex_m, "all pm1")
runm(subset(pd, age < 60), stroke_2018 ~ WTI_sd + age + sex_m, "age <60")
runm(subset(pd, age >= 60), stroke_2018 ~ WTI_sd + age + sex_m, "age >=60")
runm(subset(pd, sex_m == 1), stroke_2018 ~ WTI_sd + age, "male")
runm(subset(pd, sex_m == 0), stroke_2018 ~ WTI_sd + age, "female")
writeLines(lines, "D:/NHANES/qc/phase5_charls_strata.txt")
cat("\nDONE\n")
