# ============================================================================
# 12b_physician_subset.R  (Opt-3: physician-diagnosed stroke subset sensitivity)
# Redefine 2018 incident stroke using da007_8_ (diagnosed by doctor) only,
# rerun Cox M1/M3 + FG M1 vs main (da019∪da007) results.
# Output: qc/opt3_physician_subset.txt
# Date: 2026-08-16 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({library(survival); library(cmprsk); library(dplyr); library(readr); library(haven)})
set.seed(42)
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed")
lines <- character(0)
logline <- function(...) { l <- sprintf(...); lines <<- c(lines, l); cat(l, "\n") }

pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)
d <- pr %>% left_join(ev, by = "ID_12") %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         sex_m = ifelse(sex == 1, 1, 0), age = as.numeric(age),
         w = bloodweight / mean(bloodweight, na.rm=TRUE),
         ftime = pmin(time, 7.0)) %>%
  filter(!is.na(WTI_sd) & !is.na(age) & !is.na(bmi) & !is.na(stroke))
# physician-diagnosed subset: 2018 da007_8_==1 (doctor-diagnosed history)
hs18 <- read_dta(file.path(RAW, "CHARLS/CHARLS_1725074232_3/CHARLS/2018/Health_Status_and_Functioning.dta"),
                 col_select = c(ID, da007_8_)) %>%
  mutate(ID_12 = as.character(ID) %>% stringr::str_trim(),
         phys = as.numeric(da007_8_) == 1) %>%
  select(ID_12, phys)
d <- d %>% left_join(hs18, by = "ID_12") %>%
  mutate(stroke_phys = ifelse(is.na(phys), FALSE, phys))
logline("main events (any source): %d | physician-diagnosed subset events: %d",
    sum(d$stroke), sum(d$stroke_phys))
run_cox <- function(yvar, tag) {
  f <- as.formula(paste("Surv(ftime,", yvar, ") ~ WTI_sd + age + sex_m"))
  m <- coxph(f, data = d, weights = w, cluster = communityID)
  s <- summary(m)$coefficients["WTI_sd", ]
  logline("%-18s HR=%.4f (%.4f-%.4f) p=%.4f", tag, s[["exp(coef)"]],
      exp(log(s[["exp(coef)"]]) - 1.96 * s[["robust se"]]),
      exp(log(s[["exp(coef)"]]) + 1.96 * s[["robust se"]]), s[["Pr(>|z|)"]])
}
run_cox("stroke", "Cox-M1 any")
run_cox("stroke_phys", "Cox-M1 phys-only")
writeLines(lines, "D:/NHANES/qc/opt3_physician_subset.txt")
cat("\nDONE\n")
