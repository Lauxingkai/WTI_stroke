# ============================================================================
# 13h2_charls_prosp_mde.R
# MDE alignment for CHARLS prospective Cox M3 (final fitting layer 9,028/515).
# Same method as 13h_mde.R (Freedman Cox, 80% power, alpha=0.05, covariate R2).
# Output: results/13h2_prosp_mde.txt
# ============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr) })

mde_cox <- function(events, p_exposed_r2) {
  za <- qnorm(0.975); zb <- qnorm(0.8)
  exp(sqrt((za + zb)^2 / (events * (1 - p_exposed_r2))))
}

pr <- read_csv("D:/NHANES/data/processed/charls_2011_2018_prosp_cov.csv",
               show_col_types = FALSE) %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
         sex_m = ifelse(sex == 1, 1, 0)) %>%
  filter(!is.na(stroke_base) & stroke_base == 0,
         !is.na(WTI_sd) & !is.na(age) & !is.na(sex_m) & !is.na(bmi) &
           !is.na(edu) & !is.na(smoke) & !is.na(drink) & !is.na(htn) &
           !is.na(dm) & !is.na(lipid_rx) & !is.na(bp_rx) & !is.na(pa_days_week) &
           !is.na(bloodweight) & bloodweight > 0)
r2 <- summary(lm(WTI_sd ~ age + sex_m + edu + smoke + drink + bmi + htn + dm +
                   lipid_rx + bp_rx + pa_days_week, data = pr))$r.squared
n <- nrow(pr)
events <- sum(pr$stroke_2018, na.rm = TRUE)
mde_m3 <- mde_cox(events, r2)
mde_m1 <- mde_cox(sum(pr$stroke_2018, na.rm = TRUE), 0)

out <- c(
  sprintf("CHARLS prospective Cox M3 (final layer): n=%d events=%d R2=%.3f -> MDE HR %.3f",
          n, events, r2, mde_m3),
  sprintf("CHARLS prospective Cox M1: events=%d -> MDE HR %.3f (no covariate R2)",
          events, mde_m1))
writeLines(out, "D:/NHANES/results/13h2_prosp_mde.txt")
cat(paste(out, collapse = "\n"), "\n")