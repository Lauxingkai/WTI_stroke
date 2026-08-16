# ============================================================================
# 13h_mde.R
# Minimum detectable effect (MDE) for the new layers, same methods as
# 08_audit_vif_mde.R (Hsieh logistic / Freedman Cox, 80% power, alpha=0.05):
#   A) CHARLS 2015 cross-sectional M3 (events 252)
#   B) NHANES-NDI stroke-death Cox M3 (events 83)
#   C) NHANES-NDI all-cause Cox M3 (events 1428)
# Output: results/13h_mde.txt
# ============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(survival) })

mde_logistic <- function(n, events, p_exposed_r2) {
  p <- events / n
  za <- qnorm(0.975); zb <- qnorm(0.8)
  exp(sqrt((za + zb)^2 / (n * p * (1 - p) * (1 - p_exposed_r2))))
}
mde_cox <- function(events, p_exposed_r2) {
  za <- qnorm(0.975); zb <- qnorm(0.8)
  exp(sqrt((za + zb)^2 / (events * (1 - p_exposed_r2))))
}

out <- c()

# ---- A: CHARLS 2015 cross M3 ----
ch <- read_csv("D:/NHANES/data/processed/charls_2015_cross_cov.csv", show_col_types = FALSE) %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
         sex_m = ifelse(sex == 1, 1, 0)) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age) &
           !is.na(edu) & !is.na(smoke) & !is.na(drink) & !is.na(htn) & !is.na(dm) &
           !is.na(lipid_rx) & !is.na(bp_rx) & !is.na(pa_days_week) & !is.na(stroke_base))
r2_a <- summary(lm(WTI_sd ~ age + sex_m + edu + smoke + drink + bmi + htn + dm +
                     lipid_rx + bp_rx + pa_days_week, data = ch))$r.squared
out <- c(out, sprintf("CHARLS2015 cross M3: n=%d events=%d R2=%.3f -> MDE OR %.3f",
                      nrow(ch), sum(ch$stroke_base), r2_a,
                      mde_logistic(nrow(ch), sum(ch$stroke_base), r2_a)))

# ---- B/C: NHANES-NDI Cox ----
nh <- read_csv("D:/NHANES/data/processed/nhanes_fasting_cross_cov.csv",
               show_col_types = FALSE)
mort <- read_csv("D:/NHANES/data/nhanes_mort2019.csv", show_col_types = FALSE) %>%
  select(seqn, mortstat, ucod_leading, permth_int)
nh <- nh %>% left_join(mort, by = c("SEQN" = "seqn")) %>%
  mutate(death = ifelse(mortstat == 1, 1, 0),
         death_stroke = ifelse(mortstat == 1 & ucod_leading == 5, 1, 0),
         WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
         pa_ter = cut(pa_min_day, quantile(pa_min_day, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                      include.lowest = TRUE, labels = c("L", "M", "H"))) %>%
  filter(!is.na(permth_int), !is.na(mortstat), !is.na(WTI))
r2_ndi <- summary(lm(WTI_sd ~ RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink +
                       bmi + htn + dm + statin + bp_rx + pa_ter, data = nh))$r.squared
out <- c(out, sprintf("NHANES-NDI stroke-death M3: events=83 R2=%.3f -> MDE HR %.3f",
                      r2_ndi, mde_cox(83, r2_ndi)))
out <- c(out, sprintf("NHANES-NDI all-cause M3: events=1428 R2=%.3f -> MDE HR %.3f",
                      r2_ndi, mde_cox(1428, r2_ndi)))
out <- c(out, sprintf("NHANES-NDI stroke-death M1: events=83 -> MDE HR %.3f (no covariate R2)",
                      mde_cox(83, 0)))
out <- c(out, sprintf("NHANES-NDI all-cause M1: events=1428 -> MDE HR %.3f (no covariate R2)",
                      mde_cox(1428, 0)))

writeLines(out, "D:/NHANES/results/13h_mde.txt")
cat(paste(out, collapse = "\n"), "\n")
