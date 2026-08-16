# ============================================================================
# 08_audit_vif_mde.R  (Phase 5: T18 VIF + T20 minimum detectable effect)
# VIF: 1/(1-R2) per M3 covariate (unweighted glm approximation; survey VIF
#      unavailable without car). MDE: minimum OR/HR detectable at alpha=0.05,
#      power=0.8 given observed n/events and exposure-vs-covariates R2
#      (Hsieh et al. 1998 for logistic; Freedman approximation for Cox).
# Output: qc/phase5_vif_mde.txt
# Date: 2026-08-16 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({library(dplyr); library(readr); library(survival)})
set.seed(42)
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed"); QC <- file.path(RAW, "qc")
lines <- character(0)
logline <- function(...) { l <- sprintf(...); lines <<- c(lines, l); cat(l, "\n") }

vif_glm <- function(dat, resp, preds) {
  f <- as.formula(paste(resp, "~", paste(preds, collapse = " + ")))
  r2 <- sapply(preds, function(p) {
    others <- setdiff(preds, p)
    ff <- as.formula(paste(p, "~", paste(others, collapse = " + ")))
    m <- lm(ff, data = dat)
    summary(m)$r.squared
  })
  data.frame(var = preds, vif = 1 / (1 - r2))
}

mde_logistic <- function(n, events, p_exposed_r2, sd_beta = 1) {
  p <- events / n
  vif_adj <- 1 - p_exposed_r2
  # Hsieh: n = (za+zb)^2 / (p*(1-p)*beta^2*(1-rho2)) ; beta per 1-SD
  za <- qnorm(0.975); zb <- qnorm(0.8)
  beta_min <- sqrt((za + zb)^2 / (n * p * (1 - p) * vif_adj))
  exp(beta_min)   # OR per 1 SD
}

mde_cox <- function(events, p_exposed_r2) {
  za <- qnorm(0.975); zb <- qnorm(0.8)
  beta_min <- sqrt((za + zb)^2 / (events * (1 - p_exposed_r2)))
  exp(beta_min)   # HR per 1 SD
}

# ---------------- NHANES M3 ----------------
logline("=== T18 VIF: NHANES M3 covariates ===")
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE))
nh_m3 <- c("RIDAGEYR", "RIAGENDR", "RIDRETH1", "edu", "smoke", "drink",
           "bmi", "htn", "dm", "statin", "bp_rx")
v1 <- vif_glm(nh, "WTI_sd", nh_m3)
print(v1); logline("max VIF (NHANES M3): %.2f (%s)", max(v1$vif), v1$var[which.max(v1$vif)])
logline("")
nh_events <- sum(nh$stroke, na.rm = TRUE)
r2_nh <- summary(lm(WTI_sd ~ RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink +
                      bmi + htn + dm + statin + bp_rx, data = nh))$r.squared
logline("NHANES M3: n=%d events=%d WTI|X R2=%.3f", nrow(nh), nh_events, r2_nh)
logline("MDE OR per 1 SD (80%% power): %.3f", mde_logistic(nrow(nh), nh_events, r2_nh))
logline("M1 exposure R2: %.3f", summary(lm(WTI_sd ~ RIDAGEYR + RIAGENDR, data = nh))$r.squared)
logline("MDE M1 OR: %.3f", mde_logistic(nrow(nh), nh_events,
                                        summary(lm(WTI_sd ~ RIDAGEYR + RIAGENDR, data = nh))$r.squared))
logline("")

# ---------------- CHARLS cross M3 ----------------
logline("=== T18 VIF: CHARLS cross M3 covariates ===")
ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         age = as.numeric(age), sex_m = ifelse(sex == 1, 1, 0)) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age))
ch_m3 <- c("age", "sex_m", "edu", "smoke", "drink", "bmi", "htn", "dm", "lipid_rx", "bp_rx")
v2 <- vif_glm(ch, "WTI_sd", ch_m3)
print(v2); logline("max VIF (CHARLS M3): %.2f (%s)", max(v2$vif), v2$var[which.max(v2$vif)])
r2_ch <- summary(lm(WTI_sd ~ age + sex_m + edu + smoke + drink + bmi + htn + dm +
                      lipid_rx + bp_rx, data = ch))$r.squared
logline("CHARLS cross M3: n=%d events=%d R2=%.3f -> MDE OR %.3f",
        nrow(ch), sum(ch$stroke_base, na.rm=TRUE), r2_ch,
        mde_logistic(nrow(ch), sum(ch$stroke_base, na.rm=TRUE), r2_ch))
logline("")

# ---------------- CHARLS prospective Cox M3 ----------------
logline("=== T20 MDE: CHARLS prospective Cox M3 ===")
pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)
d <- pr %>% left_join(ev, by = "ID_12") %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         age = as.numeric(age), sex_m = ifelse(sex == 1, 1, 0)) %>%
  filter(!is.na(WTI_sd) & !is.na(age) & !is.na(bmi) & !is.na(stroke))
r2_pr <- summary(lm(WTI_sd ~ age + sex_m + edu + smoke + drink + bmi + htn + dm +
                      lipid_rx + bp_rx, data = d))$r.squared
logline("CHARLS Cox M3: n=%d events=%d R2=%.3f -> MDE HR %.3f",
        nrow(d), sum(d$stroke), r2_pr, mde_cox(sum(d$stroke), r2_pr))
logline("M1 (age+sex only) R2=%.3f -> MDE HR %.3f",
        summary(lm(WTI_sd ~ age + sex_m, data = d))$r.squared,
        mde_cox(sum(d$stroke), summary(lm(WTI_sd ~ age + sex_m, data = d))$r.squared))

writeLines(lines, file.path(QC, "phase5_vif_mde.txt"))
cat("\nDONE\n")
