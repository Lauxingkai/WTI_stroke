# ============================================================================
# 12c_calibrated_nri.R  (Opt-4: calibrated NRI/IDI per Leening 2014)
# Recalibrate predicted probabilities before recomputing continuous NRI/IDI
# (fit calibration slope+intercept on outcome, then recompute). Compares
# raw vs calibrated NRI/IDI for WTI vs base in both cohorts.
# Output: qc/opt4_calibrated_nri.txt
# Date: 2026-08-16 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({library(dplyr); library(readr); library(haven)})
set.seed(42)
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw"); OUT <- file.path(RAW, "data/processed")
lines <- character(0)
logline <- function(...) { l <- sprintf(...); lines <<- c(lines, l); cat(l, "\n") }

nri_idi <- function(p0, p1, y) {
  d <- p1 - p0
  up_e <- mean(d[y == 1] > 0); dn_e <- mean(d[y == 1] < 0)
  up_n <- mean(d[y == 0] > 0); dn_n <- mean(d[y == 0] < 0)
  c(nri = (up_e - dn_e) + (dn_n - up_n),
    idi = (mean(p1[y == 1]) - mean(p0[y == 1])) - (mean(p1[y == 0]) - mean(p0[y == 0])))
}
# calibration: fit intercept+slope via glm(y ~ offset(logit(p)))
calib <- function(p, y) {
  lo <- log(p / (1 - p))
  m <- glm(y ~ lo, family = binomial())
  a <- coef(m)[1]; b <- coef(m)[2]
  function(pnew) plogis(a + b * log(pnew / (1 - pnew)))
}

run <- function(dat, yvar, tag) {
  f0 <- glm(as.formula(paste(yvar, "~ age + sex")), data = dat, family = binomial())
  f1 <- glm(as.formula(paste(yvar, "~ WTI + age + sex")), data = dat, family = binomial())
  p0 <- predict(f0, type = "response"); p1 <- predict(f1, type = "response")
  ok <- !is.na(p0) & !is.na(p1) & !is.na(dat[[yvar]])
  y <- dat[[yvar]][ok]; p0k <- p0[ok]; p1k <- p1[ok]
  raw <- nri_idi(p0k, p1k, y)
  # split-half calibration to avoid overfitting: calibrate on half, apply to all
  n <- length(y); idx <- sample(n, n / 2)
  c0 <- calib(p0k[idx], y[idx]); c1 <- calib(p1k[idx], y[idx])
  cal <- nri_idi(c0(p0k), c1(p1k), y)
  logline("%s raw NRI=%.4f IDI=%.6f | calibrated NRI=%.4f IDI=%.6f",
      tag, raw["nri"], raw["idi"], cal["nri"], cal["idi"])
}

nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov.csv"), show_col_types = FALSE)
nh$age <- nh$RIDAGEYR; nh$sex <- nh$RIAGENDR
run(nh, "stroke", "NHANES")
ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(age = as.numeric(age), sex = as.numeric(sex)) %>%
  filter(!is.na(age) & !is.na(bmi) & !is.na(sex))
run(ch, "stroke_base", "CHARLS")
writeLines(lines, "D:/NHANES/qc/opt4_calibrated_nri.txt")
cat("\nDONE\n")
