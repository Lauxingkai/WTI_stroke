# ============================================================================
# 08_audit_consistency.R  (Phase 2: T7 internal consistency of effect tables)
# CI symmetry (log scale) + p-value vs CI cross-check for every estimate.
# Output: qc/phase2_consistency.txt
# Date: 2026-08-16
# ============================================================================
suppressPackageStartupMessages({library(dplyr); library(readr)})
RES <- "D:/NHANES/results"
out <- "D:/NHANES/qc/phase2_consistency.txt"
lines <- character(0)
logline <- function(...) { l <- sprintf(...); lines <<- c(lines, l); cat(l, "\n") }

check <- function(tag, est, lo, hi, p) {
  sym_ok <- abs(log(hi) - log(est) - (log(est) - log(lo))) < 1e-4
  se <- (log(hi) - log(lo)) / (2 * 1.96)
  z <- log(est) / se
  p_ci <- 2 * pnorm(-abs(z))
  p_ok <- abs(p_ci - p) < 0.02
  logline("%-24s est=%.3f (%.3f-%.3f) p=%.4g | sym=%s | p_from_CI=%.4g match=%s",
          tag, est, lo, hi, p, ifelse(sym_ok, "OK", "FAIL"),
          p_ci, ifelse(p_ok, "OK", "FAIL"))
  c(sym_ok, p_ok)
}

m  <- read_csv(file.path(RES, "03_main_models.csv"), show_col_types = FALSE)
cf <- read_csv(file.path(RES, "03c_cox_fg.csv"), show_col_types = FALSE)
l2 <- read_csv(file.path(RES, "05b_lag2.csv"), show_col_types = FALSE)
ic <- read_csv(file.path(RES, "05c_interval.csv"), show_col_types = FALSE)

logline("=== 03_main_models ===")
for (i in seq_len(nrow(m)))
  check(paste(m$cohort[i], m$layer[i], m$model[i]), m$est[i], m$lo[i], m$hi[i], m$p[i])
logline("=== 03c cox/fg ===")
for (i in seq_len(nrow(cf)))
  check(cf$model[i], cf$hr[i], cf$lo[i], cf$hi[i], cf$p[i])
logline("=== 05b lag2 ===")
for (i in seq_len(nrow(l2)))
  check(l2$model[i], l2$hr[i], l2$lo[i], l2$hi[i], l2$p[i])
logline("=== 05c interval ===")
for (i in seq_len(nrow(ic)))
  check(ic$model[i], ic$est[i], ic$lo[i], ic$hi[i], ic$p[i])

writeLines(lines, out)
cat("\nDONE\n")
