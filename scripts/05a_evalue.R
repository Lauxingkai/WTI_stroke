# ============================================================================
# 05a_evalue.R
# Analysis : E-values for every cohort x model (VanderWeele & Ding 2017,
#            Ann Intern Med 167:268-274, PMID 28693043)
# Inputs   : results/03_main_models.csv (survey-weighted ORs)
#            results/03c_cox_fg.csv (Cox HR + Fine-Gray sHR)
# Output   : results/05a_evalue.csv
# Note     : rare-outcome OR ~ RR approximation; point estimate AND CI-upper
#            E-values reported (conservative: use CI-upper E-value)
# Date     : 2026-08-15 | Seed: 42 (no randomness; kept for convention)
# ============================================================================
suppressPackageStartupMessages({library(dplyr); library(readr)})
set.seed(42)
RAW <- "D:/NHANES"; RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "05a_evalue_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

evalue <- function(rr, rr_hi) {
  rr    <- ifelse(rr    < 1, 1 / rr,    rr)
  rr_hi <- ifelse(rr_hi < 1, 1,         rr_hi)
  E_pt  <- rr    + sqrt(rr    * (rr    - 1))
  E_ci  <- rr_hi + sqrt(rr_hi * (rr_hi - 1))
  c(evalue_est = E_pt, evalue_ci = E_ci)
}

rows <- list()
add <- function(src, est, lo, hi, p) {
  e <- evalue(est, hi)
  rows[[length(rows)+1]] <<- data.frame(
    source = src, est = est, lo = lo, hi = hi, p = p,
    evalue_est = e[["evalue_est"]], evalue_ci = e[["evalue_ci"]])
  logline(sprintf("%-22s est=%.3f (%.3f-%.3f) p=%.3f | E=%.2f E_ci=%.2f",
                  src, est, lo, hi, p, e[["evalue_est"]], e[["evalue_ci"]]))
}

m <- read_csv(file.path(RES, "03_main_models.csv"), show_col_types = FALSE)
for (i in seq_len(nrow(m))) {
  add(sprintf("%s-%s-%s", m$cohort[i], m$layer[i], m$model[i]),
      m$est[i], m$lo[i], m$hi[i], m$p[i])
}
c <- read_csv(file.path(RES, "03c_cox_fg.csv"), show_col_types = FALSE)
for (i in seq_len(nrow(c))) {
  add(sprintf("CHARLS-prosp-%s", c$model[i]), c$hr[i], c$lo[i], c$hi[i], c$p[i])
}

write_csv(bind_rows(rows), file.path(RES, "05a_evalue.csv"))
logline("\n=== 05a EVALUE COMPLETE ===")
close(logf)
