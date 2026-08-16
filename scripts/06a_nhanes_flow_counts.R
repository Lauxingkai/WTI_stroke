# ============================================================================
# 06a_nhanes_flow_counts.R
# Recompute step-wise exclusion counts for NHANES (Figure 1 flow diagram).
# Output: results/06a_nhanes_flow_counts.csv ; results/06a_flow_counts_checks.txt
# Date: 2026-08-15 | Seed: 42 (no randomness)
# ============================================================================
suppressPackageStartupMessages({library(haven); library(dplyr); library(readr)})
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw"); RES <- file.path(RAW, "results")
logf <- file(file.path(RES, "06a_flow_counts_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

rows <- list()
add <- function(step, n, note = "") {
  rows[[length(rows)+1]] <<- data.frame(cohort = "NHANES", step = step, n = n, note = note)
  logline(sprintf("%-42s n=%d %s", step, n, note))
}
cyc <- c("D","E","F","G","H","I","J")
n_total <- 0; n_age40 <- 0; n_fasting <- 0; n_wc <- 0; n_stroke <- 0; n_final <- 0
stroke_total <- 0
for (cy in cyc) {
  demo <- read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy)))
  bmx  <- read_xpt(file.path(NRAW, sprintf("BMX_%s.XPT", cy)))
  mcq  <- read_xpt(file.path(NRAW, sprintf("MCQ_%s.XPT", cy)))
  tri  <- read_xpt(file.path(NRAW, sprintf("TRIGLY_%s.XPT", cy)))
  wcol <- grep("WTSAF", names(tri), value = TRUE)[1]
  n_total <- n_total + nrow(demo)
  d0 <- demo %>% filter(as.numeric(RIDAGEYR) >= 40)
  n_age40 <- n_age40 + nrow(d0)
  d1 <- d0 %>%
    inner_join(tri %>% select(SEQN, LBXTR, all_of(wcol)), by = "SEQN") %>%
    filter(as.numeric(.data[[wcol]]) > 0 & !is.na(as.numeric(LBXTR)))
  n_fasting <- n_fasting + nrow(d1)
  d2 <- d1 %>%
    inner_join(bmx %>% select(SEQN, BMXWAIST), by = "SEQN") %>%
    filter(!is.na(as.numeric(BMXWAIST)))
  n_wc <- n_wc + nrow(d2)
  d3 <- d2 %>% inner_join(mcq %>% select(SEQN, MCQ160F), by = "SEQN")
  n_stroke <- n_stroke + nrow(d3)
  # analytic cohort = d3 (MCQ160F stroke; 7/9 refused/don't-know as non-stroke,
  # consistent with 01_id_linkage.R; MCQ160E->F P0 fix 2026-08-16)
  n_final <- n_final + nrow(d3)
  stroke_total <- stroke_total + sum(as.numeric(d3$MCQ160F) == 1)
}
add("7 cycles (D-J) participants with DEMO record", n_total)
add("Age >= 40", n_age40)
add("+ fasting subsample (WTSAF2YR > 0)", n_fasting)
add("+ measured waist circumference", n_wc)
add("+ stroke status available", n_stroke)
add("Analytic cohort (complete WTI, age>=40, fasting)", n_final, sprintf("stroke n=%d", stroke_total))
stopifnot(n_final == 10302)   # stroke count re-derived 2026-08-16 (P0 fix)

write_csv(bind_rows(rows), file.path(RES, "06a_nhanes_flow_counts.csv"))
logline("\n=== 06a COMPLETE ===")
close(logf)
