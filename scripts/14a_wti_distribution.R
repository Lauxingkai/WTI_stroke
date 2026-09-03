# ============================================================================
# 14a_wti_distribution.R
# WTI distribution (mean, SD, median, IQR, p10, p90) per analysis layer,
# on the primary (maximal M1) samples, for reporting absolute effect sizes
# alongside per-1-SD estimates (review item E16 / M9).
# Output: results/14a_wti_distribution.csv ; results/14a_checks.txt
# Date: 2026-08-20 | Seed: 42 (no randomness)
# ============================================================================
suppressPackageStartupMessages({library(dplyr); library(readr)})
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")
logf <- file(file.path(RES, "14a_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

summarize_wti <- function(d, tag) {
  d <- d %>% filter(!is.na(WTI))
  q <- quantile(d$WTI, probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE)
  data.frame(layer = tag, n = nrow(d),
             mean = mean(d$WTI, na.rm = TRUE), sd = sd(d$WTI, na.rm = TRUE),
             p10 = q[1], p25 = q[2], median = q[3], p75 = q[4], p90 = q[5])
}

rows <- list()

nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov_v2.csv"), show_col_types = FALSE)
rows[[1]] <- summarize_wti(nh, "NHANES fasting cross (analytic cohort)")

c11 <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(age) &
           !is.na(sex) & !is.na(stroke_base))
rows[[2]] <- summarize_wti(c11, "CHARLS 2011 cross (M1 maximal sample)")

ev <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)
pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE) %>%
  left_join(ev %>% select(ID_12, stroke), by = "ID_12") %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(age) &
           !is.na(sex) & !is.na(stroke))
rows[[3]] <- summarize_wti(pr, "CHARLS 2011 prospective (M1 maximal sample)")

c15 <- read_csv(file.path(OUT, "charls_2015_cross_cov.csv"), show_col_types = FALSE) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(age) & !is.na(sex))
rows[[4]] <- summarize_wti(c15, "CHARLS 2015 replication (M1 eligible)")

out <- bind_rows(rows) %>% mutate(across(where(is.numeric), ~ round(., 2)))
write_csv(out, file.path(RES, "14a_wti_distribution.csv"))
logline(paste(capture.output(print(out)), collapse = "\n"))
logline("\n=== 14a COMPLETE ===")
close(logf)
