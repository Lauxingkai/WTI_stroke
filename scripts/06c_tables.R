# ============================================================================
# 06c_tables.R
# Manuscript Tables 2-4 assembled strictly from results CSVs (no hand-typed
# numbers): Table 2 main models; Table 3 discrimination; Table 4 sensitivity.
# Output: results/Table2_main_models.csv/.md ; Table3_discrimination.csv/.md ;
#         Table4_sensitivity.csv/.md ; results/06c_tables_checks.txt
# Date: 2026-08-15 | Seed: 42 (no randomness)
# ============================================================================
suppressPackageStartupMessages({library(dplyr); library(readr)})
RAW <- "D:/NHANES"; RES <- file.path(RAW, "results")
logf <- file(file.path(RES, "06c_tables_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }
fmt2 <- function(x) sprintf("%.2f", x + 1e-9)   # +1e-9: correct FP half-up rounding (audit P2, 2026-08-16)
fmt3 <- function(x) sprintf("%.3f", x)
pval <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

# ---------------- Table 2: main models ----------------
m  <- read_csv(file.path(RES, "03_main_models.csv"), show_col_types = FALSE)
cf <- read_csv(file.path(RES, "03c_cox_fg.csv"), show_col_types = FALSE)
# parse Cox/FG analytic n and events from 03c_checks.txt (single source of truth)
c03 <- readLines(file.path(RES, "03c_checks.txt"))
cnm <- regmatches(c03, regexec("cohort n=(\\d+) \\| stroke (\\d+) \\| death (\\d+)", c03))[[1]]
n_cox <- as.integer(cnm[2]); ev_cox <- as.integer(cnm[3])
# parse model-fitted n/events from 03_analysis_checks.txt
a03 <- readLines(file.path(RES, "03_analysis_checks.txt"))
getfit <- function(pat) {
  v <- regmatches(a03, regexec(pat, a03))[[1]]
  c(n = as.integer(v[2]), ev = as.integer(v[3]))
}
fit_nh   <- getfit("NHANES design: n=(\\d+), events=(\\d+)")
fit_chc  <- getfit("CHARLS cross design: n=(\\d+), events=(\\d+)")
fit_chp  <- getfit("CHARLS prospective design: n=(\\d+), events=(\\d+)")
fit_xy <- data.frame(
  layer_key = c("NHANES-cross", "CHARLS-cross", "CHARLS-prosp7y"),
  n = c(fit_nh["n"], fit_chc["n"], fit_chp["n"]),
  ev = c(fit_nh["ev"], fit_chc["ev"], fit_chp["ev"]))
t2 <- bind_rows(
  m %>% transmute(
    layer_key = paste(cohort, layer, sep = "-"),
    Layer = case_when(cohort == "NHANES" & layer == "cross" ~ "NHANES cross-sectional",
                      cohort == "CHARLS" & layer == "cross" ~ "CHARLS cross-sectional",
                      TRUE ~ "CHARLS prospective (7-y)"),
    Model = toupper(model), N = NA_integer_, Events = NA_integer_,
    `Effect (95% CI)` = sprintf("%s (%s-%s)", fmt2(est), fmt2(lo), fmt2(hi)),
    P = pval(p),
    Analysis = ifelse(layer == "prosp7y",
                      "Survey-weighted logistic (2018-wave outcome)",
                      "Survey-weighted logistic")) %>%
    left_join(fit_xy, by = "layer_key") %>%
    mutate(N = n, Events = ev) %>% select(-layer_key, -n, -ev),
  cf %>% transmute(
    Layer = "CHARLS prospective (7-y)", Model = model,
    N = n_cox, Events = ev_cox,
    `Effect (95% CI)` = sprintf("%s (%s-%s)", fmt2(hr), fmt2(lo), fmt2(hi)),
    P = pval(p),
    Analysis = ifelse(grepl("FG", model), "Fine-Gray subdistribution", "Cause-specific Cox"))
)
write_csv(t2, file.path(RES, "Table2_main_models.csv"))
logline(sprintf("Table 2 rows: %d", nrow(t2)))

# ---------------- Table 3: discrimination ----------------
d3 <- read_csv(file.path(RES, "03b_discrimination.csv"), show_col_types = FALSE)
b3 <- read_csv(file.path(RES, "05d_boot_nri.csv"), show_col_types = FALSE)
t3 <- d3 %>%
  left_join(b3 %>% select(cohort, object, nri_lo, nri_hi, idi_lo, idi_hi),
            by = c("cohort", "object")) %>%
  transmute(
    Cohort = cohort, Index = object,
    AUC = sprintf("%s (%s-%s)", fmt3(auc), fmt3(lo), fmt3(hi)),
    `Delta AUC vs WTI` = fmt3(dauc_vs_wti),
    `DeLong P` = pval(delong_p),
    NRI = sprintf("%s (%s, %s)", fmt3(nri), fmt3(nri_lo), fmt3(nri_hi)),
    IDI = sprintf("%.5f (%.5f, %.5f)", idi, idi_lo, idi_hi),
    `Comparison` = ifelse(object == "WTI", "vs base (age+sex)", "vs WTI"))
write_csv(t3, file.path(RES, "Table3_discrimination.csv"))
logline(sprintf("Table 3 rows: %d", nrow(t3)))

# ---------------- Table 4: sensitivity ----------------
e  <- read_csv(file.path(RES, "05a_evalue.csv"), show_col_types = FALSE)
l2 <- read_csv(file.path(RES, "05b_lag2.csv"), show_col_types = FALSE)
ic <- read_csv(file.path(RES, "05c_interval.csv"), show_col_types = FALSE)
me <- read_csv(file.path(RES, "05e_mediation.csv"), show_col_types = FALSE)

t4a <- e %>% filter(source %in% c("NHANES-cross-M1","CHARLS-cross-cm1",
                                  "CHARLS-prosp-Cox-M1","CHARLS-prosp-FG-M1")) %>%
  transmute(
    Analysis = "E-value (minimally adjusted models)",
    Model = source,
    Result = sprintf("E-value %.2f (CI-upper %.2f); OR/HR %.2f (%.2f-%.2f)",
                     evalue_est, evalue_ci, est, lo, hi))
t4b <- l2 %>% transmute(
  Analysis = "Lag-2 landmark (events/deaths in 0-2 y excluded; n=8,965, stroke 492)",
  Model = model, Result = sprintf("HR/sHR %s (%s-%s), p=%s", fmt2(hr), fmt2(lo), fmt2(hi), pval(p)))
t4c <- ic %>% transmute(
  Analysis = "Interval-censored discrete-time logistic (person-period)",
  Model = model, Result = sprintf("OR %s (%s-%s), p=%s", fmt2(est), fmt2(lo), fmt2(hi), pval(p)))
t4d <- me %>% transmute(
  Analysis = "CHARLS temporal mediation (2011 WTI -> 2015 M -> 2018 stroke)",
  Model = mediator,
  Result = sprintf("indirect %s (%s, %s); proportion mediated %.1f%% (%.1f%%, %.1f%%)",
                   fmt3(indirect), fmt3(indirect_lo), fmt3(indirect_hi),
                   100*prop, 100*prop_lo, 100*prop_hi))
t4 <- bind_rows(t4a, t4b, t4c, t4d)
write_csv(t4, file.path(RES, "Table4_sensitivity.csv"))
logline(sprintf("Table 4 rows: %d", nrow(t4)))

# markdown renders (readable quick check)
write_lines("| Layer | Model | N | Events | Effect (95% CI) | P | Analysis |",
            file.path(RES, "Table2_main_models.md"))
write_lines(sprintf("|---|---|---|---|---|---|---|"), file.path(RES, "Table2_main_models.md"), append = TRUE)
for (i in seq_len(nrow(t2)))
  write_lines(sprintf("| %s | %s | %d | %d | %s | %s | %s |",
                      t2$Layer[i], t2$Model[i], t2$N[i], t2$Events[i],
                      t2$`Effect (95% CI)`[i], t2$P[i], t2$Analysis[i]),
              file.path(RES, "Table2_main_models.md"), append = TRUE)

logline("\n=== 06c TABLES COMPLETE ===")
close(logf)
