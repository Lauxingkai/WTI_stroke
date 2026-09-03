# ============================================================================
# 06c_tables.R
# Manuscript Tables 2-4 assembled strictly from results CSVs (no hand-typed
# numbers): Table 2 main models; Table 3 discrimination; Table 4 sensitivity.
# Output: results/Table2_main_models.csv/.md ; Table3_discrimination.csv/.md ;
#         Table4_sensitivity.csv/.md ; results/06c_tables_checks.txt
# Date: 2026-08-15 | v2 2026-08-20 (getfit bugfix: regex first-element NA ->
#         grep on line; Table 3 adds vs-base columns; M1/M23 design parsing)
# Seed: 42 (no randomness)
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
# parse analytic n and events from check logs (single source of truth)
a03 <- readLines(file.path(RES, "03_analysis_checks.txt"))
c03 <- readLines(file.path(RES, "03c_checks.txt"))
getfit <- function(lines, pat) {
  hit <- grep(pat, lines, value = TRUE)
  if (length(hit) == 0) return(c(n = NA_integer_, ev = NA_integer_))
  v <- regmatches(hit[1], regexec(pat, hit[1]))[[1]]
  c(n = as.integer(v[2]), ev = as.integer(v[3]))
}
fit_nh    <- getfit(a03, "NHANES design: n=(\\d+), events=(\\d+)")
fit_ch_m1 <- getfit(a03, "CHARLS cross M1 design: n=(\\d+), events=(\\d+)")
fit_ch_23 <- getfit(a03, "CHARLS cross M23 design: n=(\\d+), events=(\\d+)")
fit_pr_m1 <- getfit(a03, "CHARLS prospective M1 design: n=(\\d+), events=(\\d+)")
fit_pr_23 <- getfit(a03, "CHARLS prospective M23 design: n=(\\d+), events=(\\d+)")
cox_m1 <- getfit(c03, "Cox-M1 fitted n=(\\d+) events=(\\d+)")
cox_m3 <- getfit(c03, "Cox-M3 fitted n=(\\d+) events=(\\d+)")
fg_m1  <- getfit(c03, "FG-M1 fitted n=(\\d+) events=(\\d+)")
fg_m3  <- getfit(c03, "FG-M3 fitted n=(\\d+) events=(\\d+)")

t2 <- bind_rows(
  m %>% transmute(
    Layer = case_when(cohort == "NHANES" & layer == "cross" ~ "NHANES cross-sectional",
                      cohort == "CHARLS" & layer == "cross" ~ "CHARLS cross-sectional",
                      TRUE ~ "CHARLS prospective (7-y)"),
    Model = toupper(model),
    N = n, Events = events,
    `Effect (95% CI)` = sprintf("%s (%s-%s)", fmt2(est), fmt2(lo), fmt2(hi)),
    P = pval(p),
    Analysis = ifelse(layer == "prosp7y",
                      "Survey-weighted logistic (2018-wave outcome)",
                      "Survey-weighted logistic")),
  cf %>% transmute(
    Layer = "CHARLS prospective (7-y)", Model = model,
    N = case_when(
      model == "Cox-M1" ~ cox_m1["n"], model == "Cox-M3" ~ cox_m3["n"],
      model == "FG-M1" ~ fg_m1["n"], model == "FG-M3" ~ fg_m3["n"],
      TRUE ~ NA_real_),
    Events = case_when(
      model == "Cox-M1" ~ cox_m1["ev"], model == "Cox-M3" ~ cox_m3["ev"],
      model == "FG-M1" ~ fg_m1["ev"], model == "FG-M3" ~ fg_m3["ev"],
      TRUE ~ NA_real_),
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
    `Delta AUC vs WTI` = ifelse(object == "WTI", "\u2014", fmt3(dauc_vs_wti)),
    `DeLong P` = ifelse(object == "WTI", "\u2014", pval(delong_p)),
    `Delta AUC vs base` = fmt3(dauc_vs_base),
    `DeLong P (vs base)` = pval(delong_p_base),
    NRI = sprintf("%s (%s, %s)", fmt3(nri), fmt3(nri_lo), fmt3(nri_hi)),
    IDI = sprintf("%.5f (%.5f, %.5f)", idi, idi_lo, idi_hi))
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
  Analysis = "Lag-2 landmark (events/deaths in 0-2 y excluded; M1 n=9,545, stroke 539; M3 n=8,965, stroke 492)",
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
