# ============================================================================
# 13e_charls2015_analysis.R
# Opt-1: CHARLS 2015 blood-based cross-sectional replication layer.
# Mirrors 03_analysis.R STEP 2 (2011 layer):
#   weighted logistic M1/M2/M3 + tertiles, svydesign(ids=communityID,
#   strata=urban_nbs, weights=normalized blood weight).
# Extras vs 2011: (a) sensitivity with Weights.dta Biomarker_weight (bw_alt);
#   (b) physician-confirmed stroke outcome (M1/M2 only, 33 events);
#   (c) sex-stratified M1 for cross-wave narrative.
# Input  : D:/NHANES/data/charls_2015_cross_cov.csv (13d_build_2015.py)
# Outputs: results/13_2015_main_models.csv ; results/13_2015_checks.txt
# ============================================================================

suppressPackageStartupMessages({
  library(survey); library(dplyr); library(readr)
})

RAW <- "D:/NHANES"
RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "13_2015_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

res_rows <- list()
add_res <- function(cohort, layer, model, n, events, est, lo, hi, p) {
  res_rows[[length(res_rows) + 1]] <<- data.frame(
    cohort, layer, model, n, events, est, lo, hi, p)
}

ch <- read_csv(file.path(RAW, "data/processed/charls_2015_cross_cov.csv"),
               show_col_types = FALSE) %>%
  mutate(
    WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
    WTI_ter = cut(WTI, quantile(WTI, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                  include.lowest = TRUE, labels = c("T1", "T2", "T3")),
    pa_ter = cut(pa_days_week, c(-1, 0, 1, 100), labels = c("0d", "1-6d", "7d")),
    sex_m = ifelse(sex == 1, 1, 0),
    w_norm = bloodweight / mean(bloodweight, na.rm = TRUE),
    w_alt_norm = bw_alt / mean(bw_alt, na.rm = TRUE)
  ) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age))

logline(sprintf("CHARLS 2015 design: n=%d, events=%d, male=%d",
                nrow(ch), sum(ch$stroke_base, na.rm = TRUE), sum(ch$sex_m, na.rm = TRUE)))
logline(sprintf("CHARLS 2015 edu NA: %d (%.1f%%) | stroke_phys: %d",
                sum(is.na(ch$edu)), 100 * mean(is.na(ch$edu)),
                sum(ch$stroke_phys, na.rm = TRUE)))

chd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm,
                 data = ch, nest = TRUE)

extr <- function(fit) {
  b <- coef(fit)["WTI_sd"]; se <- sqrt(vcov(fit)["WTI_sd", "WTI_sd"])
  c(est = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se),
    p = 2 * pnorm(-abs(b / se)))
}

# ---- primary M1-M3 ----
cm1 <- svyglm(stroke_base ~ WTI_sd + age + sex_m, family = quasibinomial(), design = chd)
cm2 <- svyglm(stroke_base ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi,
              family = quasibinomial(), design = chd)
cm3 <- svyglm(stroke_base ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
                htn + dm + lipid_rx + bp_rx + pa_ter,
              family = quasibinomial(), design = chd)
for (tag in c("cm1", "cm2", "cm3")) {
  fit <- get(tag)
  e <- extr(fit)
  nn <- length(residuals(fit))
  ev <- sum(fit$y == 1, na.rm = TRUE)
  add_res("CHARLS-2015", "cross", toupper(tag), nn, ev, e[1], e[2], e[3], e[4])
  logline(sprintf("CHARLS2015 %s: OR=%.3f (%.3f-%.3f) p=%.4f  [n=%d, events=%d]",
                  toupper(tag), e[1], e[2], e[3], e[4], nn, ev))
}
cm3t <- svyglm(stroke_base ~ WTI_ter + age + sex_m + edu + smoke + drink + bmi +
                 htn + dm + lipid_rx + bp_rx + pa_ter,
               family = quasibinomial(), design = chd)
tt <- coef(summary(cm3t))[c("WTI_terT2", "WTI_terT3"), ]
logline(sprintf("CHARLS2015 tertile: T2 OR=%.3f p=%.3f | T3 OR=%.3f p=%.3f",
                exp(tt[1, 1]), tt[1, 4], exp(tt[2, 1]), tt[2, 4]))

# ---- sensitivity A: Biomarker_weight (Weights.dta) ----
chd_alt <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_alt_norm,
                     data = ch, nest = TRUE)
ca1 <- svyglm(stroke_base ~ WTI_sd + age + sex_m, family = quasibinomial(), design = chd_alt)
ca2 <- svyglm(stroke_base ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi,
              family = quasibinomial(), design = chd_alt)
ca3 <- svyglm(stroke_base ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
                htn + dm + lipid_rx + bp_rx + pa_ter,
              family = quasibinomial(), design = chd_alt)
for (tag in c("ca1", "ca2", "ca3")) {
  fit <- get(tag)
  e <- extr(fit)
  add_res("CHARLS-2015", "cross-altw", toupper(tag), length(residuals(fit)),
          sum(fit$y == 1, na.rm = TRUE), e[1], e[2], e[3], e[4])
  logline(sprintf("CHARLS2015 %s (alt weight): OR=%.3f (%.3f-%.3f) p=%.4f",
                  toupper(tag), e[1], e[2], e[3], e[4]))
}

# ---- sensitivity B: physician-confirmed stroke (M1/M2; 33 events) ----
cp1 <- svyglm(stroke_phys ~ WTI_sd + age + sex_m, family = quasibinomial(), design = chd)
cp2 <- svyglm(stroke_phys ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi,
              family = quasibinomial(), design = chd)
for (tag in c("cp1", "cp2")) {
  fit <- get(tag)
  e <- extr(fit)
  add_res("CHARLS-2015", "cross-phys", toupper(tag), length(residuals(fit)),
          sum(fit$y == 1, na.rm = TRUE), e[1], e[2], e[3], e[4])
  logline(sprintf("CHARLS2015 %s (physician-confirmed): OR=%.3f (%.3f-%.3f) p=%.4f",
                  toupper(tag), e[1], e[2], e[3], e[4]))
}

# ---- sensitivity C: sex-stratified M1 ----
for (sx in c(0, 1)) {
  sub <- subset(chd, sex_m == sx)
  fit <- svyglm(stroke_base ~ WTI_sd + age, family = quasibinomial(), design = sub)
  e <- extr(fit)
  add_res("CHARLS-2015", ifelse(sx == 1, "cross-men", "cross-women"), "M1",
          length(residuals(fit)), sum(fit$y == 1, na.rm = TRUE),
          e[1], e[2], e[3], e[4])
  logline(sprintf("CHARLS2015 M1 %s: OR=%.3f (%.3f-%.3f) p=%.4f  [n=%d, events=%d]",
                  ifelse(sx == 1, "men", "women"), e[1], e[2], e[3], e[4],
                  length(residuals(fit)), sum(fit$y == 1, na.rm = TRUE)))
}

# ---- sensitivity D: age-stratified M1 (mirrors 09_round3_s2 2011 strata) ----
for (ag in list(c(45, 59), c(60, 200))) {
  lbl <- ifelse(ag[2] > 100, "60plus", "45to59")
  sub <- subset(chd, age >= ag[1] & age <= ag[2])
  fit <- svyglm(stroke_base ~ WTI_sd + sex_m, family = quasibinomial(), design = sub)
  e <- extr(fit)
  add_res("CHARLS-2015", paste0("cross-age-", lbl), "M1",
          length(residuals(fit)), sum(fit$y == 1, na.rm = TRUE),
          e[1], e[2], e[3], e[4])
  logline(sprintf("CHARLS2015 M1 age %s: OR=%.3f (%.3f-%.3f) p=%.4f  [n=%d, events=%d]",
                  lbl, e[1], e[2], e[3], e[4],
                  length(residuals(fit)), sum(fit$y == 1, na.rm = TRUE)))
}

# ---- export ----
write_csv(bind_rows(res_rows), file.path(RES, "13_2015_main_models.csv"))
logline("\n=== 13e CHARLS 2015 REPLICATION COMPLETE ===")
close(logf)
