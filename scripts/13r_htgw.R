# ============================================================================
# 13r_htgw.R
# HTGW binary-exposure associations with stroke (review round 2026-08-18, item 3):
#   dichotomous HTGW (WC >= 90/80 cm for men/women AND TG >= 1.69 mmol/L),
#   M1 (age, sex) / M2 (+race[NHANES], edu, smoke, drink, bmi) / M3 (+htn, dm,
#   lipid-lowering + antihypertensive medication, physical activity),
#   survey-weighted logistic, in NHANES fasting, CHARLS 2011 and CHARLS 2015.
# Mirrors the WTI models in 03_analysis.R / 13e; supports the WTI-HTGW
# equivalence claim at the association level (Table S8c).
# Output: results/13r_htgw_models.csv ; results/13r_checks.txt
# ============================================================================
suppressPackageStartupMessages({
  library(survey); library(haven); library(dplyr); library(readr)
})
set.seed(42)
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw")
OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")
logf <- file(file.path(RES, "13r_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

rows <- list()
add_row <- function(layer, model, n, ev, est, lo, hi, p) {
  rows[[length(rows) + 1]] <<- data.frame(
    layer = layer, model = model, n = n, ev = ev,
    est = as.numeric(est), lo = as.numeric(lo), hi = as.numeric(hi), p = as.numeric(p))
}
extr <- function(fit) {
  b <- unname(coef(fit)["htgw"]); se <- unname(sqrt(vcov(fit)["htgw", "htgw"]))
  unname(c(est = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se),
           p = 2 * pnorm(-abs(b / se))))
}
run3 <- function(d_m1, d_m23, layer) {
  # M1 on maximal sample (exposure/outcome/age/sex/positive weight complete)
  dsn1 <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm,
                    data = d_m1, nest = TRUE)
  fit <- svyglm(stroke_y ~ htgw + age + sex_m, family = quasibinomial(), design = dsn1)
  e <- extr(fit)
  add_row(layer, "M1", length(residuals(fit)), sum(fit$y == 1, na.rm = TRUE),
          e[1], e[2], e[3], e[4])
  logline(sprintf("%s M1: OR=%.3f (%.3f-%.3f) p=%.4f (n=%d, events=%d)", layer,
                  e[1], e[2], e[3], e[4], length(residuals(fit)), sum(fit$y == 1, na.rm = TRUE)))
  # M2/M3 on covariate-complete sample
  dsn23 <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm,
                     data = d_m23, nest = TRUE)
  for (tag in c("m2", "m3")) {
    fit <- svyglm(get(paste0(tag, "f")), family = quasibinomial(), design = dsn23)
    e <- extr(fit)
    add_row(layer, toupper(tag), length(residuals(fit)), sum(fit$y == 1, na.rm = TRUE),
            e[1], e[2], e[3], e[4])
    logline(sprintf("%s %s: OR=%.3f (%.3f-%.3f) p=%.4f", layer, toupper(tag),
                    e[1], e[2], e[3], e[4]))
  }
}

# ---------------- NHANES ----------------
logline("=== NHANES fasting (HTGW binary) ===")
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov_v2.csv"), show_col_types = FALSE)
des <- lapply(c("D", "E", "F", "G", "H", "I", "J"), function(cy) {
  demo <- read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy)))
  demo %>% transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
}) %>% bind_rows()
nh <- nh %>%
  left_join(des, by = c("SEQN" = "SEQN", "CYCLE.x" = "CYCLE")) %>%
  rename(CYCLE = CYCLE.x) %>%
  mutate(
    wt = WTSAF / 7,
    psu = paste0(CYCLE, "_", SDMVPSU),
    stra = paste0(CYCLE, "_", SDMVSTRA),
    wc_thr = ifelse(RIAGENDR == 1, 90, 80),
    htgw = as.numeric(BMXWAIST >= wc_thr & TG_mmol >= 1.69),
    pa_ter = cut(pa_mvpaw_min, quantile(pa_mvpaw_min, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                 include.lowest = TRUE, labels = c("L", "M", "H")),
    stroke_y = as.numeric(stroke)
  )
nhd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)
m1f <- stroke_y ~ htgw + RIDAGEYR + RIAGENDR
m2f <- stroke_y ~ htgw + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi
m3f <- stroke_y ~ htgw + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink +
  bmi + htn + dm + statin + bp_rx + pa_ter
for (tag in c("m1", "m2", "m3")) {
  fit <- svyglm(get(paste0(tag, "f")), family = quasibinomial(), design = nhd)
  e <- extr(fit)
  add_row("NHANES-cross-HTgw", toupper(tag), nrow(nhd$variables), sum(nhd$variables$stroke_y),
          e[1], e[2], e[3], e[4])
  logline(sprintf("NHANES-cross-HTgw %s: OR=%.3f (%.3f-%.3f) p=%.4f", toupper(tag),
                  e[1], e[2], e[3], e[4]))
}
logline(sprintf("NHANES HTGW prevalence weighted %.1f%%",
                100 * as.numeric(svymean(~htgw, nhd, na.rm = TRUE))))

# ---------------- CHARLS 2011 ----------------
logline("\n=== CHARLS 2011 cross (HTGW binary) ===")
c11 <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(
    wc_thr = ifelse(sex == 1, 90, 80),
    htgw = as.numeric(WC_cm >= wc_thr & TG_mmol >= 1.69),
    sex_m = ifelse(sex == 1, 1, 0),
    stroke_y = as.numeric(stroke_base),
    w_norm = bloodweight / mean(bloodweight, na.rm = TRUE)
  )
c11_m1 <- c11 %>% filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(age) &
                           !is.na(sex_m) & !is.na(WTI) & !is.na(stroke_y))
c11_m23 <- c11 %>% filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age) &
                            !is.na(WTI) & !is.na(stroke_y))
m2f <- stroke_y ~ htgw + age + sex_m + edu + smoke + drink + bmi
m3f <- stroke_y ~ htgw + age + sex_m + edu + smoke + drink + bmi + htn + dm +
  lipid_rx + bp_rx + pa_days_week
run3(c11_m1, c11_m23, "CHARLS2011-cross-HTgw")

# ---------------- CHARLS 2015 ----------------
logline("\n=== CHARLS 2015 cross (HTGW binary) ===")
c15 <- read_csv(file.path(OUT, "charls_2015_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(
    wc_thr = ifelse(sex == 1, 90, 80),
    htgw = as.numeric(WC_cm >= wc_thr & TG_mmol >= 1.69),
    sex_m = ifelse(sex == 1, 1, 0),
    stroke_y = as.numeric(stroke_base),
    w_norm = bloodweight / mean(bloodweight, na.rm = TRUE)
  )
# 2015 M1 mirrors the WTI 2015 M1 sample of 13e (design-eligible: positive
# weight, complete BMI/age; plus complete WTI [=WC+TG] and sex)
c15_m1 <- c15 %>% filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age) &
                           !is.na(sex_m) & !is.na(WTI) & !is.na(stroke_y))
c15_m23 <- c15 %>% filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age) &
                            !is.na(WTI) & !is.na(stroke_y))
m2f <- stroke_y ~ htgw + age + sex_m + edu + smoke + drink + bmi
m3f <- stroke_y ~ htgw + age + sex_m + edu + smoke + drink + bmi + htn + dm +
  lipid_rx + bp_rx + pa_days_week
run3(c15_m1, c15_m23, "CHARLS2015-cross-HTgw")

write_csv(bind_rows(rows), file.path(RES, "13r_htgw_models.csv"))
logline("\n=== 13r COMPLETE ===")
close(logf)
