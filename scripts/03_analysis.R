# ============================================================================
# 03_analysis.R
# Analysis : WTI x stroke - primary weighted models (cross-sectional both
#            cohorts + CHARLS 7-year prospective transition model)
# Date     : 2026-08-15
# Seed     : 42
# R        : 4.6.1
# Packages : survey, dplyr, stringr, readr
# Note     : Cox + Fine-Gray + discrimination + mediation follow in 03b/04
# ============================================================================

suppressPackageStartupMessages({
  library(survey); library(haven); library(dplyr); library(stringr); library(readr)
})
set.seed(42)

RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw")
OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "03_analysis_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

res_rows <- list()
add_res <- function(cohort, layer, model, n, events, est, lo, hi, p) {
  res_rows[[length(res_rows)+1]] <<- data.frame(
    cohort, layer, model, n, events, est, lo, hi, p)
}

# ---------------------------------------------------------------------------
# 1. NHANES fasting subsample: survey design + 3 models
# ---------------------------------------------------------------------------
logline("=== STEP 1: NHANES weighted logistic ===")
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov.csv"), show_col_types = FALSE)
# masked design variables from DEMO files
des <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  demo <- read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy)))
  demo %>% transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
})
des <- bind_rows(des)
nh <- nh %>% left_join(des, by = c("SEQN" = "SEQN", "CYCLE.x" = "CYCLE")) %>%
  rename(CYCLE = CYCLE.x) %>%
  mutate(
    wt = WTSAF / 7,                                   # pooled 7-cycle fasting weight
    psu  = paste0(CYCLE, "_", SDMVPSU),               # cycle-specific masked PSU
    stra = paste0(CYCLE, "_", SDMVSTRA),
    WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
    WTI_ter = cut(WTI, quantile(WTI, c(0,1/3,2/3,1), na.rm=TRUE), include.lowest=TRUE,
                  labels = c("T1","T2","T3")),
    pa_ter = cut(pa_min_day, quantile(pa_min_day, c(0,1/3,2/3,1), na.rm=TRUE),
                 include.lowest=TRUE, labels = c("L","M","H"))
  )
nhd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)
logline(sprintf("NHANES design: n=%d, events=%d", nrow(nh), sum(nh$stroke)))

m1 <- svyglm(stroke ~ WTI_sd + RIDAGEYR + RIAGENDR, family = quasibinomial(), design = nhd)
m2 <- svyglm(stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi,
             family = quasibinomial(), design = nhd)
m3 <- svyglm(stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink +
               bmi + htn + dm + statin + bp_rx + pa_ter,
             family = quasibinomial(), design = nhd)
extr <- function(fit, tag) {
  b <- coef(fit)["WTI_sd"]; se <- sqrt(vcov(fit)["WTI_sd", "WTI_sd"])
  c(est = exp(b), lo = exp(b - 1.96*se), hi = exp(b + 1.96*se), p = 2*pnorm(-abs(b/se)))
}
for (tag in c("m1","m2","m3")) {
  e <- extr(get(tag), tag)
  add_res("NHANES", "cross", toupper(tag), nrow(nh), sum(nh$stroke), e[1], e[2], e[3], e[4])
  logline(sprintf("NHANES %s: OR=%.3f (%.3f-%.3f) p=%.4f", toupper(tag), e[1], e[2], e[3], e[4]))
}
m3t <- svyglm(stroke ~ WTI_ter + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink +
                bmi + htn + dm + statin + bp_rx + pa_ter,
              family = quasibinomial(), design = nhd)
tt <- coef(summary(m3t))[c("WTI_terT2","WTI_terT3"),]
logline(sprintf("NHANES tertile: T2 OR=%.3f p=%.3f | T3 OR=%.3f p=%.3f",
                exp(tt[1,1]), tt[1,4], exp(tt[2,1]), tt[2,4]))

# ---------------------------------------------------------------------------
# 2. CHARLS 2011 cross-sectional: survey design + 3 models
# ---------------------------------------------------------------------------
logline("\n=== STEP 2: CHARLS weighted logistic ===")
ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(
    WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
    WTI_ter = cut(WTI, quantile(WTI, c(0,1/3,2/3,1), na.rm=TRUE), include.lowest=TRUE,
                  labels = c("T1","T2","T3")),
    pa_ter = cut(pa_days_week, c(-1, 0, 1, 100), labels = c("0d","1-6d","7d")),
    sex_m = ifelse(sex == 1, 1, 0),   # [VERIFY] rgender: 1=male
    w_norm = bloodweight / mean(bloodweight, na.rm = TRUE)   # normalized: avoids numeric blowup
  ) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age))
chd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm,
                 data = ch, nest = TRUE)
logline(sprintf("CHARLS cross design: n=%d, events=%d", nrow(ch), sum(ch$stroke_base)))

cm1 <- svyglm(stroke_base ~ WTI_sd + age + sex_m, family = quasibinomial(), design = chd)
cm2 <- svyglm(stroke_base ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi,
              family = quasibinomial(), design = chd)
cm3 <- svyglm(stroke_base ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
                htn + dm + lipid_rx + bp_rx + pa_ter,
              family = quasibinomial(), design = chd)
for (tag in c("cm1","cm2","cm3")) {
  e <- extr(get(tag), tag)
  add_res("CHARLS", "cross", tag, nrow(ch), sum(ch$stroke_base), e[1], e[2], e[3], e[4])
  logline(sprintf("CHARLS %s: OR=%.3f (%.3f-%.3f) p=%.4f", tag, e[1], e[2], e[3], e[4]))
}
cm3t <- svyglm(stroke_base ~ WTI_ter + age + sex_m + edu + smoke + drink + bmi +
                 htn + dm + lipid_rx + bp_rx + pa_ter,
               family = quasibinomial(), design = chd)
tt2 <- coef(summary(cm3t))[c("WTI_terT2","WTI_terT3"),]
logline(sprintf("CHARLS tertile: T2 OR=%.3f p=%.3f | T3 OR=%.3f p=%.3f",
                exp(tt2[1,1]), tt2[1,4], exp(tt2[2,1]), tt2[2,4]))

# ---------------------------------------------------------------------------
# 3. CHARLS prospective: 7-year weighted model (transition; Cox/Fine-Gray in 03b)
# ---------------------------------------------------------------------------
logline("\n=== STEP 3: CHARLS prospective 7-year weighted model ===")
pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE) %>%
  mutate(
    WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
    WTI_ter = cut(WTI, quantile(WTI, c(0,1/3,2/3,1), na.rm=TRUE), include.lowest=TRUE,
                  labels = c("T1","T2","T3")),
    pa_ter = cut(pa_days_week, c(-1, 0, 1, 100), labels = c("0d","1-6d","7d")),
    sex_m = ifelse(sex == 1, 1, 0),
    w_norm = bloodweight / mean(bloodweight, na.rm = TRUE)
  ) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age))
prd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm,
                 data = pr, nest = TRUE)
logline(sprintf("CHARLS prospective design: n=%d, events=%d", nrow(pr), sum(pr$stroke_2018)))

pm1 <- svyglm(stroke_2018 ~ WTI_sd + age + sex_m, family = quasibinomial(), design = prd)
pm2 <- svyglm(stroke_2018 ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi,
              family = quasibinomial(), design = prd)
pm3 <- svyglm(stroke_2018 ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
                htn + dm + lipid_rx + bp_rx + pa_ter,
              family = quasibinomial(), design = prd)
for (tag in c("pm1","pm2","pm3")) {
  e <- extr(get(tag), tag)
  add_res("CHARLS", "prosp7y", tag, nrow(pr), sum(pr$stroke_2018), e[1], e[2], e[3], e[4])
  logline(sprintf("CHARLS-prosp %s: OR=%.3f (%.3f-%.3f) p=%.4f", tag, e[1], e[2], e[3], e[4]))
}

# ---------------------------------------------------------------------------
# 4. Export
# ---------------------------------------------------------------------------
res <- bind_rows(res_rows)
write_csv(res, file.path(RES, "03_main_models.csv"))
logline("\n=== 03 PRIMARY MODELS COMPLETE ===")
close(logf)

