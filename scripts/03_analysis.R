# ============================================================================
# 03_analysis.R
# Analysis : WTI x stroke - primary weighted models (cross-sectional both
#            cohorts + CHARLS 7-year prospective transition model)
# Date     : 2026-08-15 | v2 2026-08-20 (M1 on maximal model-wise samples)
# Seed     : 42
# R        : 4.6.1
# Packages : survey, dplyr, stringr, readr
# Note     : Cox + Fine-Gray + discrimination + mediation follow in 03b/04
# v2 change: CHARLS M1 (age+sex) fitted on the maximal sample per layer
#            (no BMI/covariate requirement); M2/M3 keep the covariate-
#            complete sample. WTI is standardized once per layer on the
#            full input file, so the per-1-SD unit is shared across M1-M3.
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
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov_v2.csv"), show_col_types = FALSE)
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
    pa_ter = cut(pa_mvpaw_min, quantile(pa_mvpaw_min, c(0,1/3,2/3,1), na.rm=TRUE),
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
  fit <- get(tag)
  e <- extr(fit, tag)
  nn <- length(residuals(fit)); ev <- sum(fit$y == 1, na.rm = TRUE)
  add_res("NHANES", "cross", toupper(tag), nn, ev, e[1], e[2], e[3], e[4])
  logline(sprintf("NHANES %s: OR=%.3f (%.3f-%.3f) p=%.4f", toupper(tag), e[1], e[2], e[3], e[4]))
}
m3t <- svyglm(stroke ~ WTI_ter + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink +
                bmi + htn + dm + statin + bp_rx + pa_ter,
              family = quasibinomial(), design = nhd)
tt <- coef(summary(m3t))[c("WTI_terT2","WTI_terT3"),]
logline(sprintf("NHANES tertile: T2 OR=%.3f p=%.3f | T3 OR=%.3f p=%.3f",
                exp(tt[1,1]), tt[1,4], exp(tt[2,1]), tt[2,4]))

# ---------------------------------------------------------------------------
# 2. CHARLS 2011 cross-sectional: M1 on maximal sample, M2/M3 on
#    covariate-complete sample
# ---------------------------------------------------------------------------
logline("\n=== STEP 2: CHARLS weighted logistic ===")
ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(
    WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
    WTI_ter = cut(WTI, quantile(WTI, c(0,1/3,2/3,1), na.rm=TRUE), include.lowest=TRUE,
                  labels = c("T1","T2","T3")),
    pa_ter = cut(pa_days_week, c(-1, 0, 1, 100), labels = c("0d","1-6d","7d")),
    sex_m = ifelse(sex == 1, 1, 0),   # rgender: 1=male
    w_norm = bloodweight / mean(bloodweight, na.rm = TRUE)   # normalized: avoids numeric blowup
  )
# M1 sample: maximal (exposure + outcome + age/sex + positive blood weight)
ch1 <- ch %>% filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(age) &
                       !is.na(sex_m) & !is.na(WTI) & !is.na(stroke_base))
chd1 <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm,
                  data = ch1, nest = TRUE)
logline(sprintf("CHARLS cross M1 design: n=%d, events=%d", nrow(ch1), sum(ch1$stroke_base)))
# M2/M3 sample: covariate-complete (as v1)
ch23 <- ch %>% filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age))
chd23 <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm,
                   data = ch23, nest = TRUE)
logline(sprintf("CHARLS cross M23 design: n=%d, events=%d", nrow(ch23), sum(ch23$stroke_base)))

cm1 <- svyglm(stroke_base ~ WTI_sd + age + sex_m, family = quasibinomial(), design = chd1)
cm2 <- svyglm(stroke_base ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi,
              family = quasibinomial(), design = chd23)
cm3 <- svyglm(stroke_base ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
                htn + dm + lipid_rx + bp_rx + pa_ter,
              family = quasibinomial(), design = chd23)
for (tag in c("cm1","cm2","cm3")) {
  fit <- get(tag)
  e <- extr(fit, tag)
  nn <- length(residuals(fit)); ev <- sum(fit$y == 1, na.rm = TRUE)
  add_res("CHARLS", "cross", tag, nn, ev, e[1], e[2], e[3], e[4])
  logline(sprintf("CHARLS %s: OR=%.3f (%.3f-%.3f) p=%.4f (n=%d, events=%d)",
                  tag, e[1], e[2], e[3], e[4], nn, ev))
}
cm3t <- svyglm(stroke_base ~ WTI_ter + age + sex_m + edu + smoke + drink + bmi +
                 htn + dm + lipid_rx + bp_rx + pa_ter,
               family = quasibinomial(), design = chd23)
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
  )
pr1 <- pr %>% filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(age) &
                       !is.na(sex_m) & !is.na(WTI) & !is.na(stroke_2018))
prd1 <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm,
                  data = pr1, nest = TRUE)
logline(sprintf("CHARLS prospective M1 design: n=%d, events=%d",
                nrow(pr1), sum(pr1$stroke_2018)))
pr23 <- pr %>% filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age))
prd23 <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm,
                   data = pr23, nest = TRUE)
logline(sprintf("CHARLS prospective M23 design: n=%d, events=%d",
                nrow(pr23), sum(pr23$stroke_2018)))

pm1 <- svyglm(stroke_2018 ~ WTI_sd + age + sex_m, family = quasibinomial(), design = prd1)
pm2 <- svyglm(stroke_2018 ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi,
              family = quasibinomial(), design = prd23)
pm3 <- svyglm(stroke_2018 ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
                htn + dm + lipid_rx + bp_rx + pa_ter,
              family = quasibinomial(), design = prd23)
for (tag in c("pm1","pm2","pm3")) {
  fit <- get(tag)
  e <- extr(fit, tag)
  nn <- length(residuals(fit)); ev <- sum(fit$y == 1, na.rm = TRUE)
  add_res("CHARLS", "prosp7y", tag, nn, ev, e[1], e[2], e[3], e[4])
  logline(sprintf("CHARLS-prosp %s: OR=%.3f (%.3f-%.3f) p=%.4f (n=%d, events=%d)",
                  tag, e[1], e[2], e[3], e[4], nn, ev))
}

# ---------------------------------------------------------------------------
# 4. Export
# ---------------------------------------------------------------------------
res <- bind_rows(res_rows)
write_csv(res, file.path(RES, "03_main_models.csv"))
logline("\n=== 03 PRIMARY MODELS COMPLETE ===")
close(logf)
