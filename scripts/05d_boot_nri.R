# ============================================================================
# 05d_boot_nri.R
# Analysis : continuous NRI + IDI with bootstrap percentile 95% CIs
#            (B = 1000, seed 42) for the 7-object comparison set, both cohorts
# Convention (same as 03b): WTI vs base model (age+sex); each of the other
#            6 objects vs WTI. Estimates identical to 03b point values; this
#            script adds bootstrap percentile CIs (03b kept SD only).
# Inputs   : data/processed/nhanes_fasting_cross_cov.csv + GLU_*/BMX_* XPT
#            data/processed/charls_2011_cross_cov.csv
# Output   : results/05d_boot_nri.csv ; results/05d_boot_nri_checks.txt
# Date     : 2026-08-15 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({
  library(pROC); library(dplyr); library(readr); library(haven)
})
set.seed(42)
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw")
OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "05d_boot_nri_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

nri_idi_boot <- function(p0, p1, y, B = 1000) {
  nri_cont <- function(pp0, pp1, yy) {
    dd <- pp1 - pp0
    up_ev <- mean(dd[yy == 1] > 0); dn_ev <- mean(dd[yy == 1] < 0)
    up_ne <- mean(dd[yy == 0] > 0); dn_ne <- mean(dd[yy == 0] < 0)
    (up_ev - dn_ev) + (dn_ne - up_ne)
  }
  idi_est <- (mean(p1[y == 1]) - mean(p0[y == 1])) - (mean(p1[y == 0]) - mean(p0[y == 0]))
  nri_b <- idi_b <- numeric(B)
  for (b in seq_len(B)) {
    ib <- sample(seq_along(y), replace = TRUE)
    nri_b[b] <- nri_cont(p0[ib], p1[ib], y[ib])
    idi_b[b] <- ((mean(p1[ib][y[ib] == 1]) - mean(p0[ib][y[ib] == 1])) -
                   (mean(p1[ib][y[ib] == 0]) - mean(p0[ib][y[ib] == 0])))
  }
  c(nri = nri_cont(p0, p1, y), idi = idi_est,
    nri_lo = unname(quantile(nri_b, 0.025)), nri_hi = unname(quantile(nri_b, 0.975)),
    idi_lo = unname(quantile(idi_b, 0.025)), idi_hi = unname(quantile(idi_b, 0.975)),
    nri_sd = sd(nri_b), idi_sd = sd(idi_b))
}

rows <- list()
add <- function(cohort, obj, v) {
  rows[[length(rows)+1]] <<- data.frame(cohort = cohort, object = obj,
    nri = v["nri"], nri_lo = v["nri_lo"], nri_hi = v["nri_hi"],
    idi = v["idi"], idi_lo = v["idi_lo"], idi_hi = v["idi_hi"],
    nri_sd = v["nri_sd"], idi_sd = v["idi_sd"])
  logline(sprintf("%s %-5s NRI=%.3f (%.3f, %.3f) | IDI=%.5f (%.5f, %.5f)",
                  cohort, obj, v["nri"], v["nri_lo"], v["nri_hi"],
                  v["idi"], v["idi_lo"], v["idi_hi"]))
}

run_boot <- function(dat, yvar, cohort, covars = "age + sex") {
  objs <- c("WTI", "WC", "TG", "TyG", "TyGWC", "ABSI", "HTGW")
  f0 <- glm(as.formula(paste(yvar, "~", covars)), data = dat, family = binomial(),
            na.action = na.exclude)
  p0 <- predict(f0, type = "response")
  for (obj in objs) {
    f1 <- glm(as.formula(paste(yvar, "~", obj, "+", covars)), data = dat, family = binomial(),
              na.action = na.exclude)
    p1 <- predict(f1, type = "response")
    ok <- !is.na(p1) & !is.na(p0) & !is.na(dat[[yvar]])
    yok <- dat[[yvar]][ok]; p0k <- p0[ok]; p1k <- p1[ok]
    if (obj == "WTI") {
      v <- nri_idi_boot(p0k, p1k, yok)
    } else {
      fw <- glm(as.formula(paste(yvar, "~ WTI +", covars)), data = dat, family = binomial(),
                na.action = na.exclude)
      pw <- predict(fw, type = "response")[ok]
      v <- nri_idi_boot(pw, p1k, yok)
    }
    add(cohort, obj, v)
  }
}

logline("=== NHANES 7-object bootstrap NRI/IDI ===")
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov.csv"), show_col_types = FALSE)
glu <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  read_xpt(file.path(NRAW, sprintf("GLU_%s.XPT", cy))) %>%
    transmute(SEQN, LBXGLU = as.numeric(LBXGLU), CYCLE = cy)
}) %>% bind_rows()
bmx <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  read_xpt(file.path(NRAW, sprintf("BMX_%s.XPT", cy))) %>%
    transmute(SEQN, BMXHT = as.numeric(BMXHT), CYCLE = cy)
}) %>% bind_rows()
nh <- nh %>%
  left_join(glu, by = c("SEQN", "CYCLE.x" = "CYCLE")) %>%
  left_join(bmx, by = c("SEQN", "CYCLE.x" = "CYCLE")) %>%
  mutate(
    FPG_mmol = LBXGLU * 0.0555,
    TyG   = log(TG_mmol * FPG_mmol / 2),
    TyGWC = TyG * BMXWAIST,
    WC    = as.numeric(BMXWAIST),
    TG    = TG_mmol,
    ABSI  = (WC / 100) / (bmi^(2/3) * (BMXHT / 100)^0.5),
    HTGW  = ifelse((RIAGENDR == 1 & WC >= 90) | (RIAGENDR == 2 & WC >= 80), TG >= 1.69, FALSE)
  ) %>%
  filter(!is.na(TyG) & !is.na(ABSI))
run_boot(nh, "stroke", "NHANES", covars = "RIDAGEYR + RIAGENDR")

logline("\n=== CHARLS 7-object bootstrap NRI/IDI ===")
ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(
    FPG_mmol = newglu * 0.0555,
    TyG   = log(TG_mmol * FPG_mmol / 2),
    TyGWC = TyG * WC_cm,
    WC    = WC_cm, TG = TG_mmol,
    ABSI  = (WC_cm / 100) / (bmi^(2/3) * (ht_cm / 100)^0.5),
    HTGW  = ifelse((sex == 1 & WC_cm >= 90) | (sex == 2 & WC_cm >= 80), TG_mmol >= 1.69, FALSE),
    age = as.numeric(age), sex = as.numeric(sex)
  ) %>%
  filter(!is.na(TyG) & !is.na(ABSI) & !is.na(bmi) & !is.na(age))
run_boot(ch, "stroke_base", "CHARLS", covars = "age + sex")

write_csv(bind_rows(rows), file.path(RES, "05d_boot_nri.csv"))
logline("\n=== 05d BOOT NRI COMPLETE ===")
close(logf)
