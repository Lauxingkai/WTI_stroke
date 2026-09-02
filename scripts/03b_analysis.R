# ============================================================================
# 03b_analysis.R
# Analysis : WTI x stroke - discrimination (7 objects) + RCS dose-response
# Date     : 2026-08-15 | Seed: 42
# R        : 4.6.1 | Packages: pROC, splines, survey, dplyr, readr, haven
# Note     : Cox + Fine-Gray follow in 03c (needs wave-level event-time prep)
# ============================================================================
suppressPackageStartupMessages({
  library(pROC); library(splines); library(survey); library(haven)
  library(dplyr); library(readr)
})
set.seed(42)
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw")
OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "03b_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

disc_rows <- list()
add_disc <- function(cohort, obj, auc, lo, hi, dauc_vs_wti, delong_p, nri, idi,
                     dauc_vs_base = NA_real_, delong_p_base = NA_real_) {
  disc_rows[[length(disc_rows)+1]] <<- data.frame(
    cohort, object = obj, auc, lo, hi, dauc_vs_wti, delong_p, nri, idi,
    dauc_vs_base, delong_p_base)
}

nri_idi <- function(p0, p1, y, B = 500) {
  nri_cont <- function(pp0, pp1, yy) {
    d <- pp1 - pp0
    up_ev   <- mean(d[yy == 1] > 0); dn_ev <- mean(d[yy == 1] < 0)
    up_ne   <- mean(d[yy == 0] > 0); dn_ne <- mean(d[yy == 0] < 0)
    (up_ev - dn_ev) + (dn_ne - up_ne)
  }
  idi_est <- (mean(p1[y==1]) - mean(p0[y==1])) - (mean(p1[y==0]) - mean(p0[y==0]))
  nri_b <- idi_b <- numeric(B)
  for (b in seq_len(B)) {
    ib <- sample(seq_along(y), replace = TRUE)
    nri_b[b] <- nri_cont(p0[ib], p1[ib], y[ib])
    idi_b[b] <- ((mean(p1[ib][y[ib]==1]) - mean(p0[ib][y[ib]==1])) -
                   (mean(p1[ib][y[ib]==0]) - mean(p0[ib][y[ib]==0])))
  }
  c(nri = nri_cont(p0, p1, y), idi = idi_est,
    nri_sd = sd(nri_b), idi_sd = sd(idi_b))
}

run_disc <- function(dat, yvar, cohort, covars = "age + sex") {
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
    roc1 <- roc(yok, p1k, quiet = TRUE)
    a1 <- as.numeric(auc(roc1))
    ci1 <- as.numeric(ci.auc(roc1))
    if (obj == "WTI") {
      rt <- roc.test(roc(yok, p0k, quiet=TRUE), roc1, method = "delong")
      dp <- rt$p.value; da <- a1 - as.numeric(auc(roc(yok, p0k, quiet=TRUE)))
      ni <- nri_idi(p0k, p1k, yok)
      add_disc(cohort, obj, a1, ci1[1], ci1[3], da, dp, ni["nri"], ni["idi"],
               dauc_vs_base = da, delong_p_base = dp)
    } else {
      fw <- glm(as.formula(paste(yvar, "~ WTI +", covars)), data = dat, family = binomial(),
                na.action = na.exclude)
      pw <- predict(fw, type = "response")[ok]
      rt <- roc.test(roc(yok, pw, quiet=TRUE), roc1, method = "delong")
      dp <- rt$p.value; da <- a1 - as.numeric(auc(roc(yok, pw, quiet=TRUE)))
      ni <- nri_idi(pw, p1k, yok)
      # vs base (age+sex) comparison, for every index
      rtb <- roc.test(roc(yok, p0k, quiet=TRUE), roc1, method = "delong")
      da_b <- a1 - as.numeric(auc(roc(yok, p0k, quiet=TRUE)))
      add_disc(cohort, obj, a1, ci1[1], ci1[3], da, dp, ni["nri"], ni["idi"],
               dauc_vs_base = da_b, delong_p_base = rtb$p.value)
    }
    logline(sprintf("%s %-5s AUC=%.3f (%.3f-%.3f) dAUC=%.3f p=%.3f NRI=%.3f IDI=%.3f",
                    cohort, obj, a1, ci1[1], ci1[3], da, dp, ni["nri"], ni["idi"]))
  }
}

# ---------------------------------------------------------------------------
# 1. NHANES: add fasting glucose (GLU) + height (BMX) -> 7 objects
# ---------------------------------------------------------------------------
logline("=== STEP 1: NHANES 7-object discrimination ===")
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov_v2.csv"), show_col_types = FALSE)
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
    TG_mmol2 = TG_mmol,
    TyG   = log(TG_mmol2 * FPG_mmol / 2),
    TyGWC = TyG * BMXWAIST,
    WC    = as.numeric(BMXWAIST),
    TG    = TG_mmol2,
    ABSI  = (WC / 100) / (bmi^(2/3) * (BMXHT / 100)^0.5),
    HTGW  = ifelse((RIAGENDR == 1 & WC >= 90) | (RIAGENDR == 2 & WC >= 80), TG >= 1.69, FALSE)
  ) %>%
  filter(!is.na(TyG) & !is.na(ABSI))
run_disc(nh, "stroke", "NHANES", covars = "RIDAGEYR + RIAGENDR")

# design variables (rebuilt: DEMO masked PSU/strata)
des <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy))) %>%
    transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
}) %>% bind_rows()
nh <- nh %>%
  left_join(des, by = c("SEQN", "CYCLE.x" = "CYCLE")) %>%
  mutate(wt = WTSAF / 7,
         psu = paste0(CYCLE.y, "_", SDMVPSU),
         stra = paste0(CYCLE.y, "_", SDMVSTRA))

# ---------------------------------------------------------------------------
# 2. CHARLS cross-sectional: 7 objects (newglu already in covariates)
# ---------------------------------------------------------------------------
logline("\n=== STEP 2: CHARLS 7-object discrimination ===")
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
run_disc(ch, "stroke_base", "CHARLS", covars = "age + sex")

# ---------------------------------------------------------------------------
# 3. RCS dose-response (weighted, M2 covariates, splines::ns df=3)
# ---------------------------------------------------------------------------
logline("\n=== STEP 3: RCS (ns df=3) nonlinearity ===")
rcs_test <- function(fit_lin, fit_ns, tag) {
  b <- coef(fit_ns); V <- vcov(fit_ns)
  idx <- grep("^ns", names(b))
  wald <- as.numeric(t(b[idx]) %*% solve(V[idx, idx]) %*% b[idx])
  p <- pchisq(wald, df = length(idx) - 1, lower.tail = FALSE)
  logline(sprintf("%s: nonlinear Wald chi2=%.2f df=%d p=%.4f", tag, wald, length(idx)-1, p))
  data.frame(layer = tag, chi2 = wald, df = length(idx)-1, p = p)
}
rcs_out <- list()
# NHANES
nhn <- nh %>% mutate(WTI2 = WTI)
nd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nhn, nest = TRUE)
l0 <- svyglm(stroke ~ WTI + RIDAGEYR + RIAGENDR + edu + smoke + drink + bmi,
             family = quasibinomial(), design = nd)
l1 <- svyglm(stroke ~ ns(WTI, 3) + RIDAGEYR + RIAGENDR + edu + smoke + drink + bmi,
             family = quasibinomial(), design = nd)
rcs_out[[1]] <- rcs_test(l0, l1, "NHANES-cross")
# CHARLS cross
ch2 <- ch %>% mutate(w_norm = bloodweight / mean(bloodweight, na.rm=TRUE))
cd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm, data = ch2, nest = TRUE)
c0 <- svyglm(stroke_base ~ WTI + age + sex + edu + smoke + drink + bmi,
             family = quasibinomial(), design = cd)
c1 <- svyglm(stroke_base ~ ns(WTI, 3) + age + sex + edu + smoke + drink + bmi,
             family = quasibinomial(), design = cd)
rcs_out[[2]] <- rcs_test(c0, c1, "CHARLS-cross")
# CHARLS prospective
pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE) %>%
  mutate(w_norm = bloodweight / mean(bloodweight, na.rm=TRUE), age = as.numeric(age), sex = as.numeric(sex)) %>%
  filter(!is.na(bmi) & !is.na(age))
pd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm, data = pr, nest = TRUE)
p0 <- svyglm(stroke_2018 ~ WTI + age + sex + edu + smoke + drink + bmi,
             family = quasibinomial(), design = pd)
p1 <- svyglm(stroke_2018 ~ ns(WTI, 3) + age + sex + edu + smoke + drink + bmi,
             family = quasibinomial(), design = pd)
rcs_out[[3]] <- rcs_test(p0, p1, "CHARLS-prosp")

# ---------------------------------------------------------------------------
# 4. Export
# ---------------------------------------------------------------------------
write_csv(bind_rows(disc_rows), file.path(RES, "03b_discrimination.csv"))
write_csv(bind_rows(rcs_out), file.path(RES, "03b_rcs_p.csv"))
logline("\n=== 03b COMPLETE ===")
close(logf)
