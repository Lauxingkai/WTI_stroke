# ============================================================================
# 05e_mediation.R
# Analysis : CHARLS temporal mediation - 2011 WTI -> 2015 mediator -> 2018
#            incident stroke (7-year window). Product-of-coefficients (a*b)
#            with bootstrap percentile CIs (B = 1000, individual resampling,
#            survey-weighted refit each draw). Mediators: ln(CRP), eGFR
#            (CKD-EPI 2021 creatinine-only, mg/dL).
# Temporal purification: exclude stroke reported at 2013 wave and death
#            before 2015 (mediator measurement).
# Four identification assumptions (VanderWeele 2016, PMID 26653405) are
#            stated in the manuscript; cross-sectional caution per protocol.
# Inputs   : charls_2011_2018_prosp_cov.csv ; charls_events_2011_2018.csv ;
#            charls_2015_mediator.csv (from 05e_prep_mediator.py)
# Output   : results/05e_mediation.csv ; results/05e_mediation_checks.txt
# Date     : 2026-08-15 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({library(survey); library(dplyr); library(readr)})
set.seed(42)
options(survey.lonely.psu = "adjust")   # bootstrap subsamples may hold 1 PSU per stratum
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "05e_mediation_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)
md <- read_csv(file.path(OUT, "charls_2015_mediator.csv"), show_col_types = FALSE,
               col_types = cols(ID_12 = col_character()))

d <- pr %>%
  left_join(ev, by = "ID_12") %>%
  left_join(md, by = "ID_12") %>%
  mutate(
    WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
    sex_m = ifelse(sex == 1, 1, 0),
    sex_f = ifelse(sex == 2, 1, 0),
    w = bloodweight / mean(bloodweight, na.rm=TRUE),
    age = as.numeric(age),
    # eGFR CKD-EPI 2021 creatinine-only (Scr mg/dL) [VERIFY unit = mg/dL]
    kappa = ifelse(sex == 2, 0.7, 0.9),
    alpha = ifelse(sex == 2, -0.241, -0.302),
    eGFR = 142 * pmin(crea_2015 / kappa, 1)^alpha *
      pmax(crea_2015 / kappa, 1)^(-1.200) * 0.9938^age * 1.012^sex_f,
    lnCRP = log(crp_2015),
    Y = as.integer(stk15 | stk18)
  ) %>%
  filter(!(stk13 %in% TRUE) & (is.na(death_t) | death_t >= 4)) %>%
  filter(!is.na(WTI_sd) & !is.na(age) & !is.na(bmi))

C <- "age + sex_m + edu + smoke + drink + bmi + htn + dm + lipid_rx + bp_rx + pa_days_week"

run_med <- function(Mvar, tag) {
  dd <- d %>% filter(!is.na(.data[[Mvar]]) & !is.na(Y))
  logline(sprintf("%s: n=%d events=%d", tag, nrow(dd), sum(dd$Y)))
  des <- svydesign(ids = ~communityID, strata = ~urban_nbs,
                   weights = ~w, data = dd, nest = TRUE)
  fM <- as.formula(paste(Mvar, "~ WTI_sd +", C))
  fY <- as.formula(paste("Y ~ WTI_sd +", Mvar, "+", C))
  est_once <- function(idx = NULL) {
    if (is.null(idx)) idx <- seq_len(nrow(dd))
    desb <- svydesign(ids = ~communityID, strata = ~urban_nbs,
                      weights = ~w, data = dd[idx, ], nest = TRUE)
    mM <- svyglm(fM, design = desb)
    mY <- svyglm(fY, family = quasibinomial(), design = desb)
    a <- coef(mM)["WTI_sd"]
    b <- coef(mY)[Mvar]
    cprime <- coef(mY)["WTI_sd"]
    c(indirect = a * b, direct = cprime, total = a * b + cprime)
  }
  pt <- est_once()
  names(pt) <- c("indirect", "direct", "total")
  B <- 1000
  boot <- matrix(NA_real_, B, 3)
  n <- nrow(dd)
  for (b in seq_len(B)) {
    idx <- sample(n, replace = TRUE)
    boot[b, ] <- tryCatch(est_once(idx), error = function(e) rep(NA_real_, 3))
  }
  ok <- complete.cases(boot)
  q <- function(v) {
    v <- v[is.finite(v)]
    if (length(v) < 2) return(c(NA_real_, NA_real_))
    unname(quantile(v, c(0.025, 0.975)))
  }
  logline(sprintf("%s: bootstrap draws failed: %d / %d", tag, B - sum(ok), B))
  out <- data.frame(
    mediator = tag, n = n, events = sum(dd$Y),
    indirect = pt[["indirect"]], indirect_lo = q(boot[ok, 1])[1],
    indirect_hi = q(boot[ok, 1])[2],
    direct = pt[["direct"]], direct_lo = q(boot[ok, 2])[1],
    direct_hi = q(boot[ok, 2])[2],
    total = pt[["total"]], total_lo = q(boot[ok, 3])[1],
    total_hi = q(boot[ok, 3])[2],
    prop = pt[["indirect"]] / pt[["total"]],
    prop_lo = q(boot[ok, 1] / boot[ok, 3])[1],
    prop_hi = q(boot[ok, 1] / boot[ok, 3])[2])
  logline(sprintf("%s: a*b=%.4f (%.4f, %.4f) | c'=%.4f | total=%.4f | prop=%.1f%% (%.1f, %.1f)",
                  tag, out$indirect, out$indirect_lo, out$indirect_hi,
                  out$direct, out$total,
                  100 * out$prop, 100 * out$prop_lo, 100 * out$prop_hi))
  out
}

rows <- bind_rows(run_med("lnCRP", "lnCRP"), run_med("eGFR", "eGFR"))
write_csv(rows, file.path(RES, "05e_mediation.csv"))
logline("\n=== 05e MEDIATION COMPLETE ===")
close(logf)
