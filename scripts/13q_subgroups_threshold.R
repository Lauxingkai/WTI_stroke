# ============================================================================
# 13q_subgroups_threshold.R
# A+B+C fixes (2026-08-18, cross-check report follow-up):
#   C2. Smoking / drinking / diabetes subgroup interaction tests for WTI-stroke:
#       CHARLS 2011 cross M1, CHARLS 2015 cross M1, CHARLS 2011 prospective Cox M1
#   C1. Two-piecewise threshold analysis for the CHARLS prospective wave-first
#       Cox model (M2-adjusted, matching the Figure 2 spline adjustment set),
#       with bootstrap (community-resampling) CI for the turning point.
# Outputs: results/13q_subgroups.csv ; results/13q_threshold.csv ;
#          results/13q_checks.txt
# Note: exploratory, data-driven threshold - not externally validated.
# ============================================================================
suppressPackageStartupMessages({
  library(survival); library(cmprsk); library(survey); library(dplyr); library(readr)
})
set.seed(42)
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")
logf <- file(file.path(RES, "13q_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }
fmt <- function(x) sprintf("%.3f", x)
pval <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

# ---------------- data ----------------
c11 <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE)
c15 <- read_csv(file.path(OUT, "charls_2015_cross_cov.csv"), show_col_types = FALSE)
pr  <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev  <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)

# ---------------- helpers ----------------
subgroup_logistic <- function(d, yvar, label, design_vars = TRUE) {
  d <- d %>% mutate(WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
                    sex_m = as.numeric(sex) == 1) %>%
    filter(!is.na(WTI) & !is.na(age) & !is.na(sex_m) & !is.na(bloodweight) &
             !is.na(communityID) & !is.na(urban_nbs) & !is.na(.data[[yvar]]))
  rows <- list()
  for (g in c("smoke", "drink", "dm")) {
    dd <- d %>% filter(!is.na(.data[[g]]) & .data[[g]] %in% c(0, 1)) %>%
      mutate(grp = .data[[g]],
             wnorm = bloodweight / mean(bloodweight, na.rm = TRUE))
    dsn <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~wnorm, data = dd)
    # within-stratum M1
    for (s in c(0, 1)) {
      sub <- subset(dsn, grp == s)
      f <- if (yvar == "stroke_base") {
        svyglm(stroke_base ~ WTI_sd + age + sex_m, family = quasibinomial(), design = sub)
      } else {
        svyglm(stroke_2018 ~ WTI_sd + age + sex_m, family = quasibinomial(), design = sub)
      }
      cf <- summary(f)$coefficients
      w <- cf[grepl("WTI_sd", rownames(cf)), , drop = FALSE][1, ]
      rows[[length(rows) + 1]] <- data.frame(
        layer = label, var = g, stratum = s, n = nrow(sub$variables),
        est = as.numeric(w["Estimate"]), se = as.numeric(w["Std. Error"]),
        p = as.numeric(w["Pr(>|t|)"]))
    }
    # interaction model
    if (yvar == "stroke_base") {
      fi <- svyglm(stroke_base ~ WTI_sd * grp + age + sex_m, family = quasibinomial(), design = dsn)
    } else {
      fi <- svyglm(stroke_2018 ~ WTI_sd * grp + age + sex_m, family = quasibinomial(), design = dsn)
    }
    cfi <- summary(fi)$coefficients
    wi <- cfi[grepl("WTI_sd:grp", rownames(cfi)), , drop = FALSE][1, ]
    rows[[length(rows) + 1]] <- data.frame(
      layer = label, var = g, stratum = NA, n = nrow(dsn$variables),
      est = as.numeric(wi["Estimate"]), se = as.numeric(wi["Std. Error"]),
      p = as.numeric(wi["Pr(>|t|)"]), interaction = TRUE)
  }
  bind_rows(rows)
}

subgroup_cox <- function(d, label) {
  d <- d %>% left_join(ev %>% select(ID_12, stroke, time, death), by = "ID_12") %>%
    mutate(WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
           sex_m = as.numeric(sex) == 1,
           ftime = pmin(time, 7.0),
           w = bloodweight / mean(bloodweight, na.rm = TRUE)) %>%
    filter(!is.na(WTI_sd) & !is.na(age) & !is.na(stroke) & !is.na(ftime) &
             !is.na(sex_m) & !is.na(bloodweight) & !is.na(communityID))
  rows <- list()
  for (g in c("smoke", "drink", "dm")) {
    dd <- d %>% filter(!is.na(.data[[g]]) & .data[[g]] %in% c(0, 1)) %>%
      mutate(grp = .data[[g]])
    for (s in c(0, 1)) {
      sub <- dd %>% filter(.data[[g]] == s)
      f <- coxph(Surv(ftime, stroke) ~ WTI_sd + age + sex_m,
                 data = sub, weights = w, cluster = communityID)
      sm <- summary(f)$coefficients
      w <- sm[grepl("WTI_sd", rownames(sm)), , drop = FALSE][1, ]
      rows[[length(rows) + 1]] <- data.frame(
        layer = label, var = g, stratum = s, n = nrow(sub),
        est = log(as.numeric(w["exp(coef)"])), se = as.numeric(w["robust se"]),
        p = as.numeric(w["Pr(>|z|)"]))
    }
    fi <- coxph(Surv(ftime, stroke) ~ WTI_sd * grp + age + sex_m,
                data = dd, weights = w, cluster = communityID)
    smi <- summary(fi)$coefficients
    wi <- smi[grepl("WTI_sd:", rownames(smi)), , drop = FALSE][1, ]
    rows[[length(rows) + 1]] <- data.frame(
      layer = label, var = g, stratum = NA, n = nrow(dd),
      est = log(as.numeric(wi["exp(coef)"])), se = as.numeric(wi["robust se"]),
      p = as.numeric(wi["Pr(>|z|)"]), interaction = TRUE)
  }
  bind_rows(rows)
}

sub <- bind_rows(
  subgroup_logistic(c11, "stroke_base", "CHARLS-2011-cross"),
  subgroup_logistic(c15, "stroke_base", "CHARLS-2015-cross"),
  subgroup_cox(pr, "CHARLS-2011-prosp")
) %>% mutate(
  OR_HR = exp(est),
  lo = exp(est - 1.96 * se),
  hi = exp(est + 1.96 * se),
  p = pmin(p, 1))
write_csv(sub, file.path(RES, "13q_subgroups.csv"))
logline("=== 13q subgroups written ===")
logline(paste(capture.output(print(as.data.frame(
  sub %>% select(layer, var, stratum, n, OR_HR, lo, hi, p) %>%
    mutate(across(where(is.numeric), ~round(., 3)))), row.names = FALSE)), collapse = "\n"))

# ---------------- two-piecewise threshold (CHARLS prospective, wave-first) ----------------
d <- pr %>% left_join(ev %>% select(ID_12, stroke, time, death), by = "ID_12") %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
         sex_m = as.numeric(sex) == 1,
         ftime = pmin(time, 7.0),
         w = bloodweight / mean(bloodweight, na.rm = TRUE)) %>%
  filter(!is.na(WTI) & !is.na(age) & !is.na(sex_m) & !is.na(edu) & !is.na(smoke) &
           !is.na(drink) & !is.na(bmi) & !is.na(stroke) & !is.na(ftime) &
           !is.na(bloodweight) & !is.na(communityID))
logline(sprintf("threshold grid cohort: n=%d | stroke %d | death %d",
                nrow(d), sum(d$stroke), sum(d$death)))

base_form <- Surv(ftime, stroke) ~ WTI + age + sex_m + edu + smoke + drink + bmi
lin <- coxph(base_form, data = d, weights = w, cluster = communityID)
grid_k <- quantile(d$WTI, probs = seq(0.10, 0.90, 0.01))
lls <- sapply(grid_k, function(k) {
  dd <- d %>% mutate(WTI1 = pmin(WTI, k), WTI2 = pmax(WTI - k, 0))
  f <- coxph(Surv(ftime, stroke) ~ WTI1 + WTI2 + age + sex_m + edu + smoke + drink + bmi,
             data = dd, weights = w, cluster = communityID)
  as.numeric(logLik(f))
})
k_star <- grid_k[which.max(lls)]
lrt <- 2 * (max(lls) - as.numeric(logLik(lin)))
lrt_p <- pchisq(lrt, df = 1, lower.tail = FALSE)
logline(sprintf("turning point k* = %.1f | LRT chi2=%.2f p=%.4f", k_star, lrt, lrt_p))

# piecewise model at k* (per 1-SD below/above via segment SDs)
dd <- d %>% mutate(WTI1 = pmin(WTI, k_star), WTI2 = pmax(WTI - k_star, 0))
f_star <- coxph(Surv(ftime, stroke) ~ WTI1 + WTI2 + age + sex_m + edu + smoke + drink + bmi,
                data = dd, weights = w, cluster = communityID)
sm <- summary(f_star)$coefficients
ex2 <- function(rn) {
  r <- sm[grepl(rn, rownames(sm)), , drop = FALSE][1, ]
  c(coef = as.numeric(r["coef"]), se = as.numeric(r["robust se"]))
}
b1 <- ex2("WTI1"); b2 <- ex2("WTI2")
sd_full <- sd(d$WTI); sd_lo <- sd(pmin(d$WTI, k_star)); sd_hi <- sd(pmax(d$WTI - k_star, 0))
# per-10-unit effects (common absolute scale, avoids the segment-SD caveat)
hr10_lo <- exp(b1["coef"] * 10); hr10_lo_lo <- exp((b1["coef"] - 1.96 * b1["se"]) * 10)
hr10_lo_hi <- exp((b1["coef"] + 1.96 * b1["se"]) * 10)
hr10_hi <- exp(b2["coef"] * 10); hr10_hi_lo <- exp((b2["coef"] - 1.96 * b2["se"]) * 10)
hr10_hi_hi <- exp((b2["coef"] + 1.96 * b2["se"]) * 10)
res_k <- data.frame(
  k = k_star, lrt = lrt, lrt_p = lrt_p, n = nrow(d), events = sum(d$stroke),
  hr_below_per1SD = exp(b1["coef"] * sd_lo),
  hr_below_lo = exp((b1["coef"] - 1.96 * b1["se"]) * sd_lo),
  hr_below_hi = exp((b1["coef"] + 1.96 * b1["se"]) * sd_lo),
  hr_above_per1SD = exp(b2["coef"] * sd_hi),
  hr_above_lo = exp((b2["coef"] - 1.96 * b2["se"]) * sd_hi),
  hr_above_hi = exp((b2["coef"] + 1.96 * b2["se"]) * sd_hi),
  hr_below_per10 = hr10_lo, hr_below_per10_lo = hr10_lo_lo, hr_below_per10_hi = hr10_lo_hi,
  hr_above_per10 = hr10_hi, hr_above_per10_lo = hr10_hi_lo, hr_above_per10_hi = hr10_hi_hi,
  sd_lo = sd_lo, sd_hi = sd_hi)
logline(sprintf("segment SDs: sd_lo=%.1f sd_hi=%.1f | per10 below HR=%.3f (%.3f-%.3f) above HR=%.3f (%.3f-%.3f)",
                sd_lo, sd_hi, hr10_lo, hr10_lo_lo, hr10_lo_hi, hr10_hi, hr10_hi_lo, hr10_hi_hi))

# bootstrap CI for k* (community resampling, coarse grid for speed)
comms <- unique(d$communityID)
nb <- 500
boot_k <- replicate(nb, {
  cs <- sample(comms, replace = TRUE)
  db <- do.call(rbind, lapply(cs, function(cc) d %>% filter(communityID == cc)))
  lls_b <- sapply(quantile(db$WTI, probs = seq(0.10, 0.90, 0.10)), function(k) {
    dbb <- db %>% mutate(WTI1 = pmin(WTI, k), WTI2 = pmax(WTI - k, 0))
    f <- coxph(Surv(ftime, stroke) ~ WTI1 + WTI2 + age + sex_m + edu + smoke + drink + bmi,
               data = dbb, weights = w, cluster = communityID)
    as.numeric(logLik(f))
  })
  quantile(db$WTI, probs = seq(0.10, 0.90, 0.10))[which.max(lls_b)]
})
k_ci <- quantile(boot_k, c(0.025, 0.975), na.rm = TRUE)
res_k$k_lo <- unname(k_ci[1]); res_k$k_hi <- unname(k_ci[2])
res_k$sd_full <- sd_full
write_csv(res_k, file.path(RES, "13q_threshold.csv"))
logline(sprintf("bootstrap k 95%%CI: %.1f - %.1f | below HR/1SD %.2f (%.2f-%.2f) | above HR/1SD %.2f (%.2f-%.2f)",
                res_k$k_lo, res_k$k_hi,
                res_k$hr_below_per1SD + 1e-9, res_k$hr_below_lo + 1e-9, res_k$hr_below_hi + 1e-9,
                res_k$hr_above_per1SD + 1e-9, res_k$hr_above_lo + 1e-9, res_k$hr_above_hi + 1e-9))
logline(paste(capture.output(print(as.data.frame(
  res_k %>% mutate(across(where(is.numeric), ~round(., 3)))), row.names = FALSE)),
  collapse = "\n"))
logline("\n=== 13q COMPLETE ===")
close(logf)
