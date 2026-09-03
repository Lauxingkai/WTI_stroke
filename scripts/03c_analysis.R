# ============================================================================
# 03c_analysis.R
# Analysis : CHARLS prospective - cause-specific Cox + Fine-Gray dual report
# Date     : 2026-08-15 | v2 2026-08-20 (M1 on maximal model-wise sample)
# Seed     : 42
# R        : 4.6.1 | Packages: survival, cmprsk, dplyr, readr
# Note     : death data cover 2011-2013 (Exit module only); Fine-Gray is a
#            lower-bound sensitivity - stated limitation in report
# v2 change: Cox-M1 and FG-M1 fitted on the maximal prospective sample
#            (exposure/outcome/age/sex/positive blood weight complete);
#            Cox-M3/FG-M3 keep the covariate-complete sample. Fine-Gray
#            models are unweighted with iid standard errors (crr has no
#            weight/cluster argument); a community block-bootstrap
#            sensitivity is provided in Table S3.
# ============================================================================
suppressPackageStartupMessages({library(survival); library(cmprsk); library(dplyr); library(readr)})
set.seed(42)
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "03c_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)
d <- pr %>% left_join(ev, by = "ID_12") %>%
  mutate(
    WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
    sex_m = ifelse(sex == 1, 1, 0),
    w = bloodweight / mean(bloodweight, na.rm=TRUE),
    age = as.numeric(age)
  )

# competing-risk status: 0=censored, 1=stroke, 2=death
d <- d %>% mutate(
  fstatus = ifelse(stroke, 1, ifelse(death, 2, 0)),
  ftime = pmin(time, 7.0)
)

# M1 sample: maximal (WTI/age/sex/fstatus/ftime complete, weight > 0)
d1 <- d %>% filter(!is.na(WTI_sd) & !is.na(age) & !is.na(sex_m) &
                     !is.na(stroke) & !is.na(fstatus) & !is.na(ftime) &
                     !is.na(bloodweight) & bloodweight > 0)
# M3 sample: covariate-complete (as v1)
d3 <- d %>% filter(!is.na(WTI_sd) & !is.na(age) & !is.na(bmi) & !is.na(stroke) &
                     !is.na(fstatus) & !is.na(ftime))

logline(sprintf("M1 cohort n=%d | stroke %d | death %d", nrow(d1), sum(d1$stroke), sum(d1$death)))
logline(sprintf("M3 cohort n=%d | stroke %d | death %d", nrow(d3), sum(d3$stroke), sum(d3$death)))
m3form <- Surv(ftime, stroke) ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
  htn + dm + lipid_rx + bp_rx + pa_days_week

# ---- cause-specific Cox (death censored) ----
c1 <- coxph(Surv(ftime, stroke) ~ WTI_sd + age + sex_m,
            data = d1, weights = w, cluster = communityID)
c3 <- coxph(m3form, data = d3, weights = w, cluster = communityID)
extr_cox <- function(fit, tag) {
  s <- summary(fit)$coefficients
  s <- s[grepl("WTI_sd", rownames(s)), , drop = FALSE][1, ]
  if (any(is.na(s[c("exp(coef)", "robust se", "Pr(>|z|)")]))) {
    logline(sprintf("%s: WTI_sd row missing/NA", tag)); return(NULL)
  }
  add_hr(tag, s[["exp(coef)"]], s[["robust se"]], s[["Pr(>|z|)"]])
  logline(sprintf("Cox %s: HR=%.3f p=%.4f", tag, s[["exp(coef)"]], s[["Pr(>|z|)"]]))
}
hr_rows <- list()
add_hr <- function(tag, hr, se, p) {
  hr_rows[[length(hr_rows)+1]] <<- data.frame(
    model = tag, hr = hr, lo = hr / exp(1.96 * se), hi = hr * exp(1.96 * se), p = p)
}
extr_cox(c1, "Cox-M1"); extr_cox(c3, "Cox-M3")
logline(sprintf("Cox-M1 fitted n=%d events=%d | Cox-M3 fitted n=%d events=%d",
                c1$n, c1$nevent, c3$n, c3$nevent))
ph <- cox.zph(c3)
logline(sprintf("PH test global p=%.4f", ph$table["GLOBAL", "p"]))

# ---- Fine-Gray subdistribution (death = competing; unweighted, iid SE) ----
fg1 <- crr(d1$ftime, d1$fstatus, cov1 = as.matrix(d1[, c("WTI_sd", "age", "sex_m")]),
           failcode = 1, cencode = 0)
cov3 <- as.matrix(d3[, c("WTI_sd", "age", "sex_m", "edu", "smoke", "drink",
                         "bmi", "htn", "dm", "lipid_rx", "bp_rx", "pa_days_week")])
keep3 <- complete.cases(cov3)
fg3 <- crr(d3$ftime[keep3], d3$fstatus[keep3], cov1 = cov3[keep3, , drop = FALSE],
           failcode = 1, cencode = 0)
logline(sprintf("FG-M1 fitted n=%d events=%d | FG-M3 fitted n=%d events=%d",
                length(d1$ftime), sum(d1$fstatus == 1),
                length(d3$ftime[keep3]), sum(d3$fstatus[keep3] == 1)))
extr_fg <- function(fit, tag) {
  s <- summary(fit)$conf.int[1, ]
  hr <- s[1]; lo <- s[3]; hi <- s[4]
  se <- (log(hi) - log(lo)) / (2 * 1.96)         # back-computed from CI
  p <- 2 * pnorm(-abs(log(hr) / se))
  add_hr(tag, hr, se, p)
  logline(sprintf("FineGray %s: sHR=%.3f (%.3f-%.3f) p=%.4f", tag, hr, lo, hi, p))
}
extr_fg(fg1, "FG-M1"); extr_fg(fg3, "FG-M3")

write_csv(bind_rows(hr_rows), file.path(RES, "03c_cox_fg.csv"))
logline("\n=== 03c COMPLETE ===")
close(logf)
