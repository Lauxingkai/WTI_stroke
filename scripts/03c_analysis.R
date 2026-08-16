# ============================================================================
# 03c_analysis.R
# Analysis : CHARLS prospective - cause-specific Cox + Fine-Gray dual report
# Date     : 2026-08-15 | Seed: 42
# R        : 4.6.1 | Packages: survival, cmprsk, dplyr, readr
# Note     : death data cover 2011-2013 (Exit module only); Fine-Gray is a
#            lower-bound sensitivity - stated limitation in report
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
  ) %>%
  filter(!is.na(WTI_sd) & !is.na(age) & !is.na(bmi) & !is.na(stroke))

# competing-risk status: 0=censored, 1=stroke, 2=death
d <- d %>% mutate(
  fstatus = ifelse(stroke, 1, ifelse(death, 2, 0)),
  ftime = pmin(time, 7.0)
)

logline(sprintf("cohort n=%d | stroke %d | death %d", nrow(d), sum(d$stroke), sum(d$death)))
m3form <- Surv(ftime, stroke) ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
  htn + dm + lipid_rx + bp_rx + pa_days_week

# ---- cause-specific Cox (death censored) ----
c1 <- coxph(Surv(ftime, stroke) ~ WTI_sd + age + sex_m,
            data = d, weights = w, cluster = communityID)
c3 <- coxph(m3form, data = d, weights = w, cluster = communityID)
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
ph <- cox.zph(c3)
logline(sprintf("PH test global p=%.4f", ph$table["GLOBAL", "p"]))

# ---- Fine-Gray subdistribution (death = competing) ----
fg1 <- crr(d$ftime, d$fstatus, cov1 = as.matrix(d[, c("WTI_sd", "age", "sex_m")]),
           failcode = 1, cencode = 0)
fg3 <- crr(d$ftime, d$fstatus,
           cov1 = as.matrix(d[, c("WTI_sd", "age", "sex_m", "edu", "smoke", "drink",
                                  "bmi", "htn", "dm", "lipid_rx", "bp_rx", "pa_days_week")]),
           failcode = 1, cencode = 0)
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
