# ============================================================================
# 05b_lag2.R
# Analysis : Lag-2-year sensitivity (landmark at 2y) for CHARLS prospective
#            layer - Cox cause-specific + Fine-Gray, M1/M3 (same specs as 03c)
# Logic    : exclude participants with stroke/death inside (0, 2] years, then
#            restart follow-up at t=2 (landmark); buffers reverse causation
# Inputs   : data/processed/charls_2011_2018_prosp_cov.csv
#            data/processed/charls_events_2011_2018.csv
# Output   : results/05b_lag2.csv ; results/05b_lag2_checks.txt
# Date     : 2026-08-15 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({library(survival); library(cmprsk); library(dplyr); library(readr)})
set.seed(42)
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "05b_lag2_checks.txt"), open = "wt")
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
# M1: maximal sample (mirrors 03c v2); M3: covariate-complete
d1 <- d %>% filter(!is.na(WTI_sd) & !is.na(age) & !is.na(sex_m) & !is.na(stroke) &
                     !is.na(bloodweight) & bloodweight > 0)
d3 <- d %>% filter(!is.na(WTI_sd) & !is.na(age) & !is.na(bmi) & !is.na(stroke))

# landmark: exclude events inside (0,2] -> stk13 stroke or death_t < 2
landmark <- function(x) {
  x %>% filter(!(stk13 %in% TRUE) & (is.na(death_t) | death_t >= 2)) %>%
    mutate(
      ftime = pmin(time, 7.0) - 2,
      fstatus = ifelse(stroke, 1, ifelse(death, 2, 0))
    )
}
d0 <- d1
d1 <- landmark(d1)
d3 <- landmark(d3)
logline(sprintf("full n=%d (stroke %d, death %d) | after landmark M1 n=%d (stroke %d, death %d) | M3 n=%d (stroke %d, death %d)",
                nrow(d0), sum(d0$stroke), sum(d0$death), nrow(d1), sum(d1$stroke), sum(d1$death),
                nrow(d3), sum(d3$stroke), sum(d3$death)))

m3form <- Surv(ftime, stroke) ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
  htn + dm + lipid_rx + bp_rx + pa_days_week

hr_rows <- list()
add_hr <- function(tag, hr, se, p) {
  hr_rows[[length(hr_rows)+1]] <<- data.frame(
    model = tag, hr = hr, lo = hr / exp(1.96 * se), hi = hr * exp(1.96 * se), p = p)
}
c1 <- coxph(Surv(ftime, stroke) ~ WTI_sd + age + sex_m,
            data = d1, weights = w, cluster = communityID)
c3 <- coxph(m3form, data = d3, weights = w, cluster = communityID)
extr_cox <- function(fit, tag) {
  s <- summary(fit)$coefficients
  s <- s[grepl("WTI_sd", rownames(s)), , drop = FALSE][1, ]
  add_hr(tag, s[["exp(coef)"]], s[["robust se"]], s[["Pr(>|z|)"]])
  logline(sprintf("Lag2 Cox %s: HR=%.3f p=%.4f", tag, s[["exp(coef)"]], s[["Pr(>|z|)"]]))
}
extr_cox(c1, "Lag2-Cox-M1"); extr_cox(c3, "Lag2-Cox-M3")
ph <- cox.zph(c3)
logline(sprintf("Lag2 PH test global p=%.4f", ph$table["GLOBAL", "p"]))

fg1 <- crr(d1$ftime, d1$fstatus, cov1 = as.matrix(d1[, c("WTI_sd", "age", "sex_m")]),
           failcode = 1, cencode = 0)
fg3 <- crr(d3$ftime, d3$fstatus,
           cov1 = as.matrix(d3[, c("WTI_sd", "age", "sex_m", "edu", "smoke", "drink",
                                  "bmi", "htn", "dm", "lipid_rx", "bp_rx", "pa_days_week")]),
           failcode = 1, cencode = 0)
extr_fg <- function(fit, tag) {
  s <- summary(fit)$conf.int[1, ]
  hr <- s[1]; lo <- s[3]; hi <- s[4]
  se <- (log(hi) - log(lo)) / (2 * 1.96)
  p <- 2 * pnorm(-abs(log(hr) / se))
  add_hr(tag, hr, se, p)
  logline(sprintf("Lag2 FineGray %s: sHR=%.3f (%.3f-%.3f) p=%.4f", tag, hr, lo, hi, p))
}
extr_fg(fg1, "Lag2-FG-M1"); extr_fg(fg3, "Lag2-FG-M3")

write_csv(bind_rows(hr_rows), file.path(RES, "05b_lag2.csv"))
logline("\n=== 05b LAG2 COMPLETE ===")
close(logf)
