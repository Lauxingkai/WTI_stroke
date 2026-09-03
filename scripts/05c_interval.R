# ============================================================================
# 05c_interval.R
# Analysis : Interval-censoring sensitivity - discrete-time (person-period)
#            logistic model, 3 intervals (0-2, 2-4, 4-7 y), stroke outcome
#            only (death as right-censoring; competing risk handled in 03c)
# Logic    : events are known only to the wave (2013/2015/2018) at which they
#            were first reported -> interval-censored. Person-period rows with
#            interval indicators; WTI per SD, M1 and M3 covariates as in 03c.
# Inputs   : data/processed/charls_2011_2018_prosp_cov.csv
#            data/processed/charls_events_2011_2018.csv
# Output   : results/05c_interval.csv ; results/05c_interval_checks.txt
# Date     : 2026-08-15 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({library(survey); library(dplyr); library(readr)})
set.seed(42)
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "05c_interval_checks.txt"), open = "wt")
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

# interval index: stroke at wave -> interval 1/2/3; death (Exit <=2013) censors
# inside interval 1. death_t < stroke_t handled by capping rows at death.
add_interval <- function(d) {
  d <- d %>% mutate(
    j_stroke = case_when(stroke & stroke_t <= 2 ~ 1L,
                         stroke & stroke_t <= 4 ~ 2L,
                         stroke ~ 3L, TRUE ~ NA_integer_),
    j_death  = ifelse(death & !is.na(death_t) & death_t < 4, 1L, NA_integer_),
    j_last   = case_when(
      !is.na(j_stroke) & !is.na(j_death) ~ pmin(j_stroke, ifelse(death_t <= stroke_t, j_death, j_stroke)),
      !is.na(j_stroke) ~ j_stroke,
      !is.na(j_death)  ~ j_death,
      TRUE ~ 3L)
  )
  d <- d %>% mutate(j_stroke = ifelse(!is.na(j_death) & !is.na(j_stroke) &
                                        death_t <= stroke_t, NA_integer_, j_stroke))
  d
}
d1 <- add_interval(d1)
d3 <- add_interval(d3)

make_pp <- function(d) {
  d %>%
    reframe(ID_12 = ID_12, k = seq_len(j_last), .by = ID_12) %>%
    left_join(d, by = "ID_12") %>%
    mutate(event = as.integer(k == j_stroke & !is.na(j_stroke)),
           k = factor(k, levels = 1:3))
}
pp1 <- make_pp(d1)
pp3 <- make_pp(d3)
logline(sprintf("person-period M1 rows: %d | events: %d | subjects: %d | M3 rows: %d | events: %d | subjects: %d",
                nrow(pp1), sum(pp1$event), n_distinct(pp1$ID_12),
                nrow(pp3), sum(pp3$event), n_distinct(pp3$ID_12)))

ppd1 <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w,
                  data = pp1, nest = TRUE)
ppd3 <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w,
                  data = pp3, nest = TRUE)

fit_dt <- function(form, tag, design) {
  m <- svyglm(form, family = quasibinomial(), design = design)
  s <- coef(summary(m))
  r <- s[grepl("WTI_sd", rownames(s)), , drop = FALSE][1, ]
  out <- data.frame(model = tag, est = exp(r[1]), lo = exp(r[1] - 1.96 * r[2]),
                    hi = exp(r[1] + 1.96 * r[2]), p = 2 * pnorm(-abs(r[1] / r[2])))
  logline(sprintf("%s: OR=%.3f (%.3f-%.3f) p=%.4f", tag, out$est, out$lo, out$hi, out$p))
  out
}
m1 <- fit_dt(event ~ WTI_sd + age + sex_m + k, "IC-M1", ppd1)
m3 <- fit_dt(event ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
               htn + dm + lipid_rx + bp_rx + pa_days_week + k, "IC-M3", ppd3)
# interval main effects (M3)
m3f <- svyglm(event ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
                htn + dm + lipid_rx + bp_rx + pa_days_week + k,
              family = quasibinomial(), design = ppd3)
s3 <- coef(summary(m3f))
for (j in c("k2", "k3")) {
  if (j %in% rownames(s3))
    logline(sprintf("M3 interval %s vs k1: OR=%.3f p=%.4f", j, exp(s3[j, 1]), s3[j, 4]))
}
write_csv(bind_rows(m1, m3), file.path(RES, "05c_interval.csv"))
logline("\n=== 05c INTERVAL COMPLETE ===")
close(logf)
