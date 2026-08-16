# ============================================================================
# 13g_ndi_cox.R
# NDI prospective layer: link 2019 public-use mortality (by SEQN) to the
# NHANES fasting cross-sectional cohort (nhanes_fasting_cross_cov.csv, D-J)
# and fit survey-weighted cause-specific Cox models:
#   outcome A: all-cause death (MORTSTAT=1, time=PERMTH_INT months)
#   outcome B: stroke death (UCOD_LEADING=5, I60-I69)
# Exposure: WTI per SD. Design: cycle-specific strata/PSU, wt=WTSAF/7 (pooled).
# Outputs: results/13g_ndi_cox_models.csv ; results/13g_ndi_checks.txt
# ============================================================================
suppressPackageStartupMessages({
  library(survey); library(survival); library(haven); library(dplyr); library(readr)
})

RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw")
OUT <- file.path(RAW, "data"); PROC <- file.path(RAW, "data/processed")
RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "13g_ndi_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

res_rows <- list()
add_res <- function(cohort, layer, model, n, events, est, lo, hi, p) {
  res_rows[[length(res_rows) + 1]] <<- data.frame(
    cohort, layer, model, n, events, est, lo, hi, p)
}

# ---- cohort + mortality link ----
nh <- read_csv(file.path(PROC, "nhanes_fasting_cross_cov.csv"), show_col_types = FALSE)
mort <- read_csv(file.path(OUT, "nhanes_mort2019.csv"), show_col_types = FALSE) %>%
  select(seqn, eligstat, mortstat, ucod_leading, permth_int)

logline(sprintf("NHANES cohort n=%d; linked to mortality: %d (%.1f%%)",
                nrow(nh), sum(nh$SEQN %in% mort$seqn),
                100 * mean(nh$SEQN %in% mort$seqn)))

nh <- nh %>% left_join(mort, by = c("SEQN" = "seqn"))
logline(sprintf("eligstat among cohort: ")); print(table(nh$eligstat, useNA = "ifany"))
logline(sprintf("mortstat: deceased=%d alive=%d NA=%d",
                sum(nh$mortstat == 1, na.rm = TRUE),
                sum(nh$mortstat == 0, na.rm = TRUE), sum(is.na(nh$mortstat))))
logline(sprintf("stroke deaths (ucod 5): %d", sum(nh$ucod_leading == 5, na.rm = TRUE)))
logline(sprintf("permth_int: median %.0f months | NA=%d",
                median(nh$permth_int, na.rm = TRUE), sum(is.na(nh$permth_int))))

# ---- survey design (cycle-specific masked PSU/strata, pooled wt) ----
des <- lapply(c("D", "E", "F", "G", "H", "I", "J"), function(cy) {
  demo <- read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy)))
  demo %>% transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
})
des <- bind_rows(des)
nh <- nh %>% left_join(des, by = c("SEQN" = "SEQN", "CYCLE.x" = "CYCLE")) %>%
  rename(CYCLE = CYCLE.x) %>%
  mutate(
    wt = WTSAF / 7,
    psu = paste0(CYCLE, "_", SDMVPSU),
    stra = paste0(CYCLE, "_", SDMVSTRA),
    WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
    WTI_ter = cut(WTI, quantile(WTI, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                  include.lowest = TRUE, labels = c("T1", "T2", "T3")),
    pa_ter = cut(pa_min_day, quantile(pa_min_day, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                 include.lowest = TRUE, labels = c("L", "M", "H")),
    time_y = permth_int / 12,
    death = ifelse(mortstat == 1, 1, 0),
    death_stroke = ifelse(mortstat == 1 & ucod_leading == 5, 1, 0),
    # cause-specific: stroke-death event only; others treated as censored
    stroke_evt = ifelse(mortstat == 1 & ucod_leading == 5, 1,
                        ifelse(is.na(mortstat), NA, 0))
  ) %>%
  filter(!is.na(permth_int), !is.na(mortstat), !is.na(WTI), !is.na(RIDAGEYR),
         !is.na(RIAGENDR), !is.na(SDMVPSU), !is.na(SDMVSTRA))

logline(sprintf("\nanalytic n=%d | all-cause deaths=%d | stroke deaths=%d",
                nrow(nh), sum(nh$death), sum(nh$death_stroke)))
logline(sprintf("median follow-up %.2f y (max %.2f)", median(nh$time_y), max(nh$time_y)))
logline(sprintf("person-years total %.0f", sum(nh$time_y)))

nhd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)

# ---- cause-specific Cox (survey-weighted) ----
cox_extr <- function(fit) {
  b <- coef(fit)["WTI_sd"]
  se <- sqrt(vcov(fit)["WTI_sd", "WTI_sd"])
  c(est = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se),
    p = 2 * pnorm(-abs(b / se)))
}

run_cox <- function(outcome, label, tag) {
  mk <- function(f) as.formula(sprintf("Surv(time_y, %s) ~ %s", outcome, f))
  m1 <- svycoxph(mk("WTI_sd + RIDAGEYR + RIAGENDR"), design = nhd)
  m2 <- svycoxph(mk("WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi"), design = nhd)
  m3 <- svycoxph(mk("WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi + htn + dm + statin + bp_rx + pa_ter"), design = nhd)
  ev <- sum(nh[[outcome]] == 1, na.rm = TRUE)
  for (mm in c("m1", "m2", "m3")) {
    fit <- get(mm)
    e <- cox_extr(fit)
    rhs <- all.vars(fit$formula[[3]])
    nn <- sum(complete.cases(nh[c("time_y", outcome, rhs)]))
    add_res("NHANES-NDI", label, toupper(paste0(tag, mm)), nn, ev, e[1], e[2], e[3], e[4])
    logline(sprintf("NHANES-NDI %s %s: HR=%.3f (%.3f-%.3f) p=%.4f [events=%d, n=%d]",
                    label, toupper(mm), e[1], e[2], e[3], e[4], ev, nn))
  }
  m3t <- svycoxph(mk("WTI_ter + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi + htn + dm + statin + bp_rx + pa_ter"), design = nhd)
  b2 <- coef(m3t)["WTI_terT2"]; s2 <- sqrt(vcov(m3t)["WTI_terT2", "WTI_terT2"])
  b3 <- coef(m3t)["WTI_terT3"]; s3 <- sqrt(vcov(m3t)["WTI_terT3", "WTI_terT3"])
  logline(sprintf("NHANES-NDI %s tertile M3: T2 HR=%.3f p=%.3f | T3 HR=%.3f p=%.3f",
                  label, exp(b2), 2 * pnorm(-abs(b2 / s2)),
                  exp(b3), 2 * pnorm(-abs(b3 / s3))))
}

logline("\n=== all-cause mortality ===")
run_cox("death", "all-cause", "a")
logline("\n=== stroke mortality (I60-I69) ===")
run_cox("stroke_evt", "stroke-death", "s")

# ---- Fine-Gray sensitivity (unweighted, competing = other deaths) ----
fg_ok <- requireNamespace("cmprsk", quietly = TRUE)
logline(sprintf("\ncmprsk available: %s", fg_ok))
if (fg_ok) {
  fg_vars <- c("time_y", "fstatus", "WTI_sd", "RIDAGEYR", "RIAGENDR", "RIDRETH1",
               "edu", "smoke", "drink", "bmi", "htn", "dm", "statin", "bp_rx",
               "pa_ter")
  fg_dat <- nh %>% filter(!is.na(death_stroke)) %>%
    mutate(fstatus = ifelse(death_stroke == 1, 1, ifelse(death == 1, 2, 0))) %>%
    filter(complete.cases(select(., all_of(fg_vars))))
  fit <- cmprsk::crr(fg_dat$time_y, fg_dat$fstatus,
                     model.matrix(~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu +
                                    smoke + drink + bmi + htn + dm + statin + bp_rx +
                                    pa_ter, data = fg_dat)[, -1],
                     failcode = 1, cencode = 0)
  s <- summary(fit)$coef["WTI_sd", ]
  # cols: coef(logHR), exp(coef), se(coef), z, p-value
  add_res("NHANES-NDI", "stroke-death-FG", "M3", nrow(fg_dat),
          sum(fg_dat$fstatus == 1), s[2], exp(s[1] - 1.96 * s[3]),
          exp(s[1] + 1.96 * s[3]), s[5])
  logline(sprintf("NHANES-NDI stroke-death Fine-Gray M3: sHR=%.3f (%.3f-%.3f) p=%.4f",
                  s[2], exp(s[1] - 1.96 * s[3]), exp(s[1] + 1.96 * s[3]), s[5]))
}

write_csv(bind_rows(res_rows), file.path(RES, "13g_ndi_cox_models.csv"))
logline("\n=== 13g NDI COX COMPLETE ===")
close(logf)
