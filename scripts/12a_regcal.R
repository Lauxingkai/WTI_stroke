# ============================================================================
# 12a_regcal.R  (Opt-2: TG regression calibration for CHARLS prospective layer)
# Regress 2015 TG on 2011 TG (log scale) among 7,648 repeats -> calibration
# ratio lambda; naive Cox HR corrected by exp(beta/lambda) with SE via delta
# (Rosner correction, crude). Output: qc/opt2_regcal.txt
# Date: 2026-08-16 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({library(survival); library(dplyr); library(readr)})
set.seed(42)
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed"); QC <- file.path(RAW, "qc")
lines <- character(0)
logline <- function(...) { l <- sprintf(...); lines <<- c(lines, l); cat(l, "\n") }

rep <- read_csv(file.path(OUT, "charls_tg_repeated_2011_2015.csv"), show_col_types = FALSE)
rep <- rep %>% mutate(ln11 = log(newtg), ln15 = log(bl_tg)) %>%
  filter(is.finite(ln11) & is.finite(ln15))
logline("repeat pairs: %d (expect 7648)", nrow(rep))
m <- lm(ln15 ~ ln11, data = rep)
lambda <- coef(m)["ln11"]
logline("calibration slope lambda = %.4f (SE %.4f)", lambda, summary(m)$coefficients[2, 2])
# attenuation factor = 1/lambda for log-TG-derived WTI component (TG only;
# WC has its own error; here apply Rosner-style correction on the TG-log slope)
logline("attenuation factor (1/lambda) = %.4f", 1 / lambda)

# apply to Cox M1 coefficient for reference
pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)
d <- pr %>% left_join(ev, by = "ID_12") %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         sex_m = ifelse(sex == 1, 1, 0), age = as.numeric(age),
         w = bloodweight / mean(bloodweight, na.rm=TRUE),
         ftime = pmin(time, 7.0)) %>%
  filter(!is.na(WTI_sd) & !is.na(age) & !is.na(sex_m) & !is.na(stroke) &
           !is.na(ftime) & !is.na(bloodweight) & bloodweight > 0)
c1 <- coxph(Surv(ftime, stroke) ~ WTI_sd + age + sex_m, data = d, weights = w, cluster = communityID)
b <- coef(c1)["WTI_sd"]; se <- sqrt(vcov(c1)["WTI_sd", "WTI_sd"])
logline("naive Cox M1: logHR=%.4f (HR %.4f, CI %.4f-%.4f)", b, exp(b), exp(b - 1.96*se), exp(b + 1.96*se))
b_corr <- b / lambda; se_corr <- se / lambda
logline("calibrated (lambda-corrected): logHR=%.4f (HR %.4f, CI %.4f-%.4f)",
    b_corr, exp(b_corr), exp(b_corr - 1.96*se_corr), exp(b_corr + 1.96*se_corr))
logline("NOTE: correction applies to the TG component; WTI mixes WC+TG so this is an upper-bound style correction. Report as sensitivity.")
writeLines(lines, file.path(QC, "opt2_regcal.txt"))
cat("\nDONE\n")
