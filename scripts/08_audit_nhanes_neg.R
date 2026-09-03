# ============================================================================
# 08_audit_nhanes_neg.R  (Plan-C deep dive: why is NHANES null?)
# 1) stepwise adjustment diagnostic (which covariate drives attenuation)
# 2) age strata (40-59 vs >=60) M1 continuous
# 3) sex strata M1
# 4) M3 without BMI (over-adjustment check)
# Output: qc/phase5_nhanes_negative.txt
# Date: 2026-08-16
# ============================================================================
suppressPackageStartupMessages({library(survey); library(dplyr); library(readr); library(haven)})
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw"); OUT <- file.path(RAW, "data/processed")
lines <- character(0)
logline <- function(...) { l <- sprintf(...); lines <<- c(lines, l); cat(l, "\n") }

nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov_v2.csv"), show_col_types = FALSE)
des <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy))) %>%
    transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
}) %>% bind_rows()
nh <- nh %>% left_join(des, by = c("SEQN" = "SEQN", "CYCLE.x" = "CYCLE")) %>%
  rename(CYCLE = CYCLE.x) %>%
  mutate(wt = WTSAF / 7, psu = paste0(CYCLE, "_", SDMVPSU), stra = paste0(CYCLE, "_", SDMVSTRA),
         WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         pa_ter = cut(pa_mvpaw_min, quantile(pa_mvpaw_min, c(0,1/3,2/3,1), na.rm=TRUE),
                      include.lowest=TRUE, labels = c("L","M","H")))
nhd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)

runm <- function(design, form, tag) {
  m <- svyglm(form, family = quasibinomial(), design = design)
  s <- coef(summary(m))["WTI_sd", ]
  logline("%-28s OR=%.3f (%.3f-%.3f) p=%.4f", tag, exp(s[1]),
          exp(s[1] - 1.96 * s[2]), exp(s[1] + 1.96 * s[2]), s[4])
}
logline("=== 1) stepwise adjustment diagnostic (continuous per SD) ===")
runm(nhd, stroke ~ WTI_sd + RIDAGEYR + RIAGENDR, "M1 age+sex")
runm(nhd, stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink, "M2a +race/edu/smoke/drink")
runm(nhd, stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi, "M2 +BMI")
runm(nhd, stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi + htn + dm, "M3a +htn/dm")
runm(nhd, stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi + htn + dm +
       statin + bp_rx, "M3b +medications")
runm(nhd, stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi + htn + dm +
       statin + bp_rx + pa_ter, "M3 full +PA")
logline("=== 4) M3 without BMI (over-adjustment check) ===")
runm(nhd, stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + htn + dm +
       statin + bp_rx + pa_ter, "M3 minus BMI")
runm(nhd, stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + bmi, "M1a +BMI only")

logline("=== 2) age strata M1 ===")
runm(subset(nhd, RIDAGEYR < 60), stroke ~ WTI_sd + RIDAGEYR + RIAGENDR, "age 40-59 M1")
runm(subset(nhd, RIDAGEYR >= 60), stroke ~ WTI_sd + RIDAGEYR + RIAGENDR, "age >=60 M1")
logline("=== 3) sex strata M1 ===")
runm(subset(nhd, RIAGENDR == 1), stroke ~ WTI_sd + RIDAGEYR, "male M1")
runm(subset(nhd, RIAGENDR == 2), stroke ~ WTI_sd + RIDAGEYR, "female M1")

writeLines(lines, "D:/NHANES/qc/phase5_nhanes_negative.txt")
cat("\nDONE\n")
