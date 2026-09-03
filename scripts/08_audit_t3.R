# ============================================================================
# 08_audit_t3.R  (P2 investigation: NHANES M3 tertile reversal T3 OR 0.696)
# Diagnose: tertile ORs across M1/M2/M3; weighted stroke rates; group structure.
# Output: qc/phase5_t3_investigation.txt
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
         WTI_ter = cut(WTI, quantile(WTI, c(0,1/3,2/3,1), na.rm=TRUE), include.lowest=TRUE,
                       labels = c("T1","T2","T3")),
         pa_ter = cut(pa_mvpaw_min, quantile(pa_mvpaw_min, c(0,1/3,2/3,1), na.rm=TRUE),
                      include.lowest=TRUE, labels = c("L","M","H")))
nhd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)

# weighted stroke rate by tertile
rates <- svyby(~stroke, ~WTI_ter, nhd, svymean)
logline("=== weighted stroke rate by tertile ===")
print(rates)
logline("=== group structure (weighted means) ===")
g1 <- svyby(~RIDAGEYR, ~WTI_ter, nhd, svymean)
g2 <- svyby(~I(RIAGENDR == 1), ~WTI_ter, nhd, svymean)
g3 <- svyby(~bmi, ~WTI_ter, nhd, svymean)
g4 <- svyby(~I(htn), ~WTI_ter, nhd, svymean)
g5 <- svyby(~I(dm), ~WTI_ter, nhd, svymean)
logline("age by tertile:"); print(g1)
logline("male pct by tertile:"); print(g2)
logline("bmi by tertile:"); print(g3)
logline("htn pct by tertile:"); print(g4)
logline("dm pct by tertile:"); print(g5)

# tertile ORs under M1 / M2 / M3
run_ter <- function(form, tag) {
  m <- svyglm(form, family = quasibinomial(), design = nhd)
  s <- coef(summary(m))
  rows <- rownames(s)[grepl("WTI_ter", rownames(s))]
  for (r in rows)
    logline("%s %s: OR=%.3f p=%.4f", tag, r, exp(s[r,1]), s[r,4])
}
run_ter(stroke ~ WTI_ter + RIDAGEYR + RIAGENDR, "M1-ter")
run_ter(stroke ~ WTI_ter + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi, "M2-ter")
run_ter(stroke ~ WTI_ter + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi +
          htn + dm + statin + bp_rx + pa_ter, "M3-ter")
# continuous per SD under M3 for reference
m3c <- svyglm(stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi +
                htn + dm + statin + bp_rx + pa_ter, family = quasibinomial(), design = nhd)
s3 <- coef(summary(m3c))["WTI_sd", ]
logline("M3 continuous per SD: OR=%.3f (p=%.4f)", exp(s3[1]), s3[4])

writeLines(lines, "D:/NHANES/qc/phase5_t3_investigation.txt")
cat("\nDONE\n")
