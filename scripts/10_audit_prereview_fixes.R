# ============================================================================
# 10_audit_prereview_fixes.R  (pre-review fixes: Major#2 FPG sensitivity,
# Major#3 sex interaction, Minor#4 weighted AUC)
# Output: qc/prereview_fix_analyses.txt
# Date: 2026-08-16 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({library(survey); library(pROC); library(dplyr); library(readr); library(haven)})
set.seed(42)
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw"); OUT <- file.path(RAW, "data/processed")
lines <- character(0)
log <- function(...) { l <- sprintf(...); lines <<- c(lines, l); cat(l, "\n") }

# ---------------- NHANES: M3 + continuous FPG ----------------
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov.csv"), show_col_types = FALSE)
glu <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  read_xpt(file.path(NRAW, sprintf("GLU_%s.XPT", cy))) %>%
    transmute(SEQN, LBXGLU = as.numeric(LBXGLU), CYCLE = cy)
}) %>% bind_rows()
des <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy))) %>%
    transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
}) %>% bind_rows()
nh <- nh %>% left_join(glu, by = c("SEQN", "CYCLE.x" = "CYCLE")) %>%
  left_join(des, by = c("SEQN", "CYCLE.x" = "CYCLE")) %>%
  rename(CYCLE = CYCLE.x) %>%
  mutate(wt = WTSAF / 7, psu = paste0(CYCLE, "_", SDMVPSU), stra = paste0(CYCLE, "_", SDMVSTRA),
         WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         FPG_z = (LBXGLU - mean(LBXGLU, na.rm=TRUE)) / sd(LBXGLU, na.rm=TRUE),
         pa_ter = cut(pa_min_day, quantile(pa_min_day, c(0,1/3,2/3,1), na.rm=TRUE),
                      include.lowest=TRUE, labels = c("L","M","H")),
         sex_f = ifelse(RIAGENDR == 1, 1, 0))
nhd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)
run_nh <- function(form, tag) {
  m <- svyglm(form, family = quasibinomial(), design = nhd)
  s <- coef(summary(m))["WTI_sd", ]
  log("NHANES %-18s OR=%.3f (%.3f-%.3f) p=%.4f", tag, exp(s[1]),
      exp(s[1]-1.96*s[2]), exp(s[1]+1.96*s[2]), s[4])
}
run_nh(stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi +
         htn + dm + statin + bp_rx + pa_ter, "M3")
run_nh(stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi +
         htn + dm + statin + bp_rx + pa_ter + FPG_z, "M3+FPG")
# interaction: WTI_sd x sex (M1)
mi <- svyglm(stroke ~ WTI_sd * sex_f + RIDAGEYR, family = quasibinomial(), design = nhd)
si <- coef(summary(mi))["WTI_sd:sex_f", ]
log("NHANES M1 WTI x sex interaction: beta=%.4f p=%.4f", si[1], si[4])

# ---------------- CHARLS ----------------
ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         sex_m = ifelse(sex == 1, 1, 0), age = as.numeric(age),
         FPG_z = (newglu - mean(newglu, na.rm=TRUE)) / sd(newglu, na.rm=TRUE),
         pa_ter = cut(pa_days_week, c(-1, 0, 1, 100), labels = c("0d","1-6d","7d")),
         w_norm = bloodweight / mean(bloodweight, na.rm=TRUE)) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age))
chd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm, data = ch, nest = TRUE)
run_ch <- function(form, tag) {
  m <- svyglm(form, family = quasibinomial(), design = chd)
  s <- coef(summary(m))["WTI_sd", ]
  log("CHARLS %-16s OR=%.3f (%.3f-%.3f) p=%.4f", tag, exp(s[1]),
      exp(s[1]-1.96*s[2]), exp(s[1]+1.96*s[2]), s[4])
}
run_ch(stroke_base ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
         htn + dm + lipid_rx + bp_rx + pa_ter, "cm3")
run_ch(stroke_base ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi +
         htn + dm + lipid_rx + bp_rx + pa_ter + FPG_z, "cm3+FPG")
ci <- svyglm(stroke_base ~ WTI_sd * sex_m + age, family = quasibinomial(), design = chd)
cii <- coef(summary(ci))["WTI_sd:sex_m", ]
log("CHARLS cm1 WTI x sex interaction: beta=%.4f p=%.4f", cii[1], cii[4])

pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)
d <- pr %>% left_join(ev, by = "ID_12") %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         sex_m = ifelse(sex == 1, 1, 0), age = as.numeric(age),
         w = bloodweight / mean(bloodweight, na.rm=TRUE)) %>%
  filter(!is.na(WTI_sd) & !is.na(age) & !is.na(bmi) & !is.na(stroke))
pd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w, data = d, nest = TRUE)
pi <- svyglm(stroke_2018 ~ WTI_sd * sex_m + age, family = quasibinomial(), design = pd)
pii <- coef(summary(pi))["WTI_sd:sex_m", ]
log("CHARLS pm1 WTI x sex interaction: beta=%.4f p=%.4f", pii[1], pii[4])

# ---------------- weighted AUC (Minor#4) ----------------
m0n <- svyglm(stroke ~ RIDAGEYR + RIAGENDR, family = quasibinomial(), design = nhd)
m1n <- svyglm(stroke ~ WTI + RIDAGEYR + RIAGENDR, family = quasibinomial(), design = nhd)
yyn <- nhd$variables$stroke
r0n <- roc(yyn, predict(m0n, type = "response"), quiet = TRUE)
r1n <- roc(yyn, predict(m1n, type = "response"), quiet = TRUE)
log("NHANES weighted AUC (weighted roc): WTI %.3f vs base %.3f", as.numeric(auc(r1n)), as.numeric(auc(r0n)))
m0c <- svyglm(stroke_base ~ age + sex_m, family = quasibinomial(), design = chd)
m1c <- svyglm(stroke_base ~ WTI + age + sex_m, family = quasibinomial(), design = chd)
p0c <- predict(m0c, type = "response"); p1c <- predict(m1c, type = "response")
okc <- !is.na(p0c) & !is.na(p1c) & !is.na(chd$variables$stroke_base)
r0c <- roc(chd$variables$stroke_base[okc], p0c[okc], quiet = TRUE)
r1c <- roc(chd$variables$stroke_base[okc], p1c[okc], quiet = TRUE)
log("CHARLS weighted AUC (weighted roc): WTI %.3f vs base %.3f", as.numeric(auc(r1c)), as.numeric(auc(r0c)))

writeLines(lines, "D:/NHANES/qc/prereview_fix_analyses.txt")
cat("\nDONE\n")
