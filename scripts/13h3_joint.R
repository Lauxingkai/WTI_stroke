# ============================================================================
# 13h3_joint.R
# (a) MDE alignment (fixed: events from the stroke column, all waves) for
#     CHARLS prospective Cox M3 final layer;
# (b) loss-to-follow-up vs baseline WTI check (E-R8): follow-up incomplete =
#     no 2018 wave record and no stroke/death event. Simple weighted
#     comparison + adjusted association (WTI_sd -> LTFU).
# Output: results/13h2_prosp_mde.txt (overwrite, corrected) ;
#         results/13h3_ltfu_check.txt
# ============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(survey) })
set.seed(42)

mde_cox <- function(events, p_exposed_r2) {
  za <- qnorm(0.975); zb <- qnorm(0.8)
  exp(sqrt((za + zb)^2 / (events * (1 - p_exposed_r2))))
}

RAW <- "D:/NHANES/data/processed"
pr <- read_csv(file.path(RAW, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev <- read_csv(file.path(RAW, "charls_events_2011_2018.csv"), show_col_types = FALSE)
d <- pr %>% left_join(ev, by = "ID_12") %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
         sex_m = ifelse(sex == 1, 1, 0),
         w = bloodweight / mean(bloodweight, na.rm = TRUE)) %>%
  filter(!is.na(stroke_base) & stroke_base == 0)

# ---- (a) MDE (M3 fitting layer: covariate-complete) ----
d3 <- d %>% filter(!is.na(WTI_sd) & !is.na(age) & !is.na(sex_m) & !is.na(bmi) &
                     !is.na(edu) & !is.na(smoke) & !is.na(drink) & !is.na(htn) &
                     !is.na(dm) & !is.na(lipid_rx) & !is.na(bp_rx) &
                     !is.na(pa_days_week) & !is.na(bloodweight) & bloodweight > 0)
r2 <- summary(lm(WTI_sd ~ age + sex_m + edu + smoke + drink + bmi + htn + dm +
                   lipid_rx + bp_rx + pa_days_week, data = d3))$r.squared
events3 <- sum(d3$stroke, na.rm = TRUE)
mde3 <- mde_cox(events3, r2)
mde1 <- mde_cox(sum(d$stroke, na.rm = TRUE), 0)
writeLines(c(
  sprintf("CHARLS prospective Cox M3 (final layer): n=%d events=%d R2=%.3f -> MDE HR %.3f",
          nrow(d3), events3, r2, mde3),
  sprintf("CHARLS prospective Cox M1: events=%d -> MDE HR %.3f (no covariate R2)",
          sum(d$stroke, na.rm = TRUE), mde1)),
  "D:/NHANES/results/13h2_prosp_mde.txt")

# ---- (b) LTFU check (M1 layer) ----
d1 <- d %>% filter(!is.na(WTI_sd) & !is.na(age) & !is.na(sex_m) &
                     !is.na(stroke) & !is.na(bloodweight) & bloodweight > 0)
d1 <- d1 %>% mutate(
  event = as.integer(!is.na(stroke) & stroke),
  ltfu = as.integer(is.na(stk18) & !event))
n <- nrow(d1); n_ltfu <- sum(d1$ltfu)
med_full <- median(d1$WTI[d1$ltfu == 0], na.rm = TRUE)
med_ltfu <- median(d1$WTI[d1$ltfu == 1], na.rm = TRUE)
wtest <- wilcox.test(WTI ~ ltfu, data = d1)
dsgn <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w,
                  data = d1, nest = TRUE)
m <- svyglm(ltfu ~ WTI_sd + age + sex_m, design = dsgn, family = quasibinomial())
s <- coef(summary(m))["WTI_sd", ]
or   <- exp(s[1]); lo <- exp(s[1] - 1.96 * s[2]); hi <- exp(s[1] + 1.96 * s[2])
out <- c(
  sprintf("(b) LTFU check (M1 layer n=%d; LTFU=%d, %.1f%%)", n, n_ltfu, 100 * n_ltfu / n),
  sprintf("median WTI: complete-follow-up %.1f vs LTFU %.1f; Wilcoxon P=%.4f",
          med_full, med_ltfu, wtest$p.value),
  sprintf("svyglm(ltfu ~ WTI_sd + age + sex): OR=%.3f (%.3f-%.3f) P=%.4f",
          or, lo, hi, s[4]))
writeLines(out, "D:/NHANES/results/13h3_ltfu_check.txt")
cat(paste(out, collapse = "\n"), "\n")