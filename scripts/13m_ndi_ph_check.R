# 13m_ndi_ph_check.R — PH assumption + EPV for the NDI Cox models (review support)
suppressPackageStartupMessages({ library(survival); library(dplyr); library(readr) })
nh <- read_csv("D:/NHANES/data/processed/nhanes_fasting_cross_cov.csv", show_col_types = FALSE)
mort <- read_csv("D:/NHANES/data/nhanes_mort2019.csv", show_col_types = FALSE) %>%
  select(seqn, eligstat, mortstat, ucod_leading, permth_int)
nh <- nh %>% left_join(mort, by = c("SEQN" = "seqn")) %>%
  mutate(time_y = permth_int / 12,
         death = ifelse(mortstat == 1, 1, 0),
         death_stroke = ifelse(mortstat == 1 & ucod_leading == 5, 1, 0),
         stroke_evt = ifelse(mortstat == 1 & ucod_leading == 5, 1,
                             ifelse(is.na(mortstat), NA, 0)),
         WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
         pa_ter = cut(pa_min_day, quantile(pa_min_day, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                      include.lowest = TRUE, labels = c("L", "M", "H"))) %>%
  filter(!is.na(permth_int), !is.na(mortstat), !is.na(WTI), !is.na(RIDAGEYR),
         !is.na(RIAGENDR))

run <- function(out, tag) {
  m1 <- coxph(as.formula(sprintf("Surv(time_y, %s) ~ WTI_sd + RIDAGEYR + RIAGENDR", out)), data = nh)
  m3 <- coxph(as.formula(sprintf("Surv(time_y, %s) ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi + htn + dm + statin + bp_rx + pa_ter", out)), data = nh)
  z1 <- cox.zph(m1); z3 <- cox.zph(m3)
  cat(sprintf("%s M1: global PH p=%.3f | WTI p=%.3f\n", tag, z1$table[nrow(z1$table), "p"], z1$table["WTI_sd", "p"]))
  cat(sprintf("%s M3: global PH p=%.3f | WTI p=%.3f\n", tag, z3$table[nrow(z3$table), "p"], z3$table["WTI_sd", "p"]))
  # EPV: events / params (excluding strata/intercept)
  ev <- sum(nh[[out]] == 1, na.rm = TRUE)
  cat(sprintf("%s: events=%d | M3 params=%d -> EPV=%.1f\n", tag, ev,
              ncol(model.matrix(m3)) - 1, ev / (ncol(model.matrix(m3)) - 1)))
}
run("death", "all-cause")
run("stroke_evt", "stroke-death")
