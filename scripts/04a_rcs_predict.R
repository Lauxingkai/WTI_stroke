# ============================================================================
# 04a_rcs_predict.R
# Weighted ns(df=3) dose-response curves via explicit basis matrices.
# Output: results/04a_rcs_predictions.csv
# ============================================================================
suppressPackageStartupMessages({library(survey); library(splines); library(dplyr); library(readr)})
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw"); OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")

# ---- NHANES ----
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov.csv"), show_col_types = FALSE)
des <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  haven::read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy))) %>%
    transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
}) %>% bind_rows()
nh <- nh %>% left_join(des, by = c("SEQN", "CYCLE.x" = "CYCLE")) %>%
  mutate(wt = WTSAF / 7, psu = paste0(CYCLE.y, "_", SDMVPSU), stra = paste0(CYCLE.y, "_", SDMVSTRA))
k1 <- quantile(nh$WTI, c(1/3, 2/3), na.rm = TRUE)
B1 <- ns(nh$WTI, knots = k1)
nh$ns1 <- B1[,1]; nh$ns2 <- B1[,2]; nh$ns3 <- B1[,3]
nd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)
f_nh <- svyglm(stroke ~ ns1 + ns2 + ns3 + RIDAGEYR + RIAGENDR + edu + smoke + drink + bmi,
               family = quasibinomial(), design = nd)

# ---- CHARLS cross ----
ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(w_norm = bloodweight / mean(bloodweight, na.rm=TRUE), sex_m = ifelse(sex == 1, 1, 0),
         age = as.numeric(age)) %>% filter(!is.na(bmi) & !is.na(age) & !is.na(w_norm))
k2 <- quantile(ch$WTI, c(1/3, 2/3), na.rm = TRUE)
B2 <- ns(ch$WTI, knots = k2)
ch$ns1 <- B2[,1]; ch$ns2 <- B2[,2]; ch$ns3 <- B2[,3]
cd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm, data = ch, nest = TRUE)
f_ch <- svyglm(stroke_base ~ ns1 + ns2 + ns3 + age + sex_m + edu + smoke + drink + bmi,
               family = quasibinomial(), design = cd)

# ---- CHARLS prospective ----
pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE) %>%
  mutate(w_norm = bloodweight / mean(bloodweight, na.rm=TRUE), sex_m = ifelse(sex == 1, 1, 0),
         age = as.numeric(age)) %>% filter(!is.na(bmi) & !is.na(age) & !is.na(w_norm))
k3 <- quantile(pr$WTI, c(1/3, 2/3), na.rm = TRUE)
B3 <- ns(pr$WTI, knots = k3)
pr$ns1 <- B3[,1]; pr$ns2 <- B3[,2]; pr$ns3 <- B3[,3]
pd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm, data = pr, nest = TRUE)
f_pr <- svyglm(stroke_2018 ~ ns1 + ns2 + ns3 + age + sex_m + edu + smoke + drink + bmi,
               family = quasibinomial(), design = pd)

curve_grid <- function(fit, k, ref, xrange) {
  Bx <- ns(xrange, knots = k)
  Bre <- ns(c(ref, xrange), knots = k)[1, ]
  idx <- c("ns1", "ns2", "ns3")
  b <- coef(fit)[idx]; V <- vcov(fit)[idx, idx]
  d <- sweep(Bx, 2, as.numeric(Bre))
  logOR <- as.numeric(d %*% b)
  se <- sqrt(rowSums((d %*% V) * d))
  data.frame(WTI = xrange, logOR = logOR, se = se)
}
g_nh <- curve_grid(f_nh, k1, ref = 85, seq(40, 420, 5))
g_ch <- curve_grid(f_ch, k2, ref = 85, seq(40, 420, 5))
g_pr <- curve_grid(f_pr, k3, ref = 85, seq(40, 420, 5))
out <- bind_rows(
  g_nh %>% mutate(layer = "NHANES-cross"),
  g_ch %>% mutate(layer = "CHARLS-cross"),
  g_pr %>% mutate(layer = "CHARLS-prosp"))
write_csv(out, file.path(RES, "04a_rcs_predictions.csv"))
cat("RCS predictions exported:", nrow(out), "rows\n")
