# ============================================================================
# 09_round3_delong_aj.R  (Round 3: items 4/5/7 independent reimplementations)
# 5) Wald chi2<->p internal consistency for the three RCS tests
# 4) Hand-coded DeLong test for WTI vs base (NHANES, CHARLS)
# 7) Hand-coded Aalen-Johansen 7-year CIF by WTI tertile vs cmprsk::cuminc
# Output: qc/round3_delong_aj.txt
# Date: 2026-08-16 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({library(dplyr); library(readr); library(haven); library(splines)})
set.seed(42)
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw"); OUT <- file.path(RAW, "data/processed")
lines <- character(0)
log <- function(...) { l <- sprintf(...); lines <<- c(lines, l); cat(l, "\n") }

# ---- 5) RCS Wald internal consistency ----
rcs <- read_csv(file.path(RAW, "results/03b_rcs_p.csv"), show_col_types = FALSE)
for (i in seq_len(nrow(rcs))) {
  p_re <- pchisq(rcs$chi2[i], rcs$df[i], lower.tail = FALSE)
  ok <- abs(p_re - rcs$p[i]) < 1e-6
  log("RCS %-14s chi2=%.4f df=%d p=%.6g | pchisq=%.6g -> %s",
      rcs$layer[i], rcs$chi2[i], rcs$df[i], rcs$p[i], p_re, ifelse(ok, "OK", "FAIL"))
}

# ---- 4) hand-coded DeLong (DeLong 1988 U-statistics) ----
delong_p <- function(p0, p1, y) {
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  y <- as.numeric(y)
  # place values
  V01 <- function(p0_, p1_) {   # estimator of the covariance component
    # kernel for pairs: use the standard structural components
    function(pred0, pred1) {
      # pred0 = base probs, pred1 = +index probs (vectorised over subjects)
      NULL
    }
  }
  # simpler: implement via the trapezoid form of DeLong's variance
  s10 <- function(v, g) {  # structural component per group
    if (sum(g) == 0) return(0)
    vg <- v[g == 1]; vo <- v[g == 0]
    sum((vapply(vg, function(x) mean(x > vo) + 0.5 * mean(x == vo), numeric(1))) ) / length(vg)
  }
  s00 <- function(v, g) {
    if (sum(g) == 0) return(0)
    vg <- v[g == 1]; vo <- v[g == 0]
    sum((vapply(vo, function(x) mean(x < vg) + 0.5 * mean(x == vg), numeric(1))) ) / length(vo)
  }
  s1_0 <- s10(p0, y); s0_0 <- s00(p0, y)
  s1_1 <- s10(p1, y); s0_1 <- s00(p1, y)
  A0 <- s1_0 + s0_0; A1 <- s1_1 + s0_1
  # DeLong variance via structural components of each classifier
  g <- y
  v10_0 <- mean(vapply(p0[g==1], function(x) (mean(x > p0[g==0]) + 0.5*mean(x==p0[g==0]))^2, numeric(1)))
  v01_0 <- mean(vapply(p0[g==0], function(x) (mean(x < p0[g==1]) + 0.5*mean(x==p0[g==1]))^2, numeric(1)))
  v10_1 <- mean(vapply(p1[g==1], function(x) (mean(x > p1[g==0]) + 0.5*mean(x==p1[g==0]))^2, numeric(1)))
  v01_1 <- mean(vapply(p1[g==0], function(x) (mean(x < p1[g==1]) + 0.5*mean(x==p1[g==1]))^2, numeric(1)))
  var0 <- v10_0/n1 + v01_0/n0
  var1 <- v10_1/n1 + v01_1/n0
  # covariance via cross structural components
  c10 <- mean(vapply(p0[g==1], function(x) {
    (mean(x > p0[g==0]) + 0.5*mean(x==p0[g==0])) * (mean(x > p1[g==0]) + 0.5*mean(x==p1[g==0])) }, numeric(1)))
  c01 <- mean(vapply(p0[g==0], function(x) {
    (mean(x < p0[g==1]) + 0.5*mean(x==p0[g==1])) * (mean(x < p1[g==1]) + 0.5*mean(x==p1[g==1])) }, numeric(1)))
  cov01 <- c10/n1 + c01/n0
  z <- (A1 - A0) / sqrt(max(var0 + var1 - 2 * cov01, 1e-12))
  2 * pnorm(-abs(z))
}

nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov.csv"), show_col_types = FALSE)
f0 <- glm(stroke ~ RIDAGEYR + RIAGENDR, data = nh, family = binomial())
f1 <- glm(stroke ~ WTI + RIDAGEYR + RIAGENDR, data = nh, family = binomial())
p0 <- predict(f0, type = "response"); p1 <- predict(f1, type = "response")
ok <- !is.na(p0) & !is.na(p1)
dp_nh <- delong_p(p0[ok], p1[ok], nh$stroke[ok])
log("DeLong NHANES WTI vs base: hand p=%.5f | pROC ref 0.3328", dp_nh)

ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(age = as.numeric(age), sex = as.numeric(sex)) %>%
  filter(!is.na(age) & !is.na(bmi) & !is.na(sex))
g0 <- glm(stroke_base ~ age + sex, data = ch, family = binomial())
g1 <- glm(stroke_base ~ WTI + age + sex, data = ch, family = binomial())
q0 <- predict(g0, type = "response"); q1 <- predict(g1, type = "response")
ok2 <- !is.na(q0) & !is.na(q1)
dp_ch <- delong_p(q0[ok2], q1[ok2], ch$stroke_base[ok2])
log("DeLong CHARLS WTI vs base: hand p=%.5f | pROC ref 0.0336", dp_ch)

# ---- 7) hand-coded Aalen-Johansen CIF by tertile ----
pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)
d <- pr %>% left_join(ev, by = "ID_12") %>%
  mutate(WTI_t = cut(WTI, quantile(WTI, c(0,1/3,2/3,1), na.rm=TRUE),
                     include.lowest = TRUE, labels = c("T1","T2","T3")),
         fstatus = ifelse(stroke, 1, ifelse(death, 2, 0)),
         ftime = pmin(time, 7.0),
         age = as.numeric(age),
         WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE)) %>%
  filter(!is.na(WTI_t) & !is.na(ftime) & !is.na(fstatus) &
           !is.na(WTI_sd) & !is.na(age) & !is.na(bmi) & !is.na(stroke))
aj_cif <- function(dd, t_end = 7) {
  tt <- sort(unique(dd$ftime[dd$ftime > 0 & dd$ftime <= t_end]))
  S_prev <- 1; cif1 <- 0
  for (t in tt) {
    at_risk <- sum(dd$ftime >= t)
    d1 <- sum(dd$ftime == t & dd$fstatus == 1)
    d2 <- sum(dd$ftime == t & dd$fstatus == 2)
    if (at_risk > 0) {
      h1 <- d1 / at_risk; h2 <- d2 / at_risk
      cif1 <- cif1 + S_prev * h1
      S_prev <- S_prev * (1 - h1 - h2)
    }
  }
  cif1
}
for (tg in c("T1","T2","T3")) {
  dd <- d %>% filter(WTI_t == tg)
  cif <- aj_cif(dd)
  log("AJ hand CIF %s: %.4f (cmprsk ref: T1 0.0377 / T2 0.0618 / T3 0.0723)", tg, cif)
}
writeLines(lines, "D:/NHANES/qc/round3_delong_aj.txt")
cat("\nDONE\n")
