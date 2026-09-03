# ============================================================================
# 18a_review_additions.R
# Stage-6 review additions (approved A-group F3/F4):
#   F3: NHANES fasting-subsample participants vs non-fasting adults (age >= 40)
#       baseline comparison (selection-bias quantification; requested by red
#       team P2-2 / Nature M3). Output: results/18a_fasting_vs_nonfasting.csv
#   F4: Correlations of WTI with its components (WC, TG) per analysis layer
#       (reviewer request; output: results/18a_wti_correlations.csv)
# Date: 2026-08-23 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(readr); library(survey)
})
set.seed(42)
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw")
OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")
logf <- file(file.path(RES, "18a_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

# ---------------------------------------------------------------------------
# F3: NHANES fasting vs non-fasting (age >= 40)
# ---------------------------------------------------------------------------
logline("=== F3: NHANES fasting vs non-fasting (age>=40) ===")
cyc <- c("D","E","F","G","H","I","J")
statin_pat <- "ATORVASTATIN|SIMVASTATIN|ROSUVASTATIN|PRAVASTATIN|LOVASTATIN|FLUVASTATIN|PITAVASTATIN"

rows <- list()
n_fast <- 0; n_nonfast <- 0
for (cy in cyc) {
  demo <- read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy)))
  bmx  <- read_xpt(file.path(NRAW, sprintf("BMX_%s.XPT", cy)))
  tri  <- read_xpt(file.path(NRAW, sprintf("TRIGLY_%s.XPT", cy)))
  wcol <- grep("WTSAF", names(tri), value = TRUE)[1]
  paq  <- read_xpt(file.path(NRAW, sprintf("PAQ_%s.XPT", cy)))
  pav  <- intersect(c("PAQ180", "PAD680"), names(paq))[1]
  rx   <- read_xpt(file.path(NRAW, sprintf("RXQ_RX_%s.XPT", cy)))
  bpq  <- read_xpt(file.path(NRAW, sprintf("BPQ_%s.XPT", cy)))
  diq  <- read_xpt(file.path(NRAW, sprintf("DIQ_%s.XPT", cy)))
  smq  <- read_xpt(file.path(NRAW, sprintf("SMQ_%s.XPT", cy)))
  alq  <- read_xpt(file.path(NRAW, sprintf("ALQ_%s.XPT", cy)))
  alqv <- intersect(c("ALQ110", "ALQ101"), names(alq))

  mvpa <- paq %>% transmute(SEQN, pa_mvpaw_min = as.numeric(.data[[pav]]),
                            pa_mvpaw_min = if_else(is.na(pa_mvpaw_min), 0, pa_mvpaw_min))
  statin <- rx %>% filter(as.numeric(RXDUSE) == 1, grepl(statin_pat, RXDDRUG)) %>%
    select(SEQN) %>% distinct() %>% mutate(statin = 1L)

  d <- demo %>% filter(as.numeric(RIDAGEYR) >= 40) %>%
    left_join(tri %>% select(SEQN, LBXTR, all_of(wcol)), by = "SEQN") %>%
    left_join(bmx %>% select(SEQN, BMXWAIST, BMXBMI), by = "SEQN") %>%
    left_join(bpq %>% select(SEQN, BPQ020, BPQ050A), by = "SEQN") %>%
    left_join(diq %>% select(SEQN, DIQ010), by = "SEQN") %>%
    left_join(smq %>% select(SEQN, SMQ020, SMQ040), by = "SEQN") %>%
    left_join(alq %>% select(SEQN, all_of(alqv)), by = "SEQN") %>%
    left_join(mvpa, by = "SEQN") %>%
    left_join(statin, by = "SEQN") %>%
    mutate(CYCLE = cy,
           fasting = !is.na(as.numeric(.data[[wcol]])) & as.numeric(.data[[wcol]]) > 0 &
                     !is.na(as.numeric(LBXTR)),
           wt = as.numeric(WTMEC2YR) / 7,
           psu = paste0(cy, "_", SDMVPSU), stra = paste0(cy, "_", SDMVSTRA),
           drink = FALSE)
  for (v in alqv) d$drink <- d$drink |
    (!is.na(as.numeric(d[[v]])) & as.numeric(d[[v]]) == 1)
  d <- d %>% transmute(SEQN, CYCLE, fasting, wt, psu, stra,
    age = as.numeric(RIDAGEYR), sex_f = ifelse(RIAGENDR == 1, "Male", "Female"),
    edu = as.numeric(DMDEDUC2), bmi = as.numeric(BMXBMI), wc = as.numeric(BMXWAIST),
    pa_mvpaw_min, smoke = as.numeric(SMQ020) == 1, smoke = ifelse(is.na(smoke), FALSE, smoke),
    drink, htn = as.numeric(BPQ020) == 1, htn = ifelse(is.na(htn), FALSE, htn),
    bp_rx = as.numeric(BPQ050A) == 1, bp_rx = ifelse(is.na(bp_rx), FALSE, bp_rx),
    dm = as.numeric(DIQ010) == 1, dm = ifelse(is.na(dm), FALSE, dm),
    statin = if_else(is.na(statin), 0L, statin))
  n_fast <- n_fast + sum(d$fasting); n_nonfast <- n_nonfast + sum(!d$fasting)
  rows[[length(rows) + 1]] <- d
}
nh <- bind_rows(rows) %>%
  mutate(grp = ifelse(fasting, "Fasting", "Non-fasting"),
         edu_f = factor(case_when(
           edu %in% c(7, 9) ~ NA_character_,
           edu %in% c(1, 2) ~ "Less than high school",
           edu == 3         ~ "High school",
           edu == 4         ~ "Some college",
           edu == 5         ~ "College or above",
           TRUE             ~ "Missing"),
           levels = c("Less than high school", "High school", "Some college",
                      "College or above", "Missing")),
         smoke_f = ifelse(smoke, "Ever", "Never"),
         drink_f = ifelse(drink, "Yes", "No"),
         htn_f = ifelse(htn, "Yes", "No"), dm_f = ifelse(dm, "Yes", "No"),
         statin_f = ifelse(statin == 1, "Yes", "No"),
         bprx_f = ifelse(bp_rx, "Yes", "No"))
logline(sprintf("age>=40 total n=%d; fasting=%d; non-fasting=%d",
                nrow(nh), n_fast, n_nonfast))

des <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)

wq <- function(x, w, p) {
  o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  vapply(p, function(pp) {
    i <- which(cw >= pp)[1]
    if (i == 1) return(x[1])
    lam <- (pp - cw[i - 1]) / (cw[i] - cw[i - 1])
    x[i - 1] + lam * (x[i] - x[i - 1])
  }, numeric(1))
}
out <- list()
cont <- function(var, label) {
  m <- svyglm(as.formula(paste0("`", var, "` ~ grp")), design = des)
  p <- coef(summary(m))[2, 4]
  for (g in c("Fasting", "Non-fasting")) {
    sel <- nh$grp == g
    q <- wq(nh[[var]][sel], weights(des)[sel], c(0.25, 0.5, 0.75))
    out[[length(out) + 1]] <<- data.frame(
      variable = label, level = "median (IQR)", group = g,
      est = sprintf("%.1f (%.1f, %.1f)", q[2], q[1], q[3]),
      n = sum(sel), p = p)
  }
}
catg <- function(var, label, show = NULL) {
  t <- svytable(as.formula(paste0("~ `", var, "` + grp")), des)
  lv_keep <- rownames(t)[rowSums(t) > 0]
  des_ok <- subset(des, nh[[var]] %in% lv_keep)
  if (is.factor(des_ok$variables[[var]])) {
    des_ok$variables[[var]] <- droplevels(des_ok$variables[[var]])
  }
  p <- tryCatch(
    svychisq(as.formula(paste0("~ `", var, "` + grp")), des_ok,
             statistic = "Chisq")$p.value,
    error = function(e) {
      logline(sprintf("svychisq error for %s: %s", var, conditionMessage(e)))
      NA_real_
    })
  if (is.na(p) || is.nan(p)) {
    dd <- nh[nh[[var]] %in% lv_keep & !is.na(nh[[var]]), ]
    dd[[var]] <- droplevels(dd[[var]])
    des_new <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt,
                         data = dd, nest = TRUE)
    p <- tryCatch(
      svychisq(as.formula(paste0("~ `", var, "` + grp")), des_new,
               statistic = "Chisq")$p.value,
      error = function(e) NA_real_)
    logline(sprintf("FALLBACK %s -> p=%s", var, as.character(p)))
  }
  props <- prop.table(t, 2)
  lv <- lv_keep
  if (!is.null(show)) lv <- intersect(lv, show)
  if (is.na(p) || is.nan(p)) {
    tk <- svytable(as.formula(paste0("~ `", var, "` + grp")), des_ok)
    logline(sprintf("DIAG %s: des_ok rows=%d cols=%d tablesum=%.0f p=%s",
                    var, nrow(tk), ncol(tk), sum(tk), ifelse(is.nan(p), "NaN", as.character(p))))
  }
  for (j in c("Fasting", "Non-fasting")) {
    for (i in seq_along(lv)) {
      rr <- match(lv[i], rownames(t))
      nn <- sum(nh[[var]] == lv[i] & nh$grp == j, na.rm = TRUE)
      out[[length(out) + 1]] <<- data.frame(
        variable = label, level = lv[i], group = j,
        est = sprintf("%d (%.1f%%)", nn, 100 * props[rr, j]),
        n = sum(nh$grp == j), p = p)
    }
  }
}
cont("age", "Age, years"); catg("sex_f", "Male", "Male")
catg("edu_f", "Education")
cont("bmi", "BMI, kg/m2"); cont("wc", "Waist circumference, cm")
cont("pa_mvpaw_min", "Physical activity, min/day")
catg("smoke_f", "Ever smoker", "Ever"); catg("drink_f", "Alcohol drinker", "Yes")
catg("htn_f", "Hypertension", "Yes"); catg("dm_f", "Diabetes", "Yes")
catg("statin_f", "Statin use", "Yes"); catg("bprx_f", "Antihypertensive use", "Yes")
t_f3 <- bind_rows(out) %>% mutate(cohort = "NHANES age>=40")
write_csv(t_f3, file.path(RES, "18a_fasting_vs_nonfasting.csv"))
logline(sprintf("F3 table written: %d rows", nrow(t_f3)))

# ---------------------------------------------------------------------------
# F4: WTI-component correlations (unweighted Pearson + Spearman)
# ---------------------------------------------------------------------------
logline("\n=== F4: WTI correlations with WC and TG ===")
cor_rows <- list()
add_cor <- function(d, wti, wc, tg, tag) {
  d <- d %>% filter(!is.na(.data[[wti]]), !is.na(.data[[wc]]), !is.na(.data[[tg]]))
  data.frame(
    layer = tag, n = nrow(d),
    `pearson_WTI_WC` = cor(d[[wti]], d[[wc]], method = "pearson"),
    `spearman_WTI_WC` = cor(d[[wti]], d[[wc]], method = "spearman"),
    `pearson_WTI_TG` = cor(d[[wti]], d[[tg]], method = "pearson"),
    `spearman_WTI_TG` = cor(d[[wti]], d[[tg]], method = "spearman"),
    check.names = FALSE)
}
nhc <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov_v2.csv"), show_col_types = FALSE)
cor_rows[[1]] <- add_cor(nhc, "WTI", "BMXWAIST", "TG_mmol", "NHANES fasting cross (analytic cohort)")
c11 <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE)
cor_rows[[2]] <- add_cor(c11, "WTI", "WC_cm", "TG_mmol", "CHARLS 2011 cross (M1 sample)")
c15 <- read_csv(file.path(OUT, "charls_2015_cross_cov.csv"), show_col_types = FALSE)
cor_rows[[3]] <- add_cor(c15, "WTI", "WC_cm", "TG_mmol", "CHARLS 2015 replication (M1 eligible)")
t_f4 <- bind_rows(cor_rows) %>% mutate(across(where(is.numeric), ~ round(., 3)))
write_csv(t_f4, file.path(RES, "18a_wti_correlations.csv"))
logline(paste(capture.output(print(t_f4)), collapse = "\n"))

logline("\n=== 18a COMPLETE ===")
close(logf)