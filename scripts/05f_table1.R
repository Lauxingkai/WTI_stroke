# ============================================================================
# 05f_table1.R  (v2)
# Analysis : Final weighted Table 1, both cohorts, two stratifications each
#            (by outcome stroke; by WTI tertiles). Cell = raw n (weighted %);
#            design-corrected P (svychisq Rao-Scott / svyglm Wald).
# Outputs  : results/05f_table1_{nhanes,charls}_{outcome,tertile}.csv
#            results/05f_table1_checks.txt
# Date     : 2026-08-15 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({
  library(survey); library(dplyr); library(readr); library(haven)
})
set.seed(42)
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw")
OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "05f_table1_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

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
fmt_cont <- function(design, var, group, label) {
  d <- update(design, g = get(group), v = get(var))
  lv <- unique(na.omit(d$variables$g))
  p <- NA_real_
  tryCatch({
    m <- svyglm(v ~ g, design = d)
    s <- coef(summary(m)); idx <- grep("^g", rownames(s))
    if (length(idx) > 1) {
      b <- s[idx, 1]; V <- vcov(m)[idx, idx, drop = FALSE]
      p <- pchisq(as.numeric(t(b) %*% solve(V) %*% b), df = length(idx), lower.tail = FALSE)
    } else if (length(idx) == 1) p <- s[idx, 4]
  }, error = function(e) NULL)
  out <- lapply(seq_along(lv), function(j) {
    sel <- d$variables$g == lv[j]
    q <- wq(d$variables$v[sel], weights(d)[sel], c(0.25, 0.5, 0.75))
    data.frame(variable = label, level = "median (IQR)",
               group = lv[j], est = sprintf("%.1f (%.1f, %.1f)", q[2], q[1], q[3]), p = p)
  })
  bind_rows(out)
}
fmt_cat <- function(design, var, group, label, show = NULL) {
  d <- update(design, g = get(group), v = get(var))
  t <- svytable(~ v + g, d)
  ntab <- table(d$variables$v, d$variables$g)
  p <- svychisq(~ v + g, d, statistic = "Chisq")$p.value
  props <- prop.table(t, 2)
  lv <- rownames(t)
  if (!is.null(show)) lv <- intersect(lv, show)
  out <- list()
  for (j in seq_len(ncol(t))) {
    for (i in seq_along(lv)) {
      rr <- match(lv[i], rownames(t))
      nn <- ntab[match(lv[i], rownames(ntab)), j]
      out[[length(out)+1]] <- data.frame(
        variable = label, level = lv[i], group = colnames(t)[j],
        est = sprintf("%d (%.1f%%)", nn, 100 * props[rr, j]), p = p)
    }
  }
  bind_rows(out)
}

# ---------------------------------------------------------------------------
# NHANES
# ---------------------------------------------------------------------------
logline("=== NHANES Table 1 ===")
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov_v2.csv"), show_col_types = FALSE)
des <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy))) %>%
    transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
}) %>% bind_rows()
nh <- nh %>% left_join(des, by = c("SEQN" = "SEQN", "CYCLE.x" = "CYCLE")) %>%
  rename(CYCLE = CYCLE.x) %>%
  mutate(
    wt = WTSAF / 7,
    psu = paste0(CYCLE, "_", SDMVPSU),
    stra = paste0(CYCLE, "_", SDMVSTRA),
    stroke_f = ifelse(stroke, "Stroke", "No stroke"),
    wti_t = cut(WTI, quantile(WTI, c(0,1/3,2/3,1), na.rm = TRUE),
                include.lowest = TRUE, labels = c("T1","T2","T3")),
    sex_f = ifelse(RIAGENDR == 1, "Male", "Female"),
    edu_f = factor(case_when(
      edu %in% c(1,2) ~ "Less than high school",
      edu == 3        ~ "High school",
      edu == 4        ~ "Some college",
      edu == 5        ~ "College or above",
      TRUE            ~ "Missing"), levels = c(
        "Less than high school", "High school", "Some college",
        "College or above", "Missing")),
    smoke_f = ifelse(smoke, "Ever", "Never"),
    drink_f = ifelse(drink, "Yes", "No"),
    htn_f = ifelse(htn, "Yes", "No"), dm_f = ifelse(dm, "Yes", "No"),
    statin_f = ifelse(statin == 1, "Yes", "No"),
    bprx_f = ifelse(bp_rx, "Yes", "No"),
    pa_cont = as.numeric(pa_mvpaw_min)
  )
nhd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)

build_nh <- function(group) {
  cts <- bind_rows(
    fmt_cont(nhd, "RIDAGEYR", group, "Age, years"),
    fmt_cont(nhd, "bmi", group, "BMI, kg/m2"),
    fmt_cont(nhd, "WTI", group, "WTI, cm-mmol/L"),
    fmt_cont(nhd, "BMXWAIST", group, "Waist circumference, cm"),
    fmt_cont(nhd, "TG_mmol", group, "Triglycerides, mmol/L"),
    fmt_cont(nhd, "pa_cont", group, "Physical activity, min/wk"))
  cats <- bind_rows(
    fmt_cat(nhd, "sex_f", group, "Male", show = "Male"),
    fmt_cat(nhd, "edu_f", group, "Education"),
    fmt_cat(nhd, "smoke_f", group, "Ever smoker", show = "Ever"),
    fmt_cat(nhd, "drink_f", group, "Alcohol drinker", show = "Yes"),
    fmt_cat(nhd, "htn_f", group, "Hypertension", show = "Yes"),
    fmt_cat(nhd, "dm_f", group, "Diabetes", show = "Yes"),
    fmt_cat(nhd, "statin_f", group, "Statin use", show = "Yes"),
    fmt_cat(nhd, "bprx_f", group, "Antihypertensive use", show = "Yes"))
  bind_rows(cts, cats) %>% mutate(cohort = "NHANES", strata = group)
}
t1o <- build_nh("stroke_f"); t1t <- build_nh("wti_t")
write_csv(t1o, file.path(RES, "05f_table1_nhanes_outcome.csv"))
write_csv(t1t, file.path(RES, "05f_table1_nhanes_tertile.csv"))
logline(sprintf("NHANES rows: outcome=%d tertile=%d", nrow(t1o), nrow(t1t)))

# ---------------------------------------------------------------------------
# CHARLS
# ---------------------------------------------------------------------------
logline("\n=== CHARLS Table 1 ===")
ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(
    w_norm = bloodweight / mean(bloodweight, na.rm = TRUE),
    stroke_f = ifelse(stroke_base, "Stroke", "No stroke"),
    wti_t = cut(WTI, quantile(WTI, c(0,1/3,2/3,1), na.rm = TRUE),
                include.lowest = TRUE, labels = c("T1","T2","T3")),
    sex_f = ifelse(sex == 1, "Male", "Female"),
    edu_f = factor(case_when(
      edu %in% c(1,2,3,4) ~ "Primary or less",
      edu == 5           ~ "Middle school",
      edu == 6           ~ "High school",
      edu >= 7           ~ "College or above",
      TRUE               ~ "Missing"), levels = c(
        "Primary or less", "Middle school", "High school",
        "College or above", "Missing")),
    smoke_f = ifelse(smoke, "Ever", "Never"),
    drink_f = ifelse(drink, "Yes", "No"),
    htn_f = ifelse(htn, "Yes", "No"), dm_f = ifelse(dm, "Yes", "No"),
    lipidrx_f = ifelse(lipid_rx, "Yes", "No"),
    bprx_f = ifelse(bp_rx, "Yes", "No"),
    pa_cont = as.numeric(pa_days_week),
    age = as.numeric(age)
  ) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(age))
chd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm,
                 data = ch, nest = TRUE)

build_ch <- function(group) {
  cts <- bind_rows(
    fmt_cont(chd, "age", group, "Age, years"),
    fmt_cont(chd, "bmi", group, "BMI, kg/m2"),
    fmt_cont(chd, "WTI", group, "WTI, cm-mmol/L"),
    fmt_cont(chd, "WC_cm", group, "Waist circumference, cm"),
    fmt_cont(chd, "TG_mmol", group, "Triglycerides, mmol/L"),
    fmt_cont(chd, "pa_cont", group, "MVPA, days/week"))
  cats <- bind_rows(
    fmt_cat(chd, "sex_f", group, "Male", show = "Male"),
    fmt_cat(chd, "edu_f", group, "Education"),
    fmt_cat(chd, "smoke_f", group, "Ever smoker", show = "Ever"),
    fmt_cat(chd, "drink_f", group, "Alcohol drinker", show = "Yes"),
    fmt_cat(chd, "htn_f", group, "Hypertension", show = "Yes"),
    fmt_cat(chd, "dm_f", group, "Diabetes", show = "Yes"),
    fmt_cat(chd, "lipidrx_f", group, "Lipid-lowering use", show = "Yes"),
    fmt_cat(chd, "bprx_f", group, "Antihypertensive use", show = "Yes"))
  bind_rows(cts, cats) %>% mutate(cohort = "CHARLS", strata = group)
}
c1o <- build_ch("stroke_f"); c1t <- build_ch("wti_t")
write_csv(c1o, file.path(RES, "05f_table1_charls_outcome.csv"))
write_csv(c1t, file.path(RES, "05f_table1_charls_tertile.csv"))
logline(sprintf("CHARLS rows: outcome=%d tertile=%d", nrow(c1o), nrow(c1t)))

logline("\n=== 05f TABLE1 COMPLETE ===")
close(logf)
