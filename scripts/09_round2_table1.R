# ============================================================================
# 09_round2_table1.R  (Round 2, priority 4: full-cell independent recomputation
# of Table 1 using survey::svyquantile (different implementation from the
# hand-written weighted quantile in 05f) and svymean proportions.)
# Output: qc/round2_table1.txt
# Date: 2026-08-16
# ============================================================================
suppressPackageStartupMessages({library(survey); library(dplyr); library(readr); library(haven)})
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw"); OUT <- file.path(RAW, "data/processed")
lines <- character(0)
log <- function(...) { l <- sprintf(...); lines <<- c(lines, l); cat(l, "\n") }
nchk <- 0; nfail <- 0

check_cell <- function(tag, got, want) {
  nchk <<- nchk + 1
  g <- gsub("\\s", "", got); w <- gsub("\\s", "", want)
  if (g != w) { nfail <<- nfail + 1; log("FAIL %-34s got=%-26s want=%s", tag, got, want) }
}

# ---------------- NHANES ----------------
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov_v2.csv"), show_col_types = FALSE)
des <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy))) %>%
    transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
}) %>% bind_rows()
nh <- nh %>% left_join(des, by = c("SEQN" = "SEQN", "CYCLE.x" = "CYCLE")) %>%
  rename(CYCLE = CYCLE.x) %>%
  mutate(wt = WTSAF / 7, psu = paste0(CYCLE, "_", SDMVPSU), stra = paste0(CYCLE, "_", SDMVSTRA),
         stroke_f = ifelse(stroke, "Stroke", "No stroke"))
nhd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)
t1o <- read_csv(file.path(RAW, "results/05f_table1_nhanes_outcome.csv"), show_col_types = FALSE)

cont_vars <- c("RIDAGEYR", "bmi", "WTI", "BMXWAIST", "TG_mmol")
for (v in cont_vars) {
  for (grp in c("No stroke", "Stroke")) {
    sub <- subset(nhd, stroke_f == grp)
    q <- as.numeric(svyquantile(as.formula(paste("~", v)), sub, c(0.25, 0.5, 0.75), ci = FALSE)[[1]][, 1])
    got <- sprintf("%.1f (%.1f, %.1f)", q[2], q[1], q[3])
    want <- t1o$est[t1o$variable %in% c(v, "Age, years", "BMI, kg/m2", "WTI, cm-mmol/L",
                                       "Waist circumference, cm", "Triglycerides, mmol/L") &
                      t1o$group == grp & t1o$level == "median (IQR)"][1]
    if (is.na(want)) want <- "NA"
    check_cell(paste("NHANES", v, grp), got, want)
  }
}
cat_vars <- list(sex_f = "Male", smoke_f = "Ever", drink_f = "Yes", htn_f = "Yes",
                 dm_f = "Yes", statin_f = "Yes", bprx_f = "Yes")
for (v in names(cat_vars)) {
  lvl <- cat_vars[[v]]
  for (grp in c("No stroke", "Stroke")) {
    sub <- subset(nhd, stroke_f == grp)
    p <- as.numeric(svymean(as.formula(paste("~ I(", v, " == '", lvl, "')", sep = "")), sub))[1]
    n_raw <- sum(nhd$variables[[v]] == lvl & nhd$variables$stroke_f == grp, na.rm = TRUE)
    got <- sprintf("%d (%.1f%%)", n_raw, 100 * p)
    want <- t1o$est[t1o$variable %in% c(v, "Male", "Ever smoker", "Alcohol drinker",
                                       "Hypertension", "Diabetes", "Statin use",
                                       "Antihypertensive use") &
                      t1o$group == grp & t1o$level == lvl][1]
    check_cell(paste("NHANES", v, grp), got, want)
  }
}

# ---------------- CHARLS ----------------
ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(w_norm = bloodweight / mean(bloodweight, na.rm = TRUE),
         stroke_f = ifelse(stroke_base, "Stroke", "No stroke"),
         age = as.numeric(age)) %>%
  filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(bmi) & !is.na(age))
chd <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm, data = ch, nest = TRUE)
c1o <- read_csv(file.path(RAW, "results/05f_table1_charls_outcome.csv"), show_col_types = FALSE)
for (v in c("age", "bmi", "WTI", "WC_cm", "TG_mmol")) {
  for (grp in c("No stroke", "Stroke")) {
    sub <- subset(chd, stroke_f == grp)
    q <- as.numeric(svyquantile(as.formula(paste("~", v)), sub, c(0.25, 0.5, 0.75), ci = FALSE)[[1]][, 1])
    got <- sprintf("%.1f (%.1f, %.1f)", q[2], q[1], q[3])
    want <- c1o$est[c1o$variable %in% c(v, "Age, years", "BMI, kg/m2", "WTI, cm-mmol/L",
                                       "Waist circumference, cm", "Triglycerides, mmol/L") &
                     c1o$group == grp & c1o$level == "median (IQR)"][1]
    check_cell(paste("CHARLS", v, grp), got, want)
  }
}
log("\nchecked=%d failed=%d", nchk, nfail)
writeLines(lines, "D:/NHANES/qc/round2_table1.txt")
cat("\nDONE\n")
