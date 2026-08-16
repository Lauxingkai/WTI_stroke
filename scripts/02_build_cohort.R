# ============================================================================
# 02_build_cohort.R
# Analysis : WTI x stroke dual-cohort - covariate construction + Table 1/Table S1
# Date     : 2026-08-15
# Seed     : 42
# R        : 4.6.1
# Packages : haven, dplyr, stringr, readr
# Inputs   : data/processed/*.csv (from 01_id_linkage.R)
#            CHARLS 2011 health_status/demographic/biomarkers .dta
#            NHANES {PAQ,RXQ_RX,BPQ,DIQ,SMQ,ALQ,DEMO,BMX}_{D..J}.XPT
# Outputs  : data/processed/*_cov.csv ; results/table1_by_tertile.csv ;
#            results/tableS1_blood_participation.csv ; results/02_cohort_checks.txt
# Notes    : [VERIFY] marks = coding assumptions to confirm against codebook
#            CHARLS coding convention assumed: 1=Yes, 2=No
# ============================================================================

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(stringr); library(readr)
})
set.seed(42)

RAW   <- "D:/NHANES"
CHARLS_BASE <- file.path(RAW, "CHARLS/CHARLS_1725074232_3/CHARLS")
NRAW  <- file.path(RAW, "data/raw")
OUT   <- file.path(RAW, "data/processed")
RES   <- file.path(RAW, "results")

logf <- file(file.path(RES, "02_cohort_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

id12 <- function(id) {
  id <- as.character(id) %>% str_trim()
  paste0(substr(id, 1, 9), "0", substr(id, 10, 11))
}
yn <- function(x) as.numeric(x) == 1          # [VERIFY] 1=Yes convention

# ---------------------------------------------------------------------------
# 1. CHARLS covariates (built by 00_charls_covariates.py: correct strL ID)
# ---------------------------------------------------------------------------
logline("=== STEP 1: CHARLS covariates (from Python-built CSV) ===")
cov_charls <- read_csv(file.path(OUT, "charls_covariates.csv"), show_col_types = FALSE)
c11   <- read_csv(file.path(OUT, "charls_2011_cross.csv"), show_col_types = FALSE)
prosp <- read_csv(file.path(OUT, "charls_2011_2018_prosp.csv"), show_col_types = FALSE)
c11_cov   <- c11   %>% left_join(cov_charls, by = "ID_12")
prosp_cov <- prosp %>% left_join(cov_charls, by = "ID_12")
logline(sprintf("CHARLS cross n=%d, prospective n=%d; covariate join complete", nrow(c11_cov), nrow(prosp_cov)))
logline(sprintf("CHARLS pa_days_week median: %.0f | htn %.0f%% | dm %.0f%% | lipid_rx %.0f%% | bp_rx %.0f%%",
                median(c11_cov$pa_days_week, na.rm=TRUE), 100*mean(c11_cov$htn, na.rm=TRUE),
                100*mean(c11_cov$dm, na.rm=TRUE), 100*mean(c11_cov$lipid_rx, na.rm=TRUE),
                100*mean(c11_cov$bp_rx, na.rm=TRUE)))

# ---------------------------------------------------------------------------
# 2. NHANES covariates (D-J)
# ---------------------------------------------------------------------------
logline("\n=== STEP 2: NHANES covariates ===")
statin_pat <- "ATORVASTATIN|SIMVASTATIN|ROSUVASTATIN|PRAVASTATIN|LOVASTATIN|FLUVASTATIN|PITAVASTATIN"
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_DJ.csv"), show_col_types = FALSE)
nh_cov_list <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  paq <- read_xpt(file.path(NRAW, sprintf("PAQ_%s.XPT", cy)))
  pav <- intersect(c("PAQ180", "PAD680"), names(paq))[1]   # cross-cycle PA var fallback
  logline(sprintf("cycle %s: PA variable = %s", cy, pav))
  rx  <- read_xpt(file.path(NRAW, sprintf("RXQ_RX_%s.XPT", cy)))
  bpq <- read_xpt(file.path(NRAW, sprintf("BPQ_%s.XPT", cy)))
  diq <- read_xpt(file.path(NRAW, sprintf("DIQ_%s.XPT", cy)))
  smq <- read_xpt(file.path(NRAW, sprintf("SMQ_%s.XPT", cy)))
  alq <- read_xpt(file.path(NRAW, sprintf("ALQ_%s.XPT", cy)))
  alqv <- intersect(c("ALQ110", "ALQ101"), names(alq))
  demo<- read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy)))
  bmx <- read_xpt(file.path(NRAW, sprintf("BMX_%s.XPT", cy)))

  mvpa <- paq %>% transmute(SEQN, pa_min_day = as.numeric(.data[[pav]]),
                            pa_min_day = if_else(is.na(pa_min_day), 0, pa_min_day))
  statin <- rx %>% filter(as.numeric(RXDUSE) == 1, grepl(statin_pat, RXDDRUG)) %>%
    select(SEQN) %>% distinct() %>% mutate(statin = 1L)
  c1 <- demo %>% select(SEQN, DMDEDUC2) %>%
    inner_join(bpq %>% select(SEQN, BPQ020, BPQ050A), by = "SEQN") %>%
    inner_join(diq %>% select(SEQN, DIQ010), by = "SEQN") %>%
    inner_join(smq %>% select(SEQN, SMQ020, SMQ040), by = "SEQN") %>%
    inner_join(alq %>% select(SEQN, all_of(alqv)), by = "SEQN") %>%
    inner_join(bmx %>% select(SEQN, BMXBMI), by = "SEQN") %>%
    inner_join(mvpa, by = "SEQN") %>%
    left_join(statin, by = "SEQN") %>%
    mutate(CYCLE = cy, statin = if_else(is.na(statin), 0L, statin))
  c1$drink <- FALSE
  for (v in alqv) {
    c1$drink <- c1$drink | (!is.na(as.numeric(c1[[v]])) & as.numeric(c1[[v]]) == 1)
  }
  c1
})
nh_cov <- bind_rows(nh_cov_list) %>%
  transmute(
    SEQN, CYCLE,
    edu      = ifelse(as.numeric(DMDEDUC2) %in% c(7, 9), NA_real_, as.numeric(DMDEDUC2)),  # 7/9 refused/dk -> NA (audit P2-1, 2026-08-16)
    htn      = as.numeric(BPQ020) == 1, htn = ifelse(is.na(htn), FALSE, htn),
    bp_rx    = as.numeric(BPQ050A) == 1, bp_rx = ifelse(is.na(bp_rx), FALSE, bp_rx),  # NA=not asked (non-HTN)
    dm       = as.numeric(DIQ010) == 1, dm = ifelse(is.na(dm), FALSE, dm),
    smoke    = as.numeric(SMQ020) == 1, smoke = ifelse(is.na(smoke), FALSE, smoke),  # >=100 cigarettes
    smoke_now= as.numeric(SMQ040) %in% c(1, 2), smoke_now = ifelse(is.na(smoke_now), FALSE, smoke_now),
    drink,
    bmi      = as.numeric(BMXBMI),
    pa_min_day, statin
  )
nh_final <- nh %>% left_join(nh_cov, by = "SEQN")
logline(sprintf("NHANES covariate join: n=%d | statin users %d (%.1f%%) | bp_rx %.1f%% | pa_min_day median %.0f",
                nrow(nh_final), sum(nh_final$statin), 100*mean(nh_final$statin),
                100*mean(nh_final$bp_rx, na.rm=TRUE), median(nh_final$pa_min_day, na.rm=TRUE)))

# ---------------------------------------------------------------------------
# 3. Table 1 by exposure tertiles (both cohorts)
# ---------------------------------------------------------------------------
logline("\n=== STEP 3: Table 1 by WTI tertile ===")
summ <- function(d, wti, grp, label) {
  t <- d %>% mutate(tile = cut(wti, quantile(wti, c(0, 1/3, 2/3, 1), na.rm=TRUE),
                               include.lowest = TRUE, labels = c("T1","T2","T3")))
  data.frame(cohort = label, tier = grp, tile = t$tile) %>%
    group_by(cohort, tier, tile) %>% summarise(n = n(), .groups = "drop")
}
t1_char <- summ(c11_cov, c11_cov$WTI, "CHARLS-2011", "cross")
t1_nh   <- summ(nh_final, nh_final$WTI, "NHANES-DJ", "cross")
t1 <- bind_rows(t1_char, t1_nh)
write_csv(t1, file.path(RES, "table1_by_tertile.csv"))
logline(sprintf("Table1 rows written: %d", nrow(t1)))

# ---------------------------------------------------------------------------
# 4. Table S1 produced by 00_charls_covariates.py (blood participation)
# ---------------------------------------------------------------------------
logline("\n=== STEP 4: Table S1 (see results/tableS1_blood_participation.csv) ===")

# ---------------------------------------------------------------------------
# 5. Export cohort files with covariates
# ---------------------------------------------------------------------------
write_csv(c11_cov,   file.path(OUT, "charls_2011_cross_cov.csv"))
write_csv(prosp_cov, file.path(OUT, "charls_2011_2018_prosp_cov.csv"))
write_csv(nh_final,  file.path(OUT, "nhanes_fasting_cross_cov.csv"))

logline("\n=== 02 BUILD COHORT COMPLETE ===")
close(logf)
