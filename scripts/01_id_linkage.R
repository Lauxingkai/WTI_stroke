# ============================================================================
# 01_id_linkage.R
# Analysis : WTI x stroke dual-cohort (NHANES + CHARLS) - ID linkage & cohort build
# Date     : 2026-08-15
# Seed     : 42 (set for reproducibility of any resampling)
# R        : 4.6.1
# Packages : haven, dplyr, stringr, readr
# Inputs   : D:\NHANES\CHARLS\CHARLS_1725074232_3\CHARLS\{2011,2013,2015,2018}\*.dta
#            D:\NHANES\data\raw\{DEMO,BMX,MCQ,TRIGLY,FASTQX}_{D..J}.XPT
# Outputs  : D:\NHANES\data\processed\*.csv ; D:\NHANES\results\01_linkage_checks.txt
# Rules    : raw data read-only; derived outputs to data/processed/ only
# ============================================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
  library(readr)
})

set.seed(42)

RAW   <- "D:/NHANES"
CHARLS_BASE <- file.path(RAW, "CHARLS/CHARLS_1725074232_3/CHARLS")
NRAW  <- file.path(RAW, "data/raw")
OUT   <- file.path(RAW, "data/processed")
RES   <- file.path(RAW, "results")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(RES, showWarnings = FALSE, recursive = TRUE)

logf <- file(file.path(RES, "01_linkage_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

# ---------------------------------------------------------------------------
# 1. CHARLS ID official normalization (Wave2 Release Note)
#    householdID = householdID + "0"; ID_12 = householdID + substr(ID, -2, 2)
# ---------------------------------------------------------------------------
id12 <- function(id) {
  id <- as.character(id) %>% str_trim()
  stopifnot(all(nchar(id) == 11))
  paste0(substr(id, 1, 9), "0", substr(id, 10, 11))
}

logline("=== STEP 1: CHARLS ID normalization ===")

b11 <- read_dta(file.path(CHARLS_BASE, "2011/Blood_20140429.dta"))
b15 <- read_dta(file.path(CHARLS_BASE, "2015/Blood.dta"))
bm11 <- read_dta(file.path(CHARLS_BASE, "2011/biomarkers.dta"))
hs11 <- read_dta(file.path(CHARLS_BASE, "2011/health_status_and_functioning.dta"))
hs18 <- read_dta(file.path(CHARLS_BASE, "2018/Health_Status_and_Functioning.dta"))

# assertions: ID widths
stopifnot(all(nchar(str_trim(as.character(b11$ID))) == 11))
stopifnot(all(nchar(str_trim(as.character(b15$ID))) == 12))
logline(sprintf("ID width assertion OK: 2011 blood n=%d (11-digit), 2015 blood n=%d (12-digit)",
                nrow(b11), nrow(b15)))

b11$ID_12 <- id12(b11$ID)
bm11$ID_12 <- id12(bm11$ID)
hs11$ID_12 <- id12(hs11$ID)

n_link_2015 <- length(intersect(b11$ID_12, str_trim(as.character(b15$ID))))
n_link_2018 <- length(intersect(b11$ID_12, str_trim(as.character(hs18$ID))))
logline(sprintf("Linkage check: 2011 blood x 2015 blood = %d (expect 7648)", n_link_2015))
logline(sprintf("Linkage check: 2011 blood x 2018 health = %d (expect 9612)", n_link_2018))
stopifnot(n_link_2015 == 7648, n_link_2018 == 9612)

# ---------------------------------------------------------------------------
# 2. CHARLS cross-sectional layer (2011): blood TG + waist + baseline stroke
# ---------------------------------------------------------------------------
logline("\n=== STEP 2: CHARLS 2011 cross-sectional layer ===")
c11 <- b11 %>%
  inner_join(bm11 %>% select(ID_12, qm002), by = "ID_12") %>%
  left_join(hs11 %>% select(ID_12, da007_8_), by = "ID_12") %>%
  mutate(
    WC_cm   = as.numeric(qm002),
    TG_mmol = as.numeric(newtg) * 0.01129,
    WTI     = WC_cm * TG_mmol,
    stroke_base_na = is.na(da007_8_),
    stroke_base = as.numeric(da007_8_) == 1,
    stroke_base = if_else(is.na(stroke_base), FALSE, stroke_base)
  ) %>%
  filter(!is.na(WTI))
logline(sprintf("CHARLS 2011 complete cases (TG+WC): %d (expect 9870)", nrow(c11)))
logline(sprintf("CHARLS 2011 baseline stroke NA (unasked/branching): %d", sum(c11$stroke_base_na)))
logline(sprintf("CHARLS 2011 baseline stroke (NA->FALSE): %d (expect 220)", sum(c11$stroke_base)))
stopifnot(nrow(c11) == 9870, sum(c11$stroke_base) == 220)

# ---------------------------------------------------------------------------
# 3. CHARLS prospective layer: 2011 baseline (no stroke) -> 2018 incident stroke
# ---------------------------------------------------------------------------
logline("\n=== STEP 3: CHARLS prospective layer (2011 -> 2018) ===")
hs18$ID_12 <- str_trim(as.character(hs18$ID))
prosp <- c11 %>%
  filter(!stroke_base) %>%
  left_join(hs18 %>%
              select(ID_12, da007_8_, da019_w2_1) %>%
              rename(stroke_hist_2018 = da007_8_, stroke_recent_2018 = da019_w2_1),
            by = "ID_12") %>%
  mutate(
    stroke_2018 = (as.numeric(stroke_hist_2018) == 1) | (as.numeric(stroke_recent_2018) == 1),
    stroke_2018 = if_else(is.na(stroke_2018), FALSE, stroke_2018)
  )
logline(sprintf("CHARLS prospective: baseline stroke-free n=%d (expect 9650)", nrow(prosp)))
logline(sprintf("CHARLS incident stroke by 2018: %d (expect ~485)", sum(prosp$stroke_2018)))
stopifnot(nrow(prosp) == 9650)

# ---------------------------------------------------------------------------
# 4. CHARLS repeated TG (2011 x 2015)
# ---------------------------------------------------------------------------
logline("\n=== STEP 4: CHARLS repeated TG ===")
b15$ID_12 <- str_trim(as.character(b15$ID))
rep_tg <- b11 %>%
  select(ID_12, newtg) %>%
  inner_join(b15 %>% select(ID_12, bl_tg), by = "ID_12")
logline(sprintf("Repeated TG pairs 2011x2015: %d (expect 7648)", nrow(rep_tg)))
stopifnot(nrow(rep_tg) == 7648)

# ---------------------------------------------------------------------------
# 5. NHANES fasting subsample cross-sectional layer (D-J, 7 cycles)
# ---------------------------------------------------------------------------
logline("\n=== STEP 5: NHANES fasting subsample (D-J) ===")
cycles <- list(D = "D", E = "E", F = "F", G = "G", H = "H", I = "I", J = "J")
nh <- lapply(names(cycles), function(cy) {
  demo  <- read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy)))
  bmx   <- read_xpt(file.path(NRAW, sprintf("BMX_%s.XPT", cy)))
  mcq   <- read_xpt(file.path(NRAW, sprintf("MCQ_%s.XPT", cy)))
  tri   <- read_xpt(file.path(NRAW, sprintf("TRIGLY_%s.XPT", cy)))
  wcol  <- grep("WTSAF", names(tri), value = TRUE)[1]
  demo %>%
    select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1) %>%
    inner_join(bmx %>% select(SEQN, BMXWAIST), by = "SEQN") %>%
    inner_join(mcq %>% select(SEQN, MCQ160F), by = "SEQN") %>%
    inner_join(tri %>% select(SEQN, LBXTR, all_of(wcol)), by = "SEQN") %>%
    rename(WTSAF = all_of(wcol)) %>%
    mutate(CYCLE = cy)
})
nh <- bind_rows(nh) %>%
  mutate(
    fasting = as.numeric(WTSAF) > 0,
    TG_mmol = as.numeric(LBXTR) * 0.01129,
    WTI     = as.numeric(BMXWAIST) * TG_mmol,
    stroke  = as.numeric(MCQ160F) == 1
  ) %>%
  filter(fasting, !is.na(WTI), as.numeric(RIDAGEYR) >= 40)
logline(sprintf("NHANES fasting subsample complete cases: %d (expect 10302)", nrow(nh)))
logline(sprintf("NHANES stroke events: %d (MCQ160F; was 641 under erroneous MCQ160E)", sum(nh$stroke)))
stopifnot(nrow(nh) == 10302)   # stroke-event count re-derived 2026-08-16 (P0 fix MCQ160E->F)

# ---------------------------------------------------------------------------
# 6. Export
# ---------------------------------------------------------------------------
write_csv(c11 %>% select(ID_12, WC_cm, TG_mmol, WTI, stroke_base),
          file.path(OUT, "charls_2011_cross.csv"))
write_csv(prosp %>% select(ID_12, WC_cm, TG_mmol, WTI, stroke_base, stroke_2018),
          file.path(OUT, "charls_2011_2018_prosp.csv"))
write_csv(rep_tg, file.path(OUT, "charls_tg_repeated_2011_2015.csv"))
write_csv(nh, file.path(OUT, "nhanes_fasting_cross_DJ.csv"))

logline("\n=== ALL CHECKS PASSED ===")
close(logf)
