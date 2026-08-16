# ============================================================================
# 13o_ndi_strata_cycle.R  (reviewer patch C)
# NDI Cox sensitivity: cycle-specific baseline hazards via strata(CYCLE)
# in svycoxph (main models keep cycle only in the design strata).
# Also records PH (cox.zph, unweighted) + EPV for the supplementary note.
# Output: results/13o_ndi_strata_cycle.csv ; qc/13o_ndi_ph_epv.txt
# ============================================================================
suppressPackageStartupMessages({
  library(survey); library(survival); library(haven); library(dplyr); library(readr)
})
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw")
OUT <- file.path(RAW, "data"); PROC <- file.path(RAW, "data/processed")
RES <- file.path(RAW, "results"); QC <- file.path(RAW, "qc")

nh <- read_csv(file.path(PROC, "nhanes_fasting_cross_cov.csv"), show_col_types = FALSE)
mort <- read_csv(file.path(OUT, "nhanes_mort2019.csv"), show_col_types = FALSE) %>%
  select(seqn, eligstat, mortstat, ucod_leading, permth_int)
des <- lapply(c("D", "E", "F", "G", "H", "I", "J"), function(cy) {
  demo <- read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy)))
  demo %>% transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
})
des <- bind_rows(des)
nh <- nh %>% left_join(mort, by = c("SEQN" = "seqn")) %>%
  left_join(des, by = c("SEQN" = "SEQN", "CYCLE.x" = "CYCLE")) %>%
  rename(CYCLE = CYCLE.x) %>%
  mutate(wt = WTSAF / 7,
         psu = paste0(CYCLE, "_", SDMVPSU),
         stra = paste0(CYCLE, "_", SDMVSTRA),
         WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
         pa_ter = cut(pa_min_day, quantile(pa_min_day, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                      include.lowest = TRUE, labels = c("L", "M", "H")),
         time_y = permth_int / 12,
         death = ifelse(mortstat == 1, 1, 0),
         stroke_evt = ifelse(mortstat == 1 & ucod_leading == 5, 1,
                             ifelse(is.na(mortstat), NA, 0))) %>%
  filter(!is.na(permth_int), !is.na(mortstat), !is.na(WTI), !is.na(RIDAGEYR),
         !is.na(RIAGENDR), !is.na(SDMVPSU), !is.na(SDMVSTRA))
nhd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)

extr <- function(fit) {
  b <- coef(fit)["WTI_sd"]
  se <- sqrt(vcov(fit)["WTI_sd", "WTI_sd"])
  c(est = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se),
    p = 2 * pnorm(-abs(b / se)))
}

res <- list()
add <- function(layer, model, e) {
  res[[length(res) + 1]] <<- data.frame(
    layer, model, est = e[1], lo = e[2], hi = e[3], p = e[4])
}
m1a <- svycoxph(Surv(time_y, death) ~ WTI_sd + RIDAGEYR + RIAGENDR + strata(CYCLE), design = nhd)
m3a <- svycoxph(Surv(time_y, death) ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu +
                  smoke + drink + bmi + htn + dm + statin + bp_rx + pa_ter +
                  strata(CYCLE), design = nhd)
s1a <- svycoxph(Surv(time_y, stroke_evt) ~ WTI_sd + RIDAGEYR + RIAGENDR + strata(CYCLE), design = nhd)
s3a <- svycoxph(Surv(time_y, stroke_evt) ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu +
                  smoke + drink + bmi + htn + dm + statin + bp_rx + pa_ter +
                  strata(CYCLE), design = nhd)
for (nm in c("m1a", "m3a", "s1a", "s3a")) {
  e <- extr(get(nm))
  add(ifelse(startsWith(nm, "m"), "all-cause", "stroke-death"),
      paste0("strata-cycle-", toupper(substr(nm, 1, 2))), e)
  cat(sprintf("%s: HR=%.3f (%.3f-%.3f) p=%.4f\n", nm, e[1], e[2], e[3], e[4]))
}
write_csv(bind_rows(res), file.path(RES, "13o_ndi_strata_cycle.csv"))

# PH + EPV record (unweighted coxph as approximation)
lines <- character(0)
rec <- function(out, tag) {
  m1 <- coxph(as.formula(sprintf("Surv(time_y, %s) ~ WTI_sd + RIDAGEYR + RIAGENDR", out)), data = nh)
  m3 <- coxph(as.formula(sprintf("Surv(time_y, %s) ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + edu + smoke + drink + bmi + htn + dm + statin + bp_rx + pa_ter", out)), data = nh)
  z1 <- cox.zph(m1); z3 <- cox.zph(m3)
  ev <- sum(nh[[out]] == 1, na.rm = TRUE)
  lines <<- c(lines,
    sprintf("%s M1: global PH p=%.3f | WTI PH p=%.3f", tag,
            z1$table[nrow(z1$table), "p"], z1$table["WTI_sd", "p"]),
    sprintf("%s M3: global PH p=%.3f | WTI PH p=%.3f", tag,
            z3$table[nrow(z3$table), "p"], z3$table["WTI_sd", "p"]),
    sprintf("%s: events=%d | M3 params=%d -> EPV=%.1f", tag, ev,
            ncol(model.matrix(m3)) - 1, ev / (ncol(model.matrix(m3)) - 1)))
}
rec("death", "all-cause")
rec("stroke_evt", "stroke-death")
writeLines(lines, file.path(QC, "13o_ndi_ph_epv.txt"))
cat(paste(lines, collapse = "\n"), "\n")
cat("=== 13o DONE ===\n")
