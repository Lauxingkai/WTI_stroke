# verify NHANES DeLong p on the 03b discrimination analytic sample (TyG/ABSI complete)
suppressPackageStartupMessages({library(pROC); library(dplyr); library(readr); library(haven)})
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw"); OUT <- file.path(RAW, "data/processed")
nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov.csv"), show_col_types = FALSE)
glu <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  read_xpt(file.path(NRAW, sprintf("GLU_%s.XPT", cy))) %>%
    transmute(SEQN, LBXGLU = as.numeric(LBXGLU), CYCLE = cy)
}) %>% bind_rows()
bmx <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  read_xpt(file.path(NRAW, sprintf("BMX_%s.XPT", cy))) %>%
    transmute(SEQN, BMXHT = as.numeric(BMXHT), CYCLE = cy)
}) %>% bind_rows()
nh <- nh %>%
  left_join(glu, by = c("SEQN", "CYCLE.x" = "CYCLE")) %>%
  left_join(bmx, by = c("SEQN", "CYCLE.x" = "CYCLE")) %>%
  mutate(TyG = log(TG_mmol * LBXGLU * 0.0555 / 2),
         ABSI = (BMXWAIST / 100) / (bmi^(2/3) * (BMXHT / 100)^0.5)) %>%
  filter(!is.na(TyG) & !is.na(ABSI))
f0 <- glm(stroke ~ RIDAGEYR + RIAGENDR, data = nh, family = binomial())
f1 <- glm(stroke ~ WTI + RIDAGEYR + RIAGENDR, data = nh, family = binomial())
r0 <- roc(nh$stroke, predict(f0, type = "response"), quiet = TRUE)
r1 <- roc(nh$stroke, predict(f1, type = "response"), quiet = TRUE)
cat("n =", nrow(nh), "| NHANES DeLong p (03b sample):", roc.test(r0, r1, method = "delong")$p.value,
    "| 03b ref: 0.333\n")
cat("AUC:", as.numeric(auc(r1)), "| 03b ref: 0.689\n")
