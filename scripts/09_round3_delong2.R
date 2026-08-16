# DeLong p cross-validation: pROC roc.test method=delong vs method=bootstrap
suppressPackageStartupMessages({library(pROC); library(dplyr); library(readr)})
set.seed(42)
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed")

nh <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov.csv"), show_col_types = FALSE)
f0 <- glm(stroke ~ RIDAGEYR + RIAGENDR, data = nh, family = binomial())
f1 <- glm(stroke ~ WTI + RIDAGEYR + RIAGENDR, data = nh, family = binomial())
r0 <- roc(nh$stroke, predict(f0, type = "response"), quiet = TRUE)
r1 <- roc(nh$stroke, predict(f1, type = "response"), quiet = TRUE)
cat("NHANES DeLong p:", roc.test(r0, r1, method = "delong")$p.value,
    "| bootstrap p:", roc.test(r0, r1, method = "bootstrap", boot.n = 2000)$p.value, "\n")

ch <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE) %>%
  mutate(age = as.numeric(age), sex = as.numeric(sex)) %>%
  filter(!is.na(age) & !is.na(bmi) & !is.na(sex))
g0 <- glm(stroke_base ~ age + sex, data = ch, family = binomial())
g1 <- glm(stroke_base ~ WTI + age + sex, data = ch, family = binomial())
s0 <- roc(ch$stroke_base, predict(g0, type = "response"), quiet = TRUE)
s1 <- roc(ch$stroke_base, predict(g1, type = "response"), quiet = TRUE)
cat("CHARLS DeLong p:", roc.test(s0, s1, method = "delong")$p.value,
    "| bootstrap p:", roc.test(s0, s1, method = "bootstrap", boot.n = 2000)$p.value, "\n")
