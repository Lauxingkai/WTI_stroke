# ============================================================================
# 05g_mice2011.R — M-1: CHARLS 2011 cross/prosp M2/M3 多重插补敏感性（2026-09-02）
# mice(m=20, PMM, seed=42)，设计=communityID/urban_nbs/bloodweight 归一化
# 输出: results/05g_mice2011_models.csv ; results/05g_mice2011_checks.txt
# ============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(mice); library(survey) })
set.seed(42)
OUT <- "D:/NHANES/data/processed"; RES <- "D:/NHANES/results"
logf <- file(file.path(RES, "05g_mice2011_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

run_layer <- function(df, outc, label) {
  dd <- df %>% mutate(
    WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
    y = as.numeric(!!sym(outc)),
    sex_m = ifelse(sex == 1, 1, 0),
    w_norm = bloodweight / mean(bloodweight, na.rm = TRUE)
  ) %>% filter(!is.na(bloodweight) & bloodweight > 0 & !is.na(age) & !is.na(sex_m) &
                 !is.na(WTI_sd) & !is.na(y))
  vars_m3 <- c("y","WTI_sd","age","sex_m","edu","smoke","drink","bmi","htn","dm","lipid_rx","bp_rx","pa_days_week")
  miss <- sapply(dd[vars_m3], function(x) 100*mean(is.na(x)))
  logline(sprintf("[%s] n=%d events=%d | missing%%: %s", label, nrow(dd), sum(dd$y),
                  paste(sprintf("%s=%.1f", names(miss), miss), collapse = " ")))
  imp <- mice(dd[vars_m3], m = 20, method = "pmm", seed = 42, printFlag = FALSE)
  pool_fit <- function(f) {
    bs <- ses <- numeric(20)
    for (k in 1:20) {
      dk <- cbind(complete(imp, k), dd[c("communityID","urban_nbs","w_norm")])
      des <- svydesign(ids = ~communityID, strata = ~urban_nbs, weights = ~w_norm, data = dk, nest = TRUE)
      fit <- svyglm(f, family = quasibinomial(), design = des)
      bs[k] <- coef(fit)["WTI_sd"]; ses[k] <- sqrt(vcov(fit)["WTI_sd","WTI_sd"])
    }
    Q <- mean(bs); U <- mean(ses^2); Bk <- var(bs); T <- U + (1 + 1/20)*Bk
    c(est = exp(Q), lo = exp(Q - 1.96*sqrt(T)), hi = exp(Q + 1.96*sqrt(T)),
      p = 2*pnorm(-abs(Q/sqrt(T))), n = length(fits_n(dd)))
  }
  fits_n <- function(d2) seq_len(nrow(d2))
  m2 <- pool_fit(as.formula("y ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi"))
  m3 <- pool_fit(as.formula("y ~ WTI_sd + age + sex_m + edu + smoke + drink + bmi + htn + dm + lipid_rx + bp_rx + pa_days_week"))
  logline(sprintf("[%s] MI M2: OR=%.3f (%.3f-%.3f) p=%.4f", label, m2[1], m2[2], m2[3], m2[4]))
  logline(sprintf("[%s] MI M3: OR=%.3f (%.3f-%.3f) p=%.4f", label, m3[1], m3[2], m3[3], m3[4]))
  data.frame(layer = label, model = c("M2","M3"),
             est = c(m2[1], m3[1]), lo = c(m2[2], m3[2]), hi = c(m2[3], m3[3]), p = c(m2[4], m3[4]))
}
cross <- read_csv(file.path(OUT, "charls_2011_cross_cov.csv"), show_col_types = FALSE)
prosp <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
res <- bind_rows(run_layer(cross, "stroke_base", "cross2011"),
                 run_layer(prosp, "stroke_2018", "prosp2011"))
write_csv(res, file.path(RES, "05g_mice2011_models.csv"))
close(logf)
cat("MICE done")
