# ============================================================================
# 03d_domain_check.R — D-1: NHANES 域设计对照（PA 重跑配套，2026-09-02）
# 对比：A) 现行=行删后建 svydesign；B) 规范=full design + subset(design)
# 输入: data/raw/{DEMO,BMX,MCQ,TRIGLY}_{D..J}.XPT
#        data/processed/nhanes_fasting_cross_cov_v2.csv
# 输出: results/03d_domain_check.csv ; results/03d_domain_check.txt
# ============================================================================
suppressPackageStartupMessages({ library(survey); library(haven); library(dplyr); library(readr) })
set.seed(42)
RAW <- "D:/NHANES"; NRAW <- file.path(RAW, "data/raw")
OUT <- file.path(RAW, "data/processed"); RES <- file.path(RAW, "results")
logf <- file(file.path(RES, "03d_domain_check.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

# ---- full fasting set (WTSAF>0 & age>=40, 含 PA 缺失者) ----
full <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  demo <- read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy)))
  bmx  <- read_xpt(file.path(NRAW, sprintf("BMX_%s.XPT", cy)))
  mcq  <- read_xpt(file.path(NRAW, sprintf("MCQ_%s.XPT", cy)))
  tri  <- read_xpt(file.path(NRAW, sprintf("TRIGLY_%s.XPT", cy)))
  wcol <- grep("WTSAF", names(tri), value = TRUE)[1]
  demo %>% select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1) %>%
    inner_join(bmx %>% select(SEQN, BMXWAIST), by = "SEQN") %>%
    inner_join(mcq %>% select(SEQN, MCQ160F), by = "SEQN") %>%
    inner_join(tri %>% select(SEQN, LBXTR, all_of(wcol)), by = "SEQN") %>%
    rename(WTSAF = all_of(wcol)) %>% mutate(CYCLE = cy)
}) %>% bind_rows() %>%
  mutate(fasting = as.numeric(WTSAF) > 0,
         TG_mmol = as.numeric(LBXTR) * 0.01129,
         WTI = as.numeric(BMXWAIST) * TG_mmol,
         stroke = as.numeric(MCQ160F) == 1) %>%
  filter(fasting, as.numeric(RIDAGEYR) >= 40)
logline(sprintf("full fasting set (>=40y): n=%d", nrow(full)))

# 周期设计变量
des <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  read_xpt(file.path(NRAW, sprintf("DEMO_%s.XPT", cy))) %>%
    transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
}) %>% bind_rows()
full <- full %>% left_join(des, by = c("SEQN","CYCLE")) %>%
  mutate(wt = WTSAF / 7,
         psu = paste0(CYCLE, "_", SDMVPSU),
         stra = paste0(CYCLE, "_", SDMVSTRA))

# PA + 协变量 (v2)
pa <- read_csv(file.path(OUT, "nhanes_fasting_cross_cov_v2.csv"), show_col_types = FALSE) %>%
  select(SEQN, CYCLE = CYCLE.x, pa_mvpaw_min, edu, smoke, drink, bmi, htn, dm, statin, bp_rx)
full <- full %>% left_join(pa, by = c("SEQN","CYCLE"))
full <- full %>% mutate(WTI_sd = (WTI - mean(WTI, na.rm = TRUE)) / sd(WTI, na.rm = TRUE),
  pa_ter = cut(pa_mvpaw_min, quantile(pa_mvpaw_min, c(0,1/3,2/3,1), na.rm = TRUE),
               include.lowest = TRUE, labels = c("L","M","H")))

# A) 行删后建 design（现行做法）
a <- full %>% filter(!is.na(WTI), !is.na(stroke))
ada <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = a, nest = TRUE)
m1a <- svyglm(stroke ~ WTI_sd + RIDAGEYR + RIAGENDR, family = quasibinomial(), design = ada)
m3a <- svyglm(stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + factor(edu) + smoke + drink + bmi
              + htn + dm + statin + bp_rx + pa_ter, family = quasibinomial(), design = ada)

# B) full design + subset（规范）
bf <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = full, nest = TRUE)
bd <- subset(bf, !is.na(WTI) & !is.na(stroke))
m1b <- svyglm(stroke ~ WTI_sd + RIDAGEYR + RIAGENDR, family = quasibinomial(), design = bd)
m3b <- svyglm(stroke ~ WTI_sd + RIDAGEYR + RIAGENDR + RIDRETH1 + factor(edu) + smoke + drink + bmi
              + htn + dm + statin + bp_rx + pa_ter, family = quasibinomial(), design = bd)

extr <- function(fit) {
  cc <- summary(fit)$coefficients
  rn <- rownames(cc)[grepl("WTI_sd", rownames(cc))][1]
  if (is.na(rn)) stop("WTI_sd coefficient not found; names: ", paste(rownames(cc), collapse = ","))
  b <- cc[rn, "Estimate"]; se <- cc[rn, "Std. Error"]
  c(est = exp(b), se = se, lo = exp(b - 1.96*se), hi = exp(b + 1.96*se), p = 2*pnorm(-abs(b/se)))
}
e1a <- extr(m1a); e3a <- extr(m3a); e1b <- extr(m1b); e3b <- extr(m3b)
out <- data.frame(
  model = c("M1","M1","M3","M3"),
  method = c("A:rowdrop","B:subset","A:rowdrop","B:subset"),
  n = c(nrow(a), nrow(bd), nrow(a), nrow(bd)),
  est = c(e1a["est"], e1b["est"], e3a["est"], e3b["est"]),
  se = c(e1a["se"], e1b["se"], e3a["se"], e3b["se"]),
  lo = c(e1a["lo"], e1b["lo"], e3a["lo"], e3b["lo"]),
  hi = c(e1a["hi"], e1b["hi"], e3a["hi"], e3b["hi"]),
  p = c(e1a["p"], e1b["p"], e3a["p"], e3b["p"]))
logline("=== domain-design comparison ===")
for (i in seq_len(nrow(out))) logline(sprintf("%s %s: OR=%.4f (%.4f-%.4f) p=%.4f | SE=%.4f",
  out$method[i], out$model[i], out$est[i], out$lo[i], out$hi[i], out$p[i], out$se[i]))
write_csv(out, file.path(RES, "03d_domain_check.csv"))
logline("=== done ===")
close(logf)
cat("domain check done:", nrow(out), "rows")
