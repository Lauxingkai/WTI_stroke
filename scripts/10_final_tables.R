# ============================================================
# 10_final_tables.R — 论文结果表格定稿（死亡分析队列）
# Table 1: 死亡队列加权基线（按存活/死亡分层）
# Table 2: 主分析（每SD + 四分位 + P for trend）
# Table 3: 亚组 + 敏感性汇总
# 输出: output/Table1_mortality.csv / Table2_mortality.csv / Table3_mortality.csv
# ============================================================

suppressPackageStartupMessages({
  library(survey); library(survival); library(dplyr); library(tidyr)
})
options(survey.lonely.psu = "adjust")
out_dir <- "output"

# ---------- 死亡队列 ----------
mc <- readRDS("data/analysis/master_cc.RDS")
d <- mc %>% filter(cycle_sfx %in% c("C","D","E","F","G","H","I","J")) %>% mutate(wt8 = wt_2yr / 8)
mort <- readRDS("data/mortality/LMF2019_public_CJ.rds")
mort$seqn <- as.numeric(mort$seqn)
d <- d %>% left_join(mort, by = c("SEQN" = "seqn")) %>%
  filter(eligstat == 1 & !is.na(permth_int) & permth_int > 0)
d$mort_f <- factor(d$mortstat, levels = 0:1, labels = c("存活","死亡"))
d$vitk_sd_diet <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)
mk <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt8, data = dat, nest = TRUE)
dsn <- mk(d)

# ---------- Table 1: 加权基线（按死亡分层）----------
cont_vars <- c("age","BMI","PIR","TC","HDL","kcal","vitk_diet","satfat","chol_int","permth_int")
cat_vars  <- c("sex","race","education","marital","smoking","hypertension","diabetes","CHD")
rows <- list()
for (v in cont_vars) {
  by <- svyby(as.formula(paste0("~", v)), ~mort_f, dsn, svymean, na.rm = TRUE)
  se_col <- if ("se" %in% names(by)) "se" else grep("^se", names(by), value = TRUE)[1]
  rows[[v]] <- data.frame(variable = v, level = "",
    `存活` = sprintf("%.1f (%.1f)", by[[v]][1], by[[se_col]][1]),
    `死亡` = sprintf("%.1f (%.1f)", by[[v]][2], by[[se_col]][2]),
    check.names = FALSE)
}
for (v in cat_vars) {
  tbl <- svytable(as.formula(paste0("~", v, "+ mort_f")), dsn)
  pct <- 100 * prop.table(tbl, 2)
  for (lv in rownames(pct)) {
    rows[[paste0(v, ".", lv)]] <- data.frame(variable = v, level = lv,
      `存活` = sprintf("%.1f", pct[lv, "存活"]), `死亡` = sprintf("%.1f", pct[lv, "死亡"]),
      check.names = FALSE)
  }
}
t1 <- bind_rows(rows)
# 随访与事件描述行
t1 <- rbind(data.frame(variable = "随访月数", level = "",
                       `存活` = sprintf("%.1f", median(d$permth_int[d$mortstat == 0])),
                       `死亡` = sprintf("%.1f", median(d$permth_int[d$mortstat == 1])),
                       check.names = FALSE), t1)
write.csv(t1, file.path(out_dir, "Table1_mortality.csv"), row.names = FALSE)
cat("Table1 完成:", nrow(t1), "行\n")

# ---------- Table 2: 主分析（每SD + 四分位）----------
covars_m2 <- c("age","sex","race","education","PIR","marital")
covars_m3 <- c(covars_m2, "BMI","smoking","hypertension","diabetes","CHD",
               "TC","HDL","kcal","satfat","chol_int")
m2_form <- paste(covars_m2, collapse = " + ")
m3_form <- paste(covars_m3, collapse = " + ")

fit_models <- function(outcome = "mortstat", time = "permth_int") {
  rows <- list()
  for (nm in c("M1","M2","M3")) {
    f <- if (nm == "M1") as.formula(paste0("Surv(", time, ", ", outcome, ") ~ vitk_sd_diet"))
         else as.formula(paste0("Surv(", time, ", ", outcome, ") ~ vitk_sd_diet + ",
                                get(paste0("m", substring(nm, 2), "_form"))))
    fit <- svycoxph(f, design = dsn)
    ct <- summary(fit)$coefficients
    ci <- exp(confint(fit)["vitk_sd_diet", ])
    rows[[nm]] <- data.frame(model = nm,
      HR = sprintf("%.3f", exp(coef(fit)["vitk_sd_diet"])),
      LCL = sprintf("%.3f", ci[1]), UCL = sprintf("%.3f", ci[2]),
      p = signif(ct["vitk_sd_diet", ncol(ct)], 3))
  }
  bind_rows(rows)
}

# 四分位（M3 剂量反应 + trend）
q4 <- quantile(d$vitk_diet, c(0.25, 0.5, 0.75), na.rm = TRUE)
d$q4d <- cut(d$vitk_diet, breaks = c(-Inf, q4, Inf), labels = FALSE)
d$q4med <- ave(d$vitk_diet, d$q4d, FUN = function(x) median(x, na.rm = TRUE))
dsn2 <- mk(d)
rowsq <- list()
for (nm in c("M1","M2","M3")) {
  f <- if (nm == "M1") Surv(permth_int, mortstat) ~ factor(q4d)
       else as.formula(paste0("Surv(permth_int, mortstat) ~ factor(q4d) + ",
                              get(paste0("m", substring(nm, 2), "_form"))))
  fit <- svycoxph(f, design = dsn2)
  ct <- summary(fit)$coefficients
  for (k in 2:4) {
    term <- paste0("factor(q4d)", k)
    ci <- exp(confint(fit)[term, ])
    rowsq[[paste(nm, k)]] <- data.frame(model = nm, quartile = paste0("Q", k),
      HR = sprintf("%.3f", exp(coef(fit)[term])), LCL = sprintf("%.3f", ci[1]),
      UCL = sprintf("%.3f", ci[2]), p = signif(ct[term, ncol(ct)], 3))
  }
}
tq <- bind_rows(rowsq)
# trend
fit_tr <- svycoxph(Surv(permth_int, mortstat) ~ q4med + age + sex + race + education +
                     PIR + marital + BMI + smoking + hypertension + diabetes + CHD +
                     TC + HDL + kcal + satfat + chol_int, design = dsn2)
ct <- summary(fit_tr)$coefficients
t2 <- list(continuous = fit_models(), quartile = tq,
           trend_p = signif(ct["q4med", ncol(ct)], 3))
write.csv(bind_rows(t2$continuous, t2$quartile), file.path(out_dir, "Table2_mortality.csv"),
          row.names = FALSE)
cat("Table2 完成; P for trend =", t2$trend_p, "\n")

# ---------- Table 3: 敏感性 + 亚组汇总 ----------
# 敏感性（每SD, M3）
sens_rows <- list()
sens_run <- function(dat, label) {
  fit <- svycoxph(Surv(permth_int, mortstat) ~ vitk_sd_diet + age + sex + race + education +
                    PIR + marital + BMI + smoking + hypertension + diabetes + CHD +
                    TC + HDL + kcal + satfat + chol_int, design = mk(dat))
  ct <- summary(fit)$coefficients; ci <- exp(confint(fit)["vitk_sd_diet", ])
  data.frame(sensitivity = label, HR = sprintf("%.3f", exp(coef(fit)["vitk_sd_diet"])),
             LCL = sprintf("%.3f", ci[1]), UCL = sprintf("%.3f", ci[2]),
             p = signif(ct["vitk_sd_diet", ncol(ct)], 3))
}
sens_rows[[1]] <- sens_run(d, "基线（全部）")
sens_rows[[2]] <- sens_run(d %>% filter(permth_int >= 24), "排除随访<24月")
sens_rows[[3]] <- sens_run(d %>% filter(is.na(MCQ160E) | MCQ160E != 1), "排除基线卒中")
d$base_cvd <- pmax(d$MCQ160B, d$MCQ160C, d$MCQ160D, d$MCQ160E, na.rm = TRUE)
sens_rows[[4]] <- sens_run(d %>% filter(is.na(base_cvd) | base_cvd != 1), "排除基线CVD")
sens_rows[[5]] <- sens_run(d %>% filter(kcal >= 800 & kcal <= 4000), "能量<800/>4000")
d$vitk_sd_single <- d$DR1TVK / sd(d$DR1TVK, na.rm = TRUE)
sens_rows[[6]] <- {
  d2 <- d; d2$vitk_sd_diet <- d2$vitk_sd_single
  sens_run(d2, "单日回顾暴露")
}
t3 <- bind_rows(sens_rows)
write.csv(t3, file.path(out_dir, "Table3_mortality.csv"), row.names = FALSE)
cat("Table3 完成:", nrow(t3), "行\n")
message("== 10_final_tables 完成 ==")
