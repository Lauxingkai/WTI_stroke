# ============================================================
# 19_table1_ek.R — 主分析队列（E-K, 总摄入, 7周期）基线表
# 背景: 03_table1.R 基线表基于 C-K 9 周期(含 C/D 无补充剂者),
#       与 04_analysis.R 主分析队列(E-K 7周期, vitk_total_ok)口径不一致。
#       本脚本按主分析队列口径重新生成基线表。
# 输入: data/analysis/master_cc.RDS, master_pre_cc.RDS
# 输出: output/Table1_EK_加权基线.csv, Table1_EK_按卒中分层.csv, S1_EK_排除比较.csv
# ============================================================

suppressPackageStartupMessages({
  library(survey)
  library(dplyr)
  library(tidyr)
})

options(survey.lonely.psu = "adjust")
out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

master <- readRDS("data/analysis/master_cc.RDS")
pre_cc <- readRDS("data/analysis/master_pre_cc.RDS")

# 主分析队列: E-K 总摄入完整案例, wt = wt_2yr/7 (K 周期 wt_2yr 已在 02_clean 替换为 WTMECPRP)
master <- master %>% filter(vitk_total_ok & cycle_sfx != "L") %>%
  mutate(wt7 = wt_2yr / 7)

design <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt7,
                    data = master, nest = TRUE)

cont_vars <- c("age","BMI","PIR","TC","HDL","kcal","vitk_total","vitk_diet",
               "vitk_supp","satfat","chol_int")
cat_vars  <- c("sex","race","education","marital","smoking","alcohol",
               "hypertension","diabetes","CHD","supp_user","stroke")

table1 <- function(dsn, group_var = NULL) {
  rows <- list()
  for (v in cont_vars) {
    if (is.null(group_var)) {
      m <- svymean(as.formula(paste0("~", v)), dsn, na.rm = TRUE)
      rows[[v]] <- data.frame(variable = v, level = "",
        overall = sprintf("%.1f (%.1f)", coef(m)[1], SE(m)[1]), stringsAsFactors = FALSE)
    } else {
      f <- as.formula(paste0("~", v)); g <- as.formula(paste0("~", group_var))
      by <- svyby(f, g, dsn, svymean, na.rm = TRUE)
      est_col <- v
      se_col <- if ("se" %in% names(by)) "se" else grep("^se", names(by), value = TRUE)[1]
      est <- by[[est_col]]; se <- by[[se_col]]
      rows[[v]] <- data.frame(variable = v, level = "",
        overall = paste(sprintf("%.1f (%.1f)", est, se), collapse = " | "),
        stringsAsFactors = FALSE)
    }
  }
  for (v in cat_vars) {
    tbl <- svytable(as.formula(paste0("~", v)), dsn)
    pct <- 100 * prop.table(tbl)
    for (j in seq_along(pct)) {
      rows[[paste0(v, ".", names(pct)[j])]] <- data.frame(
        variable = v, level = names(pct)[j],
        overall = sprintf("%.1f", pct[j]), stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

t1 <- table1(design)
write.csv(t1, file.path(out_dir, "Table1_EK_加权基线.csv"), row.names = FALSE)

design2 <- update(design, stroke_f = factor(stroke, levels = 0:1, labels = c("无卒中","卒中")))
t1_by <- table1(design2, group_var = "stroke_f")

pvals <- c()
for (v in cont_vars) {
  tt <- tryCatch(svyttest(as.formula(paste0(v, " ~ stroke_f")), design2),
                 error = function(e) NULL)
  pvals[v] <- if (is.null(tt)) NA else format.pval(tt$p.value, digits = 3)
}
for (v in cat_vars) {
  cs <- tryCatch(svychisq(as.formula(paste0("~", v, " + stroke_f")), design2),
                 error = function(e) NULL)
  pvals[v] <- if (is.null(cs)) NA else format.pval(cs$p.value, digits = 3)
}
pval_df <- data.frame(variable = names(pvals), p = unlist(pvals), stringsAsFactors = FALSE)
t1_final <- left_join(t1_by, pval_df, by = "variable")
write.csv(t1_final, file.path(out_dir, "Table1_EK_按卒中分层.csv"), row.names = FALSE)

# S1: 排除 vs 纳入（E-K 主队列口径）
pre_cc$cohort <- factor(ifelse(pre_cc$SEQN %in% master$SEQN, 1, 0),
                        levels = 0:1, labels = c("排除","纳入"))
S1 <- lapply(cont_vars, function(v) {
  g <- pre_cc %>% group_by(cohort) %>%
    summarise(mu = mean(.data[[v]], na.rm = TRUE),
              sd = sd(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
    mutate(stat = sprintf("%.1f (%.1f)", mu, sd)) %>%
    select(cohort, stat) %>% pivot_wider(names_from = cohort, values_from = stat)
  data.frame(variable = v, g)
}) %>% bind_rows()
S1 <- S1 %>% mutate(across(everything(), ~ replace(., is.na(.), "NA")))
write.csv(S1, file.path(out_dir, "S1_EK_排除比较.csv"), row.names = FALSE)

cat("Table1_EK 完成: 主队列 n =", nrow(master), "| 卒中", sum(master$stroke == 1), "\n")
