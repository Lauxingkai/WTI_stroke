# ============================================================
# 03_table1.R — 加权基线特征表 + 排除比较表（Table1 / S1）v2 适配版
# 输入: data/analysis/master_cc.RDS（主分析完整案例）、master_pre_cc.RDS
# 主分析样本: 2003-2020（C-K，9 周期），权重 wt9 = wt_2yr/9（K 用 WTMECPRP）
# 输出: output/Table1_加权基线.csv、Table1_按卒中分层.csv、S1_排除比较.csv
# ============================================================

suppressPackageStartupMessages({
  library(survey)
  library(dplyr)
  library(tidyr)
})

options(survey.lonely.psu = "adjust")
out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

master  <- readRDS("data/analysis/master_cc.RDS")
pre_cc  <- readRDS("data/analysis/master_pre_cc.RDS")

# 主分析样本：C-K（2003-2020），权重 /9
main_cycles <- c("C","D","E","F","G","H","I","J","K")
master <- master %>% filter(cycle_sfx %in% main_cycles) %>%
  mutate(wt9 = wt_2yr / 9)

# ---------- 1. 调查设计 ----------
design <- svydesign(
  ids     = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~wt9,
  data    = master,
  nest    = TRUE
)

# ---------- 2. 加权描述 ----------
cont_vars <- c("age","BMI","PIR","TC","HDL","kcal","vitk_total","vitk_diet",
               "vitk_supp","satfat","chol_int")
cat_vars  <- c("sex","race","education","marital","smoking","alcohol",
               "hypertension","diabetes","CHD","supp_user","stroke")

table1 <- function(dsn, group_var = NULL) {
  rows <- list()
  for (v in cont_vars) {
    m <- svymean(as.formula(paste0("~", v)), dsn, na.rm = TRUE)
    if (is.null(group_var)) {
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
write.csv(t1, file.path(out_dir, "Table1_加权基线.csv"), row.names = FALSE)

# ---------- 3. 按卒中分层 + 组间检验 ----------
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
write.csv(t1_final, file.path(out_dir, "Table1_按卒中分层.csv"), row.names = FALSE)

# ---------- 4. S1 排除 vs 纳入比较（未加权，STROBE）----------
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
write.csv(S1, file.path(out_dir, "S1_排除比较.csv"), row.names = FALSE)

message("== Table1 / S1 已输出至 ", normalizePath(out_dir), " ==")
message("主分析样本 n = ", nrow(master), "，卒中 ", sum(master$stroke == 1))
