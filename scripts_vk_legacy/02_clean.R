# ============================================================
# 02_clean.R — NHANES 维生素K-卒中项目：数据清洗与队列构建（v2 适配版）
# 输入: data/raw/*.rds（nhanesA 下载 C-J/L + 用户 P_ 转换 K，共 10 周期 2003-2022）
# 输出: data/analysis/master.RDS（全周期合并）、master_cc.RDS（主分析完整案例）、
#       exclusion_log.csv、covariate_version_report.txt
# 适配说明（对照实测数据）:
#   - C/D (2003-06): 无补充剂营养素表 → vitk_supp=NA（总摄入主分析限 E-L）
#   - E-I/K/L: 补充剂表 DSQTOT 的 DSQTVK; J: DS1TOT/DS2TOT 的 DS1TVK/DS2TVK
#   - K (2019-20): 权重 WTMECPRP、婚姻 DMDMARTZ、HDL 用 LBDHDD、无 ALQ(饮酒=NA)
#   - 主分析协变量不含饮酒/补充剂使用（与手稿对齐+数据可得；二者用于敏感性）
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# ---------- 配置 ----------
raw_dir <- "data/raw"
out_dir <- "data/analysis"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 10 周期（主分析 2003-2022）
cycles <- data.frame(
  year   = c("2003-2004","2005-2006","2007-2008","2009-2010","2011-2012",
             "2013-2014","2015-2016","2017-2018","2019-2020","2021-2022"),
  sfx    = c("C","D","E","F","G","H","I","J","K","L"),
  stringsAsFactors = FALSE
)

# 补充剂表分派: "none"=无营养素表; "DSQTOT"=DSQTVK; "DS1DS2"=DS1TVK+DS2TVK
supp_map <- c(C="none", D="none", E="DSQTOT", F="DSQTOT", G="DSQTOT",
              H="DSQTOT", I="DSQTOT", J="DS1DS2", K="DSQTOT", L="DSQTOT")

# ---------- 工具函数 ----------
read_xpt <- function(table, sfx) {
  f <- file.path(raw_dir, paste0(table, "_", sfx, ".XPT"))
  if (!file.exists(f)) stop("缺失文件: ", f)
  haven::read_xpt(f)
}
first_of <- function(d, cand) {
  hit <- intersect(cand, names(d))
  if (length(hit)) hit[1] else NA_character_
}
day2mean <- function(v1, v2) rowMeans(cbind(v1, v2), na.rm = TRUE)

# ---------- 主流程 ----------
master_list <- list()
version_report <- character()

for (i in seq_len(nrow(cycles))) {
  sfx <- cycles$sfx[i]
  yr  <- cycles$year[i]
  message("== 周期 ", yr, " (", sfx, ") ==")

  demo <- read_xpt("DEMO", sfx)
  race_var  <- if (sfx %in% c("C","D","E","F")) "RIDRETH1" else "RIDRETH3"
  marital_v <- first_of(demo, c("DMDMARTL","DMDMARTZ"))
  wt_v      <- first_of(demo, c("WTMEC2YR","WTMECPRP"))
  version_report <- c(version_report, sprintf("%s: 婚姻=%s 权重=%s 种族=%s",
                                              yr, marital_v, wt_v, race_var))

  d <- demo %>%
    select(SEQN, RIAGENDR, RIDAGEYR, DMDEDUC2, RIDEXPRG, INDFMPIR,
           SDMVPSU, SDMVSTRA, !!sym(wt_v), !!sym(marital_v), !!sym(race_var)) %>%
    rename(RIDRETH = !!sym(race_var), DMDMARTL = !!sym(marital_v),
           WTMEC2YR = !!sym(wt_v))

  # 膳食回顾（第1天；两日回顾仅 C-J 有 DR2TOT，K/L 无→单日）
  dr1 <- read_xpt("DR1TOT", sfx)
  dr2 <- tryCatch(read_xpt("DR2TOT", sfx), error = function(e) NULL)
  satf_v <- first_of(dr1, c("DR1TSFAT","DR1TSATF"))
  version_report <- c(version_report, sprintf("%s: 饱和脂肪=%s", yr, satf_v))
  diet <- dr1 %>% select(SEQN, DR1TVK, DR1TKCAL,
                         !!sym(satf_v), DR1TCHOL) %>% rename(DR1TSFAT = !!sym(satf_v))
  if (!is.null(dr2)) {
    satf2 <- first_of(dr2, c("DR2TSFAT","DR2TSATF"))
    diet <- diet %>% left_join(
      dr2 %>% select(SEQN, DR2TVK, DR2TKCAL,
                     !!sym(satf2), DR2TCHOL) %>% rename(DR2TSFAT = !!sym(satf2)),
      by = "SEQN")
  } else {
    diet <- diet %>% mutate(DR2TVK = NA_real_, DR2TKCAL = NA_real_,
                            DR2TSFAT = NA_real_, DR2TCHOL = NA_real_)
    version_report <- c(version_report, sprintf("%s: 无 DR2TOT（单日回顾）", yr))
  }

  # 补充剂（按周期分派）
  smode <- supp_map[[sfx]]
  ds <- data.frame(SEQN = demo$SEQN)
  if (smode == "DSQTOT") {
    dsq <- read_xpt("DSQTOT", sfx)
    vk <- first_of(dsq, c("DSQTVK","DSQTK1"))
    ds <- dsq %>% select(SEQN, vitk_supp = !!sym(vk))
    version_report <- c(version_report, sprintf("%s: 补充剂 DSQTOT[%s]", yr, vk))
  } else if (smode == "DS1DS2") {
    ds1 <- read_xpt("DS1TOT", sfx); ds2 <- read_xpt("DS2TOT", sfx)
    v1 <- first_of(ds1, c("DS1TVK","DS1K")); v2 <- first_of(ds2, c("DS2TVK","DS2K"))
    ds <- ds1 %>% select(SEQN, v1 = !!sym(v1)) %>%
      left_join(ds2 %>% select(SEQN, v2 = !!sym(v2)), by = "SEQN") %>%
      mutate(vitk_supp = day2mean(v1, v2)) %>% select(SEQN, vitk_supp)
    version_report <- c(version_report, sprintf("%s: 补充剂 DS1TOT[%s]+DS2TOT[%s]", yr, v1, v2))
  } else {
    ds$vitk_supp <- NA_real_
    version_report <- c(version_report, sprintf("%s: 无补充剂营养素表（vitk_supp=NA）", yr))
  }

  # 问卷与体检
  mcq <- read_xpt("MCQ", sfx) %>% select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E)
  bmx <- read_xpt("BMX", sfx) %>% select(SEQN, BMXBMI)
  bpq <- read_xpt("BPQ", sfx) %>% select(SEQN, BPQ020)
  diq <- read_xpt("DIQ", sfx) %>% select(SEQN, DIQ010)
  smq <- read_xpt("SMQ", sfx) %>% select(SEQN, SMQ020, SMQ040)
  alq_raw <- tryCatch(read_xpt("ALQ", sfx), error = function(e) NULL)
  if (is.null(alq_raw)) {
    alq <- data.frame(SEQN = demo$SEQN, ALQ101 = NA_real_, ALQ120 = NA_real_)
    version_report <- c(version_report, sprintf("%s: 无 ALQ 表", yr))
  } else {
    a1 <- first_of(alq_raw, c("ALQ101","ALQ111"))
    a2 <- first_of(alq_raw, c("ALQ120","ALQ121"))
    alq <- alq_raw %>% select(SEQN, ALQ101 = !!sym(a1))
    alq$ALQ120 <- if (is.na(a2)) NA_real_ else alq_raw[[a2]]
    version_report <- c(version_report, sprintf("%s: 饮酒变量 %s/%s", yr, a1, a2))
  }

  # 血脂：C 在 L13；D 起 TCHOL + HDL
  if (sfx == "C") {
    lip <- read_xpt("L13", sfx) %>% select(SEQN, LBXTC, LBXHDD) %>% rename(LBDHDD = LBXHDD)
  } else {
    tc  <- read_xpt("TCHOL", sfx) %>% select(SEQN, LBXTC)
    hd  <- read_xpt("HDL", sfx)
    hd_v <- first_of(hd, c("LBXHDD","LBDHDD"))
    version_report <- c(version_report, sprintf("%s: HDL变量=%s", yr, hd_v))
    hdl <- hd %>% select(SEQN, LBXHDD = !!sym(hd_v))
    lip <- full_join(tc, hdl, by = "SEQN")
  }

  # ---------- 合并 ----------
  dat <- d %>%
    left_join(diet, by = "SEQN") %>%
    left_join(ds,   by = "SEQN") %>%
    left_join(mcq,  by = "SEQN") %>%
    left_join(bmx,  by = "SEQN") %>%
    left_join(bpq,  by = "SEQN") %>%
    left_join(diq,  by = "SEQN") %>%
    left_join(smq,  by = "SEQN") %>%
    left_join(alq,  by = "SEQN") %>%
    left_join(lip,  by = "SEQN") %>%
    mutate(cycle_year = yr, cycle_sfx = sfx)

  master_list[[i]] <- dat
}

# 合并前统一列类型（nhanesA 与 P_ 文件 labelled/factor 差异；已 factor 化的协变量除外）
fact_cols <- c("sex","age_group","race","education","marital","BMI_group",
               "smoking","alcohol","hypertension","diabetes","CHD","supp_user")
master_list <- lapply(master_list, function(dd) {
  for (v in setdiff(names(dd), fact_cols)) {
    x <- dd[[v]]
    if (is.factor(x)) dd[[v]] <- as.numeric(as.character(x))
    else if (inherits(x, "haven_labelled")) dd[[v]] <- as.numeric(x)
  }
  dd
})
master <- bind_rows(master_list)

# ---------- 变量派生 ----------
master <- master %>%
  mutate(
    # --- 暴露 ---
    vitk_diet = day2mean(DR1TVK, DR2TVK),                  # 膳食维生素K µg/天（单日或两日均值）
    vitk_total = vitk_diet + ifelse(is.na(vitk_supp), 0, vitk_supp), # 总摄入（C/D 实际=膳食，主分析限 E-L 时需过滤）
    vitk_total_ok = !is.na(vitk_diet) & !(cycle_sfx %in% c("C","D") & is.na(vitk_supp)),
    kcal      = day2mean(DR1TKCAL, DR2TKCAL),
    satfat    = day2mean(DR1TSFAT, DR2TSFAT),
    chol_int  = day2mean(DR1TCHOL, DR2TCHOL),
    vitk_density = vitk_total / (kcal / 1000),             # nutrient density（敏感性）

    # --- 结局 ---
    stroke = case_when(MCQ160E == 1 ~ 1L, MCQ160E == 2 ~ 0L, TRUE ~ NA_integer_),

    # --- 协变量 ---
    sex      = factor(RIAGENDR, levels = 1:2, labels = c("男性","女性")),
    age      = RIDAGEYR,
    age_group = factor(ifelse(age <= 60, 1, 2), levels = 1:2, labels = c("20-60",">60")),
    race     = factor(RIDRETH, levels = 1:5,
                      labels = c("墨西哥裔美籍","其他西班牙裔","非西班牙裔白人",
                                 "非西班牙裔黑人","其他/多种族")),
    education = factor(DMDEDUC2, levels = 1:5,
                       labels = c("<9年级","9-11年级","高中/GED","大学或AA学位","大学及以上"),
                       exclude = c(7, 9)),
    marital   = factor(DMDMARTL, levels = 1:6,
                       labels = c("已婚","丧偶","离异","分居","从未结婚","同居"),
                       exclude = c(77, 99)),
    PIR       = INDFMPIR,
    BMI       = BMXBMI,
    BMI_group = factor(ifelse(BMXBMI >= 25, 2, 1), levels = 1:2, labels = c("<25","≥25")),
    smoking   = factor(case_when(SMQ020 == 1 & SMQ040 %in% c(1, 2) ~ 3L,
                                 SMQ020 == 1 & SMQ040 == 3    ~ 2L,
                                 SMQ020 == 2                  ~ 1L,
                                 TRUE                         ~ NA_integer_),
                       levels = 1:3, labels = c("从不","既往","当前")),
    alcohol   = factor(case_when(ALQ101 == 2 ~ 1L,
                                 ALQ101 == 1 ~ 2L,
                                 TRUE        ~ NA_integer_),
                       levels = 1:2, labels = c("从不","当前")),
    hypertension = factor(case_when(BPQ020 == 1 ~ 1L, BPQ020 == 2 ~ 0L, TRUE ~ NA_integer_),
                          levels = 0:1, labels = c("否","是")),
    diabetes  = factor(case_when(DIQ010 == 1 ~ 1L, DIQ010 == 2 ~ 0L, TRUE ~ NA_integer_),
                       levels = 0:1, labels = c("否","是")),
    CHD       = factor(case_when(pmax(MCQ160B, MCQ160C, MCQ160D, na.rm = TRUE) == 1 ~ 1L,
                                 pmax(MCQ160B, MCQ160C, MCQ160D, na.rm = TRUE) == 2 ~ 0L,
                                 TRUE ~ NA_integer_),
                       levels = 0:1, labels = c("否","是")),
    TC        = LBXTC,
    HDL       = ifelse(is.na(LBDHDD), LBXHDD, LBDHDD),   # 跨周期统一（C:L13 用 LBXHDD→LBDHDD；D 起 HDL 表 LBXHDD）
    supp_user = factor(case_when(!is.na(vitk_supp) ~ 1L, TRUE ~ 0L),
                       levels = 0:1, labels = c("否","是")),
    wt_2yr    = WTMEC2YR
  )

# ---------- 排除（顺序与方案一致）----------
n0 <- nrow(master)
log <- data.frame(step = 0, reason = "初始合并", n = n0, stringsAsFactors = FALSE)
track <- function(dat, reason) {
  nn <- nrow(dat)
  log <<- rbind(log, data.frame(step = nrow(log), reason = reason, n = nn))
  nn
}

master <- master %>% filter(!is.na(age) & age >= 20);                 track(master, "排除 <20 岁")
master <- master %>% filter(is.na(RIDEXPRG) | RIDEXPRG != 1);               track(master, "排除孕妇")
master <- master %>% filter(is.na(kcal) | (kcal >= 500 & kcal <= 5000)); track(master, "排除能量异常(<500/>5000)")
master <- master %>% filter(!is.na(stroke));                          track(master, "排除结局缺失")
master <- master %>% filter(!is.na(vitk_diet));                       track(master, "排除膳食暴露缺失")

# 主分析完整案例（不含 alcohol/supp_user——与手稿对齐+数据可得性）
master_pre_cc <- master
cc_vars <- c("sex","age","race","education","marital","PIR","BMI","smoking",
             "hypertension","diabetes","CHD","TC","HDL","kcal","satfat","chol_int")
master_cc <- master %>% filter(if_all(all_of(cc_vars), ~ !is.na(.)))
track(master_cc, "完整案例(主分析协变量)")

# ---------- 输出 ----------
saveRDS(master,      file.path(out_dir, "master.RDS"))
saveRDS(master_cc,   file.path(out_dir, "master_cc.RDS"))
saveRDS(master_pre_cc, file.path(out_dir, "master_pre_cc.RDS"))
write.csv(log, file.path(out_dir, "exclusion_log.csv"), row.names = FALSE)
writeLines(version_report, file.path(out_dir, "covariate_version_report.txt"))

message("== 清洗完成 ==")
message("全队列 n = ", nrow(master), "；主分析完整案例 n = ", nrow(master_cc))
message("卒中例数: 全队列 ", sum(master$stroke == 1, na.rm = TRUE),
        "；完整案例 ", sum(master_cc$stroke == 1, na.rm = TRUE))
message("总摄入可用(非C/D且非缺失): ", sum(master_cc$vitk_total_ok, na.rm = TRUE))
message("== 请核对 exclusion_log.csv（STROBE 流程图）==")
