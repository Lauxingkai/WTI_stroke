# T1_verify_stroke_exclusion.R — 审稿 T1 核实：卒中缺失排除对死亡分析的影响
# 目的：重建不排除卒中缺失的死亡队列，对比主分析（每SD三模型+四分位+trend）是否变化
# 依据：02_clean.R 逻辑（仅 C-J 周期，用于死亡队列）；10_final_tables.R 分析口径
# 输出：results/T25_T1核实_卒中排除影响.csv

suppressPackageStartupMessages({
  library(survey); library(survival); library(dplyr)
})
options(survey.lonely.psu = "adjust")

raw <- "D:/NHANES/data/raw"
mort_dir <- "D:/OneDrive - Business/小黑屋/level 1专题课/第19期NHANES/NHANES数据库挖掘专题课课前资料/3.死亡数据"
out <- "D:/NHANES/results"

sfx_list <- c("C","D","E","F","G","H","I","J")
yr_list  <- c("2003-2004","2005-2006","2007-2008","2009-2010","2011-2012",
              "2013-2014","2015-2016","2017-2018")

first_of <- function(d, cand) {
  hit <- intersect(cand, names(d))
  if (length(hit)) hit[1] else NA_character_
}
day2mean <- function(v1, v2) rowMeans(cbind(v1, v2), na.rm = TRUE)

build_cycle <- function(sfx, yr) {
  read_xpt <- function(table) haven::read_xpt(file.path(raw, paste0(table, "_", sfx, ".XPT")))
  demo <- read_xpt("DEMO")
  race_var  <- if (sfx %in% c("C","D","E","F")) "RIDRETH1" else "RIDRETH3"
  marital_v <- first_of(demo, c("DMDMARTL","DMDMARTZ"))
  d <- demo %>% select(SEQN, RIAGENDR, RIDAGEYR, DMDEDUC2, RIDEXPRG, INDFMPIR,
                       SDMVPSU, SDMVSTRA, WTMEC2YR, !!sym(marital_v), !!sym(race_var)) %>%
    rename(RIDRETH = !!sym(race_var), DMDMARTL = !!sym(marital_v))
  dr1 <- read_xpt("DR1TOT")
  satf_v <- first_of(dr1, c("DR1TSFAT","DR1TSATF"))
  diet <- dr1 %>% select(SEQN, DR1TVK, DR1TKCAL, !!sym(satf_v), DR1TCHOL) %>%
    rename(DR1TSFAT = !!sym(satf_v))
  dr2 <- tryCatch(read_xpt("DR2TOT"), error = function(e) NULL)
  if (!is.null(dr2)) {
    satf2 <- first_of(dr2, c("DR2TSFAT","DR2TSATF"))
    diet <- diet %>% left_join(
      dr2 %>% select(SEQN, DR2TVK, DR2TKCAL, !!sym(satf2), DR2TCHOL) %>%
        rename(DR2TSFAT = !!sym(satf2)), by = "SEQN")
  } else {
    diet <- diet %>% mutate(DR2TVK = NA_real_, DR2TKCAL = NA_real_,
                            DR2TSFAT = NA_real_, DR2TCHOL = NA_real_)
  }
  mcq <- read_xpt("MCQ") %>% select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E)
  bmx <- read_xpt("BMX") %>% select(SEQN, BMXBMI)
  bpq <- read_xpt("BPQ") %>% select(SEQN, BPQ020)
  diq <- read_xpt("DIQ") %>% select(SEQN, DIQ010)
  smq <- read_xpt("SMQ") %>% select(SEQN, SMQ020, SMQ040)
  if (sfx == "C") {
    lip <- read_xpt("L13") %>% select(SEQN, LBXTC, LBXHDD) %>% rename(LBDHDD = LBXHDD)
  } else {
    tc <- read_xpt("TCHOL") %>% select(SEQN, LBXTC)
    hd <- read_xpt("HDL")
    hd_v <- first_of(hd, c("LBXHDD","LBDHDD"))
    lip <- full_join(tc, hd %>% select(SEQN, LBXHDD = !!sym(hd_v)), by = "SEQN")
  }
  d %>%
    left_join(diet, by = "SEQN") %>%
    left_join(mcq, by = "SEQN") %>%
    left_join(bmx, by = "SEQN") %>%
    left_join(bpq, by = "SEQN") %>%
    left_join(diq, by = "SEQN") %>%
    left_join(smq, by = "SEQN") %>%
    left_join(lip, by = "SEQN") %>%
    mutate(cycle_sfx = sfx, cycle_year = yr)
}

# ---- 合并 C-J ----
cat("== 重建 C-J 合并（不排除卒中缺失） ==\n")
lst <- lapply(seq_along(sfx_list), function(i) build_cycle(sfx_list[i], yr_list[i]))
fact_cols <- c("sex","age_group","race","education","marital","BMI_group",
               "smoking","alcohol","hypertension","diabetes","CHD","supp_user")
lst <- lapply(lst, function(dd) {
  for (v in setdiff(names(dd), fact_cols)) {
    x <- dd[[v]]
    if (is.factor(x)) dd[[v]] <- as.numeric(as.character(x))
    else if (inherits(x, "haven_labelled")) dd[[v]] <- as.numeric(x)
  }
  dd
})
master <- bind_rows(lst)

master <- master %>%
  mutate(
    vitk_diet = day2mean(DR1TVK, DR2TVK),
    kcal      = day2mean(DR1TKCAL, DR2TKCAL),
    satfat    = day2mean(DR1TSFAT, DR2TSFAT),
    chol_int  = day2mean(DR1TCHOL, DR2TCHOL),
    sex      = factor(RIAGENDR, levels = 1:2, labels = c("男性","女性")),
    age      = RIDAGEYR,
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
    smoking   = factor(case_when(SMQ020 == 1 & SMQ040 %in% c(1, 2) ~ 3L,
                                 SMQ020 == 1 & SMQ040 == 3    ~ 2L,
                                 SMQ020 == 2                  ~ 1L,
                                 TRUE                         ~ NA_integer_),
                       levels = 1:3, labels = c("从不","既往","当前")),
    hypertension = factor(case_when(BPQ020 == 1 ~ 1L, BPQ020 == 2 ~ 0L, TRUE ~ NA_integer_),
                          levels = 0:1, labels = c("否","是")),
    diabetes  = factor(case_when(DIQ010 == 1 ~ 1L, DIQ010 == 2 ~ 0L, TRUE ~ NA_integer_),
                       levels = 0:1, labels = c("否","是")),
    CHD       = factor(case_when(pmax(MCQ160B, MCQ160C, MCQ160D, na.rm = TRUE) == 1 ~ 1L,
                                 pmax(MCQ160B, MCQ160C, MCQ160D, na.rm = TRUE) == 2 ~ 0L,
                                 TRUE ~ NA_integer_),
                       levels = 0:1, labels = c("否","是")),
    TC        = LBXTC,
    HDL       = ifelse(is.na(LBDHDD), LBXHDD, LBDHDD),
    wt_2yr    = WTMEC2YR,
    stroke    = case_when(MCQ160E == 1 ~ 1L, MCQ160E == 2 ~ 0L, TRUE ~ NA_integer_)
  )

# ---- 排除（不排除卒中缺失；与稿件 Methods 2.1 差异点）----
n0 <- nrow(master)
master <- master %>% filter(!is.na(age) & age >= 20)
master <- master %>% filter(is.na(RIDEXPRG) | RIDEXPRG != 1)
master <- master %>% filter(is.na(kcal) | (kcal >= 500 & kcal <= 5000))
master <- master %>% filter(!is.na(vitk_diet))
cc_vars <- c("sex","age","race","education","marital","PIR","BMI","smoking",
             "hypertension","diabetes","CHD","TC","HDL","kcal","satfat","chol_int")
master_cc <- master %>% filter(if_all(all_of(cc_vars), ~ !is.na(.)))
cat("不排除卒中时 master_cc n =", nrow(master_cc),
    "（原口径排除卒中缺失后 = 37908）; 卒中 NA 保留:", sum(is.na(master_cc$stroke)), "\n")

# ---- 死亡队列（同 10_final_tables.R）----
mort1 <- read.csv(file.path(mort_dir, "NHANES_MORT_1999_2018.csv"))
d <- master_cc %>% left_join(mort1, by = "SEQN")
d <- d %>% filter(eligstat == 1 & !is.na(permth_int) & permth_int > 0)
d$wt8 <- d$wt_2yr / 8
d$vitk_sd_diet <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)
cat("死亡队列（不排除卒中缺失）: n =", nrow(d), " 死亡 =", sum(d$mortstat == 1), "\n")

cm2 <- c("age","sex","race","education","PIR","marital")
cm3 <- c(cm2, "BMI","smoking","hypertension","diabetes","CHD","TC","HDL",
         "kcal","satfat","chol_int")
mk <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt8,
                              data = dat, nest = TRUE)
dsn <- mk(d)

# 每SD 三模型
fit_models <- function() {
  rows <- list()
  for (nm in c("M1","M2","M3")) {
    f <- if (nm == "M1") Surv(permth_int, mortstat) ~ vitk_sd_diet
         else as.formula(paste0("Surv(permth_int, mortstat) ~ vitk_sd_diet + ",
                                paste(get(paste0("cm", substring(nm, 2))), collapse = " + ")))
    fit <- svycoxph(f, design = dsn)
    ct <- summary(fit)$coefficients; ci <- exp(confint(fit)["vitk_sd_diet", ])
    rows[[nm]] <- data.frame(model = nm, HR = exp(coef(fit)["vitk_sd_diet"]),
                             LCL = ci[1], UCL = ci[2],
                             p = signif(ct["vitk_sd_diet", ncol(ct)], 4))
  }
  bind_rows(rows)
}
# 四分位 + trend
q4 <- quantile(d$vitk_diet, c(0.25, 0.5, 0.75), na.rm = TRUE)
d$q4d <- cut(d$vitk_diet, breaks = c(-Inf, q4, Inf), labels = FALSE)
d$q4med <- ave(d$vitk_diet, d$q4d, FUN = function(x) median(x, na.rm = TRUE))
dsn2 <- mk(d)
rowsq <- list()
for (nm in c("M1","M2","M3")) {
  f <- if (nm == "M1") Surv(permth_int, mortstat) ~ factor(q4d)
       else as.formula(paste0("Surv(permth_int, mortstat) ~ factor(q4d) + ",
                              paste(get(paste0("cm", substring(nm, 2))), collapse = " + ")))
  fit <- svycoxph(f, design = dsn2)
  ct <- summary(fit)$coefficients
  for (k in 2:4) {
    term <- paste0("factor(q4d)", k)
    ci <- exp(confint(fit)[term, ])
    rowsq[[paste(nm, k)]] <- data.frame(model = nm, quartile = paste0("Q", k),
      HR = exp(coef(fit)[term]), LCL = ci[1], UCL = ci[2],
      p = signif(ct[term, ncol(ct)], 4))
  }
}
fit_tr <- svycoxph(Surv(permth_int, mortstat) ~ q4med + age + sex + race + education +
                     PIR + marital + BMI + smoking + hypertension + diabetes + CHD +
                     TC + HDL + kcal + satfat + chol_int, design = dsn2)
ct <- summary(fit_tr)$coefficients
trend_p <- signif(ct["q4med", ncol(ct)], 4)

res <- bind_rows(
  mutate(fit_models(), 口径 = "不排除卒中缺失"),
  mutate(bind_rows(rowsq), 口径 = "不排除卒中缺失"),
  data.frame(model = "M3-trend", quartile = NA, HR = NA, LCL = NA, UCL = NA,
             p = trend_p, 口径 = "不排除卒中缺失")
)
write.csv(res, file.path(out, "T25_T1核实_卒中排除影响.csv"), row.names = FALSE)
cat("\n===== 结果（对照稿件: M3 HR 0.887, Q4 0.772, trend P=0.0008） =====\n")
print(res, digits = 4)
pred <- data.frame(
  指标 = c("死亡队列n","死亡数","M1 HR","M2 HR","M3 HR","Q4(M3) HR","trend P"),
  排除卒中缺失 = c("29702","3814","0.849","0.863","0.887","0.772","0.0008"),
  不排除卒中缺失 = c(as.character(nrow(d)), as.character(sum(d$mortstat == 1)),
    sprintf("%.3f", res$HR[res$model=="M1"][1]),
    sprintf("%.3f", res$HR[res$model=="M2"][1]),
    sprintf("%.3f", res$HR[res$model=="M3"][1]),
    sprintf("%.3f", res$HR[res$model=="M3" & res$quartile=="Q4" & !is.na(res$quartile)]),
    formatC(trend_p, format = "g", digits = 4)))
write.csv(pred, file.path(out, "T25_T1核实_对比汇总.csv"), row.names = FALSE)
cat("\n===== T1 核实完成 =====\n")