# ============================================================
# 12_hei.R — HEI-2020 计算（手写 NCI 标准算法, Krebs-Smith 2018）+
# C1: 死亡分析附加调整 HEI-2020 敏感性
# 周期: D-J（2005-2018, FPED 可得）；2003-04 为 MPED 不适用
# ============================================================

suppressPackageStartupMessages({
  library(survey); library(survival); library(dplyr); library(haven)
})
options(survey.lonely.psu = "adjust")
set.seed(2026)

# ---------- HEI-2020 评分函数（密度法, 13 组分, 总分 100）----------
hei2020 <- function(fped, diet) {
  # fped: FPED 每人食物当量; diet: DR1TOT 营养素（能量/钠/脂肪）
  m <- fped %>% left_join(diet, by = "SEQN")
  kcal1000 <- m$DR1TKCAL / 1000
  dens <- function(x) x / kcal1000
  pct <- function(x) 100 * x / m$DR1TKCAL
  score <- function(x, max_pts, min_d, max_d) {
    # 线性分段：0 分 at min_d → max_pts at max_d（x>=max_d 满分）
    pmin(max_pts, pmax(0, max_pts * (x - min_d) / (max_d - min_d)))
  }
  score_rev <- function(x, max_pts, max_d) {
    # 反向：0 分 at max_d → max_pts at 0
    pmax(0, max_pts * (max_d - x) / max_d)
  }
  total_fruit  <- m$DR1T_F_TOTAL
  whole_fruit  <- m$DR1T_F_TOTAL - m$DR1T_F_JUICE
  total_veg    <- m$DR1T_V_TOTAL + m$DR1T_V_LEGUMES
  greens_beans <- m$DR1T_V_DRKGR + m$DR1T_V_LEGUMES
  total_prot   <- m$DR1T_PF_TOTAL + m$DR1T_V_LEGUMES
  sea_plant    <- m$DR1T_PF_SEAFD_HI + m$DR1T_PF_SEAFD_LOW + m$DR1T_PF_SOY +
    m$DR1T_PF_NUTSDS + m$DR1T_PF_LEGUMES + m$DR1T_V_LEGUMES
  fa_ratio     <- (m$DR1TPFAT + m$DR1TMFAT) / m$DR1TSFAT
  sodium_g     <- m$DR1TSODI / 1000
  added_sugar  <- pct(m$DR1T_ADD_SUGARS * 4)   # g 糖 *4 kcal/g → %能量
  sat_fat      <- pct(m$DR1TSFAT * 9)          # g 脂肪 *9 kcal/g → %能量

  hei <- score(dens(total_fruit), 5, 0, 0.8) +
    score(dens(whole_fruit), 5, 0, 0.4) +
    score(dens(total_veg), 5, 0, 1.1) +
    score(dens(greens_beans), 5, 0, 0.2) +
    score(dens(m$DR1T_G_WHOLE), 10, 0, 1.5) +
    score(dens(m$DR1T_D_TOTAL), 10, 0, 1.3) +
    score(dens(total_prot), 5, 0, 2.5) +
    score(dens(sea_plant), 5, 0, 0.8) +
    score(fa_ratio, 10, 1.2, 2.5) +
    score_rev(dens(m$DR1T_G_REFINED), 10, 1.8) +
    score_rev(sodium_g, 10, 2.0) +
    score_rev(added_sugar, 10, 26) +
    score_rev(sat_fat, 10, 16)
  data.frame(SEQN = m$SEQN, HEI2020 = round(hei, 1))
}

# ---------- 逐周期计算 ----------
cycles <- c("D","E","F","G","H","I","J")
hei_list <- list()
for (sfx in cycles) {
  fp <- file.path("data/fped", paste0("FPED_DR1TOT_",
    c(D="0506",E="0708",F="0910",G="1112",H="1314",I="1516",J="1718")[[sfx]], "_sas"),
    paste0("fped_dr1tot_", c(D="0506",E="0708",F="0910",G="1112",H="1314",I="1516",J="1718")[[sfx]], ".sas7bdat"))
  fped <- read_sas(fp)
  diet <- read_xpt(file.path("data/raw", paste0("DR1TOT_", sfx, ".XPT"))) %>%
    select(SEQN, DR1TKCAL, DR1TSODI, DR1TSFAT, DR1TPFAT, DR1TMFAT)
  hei_list[[sfx]] <- hei2020(fped, diet)
  cat(sfx, "HEI 完成 n =", nrow(hei_list[[sfx]]), "均值 =",
      round(mean(hei_list[[sfx]]$HEI2020), 1), "\n")
}
hei_all <- bind_rows(hei_list)
saveRDS(hei_all, "data/analysis/hei2020.RDS")
cat("HEI 全部完成, 总 n =", nrow(hei_all), "\n")

# ---------- C1: 死亡分析附加调整 HEI ----------
mc <- readRDS("data/analysis/master_cc.RDS")
d <- mc %>% filter(cycle_sfx %in% cycles) %>% mutate(wt8 = wt_2yr / 8)
mort <- readRDS("data/mortality/LMF2019_public_CJ.rds")
mort$seqn <- as.numeric(mort$seqn)
d <- d %>% left_join(mort, by = c("SEQN" = "seqn")) %>%
  filter(eligstat == 1 & !is.na(permth_int) & permth_int > 0) %>%
  left_join(hei_all, by = "SEQN") %>%
  filter(!is.na(HEI2020))
d$vitk_sd_diet <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)
mk <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt8, data = dat, nest = TRUE)
covars_m3 <- c("age","sex","race","education","PIR","marital","BMI","smoking",
               "hypertension","diabetes","CHD","TC","HDL","kcal","satfat","chol_int")
m3 <- paste(covars_m3, collapse = " + ")

fit0 <- svycoxph(as.formula(paste0("Surv(permth_int, mortstat) ~ vitk_sd_diet + ", m3)),
                 design = mk(d))
fit_hei <- svycoxph(as.formula(paste0("Surv(permth_int, mortstat) ~ vitk_sd_diet + HEI2020 + ", m3)),
                    design = mk(d))
get_row <- function(fit) {
  ct <- summary(fit)$coefficients; ci <- exp(confint(fit)["vitk_sd_diet", ])
  data.frame(HR = round(exp(coef(fit)["vitk_sd_diet"]), 3), LCL = round(ci[1], 3),
             UCL = round(ci[2], 3), p = signif(ct["vitk_sd_diet", ncol(ct)], 3))
}
res <- bind_rows(
  data.frame(get_row(fit0), sensitivity = "基线(D-J, 2005-2018)"),
  data.frame(get_row(fit_hei), sensitivity = "附加调整HEI-2020")
)
write.csv(res, "results/T19_HEI敏感性.csv", row.names = FALSE)
cat("基线(D-J):", res$HR[1], "p =", res$p[1], "\n")
cat("HEI调整:", res$HR[2], "p =", res$p[2], "\n")
message("== HEI 敏感性完成 ==")
