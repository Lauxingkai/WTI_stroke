# ============================================================
# 21_flowchart_stroke.R — 卒中主分析（E-K 总摄入）STROBE 流程图
# 重建排除链: master_pre_cc 已是 02_clean.R 前 5 步排除后的中间产物,
#   无法提供逐步骤人数 → 从 data/raw/*.XPT 重建 E-K 周期排除链。
# 步骤与 02_clean.R 一致: 初始 → <20岁 → 孕妇 → 能量异常 → 结局缺失
#   → 膳食暴露缺失 → 总摄入可用 → 完整案例(=27,455 验证点)
# 输出: results/exclusion_log_stroke_ek.csv
#       output/figures/Figure3_stroke_flowchart.{tiff,png}
# ============================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(haven)
})

raw_dir <- "data/raw"
fig_dir <- "output/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

ek_cycles <- c("E", "F", "G", "H", "I", "J", "K")
has_dr2  <- c("E", "F", "G", "H", "I", "J")   # K/L 无 DR2TOT（单日回顾）
day2mean <- function(v1, v2) rowMeans(cbind(v1, v2), na.rm = TRUE)

frames <- list()
for (sfx in ek_cycles) {
  demo <- read_xpt(file.path(raw_dir, paste0("DEMO_", sfx, ".XPT")))
  dr1  <- read_xpt(file.path(raw_dir, paste0("DR1TOT_", sfx, ".XPT")))
  mcq  <- read_xpt(file.path(raw_dir, paste0("MCQ_", sfx, ".XPT")))
  d <- demo %>% select(SEQN, RIDAGEYR, RIDEXPRG) %>%
    left_join(dr1 %>% select(SEQN, DR1TKCAL, DR1TVK), by = "SEQN") %>%
    left_join(mcq %>% select(SEQN, MCQ160E), by = "SEQN")
  if (sfx %in% has_dr2) {
    dr2 <- read_xpt(file.path(raw_dir, paste0("DR2TOT_", sfx, ".XPT")))
    d <- d %>% left_join(dr2 %>% select(SEQN, DR2TVK, DR2TKCAL), by = "SEQN")
  } else {
    d$DR2TVK <- NA_real_; d$DR2TKCAL <- NA_real_
  }
  d$cycle_sfx <- sfx
  d$kcal   <- day2mean(d$DR1TKCAL, d$DR2TKCAL)   # 与 02_clean.R:174 一致（两日均值；K 无 DR2 即单日）
  d$vitk_diet <- day2mean(d$DR1TVK, d$DR2TVK)
  d$stroke <- case_when(d$MCQ160E == 1 ~ 1L, d$MCQ160E == 2 ~ 0L, TRUE ~ NA_integer_)
  frames[[sfx]] <- d
}
all_ek <- bind_rows(frames)
stopifnot(nrow(all_ek) == nrow(all_ek %>% distinct(SEQN, cycle_sfx)))

track <- function(dat, reason) data.frame(reason = reason, n = nrow(dat), stringsAsFactors = FALSE)
s1 <- all_ek
s2 <- s1 %>% filter(!is.na(age <- RIDAGEYR) & RIDAGEYR >= 20)
s3 <- s2 %>% filter(is.na(RIDEXPRG) | RIDEXPRG != 1)
s4 <- s3 %>% filter(is.na(kcal) | (kcal >= 500 & kcal <= 5000))
s5 <- s4 %>% filter(!is.na(stroke))
s6 <- s5 %>% filter(!is.na(vitk_diet))
s7 <- s6  # 总摄入可用（E-K 下等价于膳食非缺失，vitk_total_ok 定义见 02_clean.R:173）
log_df <- bind_rows(
  track(s1, "NHANES 2007-2020 (E-K cycles) interviewed sample"),
  track(s2, "Excluded: age < 20 years"),
  track(s3, "Excluded: pregnant"),
  track(s4, "Excluded: implausible energy intake (<500/>5,000 kcal/day)"),
  track(s5, "Excluded: missing stroke status"),
  track(s6, "Excluded: missing dietary vitamin K"),
  track(s7, "Total intake available (diet + supplements)")
)
# 完整案例（主分析协变量）——验证点 27,455
cc_vars <- c("sex","age","race","education","marital","PIR","BMI","smoking",
             "hypertension","diabetes","CHD","TC","HDL","kcal","satfat","chol_int")
pre <- readRDS("data/analysis/master_pre_cc.RDS")
s8 <- pre %>% filter(cycle_sfx %in% ek_cycles) %>% filter(if_all(all_of(cc_vars), ~ !is.na(.)))
log_df <- bind_rows(log_df, track(s8, "Complete case (analytic cohort)"))
n_stroke <- sum(s8$stroke == 1, na.rm = TRUE)
write.csv(log_df, "results/exclusion_log_stroke_ek.csv", row.names = FALSE)
cat("链条验证: 膳食后 n =", s6 %>% nrow(), "（期望 37,472）; 最终 n =", nrow(s8),
    "（期望 27,455）; 卒中", n_stroke, "\n")
print(log_df)

# ---------- 流程图 ----------
fbox <- function(x, y, label, w = 0.72, h = 0.07) {
  list(
    annotate("rect", xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2,
             fill = "white", color = "black"),
    annotate("text", x, y, label = label, size = 3.0)
  )
}
p_flow <- ggplot(data.frame(x = 0, y = 0)) + xlim(0, 1) + ylim(0, 1) + theme_void()
gap <- 0.105
for (i in seq_len(nrow(log_df))) {
  ytop <- 0.97 - (i - 1) * gap
  if (i > 1) {
    p_flow <- p_flow +
      annotate("segment", x = 0.5, xend = 0.5, y = ytop + gap/2 - 0.005,
               yend = ytop + 0.045, arrow = arrow(length = unit(0.014, "npc")))
  }
  lbl <- sprintf("%s\nn = %s", log_df$reason[i], format(log_df$n[i], big.mark = ","))
  p_flow <- p_flow + fbox(0.5, ytop, lbl)
}
p_flow <- p_flow +
  fbox(0.5, 0.97 - nrow(log_df) * gap - 0.03,
       sprintf("Analytic cohort: %s adults, %s prevalent strokes (weighted prevalence 3.2%%)",
               format(log_df$n[nrow(log_df)], big.mark = ","),
               format(n_stroke, big.mark = ",")), h = 0.05)

ggsave(file.path(fig_dir, "Figure3_stroke_flowchart.tiff"), p_flow,
       width = 8, height = 7.5, dpi = 300, compression = "lzw")
ggsave(file.path(fig_dir, "Figure3_stroke_flowchart.png"), p_flow,
       width = 8, height = 7.5, dpi = 300)
cat("流程图已输出\n")
