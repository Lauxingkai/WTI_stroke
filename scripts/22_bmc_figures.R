# ============================================================
# 22_bmc_figures.R — BMC Public Health 版图件（出版级规格）
# 规格: Arial 字体, 图宽 170 mm (双栏), 300 dpi TIFF (LZW) + PNG
# 图1 RCS 曲线 | 图2 亚组森林图 | 图3 排除链流程图
# 数据/计算逻辑与 20/21 脚本一致（结果数值不变，仅图形规格调整）
# ============================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(rms); library(survey); library(haven)
})
options(survey.lonely.psu = "adjust")
set.seed(2026)
# Windows 字体注册（Arial，供 tiff/png 设备使用）
suppressWarnings(windowsFonts(Arial = windowsFont("Arial")))
fig_dir <- "output/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

save_bmc <- function(p, name, h_mm) {
  ggsave(file.path(fig_dir, paste0(name, "_bmc.tiff")), p, width = 170, height = h_mm,
         units = "mm", dpi = 300, compression = "lzw")
  ggsave(file.path(fig_dir, paste0(name, "_bmc.png")), p, width = 170, height = h_mm,
         units = "mm", dpi = 300)
  cat("已输出", name, "_bmc\n")
}
bmc_theme <- theme_bw(base_size = 10) +
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.text = element_text(size = 8), legend.title = element_text(size = 8))

# ============================================================
# 图1 RCS（OR, 参考 P25, 4 节点）
# ============================================================
rcs_obj <- readRDS("results/rcs4_model.RDS")
fit <- rcs_obj$fit; knots <- rcs_obj$knots; dat <- rcs_obj$data; nl_p <- rcs_obj$nl_p
b <- coef(fit); v <- vcov(fit)
rcs_names <- paste0("rcs", 1:3)
beta <- b[rcs_names]; vbeta <- v[rcs_names, rcs_names]
xgrid <- seq(min(dat$vitk_total), max(dat$vitk_total), length.out = 200)
xref <- as.numeric(quantile(dat$vitk_total, 0.25))
Bg <- as.data.frame(unclass(as.matrix(rms::rcs(xgrid, knots)))); names(Bg) <- rcs_names
Bref <- as.numeric(unclass(as.matrix(rms::rcs(xref, knots)))[1, ])
lp <- as.matrix(Bg) %*% beta
lp_ref <- sum(Bref * beta)
se_lp <- sqrt(apply(as.matrix(Bg), 1, function(r) t(r) %*% vbeta %*% r))
curve_df <- data.frame(x = xgrid, OR = exp(lp - lp_ref),
                       lo = exp(lp - lp_ref - 1.96 * se_lp),
                       hi = exp(lp - lp_ref + 1.96 * se_lp))
bp <- 189.1
p1 <- ggplot(curve_df, aes(x)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.25) +
  geom_line(aes(y = OR), color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = xref, linetype = "dotted", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = bp, linetype = "longdash", color = "darkred", alpha = 0.55, linewidth = 0.4) +
  annotate("text", x = xref, y = max(curve_df$hi) * 0.99,
           label = sprintf("Reference: P25 = %.0f µg/day", xref), size = 2.6, hjust = 1.1) +
  annotate("text", x = bp, y = min(curve_df$lo) * 1.03,
           label = "Inflection point 189.1 µg/day", size = 2.6, hjust = 0) +
  labs(x = "Total vitamin K intake (µg/day)", y = "Odds ratio (95% CI)",
       title = sprintf(paste0("Dose-response association between total vitamin K intake ",
                              "and prevalent stroke (4-knot RCS, Model 3; ",
                              "P nonlinear = %.3f)"), nl_p)) +
  bmc_theme
save_bmc(p1, "Figure1_stroke_RCS", 120)

# ============================================================
# 图2 亚组森林图（数据: T5_亚组_完整.csv, 20 脚本产出）
# ============================================================
res_sub <- read.csv("results/T5_亚组_完整.csv", stringsAsFactors = FALSE)
en_group <- c(sex = "Sex", age = "Age", BMI = "BMI", smoking = "Smoking",
              hypertension = "Hypertension", diabetes = "Diabetes",
              alcohol = "Alcohol", education = "Education",
              income = "Income (PIR tertiles)")
en_level <- c("男性" = "Male", "女性" = "Female", "从不" = "Never",
              "既往" = "Former", "当前" = "Current", "否" = "No", "是" = "Yes",
              "<9年级" = "<9th grade", "9-11年级" = "9-11th grade",
              "高中/GED" = "High school/GED", "大学或AA学位" = "Some college/AA",
              "大学及以上" = "College+", "T1" = "Tertile 1", "T2" = "Tertile 2",
              "T3" = "Tertile 3")
strata <- res_sub %>% filter(level != "P_interaction") %>%
  mutate(group_lab = en_group[subgroup],
         level_lab = ifelse(level %in% names(en_level), en_level[level], level),
         y_lab = paste0(group_lab, " \u00b7 ", level_lab))
ints <- res_sub %>% filter(level == "P_interaction") %>%
  mutate(p_int = ifelse(is.na(p), NA, sprintf("%.3f", p))) %>% select(subgroup, p_int)
ord <- c("income", "education", "alcohol", "diabetes", "hypertension",
         "smoking", "BMI", "age", "sex")
strata$subgroup <- factor(strata$subgroup, levels = ord)
strata <- strata %>% arrange(subgroup) %>%
  mutate(y_lab = factor(y_lab, levels = rev(unique(y_lab)))) %>%
  left_join(ints, by = "subgroup")
xmax <- max(strata$UCL, na.rm = TRUE)
p2 <- ggplot(strata, aes(x = OR, y = y_lab)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = LCL, xmax = UCL), height = 0.18, linewidth = 0.4) +
  geom_point(shape = 21, size = 2.2, fill = "steelblue", color = "black") +
  geom_text(aes(x = xmax * 1.35, label = sprintf("%.2f (%.2f, %.2f)", OR, LCL, UCL)),
            size = 2.3, hjust = 0) +
  geom_text(aes(x = xmax * 2.05, label = p_int), size = 2.3, hjust = 0) +
  scale_x_log10() +
  labs(x = "Odds ratio (95% CI) per 1-SD increase in total vitamin K intake",
       y = NULL,
       title = "Subgroup analyses of total vitamin K intake and prevalent stroke (Model 3)") +
  bmc_theme + theme(panel.grid.minor = element_blank())
save_bmc(p2, "Figure2_stroke_forest", 160)

# ============================================================
# 图3 排除链流程图（E-K，数据重建逻辑同 21 脚本）
# ============================================================
raw_dir <- "data/raw"
ek_cycles <- c("E", "F", "G", "H", "I", "J", "K")
has_dr2 <- c("E", "F", "G", "H", "I", "J")
day2mean <- function(v1, v2) rowMeans(cbind(v1, v2), na.rm = TRUE)
frames <- list()
for (sfx in ek_cycles) {
  demo <- read_xpt(file.path(raw_dir, paste0("DEMO_", sfx, ".XPT")))
  dr1 <- read_xpt(file.path(raw_dir, paste0("DR1TOT_", sfx, ".XPT")))
  mcq <- read_xpt(file.path(raw_dir, paste0("MCQ_", sfx, ".XPT")))
  d <- demo %>% select(SEQN, RIDAGEYR, RIDEXPRG) %>%
    left_join(dr1 %>% select(SEQN, DR1TKCAL, DR1TVK), by = "SEQN") %>%
    left_join(mcq %>% select(SEQN, MCQ160E), by = "SEQN")
  if (sfx %in% has_dr2) {
    dr2 <- read_xpt(file.path(raw_dir, paste0("DR2TOT_", sfx, ".XPT")))
    d <- d %>% left_join(dr2 %>% select(SEQN, DR2TVK, DR2TKCAL), by = "SEQN")
  } else { d$DR2TVK <- NA_real_; d$DR2TKCAL <- NA_real_ }
  d$cycle_sfx <- sfx
  d$kcal <- day2mean(d$DR1TKCAL, d$DR2TKCAL)
  d$vitk_diet <- day2mean(d$DR1TVK, d$DR2TVK)
  d$stroke <- case_when(d$MCQ160E == 1 ~ 1L, d$MCQ160E == 2 ~ 0L, TRUE ~ NA_integer_)
  frames[[sfx]] <- d
}
all_ek <- bind_rows(frames)
s1 <- all_ek
s2 <- s1 %>% filter(!is.na(RIDAGEYR) & RIDAGEYR >= 20)
s3 <- s2 %>% filter(is.na(RIDEXPRG) | RIDEXPRG != 1)
s4 <- s3 %>% filter(is.na(kcal) | (kcal >= 500 & kcal <= 5000))
s5 <- s4 %>% filter(!is.na(stroke))
s6 <- s5 %>% filter(!is.na(vitk_diet))
cc_vars <- c("sex","age","race","education","marital","PIR","BMI","smoking",
             "hypertension","diabetes","CHD","TC","HDL","kcal","satfat","chol_int")
pre <- readRDS("data/analysis/master_pre_cc.RDS")
s8 <- pre %>% filter(cycle_sfx %in% ek_cycles) %>% filter(if_all(all_of(cc_vars), ~ !is.na(.)))
log_df <- data.frame(reason = c("NHANES 2007\u20132020 (cycles E\u2013K) interviewed sample",
                                "Excluded: age < 20 years",
                                "Excluded: pregnant",
                                "Excluded: implausible energy intake (<500/>5,000 kcal/day)",
                                "Excluded: missing stroke status",
                                "Excluded: missing dietary vitamin K",
                                "Total intake available (diet + supplements)",
                                "Complete case (analytic cohort)"),
                     n = c(nrow(s1), nrow(s2), nrow(s3), nrow(s4), nrow(s5), nrow(s6),
                           nrow(s6), nrow(s8)), stringsAsFactors = FALSE)
n_stroke <- sum(s8$stroke == 1, na.rm = TRUE)
fbox <- function(x, y, label, w = 0.74, h = 0.068) {
  list(
    annotate("rect", xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2,
             fill = "white", color = "black", linewidth = 0.4),
    annotate("text", x, y, label = label, size = 2.6, family = "Arial")
  )
}
p3 <- ggplot(data.frame(x = 0, y = 0)) + xlim(0, 1) + ylim(0, 1) + theme_void()
gap <- 0.105
for (i in seq_len(nrow(log_df))) {
  ytop <- 0.97 - (i - 1) * gap
  if (i > 1) {
    p3 <- p3 + annotate("segment", x = 0.5, xend = 0.5, y = ytop + gap/2 - 0.005,
                        yend = ytop + 0.045, arrow = arrow(length = unit(0.012, "npc")),
                        linewidth = 0.4)
  }
  p3 <- p3 + fbox(0.5, ytop,
                  sprintf("%s\nn = %s", log_df$reason[i], format(log_df$n[i], big.mark = ",")))
}
p3 <- p3 + fbox(0.5, 0.97 - nrow(log_df) * gap - 0.03,
                sprintf("Analytic cohort: %s adults, %s prevalent strokes (weighted prevalence 3.2%%)",
                        format(log_df$n[nrow(log_df)], big.mark = ","),
                        format(n_stroke, big.mark = ",")), h = 0.05)
save_bmc(p3, "Figure3_stroke_flowchart", 150)

cat("BMC 版图件完成。\n")
