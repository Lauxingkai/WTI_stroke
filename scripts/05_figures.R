# ============================================================
# 05_figures.R — 图件输出 v2（适配死亡结局主结果）
# 图1 STROBE 流程图 | 图2 死亡结局 RCS 剂量反应曲线（HR）
# 图3 死亡结局亚组森林图（全因死亡）
# 输入: exclusion_log.csv、mortality_rcs.RDS、master_cc.RDS + 死亡数据
# ============================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(rms)
  library(survey); library(survival); library(patchwork)
})
options(survey.lonely.psu = "adjust")
set.seed(2026)
fig_dir <- "output/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

save_fig <- function(p, name) {
  ggsave(file.path(fig_dir, paste0(name, ".tiff")), p, width = 8, height = 6,
         dpi = 300, compression = "lzw")
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = 8, height = 6, dpi = 300)
  message("已输出 ", name)
}

# ============================================================
# 图1 STROBE 流程图
# ============================================================
lg <- read.csv("data/analysis/exclusion_log.csv")
fbox <- function(x, y, label, w = 0.62, h = 0.09) {
  list(
    annotate("rect", xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2,
             fill = "white", color = "black"),
    annotate("text", x, y, label = label, size = 3.4)
  )
}
p_flow <- ggplot(data.frame(x = 0, y = 0)) + xlim(0, 1) + ylim(0, 1) + theme_void() +
  fbox(0.5, 0.95, sprintf("NHANES 2003-2018 合并访谈样本 n = %s",
                         format(lg$n[1], big.mark = ",")))
for (i in seq_len(nrow(lg))[-1]) {
  ytop <- 0.95 - (i - 1) * 0.13
  p_flow <- p_flow +
    annotate("segment", x = 0.5, xend = 0.5, y = ytop + 0.065, yend = ytop + 0.05,
             arrow = arrow(length = unit(0.015, "npc"))) +
    fbox(0.5, ytop, sprintf("%s\nn = %s", lg$reason[i], format(lg$n[i], big.mark = ",")))
}
p_flow <- p_flow +
  fbox(0.5, 0.95 - (nrow(lg)) * 0.13, "与 NDI 死亡数据(1999-2018)链接\n死亡分析队列 n = 29,702（全因死亡 3,814）")
save_fig(p_flow, "Figure1_flowchart")

# ============================================================
# 图2 死亡结局 RCS 剂量反应曲线（HR, 参考点 = P25）
# ============================================================
rcs_obj <- readRDS("results/mortality_rcs.RDS")
fit <- rcs_obj$fit; knots <- rcs_obj$knots; dat <- rcs_obj$data
b <- coef(fit); v <- vcov(fit)
rcs_names <- paste0("rcs", 1:3)
beta <- b[rcs_names]; vbeta <- v[rcs_names, rcs_names]

xgrid <- seq(min(dat$vitk_diet), max(dat$vitk_diet), length.out = 200)
xref <- as.numeric(quantile(dat$vitk_diet, 0.25))
Bg <- as.data.frame(unclass(as.matrix(rms::rcs(xgrid, knots))))
names(Bg) <- rcs_names
Bref <- as.numeric(unclass(as.matrix(rms::rcs(xref, knots)))[1, ])

lp <- as.matrix(Bg) %*% beta
lp_ref <- sum(Bref * beta)
hr <- exp(lp - lp_ref)
se_lp <- sqrt(apply(as.matrix(Bg), 1, function(r) t(r) %*% vbeta %*% r))
curve_df <- data.frame(x = xgrid,
                       HR = hr,
                       lo = exp(lp - lp_ref - 1.96 * se_lp),
                       hi = exp(lp - lp_ref + 1.96 * se_lp))

p_rcs <- ggplot(curve_df, aes(x)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.25) +
  geom_line(aes(y = HR), color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = xref, linetype = "dotted", color = "grey50") +
  annotate("text", x = xref, y = max(curve_df$hi) * 0.98,
           label = sprintf("参考点 %.0f µg/天", xref), size = 3, hjust = 1.1) +
  labs(x = "膳食维生素K摄入 (µg/天)", y = "HR (95% CI)",
       title = "维生素K摄入与全因死亡关联的剂量-反应曲线（4节点RCS, 模型3）") +
  theme_bw(base_size = 11)
save_fig(p_rcs, "Figure2_mortality_RCS")

# ============================================================
# 图3 死亡结局亚组森林图（全因死亡 HR, 模型3）
# ============================================================
mort_dir <- "D:/OneDrive - Business/小黑屋/level 1专题课/第19期NHANES/NHANES数据库挖掘专题课课前资料/3.死亡数据"
mc <- readRDS("data/analysis/master_cc.RDS")
d <- mc %>% filter(cycle_sfx %in% c("C","D","E","F","G","H","I","J")) %>% mutate(wt8 = wt_2yr / 8)
mort1 <- read.csv(file.path(mort_dir, "NHANES_MORT_1999_2018.csv"))
mort2 <- read.csv(file.path(mort_dir, "NHANES_specificmortality_1999_2018.csv"))
d <- d %>% left_join(mort1, by = "SEQN") %>% left_join(mort2, by = "SEQN") %>%
  filter(eligstat == 1 & !is.na(time_int) & time_int > 0)
d$vitk_sd_diet <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)
mk <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt8, data = dat, nest = TRUE)
covars_m3 <- c("age","sex","race","education","PIR","marital","BMI","smoking",
               "hypertension","diabetes","CHD","TC","HDL","kcal","satfat","chol_int")
m3 <- paste(covars_m3, collapse = " + ")

subgroups <- list(sex = "sex", age = "age_group", BMI = "BMI_group", smoking = "smoking",
                  hypertension = "hypertension", diabetes = "diabetes",
                  education = "education", income = "PIR")
rows <- list()
for (nm in names(subgroups)) {
  var <- subgroups[[nm]]
  m3_i <- paste(setdiff(covars_m3, var), collapse = " + ")
  for (lv in sort(unique(d[[var]]))) {
    d_i <- d %>% filter(.data[[var]] == lv) %>% droplevels()
    if (nrow(d_i) < 100) next
    fit_i <- tryCatch(svycoxph(as.formula(paste0("Surv(time_int, all_cause_mort) ~ vitk_sd_diet + ", m3_i)),
                               design = mk(d_i)), error = function(e) NULL)
    if (is.null(fit_i)) next
    ct <- summary(fit_i)$coefficients
    hr <- exp(coef(fit_i)["vitk_sd_diet"])
    ci <- exp(confint(fit_i)["vitk_sd_diet", ])
    rows[[paste(nm, lv)]] <- data.frame(subgroup = nm, level = as.character(lv),
                                        HR = hr, LCL = ci[1], UCL = ci[2],
                                        p = ct["vitk_sd_diet", ncol(ct)])
  }
  # 交互
  fit_int <- tryCatch(svycoxph(as.formula(paste0("Surv(time_int, all_cause_mort) ~ vitk_sd_diet * ", var, " + ",
                                                 paste(setdiff(covars_m3, var), collapse = " + "))),
                               design = mk(d)), error = function(e) NULL)
  ip <- if (is.null(fit_int)) NA_real_ else {
    ct <- summary(fit_int)$coefficients
    term_int <- grep("vitk_sd_diet.*_", rownames(ct), value = TRUE)
    if (!length(term_int)) term_int <- grep("vitk_sd_diet", rownames(ct), value = TRUE)
    term_int <- term_int[grep(":", term_int)][1]
    if (is.na(term_int)) NA_real_ else ct[term_int, ncol(ct)]
  }
  rows[[paste(nm, "int")]] <- data.frame(subgroup = nm, level = "P_interaction",
                                         HR = NA, LCL = NA, UCL = NA, p = ip)
}
sub_mort <- bind_rows(rows)
write.csv(sub_mort, file.path("results", "T16_死亡亚组.csv"), row.names = FALSE)

sub_main <- sub_mort %>% filter(!is.na(HR)) %>%
  mutate(lab = paste0(subgroup, " · ", level))
sub_int  <- sub_mort %>% filter(level == "P_interaction") %>% select(subgroup, p_int = p)
sub_main <- sub_main %>% left_join(sub_int, by = "subgroup") %>%
  mutate(interact = sprintf("交互P=%.3f", p_int))

p_forest <- ggplot(sub_main, aes(x = HR, y = reorder(lab, HR))) +
  geom_point(size = 2.4, color = "darkred") +
  geom_errorbarh(aes(xmin = LCL, xmax = UCL), height = 0.25, color = "darkred") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  geom_text(aes(label = interact, x = max(UCL, na.rm = TRUE) * 1.15), hjust = 0, size = 3) +
  scale_x_log10() +
  labs(x = "HR (95% CI), 每SD膳食维生素K（全因死亡, 模型3）", y = NULL,
       title = "亚组分析森林图（全因死亡）") +
  theme_bw(base_size = 10) +
  theme(axis.text.y = element_text(size = 8.5))
save_fig(p_forest, "Figure3_mortality_forest")

message("== 图件输出完成: ", fig_dir, " ==")
