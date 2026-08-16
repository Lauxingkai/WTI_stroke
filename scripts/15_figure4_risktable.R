# 15_figure4_risktable.R — T5b: Figure4 重做（加权 KM 曲线 + 底部 at-risk 表）
# 依据: 审稿任务 T5（KM 曲线补充风险表）
suppressPackageStartupMessages({
  library(survey); library(survival); library(dplyr); library(ggplot2)
  library(patchwork)
})
options(survey.lonely.psu = "adjust")
fig_dir <- "output/figures"

# ---------- 死亡队列（同 14_revision_analysis.R：C-J + LMF） ----------
mc <- readRDS("data/analysis/master_cc.RDS")
mort_dir <- "D:/OneDrive - Business/小黑屋/level 1专题课/第19期NHANES/NHANES数据库挖掘专题课课前资料/3.死亡数据"
d <- mc %>% filter(cycle_sfx %in% c("C","D","E","F","G","H","I","J")) %>% mutate(wt8 = wt_2yr / 8)
mort1 <- read.csv(file.path(mort_dir, "NHANES_MORT_1999_2018.csv"))
mort2 <- read.csv(file.path(mort_dir, "NHANES_specificmortality_1999_2018.csv"))
d <- d %>% left_join(mort1, by = "SEQN") %>% left_join(mort2, by = "SEQN")
d <- d %>% filter(eligstat == 1 & !is.na(time_int) & time_int > 0)
d$vitk_q <- cut(d$vitk_diet, breaks = c(-Inf, quantile(d$vitk_diet, c(0.25, 0.5, 0.75), na.rm = TRUE), Inf),
                labels = c("Q1","Q2","Q3","Q4"))
d$fup_y <- d$time_int / 12

# ---------- 加权 KM 曲线 ----------
dsn <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt8, data = d, nest = TRUE)
km <- svykm(Surv(fup_y, all_cause_mort) ~ vitk_q, design = dsn)
km_df <- bind_rows(lapply(names(km), function(g) {
  data.frame(time = km[[g]][[1]], surv = km[[g]][[2]], grp = g)
}))
p_km <- ggplot(km_df, aes(time, surv, color = grp)) +
  geom_step(linewidth = 0.9) +
  scale_color_brewer(palette = "Set1", name = "维生素K四分位") +
  labs(x = "随访年数", y = "生存概率", title = "全因死亡 Kaplan-Meier 曲线（加权）") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top",
        plot.margin = margin(5, 5, 0, 5))

# ---------- at-risk 表（未加权计数，同队列） ----------
tt <- c(0, 2, 4, 6, 8, 10)
km_uw <- survfit(Surv(fup_y, all_cause_mort) ~ vitk_q, data = d)
blocks <- cumsum(km_uw$strata)
starts <- c(1, head(blocks, -1) + 1)
risk <- sapply(levels(d$vitk_q), function(lv) {
  pos <- which(names(km_uw$strata) == paste0("vitk_q=", lv))
  idx <- starts[pos]:blocks[pos]
  t <- km_uw$time[idx]; nr <- km_uw$n.risk[idx]
  sapply(tt, function(x) {
    j <- max(which(t <= x))
    if (length(j) == 0 || is.infinite(j)) nr[1] else nr[j]
  })
})
risk_df <- as.data.frame(risk)
risk_df$year <- tt
risk_long <- tidyr::pivot_longer(risk_df, cols = starts_with("Q"), names_to = "grp", values_to = "n")
risk_long$grp <- factor(risk_long$grp, levels = levels(d$vitk_q))
risk_long$label <- sprintf("%d", risk_long$n)

p_risk <- ggplot(risk_long, aes(year, grp, label = label)) +
  geom_text(size = 3.2, family = "mono") +
  scale_y_discrete(limits = rev(levels(d$vitk_q))) +
  labs(x = "随访年数", y = NULL, title = "Number at risk") +
  theme_void(base_size = 11) +
  theme(axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 10, margin = margin(2, 0, 0, 0)),
        plot.title = element_text(size = 10, face = "bold", hjust = 0, margin = margin(0, 0, 4, 0)),
        plot.margin = margin(0, 5, 5, 5))
# 风险表 x 轴对齐主图（0-10 年）
p_risk <- p_risk + scale_x_continuous(breaks = tt, limits = range(tt), expand = c(0.01, 0.01))

p_all <- p_km / p_risk + plot_layout(heights = c(4, 1))
ggsave(file.path(fig_dir, "Figure4_KM.png"), p_all, width = 8, height = 7.5, dpi = 300)
ggsave(file.path(fig_dir, "Figure4_KM.tiff"), p_all, width = 8, height = 7.5, dpi = 300, compression = "lzw")
cat("Figure4_KM.png/tiff 已更新（含 at-risk 表）\n")
print(risk)
