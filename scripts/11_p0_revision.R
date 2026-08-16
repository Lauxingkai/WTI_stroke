# ============================================================
# 11_p0_revision.R — P0 修订：敏感性补强 + 生存描述 + KM 曲线
# C2: 排除 2015-2018 周期敏感性；分周期随访/人年/事件率表
# C3: 白蛋白（虚弱代理）调整敏感性
# C4: nutrient density 与 1-99 百分位极值剔除敏感性
# 输出: results/T17_C2排除后期周期.csv / T18_随访描述.csv / T19_白蛋白密度极值.csv
#       output/figures/Figure4_KM.png
# ============================================================

suppressPackageStartupMessages({
  library(survey); library(survival); library(dplyr); library(haven); library(ggplot2)
})
options(survey.lonely.psu = "adjust")
set.seed(2026)
out_dir <- "results"; fig_dir <- "output/figures"

# ---------- 死亡队列 ----------
mc <- readRDS("data/analysis/master_cc.RDS")
d <- mc %>% filter(cycle_sfx %in% c("C","D","E","F","G","H","I","J")) %>% mutate(wt8 = wt_2yr / 8)
mort <- readRDS("data/mortality/LMF2019_public_CJ.rds")
mort$seqn <- as.numeric(mort$seqn)
d <- d %>% left_join(mort, by = c("SEQN" = "seqn")) %>%
  filter(eligstat == 1 & !is.na(permth_int) & permth_int > 0)
d$vitk_sd_diet <- d$vitk_diet / sd(d$vitk_diet, na.rm = TRUE)

# ---------- 白蛋白（LBXSAL, g/dL）合并 ----------
alb_list <- list()
alb_list[["C"]] <- nhanesA::nhanes("L40_C") %>% select(SEQN, ALB = LBXSAL)
for (sfx in c("D","E","F","G","H","I","J")) {
  alb_list[[sfx]] <- nhanesA::nhanes(paste0("BIOPRO_", sfx)) %>% select(SEQN, ALB = LBXSAL)
}
alb <- bind_rows(alb_list)
d <- d %>% left_join(alb, by = "SEQN")
cat("白蛋白覆盖率:", round(100 * mean(!is.na(d$ALB)), 1), "%\n")

covars_m2 <- c("age","sex","race","education","PIR","marital")
covars_m3 <- c(covars_m2, "BMI","smoking","hypertension","diabetes","CHD",
               "TC","HDL","kcal","satfat","chol_int")
m3_form <- paste(covars_m3, collapse = " + ")
mk <- function(dat) svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt8, data = dat, nest = TRUE)
cox_m3 <- function(dat, exp_var = "vitk_sd_diet", extra = "") {
  f <- as.formula(paste0("Surv(permth_int, mortstat) ~ ", exp_var, " + ",
                         if (nzchar(extra)) paste(c(covars_m3, extra), collapse = " + ") else m3_form))
  fit <- tryCatch(svycoxph(f, design = mk(dat)), error = function(e) NULL)
  if (is.null(fit)) return(data.frame(HR = NA, LCL = NA, UCL = NA, p = NA))
  ct <- summary(fit)$coefficients; ci <- exp(confint(fit)[exp_var, ])
  data.frame(HR = round(exp(coef(fit)[exp_var]), 3), LCL = round(ci[1], 3),
             UCL = round(ci[2], 3), p = signif(ct[exp_var, ncol(ct)], 3))
}

# ---------- C2: 排除 2015-2018 周期 ----------
d_early <- d %>% filter(cycle_sfx %in% c("C","D","E","F","G","H"))
res_c2 <- cox_m3(d_early)
res_c2$sensitivity <- "排除2015-2018(仅2003-2014)"
cat("C2 排除后期周期:", res_c2$HR, "p =", res_c2$p, "\n")

# ---------- C3: 白蛋白调整 ----------
res_alb <- cox_m3(d %>% filter(!is.na(ALB)), extra = "ALB")
res_alb$sensitivity <- "附加调整白蛋白(虚弱代理)"
cat("C3 白蛋白调整:", res_alb$HR, "p =", res_alb$p, "\n")

# ---------- C4: density 与极值剔除 ----------
d$vitk_density_sd <- (d$vitk_diet / (d$kcal / 1000)) / sd(d$vitk_diet / (d$kcal / 1000), na.rm = TRUE)
d$vitk_sd_density <- d$vitk_density_sd
res_dens <- cox_m3(d, exp_var = "vitk_sd_density")
res_dens$sensitivity <- "nutrient density(µg/1000kcal,每SD)"
p01 <- quantile(d$vitk_diet, 0.01, na.rm = TRUE); p99 <- quantile(d$vitk_diet, 0.99, na.rm = TRUE)
res_trim <- cox_m3(d %>% filter(vitk_diet >= p01 & vitk_diet <= p99))
res_trim$sensitivity <- "剔除1-99%极值"
cat("C4 density:", res_dens$HR, "p =", res_dens$p, "; 极值剔除:", res_trim$HR, "p =", res_trim$p, "\n")

t17 <- bind_rows(res_c2, res_alb, res_dens, res_trim)
write.csv(t17, file.path(out_dir, "T17_P0敏感性.csv"), row.names = FALSE)

# ---------- 随访描述：人年/事件率/分周期 ----------
d$py <- d$permth_int / 12
cat("总人年:", round(sum(d$py * d$wt8 / sum(d$wt8) * nrow(d))), "\n")
cat("事件率:", round(1000 * sum(d$mortstat == 1) / sum(d$py)), "例/1000人年\n")
per_cycle <- d %>% group_by(cycle_sfx) %>%
  summarise(n = n(), deaths = sum(mortstat == 1),
            median_fu_mo = median(permth_int),
            rate_1000py = round(1000 * sum(mortstat) / sum(py), 1)) %>%
  mutate(rate_1000py = ifelse(is.na(rate_1000py), 0, rate_1000py))
write.csv(per_cycle, file.path(out_dir, "T18_分周期随访.csv"), row.names = FALSE)
print(per_cycle)

# ---------- KM 曲线（加权, svykm）----------
d$vitk_q <- cut(d$vitk_diet, breaks = c(-Inf, quantile(d$vitk_diet, c(0.25, 0.5, 0.75), na.rm = TRUE), Inf),
                labels = c("Q1","Q2","Q3","Q4"))
dsn <- mk(d)
km <- svykm(Surv(permth_int / 12, mortstat) ~ vitk_q, design = dsn)
km_df <- bind_rows(lapply(names(km), function(g) {
  data.frame(time = km[[g]][[1]], surv = km[[g]][[2]], grp = g)
}))
p_km <- ggplot(km_df, aes(time, surv, color = grp)) +
  geom_step(linewidth = 0.9) +
  scale_color_brewer(palette = "Set1", name = "维生素K四分位") +
  labs(x = "随访年数", y = "生存概率", title = "全因死亡 Kaplan-Meier 曲线（加权）") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top")
ggsave(file.path(fig_dir, "Figure4_KM.png"), p_km, width = 8, height = 6, dpi = 300)
ggsave(file.path(fig_dir, "Figure4_KM.tiff"), p_km, width = 8, height = 6, dpi = 300, compression = "lzw")
message("== P0 修订完成 ==")
