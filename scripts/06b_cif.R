# ============================================================================
# 06b_cif.R
# Figure 5: Fine-Gray cumulative incidence curves of incident stroke by WTI
# tertile (CHARLS 2011-2018 prospective), death as competing event.
# Output: output/figures/Figure5_CIF.pdf + .png (300 dpi)
# Date: 2026-08-15 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({library(survival); library(cmprsk); library(ggplot2)
  library(dplyr); library(readr)})
set.seed(42)
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed"); FIG <- file.path(RAW, "output/figures")

pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)
d <- pr %>% left_join(ev, by = "ID_12") %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         age = as.numeric(age),
         WTI_t = cut(WTI, quantile(WTI, c(0,1/3,2/3,1), na.rm = TRUE),
                     include.lowest = TRUE, labels = c("T1","T2","T3")),
         fstatus = ifelse(stroke, 1, ifelse(death, 2, 0)),
         ftime = pmin(time, 7.0)) %>%
  filter(!is.na(WTI_t) & !is.na(ftime) & !is.na(fstatus) &
           !is.na(WTI_sd) & !is.na(age) & !is.na(bmi) & !is.na(stroke))
logline <- function(...) cat(..., "\n")
logline(sprintf("n=%d stroke=%d death=%d", nrow(d), sum(d$stroke), sum(d$death)))

fit <- cuminc(d$ftime, d$fstatus, d$WTI_t, cencode = 0)

# tertile sHR from Fine-Gray (T2/T3 vs T1) for the caption
d <- d %>% mutate(T2 = as.integer(WTI_t == "T2"), T3 = as.integer(WTI_t == "T3"))
fg_t <- crr(d$ftime, d$fstatus, cov1 = as.matrix(d[, c("T2", "T3")]),
            failcode = 1, cencode = 0)
ft <- summary(fg_t)$conf.int
for (i in 1:2) {
  hr <- ft[i, 1]; lo <- ft[i, 3]; hi <- ft[i, 4]
  se <- (log(hi) - log(lo)) / (2 * 1.96)
  pp <- 2 * pnorm(-abs(log(hr) / se))
  logline(sprintf("FG tertile %s: sHR=%.3f (%.3f-%.3f) p=%.4f",
                  c("T2", "T3")[i], hr, lo, hi, pp))
}

extr <- function(f, group) {
  t <- f[[group]]$time; ci <- f[[group]]$est
  data.frame(time = t, cif = ci, group = group)
}
c1 <- extr(fit, "T1 1"); c2 <- extr(fit, "T2 1"); c3 <- extr(fit, "T3 1")
cd <- bind_rows(c1, c2, c3) %>%
  mutate(group = factor(sub(" 1$", "", group), levels = c("T1","T2","T3")))

WONG <- c("T1" = "#0072B2", "T2" = "#E69F00", "T3" = "#D55E00")
p <- ggplot(cd, aes(time, cif, color = group, linetype = group)) +
  geom_step(linewidth = 0.9) +
  scale_color_manual(values = WONG) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = "Follow-up (years)", y = "Cumulative incidence of stroke") +
  theme_classic(base_size = 9) +
  theme(legend.position = c(0.15, 0.85), legend.title = element_blank(),
        legend.background = element_rect(fill = NA, colour = NA),
        axis.text = element_text(colour = "black")) +
  coord_cartesian(xlim = c(0, 7), ylim = c(0, 0.10))

ggsave(file.path(FIG, "Figure5_CIF.pdf"), p, width = 3.5, height = 3.2, device = "pdf")
ggsave(file.path(FIG, "Figure5_CIF.png"), p, width = 3.5, height = 3.2, dpi = 300)
write_csv(cd, file.path(RAW, "results/06b_cif_curves.csv"))
logline("7-year CIF by tertile:")
for (g in c("T1","T2","T3")) {
  v <- cd %>% filter(group == g)
  logline(sprintf("  %s: 7y CIF = %.4f", g, tail(v$cif, 1)))
}
logline("=== 06b COMPLETE ===")
