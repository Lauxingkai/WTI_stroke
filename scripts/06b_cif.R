# ============================================================================
# 06b_cif.R  v2 (2026-08-22) — Figure 5 with number-at-risk table
# Fine-Gray cumulative incidence curves of incident stroke by WTI tertile
# (CHARLS 2011-2018 prospective), death as competing event.
# Output: output/figures/Figure5_CIF.pdf + .png (300 dpi); TIFF via _png2tiff.py
# Date: 2026-08-15 | Seed: 42 | v2: + at-risk table (BMC style)
# ============================================================================
suppressPackageStartupMessages({library(survival); library(cmprsk); library(ggplot2)
  library(dplyr); library(readr); library(gridExtra); library(grid)})
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
           !is.na(WTI_sd) & !is.na(stroke) &
           !is.na(age) & !is.na(sex) & bloodweight > 0)   # M1 sample (n = 9,636)
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

# ---- bootstrap 95% CI bands (B = 500; stratified by tertile group) ----
B <- 500
grid <- seq(0, 7, by = 0.1)
mat <- list(T1 = matrix(NA_real_, B, length(grid)),
            T2 = matrix(NA_real_, B, length(grid)),
            T3 = matrix(NA_real_, B, length(grid)))
set.seed(42)
nb <- nrow(d)
for (b in 1:B) {
  ii <- sample.int(nb, nb, replace = TRUE)
  fbt <- cuminc(d$ftime[ii], d$fstatus[ii], d$WTI_t[ii], cencode = 0)
  for (g in c("T1", "T2", "T3")) {
    key <- paste0(g, " 1")
    if (is.null(fbt[[key]])) next
    tt <- fbt[[key]]$time; cc <- fbt[[key]]$est
    if (max(tt) < 7) next
    mat[[g]][b, ] <- approx(tt, cc, xout = grid, rule = 2)$y
  }
}
cd$lo <- NA_real_; cd$hi <- NA_real_
for (g in c("T1", "T2", "T3")) {
  ok <- rowSums(is.na(mat[[g]])) == 0
  if (sum(ok) < 50) next
  q <- apply(mat[[g]][ok, , drop = FALSE], 2,
             function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  sub <- cd[cd$group == g, ]
  cd$lo[cd$group == g] <- approx(grid, q[1, ], xout = sub$time, rule = 2)$y
  cd$hi[cd$group == g] <- approx(grid, q[2, ], xout = sub$time, rule = 2)$y
}
logline("bootstrap CI bands:"); print(summary(cd$lo)); print(summary(cd$hi))

WONG <- c("T1" = "#0072B2", "T2" = "#E69F00", "T3" = "#009E73")
p <- ggplot(cd, aes(time, cif, color = group, linetype = group)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = group), alpha = 0.12, colour = NA,
              show.legend = FALSE) +
  geom_step(linewidth = 0.9) +
  scale_color_manual(values = WONG, name = NULL) +
  scale_fill_manual(values = WONG, name = NULL, guide = "none") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 7, by = 1)) +
  labs(x = "Follow-up (years)", y = "Cumulative incidence of stroke") +
  theme_classic(base_size = 9) +
  theme(legend.position = c(0.16, 0.85), legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA, colour = NA),
        axis.text = element_text(colour = "black")) +
  coord_cartesian(xlim = c(0, 7), ylim = c(0, 0.09))

# ---- number-at-risk table (at t = 0, 2, 4, 7) ----
tp <- c(0, 2, 4, 7)
atrisk <- sapply(levels(d$WTI_t), function(g) {
  sapply(tp, function(ti) sum(d$ftime[d$WTI_t == g] >= ti))
})
colnames(atrisk) <- levels(d$WTI_t)
tab_df <- data.frame(
  `Follow-up (y)` = tp,
  T1 = atrisk[, "T1"], T2 = atrisk[, "T2"], T3 = atrisk[, "T3"],
  check.names = FALSE)
logline("Number at risk (0/2/4/7 y):")
print(tab_df)

tt <- tableGrob(tab_df, rows = NULL,
                theme = ttheme_minimal(base_size = 7,
                                       core = list(fg_params = list(col = "black")),
                                       colhead = list(fg_params = list(col = "black", fontface = "bold"))))

p <- p + theme(axis.title.x = element_blank())   # "Follow-up (y)" lives in the risk-table header
# ---- table height MUST be computed in a REAL device context ----
# bug: grobHeight(tt) returns a single 1grobheight (0.787in), not the table
# total; the true height is sum(tt$heights) (each row = max(grobheight)+4mm).
# With a device open this resolves to 1.154in for 5 rows (pit #33).
tmpf <- tempfile(fileext = ".png")
png(tmpf, width = 1005, height = 1200, units = "px", res = 300, type = "cairo")
grob_h <- as.numeric(convertHeight(sum(tt$heights), "in"))
dev.off()
unlink(tmpf)
cat(sprintf("table grob height = %.4f in\n", grob_h))
combo <- arrangeGrob(p, tt, nrow = 2, heights = unit(c(3.1, grob_h), "in"))
H <- 3.1 + grob_h + 0.03
ggsave(file.path(FIG, "Figure5_CIF.pdf"), combo, width = 3.35, height = H, device = "pdf", bg = "white")
ggsave(file.path(FIG, "Figure5_CIF.png"), combo, width = 3.35, height = H, dpi = 300, type = "cairo", bg = "white")
write_csv(cd, file.path(RAW, "results/06b_cif_curves.csv"))
logline("7-year CIF by tertile:")
for (g in c("T1","T2","T3")) {
  v <- cd %>% filter(group == g)
  logline(sprintf("  %s: 7y CIF = %.4f", g, tail(v$cif, 1)))
}
logline("=== 06b v2 COMPLETE ===")