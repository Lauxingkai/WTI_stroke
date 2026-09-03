# ============================================================================
# 13j_figure1_v3.R  v5 (2026-08-22) — BMC 170-mm layout, compact labels
# Figure 1: participant flow diagram (STROBE-style), three-branch layout.
# v5 vs v4: box labels redesigned for 170 mm width (<=18 chars/line, 8 pt);
#   long exclusion notes removed (kept in Results text), three short headers.
# Counts: NHANES from results/06a_nhanes_flow_counts.csv; CHARLS asserted in
#   01_id_linkage.R / 13d build checks; NDI from results/13g_ndi_checks.txt.
# Output: output/figures/Figure1_flow.pdf + .png (300 dpi); TIFF via _png2tiff.py
# ============================================================================
suppressPackageStartupMessages({ library(ggplot2); library(dplyr); library(readr) })
RAW <- "D:/NHANES"; RES <- file.path(RAW, "results"); FIG <- file.path(RAW, "output/figures")

fc <- read_csv(file.path(RES, "06a_nhanes_flow_counts.csv"), show_col_types = FALSE)
n_total <- fc$n[fc$step == "7 cycles (D-J) participants with DEMO record"]
n_age   <- fc$n[fc$step == "Age >= 40"]
n_fast  <- fc$n[fc$step == "+ fasting subsample (WTSAF2YR > 0)"]
n_wc    <- fc$n[fc$step == "+ measured waist circumference"]

ch_blood <- 11847; ch_comp <- 9870; ch_stroke0 <- 220; ch_prosp <- 9650; ch_inc <- 569
ch_m1 <- 9856; ch_adj <- 9206
ch_pm1 <- 9636; ch_padj <- 9028
c15_blood <- 13420; c15_comp <- 12899; c15_design <- 12501
c15_m1 <- 12213; c15_m23 <- 11203; c15_ev <- 252
ndi_elig <- 10289; ndi_death <- 1428

fbox <- function(x, y, label, w = 0.72, h = 0.30, highlight = FALSE, tsize = 2.6) {
  col <- if (highlight) "#0072B2" else "black"
  lwd <- if (highlight) 1.4 else 0.8
  list(
    annotate("rect", xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2,
             fill = "white", color = col, linewidth = lwd),
    annotate("text", x = x, y = y, label = label, size = tsize,
             hjust = 0.5, lineheight = 0.9)
  )
}
farr <- function(x1, y1, x2, y2) {
  annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
           arrow = arrow(length = unit(0.12, "cm")), linewidth = 0.5)
}
fexc <- function(x, y, label) {
  annotate("text", x = x, y = y, label = label, size = 2.4,
           hjust = 0, color = "grey30")
}

XL <- 0.40; XM <- 1.55; XR <- 2.70
ysN <- seq(0.95, 0.05, length.out = 6)
ysC <- seq(0.95, 0.05, length.out = 6)
ys15 <- c(0.86, 0.58, 0.30, 0.02)

gg <- ggplot() + theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA)) +
  xlim(0, 3.9) + ylim(-0.15, 1.22)

# ---------------- NHANES branch (5 cross + 1 NDI) ----------------
gg <- gg +
  fbox(XL, ysN[1], sprintf("NHANES 2005-18\ncycles D-J, exam\n(n = %s)", format(n_total, big.mark = ","))) +
  fbox(XL, ysN[2], sprintf("Aged \u2265 40 y\n(n = %s)", format(n_age, big.mark = ","))) +
  fbox(XL, ysN[3], sprintf("Fasting subsample\n(WTSAF2YR > 0)\n(n = %s)", format(n_fast, big.mark = ","))) +
  fbox(XL, ysN[4], sprintf("Measured WC\n(n = %s)", format(n_wc, big.mark = ","))) +
  fbox(XL, ysN[5], "Analytic cohort\n(n = 10,302)\nstroke 531", highlight = TRUE) +
  fbox(XL, ysN[6], sprintf("NDI linkage to\nDec 31, 2019\n(n = %s)\n%s deaths; 83\nstroke (I60-I69)",
                           format(ndi_elig, big.mark = ","),
                           format(ndi_death, big.mark = ",")), highlight = TRUE, tsize = 2.3) +
  farr(XL, ysN[1] - 0.15, XL, ysN[2] + 0.15) +
  farr(XL, ysN[2] - 0.15, XL, ysN[3] + 0.15) +
  farr(XL, ysN[3] - 0.15, XL, ysN[4] + 0.15) +
  farr(XL, ysN[4] - 0.15, XL, ysN[5] + 0.15) +
  farr(XL, ysN[5] - 0.15, XL, ysN[6] + 0.15) +
  fexc(0.90, ysN[2] - 0.10, "nonfasting") +
  fexc(0.88, ysN[5] - 0.10, sprintf("linkage\ninelig. %s", 10302 - ndi_elig)) +
  annotate("text", x = XL, y = 1.16, label = "NHANES\ncross + NDI",
           size = 3.0, fontface = "bold", lineheight = 0.85)

# ---------------- CHARLS 2011 branch (6 boxes) ----------------
gg <- gg +
  fbox(XM, ysC[1], sprintf("CHARLS 2011 blood\n(n = %s)", format(ch_blood, big.mark = ","))) +
  fbox(XM, ysC[2], sprintf("Complete TG + WC\n(n = %s)\nstroke %s",
                           format(ch_comp, big.mark = ","), ch_stroke0)) +
  fbox(XM, ysC[3], sprintf("Cross-sectional\ncohort (n = %s)\nM1 %s (220)\nadj. %s (178)",
                           format(ch_comp, big.mark = ","),
                           format(ch_m1, big.mark = ","),
                           format(ch_adj, big.mark = ",")), highlight = TRUE) +
  fbox(XM, ysC[4], sprintf("Baseline stroke-free\n(n = %s)", format(ch_prosp, big.mark = ","))) +
  fbox(XM, ysC[5], sprintf("Incident stroke\nby 2018 (n = %s)", ch_inc)) +
  fbox(XM, ysC[6], sprintf("Prospective cohort\nM1 %s (569)\nadj. %s (515)",
                           format(ch_pm1, big.mark = ","),
                           format(ch_padj, big.mark = ",")), highlight = TRUE) +
  farr(XM, ysC[1] - 0.15, XM, ysC[2] + 0.15) +
  farr(XM, ysC[2] - 0.15, XM, ysC[3] + 0.15) +
  farr(XM, ysC[2] - 0.15, XM, ysC[4] + 0.15) +
  farr(XM, ysC[4] - 0.15, XM, ysC[5] + 0.15) +
  farr(XM, ysC[5] - 0.15, XM, ysC[6] + 0.15) +
  annotate("text", x = XM, y = 1.16, label = "CHARLS 2011\ncross + prosp.",
           size = 3.0, fontface = "bold", lineheight = 0.85)

# ---------------- CHARLS 2015 replication branch (4 boxes) ----------------
gg <- gg +
  fbox(XR, ys15[1], sprintf("CHARLS 2015 blood\n(n = %s)", format(c15_blood, big.mark = ","))) +
  fbox(XR, ys15[2], sprintf("Complete TG + WC\n(n = %s)\nstroke %s",
                            format(c15_comp, big.mark = ","), 266), ) +
  fbox(XR, ys15[3], sprintf("Design-eligible\n(n = %s)", format(c15_design, big.mark = ","))) +
  fbox(XR, ys15[4], sprintf("Replication cohort\nM1 %s\nM2/M3 %s\nstroke %s",
                            format(c15_m1, big.mark = ","),
                            format(c15_m23, big.mark = ","), c15_ev), highlight = TRUE) +
  farr(XR, ys15[1] - 0.15, XR, ys15[2] + 0.15) +
  farr(XR, ys15[2] - 0.15, XR, ys15[3] + 0.15) +
  farr(XR, ys15[3] - 0.15, XR, ys15[4] + 0.15) +
  fexc(XR + 0.48, ys15[1] - 0.07, sprintf("missing\nTG/WC %s", c15_blood - c15_comp)) +
  annotate("text", x = XR, y = 1.16, label = "CHARLS 2015\nreplication",
           size = 3.0, fontface = "bold", lineheight = 0.85)

ggsave(file.path(FIG, "Figure1_flow.pdf"), gg, width = 6.7, height = 4.0, device = "pdf", bg = "white")
ggsave(file.path(FIG, "Figure1_flow.png"), gg, width = 6.7, height = 4.0, dpi = 300, bg = "white")
cat("=== 13j FIGURE1 v5 COMPLETE ===\n")