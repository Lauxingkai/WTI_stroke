# ============================================================================
# 13j_figure1_v3.R
# Figure 1 v3: participant flow diagram (STROBE-style), three-branch layout:
#   NHANES cross-sectional + NDI prospective extension
#   CHARLS 2011 cross-sectional + 7-year prospective
#   CHARLS 2015 replication layer (new)
# Fixes vs v2: NHANES analytic cohort stroke 641 -> 531 (MCQ160F P0 fix);
#   adds NDI follow-up box and the 2015 replication branch.
# Counts: NHANES from results/06a_nhanes_flow_counts.csv; CHARLS asserted in
#   01_id_linkage.R / 13d build checks; NDI from results/13g_ndi_checks.txt.
# Output: output/figures/Figure1_flow.pdf + .png (300 dpi)
# Date: 2026-08-16
# ============================================================================
suppressPackageStartupMessages({ library(ggplot2); library(dplyr); library(readr) })
RAW <- "D:/NHANES"; RES <- file.path(RAW, "results"); FIG <- file.path(RAW, "output/figures")

fc <- read_csv(file.path(RES, "06a_nhanes_flow_counts.csv"), show_col_types = FALSE)
n_total <- fc$n[fc$step == "7 cycles (D-J) participants with DEMO record"]
n_age   <- fc$n[fc$step == "Age >= 40"]
n_fast  <- fc$n[fc$step == "+ fasting subsample (WTSAF2YR > 0)"]
n_wc    <- fc$n[fc$step == "+ measured waist circumference"]

# CHARLS 2011 (results/01_linkage_checks.txt) + 2015 (13d/13e) + NDI (13g)
ch_blood <- 11847; ch_comp <- 9870; ch_stroke0 <- 220; ch_prosp <- 9650; ch_inc <- 507
c15_blood <- 13420; c15_comp <- 12899; c15_stroke <- 289
c15_design <- 12501; c15_m1 <- 12213; c15_m23 <- 11203; c15_ev <- 252
ndi_elig <- 10289; ndi_death <- 1428; ndi_stroke_death <- 83

fbox <- function(x, y, label, w = 0.62, h = 0.30, highlight = FALSE) {
  col <- if (highlight) "#0072B2" else "black"
  lwd <- if (highlight) 1.4 else 0.8
  list(
    annotate("rect", xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2,
             fill = "white", color = col, linewidth = lwd),
    annotate("text", x = x, y = y, label = label, size = 3.0,
             hjust = 0.5, lineheight = 0.9)
  )
}
farr <- function(x1, y1, x2, y2) {
  annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
           arrow = arrow(length = unit(0.12, "cm")), linewidth = 0.5)
}
fexc <- function(x, y, label) {
  annotate("text", x = x, y = y, label = label, size = 2.6,
           hjust = 0, color = "grey30")
}

XL <- 0.32; XM <- 1.25; XR <- 2.18
ysN <- seq(0.95, 0.05, length.out = 6)
ysC <- seq(0.95, 0.05, length.out = 6)
ys15 <- c(0.86, 0.58, 0.30, 0.02)

gg <- ggplot() + theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA)) +
  xlim(0, 3.35) + ylim(-0.02, 1)

# ---------------- NHANES branch (5 cross + 1 NDI) ----------------
gg <- gg +
  fbox(XL, ysN[1], sprintf("NHANES 2005-2018, cycles D-J\nparticipants with exam data\n(n = %s)", format(n_total, big.mark = ","))) +
  fbox(XL, ysN[2], sprintf("Aged \u2265 40 years\n(n = %s)", format(n_age, big.mark = ","))) +
  fbox(XL, ysN[3], sprintf("Fasting morning subsample\n(WTSAF2YR > 0, valid TG)\n(n = %s)", format(n_fast, big.mark = ","))) +
  fbox(XL, ysN[4], sprintf("Measured waist circumference\n(n = %s)", format(n_wc, big.mark = ","))) +
  fbox(XL, ysN[5], sprintf("Analytic cohort\n(n = 10,302; stroke 531)"), highlight = TRUE) +
  fbox(XL, ysN[6], sprintf("NDI linkage through Dec 31, 2019\nn = %s; median 6.9 y\n%s deaths (83 stroke deaths, I60-I69)",
                           format(ndi_elig, big.mark = ","),
                           format(ndi_death, big.mark = ",")), highlight = TRUE) +
  farr(XL, ysN[1] - 0.15, XL, ysN[2] + 0.15) +
  farr(XL, ysN[2] - 0.15, XL, ysN[3] + 0.15) +
  farr(XL, ysN[3] - 0.15, XL, ysN[4] + 0.15) +
  farr(XL, ysN[4] - 0.15, XL, ysN[5] + 0.15) +
  farr(XL, ysN[5] - 0.15, XL, ysN[6] + 0.15) +
  fexc(0.68, ysN[1] - 0.09, sprintf("excluded <40 y: %s", format(n_total - n_age, big.mark = ","))) +
  fexc(0.68, ysN[2] - 0.09, sprintf("nonfasting: %s", format(n_age - n_fast, big.mark = ","))) +
  fexc(0.68, ysN[3] - 0.09, sprintf("missing WC: %s", format(n_fast - n_wc, big.mark = ","))) +
  fexc(0.68, ysN[5] - 0.09, sprintf("linkage-ineligible: %s", 10302 - ndi_elig)) +
  annotate("text", x = XL, y = 0.995, label = "NHANES (cross-sectional + NDI prospective)",
           size = 3.4, fontface = "bold")

# ---------------- CHARLS 2011 branch (6 boxes) ----------------
gg <- gg +
  fbox(XM, ysC[1], sprintf("CHARLS 2011 blood sample\n(n = %s)", format(ch_blood, big.mark = ","))) +
  fbox(XM, ysC[2], sprintf("Complete TG + WC\n(n = %s; baseline stroke %s)",
                           format(ch_comp, big.mark = ","), ch_stroke0)) +
  fbox(XM, ysC[3], sprintf("Cross-sectional analytic cohort\n(n = %s)", format(ch_comp, big.mark = ",")), highlight = TRUE) +
  fbox(XM, ysC[4], sprintf("Baseline stroke-free\n(n = %s)", format(ch_prosp, big.mark = ","))) +
  fbox(XM, ysC[5], sprintf("Followed to 2018 (7 years)\nincident stroke (n = %s)", ch_inc)) +
  fbox(XM, ysC[6], sprintf("Prospective analytic cohort\n(n = %s; stroke %s)",
                           format(ch_prosp, big.mark = ","), ch_inc), highlight = TRUE) +
  farr(XM, ysC[1] - 0.15, XM, ysC[2] + 0.15) +
  farr(XM, ysC[2] - 0.15, XM, ysC[3] + 0.15) +
  farr(XM, ysC[2] - 0.15, XM, ysC[4] + 0.15) +
  farr(XM, ysC[4] - 0.15, XM, ysC[5] + 0.15) +
  farr(XM, ysC[5] - 0.15, XM, ysC[6] + 0.15) +
  fexc(XM + 0.36, ysC[1] - 0.09, sprintf("missing TG/WC: %s", format(ch_blood - ch_comp, big.mark = ","))) +
  fexc(XM + 0.36, ysC[2] + 0.02, sprintf("baseline stroke excluded: %s", ch_stroke0)) +
  annotate("text", x = XM, y = 0.995, label = "CHARLS 2011 (cross-sectional + prospective)",
           size = 3.4, fontface = "bold")

# ---------------- CHARLS 2015 replication branch (4 boxes) ----------------
gg <- gg +
  fbox(XR, ys15[1], sprintf("CHARLS 2015 blood sample\n(n = %s)", format(c15_blood, big.mark = ","))) +
  fbox(XR, ys15[2], sprintf("Complete TG + WC\n(n = %s; prevalent stroke %s)",
                            format(c15_comp, big.mark = ","), c15_stroke)) +
  fbox(XR, ys15[3], sprintf("Design-eligible\n(n = %s)", format(c15_design, big.mark = ","))) +
  fbox(XR, ys15[4], sprintf("Replication analytic cohort\nM1 n = %s; M2/M3 n = %s\n(stroke %s)",
                            format(c15_m1, big.mark = ","),
                            format(c15_m23, big.mark = ","), c15_ev), highlight = TRUE) +
  farr(XR, ys15[1] - 0.15, XR, ys15[2] + 0.15) +
  farr(XR, ys15[2] - 0.15, XR, ys15[3] + 0.15) +
  farr(XR, ys15[3] - 0.15, XR, ys15[4] + 0.15) +
  fexc(XR + 0.44, ys15[1] - 0.07, sprintf("missing TG/WC: %s", c15_blood - c15_comp)) +
  fexc(XR + 0.44, ys15[2] - 0.07, sprintf("design-ineligible: %s", c15_comp - c15_design)) +
  fexc(XR + 0.44, ys15[3] - 0.07, sprintf("covariate-missing\n(M2/M3): %s", format(c15_m1 - c15_m23, big.mark = ","))) +
  annotate("text", x = XR, y = 0.995, label = "CHARLS 2015 (replication)",
           size = 3.4, fontface = "bold")

ggsave(file.path(FIG, "Figure1_flow.pdf"), gg, width = 11.6, height = 6.8, device = "pdf", bg = "white")
ggsave(file.path(FIG, "Figure1_flow.png"), gg, width = 11.6, height = 6.8, dpi = 300, bg = "white")
cat("=== 13j FIGURE1 v3 COMPLETE ===\n")
