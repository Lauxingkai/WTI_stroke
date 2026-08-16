# ============================================================================
# 06d_figure1_flow.R (v2)
# Figure 1: participant flow diagram (STROBE-style), dual-cohort layout.
# NHANES counts read from results/06a_nhanes_flow_counts.csv; CHARLS counts
# from results/01_linkage_checks.txt (asserted in 01_id_linkage.R).
# ggplot2 manual layout (21_flowchart_stroke.R pattern; Graphviz/DiagrammeR
# not installed on this machine). White background, black boxes.
# Output: output/figures/Figure1_flow.pdf + .png (300 dpi)
# Date: 2026-08-15
# ============================================================================
suppressPackageStartupMessages({library(ggplot2); library(dplyr); library(readr)})
RAW <- "D:/NHANES"; RES <- file.path(RAW, "results"); FIG <- file.path(RAW, "output/figures")

fc <- read_csv(file.path(RES, "06a_nhanes_flow_counts.csv"), show_col_types = FALSE)
n_total <- fc$n[fc$step == "7 cycles (D-J) participants with DEMO record"]
n_age   <- fc$n[fc$step == "Age >= 40"]
n_fast  <- fc$n[fc$step == "+ fasting subsample (WTSAF2YR > 0)"]
n_wc    <- fc$n[fc$step == "+ measured waist circumference"]

# CHARLS branch (source: results/01_linkage_checks.txt, 2026-08-15)
ch_blood <- 11847; ch_comp <- 9870; ch_stroke0 <- 220; ch_prosp <- 9650; ch_inc <- 507

fbox <- function(x, y, label, w = 0.66, h = 0.34, highlight = FALSE) {
  col <- if (highlight) "#0072B2" else "black"
  lwd <- if (highlight) 1.4 else 0.8
  list(
    annotate("rect", xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2,
             fill = "white", color = col, linewidth = lwd),
    annotate("text", x = x, y = y, label = label, size = 3.4,
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

XL <- 0.35; XR <- 1.15
ysN <- seq(0.92, 0.08, length.out = 5)
ysC <- seq(0.92, 0.08, length.out = 6)

gg <- ggplot() + theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA)) +
  xlim(0, 2.0) + ylim(0, 1)

# ---------------- NHANES branch (5 boxes) ----------------
gg <- gg +
  fbox(XL, ysN[1], sprintf("NHANES 2005-2018, cycles D-J\nparticipants with exam data\n(n = %s)", format(n_total, big.mark = ","))) +
  fbox(XL, ysN[2], sprintf("Aged \u2265 40 years\n(n = %s)", format(n_age, big.mark = ","))) +
  fbox(XL, ysN[3], sprintf("Fasting morning subsample\n(WTSAF2YR > 0, valid TG)\n(n = %s)", format(n_fast, big.mark = ","))) +
  fbox(XL, ysN[4], sprintf("Measured waist circumference\n(n = %s)", format(n_wc, big.mark = ","))) +
  fbox(XL, ysN[5], sprintf("Analytic cohort\n(n = 10,302; stroke 641)"), highlight = TRUE) +
  farr(XL, ysN[1] - 0.17, XL, ysN[2] + 0.17) +
  farr(XL, ysN[2] - 0.17, XL, ysN[3] + 0.17) +
  farr(XL, ysN[3] - 0.17, XL, ysN[4] + 0.17) +
  farr(XL, ysN[4] - 0.17, XL, ysN[5] + 0.17) +
  fexc(0.60, ysN[1] - 0.10, sprintf("excluded <40 y: %s", format(n_total - n_age, big.mark = ","))) +
  fexc(0.60, ysN[2] - 0.10, sprintf("nonfasting: %s", format(n_age - n_fast, big.mark = ","))) +
  fexc(0.60, ysN[3] - 0.10, sprintf("missing WC: %s", format(n_fast - n_wc, big.mark = ","))) +
  annotate("text", x = XL, y = 0.98, label = "NHANES (cross-sectional)",
           size = 3.6, fontface = "bold")

# ---------------- CHARLS branch (6 boxes) ----------------
gg <- gg +
  fbox(XR, ysC[1], sprintf("CHARLS 2011 blood sample\n(n = %s)", format(ch_blood, big.mark = ","))) +
  fbox(XR, ysC[2], sprintf("Complete TG + WC\n(n = %s; baseline stroke %s)",
                           format(ch_comp, big.mark = ","), ch_stroke0)) +
  fbox(XR, ysC[3], sprintf("Cross-sectional analytic cohort\n(n = %s)", format(ch_comp, big.mark = ",")), highlight = TRUE) +
  fbox(XR, ysC[4], sprintf("Baseline stroke-free\n(n = %s)", format(ch_prosp, big.mark = ","))) +
  fbox(XR, ysC[5], sprintf("Followed to 2018 (7 years)\nincident stroke (n = %s)", ch_inc)) +
  fbox(XR, ysC[6], sprintf("Prospective analytic cohort\n(n = %s; stroke %s)",
                           format(ch_prosp, big.mark = ","), ch_inc), highlight = TRUE) +
  farr(XR, ysC[1] - 0.17, XR, ysC[2] + 0.17) +
  farr(XR, ysC[2] - 0.17, XR, ysC[3] + 0.17) +
  farr(XR, ysC[2] - 0.17, XR, ysC[4] + 0.17) +
  farr(XR, ysC[4] - 0.17, XR, ysC[5] + 0.17) +
  farr(XR, ysC[5] - 0.17, XR, ysC[6] + 0.17) +
  fexc(XR + 0.42, ysC[1] - 0.10, sprintf("missing TG/WC: %s", format(ch_blood - ch_comp, big.mark = ","))) +
  fexc(XR + 0.42, ysC[2] + 0.05, sprintf("baseline stroke excluded: %s", ch_stroke0)) +
  annotate("text", x = XR, y = 0.98, label = "CHARLS (cross-sectional + prospective)",
           size = 3.6, fontface = "bold")

ggsave(file.path(FIG, "Figure1_flow.pdf"), gg, width = 9.0, height = 6.5, device = "pdf", bg = "white")
ggsave(file.path(FIG, "Figure1_flow.png"), gg, width = 9.0, height = 6.5, dpi = 300, bg = "white")
cat("=== 06d FIGURE1 COMPLETE ===\n")
