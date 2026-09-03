# ============================================================================
# create_figure1_wti.R v14 (2026-09-01) — participant flow, STROBE style
# v14: SIMPLIFIED cohort-chain layout following the same-journal precedent
#      (Li et al., Lipids Health Dis 2025;24:7, Fig. 1): each column is a
#      top-down cohort chain (start -> key intermediate cohort -> analytic
#      cohort), one MERGED exclusion box per chain segment (multi-bullet),
#      model-layer detail (M1/M2-M3 fitted n) left to Results/Table 2.
#      Black/white with light-grey exclusion boxes; terminal analytic-cohort
#      boxes highlighted (penwidth 1.8). Fonts: main 13 pt, exclusion 11 pt
#      (>=7 pt physical at 170 mm / 300 dpi per make-figures rubric D.10).
# Counts: 06a_nhanes_flow_counts.csv / 01_id_linkage.R / 03_analysis.R;
#         stopifnot arithmetic balance.
# ============================================================================
suppressPackageStartupMessages({
  library(DiagrammeR); library(DiagrammeRsvg); library(rsvg); library(dplyr); library(readr)
})

RAW <- "D:/NHANES"; RES <- file.path(RAW, "results"); FIG <- file.path(RAW, "output/figures")

fc <- read_csv(file.path(RES, "06a_nhanes_flow_counts.csv"), show_col_types = FALSE)
n_exam <- fc$n[fc$step == "7 cycles (D-J) participants with DEMO record"]
n_age  <- fc$n[fc$step == "Age >= 40"]
n_fast <- fc$n[fc$step == "+ fasting subsample (WTSAF2YR > 0)"]
n_ana  <- fc$n[fc$step == "+ measured waist circumference"]

ndi_elig <- 10289
ch_blood <- 11847; ch_comp <- 9870; ch_stroke0 <- 220; ch_prosp <- 9650
c15_blood <- 13420; c15_comp <- 12899; c15_design <- 12501; c15_m1 <- 12213

stopifnot(
  n_exam == 70190, n_age == 26282, n_fast == 10766, n_ana == 10302,
  n_exam - n_age == 43908, n_age - n_fast == 15516, n_fast - n_ana == 464,
  n_ana - ndi_elig == 13,
  ch_blood - ch_comp == 1977, ch_comp - ch_prosp == ch_stroke0,
  c15_blood - c15_comp == 521, c15_comp - c15_design == 398,
  c15_design - c15_m1 == 288
)
cat(">> arithmetic balance OK\n")

STYLE_HEADER <- '  graph [layout=dot, rankdir=TB, fontname="Arial",
         splines=ortho, nodesep=0.40, ranksep=0.45]
  node  [shape=box, style=filled, fillcolor=white,
         color=black, fontname="Arial", fontsize=13, penwidth=1.2,
         margin="0.14,0.09"]
  edge  [color=black, arrowhead=normal, arrowsize=0.75, penwidth=1.0]'

nbox <- function(id, label, fontsize = 13, pw = 1.2) {
  sprintf('  %s [label="%s", fontsize=%d, penwidth=%.1f]', id, label, fontsize, pw)
}
nexc <- function(id, label) {
  sprintf('  %s [label="%s", fillcolor="#F0F0F0", fontsize=11]', id, label)
}
# zero-size invisible mid anchors: the two chain segments meet flush -> no break
nmid <- function(id) {
  sprintf('  %s [shape=point, style=invis, width=0, height=0, fixedsize=true, margin=0]', id)
}
edge   <- function(a, b) sprintf('  %s -> %s', a, b)
edgenh <- function(a, b) sprintf('  %s -> %s [arrowhead=none]', a, b)  # chain first half
edexc  <- function(a, b) sprintf('  %s -> %s [constraint=false]', a, b)  # mid -> exclusion (side-hang)
nk <- function(a, b) sprintf('  { rank=same; %s; %s }', a, b)

nodes <- c(
  # ---- Column A — NHANES ----
  nbox("A1", sprintf("NHANES 2005-2018\\n(n = %s)", format(n_exam, big.mark = ","))),
  nbox("A2", sprintf("Analytic cohort\\n(n = %s)", format(n_ana, big.mark = ","))),
  nbox("A3", sprintf("NDI-linked cohort\\n(n = %s)", format(ndi_elig, big.mark = ",")), pw = 1.8),
  nexc("eA1", sprintf("Excluded (n = %s)\\n\\l• Age < 40 y: %s\\l• Nonfasting: %s\\l• Missing WC: %s\\l",
        format(n_exam - n_ana, big.mark = ","),
        format(n_exam - n_age, big.mark = ","),
        format(n_age - n_fast, big.mark = ","),
        format(n_fast - n_ana, big.mark = ","))),
  nexc("eA2", sprintf("Excluded (n = %s)\\n\\l• No NDI linkage: %s\\l", n_ana - ndi_elig, n_ana - ndi_elig)),
  # ---- Column B — CHARLS 2011 ----
  nbox("B1", sprintf("CHARLS 2011 blood sample\\n(n = %s)", format(ch_blood, big.mark = ","))),
  nbox("B2", sprintf("Cross-sectional\\nanalytic cohort\\n(n = %s)", format(ch_comp, big.mark = ","))),
  nbox("B3", sprintf("Prospective analytic\\ncohort\\n(n = %s)", format(ch_prosp, big.mark = ",")), pw = 1.8),
  nexc("eB1", sprintf("Excluded (n = %s)\\n\\l• Missing TG/WC: %s\\l",
        format(ch_blood - ch_comp, big.mark = ","),
        format(ch_blood - ch_comp, big.mark = ","))),
  nexc("eB2", sprintf("Excluded (n = %s)\\n\\l• Baseline stroke: %s\\l", ch_stroke0, ch_stroke0)),
  # ---- Column C — CHARLS 2015 ----
  nbox("C1", sprintf("CHARLS 2015 blood sample\\n(n = %s)", format(c15_blood, big.mark = ","))),
  nbox("C2", sprintf("Complete TG + WC\\n(n = %s)", format(c15_comp, big.mark = ","))),
  nbox("C3", sprintf("Design-eligible\\n(n = %s)", format(c15_design, big.mark = ","))),
  nbox("C4", sprintf("Replication analytic\\ncohort\\n(n = %s)", format(c15_m1, big.mark = ",")), pw = 1.8),
  nexc("eC1", sprintf("Excluded (n = %s)\\n\\l• Missing TG/WC: %s\\l",
        format(c15_blood - c15_comp, big.mark = ","),
        format(c15_blood - c15_comp, big.mark = ","))),
  nexc("eC2", sprintf("Excluded (n = %s)\\n\\l• Not design-eligible: %s\\l",
        format(c15_comp - c15_design, big.mark = ","),
        format(c15_comp - c15_design, big.mark = ","))),
  nexc("eC3", sprintf("Excluded (n = %s)\\n\\l• Incomplete WTI, sex, or stroke status: %s\\l",
        format(c15_design - c15_m1, big.mark = ","),
        format(c15_design - c15_m1, big.mark = ",")))
)

mids <- c(
  nmid("mA12"), nmid("mA23"),
  nmid("mB12"), nmid("mB23"),
  nmid("mC12"), nmid("mC23"), nmid("mC34")
)

edges <- c(
  edgenh("A1","mA12"), edge("mA12","A2"), edgenh("A2","mA23"), edge("mA23","A3"),
  edexc("mA12","eA1"), edexc("mA23","eA2"),
  edgenh("B1","mB12"), edge("mB12","B2"), edgenh("B2","mB23"), edge("mB23","B3"),
  edexc("mB12","eB1"), edexc("mB23","eB2"),
  edgenh("C1","mC12"), edge("mC12","C2"), edgenh("C2","mC23"), edge("mC23","C3"),
  edgenh("C3","mC34"), edge("mC34","C4"),
  edexc("mC12","eC1"), edexc("mC23","eC2"), edexc("mC34","eC3")
)

ranks <- c(
  nk("mA12","eA1"), nk("mA23","eA2"),
  nk("mB12","eB1"), nk("mB23","eB2"),
  nk("mC12","eC1"), nk("mC23","eC2"), nk("mC34","eC3"),
  "  { rank=same; A1; B1; C1 }"
)

dot <- paste(c('digraph wti_flow {', STYLE_HEADER, nodes, mids, edges, ranks, '}'), collapse = "\n")
writeLines(dot, file.path(RAW, "results/figure1_flow.dot"))

svg <- export_svg(grViz(dot))
raw <- charToRaw(svg)
# 170 mm full-width @ 300 dpi = 2007 px (BMC spec); 600 dpi line-art copy retained
rsvg_png(raw, file.path(FIG, "Figure1_flow.png"), width = 2007)
rsvg_png(raw, file.path(FIG, "Figure1_flow_600.png"), width = 4014)
tryCatch({
  rsvg_pdf(raw, file.path(FIG, "Figure1_flow.pdf"))
}, error = function(e) {
  cat("!! Figure1_flow.pdf blocked (viewer open):", conditionMessage(e), "\n")
})
cat("=== create_figure1_wti.R v14 COMPLETE ===\n")
