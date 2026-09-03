# 18b_render_dag.R — DAG figure (Stage-6 B2) via DiagrammeR + rsvg (same pipeline as Figure 1)
suppressPackageStartupMessages({library(DiagrammeR); library(DiagrammeRsvg); library(rsvg)})
RAW <- "D:/NHANES"; FIG <- file.path(RAW, "output/figures")
dot <- readLines(file.path(FIG, "fig_dag_wti.dot"), warn = FALSE, encoding = "UTF-8")
dot <- paste(dot, collapse = "\n")
svg <- export_svg(grViz(dot))
raw <- charToRaw(svg)
tryCatch(
  rsvg_png(raw, file.path(FIG, "FigureS1_DAG.png"), width = 2007),
  error = function(e) cat("!! png fail:", conditionMessage(e), "\n"))
tryCatch(
  rsvg_pdf(raw, file.path(FIG, "FigureS1_DAG.pdf")),
  error = function(e) cat("!! pdf fail:", conditionMessage(e), "\n"))
cat("=== 18b DAG COMPLETE ===\n")