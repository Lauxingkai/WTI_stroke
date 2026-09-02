# 05_check_pfiles_vars.R — 检查 K 周期 P_ 文件变量完整性
library(haven)
base <- "D:/NHANES/VitminK_and_stroke/04Date/2017-2020"
chk <- function(nm, vars) {
  f <- file.path(base, nm)
  d <- tryCatch(read_xpt(f), error = function(e) NULL)
  if (is.null(d)) { cat(nm, ": 读取失败\n"); return(invisible(NULL)) }
  hit <- intersect(vars, names(d))
  cat(sprintf("%-16s n=%6d  关键变量: %s\n", nm, nrow(d), paste(hit, collapse = ", ")))
}
chk("P_DEMO.XPT",   c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH3","DMDEDUC2","DMDMARTL","INDFMPIR","RIDEXPRG","WTMEC2YR","SDMVPSU","SDMVSTRA"))
chk("P_DR1TOT.XPT", c("SEQN","DR1TVK","DR1TKCAL","DR1TSATF","DR1TCHOL"))
chk("P_MCQ.XPT",    c("SEQN","MCQ160E"))
chk("P_DSQTOT.XPT", c("SEQN","DSQTVK","DSQTKCAL"))
chk("P_BMX.XPT",    c("SEQN","BMXBMI"))
chk("P_BPQ.XPT",    c("SEQN","BPQ020","BPQ080"))
chk("P_DIQ.XPT",    c("SEQN","DIQ010"))
chk("P_SMQ.XPT",    c("SEQN","SMQ020","SMQ040"))
chk("P_TCHOL.XPT",  c("SEQN","LBXTC"))
chk("P_HDL.XPT",    c("SEQN","LBDHDD"))
