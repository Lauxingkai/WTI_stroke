# ============================================================
# 03_batch_download_nhanesA.R — 用 nhanesA 包批量下载 2003-2022 全 10 周期
# 输出: data/raw/<TABLE>_<SUFFIX>.rds（已含 XPT 解析结果，省去 read_xpt）
# 表映射说明（按 NHANES 历史）：
#   C/D (2003-06): 补充剂仅 DSQ1 问卷（无营养素）→ 无 DS1TOT/DS2TOT
#   E-I (2007-16): 补充剂为 DSQTOT（含两天营养素 DSQTK1/DSQTK2）
#   J/K/L (2017-22): 补充剂拆为 DS1TOT + DS2TOT
#   血脂: C=L13(含TC+HDL), D 起 TCHOL + HDL 分表
# ============================================================

library(nhanesA)
options(timeout = 300)

OUT <- "data/raw"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

table_map <- list(
  C = c("DEMO","DR1TOT","DR2TOT","DSQ1","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","L13"),
  D = c("DEMO","DR1TOT","DR2TOT","DSQ1","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  E = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  F = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  G = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  H = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  I = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  J = c("DEMO","DR1TOT","DR2TOT","DS1TOT","DS2TOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  K = c("DEMO","DR1TOT","DR2TOT","DS1TOT","DS2TOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  L = c("DEMO","DR1TOT","DR2TOT","DS1TOT","DS2TOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL")
)

years <- c(C="2003-2004", D="2005-2006", E="2007-2008", F="2009-2010", G="2011-2012",
           H="2013-2014", I="2015-2016", J="2017-2018", K="2019-2020", L="2021-2022")

total <- sum(lengths(table_map))
done <- 0; fail <- character()
for (sfx in names(table_map)) {
  cat(sprintf("\n===== 周期 %s (%s) =====\n", sfx, years[[sfx]]))
  for (tb in table_map[[sfx]]) {
    dest <- file.path(OUT, sprintf("%s_%s.rds", tb, sfx))
    if (file.exists(dest)) { done <- done + 1; cat("  [skip]", basename(dest), "\n"); next }
    ok <- FALSE
    for (attempt in 1:2) {
      cat("  [get ]", sprintf("%s_%s", tb, sfx), sprintf("(尝试%d)", attempt), "\n")
      d <- tryCatch(nhanesA::nhanes(sprintf("%s_%s", tb, sfx)), error = function(e) NULL)
      if (!is.null(d) && nrow(d) > 0) {
        saveRDS(d, dest)
        ok <- TRUE
        cat("    ✓", nrow(d), "行 ×", ncol(d), "列\n")
        break
      }
      Sys.sleep(5)
    }
    if (ok) done <- done + 1 else { fail <- c(fail, sprintf("%s_%s", tb, sfx)); cat("    ✗ 失败\n") }
  }
}
cat(sprintf("\n========== 完成: %d/%d ==========\n", done, total))
if (length(fail)) {
  cat("失败清单:\n"); cat(paste(fail, collapse = "\n"), "\n")
  writeLines(fail, file.path(OUT, "download_failures.txt"))
} else {
  cat("全部下载成功！\n")
}
