# 00_verify_local.R — 关卡①本地数据变量验证（用户已有 XPT）
library(haven)

dirs <- c("F:/总/xiaoiheiwu/VitminK_and_stroke/04Date",
          "F:/总/xiaoiheiwu/魔鬼训练营/NHANSE/04数据库")
files <- list.files(dirs, pattern = "\\.XPT$", full.names = TRUE, recursive = TRUE)
cat("本地 XPT 总数:", length(files), "\n\n")

# 查找指定文件
find_f <- function(pat) {
  hit <- files[grepl(pat, files, ignore.case = TRUE)]
  if (length(hit)) hit[1] else NA
}

checks <- list(
  c("DR1TOT_C", "DR1TOT_C", "维生素K(膳食2003-04)"),
  c("DR1TOT_J", "DR1TOT_J", "维生素K(膳食2017-18)"),
  c("DSQTOT_E", "DSQTOT_E", "补充剂VK(2007-08)"),
  c("DSQTOT_J", "DSQTOT_J", "补充剂VK(2017-18)"),
  c("DS1TOT_J", "DS1TOT_J", "补充剂VK(2017-18 DS1)"),
  c("MCQ_C", "MCQ_C", "卒中问卷(2003-04)"),
  c("MCQ_J", "MCQ_J", "卒中问卷(2017-18)"),
  c("DEMO_C", "DEMO_C", "人口学/权重(2003-04)"),
  c("DEMO_J", "DEMO_J", "人口学/权重(2017-18)")
)

for (chk in checks) {
  pat <- chk[1]; label <- chk[3]
  fp <- find_f(paste0(pat, "\\.XPT"))
  if (is.na(fp)) { cat("== ", label, ": 文件缺失\n"); next }
  d <- tryCatch(read_xpt(fp), error = function(e) NULL)
  if (is.null(d)) { cat("== ", label, ": 读取失败\n"); next }
  nm <- names(d)
  # 维生素K候选变量
  vk <- nm[grepl("VK|TVK|TK", nm, ignore.case = TRUE)]
  cat("== ", label, " (", basename(fp), ")\n", sep = "")
  cat("   行数:", nrow(d), " 维生素K相关变量:", paste(vk, collapse = ", "), "\n")
  # 其他关键变量抽查
  for (v in c("SEQN","MCQ160E","WTMEC2YR","SDMVPSU","SDMVSTRA","RIAGENDR","RIDAGEYR")) {
    if (v %in% nm) cat("   ", v, "✓")
  }
  cat("\n\n")
}
