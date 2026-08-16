# 实测 nhanesA 包下载（DEMO_C, 2003-2004）
cat("== nhanesA 版本:", as.character(packageVersion("nhanesA")), "==\n")
t0 <- Sys.time()
res <- tryCatch({
  d <- nhanesA::nhanes("DEMO_C")
  list(ok = TRUE, d = d)
}, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))

if (res$ok) {
  d <- res$d
  cat("== 下载成功 ==\n")
  cat("行数:", nrow(d), " 列数:", ncol(d), " 耗时:", round(difftime(Sys.time(), t0, units = "secs"), 1), "s\n")
  cat("关键列存在性:\n")
  for (v in c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2","INDFMPIR","WTMEC2YR","SDMVPSU","SDMVSTRA")) {
    cat("  ", v, ":", v %in% names(d), "\n")
  }
  # 保存为 RDS 供后续使用
  dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
  saveRDS(d, "data/raw/DEMO_C.rds")
  cat("已保存 data/raw/DEMO_C.rds\n")
} else {
  cat("== 下载失败 ==\n")
  cat("错误:", res$msg, "\n")
}
