# nhanesA 在新网络下实测下载 DEMO_C
library(nhanesA)
cat("nhanesA 版本:", as.character(packageVersion("nhanesA")), "\n")
t0 <- Sys.time()
res <- tryCatch({
  d <- nhanesA::nhanes("DEMO_C")
  list(ok = TRUE, d = d)
}, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))

if (res$ok) {
  d <- res$d
  cat("下载返回: 行数", nrow(d), " 列数", ncol(d),
      " 耗时", round(difftime(Sys.time(), t0, units = "secs"), 1), "s\n")
  if (nrow(d) > 0) {
    cat("关键变量:", paste(intersect(c("SEQN","RIAGENDR","RIDAGEYR","WTMEC2YR"), names(d)), collapse = ","), "\n")
    dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
    saveRDS(d, "data/raw/DEMO_C.rds")
    cat("已保存 data/raw/DEMO_C.rds\n")
  } else {
    cat("!! 空数据（大概率仍被 Akamai 拦截）\n")
  }
} else {
  cat("下载失败:", res$msg, "\n")
}
