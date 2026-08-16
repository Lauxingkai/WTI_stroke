# 测试 wininet 下载方法（Windows 系统网络栈，行为接近浏览器）
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.XPT"
dest <- "data/raw/DEMO_C.XPT"
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
if (file.exists(dest)) file.remove(dest)

t0 <- Sys.time()
ok <- tryCatch({
  download.file(url, dest, mode = "wb", method = "wininet", quiet = TRUE)
  TRUE
}, error = function(e) { cat("错误:", conditionMessage(e), "\n"); FALSE })

if (ok && file.exists(dest)) {
  sz <- file.info(dest)$size
  cat("== wininet 下载成功 ==\n")
  cat("大小:", sz, "bytes, 耗时:", round(difftime(Sys.time(), t0, units = "secs"), 1), "s\n")
  # 验证 XPT 可读
  d <- tryCatch(haven::read_xpt(dest), error = function(e) NULL)
  if (!is.null(d)) cat("XPT 可读: 行数", nrow(d), "列数", ncol(d), "\n")
} else {
  cat("== wininet 失败（HTTP 403 或网络错误）==\n")
}
