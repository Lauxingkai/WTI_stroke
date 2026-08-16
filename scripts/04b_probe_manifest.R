# 探查 nhanesA manifest 中 K/L 周期表与 URL
library(nhanesA)
cat("== nhanesA 函数检查 ==\n")
cat("nhanesTables 存在:", "nhanesTables" %in% getNamespaceExports("nhanesA"), "\n")
cat("nhanesManifest 存在:", "nhanesManifest" %in% getNamespaceExports("nhanesA"), "\n")

# 列出 2019-2020 周期全部表（含 URL）
for (yr in c("2019-2020", "2021-2022")) {
  cat("\n===== 周期", yr, "=====\n")
  tb <- tryCatch(nhanesA::nhanesTables(cycle = yr, details = TRUE),
                 error = function(e) NULL)
  if (is.null(tb)) { cat("nhanesTables 失败\n"); next }
  print(tb[, c("Table", "Years", "Date.Published")], row.names = FALSE)
  cat("表数量:", nrow(tb), "\n")
}
