# 探查 nhanesA manifest（无周期参数版本）与 K/L 表 URL
library(nhanesA)
m <- tryCatch(nhanesA::nhanesManifest(), error = function(e) NULL)
if (is.null(m)) {
  cat("nhanesManifest 失败\n")
} else {
  cat("manifest 维度:", dim(m), "\n")
  cat("列名:", paste(names(m), collapse = ", "), "\n")
  # 找 K/L 周期表
  k <- m[grepl("_K$", m$table) | grepl("_L$", m$table), ]
  cat("K/L 表数量:", nrow(k), "\n")
  print(k[, c("table", "url", "public_year")], row.names = FALSE)
}
