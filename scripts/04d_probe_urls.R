# 04d_probe_urls.R — 用 R download.file 探测 K/L 周期正确 URL（R 能过挑战而 curl 不能）
library(haven)
options(timeout = 120)

probe <- function(url) {
  tmp <- tempfile(fileext = ".xpt")
  ok <- tryCatch({ download.file(url, tmp, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) FALSE)
  if (!ok || file.info(tmp)$size < 1000) { unlink(tmp); return("FAIL/空") }
  d <- tryCatch(read_xpt(tmp), error = function(e) NULL)
  unlink(tmp)
  if (is.null(d)) "HTML挑战页" else sprintf("OK %d行", nrow(d))
}

# 候选 URL 矩阵
cands <- c(
  "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/%s.XPT",
  "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2022/DataFiles/%s.XPT",
  "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2023/DataFiles/%s.XPT",
  "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2024/DataFiles/%s.XPT",
  "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2025/DataFiles/%s.XPT"
)
files <- c("DEMO_K", "DR1TOT_K", "DSQTOT_L", "DS1TOT_L", "DS2TOT_L", "DSQTOT_K")
for (fl in files) {
  cat("== ", fl, " ==\n")
  for (u in cands) {
    url <- sprintf(u, fl)
    r <- probe(url)
    cat("   ", sub("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/", "", url), "->", r, "\n")
    if (grepl("^OK", r)) break
  }
}
