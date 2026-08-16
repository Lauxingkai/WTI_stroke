# 04_fix_downloads.R — 修复 K 周期 + L 补充剂表下载
# nhanesA manifest 的 Public/2021/ 前缀导致 404；改用直连 URL 候选
library(haven)
options(timeout = 300)
OUT <- "data/raw"

# K 周期表
k_tables <- c("DEMO","DR1TOT","DR2TOT","DS1TOT","DS2TOT","MCQ","BMX","BPQ",
              "DIQ","SMQ","ALQ","TCHOL","HDL")
k_urls <- c(
  "https://wwwn.cdc.gov/Nchs/Nhanes/2019-2020/%s.XPT",
  "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2022/DataFiles/%s.XPT",
  "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/%s.XPT"
)

dl_rds <- function(url, dest) {
  tmp <- tempfile(fileext = ".xpt")
  ok <- tryCatch({ download.file(url, tmp, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) FALSE)
  if (!ok || file.info(tmp)$size < 1000) return(FALSE)
  d <- tryCatch(read_xpt(tmp), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(FALSE)
  saveRDS(d, dest)
  unlink(tmp)
  TRUE
}

# ---- K 周期 ----
cat("===== K 周期 (2019-2020) =====\n")
for (tb in k_tables) {
  dest <- file.path(OUT, sprintf("%s_K.rds", tb))
  if (file.exists(dest)) { cat("  [skip]", tb, "\n"); next }
  got <- FALSE
  for (u in k_urls) {
    url <- sprintf(u, paste0(tb, "_K"))
    if (dl_rds(url, dest)) {
      d <- readRDS(dest)
      cat(sprintf("  ✓ %s_K (%d行×%d列) <- %s\n", tb, nrow(d), ncol(d), url))
      got <- TRUE; break
    }
  }
  if (!got) cat("  ✗", tb, "失败\n")
}

# ---- L 周期补充剂 ----
cat("===== L 周期补充剂表 =====\n")
# 候选：DSQTOT_L（可能命名回归）与正确路径的 DS1TOT_L/DS2TOT_L
cand <- list(
  DS1TOT_L = c("https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/DS1TOT_L.XPT",
               "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2024/DataFiles/DS1TOT_L.XPT"),
  DS2TOT_L = c("https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/DS2TOT_L.XPT",
               "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2024/DataFiles/DS2TOT_L.XPT"),
  DSQTOT_L = c("https://wwwn.cdc.gov/Nchs/Nhanes/2021-2022/DSQTOT_L.XPT",
               "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2024/DataFiles/DSQTOT_L.XPT")
)
for (nm in names(cand)) {
  dest <- file.path(OUT, paste0(nm, ".rds"))
  if (file.exists(dest)) { cat("  [skip]", nm, "\n"); next }
  got <- FALSE
  for (u in cand[[nm]]) {
    if (dl_rds(u, dest)) {
      d <- readRDS(dest)
      cat(sprintf("  ✓ %s (%d行×%d列) VK变量: %s\n", nm, nrow(d), ncol(d),
                  paste(grep("VK", names(d), value = TRUE), collapse = ",")))
      got <- TRUE; break
    }
  }
  if (!got) cat("  ✗", nm, "失败\n")
}
cat("===== 修复完成 =====\n")
