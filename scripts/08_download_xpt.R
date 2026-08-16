# 08_download_xpt.R — 用 manifest URL + download.file 下载 XPT（read_xpt 兼容版）
# C-J: nhanesA manifest 的 DataURL；L: Public/2021/DataFiles/；K: 用户 P_ XPT 拷贝
library(haven)
options(timeout = 300)
OUT <- "data/raw"

is_html <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  head <- readBin(con, "raw", n = 12)
  txt <- rawToChar(head, multiple = FALSE)
  grepl("<!DOCTYPE|<html", txt, ignore.case = TRUE)
}

# manifest 获取 C-J 表 URL
m <- nhanesA::nhanesManifest()
m$Table <- toupper(m$Table)
url_of <- function(tbl) {
  hit <- m[m$Table == tbl, ]
  if (nrow(hit)) hit$DataURL[1] else NA_character_
}

# 表清单（同 03 脚本）
table_map <- list(
  C = c("DEMO","DR1TOT","DR2TOT","DSQ1","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","L13"),
  D = c("DEMO","DR1TOT","DR2TOT","DSQ1","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  E = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  F = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  G = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  H = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  I = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  J = c("DEMO","DR1TOT","DR2TOT","DS1TOT","DS2TOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  K = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL"),
  L = c("DEMO","DR1TOT","DR2TOT","DSQTOT","MCQ","BMX","BPQ","DIQ","SMQ","ALQ","TCHOL","HDL")
)
K_URL <- c("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2019/DataFiles/%s.XPT",
           "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2020/DataFiles/%s.XPT")
L_URL <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/%s.XPT"

dl_xpt <- function(url, dest) {
  if (file.exists(dest)) { if (!is_html(dest)) return(TRUE) else file.remove(dest) }
  ok <- tryCatch({ download.file(url, dest, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) FALSE)
  if (ok && file.exists(dest) && !is_html(dest)) TRUE else { if (file.exists(dest)) file.remove(dest); FALSE }
}

ok_n <- 0; fail <- character()
for (sfx in names(table_map)) {
  cat("== 周期", sfx, "==\n")
  for (tb in table_map[[sfx]]) {
    tbl <- paste0(tb, "_", sfx)
    dest <- file.path(OUT, paste0(tbl, ".XPT"))
    if (sfx == "L") {
      got <- dl_xpt(sprintf(L_URL, tbl), dest)
    } else if (sfx == "K") {
      got <- any(sapply(K_URL, function(u) dl_xpt(sprintf(u, tbl), dest)))
    } else {
      u <- url_of(tbl)
      got <- !is.na(u) && dl_xpt(paste0("https://wwwn.cdc.gov", u), dest)
    }
    if (got) { ok_n <- ok_n + 1; cat("  ✓", tbl, "\n") }
    else { fail <- c(fail, tbl); cat("  ✗", tbl, "\n") }
  }
}
# K 周期：P_ 文件拷贝
K_map <- c(P_DEMO="DEMO_K", P_DR1TOT="DR1TOT_K", P_MCQ="MCQ_K", P_DSQTOT="DSQTOT_K",
           P_BMX="BMX_K", P_BPQ="BPQ_K", P_DIQ="DIQ_K", P_SMQ="SMQ_K",
           P_TCHOL="TCHOL_K", P_HDL="HDL_K")
src <- "D:/NHANES/VitminK_and_stroke/04Date/2017-2020"
for (p in names(K_map)) {
  f <- file.path(src, paste0(p, ".XPT"))
  if (file.exists(f)) {
    file.copy(f, file.path(OUT, paste0(K_map[[p]], ".XPT")), overwrite = TRUE)
    ok_n <- ok_n + 1; cat("  ✓", K_map[[p]], "(P_拷贝)\n")
  } else fail <- c(fail, K_map[[p]])
}
cat(sprintf("\n== 完成: %d 成功 ==", ok_n))
if (length(fail)) { cat("\n失败:"); cat(paste(fail, collapse = ", "), "\n") }
