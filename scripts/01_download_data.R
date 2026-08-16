# ============================================================
# 01_download_data.R — NHANES 维生素K-卒中项目数据下载
# 周期：2003-04 (C) 至 2021-22 (L) 共 10 周期
# 模式1（推荐）：本地 XPT 目录 —— 浏览器手动下载 XPT 放入
#                D:/nhanes_xpt/（或 NHANES_LOCAL_XPT_DIR 指定），
#                绕过 CDC Akamai bot 防护
# 模式2：R 在线下载兜底（可能被 Akamai 拦截，视网络环境）
# 用法：
#   Rscript 01_download_data.R            # 在线下载全部（模式2）
#   Rscript 01_download_data.R --test     # 仅测试连通性（下载 DEMO_C）
#   Rscript 01_download_data.R --local    # 仅从本地目录核对文件存在性
# ============================================================

options(timeout = 300)

# ---------- 周期与表定义 ----------
cycles <- data.frame(
  year   = c("2003-2004","2005-2006","2007-2008","2009-2010","2011-2012",
             "2013-2014","2015-2016","2017-2018","2019-2020","2021-2022"),
  suffix = c("C","D","E","F","G","H","I","J","K","L"),
  stringsAsFactors = FALSE
)

# 核心表（每周期均需）
core_tables <- c("DEMO","DR1TOT","DR2TOT","DS1TOT","DS2TOT",
                 "MCQ","BMX","BPQ","DIQ","SMQ","ALQ")

# 血脂表：跨周期表名不同（TC/HDL）
# 2003-04: L13_C (LBXTC, LBDHDD)
# 2005-06: TCHOL_D + HDL_D
# 2007-08 起: TCHOL_* + HDL_*
lipid_tables <- data.frame(
  suffix = c("C","D","E","F","G","H","I","J","K","L"),
  tchol  = c("L13","TCHOL","TCHOL","TCHOL","TCHOL","TCHOL","TCHOL","TCHOL","TCHOL","TCHOL"),
  hdl    = c("L13","HDL","HDL","HDL","HDL","HDL","HDL","HDL","HDL","HDL"),
  stringsAsFactors = FALSE
)

# ---------- 路径配置 ----------
local_dir <- Sys.getenv("NHANES_LOCAL_XPT_DIR", unset = "D:/nhanes_xpt")
data_dir  <- "data/raw"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# ---------- 工具函数 ----------
is_xpt_file <- function(path) {
  # 真正的 XPT 以 SAS 二进制头开始；Akamai 拦截页为 HTML（<!DOCTYPE）
  if (!file.exists(path)) return(FALSE)
  con <- file(path, "rb"); on.exit(close(con))
  head <- readBin(con, "raw", n = 12)
  txt <- rawToChar(head, multiple = FALSE)
  !grepl("<!DOCTYPE|<html", txt, ignore.case = TRUE)
}

xpt_url <- function(table, suffix) {
  paste0("https://wwwn.cdc.gov/Nchs/Nhanes/",
         cycles$year[cycles$suffix == suffix], "/",
         table, "_", suffix, ".XPT")
}

download_one <- function(table, suffix, dest_dir) {
  fn <- paste0(table, "_", suffix, ".XPT")
  dest <- file.path(dest_dir, fn)
  if (file.exists(dest) && is_xpt_file(dest)) { message("  [skip] ", fn, " (已存在且有效)"); return(TRUE) }
  url <- xpt_url(table, suffix)
  message("  [get ] ", url)
  ok <- tryCatch({
    download.file(url, dest, mode = "wb", quiet = TRUE)
    is_xpt_file(dest)
  }, error = function(e) { message("  [FAIL] ", conditionMessage(e)); FALSE })
  if (!ok) {
    if (file.exists(dest)) file.remove(dest)
    message("  [FAIL] ", fn, " 非有效 XPT（被 Akamai 拦截）")
  }
  ok
}

# ---------- 主流程 ----------
args <- commandArgs(trailingOnly = TRUE)
mode <- if ("--test" %in% args) "test" else if ("--local" %in% args) "local" else "online"

# nhanesA 包可用性（实测：本网络环境下同样被 Akamai 拦截，仅作记录）
has_nhanesA <- requireNamespace("nhanesA", quietly = TRUE)
if (has_nhanesA) {
  message("nhanesA ", as.character(utils::packageVersion("nhanesA")),
          " 已安装（注意：实测其内部 download.file 同样被 CDC Akamai 拦截）")
}

if (mode == "test") {
  # 连通性测试：仅 DEMO_C（2003-2004 最小表）
  message("== 连通性测试：DEMO_C.XPT ==")
  ok <- download_one("DEMO", "C", data_dir)
  if (ok) {
    sz <- file.info(file.path(data_dir, "DEMO_C.XPT"))$size
    message("== 测试成功：DEMO_C.XPT 大小 ", sz, " bytes ==")
  } else {
    message("== 在线下载被拦截（预期内）。请手动下载 XPT 至 ", local_dir, " ==")
    message("   并在运行时设置环境变量 NHANES_LOCAL_XPT_DIR=", local_dir)
  }
  quit(save = "no")
}

if (mode == "local") {
  message("== 本地核对模式：检查 ", local_dir, " ==")
  total <- 0
  for (i in seq_len(nrow(cycles))) {
    sfx <- cycles$suffix[i]
    for (tb in core_tables) {
      fn <- file.path(local_dir, paste0(tb, "_", sfx, ".XPT"))
      if (file.exists(fn)) { total <- total + 1 } else {
        message("  缺失: ", basename(fn))
      }
    }
  }
  message("== 本地文件核对完成：", total, "/", nrow(cycles) * length(core_tables),
          " 个核心表存在 ==")
  quit(save = "no")
}

# online 模式
message("== 在线下载模式（可能被 Akamai 拦截）==")
for (i in seq_len(nrow(cycles))) {
  sfx <- cycles$suffix[i]
  message("[周期 ", cycles$year[i], " (", sfx, ")]")
  for (tb in core_tables) download_one(tb, sfx, data_dir)
  # 血脂表
  lt <- lipid_tables[lipid_tables$suffix == sfx, ]
  if (lt$tchol == "L13") download_one("L13", sfx, data_dir)
  else download_one(lt$tchol, sfx, data_dir)
  if (lt$hdl == "L13") {} else download_one(lt$hdl, sfx, data_dir)
}
message("== 下载流程完成 ==")
message("提示：若上述文件多数下载失败（HTTP 403），说明被 CDC Akamai 拦截。")
message("请运行: Rscript scripts/00_make_download_manifest.R 生成清单，")
message("然后用浏览器按清单（data/download_manifest.csv 共 129 个文件）")
message("逐个下载至 D:/nhanes_xpt/，再运行: Rscript scripts/01_download_data.R --local 核对。")
