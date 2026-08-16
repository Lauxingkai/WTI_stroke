# ============================================================
# 00_make_download_manifest.R — 生成 CDC 数据手动下载清单
# 输出: data/download_manifest.csv（周期、表、URL、目标文件名）
# 用途: CDC Akamai 拦截自动下载时，用户用浏览器按清单批量下载
# ============================================================

cycles <- data.frame(
  year   = c("2003-2004","2005-2006","2007-2008","2009-2010","2011-2012",
             "2013-2014","2015-2016","2017-2018","2019-2020","2021-2022"),
  suffix = c("C","D","E","F","G","H","I","J","K","L"),
  stringsAsFactors = FALSE
)

core_tables <- c("DEMO","DR1TOT","DR2TOT","DS1TOT","DS2TOT",
                 "MCQ","BMX","BPQ","DIQ","SMQ","ALQ")

# 血脂表：C 周期在 L13（含 TC+HDL），D 起 TCHOL + HDL 分开
lipid_tables <- data.frame(
  suffix = c("C","D","E","F","G","H","I","J","K","L"),
  tchol  = c("L13","TCHOL","TCHOL","TCHOL","TCHOL","TCHOL","TCHOL","TCHOL","TCHOL","TCHOL"),
  hdl    = c(NA,"HDL","HDL","HDL","HDL","HDL","HDL","HDL","HDL","HDL"),
  stringsAsFactors = FALSE
)

rows <- list()
for (i in seq_len(nrow(cycles))) {
  sfx <- cycles$suffix[i]
  yr  <- cycles$year[i]
  for (tb in core_tables) {
    rows[[length(rows) + 1]] <- data.frame(
      cycle = yr, table = tb, file = paste0(tb, "_", sfx, ".XPT"),
      url = paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", yr, "/", tb, "_", sfx, ".XPT"),
      stringsAsFactors = FALSE)
  }
  lt <- lipid_tables[lipid_tables$suffix == sfx, ]
  rows[[length(rows) + 1]] <- data.frame(
    cycle = yr, table = lt$tchol, file = paste0(lt$tchol, "_", sfx, ".XPT"),
    url = paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", yr, "/", lt$tchol, "_", sfx, ".XPT"),
    stringsAsFactors = FALSE)
  if (!is.na(lt$hdl)) {
    rows[[length(rows) + 1]] <- data.frame(
      cycle = yr, table = lt$hdl, file = paste0(lt$hdl, "_", sfx, ".XPT"),
      url = paste0("https://wwwn.cdc.gov/Nchs/Nhanes/", yr, "/", lt$hdl, "_", sfx, ".XPT"),
      stringsAsFactors = FALSE)
  }
}

manifest <- do.call(rbind, rows)
dir.create("data", showWarnings = FALSE, recursive = TRUE)
write.csv(manifest, "data/download_manifest.csv", row.names = FALSE)
cat("清单已生成: data/download_manifest.csv\n")
cat("文件总数:", nrow(manifest), "\n")
cat("按周期分布:\n")
print(table(manifest$cycle))
cat("总数据量估算: DR1TOT/DR2TOT 每文件约 5-20MB，合计约 1-2 GB\n")
