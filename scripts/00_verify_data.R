# 00_verify_data.R — 关卡①：数据完整性 + 关键变量验证
library(haven)

raw <- "data/raw"
files <- list.files(raw, pattern = "\\.XPT$", full.names = TRUE)
cat("== 文件计数:", length(files), "/ 129 ==\n")
sz <- sum(file.info(files)$size)
cat("== 总大小:", round(sz / 1024^2, 1), "MB ==\n\n")

# 按周期检查关键变量
cycles <- data.frame(
  year = c("2003-2004","2005-2006","2007-2008","2009-2010","2011-2012",
           "2013-2014","2015-2016","2017-2018","2019-2020","2021-2022"),
  sfx = c("C","D","E","F","G","H","I","J","K","L"),
  stringsAsFactors = FALSE)

cat("== 各周期关键变量检查 ==\n")
for (i in seq_len(nrow(cycles))) {
  sfx <- cycles$sfx[i]; yr <- cycles$year[i]
  f <- function(tb) file.path(raw, paste0(tb, "_", sfx, ".XPT"))
  out <- character()
  if (file.exists(f("DEMO"))) {
    d <- read_xpt(f("DEMO"))
    out <- c(out, sprintf("DEMO:%d行", nrow(d)),
             ifelse(all(c("SEQN","RIAGENDR","RIDAGEYR","WTMEC2YR","SDMVPSU","SDMVSTRA") %in% names(d)), "权重✓", "权重✗"))
  } else out <- c(out, "DEMO缺失")
  if (file.exists(f("DR1TOT"))) {
    d <- read_xpt(f("DR1TOT"))
    vk <- intersect(c("DR1TVK","VB1K"), names(d))
    out <- c(out, sprintf("DR1TOT:%d行 VK=%s", nrow(d),
                          if (length(vk)) paste0(vk, collapse="/") else "✗缺失"))
  } else out <- c(out, "DR1TOT缺失")
  if (file.exists(f("MCQ"))) {
    d <- read_xpt(f("MCQ"))
    out <- c(out, sprintf("MCQ160E:%s", ifelse("MCQ160E" %in% names(d), "✓", "✗")))
  } else out <- c(out, "MCQ缺失")
  if (file.exists(f("DS1TOT"))) {
    d <- read_xpt(f("DS1TOT"))
    vk <- intersect(c("DS1K","DS1TVK","DS1VK"), names(d))
    out <- c(out, sprintf("DS1TOT VK=%s", if (length(vk)) paste0(vk, collapse="/") else "✗缺失"))
  } else out <- c(out, "DS1TOT缺失")
  cat(yr, "(", sfx, "):", paste(out, collapse = " | "), "\n")
}
