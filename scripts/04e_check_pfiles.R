# 检查用户本地 P_ 文件（04Date/2017-2020/）是否含 2019-2020 (K) 数据
library(haven)
f <- "F:/总/xiaoiheiwu/VitminK_and_stroke/04Date/2017-2020/P_DR1TOT.XPT"
if (file.exists(f)) {
  d <- read_xpt(f)
  cat("P_DR1TOT: 行数", nrow(d), "列数", ncol(d), "\n")
  cat("SEQN 范围:", min(d$SEQN, na.rm = TRUE), "-", max(d$SEQN, na.rm = TRUE), "\n")
  cat("维生素K变量:", paste(grep("VK", names(d), value = TRUE), collapse = ","), "\n")
  cat("关键列:", paste(intersect(c("SEQN","DR1TVK","DR1TKCAL"), names(d)), collapse = ","), "\n")
} else cat("文件不存在\n")

# 对比：2017-18 (J) 的 SEQN 范围
d2 <- read_xpt("F:/总/xiaoiheiwu/VitminK_and_stroke/04Date/2017-2020/P_DEMO.XPT")
cat("\nP_DEMO: 行数", nrow(d2), "\n")
cat("SEQN 范围:", min(d2$SEQN, na.rm = TRUE), "-", max(d2$SEQN, na.rm = TRUE), "\n")
