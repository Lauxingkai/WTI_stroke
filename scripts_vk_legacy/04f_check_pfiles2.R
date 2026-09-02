# 校准 SEQN 范围：判断 P_ 文件是否含 K 周期（2019-2020）
library(dplyr)

# 已下载的 DEMO_J（2017-18）与 DEMO_L（2021-22）作参照
dj <- readRDS("D:/NHANES/data/raw/DEMO_J.rds")
dl <- readRDS("D:/NHANES/data/raw/DEMO_L.rds")
cat("DEMO_J (2017-18): SEQN", min(dj$SEQN), "-", max(dj$SEQN), " n=", nrow(dj), "\n")
cat("DEMO_L (2021-22): SEQN", min(dl$SEQN), "-", max(dl$SEQN), " n=", nrow(dl), "\n")

# P_DEMO（用户 2017-2020 合并）
pd <- haven::read_xpt("D:/NHANES/VitminK_and_stroke/04Date/2017-2020/P_DEMO.XPT")
cat("P_DEMO: SEQN", min(pd$SEQN), "-", max(pd$SEQN), " n=", nrow(pd), "\n")
# SEQN 落在 J 范围（< max J）与 K 范围（> max J）的计数
jmax <- max(dj$SEQN)
cat("  <= 2017-18 max (", jmax, "):", sum(pd$SEQN <= jmax), " 行（J 部分）\n")
cat("  > 2017-18 max:     ", sum(pd$SEQN > jmax), " 行（K 部分）\n")

# P_MCQ / P_DR1TOT 同样划分
pm <- haven::read_xpt("D:/NHANES/VitminK_and_stroke/04Date/2017-2020/P_MCQ.XPT")
cat("\nP_MCQ: n=", nrow(pm), " MCQ160E:", "MCQ160E" %in% names(pm), "\n")
pr <- haven::read_xpt("D:/NHANES/VitminK_and_stroke/04Date/2017-2020/P_DR1TOT.XPT")
cat("P_DR1TOT: n=", nrow(pr), " DR1TVK:", "DR1TVK" %in% names(pr), "\n")

# P_DSQTOT 补充剂
pds <- tryCatch(haven::read_xpt("D:/NHANES/VitminK_and_stroke/04Date/2017-2020/P_DSQTOT.XPT"), error = function(e) NULL)
if (!is.null(pds)) cat("P_DSQTOT: n=", nrow(pds), " VK变量:", paste(grep("VK", names(pds), value = TRUE), collapse = ","), "\n")
