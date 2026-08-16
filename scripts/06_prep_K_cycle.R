# 06_prep_K_cycle.R — K 周期（2019-2020）P_ 文件转 RDS 统一命名 + J 周期补充剂结构检查
library(haven)
src <- "D:/NHANES/VitminK_and_stroke/04Date/2017-2020"
dst <- "D:/NHANES/data/raw"

# P_ -> K 命名映射
map <- c(P_DEMO="DEMO_K", P_DR1TOT="DR1TOT_K", P_MCQ="MCQ_K", P_DSQTOT="DSQTOT_K",
         P_BMX="BMX_K", P_BPQ="BPQ_K", P_DIQ="DIQ_K", P_SMQ="SMQ_K",
         P_TCHOL="TCHOL_K", P_HDL="HDL_K")
for (p in names(map)) {
  f <- file.path(src, paste0(p, ".XPT"))
  if (!file.exists(f)) { cat("缺失:", p, "\n"); next }
  d <- read_xpt(f)
  saveRDS(d, file.path(dst, paste0(map[[p]], ".rds")))
  cat(sprintf("%s -> %s.rds (%d行)\n", p, map[[p]], nrow(d)))
}

# J 周期补充剂表结构确认
cat("\n== J 周期补充剂表 ==\n")
for (tb in c("DSQTOT_J", "DS1TOT_J", "DS2TOT_J")) {
  f <- file.path(dst, paste0(tb, ".rds"))
  if (file.exists(f)) {
    d <- readRDS(f)
    cat(sprintf("%s: %d行, VK变量: %s\n", tb, nrow(d),
                paste(grep("VK", names(d), value = TRUE), collapse = ",")))
  } else cat(tb, ": 不存在\n")
}
