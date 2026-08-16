# 检查本机 R 包安装情况
pkgs <- c("nhanesA","RNHANES","survey","srvyr","rms","tableone","gtsummary",
          "ggplot2","dplyr","tidyr","readxl","haven","foreign","splines",
          "emmeans","marginaleffects","EValue","forestplot","patchwork")
ip <- rownames(installed.packages())
miss <- setdiff(pkgs, ip)
cat("== 已安装 ==", "\n")
cat(pkgs[pkgs %in% ip], sep = "\n")
cat("\n== 缺失 ==", "\n")
cat(if (length(miss)) miss else "(无)", sep = "\n")
cat("\nR 版本:", R.version.string, "\n")
