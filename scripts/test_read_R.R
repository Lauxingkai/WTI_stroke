library(haven)
library(fs)

# 测试读取有问题的文件
file_path <- "D:/NHANES/Data/2021-2022/Demographics/DEMO_L.xpt"

cat("Testing read_xpt...\n")
tryCatch({
  df <- read_xpt(file_path)
  cat("Success! Rows:", nrow(df), "Cols:", ncol(df), "\n")
  cat("First few columns:", paste(head(names(df)), collapse=", "), "\n")
}, error = function(e) {
  cat("Error:", conditionMessage(e), "\n")
})

# 检查其他L周期文件
cat("\nChecking other L files...\n")
l_files <- dir_ls("D:/NHANES/Data/2021-2022", regex = "\\.xpt$", recurse = TRUE)
for (f in l_files[1:5]) {
  cat(basename(f), ": ")
  tryCatch({
    df <- read_xpt(f)
    cat(nrow(df), "rows\n")
  }, error = function(e) {
    cat("Error\n")
  })
}
