# 验证官方 2019 public-use LMF（对照用户本地 CSV）
suppressPackageStartupMessages(library(readr))
dir <- "D:/NHANES/data/mortality"
years <- c("2003_2004","2005_2006","2007_2008","2009_2010","2011_2012",
           "2013_2014","2015_2016","2017_2018")
all_rows <- list()
for (y in years) {
  f <- file.path(dir, paste0("NHANES_", y, "_MORT_2019_PUBLIC.dat"))
  d <- read_fwf(f,
                col_positions = fwf_cols(seqn = c(1, 14), eligstat = c(15, 15),
                                         mortstat = c(16, 16), ucod_leading = c(17, 19),
                                         permth_int = c(43, 45), permth_exm = c(46, 48)),
                col_types = cols(seqn = col_character(), eligstat = col_integer(),
                                 mortstat = col_integer(), ucod_leading = col_integer(),
                                 permth_int = col_integer(), permth_exm = col_integer()))
  d$cycle <- y
  all_rows[[y]] <- d
  cat(sprintf("%s: n=%d 死亡=%d\n", y, nrow(d), sum(d$mortstat == 1, na.rm = TRUE)))
}
mort_official <- bind_rows(all_rows)
saveRDS(mort_official, file.path(dir, "LMF2019_public_CJ.rds"))
cat("合计 n =", nrow(mort_official), "死亡 =", sum(mort_official$mortstat == 1, na.rm = TRUE), "\n")

# 对照用户本地 CSV
local <- read.csv("D:/OneDrive - Business/小黑屋/level 1专题课/第19期NHANES/NHANES数据库挖掘专题课课前资料/3.死亡数据/NHANES_MORT_1999_2018.csv")
both <- merge(mort_official, local, by.x = "seqn", by.y = "SEQN")
cat("官方与本地 SEQN 匹配:", nrow(both), "/", nrow(mort_official), "\n")
cat("mortstat 一致性:", mean(both$mortstat == both$mortstat.y, na.rm = TRUE), "\n")
cat("permth_int 一致性:", mean(both$permth_int == both$permth_int.y, na.rm = TRUE), "\n")
