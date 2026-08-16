# ============================================================================
# 13f_ndi_parse.R
# NDI public-use linked mortality: parse 8 cycle files with the official
# NHANES fixed-width layout (R_ReadInProgramAllSurveys.R, CDC) and bind.
# Outputs: data/nhanes_mort2019.csv ; results/13f_ndi_checks.txt
# ============================================================================
suppressPackageStartupMessages({ library(readr); library(dplyr) })

RAW <- "D:/NHANES"
MORT <- file.path(RAW, "data/mortality")
RES <- file.path(RAW, "results")

logf <- file(file.path(RES, "13f_ndi_checks.txt"), open = "wt")
logline <- function(...) { cat(..., "\n"); cat(..., "\n", file = logf) }

# official NHANES layout (CDC R_ReadInProgramAllSurveys.R, 2019 LMF)
cols <- fwf_cols(seqn = c(1, 6), eligstat = c(15, 15), mortstat = c(16, 16),
                 ucod_leading = c(17, 19), diabetes = c(20, 20),
                 hyperten = c(21, 21), permth_int = c(43, 45),
                 permth_exm = c(46, 48))

files <- list.files(MORT, pattern = "^NHANES_.*_MORT_2019_PUBLIC\\.dat$",
                    full.names = TRUE)
logline(sprintf("files found: %d", length(files)))

out <- lapply(files, function(f) {
  d <- read_fwf(f, col_types = "iiiiiiii", col_positions = cols, na = c("", "."))
  d <- mutate(d, cycle = sub("^NHANES_(\\d{4}_\\d{4})_MORT.*", "\\1", basename(f)))
  logline(sprintf("%s: n=%d | eligible=%d | deceased=%d | stroke-death=%d | permth_int NA=%d",
                  basename(f), nrow(d), sum(d$eligstat == 1, na.rm = TRUE),
                  sum(d$mortstat == 1, na.rm = TRUE),
                  sum(d$ucod_leading == 5, na.rm = TRUE),
                  sum(is.na(d$permth_int))))
  d
})
mort <- bind_rows(out)
logline(sprintf("TOTAL: n=%d | deceased=%d | stroke-death (ucod 5)=%d",
                nrow(mort), sum(mort$mortstat == 1, na.rm = TRUE),
                sum(mort$ucod_leading == 5, na.rm = TRUE)))
logline(sprintf("duplicate seqn: %d", sum(duplicated(mort$seqn))))
logline("eligstat distribution:")
print(table(mort$eligstat, useNA = "ifany"))
logline("mortstat x ucod_leading (deceased):")
print(table(mort$mortstat, mort$ucod_leading, useNA = "ifany"))

write_csv(mort, file.path(RAW, "data/nhanes_mort2019.csv"))
logline("saved: D:/NHANES/data/nhanes_mort2019.csv")

# inspect the bundled rds (what is LMF2019_public_CJ.rds?)
rds_path <- file.path(MORT, "LMF2019_public_CJ.rds")
logline("\n--- bundled RDS probe ---")
rds <- readRDS(rds_path)
logline(sprintf("class: %s | dim: %s", class(rds)[1],
                paste(dim(rds), collapse = "x")))
logline(sprintf("names: %s", paste(head(names(rds), 12), collapse = ", ")))
if ("seqn" %in% names(rds)) {
  logline(sprintf("rds n=%d | deceased=%d | ucod5=%d | duplicate seqn=%d",
                  nrow(rds), sum(rds$mortstat == 1, na.rm = TRUE),
                  sum(rds$ucod_leading == 5, na.rm = TRUE),
                  sum(duplicated(rds$seqn))))
  logline(sprintf("seqn overlap with parsed files: %d / %d",
                  length(intersect(rds$seqn, mort$seqn)), nrow(rds)))
}
close(logf)
cat("\n=== 13f DONE ===\n")
