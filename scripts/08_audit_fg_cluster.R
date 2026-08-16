# ============================================================================
# 08_audit_fg_cluster.R  (Phase 4, T17: Fine-Gray SE under community clustering)
# The published FG models use cmprsk::crr (iid SE, NO cluster support), while
# the Cox models used cluster(communityID). Recompute the M1 sHR SE via
# community-block bootstrap and compare with the iid CI.
# Output: qc/phase4_fg_cluster.txt
# Date: 2026-08-16 | Seed: 42
# ============================================================================
suppressPackageStartupMessages({library(survival); library(cmprsk); library(dplyr); library(readr)})
set.seed(42)
RAW <- "D:/NHANES"; OUT <- file.path(RAW, "data/processed"); QC <- file.path(RAW, "qc")
lines <- character(0)
logline <- function(...) { l <- sprintf(...); lines <<- c(lines, l); cat(l, "\n") }

pr <- read_csv(file.path(OUT, "charls_2011_2018_prosp_cov.csv"), show_col_types = FALSE)
ev <- read_csv(file.path(OUT, "charls_events_2011_2018.csv"), show_col_types = FALSE)
d <- pr %>% left_join(ev, by = "ID_12") %>%
  mutate(WTI_sd = (WTI - mean(WTI, na.rm=TRUE)) / sd(WTI, na.rm=TRUE),
         sex_m = ifelse(sex == 1, 1, 0),
         age = as.numeric(age)) %>%
  filter(!is.na(WTI_sd) & !is.na(age) & !is.na(bmi) & !is.na(stroke)) %>%
  mutate(fstatus = ifelse(stroke, 1, ifelse(death, 2, 0)),
         ftime = pmin(time, 7.0))
logline("analytic n=%d stroke=%d death=%d", nrow(d), sum(d$stroke), sum(d$death))

# original iid FG M1
fg0 <- crr(d$ftime, d$fstatus, cov1 = as.matrix(d[, c("WTI_sd", "age", "sex_m")]),
           failcode = 1, cencode = 0)
s0 <- summary(fg0)$conf.int[1, ]
hr0 <- s0[1]; lo0 <- s0[3]; hi0 <- s0[4]
logline("iid FG M1: sHR=%.4f (%.4f-%.4f)", hr0, lo0, hi0)

# community-block bootstrap
comms <- unique(d$communityID)
B <- 500
hr_b <- numeric(B)
for (b in seq_len(B)) {
  cb <- sample(comms, replace = TRUE)
  idx <- which(d$communityID %in% cb)
  db <- d[idx, ]
  f <- tryCatch(crr(db$ftime, db$fstatus,
                    cov1 = as.matrix(db[, c("WTI_sd", "age", "sex_m")]),
                    failcode = 1, cencode = 0), error = function(e) NULL)
  hr_b[b] <- if (is.null(f)) NA_real_ else summary(f)$conf.int[1, 1]
}
ok <- is.finite(hr_b)
sd_b <- sd(log(hr_b[ok]))
ci_b_norm <- exp(log(hr0) + c(-1, 1) * 1.96 * sd_b)
ci_b_pct <- unname(quantile(hr_b[ok], c(0.025, 0.975)))
logline("block bootstrap (B=%d, failed=%d): SD(log sHR)=%.4f", B, sum(!ok), sd_b)
logline("normal CI: %.4f-%.4f | percentile CI: %.4f-%.4f", ci_b_norm[1], ci_b_norm[2], ci_b_pct[1], ci_b_pct[2])
logline("iid CI width %.4f vs block-norm width %.4f (ratio %.2f)",
        log(hi0) - log(lo0), diff(log(ci_b_norm)), diff(log(ci_b_norm)) / (log(hi0) - log(lo0)))

# compare p
z_b <- log(hr0) / sd_b
logline("p (iid) = %.3g | p (block) = %.3g", s0[5], 2 * pnorm(-abs(z_b)))
writeLines(lines, file.path(QC, "phase4_fg_cluster.txt"))
cat("\nDONE\n")
