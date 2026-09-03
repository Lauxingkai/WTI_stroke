suppressPackageStartupMessages({library(survey); library(haven); library(dplyr); library(readr)})
nh <- read_csv("D:/NHANES/data/processed/nhanes_fasting_cross_cov_v2.csv", show_col_types = FALSE)
des <- lapply(c("D","E","F","G","H","I","J"), function(cy) {
  demo <- read_xpt(file.path("D:/NHANES/data/raw", sprintf("DEMO_%s.XPT", cy)))
  demo %>% transmute(SEQN, SDMVSTRA, SDMVPSU, CYCLE = cy)
}) %>% bind_rows()
nh <- nh %>%
  left_join(des, by = c("SEQN" = "SEQN", "CYCLE.x" = "CYCLE")) %>%
  rename(CYCLE = CYCLE.x) %>%
  mutate(
    wt = WTSAF / 7,
    psu = paste0(CYCLE, "_", SDMVPSU),
    stra = paste0(CYCLE, "_", SDMVSTRA),
    wc_thr = ifelse(RIAGENDR == 1, 90, 80),
    htgw = as.numeric(BMXWAIST >= wc_thr & TG_mmol >= 1.69),
    pa_ter = cut(pa_mvpaw_min, quantile(pa_mvpaw_min, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                 include.lowest = TRUE, labels = c("L", "M", "H")),
    stroke_y = as.numeric(stroke)
  )
print(dim(nh))
print(table(nh$htgw, useNA = "ifany"))
print(table(nh$stroke_y, useNA = "ifany"))
nhd <- svydesign(ids = ~psu, strata = ~stra, weights = ~wt, data = nh, nest = TRUE)
f <- svyglm(stroke_y ~ htgw + RIDAGEYR + RIAGENDR, family = quasibinomial(), design = nhd)
print(summary(f)$coefficients)
cat("coef htgw:", coef(f)["htgw"], "\n")
