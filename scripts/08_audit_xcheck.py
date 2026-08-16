# ============================================================================
# 08_audit_xcheck.py  (Phase 5, Iron Rule 2: independent-implementation check)
# Recompute NHANES M1 (stroke ~ WTI_sd + age + sex) with Python statsmodels
# weighted logistic + cluster-robust (psu) SE, compare with R survey output
# (OR 1.077, 95% CI 1.005-1.154, p=0.0367).
# Date: 2026-08-16
# ============================================================================
import pandas as pd
import numpy as np
import pyreadstat
import statsmodels.api as sm

OUT = r"D:\NHANES\data\processed"
NRAW = r"D:\NHANES\data\raw"

nh = pd.read_csv(OUT + r"\nhanes_fasting_cross_cov.csv")
frames = []
for cy in list("DEFGHIJ"):
    df, _ = pyreadstat.read_xport(NRAW + rf"\DEMO_{cy}.XPT", usecols=["SEQN", "SDMVSTRA", "SDMVPSU"])
    df["CYCLE"] = cy
    frames.append(df)
des = pd.concat(frames, ignore_index=True)
nh = nh.merge(des, left_on=["SEQN", "CYCLE.x"], right_on=["SEQN", "CYCLE"], how="left")
nh["wt"] = nh["WTSAF"] / 7
nh["psu"] = nh["CYCLE.y"].astype(str) + "_" + nh["SDMVPSU"].astype(str)
nh["WTI_sd"] = (nh["WTI"] - nh["WTI"].mean()) / nh["WTI"].std()

X = sm.add_constant(nh[["WTI_sd", "RIDAGEYR", "RIAGENDR"]].astype(float))
y = nh["stroke"].astype(float)
w = nh["wt"]

m = sm.GLM(y, X, family=sm.families.Binomial(), freq_weights=w).fit(cov_type="cluster", cov_kwds={"groups": nh["psu"]})
b = m.params["WTI_sd"]; se = m.bse["WTI_sd"]
print(f"independent check NHANES M1: OR={np.exp(b):.4f} CI=({np.exp(b-1.96*se):.4f}-{np.exp(b+1.96*se):.4f}) p={m.pvalues['WTI_sd']:.4f}")
print(f"R survey reference        : OR=1.077 CI=(1.005-1.154) p=0.0367")
print("n=", len(nh), "events=", int(y.sum()))
