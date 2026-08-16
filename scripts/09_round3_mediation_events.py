# ============================================================================
# 09_round3_mediation_events.py  (Round 3: items 6 & 8)
# 6) Mediation product-of-coefficients point estimates, independent Python path
#    (weighted OLS for M~X; weighted Logit for Y~X+M), vs 05e R survey output.
# 8) Event table independent rebuild: stk13/15/18 counts from raw .dta via a
#    different code structure, vs 03c events file.
# Output: qc/round3_mediation_events.txt
# Date: 2026-08-16
# ============================================================================
import pandas as pd
import numpy as np
import pyreadstat
import statsmodels.api as sm

RAW = r"D:\NHANES"; BASE = RAW + r"\CHARLS\CHARLS_1725074232_3\CHARLS"
OUT = RAW + r"\data\processed"
lines = []
def log(s=""):
    lines.append(str(s)); print(s)

# ---------- 6) mediation point estimates ----------
pr = pd.read_csv(OUT + r"\charls_2011_2018_prosp_cov.csv", dtype={"ID_12": str})
ev = pd.read_csv(OUT + r"\charls_events_2011_2018.csv", dtype={"ID_12": str})
md = pd.read_csv(OUT + r"\charls_2015_mediator.csv", dtype={"ID_12": str})
d = pr.merge(ev, on="ID_12", how="left").merge(md, on="ID_12", how="left")
d["WTI_sd"] = (d["WTI"] - d["WTI"].mean()) / d["WTI"].std()
d["sex_m"] = (d["sex"] == 1).astype(float)
d["sex_f"] = (d["sex"] == 2).astype(float)
d["age"] = pd.to_numeric(d["age"], errors="coerce")
d["w"] = d["bloodweight"] / d["bloodweight"].mean()
d["kappa"] = np.where(d["sex"] == 2, 0.7, 0.9)
d["alpha"] = np.where(d["sex"] == 2, -0.241, -0.302)
d["eGFR"] = (142 * np.minimum(d["crea_2015"] / d["kappa"], 1) ** d["alpha"] *
             np.maximum(d["crea_2015"] / d["kappa"], 1) ** (-1.200) *
             0.9938 ** d["age"] * 1.012 ** d["sex_f"])
d["lnCRP"] = np.log(d["crp_2015"])
d["Y"] = ((d["stk15"] | d["stk18"]).astype(float))
d = d[(~d["stk13"].fillna(False)) & (d["death_t"].isna() | (d["death_t"] >= 4))].copy()
C = ["age", "sex_m", "edu", "smoke", "drink", "bmi", "htn", "dm", "lipid_rx", "bp_rx", "pa_days_week"]

def med(Mvar, tag):
    dd = d[d[Mvar].notna() & d["Y"].notna()].copy()
    dd = dd.dropna(subset=["WTI_sd", Mvar, "Y"] + C)
    Xm = sm.add_constant(dd[["WTI_sd"] + C].astype(float))
    Xy = sm.add_constant(dd[["WTI_sd", Mvar] + C].astype(float))
    mM = sm.WLS(dd[Mvar].astype(float), Xm, weights=dd["w"]).fit()
    mY = sm.GLM(dd["Y"], Xy, family=sm.families.Binomial(), freq_weights=dd["w"]).fit()
    a = mM.params["WTI_sd"]; b = mY.params[Mvar]; cp = mY.params["WTI_sd"]
    log(f"{tag}: n={len(dd)} events={int(dd.Y.sum())} | a={a:.5f} b={b:.5f} c'={cp:.5f} indirect={a*b:.5f} total={a*b+cp:.5f}")

med("lnCRP", "lnCRP")
med("eGFR", "eGFR")
log("05e R refs: lnCRP indirect=-0.0014 direct=0.0369 total=0.0356 | eGFR indirect=0.0002 direct=0.0352 total=0.0354")

# ---------- 8) event table independent rebuild ----------
def sid(s):
    return s.astype(str).str.strip()
hs13, _ = pyreadstat.read_dta(BASE + r"\2013\Health_Status_and_Functioning.dta", usecols=["ID", "da019_w2_1"])
hs15, _ = pyreadstat.read_dta(BASE + r"\2015\Health_Status_and_Functioning.dta", usecols=["ID", "da019_w2_1", "zda007_8_"])
hs18, _ = pyreadstat.read_dta(BASE + r"\2018\Health_Status_and_Functioning.dta", usecols=["ID", "da019_w2_1", "da007_8_"])
h11, _ = pyreadstat.read_dta(BASE + r"\2011\health_status_and_functioning.dta", usecols=["ID", "da007_8_"])
h11["ID_12"] = sid(h11.ID).str[:9] + "0" + sid(h11.ID).str[9:11]
base_ids = set(pd.read_csv(OUT + r"\charls_2011_2018_prosp_cov.csv", usecols=["ID_12"], dtype={"ID_12": str})["ID_12"])
s13 = pd.DataFrame({"ID_12": sid(hs13.ID), "stk13": pd.to_numeric(hs13.da019_w2_1, errors="coerce") == 1})
s15 = pd.DataFrame({"ID_12": sid(hs15.ID),
                    "stk15": ((pd.to_numeric(hs15.da019_w2_1, errors="coerce") == 1) |
                              (pd.to_numeric(hs15.zda007_8_, errors="coerce") == 1))})
s18 = pd.DataFrame({"ID_12": sid(hs18.ID),
                    "stk18": ((pd.to_numeric(hs18.da019_w2_1, errors="coerce") == 1) |
                              (pd.to_numeric(hs18.da007_8_, errors="coerce") == 1))})
m = pd.DataFrame({"ID_12": sorted(base_ids)})
m = m.merge(s13, on="ID_12", how="left").merge(s15, on="ID_12", how="left").merge(s18, on="ID_12", how="left")
for c in ["stk13", "stk15", "stk18"]:
    m[c] = m[c].fillna(False)
m["stroke"] = m.stk13 | m.stk15 | m.stk18
log(f"\nrebuild: n={len(m)} stk13={int(m.stk13.sum())} stk15={int(m.stk15.sum())} stk18={int(m.stk18.sum())} total stroke={int(m.stroke.sum())}")
old = pd.read_csv(OUT + r"\charls_events_2011_2018.csv", dtype={"ID_12": str})
log(f"03c ref: n={len(old)} stroke={int(old.stroke.sum())} (stk13/stk15/stk18 counts below)")
log("03c events stroke_t dist:", old[old.stroke].stroke_t.value_counts().sort_index().to_dict())
log("rebuild by wave (first-wave approximation):", {
    "2y": int(m.stk13.sum()), "4y": int((~m.stk13 & m.stk15).sum()), "7y": int((~m.stk13 & ~m.stk15 & m.stk18).sum())})

open(RAW + r"\qc\round3_mediation_events.txt", "w", encoding="utf-8").write("\n".join(lines))
print("\nDONE")
