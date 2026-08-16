# ============================================================================
# 00_charls_covariates.py
# Build CHARLS 2011 covariates with correct ID handling (pyreadstat keeps
# strL IDs intact, unlike R haven which misreads them as floats).
# Outputs: charls_covariates.csv, charls_tableS1.csv
# ============================================================================
import pyreadstat
import pandas as pd
import numpy as np

BASE = r"D:\NHANES\CHARLS\CHARLS_1725074232_3\CHARLS"
OUT = r"D:\NHANES\data\processed"

def id12(x):
    s = x.astype(str).str.strip()
    assert (s.str.len() == 11).all(), "2011 ID width != 11"
    return s.str[:9] + "0" + s.str[9:11]

hs11, _ = pyreadstat.read_dta(BASE + r"\2011\health_status_and_functioning.dta")
dm11, _ = pyreadstat.read_dta(BASE + r"\2011\demographic_background.dta")
bm11, _ = pyreadstat.read_dta(BASE + r"\2011\biomarkers.dta")
b11,  _ = pyreadstat.read_dta(BASE + r"\2011\Blood_20140429.dta")

print("hs11 ID len:", hs11.ID.astype(str).str.len().value_counts().to_dict())
print("dm11 ID len:", dm11.ID.astype(str).str.len().value_counts().to_dict())

def yn(x):
    return pd.to_numeric(x, errors="coerce") == 1

def pa_days(yes, days):
    y = yn(yes)
    d = pd.to_numeric(days, errors="coerce")
    out = np.where(y & d.notna(), d, 0)
    return np.nan_to_num(out)

hs = hs11.copy()
hs["ID_12"] = id12(hs.ID)

cov = pd.DataFrame({
    "ID_12": hs.ID_12,
    "htn": yn(hs.da007_1_),
    "dyslipid": yn(hs.da007_2_),
    "dm": yn(hs.da007_3_),
    "lipid_rx": pd.to_numeric(hs.da010_2_s2, errors="coerce") == 2,  # checkbox 2=checked
    "bp_rx": pd.to_numeric(hs.da011s2, errors="coerce") == 2,
    "smoke": yn(hs.da059),
    "smoke_now": yn(hs.da061),
    "drink": pd.to_numeric(hs.da067, errors="coerce").isin([1, 2]),
    "pa_days_week": pa_days(hs.da051_1_, hs.da052_1_) + pa_days(hs.da051_2_, hs.da052_2_),
})

dm = dm11.copy()
dm["ID_12"] = id12(dm.ID)
dm["age"] = 2011 - pd.to_numeric(dm.ba002_1, errors="coerce")   # birth year -> age
dm["age"] = dm.age.fillna(pd.to_numeric(dm.ba004, errors="coerce"))  # fallback
dm["sex"] = pd.to_numeric(dm.rgender, errors="coerce").where(
    lambda s: s.isin([1, 2]))                                   # keep 1=male, 2=female only
cov = cov.merge(
    dm[["ID_12", "age", "bd001", "sex"]].rename(columns={"bd001": "edu"}),
    on="ID_12", how="left")

bm = bm11.copy()
bm["ID_12"] = id12(bm.ID)
ht = pd.to_numeric(bm.qh006, errors="coerce")
ht_cm = np.where(ht.between(0.5, 3), ht * 100,         # metres -> cm
                 np.where(ht.between(30, 100), ht + 100,  # dropped leading digit (43->143)
                          np.where(ht.between(100, 250), ht, np.nan)))  # cm keep
bm["ht_cm"] = ht_cm
bm["wt_kg"] = pd.to_numeric(bm.ql002, errors="coerce")
bm["bmi"] = bm.wt_kg / (bm.ht_cm / 100) ** 2
bm.loc[~bm.bmi.between(10, 70), "bmi"] = np.nan                 # plausible range gate
cov = cov.merge(bm[["ID_12", "ht_cm", "wt_kg", "bmi"]], on="ID_12", how="left")

# bloodweight (survey weight for blood-based analyses) + community/urban strata
b = b11.copy()
b["ID_12"] = id12(b.ID)
cov = cov.merge(b[["ID_12", "bloodweight", "newglu", "newtg", "newhdl", "newldl", "newhba1c"]],
                on="ID_12", how="left")
cov["communityID"] = cov.ID_12.str[:7]
psu, _ = pyreadstat.read_dta(BASE + r"\2011\psu.dta")
psu["communityID"] = psu.communityID.astype(str).str.strip()
cov = cov.merge(psu[["communityID", "urban_nbs"]], on="communityID", how="left")

print("cov rows:", len(cov))
print("lipid_rx TRUE:", cov.lipid_rx.sum(), "| bp_rx TRUE:", cov.bp_rx.sum(),
      "| htn:", cov.htn.sum(), "| dm:", cov.dm.sum())
print("pa_days_week distribution:", cov.pa_days_week.describe().round(2).to_dict())

cov.to_csv(OUT + r"\charls_covariates.csv", index=False)

# Table S1: blood participation vs non-participation (age/sex/edu)
b_ids = set(id12(b11.ID))
s1 = cov.assign(in_blood=cov.ID_12.isin(b_ids)).groupby("in_blood").agg(
    n=("ID_12", "size"),
    age_mean=("age", "mean"), age_sd=("age", "std"),
    male_pct=("sex", lambda s: 100 * (pd.to_numeric(s, errors="coerce") == 1).mean()),
    edu_median=("edu", "median"),
).reset_index()
s1.to_csv(r"D:\NHANES\results\tableS1_blood_participation.csv", index=False)
print(s1.to_string())
print("DONE")
