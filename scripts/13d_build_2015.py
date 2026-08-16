# ============================================================================
# 13d_build_2015.py
# Opt-1: CHARLS 2015 blood-based cross-sectional replication layer.
# Mirrors the 2011 pipeline (00_charls_covariates.py + 01_id_linkage.R):
#   WTI = waist(cm, qm002) x TG(mmol/L, bl_tg x 0.01129)
#   outcome: prevalent stroke by 2015 = zda007_8_==1 | da007_8_==1
#   design : ids=communityID, strata=urban_nbs (2011 psu.dta, 100% coverage),
#            weights=Blood_weight (normalized downstream in R, as 2011)
# Outputs: data/charls_2015_cross_cov.csv (sandbox: pwsh cannot write
#          data/processed/, so the 2015 layer output lives in data/); console checks
# ============================================================================
import pyreadstat
import pandas as pd
import numpy as np

BASE = r"D:\NHANES\CHARLS\CHARLS_1725074232_3\CHARLS"
OUT = r"D:\NHANES\data\processed"

def yn(x):
    return pd.to_numeric(x, errors="coerce") == 1

def pa_days(yes, days):
    y = yn(yes)
    d = pd.to_numeric(days, errors="coerce")
    out = np.where(y & d.notna(), d, 0)
    return np.nan_to_num(out)

def cid(x):
    return x.astype(str).str.strip().str.replace(r"\.0$", "", regex=True)

bl, _ = pyreadstat.read_dta(BASE + r"\2015\Blood.dta")
bm, _ = pyreadstat.read_dta(BASE + r"\2015\Biomarker.dta")
hs, _ = pyreadstat.read_dta(BASE + r"\2015\Health_Status_and_Functioning.dta")
dm, _ = pyreadstat.read_dta(BASE + r"\2015\Demographic_Background.dta")
si, _ = pyreadstat.read_dta(BASE + r"\2015\Sample_Infor.dta")
psu, _ = pyreadstat.read_dta(BASE + r"\2011\psu.dta")
dm13, _ = pyreadstat.read_dta(BASE + r"\2013\Demographic_Background.dta")

# ---- base: blood participants (2015 ID is already 12-digit) ----
bl = bl.copy()
bl["ID_12"] = bl.ID.astype(str).str.strip()
assert bl.ID_12.str.len().eq(12).all(), "2015 ID width != 12"
print("2015 blood participants:", len(bl))

df = pd.DataFrame({
    "ID_12": bl.ID_12,
    "TG_mgdl": pd.to_numeric(bl.bl_tg, errors="coerce"),
    "bloodweight": pd.to_numeric(bl.Blood_weight, errors="coerce"),
})
df["TG_mmol"] = df.TG_mgdl * 0.01129

# ---- biomarker: waist, height, weight ----
bm = bm.copy()
bm["ID_12"] = bm.ID.astype(str).str.strip()
bmf = bm[["ID_12", "qm002", "qi002", "ql002"]].rename(
    columns={"qm002": "WC_raw", "qi002": "ht_raw", "ql002": "wt_raw"})
df = df.merge(bmf, on="ID_12", how="left")

wc = pd.to_numeric(df.WC_raw, errors="coerce")
df["WC_cm"] = np.where(wc.between(40, 200), wc, np.nan)
ht = pd.to_numeric(df.ht_raw, errors="coerce")
df["ht_cm"] = np.where(ht.between(0.5, 3), ht * 100,
              np.where(ht.between(30, 100), ht + 100,
              np.where(ht.between(100, 250), ht, np.nan)))
wt = pd.to_numeric(df.wt_raw, errors="coerce")
df["wt_kg"] = np.where(wt.between(25, 200), wt, np.nan)
df["bmi"] = df.wt_kg / (df.ht_cm / 100) ** 2
df.loc[~df.bmi.between(10, 70), "bmi"] = np.nan
df["WTI"] = df.WC_cm * df.TG_mmol

# ---- health: diseases, meds, lifestyle ----
hs = hs.copy()
hs["ID_12"] = hs.ID.astype(str).str.strip()
hz = hs[["ID_12", "zda007_1_", "zda007_2_", "zda007_3_", "zda007_8_",
         "da007_1_", "da007_2_", "da007_3_", "da007_8_",
         "da007_w2_2_8_", "da019_w2_1",
         "da010_2_s2", "da011s2",
         "da059", "da061", "da067",
         "da051_1_", "da052_1_", "da051_2_", "da052_2_",
         "xrgender"]].copy()

for k, i in [("htn", 1), ("dyslipid", 2), ("dm", 3), ("stroke", 8)]:
    hz[f"_{k}_z"] = yn(hz[f"zda007_{i}_"])
    hz[f"_{k}_d"] = yn(hz[f"da007_{i}_"])
hz["stroke_base_na"] = hz["_stroke_z"].isna() & hz["_stroke_d"].isna()
hz["htn"] = hz["_htn_z"].fillna(False) | hz["_htn_d"].fillna(False)
hz["dyslipid"] = hz["_dyslipid_z"].fillna(False) | hz["_dyslipid_d"].fillna(False)
hz["dm"] = hz["_dm_z"].fillna(False) | hz["_dm_d"].fillna(False)
hz["stroke_base"] = hz["_stroke_z"].fillna(False) | hz["_stroke_d"].fillna(False)
hz["stroke_phys"] = hz.stroke_base & (yn(hz.da007_w2_2_8_) | yn(hz.da019_w2_1))
hz["panel_zda"] = hz["_htn_z"].notna() | hz["_stroke_z"].notna()
hz["lipid_rx"] = pd.to_numeric(hz.da010_2_s2, errors="coerce") == 2
hz["bp_rx"] = pd.to_numeric(hz.da011s2, errors="coerce") == 2
hz["smoke"] = yn(hz.da059)
hz["smoke_now"] = pd.to_numeric(hz.da061, errors="coerce") == 1
hz["drink"] = pd.to_numeric(hz.da067, errors="coerce").isin([1, 2])
hz["pa_days_week"] = (pa_days(hz.da051_1_, hz.da052_1_) +
                      pa_days(hz.da051_2_, hz.da052_2_))
hz["sex_hs"] = pd.to_numeric(hz.xrgender, errors="coerce")

df = df.merge(hz.drop(columns=[c for c in hz.columns if c.startswith("_")]),
              on="ID_12", how="left")

# ---- demographic: sex, birth year, education ----
dm = dm.copy()
dm["ID_12"] = dm.ID.astype(str).str.strip()
byr = pd.to_numeric(dm.ba004_w3_1, errors="coerce")
byr_self = pd.to_numeric(dm.ba002_1, errors="coerce")
# birth year = self-reported when it differs from ID card/Hukou, else ID-card year
birth_year = byr.where(~(yn(dm.ba002) & byr_self.notna()), byr_self)
dm["age"] = 2015 - birth_year
dm.loc[~dm.age.between(20, 120), "age"] = np.nan
dm["sex"] = pd.to_numeric(dm.ba000_w2_3, errors="coerce").where(
    lambda s: s.isin([1, 2]))
dm["edu_raw"] = pd.to_numeric(dm.bd001_w2_4, errors="coerce")
dm["edu"] = dm.edu_raw.where(dm.edu_raw.between(1, 11))
df = df.merge(dm[["ID_12", "age", "sex", "edu", "edu_raw"]], on="ID_12", how="left")
df["sex"] = df.sex.fillna(df.sex_hs)

# ---- education carry-forward (bd001_w2_4==12 = no change since 2013):
#      2015-reported (1-11) -> 2011 covariates -> 2013 demographic ----
cov11 = pd.read_csv(OUT + r"\charls_covariates.csv", usecols=["ID_12", "edu"],
                    dtype={"ID_12": str})
cov11 = cov11.rename(columns={"edu": "edu_11"})
df = df.merge(cov11, on="ID_12", how="left")
carry11 = df.edu.isna() & df.edu_11.notna()
df.loc[carry11, "edu"] = df.loc[carry11, "edu_11"]

dm13 = dm13.copy()
dm13["ID_12"] = dm13.ID.astype(str).str.strip()
e13a = pd.to_numeric(dm13.bd001, errors="coerce")
e13b = pd.to_numeric(dm13.bd001_w2_4, errors="coerce")
e13z = pd.to_numeric(dm13.zbd001, errors="coerce")
e13 = e13z.where(e13z.between(1, 11),
      e13a.where(e13a.between(1, 11), e13b.where(e13b.between(1, 11))))
dm13["edu_13"] = e13
df = df.merge(dm13[["ID_12", "edu_13"]], on="ID_12", how="left")
carry13 = df.edu.isna() & df.edu_13.notna()
df.loc[carry13, "edu"] = df.loc[carry13, "edu_13"]
print("edu: 2015-reported n=%d | carried from 2011 n=%d | carried from 2013 n=%d | still NA n=%d" %
      (df.edu_raw.between(1, 11).sum(), carry11.sum(), carry13.sum(), df.edu.isna().sum()))

# ---- sample info: crosssection flag (semantics unclear in release; kept raw)
#      + panel indicator from zda carry-over records ----
si = si.copy()
si["ID_12"] = si.ID.astype(str).str.strip()
df = df.merge(si[["ID_12", "crosssection"]].rename(
    columns={"crosssection": "cs_flag_raw"}), on="ID_12", how="left")
print("cs_flag_raw: 1=%d 0=%d | panel (any zda defined): %d" %
      (int((df.cs_flag_raw == 1).sum()), int((df.cs_flag_raw == 0).sum()),
       int(df.panel_zda.sum())))
print("panel with edu_raw==12: %d; of these carried: %d" %
      ((df.edu_raw == 12).sum(),
       ((df.edu_raw == 12) & (df.edu_11.notna() | df.edu_13.notna())).sum()))

# ---- strata: 2011 psu urban_nbs (100% coverage verified in 13c) ----
psu = psu.copy()
psu["communityID"] = cid(psu.communityID)
df["communityID"] = cid(bl.communityID)
df = df.merge(psu[["communityID", "urban_nbs"]], on="communityID", how="left")
print("urban_nbs coverage: %.2f%%" % (100 * df.urban_nbs.notna().mean()))

# ---- parity blood variables + alt biomarker weight (sensitivity) ----
for v, name in [("bl_hdl", "hdl"), ("bl_ldl", "ldl"), ("bl_cho", "cho"),
                ("bl_glu", "glu"), ("bl_hbalc", "hbalc"), ("bl_crp", "crp"),
                ("bl_crea", "crea")]:
    df[name] = pd.to_numeric(bl[v], errors="coerce").values
w15, _ = pyreadstat.read_dta(BASE + r"\2015\Weights.dta")
w15 = w15.copy()
w15["ID_12"] = w15.ID.astype(str).str.strip()
df = df.merge(w15[["ID_12", "Biomarker_weight"]].rename(
    columns={"Biomarker_weight": "bw_alt"}), on="ID_12", how="left")

# ---- summary checks ----
print("\nrows:", len(df))
print("WTI complete (TG+WC valid):", df.WTI.notna().sum(),
      "| stroke (prevalent):", int(df.stroke_base.sum()),
      "| stroke_phys:", int(df.stroke_phys.sum()),
      "| stroke NA:", int(df.stroke_base_na.sum()))
print("htn:", int(df.htn.sum()), "| dyslipid:", int(df.dyslipid.sum()),
      "| dm:", int(df.dm.sum()))
print("lipid_rx:", int(df.lipid_rx.sum()), "| bp_rx:", int(df.bp_rx.sum()))
print("bloodweight non-missing:", int(df.bloodweight.notna().sum()),
      "| bmi non-missing:", int(df.bmi.notna().sum()))
print("TG top-coded (500):", int((df.TG_mgdl == 500).sum()))
print("age median:", df.age.median(), "| sex 1=male:", int((df.sex == 1).sum()),
      "| sex 2=female:", int((df.sex == 2).sum()))
print("TG_mmol median:", round(df.TG_mmol.median(), 3),
      "| WC_cm median:", round(df.WC_cm.median(), 1),
      "| WTI median:", round(df.WTI.median(), 2))

# cross-check Blood.dta weight vs Weights.dta Biomarker_weight
chk = df[["ID_12", "bloodweight", "bw_alt"]].dropna()
print("Blood_weight vs Biomarker_weight: corr %.6f | median ratio %.3f | "
      "max diff %.1f" %
      (chk.bloodweight.corr(chk.bw_alt),
       (chk.bloodweight / chk.bw_alt).median(),
       (chk.bloodweight - chk.bw_alt).abs().max()))

keep = ["ID_12", "WC_cm", "TG_mmol", "WTI", "stroke_base", "stroke_phys",
        "stroke_base_na", "htn", "dyslipid", "dm", "lipid_rx", "bp_rx",
        "smoke", "smoke_now", "drink", "pa_days_week", "age", "edu", "sex",
        "ht_cm", "wt_kg", "bmi", "bloodweight", "bw_alt", "communityID",
        "urban_nbs", "TG_mgdl", "hdl", "ldl", "cho", "glu", "hbalc", "crp",
        "crea", "cs_flag_raw", "panel_zda"]
df[keep].to_csv(r"D:\NHANES\data\charls_2015_cross_cov.csv", index=False)
print("saved: D:\\NHANES\\data\\charls_2015_cross_cov.csv")
print("DONE")
