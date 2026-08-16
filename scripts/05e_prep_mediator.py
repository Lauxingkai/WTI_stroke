# ============================================================================
# 05e_prep_mediator.py
# Extract 2015 blood mediators (CRP/creatinine/cystatin C) with 12-digit ID
# for the CHARLS temporal mediation layer (2011 exposure -> 2015 M -> 2018 Y).
# Output: data/processed/charls_2015_mediator.csv
# ============================================================================
import pyreadstat
import pandas as pd

BASE = r"D:\NHANES\CHARLS\CHARLS_1725074232_3\CHARLS"
OUT = r"D:\NHANES\data\processed"

df, meta = pyreadstat.read_dta(
    BASE + r"\2015\Blood.dta",
    usecols=["ID", "bl_crp", "bl_crea", "bl_cysc", "bl_glu", "bl_fasting"])

m = pd.DataFrame({
    "ID_12": df["ID"].astype(str).str.strip(),
    "crp_2015": pd.to_numeric(df["bl_crp"], errors="coerce"),
    "crea_2015": pd.to_numeric(df["bl_crea"], errors="coerce"),
    "cysc_2015": pd.to_numeric(df["bl_cysc"], errors="coerce"),
    "glu_2015": pd.to_numeric(df["bl_glu"], errors="coerce"),
    "fasting_2015": pd.to_numeric(df["bl_fasting"], errors="coerce"),
})
assert m["ID_12"].str.len().eq(12).all(), "ID length check failed"
print("rows:", len(m), "| ID len ok")
print("missing: CRP", m.crp_2015.isna().mean().round(4),
      "| crea", m.crea_2015.isna().mean().round(4),
      "| cysc", m.cysc_2015.isna().mean().round(4))
m.to_csv(OUT + r"\charls_2015_mediator.csv", index=False)
print("DONE")
