# ============================================================================
# 08_audit_missing.py  (Phase 1: T4 missing-data & exclusion-chain audit, CHARLS)
# Independent recomputation from raw .dta files + charls_covariates.csv.
# Output: qc/phase1_missing_chain.txt
# Date: 2026-08-16
# ============================================================================
import pyreadstat
import pandas as pd
import numpy as np

BASE = r"D:\NHANES\CHARLS\CHARLS_1725074232_3\CHARLS"
OUT = r"D:\NHANES\qc"
lines = []

def id12_2011(s):
    s = s.astype(str).str.strip()
    assert (s.str.len() == 11).all()
    return s.str[:9] + "0" + s.str[9:11]

blood11, _ = pyreadstat.read_dta(BASE + r"\2011\Blood_20140429.dta", usecols=["ID", "newtg"])
hs11, _ = pyreadstat.read_dta(BASE + r"\2011\health_status_and_functioning.dta",
                              usecols=["ID", "da007_8_"])
bm11, _ = pyreadstat.read_dta(BASE + r"\2011\biomarkers.dta", usecols=["ID", "qm002"])
blood11["ID_12"] = id12_2011(blood11.ID)
hs11["ID_12"] = id12_2011(hs11.ID)
bm11["ID_12"] = id12_2011(bm11.ID)

n_blood = len(blood11)
tg_miss = blood11.newtg.isna().mean()
lines.append(f"2011 blood sample n={n_blood}; TG(newtg) missing: {blood11.newtg.isna().sum()} ({tg_miss:.3%})")

# cohort chain 11,847 -> 9,870
m = blood11.merge(hs11[["ID_12", "da007_8_"]], on="ID_12", how="left") \
           .merge(bm11[["ID_12", "qm002"]], on="ID_12", how="left")
m["wc_num"] = pd.to_numeric(m.qm002, errors="coerce")
m["tg_num"] = pd.to_numeric(m.newtg, errors="coerce")
complete = m.wc_num.notna() & m.tg_num.notna()
only_tg_miss = m.wc_num.notna() & m.tg_num.isna()
only_wc_miss = m.wc_num.isna() & m.tg_num.notna()
both_miss = m.wc_num.isna() & m.tg_num.isna()
lines.append(f"complete TG+WC (analytic 9,870 expected): {complete.sum()}")
lines.append(f"excluded breakdown: TG-only missing {only_tg_miss.sum()}, "
             f"WC-only missing {only_wc_miss.sum()}, both missing {both_miss.sum()} "
             f"(total {m.shape[0]-complete.sum()})")
stk_base = pd.to_numeric(m.da007_8_, errors="coerce")
lines.append(f"baseline stroke (da007_8_==1) among complete: {(complete & (stk_base == 1)).sum()} (expect 220); "
             f"da007_8_ NA: {stk_base.isna().sum()}")
lines.append(f"prospective (complete minus baseline stroke): {complete.sum() - (complete & (stk_base == 1)).sum()} (expect 9650)")

# covariate missingness within the 9,870 cross-sectional cohort (from 00 output)
cov = pd.read_csv(r"D:\NHANES\data\processed\charls_covariates.csv", dtype={"ID_12": str})
cross_ids = set(m.loc[complete, "ID_12"])
c = cov[cov.ID_12.isin(cross_ids)].copy()
covars = ["age", "sex", "edu", "smoke", "drink", "bmi", "htn", "dm", "lipid_rx", "bp_rx", "pa_days_week", "bloodweight", "urban_nbs"]
lines.append(f"\ncovariate missingness within 9,870 cross-sectional cohort (n={len(c)}):")
miss_rows = pd.Series(True, index=c.index)
for v in covars:
    nm = c[v].isna().sum()
    lines.append(f"  {v}: missing {nm} ({nm/len(c):.3%})")
    if v != "urban_nbs":
        miss_rows &= c[v].isna()
m3_needed = ["age", "sex", "edu", "smoke", "drink", "bmi", "htn", "dm", "lipid_rx", "bp_rx", "pa_days_week", "bloodweight"]
complete_m3 = c[m3_needed].notna().all(axis=1)
lines.append(f"complete for M3 covariate set: {complete_m3.sum()} -> excluded {len(c)-complete_m3.sum()} (expect 9214, excluded 656)")
# which covariate drives exclusion
for v in m3_needed:
    others = [x for x in m3_needed if x != v]
    base_ok = c[others].notna().all(axis=1)
    adds = base_ok & c[v].isna()
    lines.append(f"    additionally excluded by {v}: {adds.sum()}")

with open(OUT + r"\phase1_missing_chain.txt", "w", encoding="utf-8") as f:
    f.write("\n".join(lines))
print("\n".join(lines))
print("\nDONE")
