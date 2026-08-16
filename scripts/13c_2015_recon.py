# ============================================================================
# 13c_2015_recon.py
# Opt-1 step C: empirical calibration before building the 2015 layer.
#  1) 2011 psu urban_nbs x 2015 bb001_w3_2 cross-tab (by communityID) +
#     coverage of 2015 blood participants by 2011 psu.
#  2) Medication checkbox semantics: da011s1/s2, da010_2_s1/s2 vs disease
#     flags, in both 2011 and 2015 (settle bp_rx / lipid_rx definition).
#  3) 2011 da051_3_ existence; 2015 qi002/ql002 units; bl_tg top-coding.
#  4) Extract 2015 questionnaire PDF text for DA007 disease-list order.
# Outputs: console log + data/tmp_2015_questionnaire.txt
# ============================================================================
import os
import pyreadstat
import pandas as pd
import numpy as np

BASE = r"D:\NHANES\CHARLS\CHARLS_1725074232_3\CHARLS"
OUT = r"D:\NHANES\data"
pd.set_option("display.width", 200)

# ---------- load ----------
dm15, _ = pyreadstat.read_dta(BASE + r"\2015\Demographic_Background.dta")
hs15, _ = pyreadstat.read_dta(BASE + r"\2015\Health_Status_and_Functioning.dta")
bm15, _ = pyreadstat.read_dta(BASE + r"\2015\Biomarker.dta")
bl15, _ = pyreadstat.read_dta(BASE + r"\2015\Blood.dta")
hs11, _ = pyreadstat.read_dta(BASE + r"\2011\health_status_and_functioning.dta")
psu11, _ = pyreadstat.read_dta(BASE + r"\2011\psu.dta")

# ---------- 1) strata calibration ----------
psu11 = psu11.copy()
psu11["cid"] = psu11.communityID.astype(str).str.strip()
dm15 = dm15.copy()
dm15["cid"] = dm15.communityID.astype(str).str.strip()
bl15 = bl15.copy()
bl15["cid"] = bl15.communityID.astype(str).str.strip()

m = dm15[["cid", "bb001_w3_2"]].merge(psu11[["cid", "urban_nbs"]], on="cid", how="left")
print("== 1) 2015 respondents with 2011 psu urban_nbs: %.1f%%" %
      (100 * m.urban_nbs.notna().mean()))
ct = pd.crosstab(m.bb001_w3_2, m.urban_nbs, dropna=False)
print(ct)
blc = bl15.merge(psu11[["cid", "urban_nbs"]], on="cid", how="left")
print("2015 blood participants n=%d; with 2011 urban_nbs: %.1f%%" %
      (len(blc), 100 * blc.urban_nbs.notna().mean()))
print("2015 blood participants with bb001_w3_2 available: %.1f%%" %
      (100 * bl15.merge(dm15[["ID", "bb001_w3_2"]], on="ID", how="left").bb001_w3_2.notna().mean()))

# ---------- 2) medication checkbox semantics ----------
def meds(health, prefix):
    out = {}
    for v in [prefix + "s1", prefix + "s2", prefix + "s3"]:
        out[v] = pd.to_numeric(health.get(v), errors="coerce") if v in health.columns else None
    return out

for tag, hs, dis in [("2011", hs11, "da007"), ("2015", hs15, "da007")]:
    print(f"\n== 2) {tag} medication checks ==")
    for pref, dv in [("da011", "da007_1_"), ("da010_2_", "da007_2_")]:
        m = meds(hs, pref)
        dd = pd.to_numeric(hs[dv], errors="coerce")
        dis_yes = dd == 1
        for k, v in m.items():
            if v is None:
                continue
            print(f"  {tag} {k}: among {dv}==1 (n={dis_yes.sum()}): "
                  f"==2: {(v[dis_yes]==2).sum()}, ==1: {(v[dis_yes]==1).sum()}, "
                  f"==3: {(v[dis_yes]==3).sum()}, NA: {v[dis_yes].isna().sum()}")
        if dv in hs.columns:
            both = {k: (v == 2) for k, v in m.items() if v is not None}
            if not both:
                print(f"  {tag} {pref}: no *_s columns found")
                continue
            any_western = pd.concat(list(both.values()), axis=1).any(axis=1)
            print(f"  {tag} {pref}: ANY western among disease==1: {any_western[dis_yes].sum()}; "
                  f"s2-only western: {(m[pref+'s2']==2)[dis_yes].sum() if pref+'s2' in m else 'NA'}")

# ---------- 3) units & top-coding ----------
print("\n== 3) units ==")
print("2011 has da051_3_:", "da051_3_" in hs11.columns)
print("2015 qi002 height: min=%.1f max=%.1f median=%.1f" %
      (bm15.qi002.min(), bm15.qi002.max(), bm15.qi002.median()))
print("2015 ql002 weight: min=%.1f max=%.1f median=%.1f" %
      (bm15.ql002.min(), bm15.ql002.max(), bm15.ql002.median()))
print("2015 qm002 waist: min=%.1f max=%.1f median=%.1f" %
      (bm15.qm002.min(), bm15.qm002.max(), bm15.qm002.median()))
print("2015 bl_tg: n=%d, min=%.1f max=%.1f | top-coded flag sum=%d" %
      (bl15.bl_tg.notna().sum(), bl15.bl_tg.min(), bl15.bl_tg.max(),
       (bl15.bl_top_coding_tg == 1).sum()))
print("2015 da051/da052 (act1): yes=%d; (act2): yes=%d; (act3): yes=%d" %
      ((hs15.da051_1_ == 1).sum(), (hs15.da051_2_ == 1).sum(), (hs15.da051_3_ == 1).sum()))

# ---------- 4) questionnaire text ----------
qpath = os.path.join(BASE, r"2015\CHARLS_2015_Questionnaire.pdf")
txt = None
try:
    from pypdf import PdfReader
    r = PdfReader(qpath)
    txt = "\n\n".join(f"=== PAGE {i+1} ===\n{(p.extract_text() or '')}" for i, p in enumerate(r.pages))
    print("\nquestionnaire pages:", len(r.pages))
except Exception as e:
    print("questionnaire extract failed:", e)
if txt:
    with open(os.path.join(OUT, "tmp_2015_questionnaire.txt"), "w", encoding="utf-8") as f:
        f.write(txt)
    print("saved questionnaire text")
print("DONE")
