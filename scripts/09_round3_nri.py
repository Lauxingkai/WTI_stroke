# ============================================================================
# 09_round3_nri.py  (Round 3, item 1: NRI/IDI independent reimplementation)
# Recomputes continuous NRI & IDI for the 7-object set in both cohorts using
# a Python-only path (statsmodels Logit + vectorized formulas), comparing
# against results/05d_boot_nri.csv (R glm implementation).
# Output: qc/round3_nri.txt
# Date: 2026-08-16
# ============================================================================
import pandas as pd
import numpy as np
import pyreadstat
import statsmodels.api as sm

RAW = r"D:\NHANES"; OUT = RAW + r"\data\processed"; NRAW = RAW + r"\data\raw"
lines = []
def log(s=""):
    lines.append(str(s)); print(s)

def nri_idi(p0, p1, y):
    d = np.asarray(p1) - np.asarray(p0)
    y = np.asarray(y).astype(float)
    up_e = (d[y == 1] > 0).mean(); dn_e = (d[y == 1] < 0).mean()
    up_n = (d[y == 0] > 0).mean(); dn_n = (d[y == 0] < 0).mean()
    nri = (up_e - dn_e) + (dn_n - up_n)
    idi = (p1[y == 1].mean() - p0[y == 1].mean()) - (p1[y == 0].mean() - p0[y == 0].mean())
    return nri, idi

def run(dat, yvar, covars, tag):
    objs = ["WTI", "WC", "TG", "TyG", "TyGWC", "ABSI", "HTGW"]
    X0 = sm.add_constant(dat[covars].astype(float))
    f0 = sm.Logit(dat[yvar].astype(float), X0).fit(disp=0)
    p0 = f0.predict(X0)
    ref = pd.read_csv(RAW + r"\results\05d_boot_nri.csv")
    for obj in objs:
        X1 = sm.add_constant(pd.concat([dat[[obj]], dat[covars]], axis=1).astype(float))
        f1 = sm.Logit(dat[yvar].astype(float), X1).fit(disp=0)
        p1 = f1.predict(X1)
        ok = np.isfinite(p1) & np.isfinite(p0) & dat[yvar].notna()
        if obj == "WTI":
            n, i = nri_idi(p0[ok], p1[ok], dat.loc[ok, yvar])
            rrow = ref[(ref.cohort == tag) & (ref.object == "WTI")].iloc[0]
        else:
            Xw = sm.add_constant(pd.concat([dat[["WTI"]], dat[covars]], axis=1).astype(float))
            fw = sm.Logit(dat[yvar].astype(float), Xw).fit(disp=0)
            pw = fw.predict(Xw)
            n, i = nri_idi(pw[ok], p1[ok], dat.loc[ok, yvar])
            rrow = ref[(ref.cohort == tag) & (ref.object == obj)].iloc[0]
        okn = abs(n - rrow.nri) < 0.005 and abs(i - rrow.idi) < 1e-5
        log(f"{tag} {obj}: py NRI={n:.4f} IDI={i:.6f} | R NRI={rrow.nri:.4f} IDI={rrow.idi:.6f} -> {'OK' if okn else 'FAIL'}")

# NHANES: rebuild 7 objects (independent Python path)
nh = pd.read_csv(OUT + r"\nhanes_fasting_cross_cov_v2.csv")
glu = pd.concat([pyreadstat.read_xport(NRAW + rf"\GLU_{cy}.XPT")[0][["SEQN", "LBXGLU"]].assign(CYCLE=cy)
                 for cy in list("DEFGHIJ")], ignore_index=True)
bmx = pd.concat([pyreadstat.read_xport(NRAW + rf"\BMX_{cy}.XPT")[0][["SEQN", "BMXHT"]].assign(CYCLE=cy)
                 for cy in list("DEFGHIJ")], ignore_index=True)
nh = nh.merge(glu, left_on=["SEQN", "CYCLE.x"], right_on=["SEQN", "CYCLE"], how="left")
nh = nh.merge(bmx, left_on=["SEQN", "CYCLE.x"], right_on=["SEQN", "CYCLE"], how="left")
nh["FPG_mmol"] = pd.to_numeric(nh.LBXGLU, errors="coerce") * 0.0555
nh["TyG"] = np.log(nh.TG_mmol * nh.FPG_mmol / 2)
nh["TyGWC"] = nh.TyG * nh.BMXWAIST
nh["WC"] = pd.to_numeric(nh.BMXWAIST, errors="coerce")
nh["TG"] = nh.TG_mmol
nh["ABSI"] = (nh.WC / 100) / (nh.bmi ** (2/3) * (nh.BMXHT / 100) ** 0.5)
nh["HTGW"] = np.where(((nh.RIAGENDR == 1) & (nh.WC >= 90)) | ((nh.RIAGENDR == 2) & (nh.WC >= 80)), nh.TG >= 1.69, False)
nh2 = nh[np.isfinite(nh.TyG) & np.isfinite(nh.ABSI)].copy()
run(nh2, "stroke", ["RIDAGEYR", "RIAGENDR"], "NHANES")

# CHARLS
ch = pd.read_csv(OUT + r"\charls_2011_cross_cov.csv")
ch["FPG_mmol"] = ch.newglu * 0.0555
ch["TyG"] = np.log(ch.TG_mmol * ch.FPG_mmol / 2)
ch["TyGWC"] = ch.TyG * ch.WC_cm
ch["WC"] = ch.WC_cm; ch["TG"] = ch.TG_mmol
ch["ABSI"] = (ch.WC_cm / 100) / (ch.bmi ** (2/3) * (ch.ht_cm / 100) ** 0.5)
ch["HTGW"] = np.where(((ch.sex == 1) & (ch.WC_cm >= 90)) | ((ch.sex == 2) & (ch.WC_cm >= 80)), ch.TG_mmol >= 1.69, False)
ch["age"] = pd.to_numeric(ch.age, errors="coerce"); ch["sex"] = pd.to_numeric(ch.sex, errors="coerce")
ch2 = ch[np.isfinite(ch.TyG) & np.isfinite(ch.ABSI) & np.isfinite(ch.bmi) & np.isfinite(ch.age) & ch.sex.isin([1, 2])].copy()
run(ch2, "stroke_base", ["age", "sex"], "CHARLS")

open(RAW + r"\qc\round3_nri.txt", "w", encoding="utf-8").write("\n".join(lines))
print("\nDONE")
