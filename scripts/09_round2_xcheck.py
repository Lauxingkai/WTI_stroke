# ============================================================================
# 09_round2_xcheck.py  (Round 2, Iron Rule 2 extended: independent re-implementation)
# 1) CHARLS cross cm1 & prospective pm1: statsmodels weighted GLM + communityID
#    cluster-robust SE  vs  R survey (1.184 / 1.126)
# 2) CHARLS Cox M1: lifelines CoxPHFitter cluster vs R survival (1.106)
# 3) Discrimination: sklearn roc_auc_score on unweighted Logit predictions
#    vs pROC (NHANES WTI 0.689; CHARLS WTI 0.647)
# 4) E-values: independent formula for all 13 model estimates
# Output: qc/round2_xcheck.txt
# Date: 2026-08-16
# ============================================================================
import pandas as pd
import numpy as np
import statsmodels.api as sm
from lifelines import CoxPHFitter
from sklearn.metrics import roc_auc_score

OUT = r"D:\NHANES\data\processed"
QC = r"D:\NHANES\qc"
lines = []
def log(s=""):
    lines.append(str(s)); print(s)

# ---------- 1) CHARLS cross cm1 ----------
ch = pd.read_csv(OUT + r"\charls_2011_cross_cov.csv")
ch["WTI_sd"] = (ch["WTI"] - ch["WTI"].mean()) / ch["WTI"].std()
ch["sex_m"] = (ch["sex"] == 1).astype(float)
ch["age"] = pd.to_numeric(ch["age"], errors="coerce")
ch["w_norm"] = ch["bloodweight"] / ch["bloodweight"].mean()
chc = ch[(ch["bloodweight"] > 0) & ch["bmi"].notna() & ch["age"].notna()].copy()
X1 = sm.add_constant(chc[["WTI_sd", "age", "sex_m"]].astype(float))
m1 = sm.GLM(chc["stroke_base"].astype(float), X1, family=sm.families.Binomial(),
            freq_weights=chc["w_norm"]).fit(cov_type="cluster", cov_kwds={"groups": chc["communityID"]})
b = m1.params["WTI_sd"]; se = m1.bse["WTI_sd"]
log(f"CHARLS cm1: py OR={np.exp(b):.4f} CI=({np.exp(b-1.96*se):.4f}-{np.exp(b+1.96*se):.4f}) p={m1.pvalues['WTI_sd']:.4f} | R ref 1.184 (1.066-1.316) p=0.0016")

# ---------- 1b) CHARLS prospective pm1 ----------
pr = pd.read_csv(OUT + r"\charls_2011_2018_prosp_cov.csv", dtype={"ID_12": str})
pr["WTI_sd"] = (pr["WTI"] - pr["WTI"].mean()) / pr["WTI"].std()
pr["sex_m"] = (pr["sex"] == 1).astype(float)
pr["age"] = pd.to_numeric(pr["age"], errors="coerce")
pr["w_norm"] = pr["bloodweight"] / pr["bloodweight"].mean()
prc = pr[(pr["bloodweight"] > 0) & pr["bmi"].notna() & pr["age"].notna()].copy()
X2 = sm.add_constant(prc[["WTI_sd", "age", "sex_m"]].astype(float))
m2 = sm.GLM(prc["stroke_2018"].astype(float), X2, family=sm.families.Binomial(),
            freq_weights=prc["w_norm"]).fit(cov_type="cluster", cov_kwds={"groups": prc["communityID"]})
b = m2.params["WTI_sd"]; se = m2.bse["WTI_sd"]
log(f"CHARLS pm1: py OR={np.exp(b):.4f} CI=({np.exp(b-1.96*se):.4f}-{np.exp(b+1.96*se):.4f}) p={m2.pvalues['WTI_sd']:.4f} | R ref 1.126 (1.050-1.207) p=0.0009")

# ---------- 2) CHARLS Cox M1 (lifelines, cluster) ----------
ev = pd.read_csv(OUT + r"\charls_events_2011_2018.csv", dtype={"ID_12": str})
d = prc.merge(ev, on="ID_12", how="left")
d["ftime"] = np.minimum(pd.to_numeric(d["time"], errors="coerce"), 7.0)
d["event"] = d["stroke"].astype(float)
d = d[d["ftime"].notna() & d["event"].notna()].copy()
cph = CoxPHFitter()
cph.fit(d[["ftime", "event", "WTI_sd", "age", "sex_m", "communityID", "w_norm"]],
        duration_col="ftime", event_col="event", cluster_col="communityID",
        weights_col="w_norm")
hr = np.exp(cph.params_["WTI_sd"]); ci = np.exp(cph.confidence_intervals_.loc["WTI_sd"])
log(f"CHARLS Cox-M1: py HR={hr:.4f} CI=({ci[0]:.4f}-{ci[1]:.4f}) p={cph.summary.loc['WTI_sd','p']:.4f} | R ref 1.106 (1.039-1.178) p=0.0016")

# ---------- 3) Discrimination AUC (unweighted Logit, sklearn) ----------
nh = pd.read_csv(OUT + r"\nhanes_fasting_cross_cov.csv")
nh["WTI_sd"] = (nh["WTI"] - nh["WTI"].mean()) / nh["WTI"].std()
f0 = sm.Logit(nh["stroke"].astype(float), sm.add_constant(nh[["RIDAGEYR", "RIAGENDR"]].astype(float))).fit(disp=0)
p0 = f0.predict(sm.add_constant(nh[["RIDAGEYR", "RIAGENDR"]].astype(float)))
f1 = sm.Logit(nh["stroke"].astype(float), sm.add_constant(nh[["WTI", "RIDAGEYR", "RIAGENDR"]].astype(float))).fit(disp=0)
p1 = f1.predict(sm.add_constant(nh[["WTI", "RIDAGEYR", "RIAGENDR"]].astype(float)))
ok = p1.notna() & p0.notna() & nh["stroke"].notna()
auc1 = roc_auc_score(nh.loc[ok, "stroke"], p1[ok])
log(f"NHANES WTI AUC: py={auc1:.4f} | R ref 0.689 (0.667-0.710)")

ch2 = pd.read_csv(OUT + r"\charls_2011_cross_cov.csv")
ch2["age"] = pd.to_numeric(ch2["age"], errors="coerce"); ch2["sex"] = pd.to_numeric(ch2["sex"], errors="coerce")
ch2 = ch2[ch2[["age", "sex", "bmi", "WTI"]].notna().all(axis=1)].copy()
g0 = sm.Logit(ch2["stroke_base"].astype(float), sm.add_constant(ch2[["age", "sex"]].astype(float))).fit(disp=0)
q0 = g0.predict(sm.add_constant(ch2[["age", "sex"]].astype(float)))
g1 = sm.Logit(ch2["stroke_base"].astype(float), sm.add_constant(ch2[["WTI", "age", "sex"]].astype(float))).fit(disp=0)
q1 = g1.predict(sm.add_constant(ch2[["WTI", "age", "sex"]].astype(float)))
ok2 = q1.notna() & q0.notna() & ch2["stroke_base"].notna()
auc2 = roc_auc_score(ch2.loc[ok2, "stroke_base"], q1[ok2])
log(f"CHARLS WTI AUC: py={auc2:.4f} | R ref 0.647 (0.606-0.688)")

# ---------- 4) E-values (independent formula) ----------
def evalue(est, lo, hi):
    rr = 1/est if est < 1 else est
    rhi = 1.0 if hi < 1 else hi
    E = rr + np.sqrt(rr*(rr-1))
    Eci = rhi + np.sqrt(rhi*(rhi-1))
    return E, Eci
mdf = pd.read_csv(r"D:\NHANES\results\03_main_models.csv")
cdf = pd.read_csv(r"D:\NHANES\results\03c_cox_fg.csv")
rows = []
for _, r in mdf.iterrows():
    E, Eci = evalue(r.est, r.lo, r.hi)
    rows.append((f"{r.cohort}-{r.layer}-{r.model}", round(E, 2), round(Eci, 2)))
for _, r in cdf.iterrows():
    E, Eci = evalue(r.hr, r.lo, r.hi)
    rows.append((f"CHARLS-prosp-{r.model}", round(E, 2), round(Eci, 2)))
ref = {
    "NHANES-cross-M1": (1.36, 1.58), "NHANES-cross-M2": (1.05, 1.52), "NHANES-cross-M3": (1.34, 1.39),
    "CHARLS-cross-cm1": (1.65, 1.96), "CHARLS-cross-cm2": (1.53, 1.86), "CHARLS-cross-cm3": (1.34, 1.72),
    "CHARLS-prosp7y-pm1": (1.50, 1.71), "CHARLS-prosp7y-pm2": (1.37, 1.60), "CHARLS-prosp7y-pm3": (1.21, 1.50),
    "CHARLS-prosp-Cox-M1": (1.45, 1.64), "CHARLS-prosp-Cox-M3": (1.14, 1.43),
    "CHARLS-prosp-FG-M1": (1.47, 1.62), "CHARLS-prosp-FG-M3": (1.16, 1.39),
}
log("\nE-value check (py vs R-script output):")
allok = True
for tag, E, Eci in rows:
    rE, rEci = ref.get(tag, (None, None))
    ok = (abs(E - rE) < 0.02) and (abs(Eci - rEci) < 0.02)
    allok &= ok
    log(f"  {tag}: py {E}/{Eci} vs R {rE}/{rEci} -> {'OK' if ok else 'FAIL'}")
log("E-value ALL OK" if allok else "E-value MISMATCH FOUND")

open(QC + r"\round2_xcheck.txt", "w", encoding="utf-8").write("\n".join(lines))
print("\nDONE")
