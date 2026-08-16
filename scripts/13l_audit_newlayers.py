# ============================================================================
# 13l_audit_newlayers.py  (数字终审·新层独立复算)
# Methodology mirrors 09_round2_xcheck.py (six-path cross-validation):
#  P1 statsmodels weighted GLM (cluster-robust) for CHARLS 2015 CM1/CM2/CM3
#     + tertiles + alt-weight + sex/age strata vs R survey refs (13_2015 CSV)
#  P2 hand-written IRLS (numpy) for CM1 point estimate
#  P3 lifelines CoxPHFitter (weighted + unweighted) for NDI AM1/AM3/SM1/SM3
#     vs R svycoxph refs (13g CSV)
#  P4 data-level independent recount of the 2015 dta build chain
#  P5 pandas read_fwf recount of the 8 NDI .dat files (independent of readr)
#  P6 MDE hand-check (rerun 13h formulas inline)
# Output: qc/13l_newlayers_audit.txt
# Date: 2026-08-16
# ============================================================================
import pandas as pd
import numpy as np
import statsmodels.api as sm
from lifelines import CoxPHFitter
import os

OUT = r"D:\NHANES\data\processed"
DATA = r"D:\NHANES\data"
QC = r"D:\NHANES\qc"
RES = r"D:\NHANES\results"
lines = []
def log(s=""):
    lines.append(str(s)); print(s)

# ---------- reference values ----------
ref15 = pd.read_csv(RES + r"\13_2015_main_models.csv")
refn = pd.read_csv(RES + r"\13g_ndi_cox_models.csv")
def rref(src, layer, model):
    r = ref15 if src == "r15" else refn
    return r[(r.layer == layer) & (r.model == model)].iloc[0]

# ============================================================================
# P1: CHARLS 2015 weighted GLM cross-check
# ============================================================================
ch = pd.read_csv(OUT + r"\charls_2015_cross_cov.csv")
ch["WTI_sd"] = (ch["WTI"] - ch["WTI"].mean()) / ch["WTI"].std()
ch["sex_m"] = (ch["sex"] == 1).astype(float)
ch["WTI_ter"] = pd.cut(ch["WTI"], np.nanquantile(ch["WTI"], [0, 1/3, 2/3, 1]),
                       include_lowest=True, labels=["T1", "T2", "T3"])
ch["pa_ter"] = pd.cut(ch["pa_days_week"], [-1, 0, 1, 100],
                      labels=["0d", "1-6d", "7d"])
ch["w_norm"] = ch["bloodweight"] / ch["bloodweight"].mean()
chc = ch[(ch["bloodweight"] > 0) & ch["bmi"].notna() & ch["age"].notna()].copy()
for c in ["edu", "smoke", "drink", "bmi", "htn", "dm", "lipid_rx", "bp_rx"]:
    chc[c] = pd.to_numeric(chc[c], errors="coerce")
log(f"P1 design-filtered n={len(chc)} (R ref 12501) | events={int(chc.stroke_base.sum())}")

def glm_cross(covs, data, outcome="stroke_base", weights="w_norm"):
    cc = data[covs + [outcome, weights, "communityID"]].copy()
    Xcols = []
    for c in covs:
        if c in ("pa_ter", "WTI_ter"):
            cc = cc[cc[c].notna()]
            d = pd.get_dummies(cc[c].astype(str), drop_first=True).astype(float)
            for dc in d.columns:
                cc[dc] = d[dc].values
                Xcols.append(dc)
        else:
            cc[c] = pd.to_numeric(cc[c], errors="coerce")
            Xcols.append(c)
    cc[outcome] = pd.to_numeric(cc[outcome], errors="coerce")
    cc = cc.dropna(subset=Xcols + [outcome])
    X = sm.add_constant(cc[Xcols].astype(float))
    try:
        m = sm.GLM(cc[outcome].astype(float), X, family=sm.families.Binomial(),
                   freq_weights=cc[weights]).fit(cov_type="cluster",
                                                cov_kwds={"groups": cc["communityID"]})
    except Exception:
        m = sm.GLM(cc[outcome].astype(float), X, family=sm.families.Binomial(),
                   freq_weights=cc[weights]).fit(cov_type="HC1")
    return m, len(cc)

covs_m1 = ["WTI_sd", "age", "sex_m"]
covs_m2 = covs_m1 + ["edu", "smoke", "drink", "bmi"]
covs_m3 = covs_m2 + ["htn", "dm", "lipid_rx", "bp_rx", "pa_ter"]
covs_m3t = ["WTI_ter", "age", "sex_m", "edu", "smoke", "drink", "bmi",
            "htn", "dm", "lipid_rx", "bp_rx", "pa_ter"]

for tag, covs, rmodel in [("CM1", covs_m1, "CM1"), ("CM2", covs_m2, "CM2"),
                          ("CM3", covs_m3, "CM3")]:
    m, n = glm_cross(covs, chc)
    b = m.params["WTI_sd"]; se = m.bse["WTI_sd"]; p = m.pvalues["WTI_sd"]
    rr = rref("r15", "cross", rmodel)
    or_r = float(rr.est); p_r = float(rr.p)
    log(f"P1 {tag}: py OR={np.exp(b):.6f} p(cluster)={p:.4f} n={n} | R OR={or_r:.6f} p={p_r:.6f} | "
        f"dOR={abs(np.exp(b)-or_r):.2e} {'OK' if abs(np.exp(b)-or_r) < 1e-4 else 'FAIL'}")

# tertile CM3t
mt, nt = glm_cross(covs_m3t, chc)
log(f"P1 CM3t: py T2={np.exp(mt.params['T2']):.3f} p={mt.pvalues['T2']:.3f} | "
    f"T3={np.exp(mt.params['T3']):.3f} p={mt.pvalues['T3']:.3f} "
    f"| R ref 1.278/0.263, 1.393/0.121 (n={nt})")

# alt-weight CA1
ch["w_alt"] = ch["bw_alt"] / ch["bw_alt"].mean()
cha = ch[(ch["bw_alt"] > 0) & ch["bmi"].notna() & ch["age"].notna()].copy()
ma, na = glm_cross(covs_m1, cha, weights="w_alt")
rr = rref("r15", "cross-altw", "CA1")
log(f"P1 CA1: py OR={np.exp(ma.params['WTI_sd']):.6f} | R OR={float(rr.est):.6f} | "
    f"dOR={abs(np.exp(ma.params['WTI_sd'])-float(rr.est)):.2e}")

# physician-confirmed CP1
mp, np_ = glm_cross(covs_m1, chc, outcome="stroke_phys")
rr = rref("r15", "cross-phys", "CP1")
log(f"P1 CP1: py OR={np.exp(mp.params['WTI_sd']):.6f} (events={int(chc.dropna(subset=['stroke_phys']).stroke_phys.sum())}) | "
    f"R OR={float(rr.est):.6f} | dOR={abs(np.exp(mp.params['WTI_sd'])-float(rr.est)):.2e}")

# sex strata M1
for sx, lbl, rlayer in [(1, "men", "cross-men"), (0, "women", "cross-women")]:
    sub = chc[chc["sex_m"] == sx]
    ms, ns = glm_cross(["WTI_sd", "age"], sub)
    rr = rref("r15", rlayer, "M1")
    log(f"P1 {lbl}: py OR={np.exp(ms.params['WTI_sd']):.6f} (n={ns}, ev={int(sub.stroke_base.sum())}) | "
        f"R OR={float(rr.est):.6f} | dOR={abs(np.exp(ms.params['WTI_sd'])-float(rr.est)):.2e}")

# age strata M1
for lo, hi, rlayer in [(45, 59, "cross-age-45to59"), (60, 200, "cross-age-60plus")]:
    sub = chc[(chc["age"] >= lo) & (chc["age"] <= hi)]
    ms, ns = glm_cross(["WTI_sd", "sex_m"], sub)
    rr = rref("r15", rlayer, "M1")
    log(f"P1 age{lo}-{hi}: py OR={np.exp(ms.params['WTI_sd']):.6f} (n={ns}, ev={int(sub.stroke_base.sum())}) | "
        f"R OR={float(rr.est):.6f} | dOR={abs(np.exp(ms.params['WTI_sd'])-float(rr.est)):.2e}")

# ============================================================================
# P2: hand-written IRLS for CM1 (weighted binomial Newton-Raphson)
# ============================================================================
cc1 = chc[covs_m1 + ["stroke_base", "w_norm"]].dropna().astype(float)
X = np.column_stack([np.ones(len(cc1)), cc1["WTI_sd"], cc1["age"], cc1["sex_m"]])
y = cc1["stroke_base"].values; w = cc1["w_norm"].values
beta = np.zeros(X.shape[1])
for _ in range(50):
    eta = X @ beta
    mu = 1 / (1 + np.exp(-eta))
    g = X.T @ (w * (y - mu))
    H = X.T @ ((w * mu * (1 - mu))[:, None] * X)
    beta_new = beta + np.linalg.solve(H, g)
    if np.max(np.abs(beta_new - beta)) < 1e-12:
        beta = beta_new; break
    beta = beta_new
log(f"P2 IRLS CM1: OR={np.exp(beta[1]):.6f} | R ref={float(rref('r15','cross','CM1').est):.6f} | "
    f"dOR={abs(np.exp(beta[1])-float(rref('r15','cross','CM1').est)):.2e}")

# ============================================================================
# P3: NDI Cox cross-check (lifelines)
# ============================================================================
nh = pd.read_csv(OUT + r"\nhanes_fasting_cross_cov.csv")
mort = pd.read_csv(DATA + r"\nhanes_mort2019.csv")
nh = nh.merge(mort[["seqn", "eligstat", "mortstat", "ucod_leading", "permth_int"]],
              left_on="SEQN", right_on="seqn", how="left")
nh["wt"] = nh["WTSAF"] / 7
nh["WTI_sd"] = (nh["WTI"] - nh["WTI"].mean()) / nh["WTI"].std()
nh["WTI_ter"] = pd.cut(nh["WTI"], np.nanquantile(nh["WTI"], [0, 1/3, 2/3, 1]),
                       include_lowest=True, labels=["T1", "T2", "T3"])
nh["pa_ter"] = pd.cut(nh["pa_min_day"],
                      np.nanquantile(nh["pa_min_day"].dropna(), [0, 1/3, 2/3, 1]),
                      include_lowest=True, labels=["L", "M", "H"])
nh["time_y"] = nh["permth_int"] / 12
nh["death"] = (nh["mortstat"] == 1).astype(float)
nh["death_stroke"] = ((nh["mortstat"] == 1) & (nh["ucod_leading"] == 5)).astype(float)
m1v = ["time_y", "death", "WTI_sd", "RIDAGEYR", "RIAGENDR", "wt"]
m3v = m1v + ["RIDRETH1", "edu", "smoke", "drink", "bmi", "htn", "dm", "statin", "bp_rx", "pa_ter"]
s1v = ["time_y", "death_stroke", "WTI_sd", "RIDAGEYR", "RIAGENDR", "wt"]
s3v = s1v + ["RIDRETH1", "edu", "smoke", "drink", "bmi", "htn", "dm", "statin", "bp_rx", "pa_ter"]
nhc = nh[(nh["permth_int"].notna()) & (nh["mortstat"].notna()) & (nh["WTI"].notna()) &
         (nh["RIDAGEYR"].notna()) & (nh["RIAGENDR"].notna()) &
         (nh["death"].notna()) & (nh["wt"] > 0)].copy()
log(f"P3 analytic n={len(nhc)} (R ref 10289) | deaths={int(nhc.death.sum())} "
    f"| stroke deaths={int(nhc.death_stroke.sum())} | med fu={nhc.time_y.median():.2f}y "
    f"| py={nhc.time_y.sum():.0f}")

def lifelines_cox(vars_, outcome, label, rlayer, rmodel, weighted=True):
    d = nhc[vars_].dropna().copy()
    for c in list(d.columns):
        if d[c].dtype == object or isinstance(d[c].dtype, pd.CategoricalDtype):
            dum = pd.get_dummies(d[c].astype(str), drop_first=True).astype(float)
            d = pd.concat([d.drop(columns=[c]), dum], axis=1)
    d = d.drop(columns=["wt"] if not weighted else [])
    cph = CoxPHFitter()
    kw = dict(duration_col="time_y", event_col=outcome,
              weights_col="wt" if weighted else None)
    cph.fit(d, **kw)
    hr = np.exp(cph.params_["WTI_sd"])
    rr = rref("rn", rlayer, rmodel)
    log(f"P3 {label}: py HR={hr:.6f} (n={len(d)}, ev={int(d[outcome].sum())}) | "
        f"R HR={float(rr.est):.6f} | dHR={abs(hr-float(rr.est)):.2e} | "
        f"R CI ({float(rr.lo):.3f}-{float(rr.hi):.3f}) p={float(rr.p):.4f}")

lifelines_cox(m1v, "death", "all-cause M1 weighted", "all-cause", "AM1")
lifelines_cox(m3v, "death", "all-cause M3 weighted", "all-cause", "AM3")
lifelines_cox(s1v, "death_stroke", "stroke-death M1 weighted", "stroke-death", "SM1")
lifelines_cox(s3v, "death_stroke", "stroke-death M3 weighted", "stroke-death", "SM3")
# unweighted variant (expected small deviation from svycoxph point estimate)
lifelines_cox(m1v, "death", "all-cause M1 UNweighted", "all-cause", "AM1")
lifelines_cox(s3v, "death_stroke", "stroke-death M3 UNweighted", "stroke-death", "SM3")

# ============================================================================
# P4: data-level recount of the 2015 build chain (independent of 13d)
# ============================================================================
import pyreadstat
BASE = r"D:\NHANES\CHARLS\CHARLS_1725074232_3\CHARLS"
bl, _ = pyreadstat.read_dta(BASE + r"\2015\Blood.dta", usecols=["ID", "bl_tg", "Blood_weight"])
bm, _ = pyreadstat.read_dta(BASE + r"\2015\Biomarker.dta", usecols=["ID", "qm002"])
hs, _ = pyreadstat.read_dta(BASE + r"\2015\Health_Status_and_Functioning.dta",
                            usecols=["ID", "zda007_8_", "da007_8_"])
d15 = pd.DataFrame({"ID": bl.ID.astype(str).str.strip(),
                    "tg": pd.to_numeric(bl.bl_tg, errors="coerce"),
                    "bw": pd.to_numeric(bl.Blood_weight, errors="coerce")})
bm2 = pd.DataFrame({"ID": bm.ID.astype(str).str.strip(),
                    "wc": pd.to_numeric(bm.qm002, errors="coerce")})
hs2 = pd.DataFrame({"ID": hs.ID.astype(str).str.strip(),
                    "z8": pd.to_numeric(hs.zda007_8_, errors="coerce"),
                    "d8": pd.to_numeric(hs.da007_8_, errors="coerce")})
m = d15.merge(bm2, on="ID", how="left").merge(hs2, on="ID", how="left")
m["wc_ok"] = m.wc.between(40, 200)
m["wti_ok"] = m.tg.notna() & m.wc_ok
m["stroke"] = ((m.z8 == 1) | (m.d8 == 1)).fillna(False)
log(f"P4 recount: blood={len(m)} | WTI complete={int(m.wti_ok.sum())} (R 12899) | "
    f"prevalent stroke={int(m.stroke.sum())} (R 289) | "
    f"stroke among WTI-complete={int((m.stroke & m.wti_ok).sum())}")
csv = pd.read_csv(OUT + r"\charls_2015_cross_cov.csv")
log(f"P4 built CSV: rows={len(csv)} | WTI complete={int(csv.WTI.notna().sum())} | "
    f"stroke={int(csv.stroke_base.sum())} | design-eligible={int(((csv.bloodweight>0)&csv.bmi.notna()&csv.age.notna()).sum())} "
    f"(R 12501)")

# ============================================================================
# P5: pandas read_fwf recount of the 8 .dat files (independent of R readr)
# ============================================================================
import glob
cols = [(0, 6), (14, 15), (15, 16), (16, 19), (19, 20), (20, 21), (42, 45), (45, 48)]
names = ["seqn", "eligstat", "mortstat", "ucod_leading", "diabetes", "hyperten", "permth_int", "permth_exm"]
tot = 0; dec = 0; ucod5 = 0; elig = 0; pm_na = 0
for f in sorted(glob.glob(DATA + r"\mortality\NHANES_*_MORT_2019_PUBLIC.dat")):
    df = pd.read_fwf(f, colspecs=cols, names=names,
                     converters={n: (lambda s: np.nan if s.strip() in ("", ".") else int(s)) for n in names})
    tot += len(df); dec += int((df.mortstat == 1).sum()); ucod5 += int((df.ucod_leading == 5).sum())
    elig += int((df.eligstat == 1).sum()); pm_na += int(df.permth_int.isna().sum())
log(f"P5 recount (pandas): total={tot} | deceased={dec} | ucod5={ucod5} | eligible={elig} | "
    f"permth NA={pm_na} | R refs: 80312 / 5950 / 315 / 47632")

# cohort-level: deaths among the analytic cohort
coh = nhc[["SEQN", "death", "death_stroke"]].copy()
cohm = mort[["seqn", "eligstat", "mortstat", "ucod_leading", "permth_int"]].copy()
coh_ck = pd.DataFrame({"SEQN": nh["SEQN"]}).merge(cohm, left_on="SEQN", right_on="seqn", how="left")
el = coh_ck.eligstat == 1
log(f"P5 cohort recount: linked={coh_ck.seqn.notna().mean():.3%} | eligible={int(el.sum())} "
    f"| deaths={int((coh_ck.mortstat==1).sum())} | stroke deaths={int(((coh_ck.mortstat==1)&(coh_ck.ucod_leading==5)).sum())} "
    f"| R refs: 100% / 10289 / 1428 / 83")

# ============================================================================
# P6: MDE hand-check (Hsieh logistic / Freedman Cox)
# ============================================================================
def mde_logistic(n, events, r2):
    p = events / n; za, zb = 1.959963985, 0.841621234
    return np.exp(np.sqrt((za + zb) ** 2 / (n * p * (1 - p) * (1 - r2))))
def mde_cox(events, r2):
    za, zb = 1.959963985, 0.841621234
    return np.exp(np.sqrt((za + zb) ** 2 / (events * (1 - r2))))
r2_15 = 0.161; r2_ndi = 0.090
log(f"P6 MDE: CHARLS2015 M3 = {mde_logistic(11203, 252, r2_15):.3f} (R 1.213-1.215) | "
    f"NDI stroke-death = {mde_cox(83, r2_ndi):.3f} (R 1.380) | "
    f"NDI all-cause = {mde_cox(1428, r2_ndi):.3f} (R 1.081)")

with open(QC + r"\13l_newlayers_audit.txt", "w", encoding="utf-8") as fh:
    fh.write("\n".join(lines))
print("saved:", QC + r"\13l_newlayers_audit.txt")
