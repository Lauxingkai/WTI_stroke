# ============================================================================
# 13n_sensitivities.py  (reviewer/statistician patch set D/E/F)
#  P1 wave heterogeneity: 2011 x 2015 WTI interaction (cluster GLM)
#  P2 log-WTI sensitivity: CHARLS 2015 M1-M3 (GLM) + NDI all-cause M1 (lifelines)
#  P3 MICE multiple imputation (sklearn IterativeImputer, m=20, Rubin) for
#     CHARLS 2015 M3 (complete-case ref OR 1.0966)
#  P4 Table S6: CHARLS 2015 baseline characteristics (weighted)
# Outputs: qc/13n_sensitivities.txt ; results/TableS6_2015_baseline.csv
# ============================================================================
import pandas as pd
import numpy as np
import statsmodels.api as sm
from lifelines import CoxPHFitter
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
from scipy import stats as sps

OUT = r"D:\NHANES\data\processed"
DATA = r"D:\NHANES\data"
QC = r"D:\NHANES\qc"
RES = r"D:\NHANES\results"
lines = []
def log(s=""):
    lines.append(str(s)); print(s)

# ============================================================================
# P1: wave heterogeneity (2011 vs 2015), WTI x wave interaction
# ============================================================================
c11 = pd.read_csv(OUT + r"\charls_2011_cross_cov.csv")
c15 = pd.read_csv(OUT + r"\charls_2015_cross_cov.csv")
def prep(d, wave):
    d = d.copy()
    d["WTI_sd"] = (d["WTI"] - d["WTI"].mean()) / d["WTI"].std()
    d["sex_m"] = (d["sex"] == 1).astype(float)
    d["age"] = pd.to_numeric(d["age"], errors="coerce")
    d["w_norm"] = d["bloodweight"] / d["bloodweight"].mean()
    d = d[(d["bloodweight"] > 0) & d["bmi"].notna() & d["age"].notna()].copy()
    d["wave"] = wave
    return d[["WTI_sd", "age", "sex_m", "stroke_base", "w_norm", "communityID", "wave"]].dropna()
p1 = pd.concat([prep(c11, 0), prep(c15, 1)], axis=0).reset_index(drop=True)
X = sm.add_constant(p1[["WTI_sd", "age", "sex_m", "wave"]])
X["WTI_x_wave"] = p1["WTI_sd"] * p1["wave"]
m = sm.GLM(p1["stroke_base"].astype(float), X, family=sm.families.Binomial(),
           freq_weights=p1["w_norm"]).fit(cov_type="cluster",
                                          cov_kwds={"groups": p1["communityID"]})
b = m.params["WTI_x_wave"]; se = m.bse["WTI_x_wave"]; p = m.pvalues["WTI_x_wave"]
log(f"P1 wave heterogeneity: n={len(p1)} | WTI x wave beta={b:.4f} se={se:.4f} "
    f"p={p:.4f} (2011 vs 2015; interaction NS => replication support)")

# ============================================================================
# P2: log-WTI sensitivity
# ============================================================================
ch = pd.read_csv(OUT + r"\charls_2015_cross_cov.csv")
ch["lWTI_sd"] = (np.log(ch["WTI"]) - np.log(ch["WTI"]).mean()) / np.log(ch["WTI"]).std()
ch["sex_m"] = (ch["sex"] == 1).astype(float)
ch["pa_ter"] = pd.cut(ch["pa_days_week"], [-1, 0, 1, 100], labels=["0d", "1-6d", "7d"])
ch["w_norm"] = ch["bloodweight"] / ch["bloodweight"].mean()
chc = ch[(ch["bloodweight"] > 0) & ch["bmi"].notna() & ch["age"].notna()].copy()
def gfit(data, covs):
    cc = data[covs + ["stroke_base", "w_norm", "communityID"]].copy()
    Xcols = []
    for c in covs:
        if c == "pa_ter":
            cc = cc[cc[c].notna()]
            d = pd.get_dummies(cc[c].astype(str), drop_first=True).astype(float)
            for dc in d.columns:
                cc[dc] = d[dc].values; Xcols.append(dc)
        else:
            cc[c] = pd.to_numeric(cc[c], errors="coerce"); Xcols.append(c)
    cc = cc.dropna(subset=Xcols + ["stroke_base"])
    X = sm.add_constant(cc[Xcols].astype(float))
    try:
        m = sm.GLM(cc["stroke_base"].astype(float), X, family=sm.families.Binomial(),
                   freq_weights=cc["w_norm"]).fit(cov_type="cluster",
                                                  cov_kwds={"groups": cc["communityID"]})
    except Exception:
        m = sm.GLM(cc["stroke_base"].astype(float), X, family=sm.families.Binomial(),
                   freq_weights=cc["w_norm"]).fit(cov_type="HC1")
    return m, len(cc)
for tag, covs in [("M1", ["lWTI_sd", "age", "sex_m"]),
                  ("M2", ["lWTI_sd", "age", "sex_m", "edu", "smoke", "drink", "bmi"]),
                  ("M3", ["lWTI_sd", "age", "sex_m", "edu", "smoke", "drink", "bmi",
                          "htn", "dm", "lipid_rx", "bp_rx", "pa_ter"])]:
    m, n = gfit(chc, covs)
    log(f"P2 log-WTI 2015 {tag}: OR={np.exp(m.params['lWTI_sd']):.3f} "
        f"CI=({np.exp(m.params['lWTI_sd']-1.96*m.bse['lWTI_sd']):.3f}-"
        f"{np.exp(m.params['lWTI_sd']+1.96*m.bse['lWTI_sd']):.3f}) "
        f"p={m.pvalues['lWTI_sd']:.3f} (n={n}) | linear CM{tag[-1] if tag!='M1' else '1'} ref 1.237/1.177/1.097")

nh = pd.read_csv(OUT + r"\nhanes_fasting_cross_cov.csv")
mort = pd.read_csv(DATA + r"\nhanes_mort2019.csv")
nh = nh.merge(mort[["seqn", "mortstat", "ucod_leading", "permth_int"]],
              left_on="SEQN", right_on="seqn", how="left")
nh["time_y"] = nh["permth_int"] / 12
nh["death"] = (nh["mortstat"] == 1).astype(float)
nh["wt"] = nh["WTSAF"] / 7
nh["lWTI_sd"] = (np.log(nh["WTI"]) - np.log(nh["WTI"]).mean()) / np.log(nh["WTI"]).std()
nhc = nh[(nh["permth_int"].notna()) & (nh["mortstat"].notna()) & (nh["WTI"].notna()) &
         (nh["RIDAGEYR"].notna()) & (nh["RIAGENDR"].notna()) & (nh["wt"] > 0)].dropna(
             subset=["time_y", "death", "lWTI_sd", "RIDAGEYR", "RIAGENDR"]).copy()
cph = CoxPHFitter()
cph.fit(nhc[["time_y", "death", "lWTI_sd", "RIDAGEYR", "RIAGENDR", "wt"]],
        duration_col="time_y", event_col="death", weights_col="wt", robust=True)
hr = np.exp(cph.params_["lWTI_sd"])
lo = np.exp(cph.confidence_intervals_.loc["lWTI_sd", "lWTI_sd" + "_lower_bound"]
            if "lWTI_sd_lower_bound" in cph.confidence_intervals_.columns
            else cph.confidence_intervals_.iloc[0, 0])
hi = np.exp(cph.confidence_intervals_.iloc[0, 1])
log(f"P2 log-WTI NDI all-cause M1: HR={hr:.3f} ({lo:.3f}-{hi:.3f}) "
    f"p={cph.summary.loc['lWTI_sd','p']:.3f} | linear ref 1.063")

# ============================================================================
# P3: MICE multiple imputation for CHARLS 2015 M3 (m=20, Rubin pooling)
# ============================================================================
imp_vars = ["WTI_sd", "age", "sex_m", "edu", "smoke", "drink", "bmi",
            "htn", "dm", "lipid_rx", "bp_rx", "pa_days_week", "stroke_base"]
chc["WTI_sd"] = (chc["WTI"] - chc["WTI"].mean()) / chc["WTI"].std()
chc["stroke_base"] = pd.to_numeric(chc["stroke_base"], errors="coerce")
chc["htn"] = pd.to_numeric(chc["htn"], errors="coerce")
chc["dm"] = pd.to_numeric(chc["dm"], errors="coerce")
chc["lipid_rx"] = pd.to_numeric(chc["lipid_rx"], errors="coerce")
chc["bp_rx"] = pd.to_numeric(chc["bp_rx"], errors="coerce")
chc["smoke"] = pd.to_numeric(chc["smoke"], errors="coerce")
chc["drink"] = pd.to_numeric(chc["drink"], errors="coerce")
chc["edu"] = pd.to_numeric(chc["edu"], errors="coerce")
imdf = chc[imp_vars].copy()
missing_before = imdf.isna().sum()
imputer = IterativeImputer(max_iter=25, random_state=42, sample_posterior=True,
                           min_value=0)
m_imp = 20
b_imp, se_imp = [], []
for i in range(m_imp):
    Xi = imputer.fit_transform(imdf)
    Xi = pd.DataFrame(Xi, columns=imp_vars)
    Xi["stroke_base"] = np.clip(np.round(Xi["stroke_base"]), 0, 1)
    pa = pd.cut(Xi["pa_days_week"], [-1, 0, 1, 100], labels=["0d", "1-6d", "7d"])
    d = pd.get_dummies(pa.astype(str), drop_first=True).astype(float)
    covs = ["WTI_sd", "age", "sex_m", "edu", "smoke", "drink", "bmi",
            "htn", "dm", "lipid_rx", "bp_rx"]
    X = sm.add_constant(pd.concat([Xi[covs].astype(float), d.reset_index(drop=True)],
                                  axis=1))
    mm = sm.GLM(Xi["stroke_base"], X, family=sm.families.Binomial(),
                freq_weights=chc["w_norm"].values).fit(cov_type="cluster",
                                                       cov_kwds={"groups": chc["communityID"].values})
    b_imp.append(mm.params["WTI_sd"]); se_imp.append(mm.bse["WTI_sd"])
b_bar = np.mean(b_imp); W = np.mean(np.square(se_imp)); B = np.var(b_imp, ddof=1)
T = W + (1 + 1 / m_imp) * B
nu = (m_imp - 1) * (1 + W / ((1 + 1 / m_imp) * B)) ** 2
tcrit = sps.t.ppf(0.975, nu)
lo = np.exp(b_bar - tcrit * np.sqrt(T)); hi = np.exp(b_bar + tcrit * np.sqrt(T))
pval = 2 * sps.t.sf(abs(b_bar / np.sqrt(T)), nu)
log(f"P3 MICE (m={m_imp}, Rubin): 2015 M3 OR={np.exp(b_bar):.3f} ({lo:.3f}-{hi:.3f}) "
    f"p={pval:.3f} | complete-case ref 1.097 (0.933-1.289) p=0.264")
log(f"   missing before imputation: {missing_before.to_dict()}")

# ============================================================================
# P4: Table S6 — CHARLS 2015 baseline (design-eligible sample, weighted)
# ============================================================================
def wquant(x, q, w):
    x = np.asarray(x, float); w = np.asarray(w, float)
    ok = ~(np.isnan(x) | np.isnan(w))
    x, w = x[ok], w[ok]
    idx = np.argsort(x); x, w = x[idx], w[idx]
    c = np.cumsum(w) / w.sum()
    return np.interp(q, c, x)
def wmean_sd(x, w):
    x = np.asarray(x, float); w = np.asarray(w, float)
    ok = ~(np.isnan(x) | np.isnan(w)); x, w = x[ok], w[ok]
    mu = np.average(x, weights=w)
    sd = np.sqrt(np.average((x - mu) ** 2, weights=w))
    return mu, sd
def wpct(flag, w):
    f = pd.to_numeric(flag, errors="coerce")
    ok = ~(f.isna() | w.isna())
    return 100 * np.average(f[ok].astype(float), weights=w[ok])

s6 = chc[chc["WTI"].notna()].copy()
s6["age"] = pd.to_numeric(s6["age"], errors="coerce")
w6 = s6["w_norm"].values
rows = {}
def block(tag, sub):
    w = sub["w_norm"]
    wv = w.values
    mu_a, sd_a = wmean_sd(sub["age"], wv)
    mu_b, sd_b = wmean_sd(sub["bmi"], wv)
    rows[tag] = {
        "n": len(sub),
        "age_mean_sd": f"{mu_a:.1f} ({sd_a:.1f})",
        "male_pct": f"{wpct(sub.sex_m, w):.1f}",
        "edu_median": f"{wquant(sub.edu, 0.5, wv):.0f}",
        "WTI_median_iqr": f"{wquant(sub.WTI, 0.5, wv):.1f} ({wquant(sub.WTI, 0.25, wv):.1f}-{wquant(sub.WTI, 0.75, wv):.1f})",
        "WC_median_iqr": f"{wquant(sub.WC_cm, 0.5, wv):.1f} ({wquant(sub.WC_cm, 0.25, wv):.1f}-{wquant(sub.WC_cm, 0.75, wv):.1f})",
        "TG_median_iqr": f"{wquant(sub.TG_mmol, 0.5, wv):.2f} ({wquant(sub.TG_mmol, 0.25, wv):.2f}-{wquant(sub.TG_mmol, 0.75, wv):.2f})",
        "BMI_mean_sd": f"{mu_b:.1f} ({sd_b:.1f})",
        "smoke_pct": f"{wpct(sub.smoke, w):.1f}",
        "drink_pct": f"{wpct(sub.drink, w):.1f}",
        "htn_pct": f"{wpct(sub.htn, w):.1f}",
        "dm_pct": f"{wpct(sub.dm, w):.1f}",
        "dyslipid_pct": f"{wpct(sub.dyslipid, w):.1f}",
        "lipid_rx_pct": f"{wpct(sub.lipid_rx, w):.1f}",
        "bp_rx_pct": f"{wpct(sub.bp_rx, w):.1f}",
    }
block("overall", s6)
block("stroke_yes", s6[s6.stroke_base == 1])
block("stroke_no", s6[s6.stroke_base == 0])
s6df = pd.DataFrame(rows).T
s6df.to_csv(RES + r"\TableS6_2015_baseline.csv")
log("P4 Table S6 (2015 baseline, weighted):")
log(s6df.to_string())

with open(QC + r"\13n_sensitivities.txt", "w", encoding="utf-8") as fh:
    fh.write("\n".join(lines))
print("saved:", QC + r"\13n_sensitivities.txt")
