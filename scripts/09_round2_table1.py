# ============================================================================
# 09_round2_table1.py  (Round 2, priority 4: Table 1 full-cell independent
# recomputation in numpy — a third implementation path distinct from 05f's R.)
# Weighted quantile: type-7 linear interpolation on cumulated weights.
# Output: qc/round2_table1.txt
# Date: 2026-08-16
# ============================================================================
import pandas as pd
import numpy as np
import pyreadstat

RAW = r"D:\NHANES"
OUT = RAW + r"\data\processed"
NRAW = RAW + r"\data\raw"
lines = []
def log(s=""):
    lines.append(str(s)); print(s)

def wq(x, w, p):
    o = np.argsort(x); x = x[o]; w = w[o]
    cw = np.cumsum(w) / w.sum()
    out = []
    for pp in p:
        i = np.searchsorted(cw, pp)
        if i == 0: out.append(x[0])
        elif i >= len(x): out.append(x[-1])
        else:
            lam = (pp - cw[i-1]) / (cw[i] - cw[i-1])
            out.append(x[i-1] + lam * (x[i] - x[i-1]))
    return np.array(out)

nchk = 0; nfail = 0
def cell(tag, got, want):
    global nchk, nfail
    nchk += 1
    if got != want:
        nfail += 1
        log(f"FAIL {tag}: got {got!r} want {want!r}")

# ---------------- NHANES ----------------
nh = pd.read_csv(OUT + r"\nhanes_fasting_cross_cov_v2.csv")
frames = []
for cy in list("DEFGHIJ"):
    df, _ = pyreadstat.read_xport(NRAW + rf"\DEMO_{cy}.XPT", usecols=["SEQN", "SDMVSTRA", "SDMVPSU"])
    df["CYCLE"] = cy
    frames.append(df)
des = pd.concat(frames, ignore_index=True)
nh = nh.merge(des, left_on=["SEQN", "CYCLE.x"], right_on=["SEQN", "CYCLE"], how="left")
nh["wt"] = nh["WTSAF"] / 7
nh["stroke_f"] = np.where(nh["stroke"], "Stroke", "No stroke")

t1o = pd.read_csv(RAW + r"\results\05f_table1_nhanes_outcome.csv")
labels_nh = {"RIDAGEYR": "Age, years", "bmi": "BMI, kg/m2", "WTI": "WTI, cm-mmol/L",
             "BMXWAIST": "Waist circumference, cm", "TG_mmol": "Triglycerides, mmol/L"}
for v, lab in labels_nh.items():
    for grp in ["No stroke", "Stroke"]:
        s = nh[nh.stroke_f == grp]
        q = wq(s[v].values, s["wt"].values, [0.25, 0.5, 0.75])
        got = f"{q[1]:.1f} ({q[0]:.1f}, {q[2]:.1f})"
        want = t1o[(t1o.variable == lab) & (t1o.group == grp)].est.iloc[0]
        cell(f"NH {lab} {grp}", got, want)

# CHARLS
ch = pd.read_csv(OUT + r"\charls_2011_cross_cov.csv")
ch["w_norm"] = ch["bloodweight"] / ch["bloodweight"].mean()
ch["stroke_f"] = np.where(ch["stroke_base"], "Stroke", "No stroke")
ch["age"] = pd.to_numeric(ch["age"], errors="coerce")
ch = ch[(ch["bloodweight"] > 0) & ch["bmi"].notna() & ch["age"].notna()]
c1o = pd.read_csv(RAW + r"\results\05f_table1_charls_outcome.csv")
labels_ch = {"age": "Age, years", "bmi": "BMI, kg/m2", "WTI": "WTI, cm-mmol/L",
             "WC_cm": "Waist circumference, cm", "TG_mmol": "Triglycerides, mmol/L"}
for v, lab in labels_ch.items():
    for grp in ["No stroke", "Stroke"]:
        s = ch[ch.stroke_f == grp]
        q = wq(s[v].values, s["w_norm"].values, [0.25, 0.5, 0.75])
        got = f"{q[1]:.1f} ({q[0]:.1f}, {q[2]:.1f})"
        want = c1o[(c1o.variable == lab) & (c1o.group == grp)].est.iloc[0]
        cell(f"CH {lab} {grp}", got, want)

log(f"\nchecked={nchk} failed={nfail}")
open(RAW + r"\qc\round2_table1.txt", "w", encoding="utf-8").write("\n".join(lines))
print("\nDONE")
