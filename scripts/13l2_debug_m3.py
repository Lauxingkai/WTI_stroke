import pandas as pd, numpy as np
ch = pd.read_csv(r"D:\NHANES\data\processed\charls_2015_cross_cov.csv")
ch["WTI_sd"] = (ch["WTI"] - ch["WTI"].mean()) / ch["WTI"].std()
ch["sex_m"] = (ch["sex"] == 1).astype(float)
ch["pa_ter"] = pd.cut(ch["pa_days_week"], [-1, 0, 1, 100], labels=["0d", "1-6d", "7d"])
ch["w_norm"] = ch["bloodweight"] / ch["bloodweight"].mean()
chc = ch[(ch["bloodweight"] > 0) & ch["bmi"].notna() & ch["age"].notna()].copy()
covs = ["WTI_sd", "age", "sex_m", "edu", "smoke", "drink", "bmi",
        "htn", "dm", "lipid_rx", "bp_rx", "pa_ter"]
cc = chc[covs + ["stroke_base", "w_norm"]].copy()
Xcols = []
for c in covs:
    if c == "pa_ter":
        d = pd.get_dummies(cc[c].astype(str), drop_first=True).astype(float)
        for dc in d.columns:
            cc[dc] = d[dc].values; Xcols.append(dc)
    else:
        cc[c] = pd.to_numeric(cc[c], errors="coerce"); Xcols.append(c)
cc["stroke_base"] = pd.to_numeric(cc["stroke_base"], errors="coerce")
cc = cc.dropna(subset=Xcols + ["stroke_base"])
print("n after dropna:", len(cc), "| events:", cc.stroke_base.sum())
print("dtypes sample:", {c: str(cc[c].dtype) for c in Xcols[:8]})
X = np.column_stack([np.ones(len(cc))] + [cc[c].astype(float).values for c in Xcols])
w = cc["w_norm"].values
H = X.T @ ((w * (1 - w * 0) )[:, None] * X)  # placeholder
H = X.T @ (w[:, None] * X)
print("H shape:", H.shape, "| rank:", np.linalg.matrix_rank(H), "| has NaN:", np.isnan(H).any(), "| has inf:", np.isinf(H).any())
print("w stats: min %.4f max %.4f nan %d" % (w.min(), w.max(), np.isnan(w).sum()))
# per-column constant check
for j, c in enumerate(["const"] + Xcols):
    col = X[:, j]
    print(c, "| unique:", len(np.unique(col)), "| NaN:", np.isnan(col).sum())
