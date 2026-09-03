# -*- coding: utf-8 -*-
"""17b_fdr_table.py (v2)
FDR table for the manuscript:
  (1) pre-specified primary family (5 analyses): CHARLS 2011 cross M1,
      CHARLS prospective Cox M1, Fine-Gray M1, CHARLS 2015 M1 replication,
      NHANES cross M1 -> BH q reported in Text (Table S10 in supplement).
  (2) all remaining reported P values tabulated as exploratory (no formal
      multiplicity control), raw P only.
P written as "P < 0.001" is treated conservatively as 0.001 (censored flag).
Output: results/17b_fdr_primary.csv ; results/17b_fdr_all.csv ;
        results/17b_fdr_checks.txt
"""
import re
from pathlib import Path

import pandas as pd
import numpy as np

MAN = Path(r"D:\NHANES\manuscript\final\manuscript_main.md")
text = MAN.read_text(encoding="utf-8")

rows = []
pat = re.compile(r"P\s*(?:=|<)\s*(\d+\.\d+)")
for m in pat.finditer(text):
    lt = text[m.start()] == "<"
    val = float(m.group(1))
    ctx = text[max(0, m.start() - 80):m.end()].replace("\n", " ")
    rows.append({"raw_P": 0.001 if lt else val, "censored_lt": lt, "context": ctx})
df = pd.DataFrame(rows)
df = df[df.raw_P <= 1.0].copy()

def bh(pvals):
    p = np.asarray(pvals, dtype=float)
    n = len(p)
    order = p.argsort()          # ascending (smallest first)
    q = np.empty(n)
    for i, idx in enumerate(order, start=1):   # i = ascending rank 1..n
        q[idx] = p[idx] * n / i
    # monotonicity: q of smaller p cannot exceed q of larger p
    best = np.inf
    for idx in order[::-1]:      # from largest p downward
        best = min(best, q[idx])
        q[idx] = best
    return np.clip(q, None, 1.0)

# ---- (1) primary family: locate the five P values by context anchors ----
anchors = {
    "CHARLS 2011 cross M1": "The CHARLS ORs were 1.17",
    "CHARLS prosp Cox M1": "the weighted Cox HR per 1-SD WTI equaled 1.12",
    "CHARLS Fine-Gray M1": "Fine-Gray subdistribution HRs were 1.13",
    "CHARLS 2015 replication M1": "M1 OR 1.24 (1.07-1.43",
    "NHANES cross M1": "each 1-SD rise in WTI corresponded to a stroke OR of 1.08",
}
primary = []
for label, anchor in anchors.items():
    hit = df[df.context.str.contains(anchor, regex=False)]
    if hit.empty:
        # anchor may sit before the P within the 80-char window; widen search
        i = text.find(anchor)
        seg = text[i:i + 220]
        cand = [r for r in rows if r["context"] in seg or seg.find(r["context"]) >= 0]
        # fallback: take the first "P =" after the anchor
        m2 = re.search(r"P\s*(?:=|<)\s*(\d+\.\d+)", seg)
        hit = df.iloc[[0]] if False else None
        if m2:
            primary.append({"label": label, "raw_P": 0.001 if m2.group(0)[1] == "<" else float(m2.group(1)),
                            "censored_lt": m2.group(0)[1] == "<", "context": seg[:120]})
        else:
            primary.append({"label": label, "raw_P": np.nan, "censored_lt": False,
                            "context": f"NOT FOUND near: {anchor}"})
    else:
        r = hit.iloc[0]
        primary.append({"label": label, "raw_P": r.raw_P, "censored_lt": r.censored_lt,
                        "context": r.context})
pf = pd.DataFrame(primary)
pf["BH_q"] = bh(pf.raw_P.to_numpy()) if pf.raw_P.notna().all() else np.nan
pf.to_csv(r"D:\NHANES\results\17b_fdr_primary.csv", index=False)

# ---- (2) all other P values ----
used_ctx = set(pf["context"])
rest = df[~df.context.isin(used_ctx)].copy()
rest["BH_q_all"] = bh(rest.raw_P.to_numpy()) if len(rest) else np.nan
rest.to_csv(r"D:\NHANES\results\17b_fdr_all.csv", index=False)

# ---- report ----
lines = [
    "PRIMARY FAMILY (BH across the 5 pre-specified analyses):",
]
for _, r in pf.iterrows():
    lt = "<" if r.censored_lt else "="
    lines.append(f"  {r.label}: P {lt} {r.raw_P:.3f} -> BH q {r.BH_q:.3f}")
sig_all = (pf.BH_q < 0.05).sum()
lines.append(f"  primary family significant after BH: {sig_all}/{len(pf)}")
lines.append("")
lines.append(f"TOTAL reported P values (incl censored): {len(df)} ; exploratory tabulated: {len(rest)}")
Path(r"D:\NHANES\results\17b_fdr_checks.txt").write_text("\n".join(lines), encoding="utf-8")
print("\n".join(lines))