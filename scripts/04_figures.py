# ============================================================================
# 04_figures.py
# Figures for WTI x stroke dual-cohort manuscript
#   Figure 2: RCS dose-response (3 panels)
#   Figure 3: forest plot (main models + Cox + Fine-Gray)
#   Figure 4: discrimination AUC comparison (7 objects, 2 cohorts)
# Style: Wong palette, Arial, 300 dpi PNG + vector PDF
# ============================================================================
import pandas as pd
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

plt.rcParams.update({
    "font.family": "Arial", "font.size": 8,
    "axes.labelsize": 9, "axes.titlesize": 10,
    "axes.spines.top": False, "axes.spines.right": False,
})
WONG = ["#000000", "#E69F00", "#56B4E9", "#009E73",
        "#F0E442", "#0072B2", "#D55E00", "#CC79A7"]
RES = r"D:\NHANES\results"
FIG = r"D:\NHANES\output\figures"
import os
os.makedirs(FIG, exist_ok=True)

# ---------------------------------------------------------------------------
# Figure 2: RCS dose-response
# ---------------------------------------------------------------------------
rcs = pd.read_csv(RES + r"\04a_rcs_predictions.csv")
layers = [("NHANES-cross", "A  NHANES 2005-2018 (cross-sectional)", WONG[5]),
          ("CHARLS-cross", "B  CHARLS 2011 (cross-sectional)", WONG[1]),
          ("CHARLS-prosp", "C  CHARLS 2011-2018 (7-year prospective)", WONG[3])]
fig, axes = plt.subplots(1, 3, figsize=(7.0, 2.6))
for ax, (ly, title, col) in zip(axes, layers):
    d = rcs[rcs.layer == ly]
    ax.plot(d.WTI, np.exp(d.logOR), color=col, lw=1.5)
    ax.fill_between(d.WTI, np.exp(d.logOR - 1.96 * d.se),
                    np.exp(d.logOR + 1.96 * d.se), color=col, alpha=0.15)
    ax.axhline(1.0, color="grey", lw=0.7, ls="--")
    ax.axvline(85, color="grey", lw=0.5, ls=":")
    ax.set_title(title, fontsize=8)
    ax.set_xlabel("WTI (cm\u00b7mmol/L)")
    ax.set_ylim(0.5, 2.5)
axes[0].set_ylabel("Odds ratio (vs. WTI = 85)")
for ax in axes[1:]:
    ax.set_ylabel("")
fig.tight_layout()
fig.savefig(FIG + r"\Figure2_RCS.pdf"); fig.savefig(FIG + r"\Figure2_RCS.png", dpi=300)
plt.close(fig)
print("Figure2 done")

# ---------------------------------------------------------------------------
# Figure 3: forest plot
# ---------------------------------------------------------------------------
main = pd.read_csv(RES + r"\03_main_models.csv")
main = main[["cohort", "layer", "model", "est", "lo", "hi"]]
main["model"] = (main.model.str.upper()
                 .str.replace(r"^CM", "M", regex=True))
main = main.assign(label=lambda x: x.cohort + " " + x.model)
fg = pd.read_csv(RES + r"\03c_cox_fg.csv")
fg = fg.assign(layer="prosp", cohort="CHARLS",
               label="CHARLS " + fg.model.str.upper())
allres = pd.concat([
    main, fg[["cohort", "layer", "model", "hr", "lo", "hi", "label"]].rename(
        columns={"hr": "est"})], ignore_index=True)
allres["grp"] = allres.layer.map({"cross": 0, "prosp7y": 1, "prosp": 2})
allres = allres.sort_values(["cohort", "grp", "model"]).reset_index(drop=True)

fig, ax = plt.subplots(figsize=(3.5, 5.0))
y = np.arange(len(allres))[::-1]
colors = [WONG[5] if "NHANES" in l else (WONG[3] if ("PM" in l or "COX" in l or "FG" in l) else WONG[1])
          for l in allres.label]
ax.errorbar(allres.est, y, xerr=[allres.est - allres.lo, allres.hi - allres.est],
            fmt="o", color="black", ms=4, lw=1.0, capsize=2)
ax.scatter(allres.est, y, color=colors, s=18, zorder=3)
ax.axvline(1.0, color="grey", lw=0.7, ls="--")
ax.set_yticks(y); ax.set_yticklabels(allres.label, fontsize=7.5)
ax.set_xlabel("OR / HR per 1-SD WTI")
ax.set_xlim(0.4, 2.2)
ax.spines["left"].set_visible(False)
fig.tight_layout()
fig.savefig(FIG + r"\Figure3_forest.pdf"); fig.savefig(FIG + r"\Figure3_forest.png", dpi=300)
plt.close(fig)
print("Figure3 done")

# ---------------------------------------------------------------------------
# Figure 4: discrimination AUC (7 objects, 2 cohorts)
# ---------------------------------------------------------------------------
disc = pd.read_csv(RES + r"\03b_discrimination.csv")
disc = disc[disc.object != "WTI"]          # reference set shown separately
objs = ["WTI", "WC", "TG", "TyG", "TyGWC", "ABSI", "HTGW"]
fig, axes = plt.subplots(1, 2, figsize=(7.0, 3.0), sharey=False)
for ax, (coh, title) in zip(axes, [("NHANES", "A  NHANES"),
                                   ("CHARLS", "B  CHARLS")]):
    d = pd.read_csv(RES + r"\03b_discrimination.csv")
    d = d[d.cohort == coh].set_index("object").loc[objs].reset_index()
    ax.errorbar(d.auc, np.arange(len(d))[::-1],
                xerr=[d.auc - d.lo, d.hi - d.auc],
                fmt="o", color="black", ms=3, lw=0.9, capsize=2)
    ax.scatter(d.auc, np.arange(len(d))[::-1],
               color=[WONG[5] if o == "WTI" else WONG[0] for o in d.object], s=22, zorder=3)
    ax.set_yticks(np.arange(len(d))[::-1]); ax.set_yticklabels(d.object, fontsize=7.5)
    ax.set_title(title, fontsize=8)
    ax.set_xlabel("AUC (95% CI)")
    ax.set_xlim(0.55, 0.80)
axes[0].set_ylabel("Index")
fig.tight_layout()
fig.savefig(FIG + r"\Figure4_AUC.pdf"); fig.savefig(FIG + r"\Figure4_AUC.png", dpi=300)
plt.close(fig)
print("Figure4 done")
print("ALL FIGURES DONE ->", FIG)
