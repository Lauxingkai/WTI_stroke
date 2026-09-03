# ============================================================================
# 04_figures.py  v2 (2026-08-22) — BMC-compliant figure revision
#   Figure 2: RCS dose-response (3 panels) — data-driven xlim/ylim, no clipping,
#             nonlinearity-P annotations, reference lines, in-figure key
#   Figure 3: forest plot (13 estimates) — grouped by cohort/layer, right-hand
#             numeric column, per-group header, in-figure legend
#   Figure 4: discrimination AUC (7 indices x 2 cohorts) — numeric labels, WTI legend
#   Style: Wong palette, Arial, 300 dpi PNG + vector PDF + TIFF (LZW), embedded fonts
# ============================================================================
import pandas as pd
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

plt.rcParams.update({
    "font.family": "Arial", "font.size": 8,
    "axes.labelsize": 9, "axes.titlesize": 10,
    "axes.spines.top": False, "axes.spines.right": False,
    "pdf.fonttype": 42, "ps.fonttype": 42,
})
WONG = ["#000000", "#E69F00", "#56B4E9", "#009E73",
        "#F0E442", "#0072B2", "#D55E00", "#CC79A7"]
RES = r"D:\NHANES\results"
FIG = r"D:\NHANES\output\figures"
import os
os.makedirs(FIG, exist_ok=True)

def save_fig(fig, name):
    png_path = os.path.join(FIG, f"{name}.png")
    fig.savefig(os.path.join(FIG, f"{name}.pdf"))
    fig.savefig(png_path, dpi=300)
    # TIFF at 300 dpi: re-read the 300-dpi PNG (canvas.buffer_rgba() would be
    # the 100-dpi default buffer — too low a resolution for submission);
    # tifffile deflate avoids Pillow's crashing tiff encoder here.
    import tifffile
    from PIL import Image
    im = np.asarray(Image.open(png_path).convert("RGB"))
    tifffile.imwrite(os.path.join(FIG, f"{name}.tiff"), im,
                     compression="deflate", resolution=(300, 300))
    plt.close(fig)
    print(f"  saved {name}.pdf/.png/.tiff ({im.shape[1]}x{im.shape[0]})")

# ---------------------------------------------------------------------------
# Figure 2: RCS dose-response (3 panels) — data-driven axes, no clipping
# ---------------------------------------------------------------------------
rcs = pd.read_csv(RES + r"\04a_rcs_predictions.csv")
layers = [("NHANES-cross", "A  NHANES 2005-2018", WONG[5], "0.116"),
          ("CHARLS-cross", "B  CHARLS 2011 (cross-sectional)", WONG[1], "0.005"),
          ("CHARLS-prosp", "C  CHARLS 2011-2018 (7-y prospective)", WONG[3], "<0.001")]

fig, axes = plt.subplots(1, 3, figsize=(6.7, 2.4), sharey=False)
for ax, (ly, title, col, pval) in zip(axes, layers):
    d = rcs[rcs.layer == ly]
    lor, se = d.logOR.values, d.se.values
    orr = np.exp(lor)
    lo = np.exp(lor - 1.96 * se); hi = np.exp(lor + 1.96 * se)
    ax.plot(d.WTI, orr, color=col, lw=1.6, label="OR (95% CI)")
    ax.fill_between(d.WTI, lo, hi, color=col, alpha=0.15)
    ax.axhline(1.0, color="grey", lw=0.7, ls="--")
    ax.axvline(85, color="grey", lw=0.5, ls=":")
    # display range: WTI 40-260 (avoid extrapolation region where CI explodes),
    # ylim 0-4 with CI band auto-clipped so the main curve shape stays readable
    ax.set_xlim(40, 260); ax.set_ylim(0, 4.0)
    ax.set_title(title, fontsize=8, linespacing=1.1)
    ax.set_xlabel("WTI (cm\u00b7mmol/L)")
    ax.legend(loc="upper left", fontsize=6.5, frameon=False)
    # P for nonlinearity in the blank upper-middle/left area (avoid legend,
    # curve and CI band; position tuned per panel)
    ppos = {"NHANES-cross": (112, 3.30), "CHARLS-cross": (62, 2.62), "CHARLS-prosp": (62, 2.62)}
    ptext = {"NHANES-cross": "P for nonlinearity\n= 0.116",
             "CHARLS-cross": "P for nonlinearity\n= 0.005",
             "CHARLS-prosp": "P for nonlinearity\n< 0.001"}
    ax.text(ppos[ly][0], ppos[ly][1], ptext[ly], fontsize=7.5,
            ha="left", va="top", color="black")
axes[0].set_ylabel("Odds ratio (vs. WTI = 85)")
for ax in axes[1:]:
    ax.set_ylabel("")
fig.subplots_adjust(left=0.075, right=0.975, top=0.87, bottom=0.16, wspace=0.18)
print("Figure2 done")
save_fig(fig, "Figure2_RCS")

# ---------------------------------------------------------------------------
# Figure 3: forest plot (13 estimates) — grouped, right-hand numeric column
# ---------------------------------------------------------------------------
main = pd.read_csv(RES + r"\03_main_models.csv")
main = main[["cohort", "layer", "model", "est", "lo", "hi"]]
main["model"] = (main.model.str.upper().str.replace(r"^CM", "M", regex=True))
fg = pd.read_csv(RES + r"\03c_cox_fg.csv")
fg = fg.assign(layer="prosp", cohort="CHARLS")

def lbl(r):
    if r.cohort == "NHANES":
        return f"NHANES {r.model}"
    if r["layer"] == "cross":
        return f"CHARLS {r.model}"
    if "Cox" in r.model.split("-")[0]:
        return f"CHARLS Cox-{r.model.split('-')[1]}"
    if "FG" in r.model.split("-")[0]:
        return f"CHARLS FG-{r.model.split('-')[1]}"
    return f"CHARLS {r.model}"

rows = []
for _, r in main.iterrows():
    grp = "Cross" if r.layer == "cross" else "Prosp"
    rows.append((grp, lbl(r), r.est, r.lo, r.hi))
for _, r in fg.iterrows():
    rows.append(("Prosp", lbl(r), r.hr, r.lo, r.hi))

# order: NHANES cross -> CHARLS cross -> CHARLS prospective block, top-down
def sort_key(row):
    grp, l, *_ = row
    if grp == "Cross":
        return (0 if l.startswith("NHANES") else 1, 0)
    return (2, 0)
rows_sorted = sorted(rows, key=sort_key)   # uppermost first
n = len(rows_sorted)

def color_of(l):
    if l.startswith("NHANES"): return WONG[5]
    if "Cox" in l: return WONG[3]
    if "FG" in l: return WONG[6]
    return WONG[1]

fig, ax = plt.subplots(figsize=(6.7, 6.6))
# display rows: header slots occupy a y level of their own (no overlap)
def build_display():
    disp = []
    def add_hdr(text): disp.append(("H", text, None, None, None, None))
    def add_row(l, est, lo, hi): disp.append(("R", l, est, lo, hi, color_of(l)))
    add_hdr("NHANES (cross-sectional)")
    for (grp, l, est, lo, hi) in rows_sorted:
        if l.startswith("NHANES"): add_row(l, est, lo, hi)
    add_hdr("CHARLS (cross-sectional)")
    for (grp, l, est, lo, hi) in rows_sorted:
        if not l.startswith("NHANES") and grp == "Cross": add_row(l, est, lo, hi)
    add_hdr("CHARLS (prospective 7-y)")
    for (grp, l, est, lo, hi) in rows_sorted:
        if grp == "Prosp": add_row(l, est, lo, hi)
    return disp
disp = build_display()
nd = len(disp)

for i, (kind, l, est, lo, hi, c) in enumerate(disp):
    y = nd - 1 - i                     # top-down
    if kind == "H":
        ax.text(0.78, y, l, fontsize=8, fontweight="bold", color="grey", ha="right",
                va="center")
        ax.axhline(y - 0.5, color="grey", lw=0.5, ls=":")
        continue
    ax.errorbar(est, y, xerr=[[est - lo], [hi - est]],
                fmt="o", color="black", ms=4, lw=1.0, capsize=2.5, zorder=3)
    ax.scatter(est, y, color=c, s=24, zorder=4)
    ax.text(1.30, y, f"{est:.2f} ({lo:.2f}-{hi:.2f})",
            va="center", ha="left", fontsize=8)
ax.axvline(1.0, color="grey", lw=0.8, ls="--")
ax.set_xscale("log")
ax.set_xticks([0.8, 0.9, 1.0, 1.2, 1.4, 1.6])
ax.set_xticklabels(["0.8", "0.9", "1.0", "1.2", "1.4", "1.6"])

row_ticks = [nd - 1 - i for i, (k, *_r) in enumerate(disp) if k == "R"]
row_labels = [r[1] for r in disp if r[0] == "R"]
ax.set_yticks(row_ticks); ax.set_yticklabels(row_labels, fontsize=8)
for tick, l in zip(ax.get_yticklabels(), row_labels):
    tick.set_color(color_of(l))
ax.set_ylim(-1.2, nd - 0.2)
ax.set_xlim(0.80, 1.62)
ax.set_xlabel("OR / HR per 1-SD WTI")
ax.spines["left"].set_visible(False)

handles = [Line2D([0], [0], marker="o", color="w", markerfacecolor=WONG[5], markersize=7, label="NHANES"),
           Line2D([0], [0], marker="o", color="w", markerfacecolor=WONG[1], markersize=7, label="CHARLS"),
           Line2D([0], [0], marker="o", color="w", markerfacecolor=WONG[3], markersize=7, label="CHARLS Cox"),
           Line2D([0], [0], marker="o", color="w", markerfacecolor=WONG[6], markersize=7, label="CHARLS Fine-Gray")]
ax.legend(handles=handles, loc="lower center", fontsize=7, frameon=False, ncol=4,
          bbox_to_anchor=(0.5, -0.16))
fig.tight_layout()
print("Figure3 done")
save_fig(fig, "Figure3_forest")

# ---------------------------------------------------------------------------
# Figure 4: discrimination AUC (7 indices x 2 cohorts) — numeric labels + WTI legend
# ---------------------------------------------------------------------------
disc = pd.read_csv(RES + r"\03b_discrimination.csv")
objs = ["WTI", "WC", "TG", "TyG", "TyGWC", "ABSI", "HTGW"]
fig, axes = plt.subplots(1, 2, figsize=(6.7, 3.2), sharey=False)
for ax, (coh, title) in zip(axes, [("NHANES", "A  NHANES"),
                                   ("CHARLS", "B  CHARLS")]):
    d = disc[disc.cohort == coh].set_index("object").loc[objs].reset_index()
    ypos = list(range(len(d)))[::-1]
    for k, p in enumerate(ypos):
        col = WONG[5] if d.object[k] == "WTI" else WONG[0]
        ax.errorbar(d.auc[k], p, xerr=[[d.auc[k] - d.lo[k]], [d.hi[k] - d.auc[k]]],
                    fmt="o", color="black", ms=3, lw=0.9, capsize=2)
        ax.scatter(d.auc[k], p, color=col, s=22, zorder=3)
        ax.text(0.795, p, f"{d.auc[k]:.3f} ({d.lo[k]:.3f}-{d.hi[k]:.3f})",
                va="center", ha="left", fontsize=8)
    ax.set_yticks(ypos); ax.set_yticklabels(d.object, fontsize=8)
    ax.set_title(title, fontsize=8.5)
    ax.set_xlabel("AUC (95% CI)", fontsize=8.5)
    ax.tick_params(labelsize=8)
    ax.set_xlim(0.55, 0.84)
axes[0].set_ylabel("Index", fontsize=8.5)
fig.tight_layout()
print("Figure4 done")
save_fig(fig, "Figure4_AUC")
print("ALL FIGURES DONE ->", FIG)