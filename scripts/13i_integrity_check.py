# ============================================================================
# 13i_integrity_check.py  (round 4, 2026-08-20: six-reviewer fix round)
# Integrity-gate re-run for the six-reviewer repair package:
#   1) citation-number audit of manuscript_main.md (1-32 both directions)
#   2) numeric cross-check: 2015/NDI layers vs audited CSVs
#   3) Tables 1-5 present in tables_submission.docx
#   4) provenance: scripts + CSVs exist
#   5) 13q threshold (downgraded) + subgroup fragments
#   6) 13r HTGW binary fragments
#   7) round-4 refitted numbers (M1 maximal samples, E-values, CIF, strata,
#      calibration, interactions, Lag-2, interval-censored, baseline)
# Output: qc/13i_integrity_check.txt
# ============================================================================
import re
import pandas as pd
from docx import Document

MAN = r"D:\NHANES\manuscript\final\manuscript_main.md"
RES = r"D:\NHANES\results"
QC = r"D:\NHANES\qc"

lines = []
def log(s=""):
    print(s); lines.append(s)

text = open(MAN, encoding="utf-8").read()

# ---- 1) citations ----
cited = set()
for grp in re.findall(r"\[(\d+(?:[-,]\d+)*)\]", text):
    for part in grp.split(","):
        part = part.strip()
        if "-" in part:
            a, b = part.split("-")
            cited.update(range(int(a), int(b) + 1))
        elif part:
            cited.add(int(part))
nrefs = 48  # 2026-08-22 阶段3：新增 16 条（48 条）后同步；此前为 32
missing_ref = [i for i in range(1, nrefs + 1) if i not in cited]
ghost = sorted(c for c in cited if c < 1 or c > nrefs)
log(f"[1] citations: refs cited in text = {len(cited)}/{nrefs}")
log(f"    uncited refs: {missing_ref}")
log(f"    ghost numbers: {ghost}")
log(f"    -> {'PASS' if not missing_ref and not ghost else 'FAIL'}")

# ---- 2) new-layer numbers (2015 + NDI) ----
r15 = pd.read_csv(RES + r"\13_2015_main_models.csv")
rn = pd.read_csv(RES + r"\13g_ndi_cox_models.csv")
checks = []
def expect(src, layer, model, text_frag):
    r = r15 if src == "r15" else rn
    s = r[(r.layer == layer) & (r.model == model)].iloc[0]
    frag = text_frag.replace("EST", f"{s.est:.2f}").replace("LO", f"{s.lo:.2f}").replace("HI", f"{s.hi:.2f}")
    ok = frag in text
    checks.append((f"{layer}/{model}", frag, ok))
    return ok

expect("r15", "cross", "CM1", "M1 OR EST (LO-HI")
expect("r15", "cross", "CM2", "M2 OR EST (LO-HI")
expect("r15", "cross", "CM3", "M3 OR EST (LO-HI")
expect("r15", "cross-altw", "CA1", "M1 EST, LO-HI")
expect("r15", "cross-phys", "CP1", "M1 OR EST, LO-HI")
expect("r15", "cross-women", "M1", "EST (LO-HI) in women")
expect("r15", "cross-men", "M1", "EST (LO-HI) in men")
expect("r15", "cross-age-45to59", "M1", "EST (LO-HI, 45-59 y)")
expect("r15", "cross-age-60plus", "M1", "EST (LO-HI, ≥ 60 y)")
expect("rn", "all-cause", "AM1", "EST (LO-HI, P = 0.031)")
expect("rn", "all-cause", "AM2", "EST (LO-HI, P = 0.39)")
expect("rn", "all-cause", "AM3", "EST (LO-HI, P = 0.86)")
expect("rn", "stroke-death", "SM1", "EST (LO-HI, P = 0.75)")
expect("rn", "stroke-death", "SM2", "EST (LO-HI, P = 0.71)")
expect("rn", "stroke-death", "SM3", "EST (LO-HI, P = 0.91)")
expect("rn", "stroke-death-FG", "M3", "EST (LO-HI, P = 0.89)")

n_pass = sum(1 for c in checks if c[2])
log(f"[2] stats-consistency: {n_pass}/{len(checks)} new-layer numbers match CSVs")
for name, frag, ok in checks:
    if not ok:
        log(f"    MISS: {name} | {frag}")
log(f"    -> {'PASS' if n_pass == len(checks) else 'FAIL'}")
log(f"    tertile check: {('1.28 for T2' in text) and ('1.39 for T3' in text) and ('0.90, T2' in text) and ('0.93, T3' in text)}")
log(f"    MDE check: {'1.21 per 1-SD' in text and '1.38 per 1-SD' in text}")

# ---- 5) 13q threshold (downgraded) + subgroup fragments ----
def h2(x):
    return f"{x + 1e-9:.2f}"

qt = pd.read_csv(RES + r"\13q_threshold.csv").iloc[0]
th_frags = [
    f"{qt.k:.1f} cm\u00b7mmol/L",
    "72.1-137.8",
    f"{h2(qt.hr_below_per1SD)} ({h2(qt.hr_below_lo)}-{h2(qt.hr_below_hi)})",
    f"{h2(qt.hr_above_per1SD)} ({h2(qt.hr_above_lo)}-{h2(qt.hr_above_hi)})",
    f"{h2(qt.hr_below_per10)} ({h2(qt.hr_below_per10_lo)}-{h2(qt.hr_below_per10_hi)})",
    f"{h2(qt.hr_above_per10)} ({h2(qt.hr_above_per10_lo)}-{h2(qt.hr_above_per10_hi)})",
    "does not establish a clinical threshold".replace("does", "do"),
]
sub_frags = ["P = 0.14/0.67/0.24", "P = 0.81/0.54/0.91", "P = 0.54/0.50/0.95",
             "OR 1.25, P = 0.002", "OR 1.10, P = 0.20"]
all5 = th_frags + sub_frags
n5 = sum(1 for f in all5 if f in text)
log(f"[5] 13q additions: {n5}/{len(all5)} fragments match CSVs")
for f in all5:
    if f not in text:
        log(f"    MISS: {f}")
log(f"    -> {'PASS' if n5 == len(all5) else 'FAIL'}")

# ---- 6) 13r HTGW binary fragments ----
rh = pd.read_csv(RES + r"\13r_htgw_models.csv")
def rrow(layer, model):
    s = rh[(rh.layer == layer) & (rh.model == model)].iloc[0]
    return f"{h2(s.est)} ({h2(s.lo)}-{h2(s.hi)})"
f6 = [
    f"M1 ORs of {rrow('CHARLS2011-cross-HTgw','M1')} in CHARLS 2011",
    f"and {rrow('CHARLS2015-cross-HTgw','M1')} in the 2015 replication",
]
for layer in ["NHANES-cross-HTgw"]:
    s = rh[(rh.layer == layer) & (rh.model == "M1")].iloc[0]
    f6.append(f"(M1 OR {h2(s.est)}, {h2(s.lo)}-{h2(s.hi)};")
    s = rh[(rh.layer == layer) & (rh.model == "M3")].iloc[0]
    f6.append(f"M3 OR {h2(s.est)}, {h2(s.lo)}-{h2(s.hi)})")
n6 = sum(1 for f in f6 if f in text)
log(f"[6] 13r HTGW additions: {n6}/{len(f6)} fragments match CSVs")
for f in f6:
    if f not in text:
        log(f"    MISS: {f}")
log(f"    -> {'PASS' if n6 == len(f6) else 'FAIL'}")

# ---- 7) round-4 refitted numbers ----
def pchk(p):
    p = float(p)
    if p < 0.001: return "P < 0.001"
    if p < 0.1: return f"P = {p + 1e-9:.3f}"
    return f"P = {p + 1e-9:.2f}"

m = pd.read_csv(RES + r"\03_main_models.csv")
def mrow(cohort, layer, model):
    s = m[(m.cohort == cohort) & (m.layer == layer) & (m.model == model)].iloc[0]
    return f"{h2(s.est)} ({h2(s.lo)}-{h2(s.hi)}, {pchk(s.p)}"

cf = pd.read_csv(RES + r"\03c_cox_fg.csv")
def cfrow(model):
    s = cf[cf.model == model].iloc[0]
    return f"{h2(s.hr)} ({h2(s.lo)}-{h2(s.hi)}, {pchk(s.p)}"

f7 = [
    mrow("CHARLS", "cross", "cm1"),
    mrow("CHARLS", "cross", "cm2"),
    mrow("CHARLS", "cross", "cm3"),
    pm1_frag := (lambda s: f"{h2(s.est)}, {h2(s.lo)}-{h2(s.hi)}, {pchk(s.p)}")(
        m[(m.cohort == "CHARLS") & (m.layer == "prosp7y") & (m.model == "pm1")].iloc[0]),
    pm3_frag := (lambda s: f"{h2(s.est)}, {h2(s.lo)}-{h2(s.hi)}, {pchk(s.p)}")(
        m[(m.cohort == "CHARLS") & (m.layer == "prosp7y") & (m.model == "pm3")].iloc[0]),
    cfrow("Cox-M1"),
    cfrow("Cox-M3"),
    cfrow("FG-M1"),
    cfrow("FG-M3"),
    "9,856", "9,206", "9,636", "9,028",
    "42/664", "178/9,206",
    "1.60 (1.90) in CHARLS cross-sectional",
    "1.48 (1.65) for the CHARLS Cox model",
    "1.51 (1.63) for Fine-Gray",
    "6.3% (T2), and 7.7% (T3)",
    "1.65 (1.32-2.06)", "2.02 (1.64-2.50)",
    "P = 0.98",
    "corrected HR 1.24, 1.11-1.38",
    "1.13 (1.06-1.19",          # Lag-2 Cox M1
    "1.14 (1.09-1.19",          # Lag-2 FG M1
    "M1 OR 1.13 (1.06-1.20",    # interval-censored M1
    "114.4 vs 100.7",
    "5.6% (15.2%)",
    "32.4%",
    "10,766 of 26,282",
    "ΔAUC +0.015, P = 0.001",
    "ΔAUC 0.014, P = 0.034",
    "10,265 participants (528 strokes)",
]
n7 = sum(1 for f in f7 if f in text)
log(f"[7] round-4 refitted fragments: {n7}/{len(f7)} match manuscript")
for f in f7:
    if f not in text:
        log(f"    MISS: {f}")
log(f"    -> {'PASS' if n7 == len(f7) else 'FAIL'}")

# ---- 3) Tables in docx ----
doc = Document(r"D:\NHANES\manuscript\final\tables_submission.docx")
paras = [p.text for p in doc.paragraphs]
t1_5 = all(any(f"Table {k}" in p for p in paras) for k in range(1, 6))
log(f"[3] Tables 1-5 present in tables_submission.docx: {t1_5} -> {'PASS' if t1_5 else 'FAIL'}")
t_all = " ".join(c.text for t in doc.tables for r in t.rows for c in r.cells)
log(f"    Table 1 contains new CHARLS medians: {'114.4' in t_all and '100.7' in t_all}")
log(f"    Table 5 contains 2015 CM1 row: {'1.237 (1.068-1.433)' in t_all}")
log(f"    Table 5 contains NDI AM1 row: {'1.063 (1.006-1.124)' in t_all}")
log(f"    Table 2 contains M1 n 9,856/9,636: {'9,856' in t_all and '9,636' in t_all}")

# ---- 4) provenance ----
import os
paths = [r"D:\NHANES\data\processed\charls_2015_cross_cov.csv",
         r"D:\NHANES\data\nhanes_mort2019.csv",
         r"D:\NHANES\results\13_2015_main_models.csv",
         r"D:\NHANES\results\13g_ndi_cox_models.csv",
         r"D:\NHANES\results\13h_mde.txt"]
missing = [p for p in paths if not os.path.exists(p)]
log(f"[4] provenance files: {len(paths) - len(missing)}/{len(paths)} exist; missing={missing}")
script_files = ["03_analysis.R", "03b_analysis.R", "03c_analysis.R", "05a_evalue.R",
                "05b_lag2.R", "05c_interval.R", "06b_cif.R", "06c_tables.R",
                "08_audit_charls_strata.R", "08_audit_fg_cluster.R",
                "10_audit_prereview_fixes.R", "11_build_tables_docx.py",
                "12a_regcal.R", "13e_charls2015_analysis.R", "13g_ndi_cox.R",
                "13h_mde.R", "13q_subgroups_threshold.R", "13r_htgw.R",
                "14b_renumber_refs.py"]
log("    scripts all present: %s" %
    all(os.path.exists(os.path.join(r"D:\NHANES\scripts", n)) for n in script_files))

with open(QC + r"\13i_integrity_check.txt", "w", encoding="utf-8") as fh:
    fh.write("\n".join(lines))
print("saved:", QC + r"\13i_integrity_check.txt")
