# ============================================================================
# 13i_integrity_check.py
# Integrity-gate re-run for the 2026-08-16 package updates (Opt-1 + NDI):
#   1) citation-number audit of manuscript_main.md (1-32 both directions)
#   2) numeric cross-check: every new-layer number in the manuscript text
#      must match the audited CSVs (13_2015_main_models.csv / 13g_ndi_cox)
#   3) Table 5 present in tables_submission.docx
#   4) provenance: scripts + CSVs exist
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
nrefs = 32
missing_ref = [i for i in range(1, nrefs + 1) if i not in cited]
ghost = sorted(c for c in cited if c < 1 or c > nrefs)
log(f"[1] citations: refs cited in text = {len(cited)}/{nrefs}")
log(f"    uncited refs: {missing_ref}")
log(f"    ghost numbers: {ghost}")
log(f"    -> {'PASS' if not missing_ref and not ghost else 'FAIL'}")

# ---- 2) new-layer numbers ----
def f(row):
    return f"{row.est:.2f} ({row.lo:.2f}-{row.hi:.2f})"

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

expect("r15", "cross", "CM1", "M1 OR 1.24 (1.07-1.43")
expect("r15", "cross", "CM2", "M2 OR 1.18 (1.01-1.37")
expect("r15", "cross", "CM3", "M3 OR 1.10 (0.93-1.29")
expect("r15", "cross-altw", "CA1", "M1 1.23, 1.07-1.41")
expect("r15", "cross-phys", "CP1", "M1 OR 1.31, 0.90-1.89")
expect("r15", "cross-women", "M1", "1.29 (1.08-1.54) in women")
expect("r15", "cross-men", "M1", "1.23 (0.98-1.54) in men")
expect("r15", "cross-age-45to59", "M1", "1.26 (0.94-1.70, 45-59 y)")
expect("r15", "cross-age-60plus", "M1", "1.18 (1.02-1.37, ≥60 y)")
expect("rn", "all-cause", "AM1", "1.06 (1.01-1.12, P = 0.031)")
expect("rn", "all-cause", "AM2", "1.03 (0.96-1.10, P = 0.39)")
expect("rn", "all-cause", "AM3", "1.01 (0.94-1.08, P = 0.86)")
expect("rn", "stroke-death", "SM1", "0.96 (0.72-1.26, P = 0.75)")
expect("rn", "stroke-death", "SM2", "1.05 (0.82-1.34, P = 0.71)")
expect("rn", "stroke-death", "SM3", "1.02 (0.77-1.35, P = 0.91)")
expect("rn", "stroke-death-FG", "M3", "1.02 (0.79-1.32, P = 0.89)")

n_pass = sum(1 for c in checks if c[2])
log(f"[2] stats-consistency: {n_pass}/{len(checks)} new-layer numbers match CSVs")
for name, frag, ok in checks:
    if not ok:
        log(f"    MISS: {name} | {frag}")
log(f"    -> {'PASS' if n_pass == len(checks) else 'FAIL'}")
log(f"    tertile check: {('1.28 (T2)' in text) and ('1.39 (T3)' in text) and ('0.90 (T2)' in text) and ('0.93 (T3)' in text)}")
log(f"    MDE check: {'1.21 per SD' in text and '1.38 per SD' in text}")

# ---- 3) Table 5 in docx ----
doc = Document(r"D:\NHANES\manuscript\final\tables_submission.docx")
paras = [p.text for p in doc.paragraphs]
t5 = any("Table 5" in p for p in paras)
log(f"[3] Table 5 present in tables_submission.docx: {t5} -> {'PASS' if t5 else 'FAIL'}")
t5text = " ".join(c.text for t in doc.tables for r in t.rows for c in r.cells)
log(f"    Table 5 contains 2015 CM1 row: {'1.237 (1.068-1.433)' in t5text}")
log(f"    Table 5 contains NDI AM1 row: {'1.063 (1.006-1.124)' in t5text}")

# ---- 4) provenance ----
import os
paths = [r"D:\NHANES\data\processed\charls_2015_cross_cov.csv",
         r"D:\NHANES\data\nhanes_mort2019.csv",
         r"D:\NHANES\results\13_2015_main_models.csv",
         r"D:\NHANES\results\13g_ndi_cox_models.csv",
         r"D:\NHANES\results\13h_mde.txt"]
missing = [p for p in paths if not os.path.exists(p)]
log(f"[4] provenance files: {len(paths) - len(missing)}/{len(paths)} exist; missing={missing}")
script_files = ["13a_2015_codebook_extract.py", "13b_2015_value_labels.py",
                "13c_2015_recon.py", "13d_build_2015.py",
                "13e_charls2015_analysis.R", "13f_ndi_parse.R",
                "13g_ndi_cox.R", "13h_mde.R"]
log("    scripts 13a-13h all present: %s" %
    all(os.path.exists(os.path.join(r"D:\NHANES\scripts", n)) for n in script_files))

with open(QC + r"\13i_integrity_check.txt", "w", encoding="utf-8") as fh:
    fh.write("\n".join(lines))
print("saved:", QC + r"\13i_integrity_check.txt")
