# ============================================================================
# 08_audit_labels.py  (Phase 0: T1-T3 codebook adjudication)
# Extract variable labels + value labels for every variable used in the
# exposure/outcome/covariate definitions, from official data files.
# Output: qc/phase0_variable_labels.txt
# Date: 2026-08-16
# ============================================================================
import pyreadstat
import os

BASE = r"D:\NHANES"
OUT = r"D:\NHANES\qc"
os.makedirs(OUT, exist_ok=True)
lines = []

def dump(meta, varname, tag):
    labels = meta.column_names_to_labels
    vv = meta.variable_value_labels
    lab = labels.get(varname, labels.get(varname.upper(), "<no label>"))
    vals = vv.get(varname, vv.get(varname.upper(), None))
    lines.append(f"[{tag}] {varname}: {lab}")
    if vals:
        for k, v in vals.items():
            lines.append(f"    {k} = {v}")

# ---- NHANES XPT files ----
nh_files = {
    "TRIGLY_D": "data/raw/TRIGLY_D.XPT", "MCQ_D": "data/raw/MCQ_D.XPT",
    "DEMO_D": "data/raw/DEMO_D.XPT", "BPQ_D": "data/raw/BPQ_D.XPT",
    "DIQ_D": "data/raw/DIQ_D.XPT", "SMQ_D": "data/raw/SMQ_D.XPT",
    "ALQ_D": "data/raw/ALQ_D.XPT", "BMX_D": "data/raw/BMX_D.XPT",
    "PAQ_D": "data/raw/PAQ_D.XPT", "GLU_D": "data/raw/GLU_D.XPT",
    "RXQ_RX_D": "data/raw/RXQ_RX_D.XPT",
}
nh_vars = {
    "TRIGLY_D": ["LBXTR", "WTSAF2YR"],
    "MCQ_D": ["MCQ160E"],
    "DEMO_D": ["RIDAGEYR", "RIAGENDR", "RIDRETH1", "DMDEDUC2", "SDMVSTRA", "SDMVPSU"],
    "BPQ_D": ["BPQ020", "BPQ050A"],
    "DIQ_D": ["DIQ010"],
    "SMQ_D": ["SMQ020", "SMQ040"],
    "ALQ_D": ["ALQ110", "ALQ101"],
    "BMX_D": ["BMXWAIST", "BMXHT", "BMXBMI"],
    "PAQ_D": ["PAQ180", "PAD680"],
    "GLU_D": ["LBXGLU"],
    "RXQ_RX_D": ["RXDUSE", "RXDDRUG"],
}
for tag, path in nh_files.items():
    try:
        df, meta = pyreadstat.read_xport(os.path.join(BASE, path), metadataonly=True)
        lines.append(f"\n=== {tag} ({path}) ===")
        for v in nh_vars[tag]:
            dump(meta, v, tag)
    except Exception as e:
        lines.append(f"\n=== {tag}: FAILED {e} ===")

# ---- CHARLS dta files ----
ch_files = {
    "hs11": r"CHARLS\CHARLS_1725074232_3\CHARLS\2011\health_status_and_functioning.dta",
    "dm11": r"CHARLS\CHARLS_1725074232_3\CHARLS\2011\demographic_background.dta",
    "bm11": r"CHARLS\CHARLS_1725074232_3\CHARLS\2011\biomarkers.dta",
    "blood11": r"CHARLS\CHARLS_1725074232_3\CHARLS\2011\Blood_20140429.dta",
    "hs13": r"CHARLS\CHARLS_1725074232_3\CHARLS\2013\Health_Status_and_Functioning.dta",
    "hs15": r"CHARLS\CHARLS_1725074232_3\CHARLS\2015\Health_Status_and_Functioning.dta",
    "hs18": r"CHARLS\CHARLS_1725074232_3\CHARLS\2018\Health_Status_and_Functioning.dta",
    "exit13": r"CHARLS\CHARLS_1725074232_3\CHARLS\2013\Exit_Interview.dta",
    "blood15": r"CHARLS\CHARLS_1725074232_3\CHARLS\2015\Blood.dta",
}
ch_vars = {
    "hs11": ["da007_1_", "da007_2_", "da007_3_", "da007_8_", "da010_2_s2", "da011s2",
             "da059", "da061", "da067", "da051_1_", "da052_1_", "da051_2_", "da052_2_"],
    "dm11": ["ba002_1", "ba004", "rgender", "bd001"],
    "bm11": ["qm002", "qh006", "ql002"],
    "blood11": ["newtg", "newglu", "newhdl", "newldl", "bloodweight"],
    "hs13": ["da019_w2_1"],
    "hs15": ["da019_w2_1", "zda007_8_"],
    "hs18": ["da019_w2_1", "da007_8_"],
    "exit13": ["exb001_1", "exb001_2"],
    "blood15": ["bl_tg", "bl_crp", "bl_crea", "bl_cysc", "bl_glu", "bl_fasting", "Blood_weight"],
}
for tag, rel in ch_files.items():
    try:
        df, meta = pyreadstat.read_dta(os.path.join(BASE, rel), metadataonly=True)
        lines.append(f"\n=== {tag} ({rel}) ===")
        for v in ch_vars[tag]:
            dump(meta, v, tag)
    except Exception as e:
        lines.append(f"\n=== {tag}: FAILED {e} ===")

with open(os.path.join(OUT, "phase0_variable_labels.txt"), "w", encoding="utf-8") as f:
    f.write("\n".join(lines))
print("DONE; lines:", len(lines))
