# ============================================================================
# 13b_2015_value_labels.py
# Opt-1 step B: dump value labels for the exact 2015 variables needed in the
# replication layer, so the mapping is checked against the data itself
# (codebook PDF text is a fallback).
# Output: data/tmp_2015_vallabels.txt
# ============================================================================
import os
import pyreadstat

BASE = r"D:\NHANES\CHARLS\CHARLS_1725074232_3\CHARLS\2015"
OUT = r"D:\NHANES\data"

need = {
    "Demographic_Background.dta": [
        "ba000_w2_3", "ba002_1", "ba004_w3_1", "bd001_w2_4", "bd002_w3",
        "ba003", "ba002",
    ],
    "Health_Status_and_Functioning.dta": [
        "xrgender", "da007_1_", "da007_2_", "da007_3_", "da007_8_",
        "da010_2_s2", "da011s1", "da011s2",
        "da059", "da061", "da067", "da069",
        "da051_1_", "da052_1_", "da051_2_", "da052_2_", "da051_3_", "da052_3_",
        "da019_w2_1", "da007_w2_2_8_", "da019s1",
    ],
    "Blood.dta": ["bl_tg", "bl_top_coding_tg", "Blood_weight", "bl_fasting",
                  "bl_glu", "bl_hdl", "bl_ldl", "bl_cho", "bl_hbalc", "bl_crp", "bl_crea"],
    "Biomarker.dta": ["qi002", "ql002", "qm002", "qm005"],
    "Weights.dta": ["Biomarker_weight", "INDV_weight_ad2"],
    "Sample_Infor.dta": ["crosssection", "died", "iyear", "imonth"],
}

lines = []
for fn, vars_ in need.items():
    path = os.path.join(BASE, fn)
    _, meta = pyreadstat.read_dta(path, metadataonly=True)
    vl = meta.variable_value_labels or {}
    lines.append(f"\n########## {fn} ##########")
    for v in vars_:
        if v in meta.column_names:
            lab = (meta.column_labels[meta.column_names.index(v)] or "").strip()
            vals = vl.get(v, {})
            vals_s = "; ".join(f"{k}={v_}" for k, v_ in vals.items())
            lines.append(f"{v}\tLABEL: {lab}\tVALUES: {vals_s}")
        else:
            lines.append(f"{v}\t*** NOT IN FILE ***")

with open(os.path.join(OUT, "tmp_2015_vallabels.txt"), "w", encoding="utf-8") as f:
    f.write("\n".join(lines))
print("saved:", os.path.join(OUT, "tmp_2015_vallabels.txt"))
print("DONE")
