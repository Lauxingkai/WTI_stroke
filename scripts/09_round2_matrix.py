# ============================================================================
# 09_round2_matrix.py  (Round 2, priority 3: cross-document number matrix)
# Asserts: (a) every effect estimate in Table 2-4 CSVs appears in the
# manuscript text at least once; (b) key numbers appear with correct values.
# Output: qc/round2_matrix.txt
# Date: 2026-08-16
# ============================================================================
import pandas as pd
import re

MAN = open(r"D:\NHANES\manuscript\final\manuscript_main.md", encoding="utf-8").read()
RES = r"D:\NHANES\results"
lines = []
def log(s=""):
    lines.append(str(s)); print(s)

# (a) Table 2-4 CSV -> manuscript text
t2 = pd.read_csv(RES + r"\Table2_main_models.csv")
t3 = pd.read_csv(RES + r"\Table3_discrimination.csv")
t4 = pd.read_csv(RES + r"\Table4_sensitivity.csv")
missing = []
def check(tag, txt):
    # normalize: compare the est string with commas/spaces removed
    key = re.sub(r"[,\s]", "", str(txt))
    hay = re.sub(r"[,\s]", "", MAN)
    if key not in hay:
        missing.append(tag)
        log(f"MISSING in manuscript: {tag} = {txt}")
for _, r in t2.iterrows():
    check(f"T2 {r.Model}", r["Effect (95% CI)"])
for _, r in t3.iterrows():
    check(f"T3 {r.Index} AUC", r["AUC"])
for _, r in t4.iterrows():
    if "indirect" in str(r.Result):
        check(f"T4 {r.Model}", str(r.Result).split(";")[0])
log(f"(a) Table->manuscript: {sum(1 for _ in t2.iterrows())+sum(1 for _ in t3.iterrows())} checked, missing={len(missing)}")

# (b) key-number spot checks (value, expected occurrences >= 1)
keys = {
    "531": 4, "1.08 (1.01-1.15)": 2, "0.689": 4, "0.647": 3,
    "0.014": 4, "1.11 (1.04-1.18)": 3, "1.12 (1.06-1.17)": 3,
    "1.14": 3, "7647644": 1,
}
for k, minn in keys.items():
    kk = re.sub(r"[,\s]", "", k)
    n = len(re.findall(re.escape(kk), re.sub(r"[,\s]", "", MAN)))
    log(f"(b) '{k}': occurrences={n} (expect >= {minn}) -> {'OK' if n >= minn else 'LOW'}")

# (c) consistency between CN review version and English (key values)
CN = open(r"D:\NHANES\manuscript\final\manuscript_main_中文审核版.md", encoding="utf-8").read()
cn_hay = re.sub(r"[,\s]", "", CN)
for k in ["531", "1.08", "0.689", "0.014", "1.11", "1.12", "7647644"]:
    kk = re.sub(r"[,\s]", "", k)
    en_n = len(re.findall(re.escape(kk), re.sub(r"[,\s]", "", MAN)))
    cn_n = len(re.findall(re.escape(kk), cn_hay))
    log(f"(c) '{k}': EN={en_n} CN={cn_n} -> {'OK' if cn_n >= 1 else 'MISSING-CN'}")

open(r"D:\NHANES\qc\round2_matrix.txt", "w", encoding="utf-8").write("\n".join(lines))
print("\nDONE")
