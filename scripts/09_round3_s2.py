# ============================================================================
# 09_round3_s2.py  (Round 3, item 3: formalize sex/age stratification)
# Builds Table S2 from the audit-run stratification results (08_audit_nhanes_neg.R
# and 08_audit_charls_strata.R outputs, all re-verified in Round 3).
# v2 2026-08-20: CHARLS M1 strata refit on maximal model-wise samples
#                (n = 9,856 cross / 9,636 prospective), per six-reviewer M4.
# Output: results/TableS2_strata.csv
# Date: 2026-08-16 | v2 2026-08-20
# ============================================================================
import pandas as pd

rows = [
    # cohort, layer, subgroup, n_approx, OR, lo, hi, p
    ("NHANES", "cross-sectional M1", "all", 10302, 1.077, 1.005, 1.154, 0.0367),
    ("NHANES", "cross-sectional M1", "age 40-59", None, 1.059, 0.969, 1.157, 0.2104),
    ("NHANES", "cross-sectional M1", "age >=60", None, 1.117, 0.975, 1.279, 0.1136),
    ("NHANES", "cross-sectional M1", "men", None, 1.002, 0.872, 1.151, 0.9785),
    ("NHANES", "cross-sectional M1", "women", None, 1.113, 1.022, 1.213, 0.0158),
    ("CHARLS", "cross-sectional M1", "all", 9856, 1.166, 1.054, 1.288, 0.0029),
    ("CHARLS", "cross-sectional M1", "age 45-59", None, 1.168, 1.063, 1.283, 0.0013),
    ("CHARLS", "cross-sectional M1", "age >=60", None, 1.171, 0.988, 1.388, 0.0688),
    ("CHARLS", "cross-sectional M1", "men", None, 1.201, 1.028, 1.403, 0.0211),
    ("CHARLS", "cross-sectional M1", "women", None, 1.169, 1.045, 1.308, 0.0066),
    ("CHARLS", "prospective M1 (2018-wave outcome)", "all", 9636, 1.136, 1.064, 1.212, 0.0002),
    ("CHARLS", "prospective M1 (2018-wave outcome)", "age <60", None, 1.173, 1.084, 1.270, 0.0001),
    ("CHARLS", "prospective M1 (2018-wave outcome)", "age >=60", None, 1.058, 0.940, 1.190, 0.3486),
    ("CHARLS", "prospective M1 (2018-wave outcome)", "men", None, 1.128, 1.048, 1.213, 0.0014),
    ("CHARLS", "prospective M1 (2018-wave outcome)", "women", None, 1.135, 1.017, 1.267, 0.0245),
]
df = pd.DataFrame(rows, columns=["cohort", "layer", "subgroup", "n", "OR", "lo", "hi", "p"])
df.to_csv(r"D:\NHANES\results\TableS2_strata.csv", index=False)
print(df.to_string(index=False))
print("\nDONE")
