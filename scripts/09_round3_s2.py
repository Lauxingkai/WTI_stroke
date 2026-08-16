# ============================================================================
# 09_round3_s2.py  (Round 3, item 3: formalize sex/age stratification)
# Builds Table S2 from the audit-run stratification results (08_audit_nhanes_neg.R
# and 08_audit_charls_strata.R outputs, all re-verified in Round 3).
# Output: results/TableS2_strata.csv
# Date: 2026-08-16
# ============================================================================
import pandas as pd

rows = [
    # cohort, layer, subgroup, n_approx, OR, lo, hi, p
    ("NHANES", "cross-sectional M1", "all", 10302, 1.077, 1.005, 1.154, 0.0367),
    ("NHANES", "cross-sectional M1", "age 40-59", None, 1.059, 0.969, 1.157, 0.2104),
    ("NHANES", "cross-sectional M1", "age >=60", None, 1.117, 0.975, 1.279, 0.1136),
    ("NHANES", "cross-sectional M1", "men", None, 1.002, 0.872, 1.151, 0.9785),
    ("NHANES", "cross-sectional M1", "women", None, 1.113, 1.022, 1.213, 0.0158),
    ("CHARLS", "cross-sectional M1", "all", 9214, 1.184, 1.066, 1.316, 0.0016),
    ("CHARLS", "cross-sectional M1", "age 45-59", None, 1.180, 1.067, 1.305, 0.0014),
    ("CHARLS", "cross-sectional M1", "age >=60", None, 1.195, 1.006, 1.420, 0.0431),
    ("CHARLS", "cross-sectional M1", "men", None, 1.260, 1.093, 1.453, 0.0016),
    ("CHARLS", "cross-sectional M1", "women", None, 1.145, 1.007, 1.302, 0.0397),
    ("CHARLS", "prospective M1 (2018-wave outcome)", "all", 9036, 1.126, 1.050, 1.207, 0.0009),
    ("CHARLS", "prospective M1 (2018-wave outcome)", "age <60", None, 1.165, 1.071, 1.266, 0.0004),
    ("CHARLS", "prospective M1 (2018-wave outcome)", "age >=60", None, 1.048, 0.922, 1.192, 0.4700),
    ("CHARLS", "prospective M1 (2018-wave outcome)", "men", None, 1.115, 1.032, 1.205, 0.0063),
    ("CHARLS", "prospective M1 (2018-wave outcome)", "women", None, 1.124, 1.000, 1.263, 0.0513),
]
df = pd.DataFrame(rows, columns=["cohort", "layer", "subgroup", "n", "OR", "lo", "hi", "p"])
df.to_csv(r"D:\NHANES\results\TableS2_strata.csv", index=False)
print(df.to_string(index=False))
print("\nDONE")
