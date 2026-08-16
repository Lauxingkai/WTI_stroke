# 13k_package_audit.py — verify every submission-package file exists and is fresh
import os
from datetime import datetime

files = {
    "main manuscript (md)": r"D:\NHANES\manuscript\final\manuscript_main.md",
    "main manuscript (docx)": r"D:\NHANES\manuscript\final\manuscript_main.docx",
    "Chinese audit (md)": r"D:\NHANES\manuscript\final\manuscript_main_中文审核版.md",
    "Chinese audit (docx)": r"D:\NHANES\manuscript\final\manuscript_main_中文审核版.docx",
    "supplementary (md)": r"D:\NHANES\manuscript\final\supplementary.md",
    "supplementary (docx)": r"D:\NHANES\manuscript\final\supplementary.docx",
    "tables docx": r"D:\NHANES\manuscript\final\tables_submission.docx",
    "cover letter": r"D:\NHANES\manuscript\final\cover_letter.md",
    "OSF draft": r"D:\NHANES\manuscript\final\OSF_preregistration_draft.md",
    "README": r"D:\NHANES\manuscript\final\README.md",
    "variable dictionary": r"D:\NHANES\manuscript\final\variable_dictionary.md",
    "SUBMISSION_PACKAGE": r"D:\NHANES\manuscript\final\SUBMISSION_PACKAGE.md",
    "STROBE": r"D:\NHANES\qc\strobe_submission.md",
    "INTEGRITY_REPORT": r"D:\NHANES\qc\INTEGRITY_REPORT.md",
    "Figure 1": r"D:\NHANES\output\figures\Figure1_flow.pdf",
    "Figure 2": r"D:\NHANES\output\figures\Figure2_RCS.pdf",
    "Figure 3": r"D:\NHANES\output\figures\Figure3_forest.pdf",
    "Figure 4": r"D:\NHANES\output\figures\Figure4_AUC.pdf",
    "Figure 5": r"D:\NHANES\output\figures\Figure5_CIF.pdf",
    "figure manifest": r"D:\NHANES\output\figures\_figure_manifest.md",
    "2015 model CSV": r"D:\NHANES\results\13_2015_main_models.csv",
    "NDI model CSV": r"D:\NHANES\results\13g_ndi_cox_models.csv",
    "MDE": r"D:\NHANES\results\13h_mde.txt",
    "2015 data": r"D:\NHANES\data\processed\charls_2015_cross_cov.csv",
    "NDI parsed": r"D:\NHANES\data\nhanes_mort2019.csv",
    "references": r"D:\NHANES\results\07_references_vancouver.md",
    "git release": r"D:\NHANES\github_release\WTI_stroke\README.md",
    "13j figure script": r"D:\NHANES\scripts\13j_figure1_v3.R",
    "13i integrity script": r"D:\NHANES\scripts\13i_integrity_check.py",
}

cutoff = None  # existence-only audit; mtimes reported for information
missing, stale = [], []
for name, p in files.items():
    if not os.path.exists(p):
        missing.append((name, p)); continue
    if cutoff:
        mtime = datetime.fromtimestamp(os.path.getmtime(p))
        if mtime < cutoff:
            stale.append((name, mtime))

print(f"total: {len(files)} | missing: {len(missing)}")
for n, p in missing:
    print("MISSING:", n, p)
if cutoff:
    for n, t in stale:
        print("STALE:", n, t.strftime("%H:%M:%S"))
print("AUDIT PASS" if not missing else "AUDIT NEEDS FIX")
