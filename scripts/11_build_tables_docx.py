# ============================================================================
# 11_build_tables_docx.py  (Task A prep: journal-ready three-line tables DOCX)
# Builds Table 2/3/4 (+Table 1 summaries) as a submission-ready DOCX using
# python-docx, from the audited CSVs (no hand-typed numbers).
# Output: manuscript/final/tables_submission.docx
# Date: 2026-08-16
# ============================================================================
import pandas as pd
from docx import Document
from docx.shared import Pt, Cm
from docx.enum.text import WD_ALIGN_PARAGRAPH

RES = r"D:\NHANES\results"
OUT = r"D:\NHANES\manuscript\final\tables_submission.docx"

doc = Document()
style = doc.styles["Normal"]
style.font.name = "Times New Roman"
style.font.size = Pt(9)

def add_table(title, df, footnote):
    h = doc.add_paragraph()
    r = h.add_run(title); r.bold = True; r.font.size = Pt(9)
    t = doc.add_table(rows=1, cols=len(df.columns))
    t.style = "Table Grid"
    for j, c in enumerate(df.columns):
        cell = t.rows[0].cells[j]
        cell.text = str(c)
        for p in cell.paragraphs:
            p.runs[0].bold = True; p.runs[0].font.size = Pt(8)
    for _, row in df.iterrows():
        cells = t.add_row().cells
        for j, v in enumerate(row):
            cells[j].text = str(v)
            for p in cells[j].paragraphs:
                p.runs[0].font.size = Pt(8)
    f = doc.add_paragraph()
    fr = f.add_run(footnote); fr.font.size = Pt(8)
    doc.add_paragraph()

t2 = pd.read_csv(RES + r"\Table2_main_models.csv")
add_table("Table 2. Main associations of WTI with stroke (per 1-SD), by cohort and model",
          t2, "OR: survey-weighted logistic (quasibinomial), M1 = age/sex, M2 = +race(NHANES)/education/smoking/drinking/BMI, "
              "M3 = +hypertension/diabetes/lipid-lowering/antihypertensive medication/physical activity. "
              "HR: weighted Cox (cluster = community); sHR: Fine-Gray subdistribution. CHARLS prospective n = 9,036 (516 events, 107 deaths); "
              "NHANES n = 10,302 (531 events).")

t3 = pd.read_csv(RES + r"\Table3_discrimination.csv")
add_table("Table 3. Discrimination of seven indices for stroke (base model: age + sex)",
          t3, "AUC: unweighted complete-case logistic predictions, DeLong CI; NRI/IDI: continuous, bootstrap percentile CIs (B = 1000). "
              "Exploratory; unweighted-calculation caveat in Limitations.")

t4 = pd.read_csv(RES + r"\Table4_sensitivity.csv")
add_table("Table 4. Sensitivity and mediation analyses",
          t4, "E-value per VanderWeele & Ding (rare-outcome approximation); Lag-2 landmark excludes events/deaths within 2 y "
              "(n = 8,965, 492 strokes); interval-censored discrete-time person-period model (3 intervals); "
              "mediation: product-of-coefficients, individual-resampling bootstrap (B = 1000), 2011 WTI -> 2015 mediator -> 2018 stroke.")

# ---- Table 5: CHARLS 2015 replication + NHANES NDI prospective layers ----
r15 = pd.read_csv(RES + r"\13_2015_main_models.csv")
rn  = pd.read_csv(RES + r"\13g_ndi_cox_models.csv")

def fmt(row):
    return f"{row.est:.3f} ({row.lo:.3f}-{row.hi:.3f})"

rows5 = []
def row5(layer, model, key, src="r15", unit="OR"):
    r = r15 if src == "r15" else rn
    s = r[(r.layer == key) & (r.model == model)].iloc[0]
    rows5.append([layer, model, fmt(s), f"{s.p:.4f}", f"{int(s.n)}", f"{int(s.events)}"])

row5("CHARLS 2015 cross", "CM1", "cross")
row5("CHARLS 2015 cross", "CM2", "cross")
row5("CHARLS 2015 cross", "CM3", "cross")
row5("CHARLS 2015 cross, alt weight", "CA3", "cross-altw")
row5("CHARLS 2015 cross, physician-confirmed", "CP1", "cross-phys")
row5("NHANES NDI all-cause", "AM1", "all-cause", src="rn", unit="HR")
row5("NHANES NDI all-cause", "AM3", "all-cause", src="rn", unit="HR")
row5("NHANES NDI stroke death", "SM1", "stroke-death", src="rn", unit="HR")
row5("NHANES NDI stroke death", "SM3", "stroke-death", src="rn", unit="HR")
row5("NHANES NDI stroke death, Fine-Gray", "M3", "stroke-death-FG", src="rn", unit="sHR")

t5 = pd.DataFrame(rows5, columns=["Layer", "Model", "OR/HR/sHR (95% CI)", "P", "n", "events"])
add_table("Table 5. Replication (CHARLS 2015) and prospective mortality (NHANES NDI) associations of WTI with stroke and death (per 1-SD)",
          t5, "CHARLS 2015: survey-weighted logistic (quasibinomial), design = community cluster + urban/rural strata, "
              "weight = 2015 blood weight normalized; M1 age/sex, M2 + education/smoking/drinking/BMI, "
              "M3 + hypertension/diabetes/lipid-lowering/antihypertensive medication/physical activity. "
              "Physician-confirmed stroke: doctor-told verification records (33 events). "
              "NHANES NDI: survey-weighted cause-specific Cox (cluster = cycle-specific PSU, strata likewise, pooled weights), "
              "follow-up through Dec 31, 2019 (median 6.9 y; 74,744 person-years); stroke death = underlying cause I60-I69. "
              "Fine-Gray: unweighted, other deaths as competing events. Full row sets in Supplementary Tables S4-S5.")

doc.save(OUT)
print("DONE ->", OUT)
