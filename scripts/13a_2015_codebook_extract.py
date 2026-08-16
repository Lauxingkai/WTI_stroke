# ============================================================================
# 13a_2015_codebook_extract.py
# Opt-1 (CHARLS 2015 replication layer) step A: codebook text extraction +
# dta variable-name/label dump for variable mapping (sex/age/edu + covariates).
# Outputs: data/tmp_2015_codebook.txt, data/tmp_2015_varlabels.txt
# ============================================================================
import os
import pyreadstat
import pandas as pd

BASE = r"D:\NHANES\CHARLS\CHARLS_1725074232_3\CHARLS\2015"
OUT = r"D:\NHANES\data"

pdf_path = os.path.join(BASE, "数据编码参考书.pdf")
txt_path = os.path.join(OUT, "tmp_2015_codebook.txt")

# ---- PDF text extraction (try pymupdf -> pdfplumber -> pypdf) ----
pdf_text = None
try:
    import fitz  # PyMuPDF
    doc = fitz.open(pdf_path)
    pdf_text = "\n\n".join(f"=== PAGE {i+1} ===\n{page.get_text()}" for i, page in enumerate(doc))
    print("PDF extracted with pymupdf:", len(pdf_text), "chars")
except Exception as e:
    print("pymupdf failed:", e)
if pdf_text is None:
    try:
        import pdfplumber
        with pdfplumber.open(pdf_path) as pdf:
            pdf_text = "\n\n".join(f"=== PAGE {i+1} ===\n{(p.extract_text() or '')}" for i, p in enumerate(pdf.pages))
        print("PDF extracted with pdfplumber:", len(pdf_text), "chars")
    except Exception as e:
        print("pdfplumber failed:", e)
if pdf_text is None:
    try:
        from pypdf import PdfReader
        r = PdfReader(pdf_path)
        pdf_text = "\n\n".join(f"=== PAGE {i+1} ===\n{(p.extract_text() or '')}" for i, p in enumerate(r.pages))
        print("PDF extracted with pypdf:", len(pdf_text), "chars")
    except Exception as e:
        print("pypdf failed:", e)

if pdf_text:
    with open(txt_path, "w", encoding="utf-8") as f:
        f.write(pdf_text)
    print("codebook text saved:", txt_path)
else:
    print("WARNING: no PDF backend available; codebook text NOT saved")

# ---- dta variable dump (name + label) for key 2015 files ----
files = {
    "demographic": "Demographic_Background.dta",
    "biomarker": "Biomarker.dta",
    "blood": "Blood.dta",
    "health": "Health_Status_and_Functioning.dta",
    "weights": "Weights.dta",
    "sample": "Sample_Infor.dta",
}

lines = []
for key, fn in files.items():
    path = os.path.join(BASE, fn)
    _, meta = pyreadstat.read_dta(path, metadataonly=True)
    lines.append(f"\n########## {key}: {fn}  (n_vars={meta.number_columns}) ##########")
    for name, lab in zip(meta.column_names, meta.column_labels):
        lab = (lab or "").replace("\n", " ").strip()
        lines.append(f"{name}\t{lab}")

with open(os.path.join(OUT, "tmp_2015_varlabels.txt"), "w", encoding="utf-8") as f:
    f.write("\n".join(lines))
print("varlabels saved:", os.path.join(OUT, "tmp_2015_varlabels.txt"))
print("DONE")
