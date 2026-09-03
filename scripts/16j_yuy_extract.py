# -*- coding: utf-8 -*-
"""提取 37349808_YuY_2023.pdf 文本（终审区块A核验用）。
输出 references/fulltext/fulltext_txt/37349808.txt
"""
from pathlib import Path

try:
    from pypdf import PdfReader
except ImportError:
    from PyPDF2 import PdfReader

FT = Path(r"D:\NHANES\references\fulltext")
OUT = FT / "fulltext_txt"
OUT.mkdir(exist_ok=True)

src = list(FT.glob("37349808_*.pdf"))[0]
dst = OUT / "37349808.txt"
reader = PdfReader(str(src))
parts = []
for i, page in enumerate(reader.pages):
    t = page.extract_text() or ""
    parts.append(f"--- PAGE {i+1} ---\n{t}")
dst.write_text("\n".join(parts), encoding="utf-8")
print(dst, dst.stat().st_size)