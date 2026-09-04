# -*- coding: utf-8 -*-
"""Post-process manuscript_main.docx: double line spacing + continuous line numbering (BMC LHD requirement)."""
import docx
from docx.shared import Pt
from docx.oxml.ns import qn
from docx.oxml import OxmlElement

SRC = r"D:\NHANES\manuscript\final\manuscript_main.docx"

doc = docx.Document(SRC)

# 1. Normal style: double spacing for all paragraphs using Normal (pandoc BodyText maps to Normal in default template? set both)
for style_name in ("Normal", "BodyText"):
    try:
        st = doc.styles[style_name]
        st.paragraph_format.line_spacing = 2.0
    except KeyError:
        pass

# 2. Walk all paragraphs: set double spacing explicitly
for p in doc.paragraphs:
    pf = p.paragraph_format
    pf.line_spacing = 2.0
    pf.space_before = Pt(0)
    pf.space_after = Pt(0)

# 3. Line numbering in section properties (continuous)
sect = doc.sections[0]
sectPr = sect._sectPr
# remove existing lnNumType if any
for el in sectPr.findall(qn('w:lnNumType')):
    sectPr.remove(el)
ln = OxmlElement('w:lnNumType')
ln.set(qn('w:countBy'), '1')
ln.set(qn('w:restart'), 'continuous')
sectPr.append(ln)

doc.save(r"D:\NHANES\manuscript\final\manuscript_main_lhd.docx")
print("saved manuscript_main_lhd.docx")