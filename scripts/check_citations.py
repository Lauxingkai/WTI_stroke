# -*- coding: utf-8 -*-
"""Citations vs reference list: accurate range/list resolution.
(2026-08-25 v2: [11-13] expands to 11,12,13; [3,4] -> 3,4.
v1 only counted the first number of range citations -> 45/48 misreport.)
"""
import re, pathlib
from collections import Counter

CUR = pathlib.Path(r"D:\NHANES\manuscript\final\manuscript_main.md").read_text(encoding="utf-8")
BAK = pathlib.Path(r"D:\NHANES\manuscript\final\backup_20260825_discussionfix\manuscript_main.md").read_text(encoding="utf-8")
LINES = CUR.splitlines()
REF = next(i for i, ln in enumerate(LINES) if ln.startswith("## References"))
BODY = " ".join(LINES[:REF])

def cited_set(txt):
    out = set()
    for m in re.finditer(r"\[(\d{1,2}(?:[,\-]\d{1,2})*)\]", txt):
        for part in m.group(1).split(","):
            part = part.strip()
            if "-" in part:
                a, b = part.split("-")
                out.update(range(int(a), int(b) + 1))
            elif part:
                out.add(int(part))
    return out

cb = cited_set(BODY)
print("正文显式引用编号:", len(cb), "| 覆盖范围:", (min(cb), max(cb)))
print("缺失(表中有、正文未引):", sorted(set(range(1, 49)) - cb))
print("多余(正文引、表中无):", sorted(cb - set(range(1, 49))))
if "backup" in str(BAK):
    bb = cited_set(" ".join(BAK.splitlines()[:REF]))
    print("与备份版对比：无丢失:", cb >= bb, "| 无新增:", cb <= bb)
