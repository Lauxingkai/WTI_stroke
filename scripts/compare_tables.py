# -*- coding: utf-8 -*-
from docx import Document
import re

def norm(s):
    return re.sub(r"\s+", " ", (s or "")).strip()

def dump_tables(path):
    doc = Document(path)
    res = []
    for ti, t in enumerate(doc.tables):
        rows = []
        for r in t.rows:
            rows.append([norm(c.text) for c in r.cells])
        res.append({"index": ti, "n_rows": len(rows), "n_cols": len(rows[0]) if rows else 0, "rows": rows})
    return res

check = dump_tables(r"D:\NHANES\manuscript\final\BHV15049_WTI_NHANES&CHARLS.docx")
sub = dump_tables(r"D:\NHANES\manuscript\final\tables_submission.docx")

print("投稿版表格数:", len(sub), "| 查重版表格数:", len(check))
for ci in range(max(len(check), len(sub))):
    c = check[ci] if ci < len(check) else None
    s = sub[ci] if ci < len(sub) else None
    if c is None or s is None:
        print(f"表{ci+1}: 缺失（查重版={'无' if c is None else '有'} / 投稿版={'无' if s is None else '有'}）"); continue
    same_shape = c["n_rows"] == s["n_rows"] and c["n_cols"] == s["n_cols"]
    diffs = []
    for i in range(min(len(c["rows"]), len(s["rows"]))):
        for j in range(min(len(c["rows"][i]), len(s["rows"][i]))):
            if c["rows"][i][j] != s["rows"][i][j]:
                diffs.append((i, j, s["rows"][i][j][:40], c["rows"][i][j][:40]))
    print(f"表{ci+1}: 查重版 {c['n_rows']}x{c['n_cols']} vs 投稿版 {s['n_rows']}x{s['n_cols']} | 形状一致={same_shape} | 差异格数={len(diffs)}")
    for d in diffs[:6]:
        print(f"   格({d[0]},{d[1]}): 投稿版={d[2]!r} vs 查重版={d[3]!r}")
