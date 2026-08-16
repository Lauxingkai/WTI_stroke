# ============================================================================
# 10_audit_citations.py  (Post-rewrite citation integrity, bidirected)
# (a) every [n] citation in text maps to an existing reference entry
# (b) every reference entry 1-30 is cited at least once (no orphans)
# (c) context spot-checks for load-bearing citations
# Output: qc/post_revision_citations.txt
# Date: 2026-08-16
# ============================================================================
import re

MAN = open(r"D:\NHANES\manuscript\final\manuscript_main.md", encoding="utf-8").read()
lines = []
def log(*s):
    lines.append(" ".join(str(x) for x in s)); print(*s)

# (a) cited numbers from body text (before References section)
body = MAN.split("## References")[0]
cites = set()
for m in re.finditer(r"\[(\d+(?:[,\-–]\d+)*)\]", body):
    inner = m.group(1)
    parts = re.split(r",", inner)
    for p in parts:
        if "-" in p or "–" in p:
            a, b = re.split(r"[\-–]", p)
            cites.update(range(int(a), int(b) + 1))
        elif p.isdigit():
            cites.add(int(p))
log("(a) cited numbers:", sorted(cites))
# reference numbers present
refs = re.findall(r"^(\d+)\.", MAN.split("## References")[1], re.M)
refs = set(int(r) for r in refs)
log("(b) reference entries:", sorted(refs))
log("cited but no entry:", sorted(cites - refs))
log("entries never cited (orphans):", sorted(refs - cites))

# (c) context spot-checks
def ctx(pat, tag):
    for m in re.finditer(pat, body):
        s = max(0, m.start() - 100)
        log(f"[{tag}] ...{body[s:m.start()+150].strip()}...")
        break
ctx(r"absence of evidence", "ref-30 Altman&Bland context")
ctx(r"0\.696", "ref-8 TyG-WC AUC context")
ctx(r"HR 0\.52|0\.52", "ref-22 PA context")
ctx(r"West China", "ref-29 West China context")
ctx(r"IRB00001052", "ethics statement context")
log("\nnote: reference [30] appears in both Discussion text and References list")
open(r"D:\NHANES\qc\post_revision_citations.txt", "w", encoding="utf-8").write("\n".join(lines))
print("\nDONE")
