
import re, pathlib
src = pathlib.Path(r"D:\NHANES\manuscript\final\manuscript_main.md").read_text(encoding="utf-8")
lines = src.splitlines()
ab = []
in_ab = False
for ln in lines:
    if ln.startswith("## Abstract"): in_ab = True; continue
    if in_ab and ln.startswith("## "): break
    if in_ab: ab.append(ln)
def wc(s): return len(s.split())
total = wc(" ".join(ab))
no_tr = wc(" ".join(l for l in ab if not l.startswith("**Trial registration")))
clean = re.sub(r"\*\*|\*", "", " ".join(ab))
clean = re.sub(r"\b(Background|Methods|Results|Conclusions|Trial registration)\b\s*:\s*", "", clean)
print("Abstract 总词数(含全部):", total)
print("Abstract 去 Trial registration 行:", no_tr)
print("Abstract 去全部标签后:", wc(clean))
