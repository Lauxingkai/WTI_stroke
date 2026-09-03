# ============================================================================
# 13p_ai_proxy_metrics.py
# AIGC-reduction process metrics (no detector available locally):
#   1) trigger-word/phrase density (AI signature vocabulary)
#   2) sentence-length distribution (mean / SD / CV = burstiness proxy)
# Run BEFORE and AFTER each revision block on manuscript_main.md.
# Output: console + qc/13p_ai_metrics.txt (appended history)
# ============================================================================
import re
import sys
from datetime import datetime

MAN = r"D:\NHANES\manuscript\final\manuscript_main.md"

TRIGGERS = [
    r"\bfurthermore\b", r"\bmoreover\b", r"\badditionally\b", r"\bnotably\b",
    r"\bdelve\b", r"\bunderscore[sd]?\b", r"\bcrucial\b", r"\bpivotal\b",
    r"\bcomprehensive\b", r"\bholistic\b", r"\bmultifaceted\b",
    r"\bseamlessly\b", r"\bleverage[sd]?\b", r"\bharness(ed|ing)?\b",
    r"\bshowcase[sd]?\b", r"\bshed[s]? light\b", r"\btapestry\b",
    r"\bplethora\b", r"\bmyriad\b", r"\bgarner(ed|ing)?\b",
    r"\bplays? (a|an) (pivotal|crucial|important|key|central) role\b",
    r"\bplays? an important role\b", r"\bit is worth noting\b",
    r"\bit should be noted\b", r"\bin summary\b", r"\btaken together\b",
    r"\boverall,?\b", r"\bfirstly\b", r"\bsecondly\b", r"\blastly\b",
    r"\bhas been widely used\b", r"\bwidely used\b",
    r"\bin recent years\b", r"\bfurther research is needed\b",
    r"\bsignificant implications\b", r"\bprovides? (novel|new) insights\b",
    r"\bhighlights? the importance\b", r"\bunderscores? the importance\b",
    r"\brealm\b", r"\blandscape\b", r"\ba wealth of\b",
    r"\brobust\b", r"\bintricate\b", r"\bfacilitates?\b",
]

text = open(MAN, encoding="utf-8").read()
n_words = len(re.findall(r"[A-Za-z0-9'’-]+", text))

hits = {}
total = 0
for pat in TRIGGERS:
    c = len(re.findall(pat, text, flags=re.IGNORECASE))
    if c:
        hits[pat] = c
        total += c

# sentence stats (rough): split on sentence-ending punctuation followed by space+capital
sents = re.split(r"(?<=[.!?])\s+(?=[A-Z(])", text)
lens = [len(re.findall(r"[A-Za-z0-9'’-]+", s)) for s in sents if len(s) > 15]
import numpy as np
lens = np.array(lens)
mean_l, sd_l, cv = lens.mean(), lens.std(), lens.std() / lens.mean()

out = []
out.append(f"=== {datetime.now().strftime('%Y-%m-%d %H:%M:%S')} ===")
out.append(f"words: {n_words} | sentences: {len(lens)}")
out.append(f"trigger hits: {total} | density per 1k words: {1000*total/n_words:.2f}")
out.append("top triggers: " + "; ".join(f"{k}={v}" for k, v in
        sorted(hits.items(), key=lambda x: -x[1])[:15]))
out.append(f"sentence length: mean {mean_l:.1f} | SD {sd_l:.1f} | CV {cv:.3f} "
           f"(burstiness proxy; higher = more human-like)")
print("\n".join(out))
with open(r"D:\NHANES\qc\13p_ai_metrics.txt", "a", encoding="utf-8") as f:
    f.write("\n".join(out) + "\n\n")
