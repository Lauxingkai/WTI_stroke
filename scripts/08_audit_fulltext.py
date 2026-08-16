# ============================================================================
# 08_audit_fulltext.py  (Phase 6: T23 full-text verification of key citations)
# Fetch Europe PMC fullTextXML for 3 key citations and extract the exact
# numeric claims used in the manuscript (compare abstract vs full-text).
# PMIDs: 29574685 (HTGW HR 1.71), 42444512 (PA HR 0.52), 39487484 (AUC 0.696)
# Output: qc/phase6_fulltext_check.txt
# Date: 2026-08-16
# ============================================================================
import re
import urllib.request
import json

QC = r"D:\NHANES\qc"
TARGETS = {
    "29574685": {"claim": "adjusted HR 1.71 (HTGW -> ischemic stroke)",
                 "patterns": [r"1\.7[0-9]\s*\(", r"hazard ratio", r"HR"]},
    "42444512": {"claim": "HR 0.52 highly active PA; mediated by metabolic indicators",
                 "patterns": [r"0\.5[0-9]\s*\(95", r"HR\s*=\s*0\.5"]},
    "39487484": {"claim": "TyG-WC AUC 0.696 (0.677-0.715)",
                 "patterns": [r"0\.69[0-9]"]},
}

def get_pmcid(pmid):
    q = f'https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=EXT_ID:{pmid}%20AND%20SRC:MED&format=json&resultType=core&pageSize=1'
    with urllib.request.urlopen(q, timeout=40) as r:
        d = json.load(r)
    res = d.get("resultList", {}).get("result", [])
    if not res:
        return None, None
    return res[0].get("pmcid"), res[0]

lines = []
for pmid, spec in TARGETS.items():
    pmcid, meta = get_pmcid(pmid)
    if meta is None:
        lines.append(f"[{pmid}] NOT RESOLVED")
        continue
    lines.append(f"\n=== PMID {pmid} ({meta.get('title','')[:70]}, {meta.get('pubYear')}) | PMCID {pmcid} ===")
    if not pmcid:
        lines.append("  no PMCID (no OA full text); abstract-level claim only")
        abstract = (meta.get("abstractText") or "")[:1500]
        for pat in spec["patterns"]:
            for mm in re.finditer(pat, abstract):
                s = max(0, mm.start() - 120)
                lines.append(f"  ABSTRACT ctx: ...{abstract[s:mm.start()+160]}...")
        continue
    url = f"https://www.ebi.ac.uk/europepmc/webservices/rest/{pmcid}/fullTextXML"
    try:
        with urllib.request.urlopen(url, timeout=60) as r:
            xml = r.read().decode("utf-8", "ignore")
    except Exception as e:
        lines.append(f"  fullTextXML fetch failed: {e}")
        continue
    text = re.sub(r"<[^>]+>", " ", xml)
    text = re.sub(r"\s+", " ", text)
    found = False
    for pat in spec["patterns"]:
        for mm in re.finditer(pat, text):
            s = max(0, mm.start() - 150)
            lines.append(f"  FULLTEXT ctx: ...{text[s:mm.start()+200]}...")
            found = True
    if not found:
        lines.append("  patterns not found in full text; claim needs manual check")
    lines.append(f"  claim under audit: {spec['claim']}")

out = QC + r"\phase6_fulltext_check.txt"
with open(out, "w", encoding="utf-8") as f:
    f.write("\n".join(lines))
print("\n".join(lines[:60]))
print("\nDONE ->", out)
