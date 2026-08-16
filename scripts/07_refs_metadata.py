# ============================================================================
# 07_refs_metadata.py
# Fetch Vancouver metadata (authors/title/journal/year/volume/issue/pages/DOI)
# from Europe PMC for the manuscript reference list, in order of first
# appearance. Output: results/07_references_vancouver.md
# Date: 2026-08-15
# ============================================================================
import json
import time
import urllib.parse
import urllib.request

# order of first appearance across Background/Methods/Results/Discussion
PMIDS = [
    "39866113",   # 1  AHA 2025 stroke statistics (Background)
    "10889128",   # 2  Lemieux 2000 HTGW
    "29574685",   # 3  HTGW x ischemic stroke, rural China
    "37850101",   # 4  HTGW stroke central China
    "36531467",   # 5  HTGW x acute stroke severity
    "39914644",   # 6  TyG-WC CHARLS non-diabetic
    "41466258",   # 7  TyG-WHtR x stroke CHARLS
    "39487484",   # 8  TyG-WC AUC 0.696 CHARLS
    "41491578",   # 9  eGDR x BMI x stroke dual-cohort paradigm
    "40203810",   # 10 WTI computable in CHARLS
    "18064739",   # 11 STROBE
    "27122601",   # 12 Nordestgaard fasting not required
    "19433651",   # 13 self-reported stroke validity 2009
    "34428763",   # 14 Strong Heart self-report validity 2021
    "28693043",   # 15 E-value
    "26653405",   # 16 VanderWeele mediation
    "17569110",   # 17 Pencina NRI/IDI 2008
    "26858290",   # 18 Austin competing risks
    "24592497",   # 19 Leening NRI critique
    "24240655",   # 20 Kerr NRI critique
    "39174982",   # 21 TyG-WC nonlinear CHARLS
    "42444512",   # 22 PA x stroke CHARLS mediation
    "41914377",   # 23 MVPA x stroke CHARLS
    "39762919",   # 24 TyG+ABSI AUC 0.579
    "41776685",   # 25 TyG-ABSI dual-cohort AUC
    "38812015",   # 26 TyG C-index > TyG-WC
    "41265733",   # 27 dynamic WC change vs indices
    "31795614",   # 28 HEXA self-report PPV
]

HEADERS = {"User-Agent": "Mozilla/5.0 (research-script; contact: local)"}

def fetch(pmid):
    q = urllib.parse.quote(f"EXT_ID:{pmid} AND SRC:MED")
    url = f"https://www.ebi.ac.uk/europepmc/webservices/rest/search?query={q}&format=json&resultType=core&pageSize=1"
    with urllib.request.urlopen(urllib.request.Request(url, headers=HEADERS), timeout=40) as r:
        d = json.load(r)
    res = d.get("resultList", {}).get("result", [])
    return res[0] if res else None

def vancouver(i, x):
    auth = x.get("authorString", "") or ""
    authors = [a.strip() for a in auth.split(",") if a.strip()]
    if len(authors) > 6:
        authors = authors[:6] + ["et al"]
    title = (x.get("title") or "").rstrip(".")
    j = x.get("journalInfo", {}) or {}
    jtitle = (j.get("journal", {}) or {}).get("title", "") or x.get("journalTitle", "")
    vol = j.get("volume", "")
    iss = j.get("issue", "")
    pages = x.get("pageInfo", "") or ""
    doi = x.get("doi", "") or ""
    out = f"{i}. " + ", ".join(authors) + f". {title}. {jtitle}. {x.get('pubYear','')}"
    if vol:
        out += f";{vol}"
        if iss:
            out += f"({iss})"
    if pages:
        out += f":{pages}"
    out += "."
    if doi:
        out += f" doi:{doi}."
    return out

lines = []
missing = []
for i, pmid in enumerate(PMIDS, 1):
    x = fetch(pmid)
    if x is None:
        missing.append(pmid)
        lines.append(f"{i}. [METADATA FETCH FAILED for PMID {pmid}]")
        print(f"MISS {pmid}")
    else:
        lines.append(vancouver(i, x))
        print(f"OK {i} {pmid}: {x.get('journalTitle','')} {x.get('pubYear','')}")
    time.sleep(0.35)

with open(r"D:\NHANES\results\07_references_vancouver.md", "w", encoding="utf-8") as f:
    f.write("# References (Vancouver, auto-generated 2026-08-15 from Europe PMC)\n\n")
    f.write("\n".join(lines))
    f.write("\n\n## Notes\n")
    f.write(f"Missing/failed: {missing if missing else 'none'}\n")
    f.write("Manual entry to append: Liu R, Wang L, Li L, Wei Q. [TyG-WC index for predicting stroke risk in middle-aged and older adults]. West China Medical Journal. 2023;38(5):656-662. (Chinese; cited for directional conclusion only, see docs/40)\n")
print("DONE; missing:", missing)
