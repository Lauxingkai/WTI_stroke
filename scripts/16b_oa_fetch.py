# -*- coding: utf-8 -*-
"""G1：14 条无全文支撑文献中 10 条 OA 文献的全文补取（付费墙 4 条已由作者手动补入）。

目标 PMID（来自 WTI_结论支撑文献_20260822.md）：
  39832145 Kachutova 2024  | 41199862 GuoY 2025 | 39319466 YangM 2024  | 37349808 YuY 2023
  38184598 DangK 2024      | 38555461 ZhaoY 2024 | 36797706 CheB 2023   | 38017519 CuiC 2023
  39780176 HuangY 2025     | 41972221 Sarebanhassanabadi 2026

管线（沿 08-22 报告协议）：Europe PMC search 找 PMCID → fullTextPDF 下载；
fallback：出版社直链（Springer/BMC: link.springer.com/content/pdf/<doi>.pdf；
Frontiers: frontiersin.org <doi>/pdf；其余跳过并报告）。
间隔 12-20s 规避限流；%PDF 校验；命名 PMID_Author_Year.pdf。
"""
import json
import random
import re
import time
import urllib.request
import urllib.parse
from pathlib import Path

FT = Path(r"D:\NHANES\references\fulltext")
TARGETS = [
    (39832145, "Kachutova", 2024),
    (41199862, "GuoY", 2025),
    (39319466, "YangM", 2024),
    (37349808, "YuY", 2023),
    (38184598, "DangK", 2024),
    (38555461, "ZhaoY", 2024),
    (36797706, "CheB", 2023),
    (38017519, "CuiC", 2023),
    (39780176, "HuangY", 2025),
    (41972221, "Sarebanhassanabadi", 2026),
]
HEADERS = {"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) research-script/1.0",
           "Accept": "application/pdf,text/html,*/*"}


def get(url, binary=False, timeout=60):
    req = urllib.request.Request(url, headers=HEADERS)
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return r.read() if binary else r.read().decode("utf-8", "replace")


def find_pmc(pmid):
    q = urllib.parse.quote(f'EXT_ID:{pmid}')
    u = f"https://www.ebi.ac.uk/europepmc/webservices/rest/search?query={q}&format=json&pageSize=1"
    try:
        d = json.loads(get(u))
        hits = (d.get("resultList") or {}).get("result") or []
        if hits:
            h = hits[0]
            return h.get("pmcid"), h.get("doi", ""), h.get("isOpenAccess")
    except Exception as e:  # noqa: BLE001
        print(f"  EPMC search err {pmid}: {e}")
    return None, "", False


def download(pmid, author, year):
    existing = list(FT.glob(f"{pmid}_*.pdf"))
    if existing:
        print(f"[SKIP] {pmid}: 已存在 {existing[0].name}")
        return "skip"
    pmcid, doi, oa = find_pmc(pmid)
    out = FT / f"{pmid}_{author}_{year}.pdf"
    # 1) Europe PMC fullTextPDF
    if pmcid:
        try:
            data = get(f"https://www.ebi.ac.uk/europepmc/webservices/rest/{pmcid}/fullTextPDF?redirect=false",
                       binary=True)
            if data[:4] == b"%PDF":
                out.write_bytes(data)
                print(f"[OK-EPMC ] {pmid} -> {out.name} ({len(data)} B, {pmcid})")
                return "ok"
            print(f"  {pmid}: EPMC PDF 非 %PDF（{data[:40]!r}），换 fallback")
        except Exception as e:  # noqa: BLE001
            print(f"  {pmid}: EPMC fullTextPDF err {e}")
    # 2) 出版社直链
    if doi:
        cands = []
        if doi.startswith("10.1186/"):
            cands.append(f"https://link.springer.com/content/pdf/{doi}.pdf")
        elif doi.startswith("10.3389/"):
            cands.append(f"https://www.frontiersin.org/journals/endocrinology/articles/{doi}/pdf"
                         if "endocrinology" in doi else f"https://www.frontiersin.org{doi.replace('10.3389/','/journals/')}/pdf")
        elif doi.startswith("10.1161/"):
            cands.append(f"https://www.ahajournals.org/doi/pdf/{doi}")
        elif doi.startswith("10.48305/"):
            # ARYA Atheroscler：官网链接模式待核实
            pass
        for c in cands:
            try:
                data = get(c, binary=True)
                if data[:4] == b"%PDF":
                    out.write_bytes(data)
                    print(f"[OK-PUB  ] {pmid} -> {out.name} ({len(data)} B, {doi})")
                    return "ok"
                print(f"  {pmid}: PUB 非 %PDF @ {c}")
            except Exception as e:  # noqa: BLE001
                print(f"  {pmid}: PUB err {c}: {e}")
    print(f"[FAIL] {pmid} ({author} {year}): 无可用全文源 (pmcid={pmcid}, doi={doi}, oa={oa})")
    return "fail"


def main():
    results = {"ok": [], "skip": [], "fail": []}
    for i, (pmid, author, year) in enumerate(TARGETS):
        r = download(pmid, author, year)
        results[r].append(pmid)
        if i < len(TARGETS) - 1:
            s = random.randint(12, 20)
            print(f"  ...sleep {s}s")
            time.sleep(s)
    print("\n=== G1 结果: OK %d / SKIP %d / FAIL %d ===" % (len(results["ok"]), len(results["skip"]), len(results["fail"])))
    print("OK:", results["ok"])
    print("FAIL:", results["fail"])


if __name__ == "__main__":
    main()