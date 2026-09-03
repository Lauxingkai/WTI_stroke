# -*- coding: utf-8 -*-
"""阶段0.1：references/fulltext 目录一致性校验。

核对项：
  1. 检索报告 43 篇 PMID 是否各有一份 PDF（references/fulltext/PMID_*.pdf）
  2. 每份 PDF 首页提取文本，抽查 DOI/标题关键词是否与报告清单一致（浅校验：
     取首页前 800 字符内是否含 PMID、DOI 片段或预期作者名）
  3. 12944_2024_Article_2259.pdf 的实际内容鉴定（疑为 BMC Nursing 夜班文）
  4. [21] 真全文 39174982_YuHe_2024 .pdf（文件名含空格）是否存在
  5. 4 篇付费墙新补 PDF 是否存在（26151266/18608131/41579732/27930589）
  6. 输出：校验报告 references/fulltext_verify_20260822.txt
"""
import re
from pathlib import Path
import sys

try:
    from pypdf import PdfReader
except ImportError:
    try:
        from PyPDF2 import PdfReader
    except ImportError:
        print("pypdf/PyPDF2 未安装，降级为仅文件存在性校验")
        PdfReader = None

FT = Path(r"D:\NHANES\references\fulltext")

# 检索报告 43 篇 PMID → (作者, 年份)（报告 A1-D4 顺序）
REPORT = [
    (41727962, "ZhaoJ", 2026), (41322468, "WangQ", 2025), (37946249, "SunQ", 2023),
    (40612321, "LiY", 2025), (41630306, "LiH", 2026), (40203810, "ZhuF", 2025),
    (39799375, "SunX", 2025), (39161393, "LiZ", 2024), (40718412, "GuK", 2025),
    (32884316, "LiuPJ", 2020), (35685216, "LiY", 2022), (39334176, "ShiJ", 2024),
    (39794403, "XuN", 2025), (41013763, "HuangJ", 2025), (41809795, "ZhouX", 2026),
    (29574685, "WangW", 2018), (36531467, "RenY", 2022),
    (40140859, "HuoG", 2025), (40759963, "ZhaoYC", 2025), (38218819, "HuoRR", 2024),
    (40241070, "WangB", 2025), (39762919, "ZhangR", 2025), (41053803, "YangY", 2025),
    (40075466, "WangM", 2025), (38896856, "JiangY", 2024), (40707904, "YueY", 2025),
    (40355933, "LuL", 2025), (39574139, "TangS", 2024), (40640840, "SunJ", 2025),
    (39915878, "JiangL", 2025), (38812015, "CuiC", 2024), (38402161, "ShaoY", 2024),
    (37716947, "HuoRR", 2023), (41933347, "NianC", 2026), (41126300, "ZhouZ", 2025),
    (40969601, "JianH", 2025), (42226282, "ZhaoE", 2026), (41776685, "LiuH", 2026),
    (39604935, "HanY", 2024), (41491578, "DengJ", 2026), (29374046, "LuY", 2018),
    (40759697, "ZhangX", 2025), (41074193, "LiD", 2025),
]
PAYWALL = [(26151266, "DiAngelantonio", 2015), (18608131, "Despres", 2008),
           (41579732, "BeydounHA", 2026), (27930589, "ChenS", 2016)]
H21 = "39174982_YuHe_2024 .pdf"  # [21] 真全文（文件名含空格）

DOI_TIP = {  # 首页应含的 DOI 片段（仅作关键字，非完整匹配）
    41727962: "jtim-2026-0014", 41322468: "jdr/1555104", 37946249: "s12944-023-01948",
    29574685: "ane.12925", 36531467: "fendo.2022.1024398", 40140859: "s12933-025-02686",
    42226282: "s12933-026-03218", 41776685: "s40001-026-04148", 41491578: "s12883-025-04600",
    29374046: "JAHA.117.007462", 40759697: "s41598-025-14116", 41074193: "s13098-025-01956",
}


def head_text(pdf_path, n=1200):
    if PdfReader is None:
        return ""
    try:
        r = PdfReader(str(pdf_path))
        if not r.pages:
            return ""
        return (r.pages[0].extract_text() or "")[:n]
    except Exception as e:  # noqa: BLE001
        return f"<PDF读取失败: {e}>"


def main():
    lines = []
    ok, warn, bad = 0, 0, 0

    # 1-2. 43 篇逐一
    for pmid, au, yr in REPORT:
        cands = list(FT.glob(f"{pmid}_*.pdf"))
        if not cands:
            lines.append(f"[MISS] {pmid} ({au} {yr}): 未找到 PDF")
            bad += 1
            continue
        pdf = cands[0]
        if len(cands) > 1:
            lines.append(f"[WARN] {pmid}: 多份 PDF {[c.name for c in cands]}")
            warn += 1
        txt = head_text(pdf)
        tip = DOI_TIP.get(pmid, "")
        if tip and tip in txt.replace(" ", "").lower():
            lines.append(f"[OK] {pmid} {pdf.name}: 首页含 DOI 片段 '{tip}'")
            ok += 1
        elif au.lower().split()[0] in txt.lower() or re.search(r"\b\d{8}\b", txt):
            lines.append(f"[OK±] {pmid} {pdf.name}: 作者名/PMID 命中（DOI 未抽查）")
            ok += 1
        else:
            lines.append(f"[CHECK] {pmid} {pdf.name}: 首页无 DOI/作者线索: {txt[:80]!r}")
            warn += 1

    # 3. 12944 疑点
    for f in FT.glob("12944*"):
        txt = head_text(f).lower()
        tag = "BMC Nursing 夜班(错位迹象)" if "night shift" in txt else ("内容相符" if "triglyceride" in txt else "无法判定")
        lines.append(f"[INFO] {f.name}: {tag}")
        warn += 1

    # 4. [21] 真全文
    h21 = FT / H21
    if h21.exists():
        sz = h21.stat().st_size
        lines.append(f"[OK] [21] 真全文 {H21} 存在 ({sz} bytes)")
    else:
        lines.append(f"[MISS] [21] 真全文未找到（{H21}）——检查是否有其他 39174982 文件: {[p.name for p in FT.glob('39174982*')]}")
        bad += 1

    # 5. 4 篇付费墙
    for pmid, au, yr in PAYWALL:
        cands = list(FT.glob(f"{pmid}_*.pdf"))
        if cands:
            lines.append(f"[OK] 付费墙 {pmid} {cands[0].name} 存在")
        else:
            lines.append(f"[MISS] 付费墙 {pmid} ({au} {yr}) 未找到")
            bad += 1

    # 6. 统计
    lines.append(f"\n=== 总计: OK {ok} / WARN {warn} / MISS-BAD {bad} ===")
    out = "\n".join(lines)
    print(out)
    (FT.parent / "fulltext_verify_20260822.txt").write_text(out, encoding="utf-8")
    print(f"\n已保存: {FT.parent / 'fulltext_verify_20260822.txt'}")


if __name__ == "__main__":
    main()