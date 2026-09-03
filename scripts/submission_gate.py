#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
submission_gate.py — 投稿前组合检查（通用层，跨项目复用）

串起三件通用检查，一条命令出投稿闸报告：
  1) 投稿包存在性审计（主稿/图/表/补充材料/cover letter/注册文件）
  2) 引用双向核验（正文 [n] ↔ References 条目，无幽灵号/无缺号）
  3) claim 登记卡检查（复用 claim_register/check_claim_register.py，--register 提供时）

用法:
  python scripts/submission_gate.py \
    --draft manuscript/final/manuscript_main.md \
    --package manuscript/final \
    --register claim_register/claim_register_WTI.json \
    --required manuscript_main_lhd.docx supplementary.docx tables_submission.docx cover_letter.md

  # 项目特定深度审计（如 13i 数字 vs 审计件）不在本层，由 --integrity <script> 显式挂接
  python scripts/submission_gate.py --draft ... --integrity scripts/13i_integrity_check.py

纯 stdlib；Windows 下 set PYTHONUTF8=1。任一 critical 项 → exit 1。
"""
import argparse
import json
import os
import re
import subprocess
import sys
from pathlib import Path

FINDINGS = []  # (level, item, detail)


def critical(item, detail):
    FINDINGS.append(("CRITICAL", item, detail))


def warn(item, detail):
    FINDINGS.append(("WARN", item, detail))


def ok(item, detail):
    FINDINGS.append(("OK", item, detail))


def load_utf8(path: str) -> str:
    return Path(path).read_text(encoding="utf-8")


def check_package(draft_dir: str, required: list, extra_optional: list):
    """投稿包存在性审计。required 缺失=critical；extra_optional 缺失=warn（按项目可选）。"""
    d = Path(draft_dir)
    if not d.is_dir():
        critical("package-dir", f"投稿包目录不存在: {d}")
        return
    for name in required:
        p = d / name
        if p.exists():
            ok(f"package:{name}", f"{p.stat().st_size} bytes")
        else:
            critical(f"package:{name}", f"缺失: {p}")
    # 图目录：存在且至少一个图文件
    fig_dir = d / "figures"
    if fig_dir.is_dir():
        n_figs = len(list(fig_dir.iterdir()))
        if n_figs:
            ok("package:figures", f"{n_figs} 个图文件（{fig_dir.name}/）")
        else:
            warn("package:figures", "figures/ 目录为空")
    else:
        warn("package:figures", "未发现 figures/ 目录（如投稿不要求可忽略）")
    for name in extra_optional:
        if (d / name).exists():
            ok(f"package-opt:{name}", "存在")
        else:
            warn(f"package-opt:{name}", f"可选文件缺失（{name}）— 按项目要求判定")


def check_citations(draft_text: str, refs_start_marker="References"):
    """引用双向核验：正文 [n] 解析 vs References 条目数。"""
    # 正文引用号（含 [1-3,5] 区间）
    cited = set()
    for grp in re.findall(r"\[(\d+(?:[-,]\d+)*)\]", draft_text):
        for part in grp.split(","):
            part = part.strip()
            if "-" in part:
                a, b = part.split("-")
                try:
                    cited.update(range(int(a), int(b) + 1))
                except ValueError:
                    warn("citation:parse", f"无法解析引用区间 {part}")
            elif part.isdigit():
                cited.add(int(part))
    if not cited:
        warn("citation:empty", "正文未发现任何 [n] 引用")
        return
    # References 段条目数（兼容 [n] 和 "n." 两种格式）
    m = re.search(r"(?im)^#+\s*" + re.escape(refs_start_marker) + r"\s*$", draft_text)
    if m:
        refs_text = draft_text[m.end():]
        n_bracket = len(re.findall(r"(?m)^\[\d+\]\s*\.?\s*", refs_text))
        n_dot = len(re.findall(r"(?m)^\d+\.\s+(?:[A-Za-z\u4e00-\u9fff])", refs_text))
        n_refs = max(n_bracket, n_dot)
    else:
        # 兜底：数行首引用格式
        n_refs = max(len(re.findall(r"(?m)^\[\d+\][\s.\-]", draft_text)),
                     len(re.findall(r"(?m)^\d+\.\s+(?:[A-Za-z\u4e00-\u9fff])", draft_text)))
        warn("citation:section", f"未定位 'References' 标题段，用行首引用计数兜底（n={n_refs}）")
    ghost = sorted(c for c in cited if c < 1 or c > n_refs)
    missing = sorted(i for i in range(1, n_refs + 1) if i not in cited)
    ok("citation:span", f"正文引用 {len(cited)} 个不同编号，References 段条目 {n_refs}")
    if ghost:
        critical("citation:ghost", f"幽灵引用号（正文出现但 References 无此编号）: {ghost}")
    else:
        ok("citation:ghost", "无幽灵号")
    if missing:
        warn("citation:uncited", f"References 有 {len(missing)} 条未被正文引用: {missing[:10]}{'...' if len(missing) > 10 else ''}")
    else:
        ok("citation:uncited", "无未被引用的条目")


def check_claim_register(register: str, draft: str):
    """联动 claim 登记卡检查器（复用，不重实现）。"""
    checker = Path(__file__).resolve().parent / ".." / "claim_register" / "check_claim_register.py"
    if not checker.exists():
        warn("claim", f"未找到登记卡检查器 {checker}（跳过）")
        return
    r = subprocess.run([sys.executable, str(checker), "--register", register, "--draft", draft],
                       capture_output=True, text=True, encoding="utf-8")
    out = (r.stdout or "") + (r.stderr or "")
    if r.returncode == 0:
        ok("claim", "登记卡与草稿一致（无 critical）")
    else:
        critical("claim", f"claim 登记卡检查未通过（exit {r.returncode}）:\n{out[-2000:]}")


def run_integrity(script: str, draft_dir: str):
    """可选：挂接项目特定深度审计（如任务A 13i 数字 vs 审计件）。

    双重判定：脚本退出码非零 → CRITICAL；退出码为零但输出含 FAIL 标记
    （写报告型脚本不设退出码的常见形态）→ WARN 并附明细，提示人工核验
    "断言过时 vs 真实不一致"。不静默放行，也不替用户定性。
    """
    p = Path(script)
    if not p.exists():
        warn("integrity", f"--integrity 脚本不存在: {p}")
        return
    r = subprocess.run([sys.executable, str(p)], capture_output=True, text=True, encoding="utf-8")
    out = (r.stdout or "") + (r.stderr or "")
    if r.returncode != 0:
        critical("integrity", f"{p.name} 退出码非零（exit {r.returncode}）:\n{out[-1500:]}")
        return
    # 提取 FAIL 标记及其上文（项目说明行），让 WARN 可操作而非孤立标记
    lines = out.splitlines()
    fail_blocks = []
    for i, ln in enumerate(lines):
        if "FAIL" in ln or "-> FAIL" in ln:
            ctx = lines[i - 1] if i > 0 else ""
            fail_blocks.append((ctx.strip(), ln.strip()))
    if fail_blocks:
        detail = "\n".join(f"  {ctx}\n  -> {flag}" for ctx, flag in fail_blocks[:10])
        warn("integrity",
             f"{p.name} 输出含 FAIL 标记（{len(fail_blocks)} 处）——需人工核验是断言过时"
             f"还是稿件与审计件不一致：\n{detail}\n（退出码为 0，未自动阻断）")
    else:
        ok("integrity", f"{p.name} 通过（exit 0，无 FAIL 标记）")


def main():
    ap = argparse.ArgumentParser(description="投稿前组合检查（通用层）")
    ap.add_argument("--draft", required=True, help="主稿 md 路径")
    ap.add_argument("--package", default=None, help="投稿包目录（默认取草稿所在目录）")
    ap.add_argument("--register", default=None, help="claim 登记卡 JSON（可选）")
    ap.add_argument("--required", nargs="*", default=[], help="投稿必备文件相对 package 的文件名")
    ap.add_argument("--optional", nargs="*", default=[], help="可选文件（缺失仅 warn）")
    ap.add_argument("--integrity", default=None, help="项目特定深度审计脚本（如 scripts/13i_integrity_check.py）")
    args = ap.parse_args()

    draft_path = Path(args.draft)
    if not draft_path.exists():
        print(f"[CRITICAL] 草稿不存在: {draft_path}")
        return 1
    draft_text = load_utf8(str(draft_path))
    package_dir = args.package or str(draft_path.parent)

    # 1) 投稿包存在性
    check_package(package_dir, args.required, args.optional)
    # 2) 引用双向核验
    check_citations(draft_text)
    # 3) claim 登记卡（可选）
    if args.register:
        check_claim_register(args.register, str(draft_path))
    # 4) 项目特定深度审计（可选）
    if args.integrity:
        run_integrity(args.integrity, package_dir)

    # 汇总
    print("=" * 62)
    print("Submission Gate — 投稿前组合检查报告")
    print("=" * 62)
    n_crit = n_warn = n_ok = 0
    for level, item, detail in FINDINGS:
        if level == "CRITICAL":
            n_crit += 1
            print(f"  [CRITICAL] {item}: {detail}")
        elif level == "WARN":
            n_warn += 1
            print(f"  [WARN] {item}: {detail}")
        else:
            n_ok += 1
    print("-" * 62)
    print(f"OK={n_ok}  WARN={n_warn}  CRITICAL={n_crit}")
    print("边界：本层做存在性/引用计数/claim 一致性；数字对审计件等深度验证由 --integrity 挂接的")
    print("      项目特定脚本（如 13i）负责。真伪终判仍需人/审稿人。")
    return 1 if n_crit else 0


if __name__ == "__main__":
    sys.exit(main())