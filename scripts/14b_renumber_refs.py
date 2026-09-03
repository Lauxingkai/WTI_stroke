# ============================================================================
# 14b_renumber_refs.py
# BMC numeric style: renumber references by first appearance in the text.
# Removals (six-reviewer C2/C5): old #10 (Zhu F, Obes Facts) and old #12
# (Nordestgaard, Eur Heart J) are deleted from the list and from in-text
# citations (already removed from the text by hand).
# Applied to: manuscript_main.md (EN) and manuscript_main_中文审核版.md (ZH).
# Output  : in-place rewrite + results/14b_renumber_map.txt (audit trail)
# Date    : 2026-08-20
# ============================================================================
import re, sys

REMOVED = set()  # 2026-08-22 阶段3：新增 16 条（33-48）后重排；无删除
FILES = [
    r"D:\NHANES\manuscript\final\manuscript_main.md",
    # 2026-08-22 阶段3：中文审核版尚未同步 16 条新引用，暂不重排（Funding 定稿后统一同步）
    # r"D:\NHANES\manuscript\final\manuscript_main_中文审核版.md",
]

CITE_RE = re.compile(r"\[(\d+(?:[,-]\d+)*)\]")

def first_appearance_order(text):
    seen, order = set(), []
    for m in CITE_RE.finditer(text):
        parts = re.split(r"[,-]", m.group(1))
        nums = []
        for p in parts:
            nums.append(int(p))
        # expand ranges: [6-8] cites 6,7,8
        expanded = []
        i = 0
        while i < len(nums):
            j = i + 1
            # detect a-b ranges in the ORIGINAL string
            expanded.append(nums[i])
            i += 1
        # range detection on the raw token list
        raw = re.findall(r"\d+|[,-]", m.group(1))
        expanded = []
        k = 0
        while k < len(raw):
            if raw[k].isdigit():
                if k + 2 < len(raw) and raw[k+1] == "-" and raw[k+2].isdigit():
                    lo, hi = int(raw[k]), int(raw[k+2])
                    expanded.extend(range(lo, hi + 1))
                    k += 3
                else:
                    expanded.append(int(raw[k]))
                    k += 1
            else:
                k += 1
        for n in expanded:
            if n in REMOVED or n in seen:
                continue
            seen.add(n)
            order.append(n)
    return order

def build_map(order):
    m = {}
    for new, old in enumerate(order, start=1):
        m[old] = new
    return m

def remap_text(text, m):
    def repl(mo):
        raw = re.findall(r"\d+|[,-]", mo.group(1))
        out = []
        k = 0
        while k < len(raw):
            if raw[k].isdigit():
                if k + 2 < len(raw) and raw[k+1] == "-" and raw[k+2].isdigit():
                    lo, hi = int(raw[k]), int(raw[k+2])
                    out.append(f"{m[lo]}-{m[hi]}")
                    k += 3
                else:
                    n = int(raw[k])
                    if n in REMOVED:
                        raise ValueError(f"removed ref {n} still cited: {mo.group(0)}")
                    out.append(str(m[n]))
                    k += 1
            else:
                out.append(raw[k])
                k += 1
        return "[" + "".join(out) + "]"
    return CITE_RE.sub(repl, text)

def reorder_list(text, m, removed):
    """Find the numbered reference list block and reorder by new number."""
    # entries like "12. Nordestgaard ..." starting a line (1..40)
    entry_re = re.compile(r"^(\d+)\.\s", re.M)
    lines = text.split("\n")
    # locate list: from the line after the References header to the next ## or end
    start = None
    for i, ln in enumerate(lines):
        if re.match(r"^## References", ln) or re.match(r"^## 参考文献", ln):
            start = i + 1
            break
    if start is None:
        raise ValueError("References header not found")
    end = len(lines)
    for j in range(start, len(lines)):
        if re.match(r"^## ", lines[j]):
            end = j
            break
    entries = {}
    order_keys = []
    i = start
    while i < end:
        mm = entry_re.match(lines[i])
        if mm:
            old = int(mm.group(1))
            if old in removed:
                i += 1
                # consume continuation lines (indented or blank-separated) of the removed entry
                while i < end and (lines[i].strip() == "" or lines[i][0] in " \t"):
                    if lines[i].strip() == "" and i + 1 < end and not lines[i+1].strip():
                        break
                    i += 1
                continue
            # collect entry body until next numbered entry
            body = [lines[i]]
            i += 1
            while i < end and not entry_re.match(lines[i]) and not re.match(r"^## ", lines[i]):
                body.append(lines[i])
                i += 1
            entries[old] = body
            order_keys.append(old)
        else:
            i += 1
    new_list = []
    for new in range(1, len(entries) + 1):
        old = next(o for o, nn in m.items() if nn == new)
        body = entries[old]
        first = entry_re.sub(f"{new}. ", body[0], count=1)
        new_list.append(first)
        new_list.extend(body[1:])
    # trim trailing blank lines within the block
    while new_list and new_list[-1].strip() == "":
        new_list.pop()
    lines[start:end] = new_list
    return "\n".join(lines)

def main():
    audit = []
    for path in FILES:
        with open(path, encoding="utf-8") as f:
            text = f.read()
        order = first_appearance_order(text)
        m = build_map(order)
        audit.append(f"== {path} ==")
        audit.append(f"first-appearance order (old numbers): {order}")
        audit.append("old -> new mapping:")
        for old in sorted(m):
            audit.append(f"  {old} -> {m[old]}")
        new_text = remap_text(text, m)
        new_text = reorder_list(new_text, m, REMOVED)
        # update count label in ZH header ("34 条" -> correct count)
        new_text = new_text.replace("（Vancouver 34 条，附 PMID 便于核验）",
                                    f"（Vancouver {len(order)} 条，附 PMID 便于核验）")
        with open(path, "w", encoding="utf-8", newline="\n") as f:
            f.write(new_text)
        audit.append(f"OK, {len(order)} refs.")
    with open(r"D:\NHANES\results\14b_renumber_map.txt", "w", encoding="utf-8") as f:
        f.write("\n".join(audit) + "\n")
    print("\n".join(audit))

if __name__ == "__main__":
    main()
