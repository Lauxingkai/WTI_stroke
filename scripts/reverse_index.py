# -*- coding: utf-8 -*-
"""reverse_index.py — 变更传播影响面反查（治理策略 §2-2）
用法: D:\\anaconda\\python.exe scripts\\reverse_index.py 0.83
      D:\\anaconda\\python.exe scripts\\reverse_index.py 0.94
扫描 英/中/supp 稿件 + Table 文件 + Figure 注 中含指定数字/关键词的行。
"""
import io, sys, os, glob, re
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')
if len(sys.argv) < 2:
    print('usage: reverse_index.py <keyword-or-number> [more...]'); sys.exit(2)
keys = sys.argv[1:]
BASE = r'D:\\NHANES\\manuscript\\final'
targets = [os.path.join(BASE, 'manuscript_main.md'), os.path.join(BASE, 'manuscript_main_%E4%B8%AD%E6%96%87%E7%BB%88%E7%89%88_20260823.md'), os.path.join(BASE, 'supplementary.md')]
targets += glob.glob(os.path.join(BASE, 'figures', '*.md'))
for p in targets:
    if not os.path.exists(p): continue
    t = open(p, encoding='utf-8', errors='replace').read()
    for i, ln in enumerate(t.splitlines(), 1):
        if any(k in ln for k in keys):
            print(f'{os.path.basename(p)} L{i}: {ln.strip()[:120]}')
