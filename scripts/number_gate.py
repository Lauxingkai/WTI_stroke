# -*- coding: utf-8 -*-
"""number_gate.py — 关键数字闸（治理策略 §2-4/§6）
规则：旧值在稿件文本中零残留；新值按位置到位（英/中/supp）。
规范化：去 \\ 转义 / -- -> - / 去 * 斜体 / 全角逗号->半角 / 去空白。
用法: D:\\anaconda\\python.exe scripts\\number_gate.py
任一 FAIL -> exit 1。
"""
import io, re, sys
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')
BASE = r'D:\\NHANES\\manuscript\\final'
FILES = {'EN': BASE + r'\\manuscript_main.md',
         'ZH': BASE + r'\\manuscript_main_中文终版_20260823.md',
         'SUPP': BASE + r'\\supplementary.md'}
def norm(t):
    t = t.replace('\\', '').replace('--', '-').replace('−', '-').replace('–', '-')
    t = t.replace('，', ',').replace('（', '(').replace('）', ')')
    return re.sub(r'[*\s]', '', t)
CONSTRAINTS = [
 # (label, file, [旧值必须 0], [新值必须 >=1])
 ('EN-PA-M3', 'EN', ['0.389', '0.856', '0.907', '0.890', '0.485'], ['0.91,95%CI0.77-1.07', '0.99(95%CI0.92-1.07', '0.73-1.38, *P* = 0.969'.replace('*',''), '0.870']),
 ('EN-T3', 'EN', ['T3OR0.70'], ['0.68, *P* = 0.006'.replace('*','')]),
 ('EN-range', 'EN', ['0.94-1.09'], ['0.91-1.09']),
 ('EN-Eval', 'EN', ['1.05to1.34', 'null(Table4)'], ['1.14to1.44']),
 ('EN-HTGW', 'EN', ['0.86(0.68-1.08)'], ['0.83,0.66-1.05']),
 ('EN-per10', 'EN', [], ['per-10cm', '1.22(1.13-1.33)', '1.00(0.99-1.01)']),
 ('EN-PA', 'EN', ['minutes/day'], ['minutes/week']),
 ('EN-DA', 'EN', ['willbeshared', 'Untilthecodeisdeposited'], ['github.com/Lauxingkai/WTI_stroke']),
 ('EN-typo', 'EN', ['particpipants', '\u005b9,'], []),
 ('EN-UK', 'EN', [], ['normalised', 'Parameterisation', 'ischaemic']),
 ('ZH-Eval', 'ZH', ['1.05-1.34', '趋近1（表4）'], ['1.14-1.44']),
 ('ZH-HTGW', 'ZH', ['0.86，0.68-1.08'], ['0.83，0.66-1.05']),
 ('ZH-PA', 'ZH', ['分钟/天'], ['分钟/周']),
 ('SUPP-HTGW', 'SUPP', ['0.86 (0.68-1.08)', '0.197'], ['0.83 (0.66-1.05)', '0.120']),
 ('SUPP-PA', 'SUPP', ['minutes/day'], []),
]
fails, notes = [], []
texts = {k: norm(open(v, encoding='utf-8').read()) for k, v in FILES.items()}
for label, fk, olds, news in CONSTRAINTS:
    t = texts[fk]
    for o in olds:
        on = norm(o)
        c = t.count(on)
        if c: fails.append(f'{label}: 旧值残留 {on[:40]} x{c}')
    for n in news:
        nn = norm(n)
        c = t.count(nn)
        if not c: fails.append(f'{label}: 新值缺失 {nn[:40]}')
if fails:
    print('FAIL (' + str(len(fails)) + '):'); [print('  ', f) for f in fails]; sys.exit(1)
print('NUMBER GATE PASS: 全部约束通过（' + str(len(CONSTRAINTS)) + ' 组）')
