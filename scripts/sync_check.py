# -*- coding: utf-8 -*-
"""sync_check.py — 发布镜像一致性闸（治理策略 §1-R2/§6）
工作区 scripts/ 与 github_release/WTI_stroke/scripts/ 哈希比对：
  - 镜像中存在的文件必须与工作区逐字节一致（镜像滞后 => FAIL）
  - 工作区新增未发布脚本 => WARN
排除 _tmp_* 与归档类。
用法: D:\\anaconda\\python.exe scripts\\sync_check.py
"""
import os, hashlib, io, sys
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')
SRC = r'D:\\NHANES\\scripts'
MIR = r'D:\\NHANES\\github_release\\WTI_stroke\\scripts'
def sha(p):
    h = hashlib.sha256()
    with open(p, 'rb') as f:
        for c in iter(lambda: f.read(65536), b''): h.update(c)
    return h.hexdigest()
def skip(fn): return fn.startswith('_tmp_') or fn.startswith('_.') or '.bak' in fn or fn.endswith(('.log', '.tmp')) or 'archive' in fn
src_f = {f for f in os.listdir(SRC) if not skip(f) and os.path.isfile(os.path.join(SRC, f))}
mir_f = {f for f in os.listdir(MIR) if not skip(f) and os.path.isfile(os.path.join(MIR, f))}
fails, warns = [], []
for f in sorted(mir_f):
    if f not in src_f: fails.append(f'镜像含工作区已无文件: {f}')
    elif sha(os.path.join(MIR, f)) != sha(os.path.join(SRC, f)): fails.append(f'镜像滞后(哈希不一致): {f}')
for f in sorted(src_f - mir_f): warns.append(f'工作区新增未发布: {f}')
if fails:
    print('SYNC FAIL (' + str(len(fails)) + '):'); [print('  ', x) for x in fails]
    if warns: print('WARN:'); [print('  ', x) for x in warns]
    sys.exit(1)
print('SYNC PASS: 镜像' , len(mir_f), '个文件全部与工作区一致')
if warns:
    print('WARN (' + str(len(warns)) + '):'); [print('  ', x) for x in warns]
