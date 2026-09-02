# -*- coding: utf-8 -*-
# 02b_pa_redefine.py  NHANES PA 变量重定义（V-1 修复，2026-09-02）
# 口径：每周中强度体力活动分钟
#   E-J: PAD645xPAQ640 + PAD630xPAQ625 + PAD675xPAQ670
#   D  : PAD080 x PAQ050Q x 单位折算(7/1/30-7) + PAD160 x PAD120 x 7/30
#   剧烈不计入；PAQ 7/9 拒答 -> NA（严格）；>960 分钟/周 cap -> NA
import pyreadstat, pandas as pd, numpy as np, sys, io
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')
RAW = r'D:\NHANES\data\raw'
OUT = r'D:\NHANES\data\processed'
def cap(v, hi=960):
    v = pd.to_numeric(v, errors='coerce')
    return np.where(v > hi, np.nan, v)
rows = {}; notes = []
for cy in 'DEFGHIJ':
    p, m = pyreadstat.read_xport(f'{RAW}\\PAQ_{cy}.XPT')
    if cy == 'D':
        walk_yn = pd.to_numeric(p['PAD020'], errors='coerce')
        q  = pd.to_numeric(p['PAQ050Q'], errors='coerce')
        u  = pd.to_numeric(p['PAQ050U'], errors='coerce')
        dur = cap(p['PAD080'])
        times_wk = np.where(u == 1, q*7, np.where(u == 2, q, np.where(u == 3, q*(30.0/7), np.nan)))
        wk_bike = np.where((walk_yn == 1) & pd.notna(times_wk) & pd.notna(dur), times_wk*dur, np.nan)
        wk_bike = np.where(walk_yn == 2, 0.0, wk_bike)
        wk_bike = np.where((walk_yn == 7) | (walk_yn == 9), np.nan, wk_bike)
        hh_yn = pd.to_numeric(p['PAQ100'], errors='coerce')
        n30 = pd.to_numeric(p['PAD120'], errors='coerce')
        hh_min = cap(p['PAD160'])
        hh = np.where((hh_yn == 1) & pd.notna(n30) & pd.notna(hh_min), n30*hh_min*(7.0/30), np.nan)
        hh = np.where(hh_yn == 2, 0.0, hh)
        hh = np.where((hh_yn == 7) | (hh_yn == 9), np.nan, hh)
        tot = np.where(pd.notna(wk_bike) & pd.notna(hh), wk_bike+hh,
              np.where(pd.notna(wk_bike), wk_bike, np.where(pd.notna(hh), hh, np.nan)))
        notes.append(f'D: walk>0={(wk_bike>0).sum()}, hh>0={(hh>0).sum()}')
    else:
        def comp(yn, days, mins):
            y = pd.to_numeric(p[yn], errors='coerce')
            dd = pd.to_numeric(p[days], errors='coerce')
            mm = cap(p[mins])
            m2 = np.where(pd.notna(mm) & pd.notna(dd) & (dd >= 1) & (dd <= 7), mm*dd, np.nan)
            out = np.where(y == 1, m2, np.nan)
            out = np.where(y == 2, 0.0, out)
            out = np.where((y == 7) | (y == 9), np.nan, out)
            return out
        c_walk = comp('PAQ635','PAQ640','PAD645')
        c_work = comp('PAQ620','PAQ625','PAD630')
        c_rec  = comp('PAQ665','PAQ670','PAD675')
        na_flag = np.any(pd.concat([p['PAQ635'].isin([7,9]), p['PAQ620'].isin([7,9]), p['PAQ665'].isin([7,9])], axis=1).values, axis=1)
        tot = np.where(na_flag, np.nan,
              np.where(pd.notna(c_walk) & pd.notna(c_work) & pd.notna(c_rec), c_walk+c_work+c_rec,
              np.where(pd.notna(c_walk) & pd.notna(c_work), c_walk+c_work,
              np.where(pd.notna(c_walk) & pd.notna(c_rec), c_walk+c_rec,
              np.where(pd.notna(c_work) & pd.notna(c_rec), c_work+c_rec,
              np.where(pd.notna(c_walk), c_walk,
              np.where(pd.notna(c_work), c_work,
              np.where(pd.notna(c_rec), c_rec, np.nan))))))))
        notes.append(f'{cy}: walk>0={(c_walk>0).sum()}, work>0={(c_work>0).sum()}, rec>0={(c_rec>0).sum()}')
    tot = np.minimum(tot, 1680.0)   # top-code: >1680 min/wk (4h/d) -> 1680（极端报告/编码异常，2026-09-02）
    d = pd.DataFrame({'SEQN': p['SEQN'].astype(int), 'CYCLE': cy, 'pa_mvpaw_min': np.round(tot,1)})
    rows[cy] = d
pa = pd.concat(rows.values(), ignore_index=True)
print('PA rows:', len(pa))
nh = pd.read_csv(OUT + r'\nhanes_fasting_cross_cov.csv')
print('cohort:', len(nh))
nh = nh.merge(pa, left_on=['SEQN','CYCLE.x'], right_on=['SEQN','CYCLE'], how='left')
if 'pa_mvpaw_min_y' in nh.columns: nh.rename(columns={'pa_mvpaw_min_y':'pa_mvpaw_min'}, inplace=True)
nh = nh.drop(columns=[c for c in ['CYCLE','pa_mvpaw_min_y'] if c in nh.columns])
nh = nh.drop(columns=['pa_min_day'], errors='ignore')   # 移除旧 PA 列（v1 遗留）
print('post-merge n:', len(nh), '| pa non-NA:', int(nh.pa_mvpaw_min.notna().sum()), '| NA:', int(nh.pa_mvpaw_min.isna().sum()))
print('describe:', nh.pa_mvpaw_min.describe().round(1).to_dict())
print('==0:', int((nh.pa_mvpaw_min==0).sum()))
print('tertiles:', pd.qcut(nh.pa_mvpaw_min, 3, labels=False, duplicates='drop').value_counts().sort_index().to_dict())
nh.to_csv(OUT + r'\nhanes_fasting_cross_cov_v2.csv', index=False)
for n in notes: print(n)
print('DONE')
