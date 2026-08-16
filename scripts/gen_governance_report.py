import pandas as pd
import json
from pathlib import Path

# 读取合并数据
df = pd.read_parquet(r'D:\NHANES\Processed\raw\nhanes_full_merged_20260816.parquet')

print('='*70)
print('NHANES Full Database Governance Report')
print('='*70)

# 基本统计
print(f'\nTotal samples: {len(df):,}')
print(f'Total variables: {len(df.columns)}')
print(f'Cycles: {df["cycle"].nunique()}')

# 各周期样本量
print('\nCycle breakdown:')
for cycle in sorted(df['cycle'].dropna().unique()):
    n = len(df[df['cycle'] == cycle])
    print(f'  {cycle}: {n:,}')

# 模块覆盖
print('\nModule coverage:')
modules = df['_module'].value_counts()
for mod, count in modules.items():
    print(f'  {mod}: {count:,} rows')

# 关键变量存在性
key_vars = ['SEQN', 'RIAGENDR', 'RIDAGEYR', 'RIDRETH1', 'WTINT2YR', 'WTMEC2YR', 
            'SDMVPSU', 'SDMVSTRA', 'BMXBMI', 'DR1TVK', 'MCQ160F']
print('\nKey variable availability:')
for var in key_vars:
    if var in df.columns:
        n_missing = df[var].isna().sum()
        print(f'  {var}: {len(df)-n_missing:,} ({100*(len(df)-n_missing)/len(df):.1f}%)')
    else:
        print(f'  {var}: NOT FOUND')

# 保存摘要
summary = {
    'total_samples': int(len(df)),
    'total_variables': int(len(df.columns)),
    'cycles_merged': int(df['cycle'].nunique()),
    'file_size_mb': round(Path(r'D:\NHANES\Processed\raw\nhanes_full_merged_20260816.parquet').stat().st_size / 1024 / 1024, 2),
    'timestamp': '2026-08-16T20:55:00'
}

with open(r'D:\NHANES\Processed\governance_summary.json', 'w') as f:
    json.dump(summary, f, indent=2)
print('\nSummary saved: D:\\NHANES\\Processed\\governance_summary.json')
