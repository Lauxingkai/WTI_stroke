"""
NHANES数据清洗脚本
清理合并后的原始数据，处理异常值和重复列
"""
import pandas as pd
import numpy as np
from pathlib import Path

BASE_DIR = Path(r"D:\NHANES")
INPUT_FILE = BASE_DIR / "Processed" / "nhanes_raw_merged.csv"
OUTPUT_FILE = BASE_DIR / "Processed" / "nhanes_cleaned.csv"

def clean_data(df):
    """数据清洗主函数"""
    print(f"原始数据: {df.shape}")
    
    # 1. 移除重复列
    dup_cols = [c for c in df.columns if c.endswith('_x') or c.endswith('_y')]
    if dup_cols:
        df = df.drop(columns=dup_cols)
        print(f"移除重复列: {dup_cols}")
    
    # 2. 清理维生素K异常值
    print(f"\nVK原始统计:")
    print(df['DR1TVK'].describe())
    
    # 保留合理范围：0-500 µg/天（文献报道美国成人平均90-130）
    vk_mask = (df['DR1TVK'] >= 0) & (df['DR1TVK'] <= 500)
    vk_removed = (~vk_mask).sum()
    df = df[vk_mask | df['DR1TVK'].isna()]
    print(f"\nVK异常值移除: {vk_removed} rows (>500 ug/day)")
    
    # 3. 清理卒中变量
    # 重要：MCQ160F = 卒中（正确），MCQ160E = 心梗（错误，已废弃）
    # 参见 HANDOFF.md B3 警示
    stroke_var = 'MCQ160F' if 'MCQ160F' in df.columns else None
    if stroke_var is None:
        print("错误: 数据中无MCQ160F（卒中）变量，请检查合并脚本")
        return df

    print(f"\n卒中变量({stroke_var})原始分布:")
    print(df[stroke_var].value_counts(dropna=False))

    # 只保留1和2，其他设为NaN
    df.loc[~df[stroke_var].isin([1, 2]), stroke_var] = np.nan
    stroke_filtered = df[stroke_var].notna().sum()
    print(f"有效卒中数据: {stroke_filtered} rows")
    
    # 4. 年龄合理性过滤（20-85岁）
    age_mask = (df['RIDAGEYR'] >= 20) & (df['RIDAGEYR'] <= 85)
    age_removed = (~age_mask).sum()
    df = df[age_mask]
    print(f"\n年龄过滤移除: {age_removed} 条")
    
    # 5. BMI合理性过滤（15-50）
    if 'BMXBMI' in df.columns:
        bmx_mask = (df['BMXBMI'] >= 15) & (df['BMXBMI'] <= 50)
        bmx_removed = (~bmx_mask).sum()
        df = df[bmx_mask]
        print(f"BMI filter removed: {bmx_removed} rows")
    
    # 6. 重命名关键变量（卒中变量用MCQ160F）
    stroke_col = 'MCQ160F' if 'MCQ160F' in df.columns else 'MCQ160E'
    var_rename = {
        'RIAGENDR': 'sex',
        'RIDAGEYR': 'age',
        'RIDRETH1': 'race',
        'DR1TVK': 'vitamin_k',
        stroke_col: 'stroke',
        'WTINT2YR': 'weight_interview',
        'WTMEC2YR': 'weight_exam',
        'SDMVPSU': 'psu',
        'SDMVSTRA': 'strata',
        'BMXBMI': 'bmi'
    }
    df = df.rename(columns=var_rename)
    
    print(f"\n清洗后数据: {df.shape}")
    
    return df

def main():
    """主函数"""
    print("=" * 60)
    print("NHANES数据清洗")
    print("=" * 60)
    
    # 读取数据
    df = pd.read_csv(INPUT_FILE)
    
    # 清洗
    df_clean = clean_data(df)
    
    # 保存
    df_clean.to_csv(OUTPUT_FILE, index=False, encoding='utf-8-sig')
    print(f"\n清洗后数据已保存: {OUTPUT_FILE}")
    print(f"文件大小: {OUTPUT_FILE.stat().st_size / 1024:.1f} KB")
    
    # 生成清洗报告（使用原始变量名统计）
    stroke_col = 'MCQ160F' if 'MCQ160F' in df.columns else 'MCQ160E'
    report = {
        "original_shape": list(df.shape),
        "cleaned_shape": list(df_clean.shape),
        "rows_removed": int(df.shape[0] - df_clean.shape[0]),
        "vk_extreme_removed": int(((df['DR1TVK'] < 0) | (df['DR1TVK'] > 500)).sum()),
        "age_filtered": int(((df['RIDAGEYR'] < 20) | (df['RIDAGEYR'] > 85)).sum()),
        "stroke_variable": stroke_col,
        "stroke_valid": int(df_clean[stroke_col].notna().sum()),
        "stroke_cases": int((df_clean[stroke_col] == 1).sum()),
        "stroke_controls": int((df_clean[stroke_col] == 2).sum()),
    }
    
    report_path = BASE_DIR / "Processed" / "cleaning_report.json"
    import json
    with open(report_path, 'w', encoding='utf-8') as f:
        json.dump(report, f, indent=2, ensure_ascii=False)
    print(f"清洗报告已保存: {report_path}")
    
    return df_clean, report

if __name__ == "__main__":
    main()
