"""
NHANES变量标准化
创建统一变量命名和缺失值编码
"""
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime
import json

INPUT_FILE = Path(r"D:\NHANES\Processed\raw\nhanes_full_merged_20260816.parquet")
OUTPUT_DIR = Path(r"D:\NHANES\Processed\standardized")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# 变量别名映射（原始名 -> 标准名）
VARIABLE_ALIASES = {
    # 人口学
    'RIAGENDR': 'sex',
    'RIDAGEYR': 'age',
    'RIDRETH1': 'race_hispan',
    'RIDRETH3': 'race_hispan_3',
    'INDFMIN2': 'poverty_income',
    'INDFMPIR': 'poverty_income_ratio',
    'DMDEDUC3': 'education',
    'DMDMARTL': 'marital_status',
    
    # 身体测量
    'BMXBMI': 'bmi',
    'BMXWT': 'weight_kg',
    'BMXHT': 'height_cm',
    'BMXWAIST': 'waist_circumference',
    
    # 血压
    'BPXSY3': 'sbp',
    'BPXDI3': 'dbp',
    
    # 饮食
    'DR1TVK': 'vitamin_k_total',
    'DR1TOT_K1': 'vitamin_k1',
    'DR1TOT_K2': 'vitamin_k2',
    
    # 医疗状况
    'MCQ160F': 'stroke',
    'MCQ160E': 'heart_attack',
    
    # 糖尿病
    'DIQ020': 'diabetes',
    'DIQ050': 'diabetes_age',
    
    # 吸烟
    'SMQ110': 'smoker',
    'SMQ130': 'smoking_pack_years',
    
    # 身体活动
    'PAQ610': 'moderate_activity',
    'PAQ710': 'vigorous_activity',
    
    # 权重和抽样
    'WTINT2YR': 'wt_interview',
    'WTMEC2YR': 'wt_exam',
    'SDMVPSU': 'psu',
    'SDMVSTRA': 'strata',
    'SDDSRVYR': 'survey_year',
}

# 缺失值编码映射
MISSING_CODES = {-9999, -7777, -5555, -3333, -1111, 7, 9}

def standardize_variables(df):
    """标准化变量"""
    print(f"原始数据: {df.shape}")
    
    # 1. 重命名变量
    rename_map = {}
    for orig, std in VARIABLE_ALIASES.items():
        if orig in df.columns and std not in df.columns:
            rename_map[orig] = std
        elif orig in df.columns and std in df.columns:
            # 如果标准名已存在，检查是否一致
            pass
    
    df = df.rename(columns=rename_map)
    print(f"重命名变量: {len(rename_map)} 个")
    
    # 2. 标准化缺失值
    numeric_cols = df.select_dtypes(include=[np.number]).columns
    missing_replaced = 0
    for col in numeric_cols:
        # 替换已知的缺失值编码
        for code in MISSING_CODES:
            mask = df[col] == code
            missing_replaced += mask.sum()
            df.loc[mask, col] = np.nan
    
    print(f"缺失值标准化: 替换 {missing_replaced} 个值")
    
    # 3. 创建派生变量
    # BMI分类
    if 'bmi' in df.columns:
        df['bmi_category'] = pd.cut(df['bmi'], 
                                   bins=[0, 18.5, 25, 30, float('inf')],
                                   labels=['underweight', 'normal', 'overweight', 'obese'])
    
    # 年龄分组
    if 'age' in df.columns:
        df['age_group'] = pd.cut(df['age'], 
                                bins=[0, 20, 40, 60, 80, 100],
                                labels=['0-19', '20-39', '40-59', '60-79', '80+'])
    
    # 维生素K分位数
    if 'vitamin_k_total' in df.columns:
        df['vitamin_k_quintile'] = pd.qcut(df['vitamin_k_total'].rank(method='first'), 
                                           q=5, labels=['Q1', 'Q2', 'Q3', 'Q4', 'Q5'],
                                           duplicates='drop')
    
    print(f"派生变量: bmi_category, age_group, vitamin_k_quintile")
    
    return df

def generate_variable_dictionary(df):
    """生成变量字典"""
    dictionary = []
    
    for col in df.columns:
        if col.startswith('_'):
            continue
            
        info = {
            'variable_name': col,
            'data_type': str(df[col].dtype),
            'non_missing': int(df[col].notna().sum()),
            'missing': int(df[col].isna().sum()),
            'missing_pct': round(float(df[col].isna().mean() * 100), 2),
            'unique_values': int(df[col].nunique()) if df[col].dtype != 'object' else None,
        }
        
        if df[col].dtype in ['float64', 'int64']:
            info['min'] = float(df[col].min()) if df[col].notna().any() else None
            info['max'] = float(df[col].max()) if df[col].notna().any() else None
            info['mean'] = round(float(df[col].mean()), 2) if df[col].notna().any() else None
            info['std'] = round(float(df[col].std()), 2) if df[col].notna().any() else None
        
        dictionary.append(info)
    
    return dictionary

def main():
    """主函数"""
    print("=" * 70)
    print("NHANES Variable Standardization")
    print("=" * 70)
    print(f"Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    
    # 读取数据
    print("Reading data...")
    df = pd.read_parquet(INPUT_FILE)
    
    # 标准化
    print("\nStandardizing variables...")
    df_std = standardize_variables(df)
    
    # 保存标准化数据
    output_file = OUTPUT_DIR / f"nhanes_standardized_{datetime.now().strftime('%Y%m%d_%H%M')}.parquet"
    df_std.to_parquet(output_file, index=False)
    print(f"\nSaved: {output_file}")
    print(f"Size: {output_file.stat().st_size / 1024/1024:.2f} MB")
    
    # 生成变量字典
    print("\nGenerating variable dictionary...")
    var_dict = generate_variable_dictionary(df_std)
    
    dict_file = OUTPUT_DIR / "variable_dictionary.json"
    with open(dict_file, 'w', encoding='utf-8') as f:
        json.dump(var_dict, f, indent=2, ensure_ascii=False)
    print(f"Saved: {dict_file}")
    
    # 生成摘要报告
    summary = {
        'timestamp': datetime.now().isoformat(),
        'original_shape': list(df.shape),
        'standardized_shape': list(df_std.shape),
        'variables_renamed': len([c for c in df_std.columns if c in VARIABLE_ALIASES.values()]),
        'derived_variables': ['bmi_category', 'age_group', 'vitamin_k_quintile'],
        'output_file': str(output_file),
        'dictionary_file': str(dict_file)
    }
    
    summary_file = OUTPUT_DIR / "standardization_summary.json"
    with open(summary_file, 'w', encoding='utf-8') as f:
        json.dump(summary, f, indent=2, ensure_ascii=False)
    print(f"Saved: {summary_file}")
    
    return df_std, summary

if __name__ == "__main__":
    main()
