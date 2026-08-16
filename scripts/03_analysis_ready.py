"""
NHANES权重应用和抽样设计
创建survey设计对象用于复杂抽样分析
"""
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime
import json

INPUT_FILE = Path(r"D:\NHANES\Processed\standardized")
OUTPUT_DIR = Path(r"D:\NHANES\Processed\analysis_ready")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

def create_survey_design(df):
    """创建survey设计对象（伪实现，实际分析时用R survey包）"""
    
    # 检查必需变量
    required = ['wt_exam', 'psu', 'strata']
    missing = [v for v in required if v not in df.columns]
    
    if missing:
        print(f"警告: 缺少必需变量: {missing}")
        print("将使用简单随机抽样近似")
        return None
    
    # 创建设计对象属性
    design = {
        'type': 'nhanes_complex_sample',
        'strata_var': 'strata',
        'psu_var': 'psu',
        'weight_var': 'wt_exam',
        'weight_type': 'exam_weight',
        'cycles': df['cycle'].nunique(),
        'total_n': len(df),
        'notes': '实际分析请使用R survey包或Python pyweighted'
    }
    
    return design

def apply_sample_filters(df):
    """应用样本筛选"""
    
    filtered = df.copy()
    filters_applied = []
    
    # 1. 年龄筛选（20-85岁）
    if 'age' in filtered.columns:
        age_mask = (filtered['age'] >= 20) & (filtered['age'] <= 85)
        n_removed = (~age_mask).sum()
        filtered = filtered[age_mask]
        filters_applied.append(f'age_20_85: removed {n_removed}')
    
    # 2. 体重筛选（合理范围）
    if 'weight_kg' in filtered.columns:
        weight_mask = (filtered['weight_kg'] >= 30) & (filtered['weight_kg'] <= 200)
        n_removed = (~weight_mask).sum()
        filtered = filtered[weight_mask]
        filters_applied.append(f'weight_30_200: removed {n_removed}')
    
    # 3. 身高筛选（合理范围）
    if 'height_cm' in filtered.columns:
        height_mask = (filtered['height_cm'] >= 100) & (filtered['height_cm'] <= 220)
        n_removed = (~height_mask).sum()
        filtered = filtered[height_mask]
        filters_applied.append(f'height_100_220: removed {n_removed}')
    
    # 4. BMI筛选（合理范围）
    if 'bmi' in filtered.columns:
        bmi_mask = (filtered['bmi'] >= 15) & (filtered['bmi'] <= 60)
        n_removed = (~bmi_mask).sum()
        filtered = filtered[bmi_mask]
        filters_applied.append(f'bmi_15_60: removed {n_removed}')
    
    print(f"样本筛选: {len(filters_applied)} 个条件")
    for f in filters_applied:
        print(f"  - {f}")
    
    return filtered, filters_applied

def generate_analysis_summary(df):
    """生成分析就绪数据摘要"""
    
    summary = {
        'timestamp': datetime.now().isoformat(),
        'total_samples': len(df),
        'total_variables': len(df.columns),
        'cycles': int(df['cycle'].nunique()),
        'cycle_breakdown': {},
        'key_variables': {}
    }
    
    # 各周期样本量
    for cycle in sorted(df['cycle'].dropna().unique()):
        cycle_data = df[df['cycle'] == cycle]
        summary['cycle_breakdown'][cycle] = {
            'n': int(len(cycle_data)),
            'female_pct': round(float((cycle_data['sex'] == 2).mean() * 100), 1) if 'sex' in cycle_data.columns else None,
            'mean_age': round(float(cycle_data['age'].mean()), 1) if 'age' in cycle_data.columns else None,
            'mean_bmi': round(float(cycle_data['bmi'].mean()), 1) if 'bmi' in cycle_data.columns else None
        }
    
    # 关键变量统计
    key_vars = ['sex', 'age', 'bmi', 'vitamin_k_total', 'stroke']
    for var in key_vars:
        if var in df.columns:
            var_data = df[var].dropna()
            summary['key_variables'][var] = {
                'n': int(len(var_data)),
                'mean': round(float(var_data.mean()), 2) if var_data.dtype in ['float64', 'int64'] else None,
                'std': round(float(var_data.std()), 2) if var_data.dtype in ['float64', 'int64'] else None,
                'missing_pct': round(float(df[var].isna().mean() * 100), 1)
            }
            
            # 分类变量分布
            if var == 'sex':
                summary['key_variables'][var]['distribution'] = {
                    str(k): int(v) for k, v in df[var].value_counts().items()
                }
            elif var == 'stroke':
                summary['key_variables'][var]['cases'] = int((df[var] == 1).sum())
                summary['key_variables'][var]['controls'] = int((df[var] == 2).sum())
    
    return summary

def main():
    """主函数"""
    print("=" * 70)
    print("NHANES Analysis-Ready Data Preparation")
    print("=" * 70)
    print(f"Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    
    # 读取标准化数据
    std_files = list(INPUT_FILE.glob("*.parquet"))
    if not std_files:
        print("错误: 未找到标准化数据文件")
        return
    
    latest_file = max(std_files, key=lambda f: f.stat().st_mtime)
    print(f"Reading: {latest_file.name}")
    df = pd.read_parquet(latest_file)
    
    # 应用样本筛选
    print("\nApplying sample filters...")
    df_filtered, filters = apply_sample_filters(df)
    
    # 创建survey设计信息
    print("\nCreating survey design...")
    design = create_survey_design(df_filtered)
    
    # 生成分析摘要
    print("\nGenerating analysis summary...")
    summary = generate_analysis_summary(df_filtered)
    
    # 保存分析就绪数据
    output_file = OUTPUT_DIR / f"nhanes_analysis_ready_{datetime.now().strftime('%Y%m%d_%H%M')}.parquet"
    df_filtered.to_parquet(output_file, index=False)
    print(f"\nSaved: {output_file}")
    print(f"Size: {output_file.stat().st_size / 1024/1024:.2f} MB")
    
    # 保存摘要
    summary_file = OUTPUT_DIR / "analysis_ready_summary.json"
    with open(summary_file, 'w', encoding='utf-8') as f:
        json.dump(summary, f, indent=2, ensure_ascii=False)
    print(f"Saved: {summary_file}")
    
    # 保存设计信息
    design_file = OUTPUT_DIR / "survey_design.json"
    with open(design_file, 'w', encoding='utf-8') as f:
        json.dump(design, f, indent=2, ensure_ascii=False)
    print(f"Saved: {design_file}")
    
    print("\n" + "=" * 70)
    print("Summary")
    print("=" * 70)
    print(f"原始样本: {summary['total_samples']:,}")
    print(f"筛选后样本: {len(df_filtered):,}")
    print(f"减少: {summary['total_samples'] - len(df_filtered):,}")
    print(f"变量数: {len(df_filtered.columns)}")
    print(f"周期数: {summary['cycles']}")
    
    return df_filtered, summary

if __name__ == "__main__":
    main()
