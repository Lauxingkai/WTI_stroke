"""
NHANES LAB和EXAM模块扩展合并（简化版）
只提取关键变量，避免列名冲突
"""
import os
import pyreadstat
import pandas as pd
from pathlib import Path
from datetime import datetime
import warnings
warnings.filterwarnings('ignore')

BASE_DIR = Path(r"D:\NHANES\Data")
INPUT_FILE = Path(r"D:\NHANES\Processed\standardized\nhanes_standardized_20260816_2120.parquet")
OUTPUT_DIR = Path(r"D:\NHANES\Processed\extended")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# 周期配置
CYCLES = [
    {"name": "2001-2002", "letter": "B"},
    {"name": "2003-2004", "letter": "C"},
    {"name": "2005-2006", "letter": "D"},
    {"name": "2007-2008", "letter": "E"},
    {"name": "2009-2010", "letter": "F"},
    {"name": "2011-2012", "letter": "G"},
    {"name": "2013-2014", "letter": "H"},
    {"name": "2015-2016", "letter": "I"},
    {"name": "2017-2018", "letter": "J"},
]

def find_file(cycle_name, letter, module, prefix):
    """查找模块文件"""
    cycle_dir = BASE_DIR / cycle_name / module
    if not cycle_dir.exists():
        return None
    
    # 精确匹配
    patterns = [f"{prefix}_{letter}.XPT", f"{prefix}_{letter}.xpt"]
    for pattern in patterns:
        file_path = cycle_dir / pattern
        if file_path.exists():
            return str(file_path)
    
    # 模糊匹配
    for f in cycle_dir.glob("*.XPT"):
        if f.name.upper().startswith(prefix.upper()):
            return str(f)
    
    return None

def read_with_encoding(file_path):
    """尝试不同编码读取"""
    for encoding in ['utf-8', 'latin-1', 'cp1252']:
        try:
            df, meta = pyreadstat.read_xport(file_path, encoding=encoding)
            return df
        except (UnicodeDecodeError, Exception):
            continue
    return None

def extract_key_lab_vars(df):
    """提取关键LAB变量"""
    # 血糖代谢
    glu_vars = [c for c in df.columns if 'GLU' in c.upper() or 'HBA1C' in c.upper() or 'HGA' in c.upper()]
    # 血脂
    lipids_vars = [c for c in df.columns if any(x in c.upper() for x in ['TC', 'LDL', 'HDL', 'TR', 'TG', 'CHOL'])]
    # 肾功能
    renal_vars = [c for c in df.columns if any(x in c.upper() for x in ['CREA', 'BUN', 'UREA', 'EGFR'])]
    # 炎症
    inflammation_vars = [c for c in df.columns if 'CRP' in c.upper()]
    # 肝功能
    liver_vars = [c for c in df.columns if any(x in c.upper() for x in ['ALT', 'AST', 'ALB', 'TP ', 'PROT'])]
    
    # 保留SEQN和关键变量
    keep_vars = ['SEQN']
    keep_vars.extend(glu_vars[:3])  # 最多保留3个血糖相关
    keep_vars.extend(lipids_vars[:8])  # 最多保留8个血脂相关
    keep_vars.extend(renal_vars[:4])  # 最多保留4个肾功能相关
    keep_vars.extend(inflammation_vars[:2])  # 最多保留2个炎症相关
    keep_vars.extend(liver_vars[:5])  # 最多保留5个肝功能相关
    
    # 去重
    keep_vars = list(set(keep_vars))
    
    # 只保留存在的列
    keep_vars = [v for v in keep_vars if v in df.columns]
    
    return df[keep_vars] if len(keep_vars) > 1 else df[['SEQN']]

def extract_key_exam_vars(df):
    """提取关键EXAM变量"""
    # 身体测量
    body_vars = [c for c in df.columns if any(x in c.upper() for x in ['BMX', 'WEIGHT', 'HEIGHT', 'BMI', 'WAIST'])]
    # 血压
    bp_vars = [c for c in df.columns if any(x in c.upper() for x in ['BPX', 'SY', 'DI'])]
    # 其他检查
    other_vars = [c for c in df.columns if any(x in c.upper() for x in ['DEXA', 'EEX', 'HEO', 'OHX', 'VIX'])]
    
    keep_vars = ['SEQN']
    keep_vars.extend(body_vars[:5])
    keep_vars.extend(bp_vars[:6])
    keep_vars.extend(other_vars[:3])
    
    keep_vars = list(set(keep_vars))
    keep_vars = [v for v in keep_vars if v in df.columns]
    
    return df[keep_vars] if len(keep_vars) > 1 else df[['SEQN']]

def process_cycle(cycle_config):
    """处理单个周期"""
    cycle_name = cycle_config["name"]
    letter = cycle_config["letter"]
    
    print(f"\nProcessing {cycle_name}...")
    
    lab_dfs = []
    exam_dfs = []
    
    # === LAB模块 ===
    lab_dirs = ["Laboratory"]
    lab_prefixes = ["LAB", "L02", "L04", "L05", "L06", "L07", "L09", "L10", "L11", 
                   "L13", "L16", "L17", "L18", "L19", "L20", "L21", "L22", "L25", "L26", "L28"]
    
    for dir_name in lab_dirs:
        for prefix in lab_prefixes:
            file_path = find_file(cycle_name, letter, dir_name, prefix)
            if file_path:
                df = read_with_encoding(file_path)
                if df is not None and len(df) > 0:
                    df = extract_key_lab_vars(df)
                    lab_dfs.append(df)
                    print(f"  LAB {prefix}: {len(df)} rows")
    
    # === EXAM模块 ===
    exam_dirs = ["Examination"]
    exam_prefixes = ["BPX", "CVX", "DEXA", "EEX", "HEO", "OHX", "Pulmonary", "THX", "VIX", "BMX"]
    
    for dir_name in exam_dirs:
        for prefix in exam_prefixes:
            file_path = find_file(cycle_name, letter, dir_name, prefix)
            if file_path:
                df = read_with_encoding(file_path)
                if df is not None and len(df) > 0:
                    df = extract_key_exam_vars(df)
                    exam_dfs.append(df)
                    print(f"  EXAM {prefix}: {len(df)} rows")
    
    # 合并同模块数据
    lab_merged = None
    if lab_dfs:
        lab_merged = lab_dfs[0]
        for df in lab_dfs[1:]:
            # 按SEQN合并，避免列名冲突
            common_cols = [c for c in df.columns if c in lab_merged.columns]
            new_cols = [c for c in df.columns if c not in lab_merged.columns]
            if new_cols:
                df_new = df[['SEQN'] + new_cols]
                lab_merged = lab_merged.merge(df_new, on='SEQN', how='outer')
    
    exam_merged = None
    if exam_dfs:
        exam_merged = exam_dfs[0]
        for df in exam_dfs[1:]:
            common_cols = [c for c in df.columns if c in exam_merged.columns]
            new_cols = [c for c in df.columns if c not in exam_merged.columns]
            if new_cols:
                df_new = df[['SEQN'] + new_cols]
                exam_merged = exam_merged.merge(df_new, on='SEQN', how='outer')
    
    if lab_merged is not None:
        lab_merged['cycle'] = cycle_name
        lab_merged['cycle_letter'] = letter
    
    if exam_merged is not None:
        exam_merged['cycle'] = cycle_name
        exam_merged['cycle_letter'] = letter
    
    return lab_merged, exam_merged

def main():
    """主函数"""
    print("=" * 70)
    print("NHANES Extended Data Merge (LAB + EXAM)")
    print("=" * 70)
    print(f"Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    
    # 读取基础数据
    print("Reading base data...")
    base_df = pd.read_parquet(INPUT_FILE)
    print(f"Base: {base_df.shape}")
    
    # 处理各周期
    all_lab = []
    all_exam = []
    
    for cycle in CYCLES:
        lab, exam = process_cycle(cycle)
        if lab is not None:
            all_lab.append(lab)
        if exam is not None:
            all_exam.append(exam)
    
    # 纵向合并
    if all_lab:
        lab_full = pd.concat(all_lab, ignore_index=True, sort=False)
        # 去重
        lab_full = lab_full.drop_duplicates(subset=['SEQN', 'cycle'], keep='first')
    else:
        lab_full = pd.DataFrame()
    
    if all_exam:
        exam_full = pd.concat(all_exam, ignore_index=True, sort=False)
        exam_full = exam_full.drop_duplicates(subset=['SEQN', 'cycle'], keep='first')
    else:
        exam_full = pd.DataFrame()
    
    print(f"\nLAB full: {lab_full.shape if not lab_full.empty else 'Empty'}")
    print(f"EXAM full: {exam_full.shape if not exam_full.empty else 'Empty'}")
    
    # 合并到基础数据
    print("\nMerging with base data...")
    merged = base_df.copy()
    
    if not lab_full.empty:
        lab_cols = [c for c in lab_full.columns if c not in merged.columns]
        if lab_cols:
            merged = merged.merge(lab_full[['SEQN', 'cycle'] + lab_cols], on=['SEQN', 'cycle'], how='left')
            print(f"  LAB: +{len(lab_cols)} columns")
    
    if not exam_full.empty:
        exam_cols = [c for c in exam_full.columns if c not in merged.columns]
        if exam_cols:
            merged = merged.merge(exam_full[['SEQN', 'cycle'] + exam_cols], on=['SEQN', 'cycle'], how='left')
            print(f"  EXAM: +{len(exam_cols)} columns")
    
    print(f"\nFinal: {merged.shape}")
    
    # 保存
    output_file = OUTPUT_DIR / f"nhanes_extended_{datetime.now().strftime('%Y%m%d_%H%M')}.parquet"
    merged.to_parquet(output_file, index=False)
    print(f"\nSaved: {output_file}")
    print(f"Size: {output_file.stat().st_size / 1024/1024:.2f} MB")
    
    # 生成报告
    report = {
        "timestamp": datetime.now().isoformat(),
        "base_shape": list(base_df.shape),
        "extended_shape": list(merged.shape),
        "lab_rows": len(lab_full) if not lab_full.empty else 0,
        "lab_cols": len(lab_full.columns) if not lab_full.empty else 0,
        "exam_rows": len(exam_full) if not exam_full.empty else 0,
        "exam_cols": len(exam_full.columns) if not exam_full.empty else 0,
        "new_variables": len(merged.columns) - len(base_df.columns),
        "output_file": str(output_file)
    }
    
    report_file = OUTPUT_DIR / "extended_merge_report.json"
    with open(report_file, 'w', encoding='utf-8') as f:
        import json
        json.dump(report, f, indent=2, ensure_ascii=False)
    print(f"Report: {report_file}")
    
    return merged, report

if __name__ == "__main__":
    main()
