"""
NHANES全库合并脚本
合并所有可用周期的完整数据
"""
import os
import pyreadstat
import pandas as pd
from pathlib import Path
from datetime import datetime
import warnings
warnings.filterwarnings('ignore')

BASE_DIR = Path(r"D:\NHANES\Data")
OUTPUT_DIR = Path(r"D:\NHANES\Processed")
RAW_DIR = OUTPUT_DIR / "raw"
RAW_DIR.mkdir(parents=True, exist_ok=True)

# 周期配置
CYCLES = [
    {"name": "1999-2000", "letter": "", "exclude": True, "reason": "命名规则不同"},
    {"name": "2001-2002", "letter": "B", "exclude": False},
    {"name": "2003-2004", "letter": "C", "exclude": False},
    {"name": "2005-2006", "letter": "D", "exclude": False},
    {"name": "2007-2008", "letter": "E", "exclude": False},
    {"name": "2009-2010", "letter": "F", "exclude": False},
    {"name": "2011-2012", "letter": "G", "exclude": False},
    {"name": "2013-2014", "letter": "H", "exclude": False},
    {"name": "2015-2016", "letter": "I", "exclude": False},
    {"name": "2017-2018", "letter": "J", "exclude": False},
    {"name": "2019-2020", "letter": "P", "exclude": True, "reason": "COVID中断"},
    {"name": "2021-2022", "letter": "L", "exclude": False, "note": "新采样设计"},
    {"name": "2023-2024", "letter": "L", "exclude": True, "reason": "疑似重复"},
]

# 模块目录映射
MODULE_DIRS = {
    "DEMO": "Demographics",
    "DR1TOT": "Dietary",
    "DR2TOT": "Dietary",
    "MCQ": "Questionnaire",
    "BMX": "Examination",
    "BPX": "Examination",
    "SMQ": "Questionnaire",
    "DIQ": "Questionnaire",
    "PAQ": "Questionnaire",
    "LAB": "Laboratory",
}

def find_file(cycle_name, letter, module):
    """查找模块文件"""
    cycle_dir = BASE_DIR / cycle_name
    module_dir = MODULE_DIRS.get(module, module)
    search_dir = cycle_dir / module_dir
    
    if not search_dir.exists():
        return None
    
    if letter:
        patterns = [f"{module}_{letter}.XPT", f"{module}_{letter}.xpt"]
    else:
        patterns = [f"{module}.XPT", f"{module}.xpt"]
    
    for pattern in patterns:
        file_path = search_dir / pattern
        if file_path.exists():
            return str(file_path)
    
    for f in search_dir.glob("*.XPT"):
        if f.name.upper().startswith(module.upper()):
            return str(f)
    
    return None

def read_module(file_path, module_name):
    """读取模块文件"""
    if not file_path or not os.path.exists(file_path):
        return None

    # 先尝试pyreadstat默认读取
    try:
        df, meta = pyreadstat.read_xport(file_path)
        df["_module"] = module_name
        df["_file"] = os.path.basename(file_path)
        return df
    except Exception:
        pass
    
    # 如果失败，尝试用R读取（某些文件编码问题）
    try:
        import subprocess
        r_script = f'''
        library(haven)
        df <- read_xpt("{file_path.replace(chr(92), "/")}")
        write.csv(df, "{file_path}.csv", row.names = FALSE)
        '''
        with open('temp_read.R', 'w', encoding='utf-8') as f:
            f.write(r_script)
        
        result = subprocess.run(['Rscript', 'temp_read.R'], 
                              capture_output=True, text=True, timeout=60)
        
        if os.path.exists(f'{file_path}.csv'):
            df = pd.read_csv(f'{file_path}.csv')
            os.remove(f'{file_path}.csv')
            os.remove('temp_read.R')
            df["_module"] = module_name
            df["_file"] = os.path.basename(file_path)
            return df
    except Exception as e:
        print(f"  R read failed for {file_path}: {e}")
    
    # 最后尝试忽略错误
    try:
        df, meta = pyreadstat.read_xport(file_path, encoding='latin-1')
        df["_module"] = module_name
        df["_file"] = os.path.basename(file_path)
        return df
    except Exception as e:
        print(f"  Error reading {file_path}: {e}")
        return None

def merge_cycle(cycle_config):
    """合并单个周期数据"""
    cycle_name = cycle_config["name"]
    letter = cycle_config["letter"]
    
    if cycle_config.get("exclude"):
        return None
    
    print(f"\nProcessing {cycle_name}...")
    
    # 读取核心模块
    modules = {}
    for mod in ["DEMO", "DR1TOT", "DR2TOT", "MCQ", "BMX", "BPX", "SMQ", "DIQ", "PAQ"]:
        file_path = find_file(cycle_name, letter, mod)
        df = read_module(file_path, mod)
        modules[mod] = df
        
        if df is not None:
            print(f"  {mod}: {len(df)} rows")
    
    # 以DEMO为基础合并
    if modules["DEMO"] is None:
        print(f"  Skip: DEMO missing")
        return None
    
    merged = modules["DEMO"].copy()
    
    # 左连接其他模块
    for mod in ["DR1TOT", "DR2TOT", "MCQ", "BMX", "BPX", "SMQ", "DIQ", "PAQ"]:
        if modules[mod] is not None:
            # 移除可能冲突的列
            drop_cols = [c for c in modules[mod].columns if c in merged.columns and c not in ["SEQN"]]
            mod_clean = modules[mod].drop(columns=drop_cols, errors='ignore')
            mod_clean = mod_clean.drop_duplicates(subset=["SEQN"], keep="first")
            merged = merged.merge(mod_clean, on="SEQN", how="left")
    
    # 最后添加周期标识
    merged["cycle"] = cycle_name
    merged["cycle_letter"] = letter
    
    print(f"  Merged: {len(merged)} rows, {len(merged.columns)} cols")
    return merged

def main():
    """主函数"""
    print("=" * 70)
    print("NHANES Full Database Merge")
    print("=" * 70)
    print(f"Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    
    all_data = []
    skipped = []
    
    for cycle in CYCLES:
        result = merge_cycle(cycle)
        if result is not None:
            all_data.append(result)
        else:
            skipped.append(cycle["name"])
    
    if not all_data:
        print("\nError: No data to merge!")
        return
    
    # 纵向合并
    print("\n" + "=" * 70)
    print("Concatenating all cycles...")
    combined = pd.concat(all_data, ignore_index=True)
    
    print(f"\nTotal samples: {len(combined):,}")
    print(f"Total variables: {len(combined.columns)}")
    print(f"Cycles merged: {combined['cycle'].nunique()}")
    
    # 保存
    output_file = RAW_DIR / f"nhanes_full_merged_{datetime.now().strftime('%Y%m%d')}.parquet"
    combined.to_parquet(output_file, index=False, engine='pyarrow')
    print(f"\nSaved: {output_file}")
    print(f"Size: {output_file.stat().st_size / 1024/1024:.2f} MB")
    
    # 生成报告
    report = {
        "timestamp": datetime.now().isoformat(),
        "total_samples": int(len(combined)),
        "total_variables": int(len(combined.columns)),
        "cycles_merged": int(combined['cycle'].nunique()),
        "skipped_cycles": skipped,
        "cycle_breakdown": {}
    }
    
    for cycle in combined['cycle'].unique():
        if pd.isna(cycle):
            continue
        cycle_data = combined[combined['cycle'] == cycle]
        report["cycle_breakdown"][cycle] = {
            "n": int(len(cycle_data)),
            "vars": int(len(cycle_data.columns))
        }
    
    report_file = OUTPUT_DIR / "merge_report.json"
    with open(report_file, 'w', encoding='utf-8') as f:
        json.dump(report, f, indent=2, ensure_ascii=False)
    
    print(f"Report: {report_file}")
    
    return combined, report

if __name__ == "__main__":
    import json
    main()
