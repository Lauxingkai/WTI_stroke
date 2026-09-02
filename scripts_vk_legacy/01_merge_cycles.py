"""
NHANES多周期数据合并脚本
合并维生素K与卒中研究所需的核心数据模块
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
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# 周期配置（修正版）
CYCLES = [
    {"name": "1999-2000", "letter": "", "exclude": True, "reason": "无DR1TOT数据"},
    {"name": "2001-2002", "letter": "B", "exclude": True, "reason": "无MCQ数据"},
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
    {"name": "2023-2024", "letter": "L", "exclude": True, "reason": "疑似重复2021-2022"},
]

# 研究必需变量
REQUIRED_VARS = {
    "demo": ["SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH1", "WTINT2YR", "WTMEC2YR", "SDMVPSU", "SDMVSTRA"],
    "dr1tot": ["SEQN", "DR1TVK"],
    "mcq": ["SEQN", "MCQ160F"],  # 卒中变量：MCQ160F=卒中，MCQ160E=心梗（已废弃）
    "bmx": ["SEQN", "BMXBMI"],
}

def find_file(cycle_name, letter, module):
    """查找模块文件"""
    cycle_dir = BASE_DIR / cycle_name
    module_dirs = {
        "demo": "Demographics",
        "dr1tot": "Dietary",
        "mcq": "Questionnaire",
        "bmx": "Examination",
    }
    
    module_dir = module_dirs.get(module, module)
    search_dir = cycle_dir / module_dir
    
    if not search_dir.exists():
        return None
    
    # 构建文件名模式
    if letter:
        patterns = [f"{module.upper()}_{letter}.XPT", f"{module.upper()}_{letter}.xpt"]
    else:
        patterns = [f"{module.upper()}.XPT", f"{module.upper()}.xpt"]
    
    # 精确匹配
    for pattern in patterns:
        file_path = search_dir / pattern
        if file_path.exists():
            return str(file_path)
    
    # 模糊匹配
    for f in search_dir.glob("*.XPT"):
        if f.name.upper().startswith(module.upper()):
            return str(f)
    
    return None

def read_module(file_path, module_name):
    """读取模块并返回DataFrame"""
    if not file_path or not os.path.exists(file_path):
        return None

    try:
        # 尝试不同编码读取
        for encoding in ['utf-8', 'latin-1', 'cp1252']:
            try:
                df, meta = pyreadstat.read_xport(file_path, encoding=encoding)
                break
            except UnicodeDecodeError:
                continue
        else:
            # 最后尝试忽略错误
            df, meta = pyreadstat.read_xport(file_path, encoding='utf-8', errors='ignore')

        df["_source_module"] = module_name
        df["_source_file"] = os.path.basename(file_path)
        return df
    except Exception as e:
        print(f"  读取失败 {file_path}: {e}")
        return None

def merge_cycle(cycle_config):
    """合并单个周期的数据"""
    cycle_name = cycle_config["name"]
    letter = cycle_config["letter"]
    
    print(f"\n处理周期: {cycle_name}")
    
    # 检查是否排除
    if cycle_config.get("exclude"):
        print(f"  跳过: {cycle_config.get('reason', '未知原因')}")
        return None
    
    # 读取核心模块
    modules = {}
    for mod in ["demo", "dr1tot", "mcq", "bmx"]:
        file_path = find_file(cycle_name, letter, mod)
        df = read_module(file_path, mod)
        modules[mod] = df
        
        if df is None:
            print(f"  警告: {mod.upper()} 数据缺失")
        else:
            print(f"  {mod.upper()}: {len(df)} 行")
    
    # 检查必需模块
    if modules["demo"] is None or modules["dr1tot"] is None or modules["mcq"] is None:
        print(f"  跳过: 核心数据不完整")
        return None
    
    # 合并：以DEMO为基础，左连接其他模块
    # 先移除重复的周期标识列
    merged = modules["demo"].copy()
    for col in ["cycle", "cycle_letter"]:
        if col in merged.columns:
            merged = merged.drop(columns=[col])

    # 左连接DR1TOT（只保留必要列，避免重复）
    dr1tot_keep = ["SEQN"] + [c for c in REQUIRED_VARS["dr1tot"] if c != "SEQN"]
    dr1tot_clean = modules["dr1tot"][dr1tot_keep].drop_duplicates(subset=["SEQN"], keep="first")
    dr1tot_clean["cycle"] = cycle_name
    dr1tot_clean["cycle_letter"] = letter
    merged = merged.merge(dr1tot_clean, on="SEQN", how="left")

    # 左连接MCQ
    mcq_keep = ["SEQN"] + [c for c in REQUIRED_VARS["mcq"] if c != "SEQN"]
    mcq_clean = modules["mcq"][mcq_keep].drop_duplicates(subset=["SEQN"], keep="first")
    mcq_clean["cycle"] = cycle_name
    mcq_clean["cycle_letter"] = letter
    merged = merged.merge(mcq_clean, on="SEQN", how="left")

    # 左连接BMX（可选）
    if modules["bmx"] is not None:
        bmx_keep = ["SEQN"] + [c for c in REQUIRED_VARS["bmx"] if c != "SEQN"]
        bmx_clean = modules["bmx"][bmx_keep].drop_duplicates(subset=["SEQN"], keep="first")
        bmx_clean["cycle"] = cycle_name
        bmx_clean["cycle_letter"] = letter
        merged = merged.merge(bmx_clean, on="SEQN", how="left")
    
    print(f"  合并后: {len(merged)} 行, {len(merged.columns)} 列")
    
    return merged

def main():
    """主函数"""
    print("=" * 60)
    print("NHANES多周期数据合并")
    print("=" * 60)
    
    all_data = []
    skipped_cycles = []
    
    for cycle_config in CYCLES:
        result = merge_cycle(cycle_config)
        if result is not None:
            all_data.append(result)
        else:
            skipped_cycles.append(cycle_config["name"])
    
    if not all_data:
        print("\n错误: 没有可合并的数据")
        return
    
    # 纵向合并所有周期
    print("\n" + "=" * 60)
    print("合并所有周期...")
    combined = pd.concat(all_data, ignore_index=True)
    
    print(f"总样本量: {len(combined)} 人")
    print(f"总变量数: {len(combined.columns)} 个")
    print(f"周期数: {combined['cycle'].nunique()} 个")
    
    # 保存原始合并数据
    raw_output = OUTPUT_DIR / "nhanes_raw_merged.csv"
    combined.to_csv(raw_output, index=False, encoding='utf-8-sig')
    print(f"\n原始数据已保存: {raw_output}")
    
    # 生成质量报告
    report = generate_quality_report(combined)
    
    # 保存合并日志
    log_path = OUTPUT_DIR / "merge_log.txt"
    with open(log_path, 'w', encoding='utf-8') as f:
        f.write("NHANES数据合并日志\n")
        f.write("=" * 60 + "\n")
        f.write(f"生成时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
        f.write(f"成功合并周期: {len(all_data)} 个\n")
        f.write(f"跳过周期: {len(skipped_cycles)} 个\n")
        if skipped_cycles:
            f.write(f"跳过详情:\n")
            for cycle in skipped_cycles:
                cfg = next((c for c in CYCLES if c["name"] == cycle), {})
                reason = cfg.get("reason", "未指定")
                f.write(f"  - {cycle}: {reason}\n")
        f.write(f"\n最终样本量: {len(combined)}\n")
        f.write(f"输出文件: {raw_output}\n")
    print(f"合并日志已保存: {log_path}")
    
    return combined, report

def generate_quality_report(combined):
    """生成数据质量报告"""
    report = {
        "timestamp": datetime.now().isoformat(),
        "total_samples": len(combined),
        "total_variables": len(combined.columns),
        "cycles_merged": int(combined['cycle'].nunique()),
        "cycles": {}
    }
    
    # 确定卒中变量名（MCQ160F=卒中，MCQ160E=心梗）
    stroke_col = 'MCQ160F' if 'MCQ160F' in combined.columns else 'MCQ160E'

    # 各周期统计
    for cycle in combined['cycle'].unique():
        cycle_data = combined[combined['cycle'] == cycle]
        report["cycles"][cycle] = {
            "n": int(len(cycle_data)),
            "vk_missing_rate": float(cycle_data['DR1TVK'].isna().mean() * 100),
            "stroke_missing_rate": float(cycle_data[stroke_col].isna().mean() * 100) if stroke_col in cycle_data.columns else None,
            "stroke_prevalence": float((cycle_data[stroke_col] == 1).sum() / cycle_data[stroke_col].notna().sum() * 100) if stroke_col in cycle_data.columns and cycle_data[stroke_col].notna().any() else None
        }

    # 整体统计
    report["overall"] = {
        "vk_missing_rate": float(combined['DR1TVK'].isna().mean() * 100),
        "stroke_missing_rate": float(combined[stroke_col].isna().mean() * 100) if stroke_col in combined.columns else None,
        "female_pct": float((combined['RIAGENDR'] == 2).mean() * 100),
        "mean_age": float(combined['RIDAGEYR'].mean()),
        "mean_bmi": float(combined['BMXBMI'].mean()) if 'BMXBMI' in combined.columns else None,
        "stroke_variable": stroke_col,
    }
    
    # 保存报告
    report_path = OUTPUT_DIR / "quality_report.json"
    with open(report_path, 'w', encoding='utf-8') as f:
        import json
        json.dump(report, f, indent=2, ensure_ascii=False, default=str)
    
    print(f"质量报告已保存: {report_path}")
    return report

if __name__ == "__main__":
    main()
