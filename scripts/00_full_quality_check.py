"""
NHANES全库数据质量检查
检查所有周期、所有模块的数据完整性
"""
import os
import pyreadstat
from pathlib import Path
from collections import defaultdict
import json
from datetime import datetime

BASE_DIR = Path(r"D:\NHANES\Data")
OUTPUT_DIR = BASE_DIR.parent / "Processed" / "quality_reports"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# 完整周期配置（含字母映射）
CYCLES = [
    {"name": "1999-2000", "letter": "", "note": "早期周期"},
    {"name": "2001-2002", "letter": "B", "note": ""},
    {"name": "2003-2004", "letter": "C", "note": ""},
    {"name": "2005-2006", "letter": "D", "note": ""},
    {"name": "2007-2008", "letter": "E", "note": ""},
    {"name": "2009-2010", "letter": "F", "note": ""},
    {"name": "2011-2012", "letter": "G", "note": ""},
    {"name": "2013-2014", "letter": "H", "note": ""},
    {"name": "2015-2016", "letter": "I", "note": ""},
    {"name": "2017-2018", "letter": "J", "note": ""},
    {"name": "2019-2020", "letter": "P", "note": "COVID中断"},
    {"name": "2021-2022", "letter": "L", "note": "新采样设计"},
    {"name": "2023-2024", "letter": "L", "note": "疑似重复"},
]

# 核心模块定义
CORE_MODULES = ["DEMO", "DR1TOT", "DR2TOT", "MCQ", "BMX", "BPX", "SMQ", "DIQ", "PAQ"]

# 扩展模块列表（按周期可能不同）
EXTENDED_MODULES = {
    "LAB": "Laboratory",
    "EXAM": "Examination",
    "HIQ": "Questionnaire",
    "DEQ": "Questionnaire",
    "RXQ": "Questionnaire",
    "HEQ": "Questionnaire",
}

def find_module_files(cycle_dir, module_prefix):
    """查找模块文件"""
    if not cycle_dir.exists():
        return []
    
    files = []
    for f in cycle_dir.rglob(f"{module_prefix}*.XPT"):
        if f.is_file():
            files.append(str(f))
    
    # 按目录分组
    by_dir = defaultdict(list)
    for f in files:
        dir_name = Path(f).parent.name
        by_dir[dir_name].append(f)
    
    return by_dir

def check_cycle_quality(cycle_config):
    """检查单个周期的质量"""
    cycle_name = cycle_config["name"]
    letter = cycle_config["letter"]
    
    result = {
        "cycle": cycle_name,
        "letter": letter,
        "status": "unknown",
        "files": {},
        "modules": {},
        "issues": []
    }
    
    cycle_dir = BASE_DIR / cycle_name
    if not cycle_dir.exists():
        result["status"] = "missing"
        result["issues"].append("周期目录不存在")
        return result
    
    # 统计文件
    all_xpt = list(cycle_dir.rglob("*.XPT"))
    result["total_files"] = len(all_xpt)
    result["total_size_mb"] = sum(f.stat().st_size for f in all_xpt) / (1024 * 1024)
    
    # 检查核心模块
    core_checks = {}
    for mod in CORE_MODULES:
        mod_files = find_module_files(cycle_dir, mod)
        
        if mod_files:
            # 读取第一个文件检查
            sample_file = next(iter(mod_files.values()))[0]
            try:
                df, meta = pyreadstat.read_xport(sample_file)
                core_checks[mod] = {
                    "found": True,
                    "file_count": sum(len(v) for v in mod_files.values()),
                    "sample_rows": len(df),
                    "sample_cols": len(df.columns),
                    "sample_file": os.path.basename(sample_file)
                }
            except Exception as e:
                core_checks[mod] = {
                    "found": True,
                    "error": str(e),
                    "file_count": sum(len(v) for v in mod_files.values())
                }
                result["issues"].append(f"{mod}: 读取错误 - {e}")
        else:
            core_checks[mod] = {"found": False}
            if mod in ["DEMO", "BMX"]:  # DEMO和BMX是必需的
                result["issues"].append(f"{mod}: 文件缺失")
    
    result["modules"] = core_checks
    
    # 确定周期状态
    if cycle_name == "2019-2020":
        result["status"] = "excluded_covid"
    elif cycle_name == "2023-2024":
        result["status"] = "potential_duplicate"
    elif core_checks.get("DEMO", {}).get("found") and core_checks.get("BMX", {}).get("found"):
        result["status"] = "available"
    else:
        result["status"] = "partial"
    
    return result

def main():
    """主函数"""
    print("=" * 70)
    print("NHANES 全库数据质量检查")
    print("=" * 70)
    print(f"检查时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print(f"数据目录: {BASE_DIR}")
    print()
    
    all_results = []
    
    for cycle in CYCLES:
        print(f"\n检查周期: {cycle['name']}...")
        result = check_cycle_quality(cycle)
        all_results.append(result)
        
        status_icon = {
            "available": "[OK]",
            "partial": "[WARN]",
            "excluded_covid": "[SKIP]",
            "potential_duplicate": "[WARN]",
            "missing": "[MISS]"
        }.get(result["status"], "[?]")
        
        print(f"  状态: {status_icon} {result['status']}")
        print(f"  文件数: {result['total_files']}")
        print(f"  大小: {result['total_size_mb']:.2f} MB")
        
        if result["issues"]:
            for issue in result["issues"]:
                print(f"  ! {issue}")
    
    # 生成汇总报告
    summary = {
        "report_time": datetime.now().isoformat(),
        "total_cycles": len(CYCLES),
        "cycles_available": sum(1 for r in all_results if r["status"] == "available"),
        "cycles_partial": sum(1 for r in all_results if r["status"] == "partial"),
        "cycles_excluded": sum(1 for r in all_results if r["status"] in ["excluded_covid", "missing"]),
        "total_files": sum(r["total_files"] for r in all_results),
        "total_size_gb": sum(r["total_size_mb"] for r in all_results) / 1024,
        "cycles": all_results
    }
    
    # 保存报告
    summary_path = OUTPUT_DIR / "quality_summary.json"
    with open(summary_path, 'w', encoding='utf-8') as f:
        json.dump(summary, f, indent=2, ensure_ascii=False)
    
    print("\n" + "=" * 70)
    print("质量检查汇总")
    print("=" * 70)
    print(f"总周期数: {summary['total_cycles']}")
    print(f"可用周期: {summary['cycles_available']}")
    print(f"部分可用: {summary['cycles_partial']}")
    print(f"排除周期: {summary['cycles_excluded']}")
    print(f"总文件数: {summary['total_files']}")
    print(f"总大小: {summary['total_size_gb']:.2f} GB")
    print(f"\n报告已保存: {summary_path}")
    
    return summary

if __name__ == "__main__":
    main()
