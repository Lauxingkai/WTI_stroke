"""
NHANES数据质量检查脚本
检查各周期数据完整性、核心变量存在性、文件可读性
"""
import os
import pyreadstat
from pathlib import Path
from collections import defaultdict
import json

BASE_DIR = Path(r"D:\NHANES\Data")

# 周期-字母映射（实际文件命名）
CYCLE_CONFIG = {
    "1999-2000": {"letter": "", "note": "早期周期，命名不同"},
    "2001-2002": {"letter": "B", "note": ""},
    "2003-2004": {"letter": "C", "note": ""},
    "2005-2006": {"letter": "D", "note": ""},
    "2007-2008": {"letter": "E", "note": ""},
    "2009-2010": {"letter": "F", "note": ""},
    "2011-2012": {"letter": "G", "note": ""},
    "2013-2014": {"letter": "H", "note": ""},
    "2015-2016": {"letter": "I", "note": ""},
    "2017-2018": {"letter": "J", "note": ""},
    "2019-2020": {"letter": "P", "note": "COVID中断，排除"},
    "2021-2022": {"letter": "L", "note": "新采样设计"},
    "2023-2024": {"letter": "L", "note": "疑似重复下载"},
}

# 核心模块
CORE_MODULES = {
    "DEMO": ("Demographics", ["RIAGENDR", "RIDAGEYR", "RIDRETH1", "WTINT2YR", "SDMVPSU", "SDMVSTRA", "SEQN"]),
    "DR1TOT": ("Dietary", ["DR1TVK"]),
    "MCQ": ("Questionnaire", ["MCQ160E"]),
    "BMX": ("Examination", ["BMXBMI"]),
}

def check_file_exists(cycle, module):
    """检查核心文件是否存在"""
    config = CYCLE_CONFIG[cycle]
    letter = config["letter"]
    module_dir, _ = CORE_MODULES[module]
    
    # 尝试多种文件名模式
    patterns = [
        f"{module}_{letter}.XPT" if letter else f"{module}.XPT",
        f"{module}_{letter}.xpt" if letter else f"{module}.xpt",
    ]
    
    cycle_dir = BASE_DIR / cycle / module_dir
    if not cycle_dir.exists():
        return None
    
    for pattern in patterns:
        file_path = cycle_dir / pattern
        if file_path.exists():
            return str(file_path)
    
    # 模糊搜索
    for f in cycle_dir.glob("*.XPT"):
        if f.name.startswith(module):
            return str(f)
    
    return None

def check_data_quality(cycle):
    """检查单个周期的数据质量"""
    result = {
        "cycle": cycle,
        "files": {},
        "issues": [],
        "sample_sizes": {},
        "variables": {}
    }
    
    config = CYCLE_CONFIG[cycle]
    
    # 1. 文件完整性检查
    cycle_dir = BASE_DIR / cycle
    if not cycle_dir.exists():
        result["issues"].append(f"目录不存在: {cycle}")
        return result
    
    xpt_files = list(cycle_dir.rglob("*.XPT"))
    result["total_files"] = len(xpt_files)
    
    # 2. 核心模块检查
    for module in CORE_MODULES.keys():
        file_path = check_file_exists(cycle, module)
        result["files"][module] = {
            "exists": file_path is not None,
            "path": file_path
        }
        
        if file_path and os.path.exists(file_path):
            try:
                df, meta = pyreadstat.read_xport(file_path)
                result["sample_sizes"][module] = len(df)
                
                # 检查关键变量
                vars_found = []
                vars_missing = []
                for var in CORE_MODULES[module][1]:
                    if var in df.columns:
                        vars_found.append(var)
                    else:
                        vars_missing.append(var)
                
                result["variables"][module] = {
                    "found": vars_found,
                    "missing": vars_missing
                }
                
                if vars_missing:
                    result["issues"].append(f"{module}: 缺失关键变量 {vars_missing}")
                    
            except Exception as e:
                result["issues"].append(f"{module}: 读取失败 - {str(e)}")
        elif not file_path:
            result["issues"].append(f"{module}: 文件未找到")
    
    # 3. 检查2023-2024是否为重复数据
    if cycle == "2023-2024":
        demo_2122 = check_file_exists("2021-2022", "DEMO")
        demo_2324 = check_file_exists("2023-2024", "DEMO")
        if demo_2122 and demo_2324:
            # 比较MD5
            import hashlib
            def file_hash(path):
                h = hashlib.md5()
                with open(path, 'rb') as f:
                    for chunk in iter(lambda: f.read(8192), b''):
                        h.update(chunk)
                return h.hexdigest()
            
            hash_2122 = file_hash(demo_2122)
            hash_2324 = file_hash(demo_2324)
            if hash_2122 == hash_2324:
                result["issues"].append("警告: 2023-2024 DEMO与2021-2022完全相同，疑似重复下载")
    
    return result

def main():
    """主函数"""
    print("=" * 60)
    print("NHANES数据质量检查报告")
    print("=" * 60)
    
    all_results = []
    for cycle in CYCLE_CONFIG.keys():
        result = check_data_quality(cycle)
        all_results.append(result)
        
        status = "PASS" if not result["issues"] else "WARN/FAIL"
        print(f"\n[{status}] {cycle}")
        print(f"  总文件数: {result['total_files']}")
        for module, info in result["files"].items():
            if info["exists"]:
                size = result["sample_sizes"].get(module, "N/A")
                vars_info = result["variables"].get(module, {})
                missing = vars_info.get("missing", [])
                status_str = "OK" if not missing else f"MISSING:{missing}"
                print(f"  {module}: {size} rows [{status_str}]")
            else:
                print(f"  {module}: [NOT FOUND]")
        
        if result["issues"]:
            for issue in result["issues"]:
                print(f"  ! {issue}")
    
    # 生成JSON报告
    report_path = BASE_DIR.parent / "data_quality_report.json"
    with open(report_path, 'w', encoding='utf-8') as f:
        json.dump(all_results, f, indent=2, ensure_ascii=False)
    print(f"\n报告已保存: {report_path}")
    
    # 统计摘要
    print("\n" + "=" * 60)
    print("摘要")
    print("=" * 60)
    
    cycles_with_vk = sum(1 for r in all_results if "DR1TOT" in r["variables"] and not r["variables"]["DR1TOT"]["missing"])
    cycles_with_stroke = sum(1 for r in all_results if "MCQ" in r["variables"] and not r["variables"]["MCQ"]["missing"])
    
    print(f"含维生素K数据的周期: {cycles_with_vk}/13")
    print(f"含卒中数据的周期: {cycles_with_stroke}/13")
    
    # 推荐分析周期
    recommended = []
    for r in all_results:
        if (r["files"].get("DEMO", {}).get("exists") and
            r["files"].get("DR1TOT", {}).get("exists") and
            r["files"].get("MCQ", {}).get("exists")):
            critical_issues = [i for i in r["issues"] if "缺失" in i or "未找到" in i]
            if not critical_issues:
                recommended.append(r["cycle"])
    
    print(f"推荐分析周期: {recommended}")

if __name__ == "__main__":
    main()
