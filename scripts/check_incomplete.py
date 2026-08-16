#!/usr/bin/env python3
"""
NHANES 补全下载脚本 - 重新下载缺失的模块
"""
import sys
import time
sys.path.insert(0, r'D:\NHANES\scripts')

from nhanes_crawler import crawl_and_download_cycle, OUTPUT_ROOT, COMPONENTS

# 需要补全的周期
incomplete_cycles = [
    ("2019", "P"),  # 2019-2020
    ("2021", "Q"),  # 2021-2022
    ("2023", "R"),  # 2023-2024
    ("2025", "S"),  # 2025-2026
]

print("=" * 70)
print("NHANES 补全下载 - 下载所有模块")
print("=" * 70)

for start_year, letter in incomplete_cycles:
    print(f"\n处理周期: {start_year}-{int(start_year)+1}")
    
    # 重新下载所有模块
    for component_name, component_key in COMPONENTS.items():
        if component_name == "Limited Access":
            continue
        
        url = f"https://wwwn.cdc.gov/nchs/nhanes/search/DataPage.aspx?Component={component_key}&CycleBeginYear={start_year}"
        print(f"  检查 {component_name}...")
        
        try:
            import requests
            from bs4 import BeautifulSoup
            
            resp = requests.get(url, headers={'User-Agent': 'Mozilla/5.0'}, timeout=30)
            soup = BeautifulSoup(resp.text, 'html.parser')
            tables = soup.find_all('table')
            
            count = 0
            for table in tables:
                rows = table.find_all('tr')
                count += len(rows) - 1
            
            print(f"    {component_name}: {count} 个文件")
            
        except Exception as e:
            print(f"    错误: {e}")
        
        time.sleep(1)

print("\n检查完成。现在运行完整下载...")
