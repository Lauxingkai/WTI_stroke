#!/usr/bin/env python3
"""
NHANES 补充下载脚本 - 下载1999-2002和2025-2026周期
"""
import sys
import os
sys.path.insert(0, r'D:\NHANES\scripts')

from nhanes_crawler import crawl_and_download_cycle, OUTPUT_ROOT
import time

# 缺失的周期
missing_cycles = [
    ("1999", "A"),  # 1999-2000
    ("2001", "B"),  # 2001-2002
    ("2025", "S"),  # 2025-2026
]

print("=" * 60)
print("NHANES 补充下载 - 完整周期")
print("=" * 60)

for start_year, letter in missing_cycles:
    print(f"\n正在下载 {start_year}-{int(start_year)+1}...")
    try:
        stats = crawl_and_download_cycle(start_year, letter)
        print(f"完成: 新增{stats['downloaded']}, 跳过{stats['skipped']}, 失败{stats['failed']}")
    except Exception as e:
        print(f"错误: {e}")
    time.sleep(2)

print("\n" + "=" * 60)
print("补充下载完成!")
print("=" * 60)
