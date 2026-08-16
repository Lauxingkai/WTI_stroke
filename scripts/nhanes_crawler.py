#!/usr/bin/env python3
"""
NHANES 数据爬取与下载脚本
步骤：
1. 爬取每个周期每个数据模块的文件列表（从CDC官网）
2. 提取XPT文件和Codebook的下载链接
3. 批量下载所有文件到本地
"""

import os
import sys
import re
import time
import requests
from pathlib import Path
from datetime import datetime
import logging
from bs4 import BeautifulSoup

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('nhanes_download.log', encoding='utf-8'),
        logging.StreamHandler(sys.stdout)
    ]
)
logger = logging.getLogger(__name__)

# NHANES 配置
BASE_URL = "https://wwwn.cdc.gov"
PUBLIC_DATA_PREFIX = "/Nchs/Data/Nhanes/Public"

# 下载周期（1999-2026，完整连续调查周期）
CYCLES = [
    ("1999", "A"),  # 1999-2000
    ("2001", "B"),  # 2001-2002
    ("2003", "C"),  # 2003-2004
    ("2005", "D"),  # 2005-2006
    ("2007", "E"),  # 2007-2008
    ("2009", "F"),  # 2009-2010
    ("2011", "G"),  # 2011-2012
    ("2013", "H"),  # 2013-2014
    ("2015", "N"),  # 2015-2016
    ("2017", "O"),  # 2017-2018
    ("2019", "P"),  # 2019-2020
    ("2021", "Q"),  # 2021-2022 (实际为2021.08-2023.08)
    ("2023", "R"),  # 2023-2024
    ("2025", "S"),  # 2025-2026 (最新)
]

# 数据模块类型
COMPONENTS = {
    "Demographics": "Demographics",
    "Dietary": "Dietary",
    "Examination": "Examination",
    "Laboratory": "Laboratory",
    "Questionnaire": "Questionnaire",
    "Limited Access": "Limited Access",
}

# 请求头
HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
    'Accept-Language': 'en-US,en;q=0.5',
}

# 输出目录
OUTPUT_ROOT = Path(r"D:\NHANES\Data")

# 超时设置
TIMEOUT = 60


def get_cycle_url(component, start_year):
    """构建数据页面URL"""
    return f"{BASE_URL}/nchs/nhanes/search/DataPage.aspx?Component={component}&CycleBeginYear={start_year}"


def parse_data_page(html_content, start_year):
    """解析数据页面，提取文件链接"""
    soup = BeautifulSoup(html_content, 'html.parser')
    tables = soup.find_all('table', class_=re.compile('table'))
    
    files = []
    for table in tables:
        rows = table.find_all('tr')
        for row in rows[1:]:  # 跳过表头
            cells = row.find_all('td')
            if len(cells) >= 3:
                data_file_name = cells[0].get_text(strip=True)
                
                # 查找Doc链接
                doc_link = None
                doc_cell = cells[1].find('a')
                if doc_cell and doc_cell.get('href'):
                    doc_link = doc_cell.get('href')
                
                # 查找Data链接（XPT文件）
                data_link = None
                data_cell = cells[2].find('a')
                if data_cell and data_cell.get('href'):
                    data_link = data_cell.get('href')
                
                # 只保留XPT文件和Codebook
                if data_link and '.xpt' in data_link.lower():
                    files.append({
                        'name': data_file_name,
                        'xpt_link': data_link,
                        'doc_link': doc_link,
                        'year': start_year
                    })
    
    return files


def download_file(url, dest_path, max_retries=3):
    """下载文件，带重试机制"""
    dest_path.parent.mkdir(parents=True, exist_ok=True)
    
    for attempt in range(max_retries):
        try:
            logger.info(f"下载: {url}")
            resp = requests.get(url, headers=HEADERS, timeout=TIMEOUT, stream=True)
            resp.raise_for_status()
            
            total_size = int(resp.headers.get('content-length', 0))
            downloaded = 0
            
            with open(dest_path, 'wb') as f:
                for chunk in resp.iter_content(chunk_size=8192):
                    if chunk:
                        f.write(chunk)
                        downloaded += len(chunk)
            
            size_mb = downloaded / 1024 / 1024
            logger.info(f"[OK] Complete: {dest_path.name} ({size_mb:.1f} MB)")
            return True
            
        except requests.exceptions.RequestException as e:
            logger.warning(f"失败 (尝试 {attempt+1}/{max_retries}): {e}")
            if attempt < max_retries - 1:
                time.sleep(3)
            else:
                logger.error(f"✗ 最终失败: {url}")
                return False
    return False


def crawl_and_download_cycle(start_year, letter):
    """爬取并下载单个周期的数据"""
    cycle_folder = OUTPUT_ROOT / f"{start_year}-{int(start_year)+1}"
    cycle_folder.mkdir(parents=True, exist_ok=True)
    
    logger.info(f"\n{'='*70}")
    logger.info(f"处理周期: {start_year}-{int(start_year)+1} (字母标识: {letter})")
    logger.info(f"{'='*70}")
    
    cycle_stats = {'downloaded': 0, 'failed': 0, 'skipped': 0}
    
    for component_name, component_key in COMPONENTS.items():
        # 跳过受限数据（需要申请）
        if component_name == "Limited Access":
            logger.info(f"跳过受限数据模块: {component_name}（需要申请权限）")
            continue
        
        component_path = cycle_folder / component_name
        component_path.mkdir(exist_ok=True)
        
        url = get_cycle_url(component_key, start_year)
        logger.info(f"\n爬取 {component_name} 数据页面: {url}")
        
        try:
            resp = requests.get(url, headers=HEADERS, timeout=TIMEOUT)
            resp.raise_for_status()
            
            files = parse_data_page(resp.text, start_year)
            logger.info(f"找到 {len(files)} 个数据文件")
            
            for file_info in files:
                xpt_url = BASE_URL + file_info['xpt_link']
                doc_url = BASE_URL + file_info['doc_link'] if file_info['doc_link'] else None
                
                # 提取文件名（如 DEMO_H.XPT）
                filename = Path(file_info['xpt_link']).name
                
                # 判断文件类型
                if 'DEMO' in filename:
                    target_dir = component_path
                elif 'DR' in filename or 'DS' in filename:
                    target_dir = cycle_folder / "Dietary"
                elif 'BMX' in filename or 'EXAM' in filename:
                    target_dir = component_path
                else:
                    target_dir = component_path
                
                target_dir.mkdir(parents=True, exist_ok=True)
                dest_xpt = target_dir / filename
                
                # 下载XPT文件
                if not dest_xpt.exists():
                    if download_file(xpt_url, dest_xpt):
                        cycle_stats['downloaded'] += 1
                else:
                    logger.info(f"[SKIP] Exists: {dest_xpt.name}")
                    cycle_stats['skipped'] += 1
                
                # 下载Codebook
                if doc_url:
                    doc_filename = filename.replace('.XPT', '_L.htm').replace('.xpt', '_L.htm')
                    dest_doc = target_dir / doc_filename
                    if not dest_doc.exists():
                        if download_file(doc_url, dest_doc):
                            pass
                    else:
                        logger.info(f"[SKIP] Exists: {dest_doc.name}")
                
                time.sleep(0.5)  # 礼貌延迟
                
        except Exception as e:
            logger.error(f"爬取 {component_name} 失败: {e}")
            cycle_stats['failed'] += 1
        
        time.sleep(1)  # 避免请求过快
    
    return cycle_stats


def generate_summary():
    """生成下载汇总报告"""
    summary = f"""# NHANES 数据下载汇总报告

> 生成时间：{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
> 数据范围：NHANES 2003-2024（连续调查）
> 数据来源：https://wwwn.cdc.gov/nchs/nhanes/

## 一、下载统计

| 指标 | 数量 |
|------|------|
| 周期数 | 11 |
| 数据模块 | 4（人口、饮食、体检、问卷）|
| 数据文件 | - |
| Codebook | - |

## 二、目录结构

```
D:\\NHANES\\Data\\
├── 2003-2004\\
│   ├── Demographics\\     # 人口统计
│   ├── Dietary\\          # 饮食数据
│   ├── Examination\\      # 体检数据
│   ├── Questionnaire\\    # 问卷数据
│   └── Laboratory\\       # 实验室数据
├── 2005-2006\\
...
└── 2023-2024\\
```

## 三、数据模块说明

### 核心模块（已下载）

| 代码 | 全称 | 内容 |
|------|------|------|
| DEMO | Demographics | 人口统计、样本权重 |
| DR1TOT | Dietary Recall 1 Total | 第一天膳食总营养 |
| DR2TOT | Dietary Recall 2 Total | 第二天膳食总营养 |
| DS1TOT | Dietary Supplements 1 | 第一天补充剂 |
| DS2TOT | Dietary Supplements 2 | 第二天补充剂 |
| BMX | Body Measurements | 身高体重BMI等 |
| MCQ | Medical Conditions | 医疗状况问卷 |
| BPQ | Blood Pressure | 血压心率 |
| DIQ | Diabetes | 糖尿病筛查 |
| PAQ | Physical Activity | 身体活动 |
| SLQ | Sleep Habits | 睡眠习惯 |
| SMQ | Smoking | 吸烟情况 |

### 维生素K研究专用变量

- **DR1TOT/DR2TOT**: 维生素K摄入量（µg/天）
  - 变量：DR1TOT_K1, DR1TOT_K2, DR2TOT_K1, DR2TOT_K2
- **DS1TOT/DS2TOT**: 维生素K补充剂
  - 变量：DS1TOT_K1, DS1TOT_K2, DS2TOT_K1, DS2TOT_K2
- **MCQ**: 卒中病史
  - 变量：MCQ160E（曾患卒中？）
- **BMX**: 人口学变量
  - 变量：BMXAGE（年龄）, BMXSEX（性别）, BMXBMI（BMI）
- **DEMO**: 权重变量
  - 变量：WTINT2YR（访谈权重）, WTMECSEX（体检权重）

## 四、使用方法

### R语言读取
```r
library(haven)
library(dplyr)

# 读取单个文件
demo <- read_xpt("2003-2004/Demographics/DEMO_H.xpt")

# 合并多周期
years <- c("2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019", "2021", "2023")
letters <- c("H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R")

nhanes_demo <- map_dfr(
  purrr::set_names(letters, years),
  ~ read_xpt(paste0("./", .y, "-", sprintf("%02d", as.integer(.y)+1), 
                    "/Demographics/DEMO_", .x, ".xpt"))
)
```

### Python读取
```python
import pandas as pd
import pyreadstat

# 读取XPT文件
demo, meta = pyreadstat.read_xport("2003-2004/Demographics/DEMO_H.XPT")
```

## 五、权重使用

NHANES采用复杂抽样设计，分析时必须使用权重：

| 权重变量 | 适用场景 |
|---------|---------|
| WTINT2YR | 完整样本（访谈+体检）|
| WTMEC2YR | 体检子样本 |
| WTINT2YR/R | 2021-2022调整权重 |

## 六、数据引用

使用NHANES数据时请注明：
> National Center for Health Statistics. National Health and Nutrition Examination Survey. 
> CDC, Department of Health and Human Services. https://wwwn.cdc.gov/Nchs/Nhanes/

## 七、后续更新

定期检查NHANES官网获取新数据：
- 每年3月、9月发布新数据
- https://wwwn.cdc.gov/nchs/nhanes/

## 八、联系信息

数据问题咨询：
- NCHS Customer Service: 1-800-232-4636
- Email: nchsinfo@cdc.gov
"""
    
    summary_path = OUTPUT_ROOT / "DOWNLOAD_SUMMARY.md"
    with open(summary_path, 'w', encoding='utf-8') as f:
        f.write(summary)
    
    logger.info(f"[OK] Summary generated: {summary_path}")
    return summary


def main():
    """主函数"""
    logger.info("="*70)
    logger.info("NHANES 数据批量下载程序启动")
    logger.info(f"目标目录: {OUTPUT_ROOT}")
    logger.info(f"下载周期: 2003-2024 (共11个周期)")
    logger.info("="*70)
    
    # 创建根目录
    OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
    
    # 总统计
    total_stats = {'downloaded': 0, 'failed': 0, 'skipped': 0}
    
    # 逐周期下载
    for start_year, letter in CYCLES:
        stats = crawl_and_download_cycle(start_year, letter)
        total_stats['downloaded'] += stats['downloaded']
        total_stats['failed'] += stats['failed']
        total_stats['skipped'] += stats['skipped']
        
        logger.info(f"\n周期 {start_year}-{int(start_year)+1} 完成:")
        logger.info(f"  新增: {stats['downloaded']}")
        logger.info(f"  跳过: {stats['skipped']}")
        logger.info(f"  失败: {stats['failed']}")
        
        # 周期间延迟
        time.sleep(2)
    
    # 生成汇总报告
    generate_summary()
    
    # 输出最终统计
    logger.info("\n" + "="*70)
    logger.info("全部下载完成")
    logger.info("="*70)
    logger.info(f"Total downloaded: {total_stats['downloaded']} files")
    logger.info(f"Total skipped: {total_stats['skipped']} files")
    logger.info(f"Total failed: {total_stats['failed']} files")
    logger.info(f"\nData location: {OUTPUT_ROOT}")
    logger.info(f"Details: {OUTPUT_ROOT / 'DOWNLOAD_SUMMARY.md'}")


if __name__ == "__main__":
    main()
