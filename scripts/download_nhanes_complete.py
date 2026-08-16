#!/usr/bin/env python3
"""
NHANES 数据批量下载脚本
目标：下载 2003-2024 全部周期的公共数据模块 + Codebook 文档
作者：Agnes AI Agent
日期：2026-08-16
"""

import os
import sys
import time
import requests
from pathlib import Path
from datetime import datetime
import logging

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
BASE_URL = "https://wwwn.cdc.gov/Nchs/Data/Nhanes"
PUBLIC_DATA_PATH = f"{BASE_URL}/Public"
CODEBOOK_BASE = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public"

# 下载周期（2003-2024）
CYCLES = [
    "2003", "2004", "2005", "2006", "2007", "2008",
    "2009", "2010", "2011", "2012", "2013", "2014",
    "2015", "2016", "2017", "2018", "2019", "2020",
    "2021", "2022", "2023", "2024"
]

# 核心数据模块（XPT文件）
CORE_MODULES = [
    "DEMO",   # Demographics
    "DR1TOT", # Dietary First Day Total
    "DR2TOT", # Dietary Second Day Total
    "DS1TOT", # Supplements First Day
    "DS2TOT", # Supplements Second Day
    "BMX",    # Body Measurements
    "MCQ",    # Medical Conditions
    "BPQ",    # Blood Pressure
    "DIQ",    # Diabetes
    "PAQ",    # Physical Activity
    "SLQ",    # Sleep
    "SMQ",    # Smoking
]

# 扩展数据模块（可选）
EXPANDED_MODULES = [
    "ALQ",    # Alcohol
    "HIQ",    # Hearing
    "DEQ",    # Depression
    "HEQ",    # Hepatitis
    "ACQ",    # Acculturation
    "INX",    # Indicators
    "FAM",    # Family
    "RIAX",   # Response Time
]

# 输出目录
OUTPUT_ROOT = Path(r"D:\NHANES\Data")

# 请求头
HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
}

# 超时设置（秒）
TIMEOUT = 30


def get_cycle_folder(cycle_year):
    """根据年份确定周期文件夹名称"""
    # NHANES 使用两年周期
    start_year = int(cycle_year)
    if start_year % 2 == 1:  # 奇数年份
        end_year = start_year + 1
    else:
        end_year = start_year
    
    # 返回标准格式如 "2003-2004"
    return f"{start_year:04d}-{end_year:04d}"


def get_data_path(cycle_str, module):
    """构建数据文件路径"""
    # 从cycle_str提取开始年份，如 "2003-2004" -> "2003"
    start_year = cycle_str.split("-")[0]
    return Path(PUBLIC_DATA_PATH) / start_year / "DataFiles"


def get_codebook_path(cycle_str, module):
    """构建Codebook HTML路径"""
    start_year = cycle_str.split("-")[0]
    return f"{CODEBOOK_BASE}/{start_year}/DataFiles/{module}_L.htm"


def get_xpt_url(cycle_str, module):
    """构建XPT文件下载URL"""
    start_year = cycle_str.split("-")[0]
    # NHANES XPT文件命名规则：模块代码_字母标识.XPT
    # 字母标识：H=2003-2004, I=2005-2006, J=2007-2008等
    module_map = {
        "2003": "H", "2004": "H",
        "2005": "I", "2006": "I",
        "2007": "J", "2008": "J",
        "2009": "K", "2010": "K",
        "2011": "L", "2012": "L",
        "2013": "M", "2014": "M",
        "2015": "N", "2016": "N",
        "2017": "O", "2018": "O",
        "2019": "P", "2020": "P",
        "2021": "Q", "2022": "Q",
        "2023": "R", "2024": "R"
    }
    
    letter = module_map.get(start_year, "H")
    filename = f"{module}_{letter}.XPT"
    url = f"https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/{start_year}/DataFiles/{filename}"
    return url


def get_codebook_url(cycle_str, module):
    """构建Codebook HTML URL"""
    start_year = cycle_str.split("-")[0]
    # Codebook文件名通常是 MODULE_L.htm
    filename = f"{module}_L.htm"
    url = f"https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/{start_year}/DataFiles/{filename}"
    return url


def download_file(url, dest_path, max_retries=3):
    """下载文件，带重试机制"""
    dest_path.parent.mkdir(parents=True, exist_ok=True)
    
    for attempt in range(max_retries):
        try:
            logger.info(f"下载: {url}")
            response = requests.get(url, headers=HEADERS, timeout=TIMEOUT, stream=True)
            response.raise_for_status()
            
            # 获取文件大小
            total_size = int(response.headers.get('content-length', 0))
            downloaded = 0
            
            with open(dest_path, 'wb') as f:
                for chunk in response.iter_content(chunk_size=8192):
                    if chunk:
                        f.write(chunk)
                        downloaded += len(chunk)
                        # 显示进度
                        if total_size > 0:
                            percent = downloaded * 100 / total_size
                            logger.debug(f"进度: {percent:.1f}%")
            
            logger.info(f"✓ 下载完成: {dest_path.name} ({downloaded / 1024 / 1024:.1f} MB)")
            return True
            
        except requests.exceptions.RequestException as e:
            logger.warning(f"下载失败 (尝试 {attempt + 1}/{max_retries}): {e}")
            if attempt < max_retries - 1:
                time.sleep(2)  # 等待后重试
            else:
                logger.error(f"✗ 下载失败: {url}")
                return False
    
    return False


def create_directory_structure():
    """创建目录结构"""
    logger.info("创建目录结构...")
    
    # 根目录
    OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
    
    # 各周期目录
    for cycle in CYCLES:
        cycle_folder = get_cycle_folder(cycle)
        cycle_path = OUTPUT_ROOT / cycle_folder
        
        # 子目录
        (cycle_path / "Demographics").mkdir(exist_ok=True)
        (cycle_path / "Dietary").mkdir(exist_ok=True)
        (cycle_path / "Examination").mkdir(exist_ok=True)
        (cycle_path / "Laboratory").mkdir(exist_ok=True)
        (cycle_path / "Questionnaire").mkdir(exist_ok=True)
        (cycle_path / "Documentation").mkdir(exist_ok=True)
    
    logger.info(f"✓ 目录结构创建完成，位于: {OUTPUT_ROOT}")


def download_cycle_data(cycle_str, modules, category):
    """下载单个周期的数据"""
    cycle_folder = get_cycle_folder(cycle_str)
    cycle_path = OUTPUT_ROOT / cycle_folder
    category_path = cycle_path / category
    
    logger.info(f"\n{'='*60}")
    logger.info(f"下载周期: {cycle_folder} - {category}数据")
    logger.info(f"{'='*60}")
    
    downloaded = []
    failed = []
    
    for module in modules:
        # XPT文件
        xpt_url = get_xpt_url(cycle_str, module)
        xpt_dest = category_path / f"{module}_{cycle_str}.XPT"
        
        if not xpt_dest.exists():
            if download_file(xpt_url, xpt_dest):
                downloaded.append(module)
            else:
                failed.append(module)
        else:
            logger.info(f"✓ 已存在: {xpt_dest.name}，跳过")
            downloaded.append(module)
        
        # Codebook HTML
        codebook_url = get_codebook_url(cycle_str, module)
        codebook_dest = category_path / f"{module}_codebook.html"
        
        if not codebook_dest.exists():
            if download_file(codebook_url, codebook_dest):
                pass  # Codebook已下载
        else:
            logger.info(f"✓ 已存在: {codebook_dest.name}，跳过")
        
        time.sleep(0.5)  # 避免请求过快
    
    return downloaded, failed


def generate_readme():
    """生成使用说明文档"""
    readme_content = """# NHANES 数据归档说明

> 生成日期：2026-08-16
> 数据范围：NHANES 2003-2024（连续调查）
> 数据来源：美国CDC国家健康与营养检查调查

## 一、目录结构

```
D:\\NHANES\\Data\\
├── 2003-2004\\
│   ├── Demographics\\      # 人口统计学数据
│   │   ├── DEMO_2003.XPT   # 主数据文件
│   │   └── DEMO_L.htm      # Codebook
│   ├── Dietary\\           # 饮食数据
│   ├── Examination\\       # 体检数据
│   ├── Laboratory\\        # 实验室数据
│   ├── Questionnaire\\     # 问卷数据
│   └── Documentation\\     # 文档说明
├── 2005-2006\\
...
└── 2023-2024\\
```

## 二、数据模块说明

### 核心模块（必选）

| 模块 | 全称 | 内容 |
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

### 维生素K研究专用

- **DR1TOT/DR2TOT**: 维生素K摄入量（µg/天）
- **DS1TOT/DS2TOT**: 维生素K补充剂
- **MCQ**: 卒中病史（MCQ160E）
- **BMX**: 人口学变量
- **DEMO**: 权重变量

## 三、使用方法

### R语言读取
```r
library(haven)
# 读取单个周期
demo <- read_xpt("2003-2004/Demographics/DEMO_2003.XPT")

# 合并多周期
library(dplyr)
nhanes_data <- map_dfr(
  c("2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019", "2021", "2023"),
  ~ read_xpt(paste0("./20", .x, "-", sprintf("%02d", as.integer(.x)+1), "/Demographics/DEMO_", .x, ".XPT"))
)
```

### Python读取
```python
import pandas as pd
# 注意：pandas默认不支持XPT，需用pyreadstat
import pyreadstat

demo, meta = pyreadstat.read_sas7bdat("2003-2004/Demographics/DEMO_2003.XPT")
```

## 四、权重使用

NHANES采用复杂抽样设计，分析时必须使用权重：

- **MEC Exam Weight**: WTMECSEX（体检子样本）
- **Full Sample Weight**: WTINT2YR（完整样本，两年周期）
- **Adjusted Weight**: WTSAF2YR（调整权重）

## 五、限制说明

1. **受限数据**：部分实验室数据（如汞、铅等）需要申请访问权限
2. **隐私保护**：数据已做脱敏处理，不可识别个人身份
3. **引用要求**：使用数据需在论文中声明

## 六、更新检查

定期检查NHANES官网获取新数据：
- https://wwwn.cdc.gov/nchs/nhanes/
- 每年3月、9月发布新数据

## 七、联系信息

数据问题：NCHS Customer Service
- 电话：1-800-232-4636
- 邮箱：nchsinfo@cdc.gov
"""
    
    readme_path = OUTPUT_ROOT / "README.md"
    with open(readme_path, 'w', encoding='utf-8') as f:
        f.write(readme_content)
    
    logger.info(f"✓ 使用说明已生成: {readme_path}")


def main():
    """主函数"""
    logger.info("="*60)
    logger.info("NHANES 数据批量下载程序启动")
    logger.info(f"目标目录: {OUTPUT_ROOT}")
    logger.info(f"下载周期: 2003-2024")
    logger.info("="*60)
    
    # 创建目录
    create_directory_structure()
    
    # 下载核心数据
    all_downloaded = []
    all_failed = []
    
    for cycle in CYCLES:
        # 核心模块下载
        demo_down, demo_fail = download_cycle_data(
            cycle, CORE_MODULES, "Demographics"
        )
        all_downloaded.extend(demo_down)
        all_failed.extend(demo_fail)
        
        # 饮食数据
        dietary_modules = ["DR1TOT", "DR2TOT", "DS1TOT", "DS2TOT"]
        diet_down, diet_fail = download_cycle_data(
            cycle, dietary_modules, "Dietary"
        )
        all_downloaded.extend(diet_down)
        all_failed.extend(diet_fail)
        
        # 体检数据
        exam_modules = ["BMX"]
        exam_down, exam_fail = download_cycle_data(
            cycle, exam_modules, "Examination"
        )
        all_downloaded.extend(exam_down)
        all_failed.extend(exam_fail)
        
        # 问卷数据
        question_modules = ["MCQ", "BPQ", "DIQ", "PAQ", "SLQ", "SMQ"]
        quest_down, quest_fail = download_cycle_data(
            cycle, question_modules, "Questionnaire"
        )
        all_downloaded.extend(quest_down)
        all_failed.extend(quest_fail)
        
        logger.info(f"\n周期 {cycle}: 成功 {len(all_downloaded)}, 失败 {len(all_failed)}")
    
    # 生成说明文档
    generate_readme()
    
    # 输出汇总
    logger.info("\n" + "="*60)
    logger.info("下载完成汇总")
    logger.info("="*60)
    logger.info(f"成功下载: {len(all_downloaded)} 个文件")
    logger.info(f"失败文件: {len(all_failed)} 个")
    
    if all_failed:
        logger.warning("\n失败列表:")
        for f in all_failed:
            logger.warning(f"  - {f}")
    
    logger.info("\n请检查 D:\\NHANES\\Data\\ 目录确认下载结果")
    logger.info("详细说明见 D:\\NHANES\\Data\\README.md")


if __name__ == "__main__":
    main()
