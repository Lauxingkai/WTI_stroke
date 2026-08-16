#!/usr/bin/env python3
"""
NHANES 补全下载脚本 - 下载所有模块
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

BASE_URL = "https://wwwn.cdc.gov"

# 需要补全的周期
INCOMPLETE_CYCLES = [
    ("2019", "P", "2019-2020"),
]

COMPONENTS = {
    "Demographics": "Demographics",
    "Dietary": "Dietary",
    "Examination": "Examination",
    "Laboratory": "Laboratory",
    "Questionnaire": "Questionnaire",
}

OUTPUT_ROOT = Path(r"D:\NHANES\Data")
HEADERS = {'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'}
TIMEOUT = 60


def get_cycle_url(component, start_year):
    return f"{BASE_URL}/nchs/nhanes/search/DataPage.aspx?Component={component}&CycleBeginYear={start_year}"


def parse_data_page(html_content, start_year):
    soup = BeautifulSoup(html_content, 'html.parser')
    tables = soup.find_all('table')
    
    files = []
    for table in tables:
        rows = table.find_all('tr')
        for row in rows[1:]:
            cells = row.find_all('td')
            if len(cells) >= 3:
                data_link = cells[2].find('a')
                if data_link and data_link.get('href'):
                    link = data_link.get('href')
                    if '.xpt' in link.lower():
                        doc_link = cells[1].find('a')
                        doc_url = doc_link.get('href') if doc_link else None
                        files.append({
                            'xpt_link': link,
                            'doc_link': doc_url,
                            'year': start_year
                        })
    return files


def download_file(url, dest_path, max_retries=3):
    dest_path.parent.mkdir(parents=True, exist_ok=True)
    
    for attempt in range(max_retries):
        try:
            resp = requests.get(url, headers=HEADERS, timeout=TIMEOUT, stream=True)
            resp.raise_for_status()
            
            with open(dest_path, 'wb') as f:
                for chunk in resp.iter_content(chunk_size=8192):
                    if chunk:
                        f.write(chunk)
            return True
            
        except Exception as e:
            if attempt < max_retries - 1:
                time.sleep(2)
            else:
                return False
    return False


def crawl_and_download_cycle(start_year, letter, cycle_name):
    cycle_folder = OUTPUT_ROOT / cycle_name
    cycle_folder.mkdir(parents=True, exist_ok=True)
    
    logger.info(f"\n{'='*60}")
    logger.info(f"补全下载: {cycle_name}")
    logger.info(f"{'='*60}")
    
    downloaded = 0
    skipped = 0
    failed = 0
    
    for component_name, component_key in COMPONENTS.items():
        component_path = cycle_folder / component_name
        component_path.mkdir(exist_ok=True)
        
        url = get_cycle_url(component_key, start_year)
        logger.info(f"\n处理 {component_name}...")
        
        try:
            resp = requests.get(url, headers=HEADERS, timeout=TIMEOUT)
            resp.raise_for_status()
            
            files = parse_data_page(resp.text, start_year)
            logger.info(f"找到 {len(files)} 个文件")
            
            for file_info in files:
                xpt_url = BASE_URL + file_info['xpt_link']
                doc_url = BASE_URL + file_info['doc_link'] if file_info['doc_link'] else None
                
                filename = Path(file_info['xpt_link']).name
                dest_xpt = component_path / filename
                
                if not dest_xpt.exists():
                    if download_file(xpt_url, dest_xpt):
                        downloaded += 1
                else:
                    logger.info(f"[SKIP] {filename}")
                    skipped += 1
                
                # 下载Codebook
                if doc_url:
                    doc_filename = filename.replace('.XPT', '_L.htm').replace('.xpt', '_L.htm')
                    dest_doc = component_path / doc_filename
                    if not dest_doc.exists():
                        download_file(doc_url, dest_doc)
                
                time.sleep(0.3)
                
        except Exception as e:
            logger.error(f"{component_name} 错误: {e}")
            failed += 1
        
        time.sleep(1)
    
    return {'downloaded': downloaded, 'skipped': skipped, 'failed': failed}


def main():
    logger.info("=" * 60)
    logger.info("NHANES 补全下载启动")
    logger.info("=" * 60)
    
    for start_year, letter, cycle_name in INCOMPLETE_CYCLES:
        stats = crawl_and_download_cycle(start_year, letter, cycle_name)
        logger.info(f"\n{cycle_name} 完成:")
        logger.info(f"  新增: {stats['downloaded']}")
        logger.info(f"  跳过: {stats['skipped']}")
        logger.info(f"  失败: {stats['failed']}")
    
    logger.info("\n全部补全下载完成!")


if __name__ == "__main__":
    main()
