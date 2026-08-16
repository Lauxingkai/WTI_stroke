"""playwright 自动化下载 NHANES XPT v3（诊断 + <a> 点击下载）
用法: python scripts/02_download_playwright.py [--test | --all]
"""
import sys, time, os
from playwright.sync_api import sync_playwright

BASE = "https://wwwn.cdc.gov/Nchs/Nhanes"
OUT = "data/raw"
os.makedirs(OUT, exist_ok=True)

def main():
    mode = sys.argv[1] if len(sys.argv) > 1 else "--test"
    with sync_playwright() as p:
        browser = p.chromium.launch(channel="msedge", headless=False)
        ctx = browser.new_context(accept_downloads=True,
                                  viewport={"width": 1280, "height": 900})
        page = ctx.new_page()

        # 1) 访问 CDC 首页，诊断挑战状态
        print("[1] 访问 CDC 首页...", flush=True)
        try:
            page.goto(BASE, wait_until="domcontentloaded", timeout=60_000)
        except Exception as e:
            print("  首页异常:", e, flush=True)
        time.sleep(20)
        try:
            title = page.title()
            cur_url = page.url
            content = page.content()[:300]
        except Exception as e:
            title, cur_url, content = "?", "?", str(e)
        print(f"  页面标题: {title[:80]}", flush=True)
        print(f"  当前URL: {cur_url[:100]}", flush=True)
        print(f"  内容开头: {content[:120]!r}", flush=True)
        cookies = [c["name"] for c in ctx.cookies()]
        print(f"  cookies: {cookies}", flush=True)

        # 2) 用 <a> 点击方式触发下载（页面不导航关闭）
        url = f"{BASE}/2003-2004/DEMO_C.XPT"
        dest = os.path.join(OUT, "DEMO_C.XPT")
        print(f"[2] <a> 点击下载测试: DEMO_C.XPT", flush=True)
        ok = False
        for attempt in range(3):
            try:
                with page.expect_download(timeout=90_000) as dl_info:
                    page.evaluate("""(u) => {
                        const a = document.createElement('a');
                        a.href = u; a.download = '';
                        document.body.appendChild(a); a.click(); a.remove();
                    }""", url)
                dl = dl_info.value
                dl.save_as(dest)
                ok = True
                print(f"  ✅ 下载事件: {dl.suggested_filename}", flush=True)
                break
            except Exception as e:
                print(f"  第 {attempt+1} 次失败: {type(e).__name__}: {str(e)[:100]}", flush=True)
                time.sleep(8)

        if ok and os.path.exists(dest):
            sz = os.path.getsize(dest)
            with open(dest, "rb") as f:
                head = f.read(16)
            print(f"  大小: {sz} bytes, 文件头: {head[:12]!r}", flush=True)
            is_bin = not (head[:15].lstrip().lower().startswith(b"<!doctype") or head[:15].lstrip().lower().startswith(b"<html"))
            print("  XPT 有效:", is_bin, flush=True)
        else:
            print("  ❌ 下载失败", flush=True)
        browser.close()

if __name__ == "__main__":
    main()
