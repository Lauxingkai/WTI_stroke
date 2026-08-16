"""playwright fetch 下载 K 周期（2019-2020 旧式路径）"""
import time, os
from playwright.sync_api import sync_playwright

BASE = "https://wwwn.cdc.gov/Nchs/Nhanes"
OUT = "data/raw"
os.makedirs(OUT, exist_ok=True)

JS_FETCH = """
async (u) => {
  const r = await fetch(u, {credentials: 'include'});
  if (!r.ok) return {status: r.status, len: 0, bytes: []};
  const buf = await r.arrayBuffer();
  return {status: r.status, len: buf.byteLength, bytes: Array.from(new Uint8Array(buf))};
}
"""

def is_html(b: bytes) -> bool:
    return b[:15].lstrip().lower().startswith(b"<!doctype") or b[:15].lstrip().lower().startswith(b"<html")

with sync_playwright() as p:
    browser = p.chromium.launch(channel="msedge", headless=False)
    ctx = browser.new_context(accept_downloads=True, viewport={"width": 1280, "height": 900})
    page = ctx.new_page()
    # 预热：先访问一个能过的页面建立 cookie
    for u in [f"{BASE}/2019-2020/", f"{BASE}/default.aspx?cycle=2019-2020"]:
        try:
            page.goto(u, wait_until="domcontentloaded", timeout=60_000)
            time.sleep(4)
            print("预热:", page.title()[:60], flush=True)
        except Exception as e:
            print("预热失败:", str(e)[:80], flush=True)
    time.sleep(6)

    url = f"{BASE}/2019-2020/DEMO_K.XPT"
    for attempt in range(5):
        try:
            res = page.evaluate(JS_FETCH, url)
            head = bytes(res["bytes"][:16])
            print(f"尝试{attempt+1}: status={res['status']} size={res['len']} html={is_html(head)}", flush=True)
            if res["status"] == 200 and res["len"] > 0 and not is_html(head):
                with open(os.path.join(OUT, "DEMO_K.XPT"), "wb") as f:
                    f.write(bytes(res["bytes"]))
                print("✅ DEMO_K 真 XPT 已保存:", res["len"], "bytes", flush=True)
                break
        except Exception as e:
            print(f"尝试{attempt+1}异常:", str(e)[:120], flush=True)
        time.sleep(10)
    browser.close()
