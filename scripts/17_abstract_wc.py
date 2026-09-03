import re

t = open(r"D:\NHANES\manuscript\final\manuscript_main.md", encoding="utf-8").read()
m = re.search(r"## Abstract\s*\n(.*?)\n## ", t, re.S)
body = m.group(1)
words = len(re.findall(r"[A-Za-z0-9][A-Za-z0-9'\-\.]*", body))
print("Abstract words:", words, "(BMC limit 350)")
