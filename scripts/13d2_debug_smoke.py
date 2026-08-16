import pyreadstat
import pandas as pd
import numpy as np

BASE = r"D:\NHANES\CHARLS\CHARLS_1725074232_3\CHARLS"
hs, _ = pyreadstat.read_dta(BASE + r"\2015\Health_Status_and_Functioning.dta")

def yn(x):
    return pd.to_numeric(x, errors="coerce") == 1

hz = hs[["ID", "zda059", "da059", "da061", "da061_w3", "da067"]].copy()
hz["smoke"] = yn(hz.zda059) | yn(hz.da059)
print("hz smoke mean:", hz.smoke.mean())
print("hz smoke counts:", hz.smoke.value_counts().to_dict())
print("zda059==1:", int(yn(hz.zda059).sum()), "| da059==1:", int(yn(hz.da059).sum()),
      "| union:", int((yn(hz.zda059) | yn(hz.da059)).sum()))
