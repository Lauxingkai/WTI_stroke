# ============================================================================
# 03c_prep_events.py
# Build wave-level event times for CHARLS prospective layer (2011 -> 2018).
#   stroke_time: first wave reporting stroke (2013=2, 2015=4, 2018=7 years)
#   death_time : death year/month from 2013 Exit interview (approx from 2011)
# Output: data/processed/charls_events_2011_2018.csv
# ============================================================================
import pyreadstat
import pandas as pd
import numpy as np

BASE = r"D:\NHANES\CHARLS\CHARLS_1725074232_3\CHARLS"
OUT = r"D:\NHANES\data\processed"

hs13, _ = pyreadstat.read_dta(BASE + r"\2013\Health_Status_and_Functioning.dta")
hs15, _ = pyreadstat.read_dta(BASE + r"\2015\Health_Status_and_Functioning.dta")
hs18, _ = pyreadstat.read_dta(BASE + r"\2018\Health_Status_and_Functioning.dta")
ex13, _ = pyreadstat.read_dta(BASE + r"\2013\Exit_Interview.dta")

def sid(df):
    return df.ID.astype(str).str.strip()

s13 = pd.DataFrame({"ID": sid(hs13),
                    "stk13": pd.to_numeric(hs13.da019_w2_1, errors="coerce") == 1})
s15 = pd.DataFrame({"ID": sid(hs15),
                    "stk15": ((pd.to_numeric(hs15.da019_w2_1, errors="coerce") == 1) |
                              (pd.to_numeric(hs15.zda007_8_, errors="coerce") == 1))})
s18 = pd.DataFrame({"ID": sid(hs18),
                    "stk18": ((pd.to_numeric(hs18.da019_w2_1, errors="coerce") == 1) |
                              (pd.to_numeric(hs18.da007_8_, errors="coerce") == 1))})

ev = s13.merge(s15, on="ID", how="outer").merge(s18, on="ID", how="outer")
ev["stk13"] = ev.stk13.fillna(False); ev["stk15"] = ev.stk15.fillna(False)
ev["stk18"] = ev.stk18.fillna(False)

ev["stroke"] = ev.stk13 | ev.stk15 | ev.stk18
ev["stroke_t"] = np.select(
    [ev.stk13, ev.stk15, ev.stk18], [2.0, 4.0, 7.0], default=7.0)

# anchor to 2011 baseline prospective cohort (ID_12 outward join): stroke first
base = pd.read_csv(OUT + r"\charls_2011_2018_prosp_cov.csv", usecols=["ID_12"],
                   dtype={"ID_12": str})
ev = base.merge(ev, left_on="ID_12", right_on="ID", how="left")
ev["stroke"] = ev.stroke.fillna(False)
ev["stroke_t"] = ev.stroke_t.fillna(7.0)

# death from 2013 Exit joined directly onto the baseline IDs
ex13 = ex13.assign(ID=sid(ex13))
yr = pd.to_numeric(ex13.exb001_1, errors="coerce")
mo = pd.to_numeric(ex13.exb001_2, errors="coerce")
ex13["death_t"] = np.where(yr.notna(), (yr - 2011) + mo.fillna(6) / 12, np.nan)
ex13["death"] = yr.notna()
ex13.loc[ex13.death_t < 0, "death_t"] = 0.0
ev = ev.merge(ex13[["ID", "death_t", "death"]], left_on="ID_12", right_on="ID", how="left")
ev["death"] = ev.death.fillna(False)
ev["death_t"] = ev.death_t.fillna(7.0)
ev["time"] = np.minimum(ev.stroke_t, ev.death_t)

print("events rows:", len(ev))
print("stroke events:", int(ev.stroke.sum()), "| death events:", int(ev.death.sum()))
print("stroke_t dist:", ev[ev.stroke].stroke_t.value_counts().sort_index().to_dict())

ev.to_csv(OUT + r"\charls_events_2011_2018.csv", index=False)
print("DONE")
