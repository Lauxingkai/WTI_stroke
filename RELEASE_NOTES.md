# Release notes (2026-08-16)

Analysis code for: "Waist-triglyceride index and stroke in middle-aged and older adults: cross-sectional and prospective evidence from the US NHANES and China CHARLS nationwide cohorts" (submitted to Lipids in Health and Disease).

- scripts/00-07: dual-cohort pipeline (data -> cohorts -> main models -> discrimination/RCS -> Cox/Fine-Gray -> advanced modules -> tables/figures -> references).
- scripts/08-10: three-round numeric audit, pre-review fixes.
- scripts/12a-12c: regression calibration (TG repeated measures), physician-diagnosed subset, calibrated NRI.
- scripts/13a-13i: CHARLS 2015 cross-sectional replication layer (codebook mapping -> build -> weighted M1-M3) + NDI 2019 public-use mortality linkage (parse -> survey-weighted cause-specific Cox) + MDE + integrity-gate re-run.
- Raw data NOT included (NHANES CDC public; CHARLS registration required). Results CSVs in results/ of the local project; contact corresponding author for access.
- R 4.6.1 + Python 3; seed 42 everywhere.
