# WTI × Stroke — NHANES + CHARLS Dual-Cohort Study

Analysis code for: **"Waist-triglyceride index and stroke in middle-aged and older adults: cross-sectional and prospective evidence from the US NHANES and China CHARLS nationwide cohorts"**

## Overview

Continuous waist-triglyceride index (WTI = waist circumference × triglycerides) as a re-parameterization of the hypertriglyceridemic-waist phenotype, tested against stroke in two nationwide cohorts (NHANES 2005-2018 fasting subsample; CHARLS 2011 cross-sectional + 7-year prospective).

## Reproducibility

- R 4.6.1 + Python 3 (package versions: `qc/environment_snapshot.txt`); seed 42 everywhere.
- Raw data: NHANES (CDC public), CHARLS (registration required) — **not** committed.
- Pipeline: `scripts/00` → `06` (data → cohorts → analysis → figures/tables).
- Audit: `scripts/08/09/10` (independent recomputation, three-round numeric audit).

## Pipeline

| Step | Script | Output |
|---|---|---|
| 00 | charls_covariates.py | CHARLS covariates + Table S1 |
| 01 | id_linkage.R | Cohort CSVs (ID assertions) |
| 02 | build_cohort.R | Covariate-enriched cohorts + Table 1 |
| 03/03b/03c | analysis.R | Main models / discrimination+RCS / Cox+Fine-Gray |
| 04 | rcs_predict.R + figures.py | Figure 2-4 |
| 05 | evalue/lag2/interval/boot_nri/mediation/table1 | Advanced modules |
| 06 | flow/cif/tables/figure1 | Figure 1, 5 + Tables 2-4 |
| 07 | refs_metadata.py | Vancouver references (Europe PMC) |
| 13a-13e | 2015 codebook extract / value labels / recon / build / analysis | CHARLS 2015 replication layer (data + M1-M3 + sensitivity) |
| 13f-13h | ndi_parse / ndi_cox / mde | NDI 2019 public-use linkage: parse 8 .dat (official layout) → svycoxph all-cause & stroke death → MDE |
| 13i | integrity_check.py | Integrity-gate re-run for the 2015+NDI additions (qc/13i_integrity_check.txt) |

## Data dictionary

See `variable_dictionary.md` (exposure/outcome/covariate definitions, both cohorts, with official variable names).

## Registration

OSF (retrospective registration; see `OSF_preregistration_draft.md`); STROBE checklist included.

## License

Code: MIT. Data: see NHANES/CHARLS terms.
