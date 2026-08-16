# Variable Dictionary — WTI × Stroke Dual-Cohort Study

> 来源：scripts/00-02（官方 codebook 裁决见 qc/phase0_裁决记录.md，含 MCQ160F 修正）。

## NHANES 2005-2018（D-J 周期，空腹亚样本）

| 变量 | 源文件/变量 | 定义 | 处理 |
|---|---|---|---|
| 年龄 | DEMO RIDAGEYR | 岁，≥40 纳入 | 连续 |
| 性别 | DEMO RIAGENDR | 1=男 2=女 | 二分类 |
| 种族 | DEMO RIDRETH1 | 种族/民族 | M2+（库特异） |
| 教育 | DEMO DMDEDUC2 | 1-5（7/9→NA，audit P2-1 修正） | 连续 |
| 腰围 | BMX BMXWAIST | cm | WTI 分量 |
| TG | TRIGLY LBXTR | mg/dL ×0.01129→mmol/L | WTI 分量 |
| FPG | GLU LBXGLU | mg/dL | 敏感性（连续） |
| 体重指数 | BMX BMXBMI | kg/m² | M2+ |
| 吸烟 | SMQ SMQ020/SMQ040 | ≥100 支/现吸 | 二分类 |
| 饮酒 | ALQ ALQ110/ALQ101 | 12 杯/年 | 二分类 |
| 高血压 | BPQ BPQ020 | 1=是 | M3 |
| 降压药 | BPQ BPQ050A | NA→未服药 | M3 |
| 糖尿病 | DIQ DIQ010 | 1=是 | M3 |
| 他汀 | RXQ_RX RXDUSE+RXDDRUG | 药名 regex | M3 |
| 体力活动 | PAQ PAQ180/PAD680 | 分钟/天（跨周期切换） | M3 三分位 |
| 卒中 | **MCQ MCQ160F**（⚠ 非 MCQ160E=心梗） | 自报医生诊断 | 结局 |
| 权重 | TRIGLY WTSAF2YR（/7 合并） | 空腹亚样本权重 | 设计 |
| PSU/strata | DEMO SDMVPSU/SDMVSTRA | 周期前缀拼接 | 设计 |

## CHARLS 2011（+2013/2015/2018 随访）

| 变量 | 源文件/变量 | 定义 | 处理 |
|---|---|---|---|
| ID | 官方规则 householdID+"0"；ID_12=…+substr(ID,-2,2) | 跨波匹配 | 键 |
| 年龄 | DM ba002_1 | 2011−出生年 | 连续 |
| 性别 | DM rgender | 1=男 2=女 | 二分类 |
| 教育 | DM bd001 | 1-11 级 | 连续（Table 1 分层） |
| 腰围 | BM qm002 | cm | WTI 分量 |
| 身高/体重 | BM qh006/ql002 | 修复规则（坑#7） | BMI |
| TG | Blood_2011 newtg | mg/dl×0.01129 | WTI 分量 |
| FPG | Blood_2011 newglu | mg/dl | 敏感性 |
| 慢病 | HS da007_1_/2_/3_/8_ | 1=是（8=卒中） | 混杂/基线排除 |
| 用药 | HS da010_2_s2/da011s2 | 2=服西药 | M3 |
| 吸烟/饮酒 | HS da059/da061/da067 | 1/2 水平 | 二分类 |
| 活动 | HS da051_1_/2_+da052_1_/2_ | 中高强度天数/周 | M3 |
| 卒中随访 | HS da019_w2_1（2013/15/18）+zda007_8_（2015）+da007_8_（2018） | 波次首次报告 | 事件 |
| 死亡 | Exit exb001_1/2（2013） | 年月 | 竞争事件 |
| 权重 | Blood_2011 bloodweight（归一化） | 血检权重 | 设计 |
| 分层/聚类 | PSU urban_nbs / communityID | 城乡/社区 | 设计 |
| 中介 | Blood_2015 bl_crp/bl_crea（eGFR CKD-EPI2021） | lnCRP/eGFR | 中介 |

## CHARLS 2015（复制层，映射裁决见 scripts/13a-13d 与 supplementary Note 2）

| 变量 | 源文件/变量 | 定义 | 处理 |
|---|---|---|---|
| ID | 各表 ID（12 位，直接使用） | 跨表匹配 | 键 |
| 年龄 | DM ba004_w3_1（ba002=2 时取 ba002_1） | 2015−出生年 | 连续，20-120 门控 |
| 性别 | DM ba000_w2_3（fallback HS xrgender） | 1=男 2=女 | 二分类 |
| 教育 | DM bd001_w2_4 | 1-11 级；12=未变→回填 2011 covariates/2013 zbd001/bd001 | 连续 |
| 腰围/身高/体重 | BM qm002/qi002/ql002 | cm/kg，同 2011 修复规则 | WTI/BMI |
| TG | Blood bl_tg | mg/dL×0.01129；500 封顶 240 例 | WTI 分量 |
| 慢病（高血压/血脂/糖尿病/卒中） | HS zda007_1_/2_/3_/8_（携带）∨ da007_1_/2_/3_/8_（新诊断） | 1=是（8=卒中） | 结局/混杂 |
| 医生确认卒中 | HS da007_w2_2_8_ ∨ da019_w2_1 | 医生告知=1 | 敏感性结局 |
| 用药 | HS da010_2_s2/da011s2 | 2=服西药（与 2011 同口径） | M3 |
| 吸烟/饮酒/活动 | HS da059/da061/da067/da051_1_/2_+da052_1_/2_ | 同 2011 口径 | M2/M3 |
| 权重 | Blood Blood_weight（敏感性：Weights Biomarker_weight） | 血检权重（归一化） | 设计 |
| 分层/聚类 | PSU(2011) urban_nbs / communityID | 100% 覆盖 | 设计 |

## NHANES NDI 死亡链接（前瞻层）

| 变量 | 源文件/列位 | 定义 | 处理 |
|---|---|---|---|
| SEQN | MORT_2019_PUBLIC.dat 1-6 位 | 序列号 | 键 |
| ELIGSTAT | 15 位 | 1=可链接 | 排除 2/3 |
| MORTSTAT | 16 位 | 0=存活 1=死亡 | 全因死亡结局 |
| UCOD_LEADING | 17-19 位 | 死因 recode；5=I60-I69 | 卒中死亡结局 |
| PERMTH_INT | 43-45 位 | 自访谈起人月数（至 2019-12-31） | 随访时间 |
| 布局 | 官方 R_ReadInProgramAllSurveys.R（CDC） | 固定宽度 | 解析 |
| 模型 | svycoxph（周期 PSU/strata，WTSAF2YR/7）；Fine-Gray（非加权） | 原因别 Cox | M1-M3 |

## 派生指标

| 指标 | 公式 | 单位 |
|---|---|---|
| WTI | WC(cm) × TG(mmol/L) | cm·mmol/L |
| TyG | ln[TG(mg/dL)×FPG(mg/dL)/2] | — |
| TyG-WC | TyG × WC | — |
| ABSI | WC(m)/(BMI^(2/3)×H(m)^(1/2)) | — |
| HTGW | 男 WC≥90/女≥80 cm 且 TG≥1.69 mmol/L | 二分类 |
| eGFR | CKD-EPI 2021 单肌酐 | mL/min/1.73m² |
