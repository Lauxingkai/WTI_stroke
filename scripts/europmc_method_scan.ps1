# Methodological landscape scan: NHANES x stroke (Europe PMC REST API)
# Query protocol per user instructions. Every hit count comes from a real API call.
# Usage: pwsh -File scripts/europmc_method_scan.ps1 | Tee-Object results/europmc_method_scan.log

$ErrorActionPreference = 'Continue'
$base = 'https://www.ebi.ac.uk/europepmc/webservices/rest/search?query={0}&format=json&resultType=core&pageSize={1}'

function Invoke-EPMCQuery {
    param(
        [string]$Label,
        [string]$Query,
        [int]$PageSize = 3
    )
    $enc = [uri]::EscapeDataString($Query)
    $uri = $base -f $enc, $PageSize
    $attempts = 0
    while ($true) {
        $attempts++
        try {
            $r = Invoke-RestMethod -Uri $uri -TimeoutSec 60
            Write-Output ("==== {0} | HIT={1} | QUERY={2}" -f $Label, $r.hitCount, $Query)
            $i = 0
            foreach ($res in $r.resultList.result) {
                $i++
                if ($i -gt $PageSize) { break }
                $jtitle = if ($res.journalInfo.journal.title) { $res.journalInfo.journal.title } else { 'NA' }
                Write-Output ("  {0} | {1} | {2} | {3} | PMID:{4}" -f $i, $res.pubYear, $res.title, $jtitle, $res.pmid)
            }
            break
        } catch {
            if ($attempts -ge 2) {
                Write-Output ("==== {0} | RETRIEVAL-FAILED: {1}" -f $Label, $_.Exception.Message)
                break
            }
            Start-Sleep -Milliseconds 2000
        }
    }
    Start-Sleep -Milliseconds 800
}

Write-Output "== Part 1: yearly publication volume (NHANES x stroke) =="
foreach ($yr in 2021, 2022, 2023, 2024, 2025, 2026) {
    $q = '(TITLE:"NHANES" OR ABSTRACT:"NHANES") AND (TITLE:"stroke" OR ABSTRACT:"stroke") AND (PUB_YEAR:{0})' -f $yr
    Invoke-EPMCQuery -Label ("YEAR-{0}" -f $yr) -Query $q -PageSize 1
}

Write-Output "== Part 2: methodological tags (NHANES AND stroke AND <method>) =="
$methods = @(
    @{ L = 'a-RCS';                 Q = '"restricted cubic spline"' },
    @{ L = 'b-mediation';           Q = '"mediation analysis"' },
    @{ L = 'c-ML';                  Q = '("machine learning" OR "XGBoost" OR "random forest")' },
    @{ L = 'd-SHAP';                Q = '"SHAP"' },
    @{ L = 'e-mixture';             Q = '("weighted quantile sum" OR "quantile g-computation" OR "qgcomp")' },
    @{ L = 'f-MR';                  Q = '"mendelian randomization"' },
    @{ L = 'g-propensity';          Q = '"propensity score"' },
    @{ L = 'h-Evalue';              Q = '"E-value"' },
    @{ L = 'i-cluster';             Q = '("latent class" OR "cluster analysis" OR "K-means")' },
    @{ L = 'j-longitudinal';        Q = '("longitudinal" OR "survival" OR "mortality")' },
    @{ L = 'k-twosampleMR';         Q = '"two-sample" AND "MR"' },
    @{ L = 'k2-twosampleMR-strict'; Q = '"two-sample mendelian randomization"' },
    @{ L = 'l-interaction';         Q = '"interaction" AND "synergy"' },
    @{ L = 'm-externalVal';         Q = '("national cohort" OR "external validation")' }
)

foreach ($m in $methods) {
    $qWin = 'NHANES AND stroke AND {0} AND (PUB_YEAR:2024 OR PUB_YEAR:2025 OR PUB_YEAR:2026)' -f $m.Q
    Invoke-EPMCQuery -Label ("{0}-2024_2026" -f $m.L) -Query $qWin -PageSize 3
    $qAll = 'NHANES AND stroke AND {0}' -f $m.Q
    Invoke-EPMCQuery -Label ("{0}-ALL" -f $m.L) -Query $qAll -PageSize 1
}

Write-Output "DONE"
