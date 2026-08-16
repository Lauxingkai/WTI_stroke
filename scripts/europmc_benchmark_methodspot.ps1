# Method-spotting for benchmark papers (Europe PMC), output to stdout + UTF-8 log
$ErrorActionPreference = 'Continue'
$patterns = @(
    @{ M = 'RCS';            R = 'restricted cubic spline|cubic spline|dose-response' },
    @{ M = 'mediation';      R = 'mediation' },
    @{ M = 'ML';             R = 'machine learning|random forest|XGBoost|gradient boosting|LASSO|logistic regression model' },
    @{ M = 'SHAP';           R = 'SHAP|Shapley' },
    @{ M = 'mixture';        R = 'weighted quantile|quantile g-computation|qgcomp|BKMR|mixture' },
    @{ M = 'MR';             R = 'Mendelian randomization|genetic instrument|instrumental variable' },
    @{ M = 'propensity';     R = 'propensity score|inverse probability|IPTW' },
    @{ M = 'Evalue';         R = 'E-value|E value|unmeasured confounding' },
    @{ M = 'cluster';        R = 'latent class|latent profile|cluster analysis|K-means' },
    @{ M = 'survival';       R = 'survival|Cox|Kaplan-Meier|mortality|longitudinal|prospective' },
    @{ M = 'interaction';    R = 'interaction|RERI|synergy|additive' },
    @{ M = 'extValidation';  R = 'external validation|validation cohort|external cohort|independent cohort' },
    @{ M = 'competingRisk';  R = 'competing risk|Fine-Gray|subdistribution' },
    @{ M = 'TMLE';           R = 'doubly robust|targeted maximum likelihood|TMLE|G-computation|g-computation' },
    @{ M = 'deepLearning';   R = 'deep learning|neural network' }
)

$lines = New-Object System.Collections.Generic.List[string]
$ids = @('42303192','42394597','41779139','42183400','42434070','42317864','42492864','42499080')
foreach ($id in $ids) {
    $enc = [uri]::EscapeDataString("EXT_ID:$id")
    $ok = $false
    for ($a = 1; $a -le 2; $a++) {
        try {
            $r = Invoke-RestMethod -Uri ('https://www.ebi.ac.uk/europepmc/webservices/rest/search?query={0}&format=json&resultType=core&pageSize=1' -f $enc) -TimeoutSec 60
            $res = $r.resultList.result[0]
            if (-not $res) { throw 'not found' }
            $ab = $res.abstractText
            if (-not $ab) { $ab = $res.abstract }
            $ab = if ($ab) { [System.Net.WebUtility]::HtmlDecode(($ab -replace '<[^>]+>', ' ')) } else { '' }
            $text = "$($res.title) $ab".ToLowerInvariant()
            $jt = if ($res.journalInfo.journal.title) { $res.journalInfo.journal.title } else { 'NA' }
            $doi = if ($res.doi) { $res.doi } else { 'NA' }
            $hits = @()
            foreach ($p in $patterns) {
                if ($text -match $p.R) { $hits += $p.M }
            }
            $line = "PMID:$id | $($res.pubYear) | $jt | DOI:$doi`nTITLE: $($res.title)`nMETHODS: $($hits -join ', ')"
            Write-Output $line
            $lines.Add($line)
            $ok = $true
            break
        } catch {
            if ($a -ge 2) {
                $line = "PMID:$id | RETRIEVAL-FAILED: $($_.Exception.Message)"
                Write-Output $line
                $lines.Add($line)
            } else { Start-Sleep -Milliseconds 2000 }
        }
    }
    Start-Sleep -Milliseconds 800
}
[System.IO.File]::WriteAllLines((Join-Path (Get-Location) 'results\benchmark_methods_utf8.txt'), $lines, (New-Object System.Text.UTF8Encoding($false)))
Write-Output 'DONE'
