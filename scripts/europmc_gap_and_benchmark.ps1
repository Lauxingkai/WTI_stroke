# Fetch abstracts of candidate benchmark papers + run "gap" queries (Europe PMC)
$ErrorActionPreference = 'Continue'
$sleepMs = 800

function Invoke-EPMC {
    param([string]$Label, [string]$Query, [int]$PageSize = 1)
    $enc = [uri]::EscapeDataString($Query)
    $uri = 'https://www.ebi.ac.uk/europepmc/webservices/rest/search?query={0}&format=json&resultType=core&pageSize={1}' -f $enc, $PageSize
    $attempts = 0
    while ($true) {
        $attempts++
        try {
            $r = Invoke-RestMethod -Uri $uri -TimeoutSec 60
            Write-Output ("==== {0} | HIT={1}" -f $Label, $r.hitCount)
            $i = 0
            foreach ($res in $r.resultList.result) {
                $i++
                if ($i -gt $PageSize) { break }
                $jt = if ($res.journalInfo.journal.title) { $res.journalInfo.journal.title } else { 'NA' }
                Write-Output ("  {0} | {1} | {2} | {3} | PMID:{4}" -f $i, $res.pubYear, $res.title, $jt, $res.pmid)
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
    Start-Sleep -Milliseconds $sleepMs
}

Write-Output "== Gap queries =="
$gaps = @(
    @{ L = 'gap-RERI';              Q = '"RERI"' },
    @{ L = 'gap-BKMR';              Q = '("Bayesian kernel machine regression" OR "BKMR")' },
    @{ L = 'gap-competingRisk';     Q = '"competing risk"' },
    @{ L = 'gap-TMLE';              Q = '("doubly robust" OR "targeted maximum likelihood" OR "TMLE")' },
    @{ L = 'gap-deepLearning';      Q = '("deep learning" OR "neural network")' }
)
foreach ($g in $gaps) {
    $qWin = 'NHANES AND stroke AND {0} AND (PUB_YEAR:2024 OR PUB_YEAR:2025 OR PUB_YEAR:2026)' -f $g.Q
    Invoke-EPMC -Label ("{0}-2024_2026" -f $g.L) -Query $qWin -PageSize 1
    $qAll = 'NHANES AND stroke AND {0}' -f $g.Q
    Invoke-EPMC -Label ("{0}-ALL" -f $g.L) -Query $qAll -PageSize 1
}

Write-Output "== Benchmark abstracts =="
$out = 'results\europmc_benchmark_abstracts.txt'
Remove-Item $out -ErrorAction SilentlyContinue
$ids = @('42303192','42394597','41779139','42183400','42434070','42317864','42492864','42499080')
foreach ($id in $ids) {
    $enc = [uri]::EscapeDataString("EXT_ID:$id")
    $attempts = 0
    while ($true) {
        $attempts++
        try {
            $r = Invoke-RestMethod -Uri ('https://www.ebi.ac.uk/europepmc/webservices/rest/search?query={0}&format=json&resultType=core&pageSize=1' -f $enc) -TimeoutSec 60
            $res = $r.resultList.result[0]
            if ($res) {
                $ab = $res.abstractText
                if (-not $ab) { $ab = $res.abstract }
                if ($ab) { $ab = ($ab -replace '<[^>]+>', ' ') }
                $jt = if ($res.journalInfo.journal.title) { $res.journalInfo.journal.title } else { 'NA' }
                Add-Content -Path $out -Value ("==== PMID:{0} | {1} | {2}" -f $id, $res.pubYear, $jt)
                Add-Content -Path $out -Value ("TITLE: {0}" -f $res.title)
                Add-Content -Path $out -Value ("ABSTRACT: {0}" -f $ab)
                Add-Content -Path $out -Value ''
            } else {
                Add-Content -Path $out -Value ("==== PMID:{0} | NOT-FOUND" -f $id)
            }
            break
        } catch {
            if ($attempts -ge 2) {
                Add-Content -Path $out -Value ("==== PMID:{0} | RETRIEVAL-FAILED: {1}" -f $id, $_.Exception.Message)
                break
            }
            Start-Sleep -Milliseconds 2000
        }
    }
    Start-Sleep -Milliseconds $sleepMs
}
Write-Output "WROTE $out"
Write-Output "DONE"
