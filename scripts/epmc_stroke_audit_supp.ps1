# epmc_stroke_audit_supp.ps1 — 补充核验：歧义文献数据来源 + WTI 替代拼写
$ErrorActionPreference = 'Stop'
$outFile = 'D:\NHANES\results\epmc_stroke_audit_supp.txt'
$sb = [System.Text.StringBuilder]::new()

function Search-EPMC([string]$label, [string]$query) {
    Start-Sleep -Milliseconds 800
    $enc = [uri]::EscapeDataString($query)
    $uri = "https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=" + $enc + "&format=json&resultType=core&pageSize=10"
    $r = $null
    for ($attempt = 1; $attempt -le 2; $attempt++) {
        try {
            $r = Invoke-RestMethod -Uri $uri -TimeoutSec 60
            break
        } catch {
            if ($attempt -eq 1) { Start-Sleep -Seconds 3 }
            else {
                [void]$sb.AppendLine("LABEL: $label")
                [void]$sb.AppendLine("ERROR: 检索失败 - $($_.Exception.Message)")
                [void]$sb.AppendLine("")
                return
            }
        }
    }
    [void]$sb.AppendLine("LABEL: $label")
    [void]$sb.AppendLine("HIT=$($r.hitCount)")
    if ($r.resultList.result) {
        foreach ($res in $r.resultList.result) {
            $title = if ($res.title) { ($res.title -replace '\s+', ' ').Trim() } else { '' }
            $jtitle = if ($res.journalInfo.journal.title) { $res.journalInfo.journal.title } else { '' }
            [void]$sb.AppendLine("  $($res.pubYear) | $title | $jtitle | PMID:$($res.pmid) | DOI:$($res.doi)")
            $abs = if ($res.abstractText) { ($res.abstractText -replace '\s+', ' ') } else { '(no abstract in core)' }
            if ($abs.Length -gt 600) { $abs = $abs.Substring(0, 600) }
            [void]$sb.AppendLine("  ABS: $abs")
        }
    }
    [void]$sb.AppendLine("")
}

[void]$sb.AppendLine("RETRIEVAL DATE: $(Get-Date -Format 'yyyy-MM-dd HH:mm')")
[void]$sb.AppendLine("")

Search-EPMC 'CTI candidate PMID 40750895 (CVD incidence/mortality, is it NHANES?)' 'EXT_ID:40750895'
Search-EPMC 'CMI candidate PMID 41923750 (modified CMI stroke, nationally representative survey?)' 'EXT_ID:41923750'
Search-EPMC 'METS-VF candidate PMID 39920850 (visceral fat metabolic score stroke mediation, NHANES?)' 'EXT_ID:39920850'
Search-EPMC 'UHR candidate PMID 40529437 (uric acid-HDL stroke risk, NHANES?)' 'EXT_ID:40529437'
Search-EPMC 'eGDR candidate PMID 39604935 (stroke & eGDR two cohorts, NHANES?)' 'EXT_ID:39604935'
Search-EPMC 'WTI alt phrasing 1: waist triglyceride x stroke' '(TITLE:"waist triglyceride" OR ABSTRACT:"waist triglyceride") AND (TITLE:"stroke" OR ABSTRACT:"stroke")'
Search-EPMC 'WTI alt phrasing 2: waist-triglyceride x stroke' '(TITLE:"waist-triglyceride" OR ABSTRACT:"waist-triglyceride") AND (TITLE:"stroke" OR ABSTRACT:"stroke")'
Search-EPMC 'WTI index existence any topic (waist triglyceride index)' 'TITLE:"waist triglyceride index" OR ABSTRACT:"waist triglyceride index"'
Search-EPMC 'WTI alt term: waist circumference-triglyceride x stroke' '(TITLE:"waist circumference-triglyceride" OR ABSTRACT:"waist circumference-triglyceride") AND (TITLE:"stroke" OR ABSTRACT:"stroke")'

$utf8 = [System.Text.UTF8Encoding]::new($false)
[System.IO.File]::WriteAllText($outFile, $sb.ToString(), $utf8)
Write-Output "SUPP DONE -> $outFile"
