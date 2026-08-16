# epmc_stroke_audit.ps1
# 批量核验 18 个代谢/胰岛素抵抗指标 × stroke × NHANES 的 Europe PMC 命中情况
# 输出: D:\NHANES\results\epmc_indicators_stroke_audit.txt
$ErrorActionPreference = 'Stop'
$outFile = 'D:\NHANES\results\epmc_indicators_stroke_audit.txt'
$sb = [System.Text.StringBuilder]::new()

function Search-EPMC([string]$label, [string]$query, [int]$maxShow = 10) {
    Start-Sleep -Milliseconds 800
    $enc = [uri]::EscapeDataString($query)
    $uri = "https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=" + $enc + "&format=json&resultType=core&pageSize=10"
    $r = $null
    for ($attempt = 1; $attempt -le 2; $attempt++) {
        try {
            $r = Invoke-RestMethod -Uri $uri -TimeoutSec 60
            break
        } catch {
            if ($attempt -eq 1) {
                [void]$sb.AppendLine("WARN retry ($label): $($_.Exception.Message)")
                Start-Sleep -Seconds 3
            } else {
                [void]$sb.AppendLine("LABEL: $label")
                [void]$sb.AppendLine("QUERY: $query")
                [void]$sb.AppendLine("ERROR: 检索失败 - $($_.Exception.Message)")
                [void]$sb.AppendLine("")
                return -1
            }
        }
    }
    [void]$sb.AppendLine("LABEL: $label")
    [void]$sb.AppendLine("QUERY: $query")
    [void]$sb.AppendLine("HIT=$($r.hitCount)")
    if ($r.resultList.result) {
        $n = 0
        foreach ($res in $r.resultList.result) {
            $n++
            if ($n -gt $maxShow) { break }
            $title = if ($res.title) { ($res.title -replace '\s+', ' ').Trim() } else { '' }
            $jtitle = if ($res.journalInfo.journal.title) { $res.journalInfo.journal.title } else { '' }
            $pmid = if ($res.pmid) { $res.pmid } else { '' }
            [void]$sb.AppendLine("  $($res.pubYear) | $title | $jtitle | PMID:$pmid")
        }
    }
    [void]$sb.AppendLine("")
    return [int]$r.hitCount
}

function Get-TermQuery([string[]]$terms) {
    $parts = @()
    foreach ($t in $terms) { $parts += "TITLE:$t"; $parts += "ABSTRACT:$t" }
    return "(" + ($parts -join " OR ") + ")"
}

$indicators = @(
    [pscustomobject]@{ Name = 'TyG index';             Terms = @('"TyG"', '"triglyceride-glucose"') },
    [pscustomobject]@{ Name = 'TyG-BMI';               Terms = @('"TyG-BMI"', '"triglyceride-glucose-body mass index"') },
    [pscustomobject]@{ Name = 'TyG-WC';                Terms = @('"TyG-WC"', '"triglyceride-glucose-waist circumference"') },
    [pscustomobject]@{ Name = 'TyG-WHtR';              Terms = @('"TyG-WHtR"', '"triglyceride-glucose-waist-to-height ratio"') },
    [pscustomobject]@{ Name = 'TyG-ABSI';              Terms = @('"TyG-ABSI"', '"triglyceride-glucose-body shape index"') },
    [pscustomobject]@{ Name = 'CTI';                   Terms = @('"C-reactive protein-triglyceride"') },
    [pscustomobject]@{ Name = 'METS-IR';               Terms = @('"METS-IR"', '"metabolic score for insulin resistance"') },
    [pscustomobject]@{ Name = 'LAP';                   Terms = @('"lipid accumulation product"') },
    [pscustomobject]@{ Name = 'VAI';                   Terms = @('"visceral adiposity index"') },
    [pscustomobject]@{ Name = 'CMI';                   Terms = @('"cardiometabolic index"') },
    [pscustomobject]@{ Name = 'NHHR';                  Terms = @('"NHHR"', '"non-HDL-cholesterol to HDL-cholesterol"') },
    [pscustomobject]@{ Name = 'AIP';                   Terms = @('"atherogenic index of plasma"') },
    [pscustomobject]@{ Name = 'UHR';                   Terms = @('"uric acid to HDL"') },
    [pscustomobject]@{ Name = 'HOMA-IR';               Terms = @('"HOMA-IR"') },
    [pscustomobject]@{ Name = 'eGDR';                  Terms = @('"eGDR"', '"estimated glucose disposal rate"') },
    [pscustomobject]@{ Name = 'CVAI';                  Terms = @('"CVAI"', '"Chinese visceral adiposity index"') },
    [pscustomobject]@{ Name = 'WTI';                   Terms = @('"waist triglyceride index"') },
    [pscustomobject]@{ Name = 'METS-VF';               Terms = @('"METS-VF"', '"metabolic score for visceral fat"') }
)

$strokeField = '(TITLE:"stroke" OR ABSTRACT:"stroke")'
$nhanesField = '(TITLE:"NHANES" OR ABSTRACT:"NHANES")'

# 连通性测试
[void]$sb.AppendLine("CONNECTIVITY TEST")
$t = Invoke-RestMethod -Uri "https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=stroke&format=json&pageSize=1" -TimeoutSec 60
[void]$sb.AppendLine("stroke HIT=$($t.hitCount)")
[void]$sb.AppendLine("")
Start-Sleep -Milliseconds 800

$hits = @{}
foreach ($ind in $indicators) {
    $name = $ind.Name
    $tq = Get-TermQuery $ind.Terms
    $l1 = "$tq AND $strokeField"
    $h1 = Search-EPMC "$name :: L1 full-text-field x stroke" $l1 10
    Start-Sleep -Milliseconds 300
    $l2 = "$l1 AND $nhanesField"
    $h2 = Search-EPMC "$name :: L2 + NHANES" $l2 10
    Start-Sleep -Milliseconds 300
    $l3 = "$l1 AND PUB_YEAR:[2024 TO 2026]"
    $h3 = Search-EPMC "$name :: L3 2024-2026" $l3 10
    $hits[$name] = [pscustomobject]@{ L1 = $h1; L2 = $h2; L3 = $h3 }
    Write-Output ("PROGRESS {0}: L1={1} L2={2} L3={3}" -f $name, $h1, $h2, $h3)
}

[void]$sb.AppendLine("========== SUMMARY ==========")
foreach ($ind in $indicators) {
    $h = $hits[$ind.Name]
    [void]$sb.AppendLine("$($ind.Name) | L1_full_stroke=$($h.L1) | L2_NHANES=$($h.L2) | L3_2024-2026=$($h.L3)")
}

$utf8 = [System.Text.UTF8Encoding]::new($false)
[System.IO.File]::WriteAllText($outFile, $sb.ToString(), $utf8)
Write-Output "DONE -> $outFile"
