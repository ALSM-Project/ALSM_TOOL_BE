<#
.SYNOPSIS
  Measures the converter on the cobol/ corpus and prints how many programs convert.

.DESCRIPTION
  Builds target/akaBatch-1.0.jar, converts every program of the corpus the way the backend
  does (-project ... -scaffold, which compiles the result and replaces each program whose Java
  does not compile with a stub) and summarizes conversion_report.json: converted / failed,
  the most frequent failure messages and how many programs were stubbed by the compile check.

  -Save writes the measurement to benchmark/baseline.json, the mark later runs are compared
  against (src/test CorpusConversionTest, run with: mvn test -Pcorpus).

.EXAMPLE
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/baseline.ps1
  powershell -NoProfile -ExecutionPolicy Bypass -File scripts/baseline.ps1 -Save
#>
[CmdletBinding()]
param(
    # Folder with the COBOL programs and copybooks, relative to the repository root.
    [string]$Corpus = 'cobol',
    # Where the converted project is written; a new folder in %TEMP% when omitted.
    [string]$OutDir,
    # Use the jar already in target/ instead of building it first.
    [switch]$SkipBuild,
    # Write benchmark/baseline.json.
    [switch]$Save,
    # How many failure groups to print and keep.
    [int]$Top = 10
)

$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
Set-Location $root

$basePackage = 'jp.co.alsm.batch'
$encoding = 'MS932'
$jar = Join-Path $root 'target/akaBatch-1.0.jar'

if (-not $SkipBuild) {
    Write-Host 'Building target/akaBatch-1.0.jar ...'
    & mvn -B -q -DskipTests package
    if ($LASTEXITCODE -ne 0) { throw "mvn package failed (exit $LASTEXITCODE)" }
}
if (-not (Test-Path $jar)) { throw "$jar not found; run without -SkipBuild" }
if (-not (Test-Path (Join-Path $root $Corpus))) { throw "corpus folder '$Corpus' not found" }

if (-not $OutDir) {
    $OutDir = Join-Path ([IO.Path]::GetTempPath()) ('alsm-baseline-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
}
$log = "$OutDir.log"
Write-Host "Converting '$Corpus' into $OutDir (log: $log) ..."
$started = Get-Date

# The converter's own progress goes to the log; only the milestones are echoed.
$writer = New-Object IO.StreamWriter($log, $false, (New-Object Text.UTF8Encoding($false)))
try {
    & java -jar $jar -project $Corpus -odir $OutDir -basePackage $basePackage -scaffold -encoding $encoding |
        ForEach-Object {
            $writer.WriteLine($_)
            if ($_ -match '^(Build check|  BUILD|  skipped|  the build|Converted|Done)') { Write-Host $_ }
        }
    $exit = $LASTEXITCODE
} finally {
    $writer.Dispose()
}
if ($exit -ne 0) { throw "the converter exited with $exit; see $log" }

$reportFile = Join-Path $OutDir 'conversion_report.json'
if (-not (Test-Path $reportFile)) { throw "no conversion_report.json in $OutDir; see $log" }
$report = Get-Content -Raw -Encoding UTF8 $reportFile | ConvertFrom-Json
$failures = @($report.failures)

# Did the compile check really run to the end? A run without it counts programs as converted
# that do not compile, so it is not comparable with one that did.
$logText = [IO.File]::ReadAllText($log)
if ($logText -match 'skipped: Maven') { $verify = 'skipped' }
elseif ($logText -match 'the build failed outside') { $verify = 'failed-outside-programs' }
elseif ($logText -match 'the build still fails after') { $verify = 'still-failing' }
elseif ($logText -match 'BUILD SUCCESS') { $verify = 'success' }
else { $verify = 'unknown' }

$stubbed = @($failures | Where-Object { $_.reason -like 'the generated Java does not compile*' })

# Group by the first line of the message.
$groups = @($failures |
    Group-Object { ($_.reason -split "`n")[0].Trim() } |
    Sort-Object @{ Expression = 'Count'; Descending = $true }, Name |
    Select-Object -First $Top |
    ForEach-Object {
        [ordered]@{
            count    = $_.Count
            message  = $_.Name
            programs = @($_.Group | Select-Object -First 5 | ForEach-Object { $_.programId })
        }
    })

$elapsed = [int]((Get-Date) - $started).TotalSeconds
Write-Host ''
Write-Host ('Programs: {0}   Converted: {1}   Failed: {2}   Stubbed after compile verify: {3}' -f `
        $report.programs, $report.converted, $report.failed, $stubbed.Count)
Write-Host "Compile verify: $verify   (took ${elapsed}s)"
Write-Host ''
Write-Host "Top $Top failure groups (first line of the message):"
foreach ($g in $groups) {
    Write-Host ('  {0,4}  {1}' -f $g.count, $g.message)
    Write-Host ('        e.g. ' + ($g.programs -join ', '))
}
if ($verify -ne 'success') {
    Write-Warning "Compile verify did not finish ($verify): converted counts programs whose Java was never compiled."
}

if ($Save) {
    if ($verify -ne 'success') { throw "not saving a baseline measured without a finished compile verify ($verify)" }
    $commit = (& git rev-parse --short HEAD).Trim()
    if (& git status --porcelain) { $commit += '-dirty' }
    $baseline = [ordered]@{
        corpus             = $Corpus
        date               = (Get-Date -Format 'yyyy-MM-dd')
        converterCommit    = $commit
        command            = "-project $Corpus -basePackage $basePackage -scaffold -encoding $encoding"
        compileVerify      = $verify
        programs           = [int]$report.programs
        converted          = [int]$report.converted
        failed             = [int]$report.failed
        stubbedAfterVerify = $stubbed.Count
        topFailureGroups   = $groups
        convertedPrograms  = @($report.succeeded | ForEach-Object { $_.programId } | Sort-Object)
    }
    $json = $baseline | ConvertTo-Json -Depth 6
    # Windows PowerShell escapes the characters ' < > & as backslash-u sequences; keep the file readable.
    $escape = [string][char]92 + 'u00'
    $json = $json.Replace($escape + '27', "'").Replace($escape + '3c', '<').Replace($escape + '3e', '>').Replace($escape + '26', '&')
    $target = Join-Path $root 'benchmark/baseline.json'
    [IO.File]::WriteAllText($target, $json + "`n", (New-Object Text.UTF8Encoding($false)))
    Write-Host ''
    Write-Host "Saved $target"
}
