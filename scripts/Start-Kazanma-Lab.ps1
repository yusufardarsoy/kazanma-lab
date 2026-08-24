param([switch]$SkipSync)

$ErrorActionPreference = "Stop"
$projectRoot = Split-Path -Parent $PSScriptRoot
Set-Location -LiteralPath $projectRoot

function Find-Rscript {
  $fromPath = Get-Command Rscript.exe -ErrorAction SilentlyContinue
  if ($fromPath) { return $fromPath.Source }
  $install = Get-ChildItem -LiteralPath "C:\Program Files\R" -Directory -ErrorAction SilentlyContinue |
    Sort-Object Name -Descending |
    Select-Object -First 1
  if ($install) {
    $candidate = Join-Path $install.FullName "bin\Rscript.exe"
    if (Test-Path -LiteralPath $candidate) { return $candidate }
  }
  throw "Rscript bulunamadı. Önce R 4.4 veya daha yenisini kur."
}

$rscript = Find-Rscript
if (-not $SkipSync) {
  Write-Host "Yeni fikstür, ilk 11, eksik ve sonuçlar kontrol ediliyor..."
  & $rscript (Join-Path $PSScriptRoot "auto_sync.R")
  if ($LASTEXITCODE -ne 0) {
    Write-Warning "Veri eşitleme tamamlanmadı. API anahtarı yoksa site yine açılır; kurulum adımlarını README'den tamamla."
  }
}

Write-Host "Kazanma Lab http://127.0.0.1:3838 adresinde açılıyor. Kapatmak için bu pencerede Ctrl+C kullan."
& $rscript -e "shiny::runApp('.', host='127.0.0.1', port=3838, launch.browser=TRUE)"
