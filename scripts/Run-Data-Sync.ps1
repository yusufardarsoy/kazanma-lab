$ErrorActionPreference = "Stop"
$projectRoot = Split-Path -Parent $PSScriptRoot
$cacheDir = Join-Path $projectRoot "data\cache"
New-Item -ItemType Directory -Path $cacheDir -Force | Out-Null

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
  throw "Rscript bulunamadı."
}

$rscript = Find-Rscript
$stamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
"[$stamp] Otomatik veri görevi başladı." | Add-Content -LiteralPath (Join-Path $cacheDir "sync.log")
Set-Location -LiteralPath $projectRoot
& $rscript (Join-Path $PSScriptRoot "auto_sync.R") 2>&1 |
  Tee-Object -FilePath (Join-Path $cacheDir "sync.log") -Append
exit $LASTEXITCODE
