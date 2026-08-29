$ErrorActionPreference = "Stop"
$taskName = "KazanmaLabDataSync"
$projectRoot = Split-Path -Parent $PSScriptRoot
$vbsRunner = Join-Path $PSScriptRoot "invisible_sync.vbs"
if (-not (Test-Path -LiteralPath $vbsRunner)) { throw "Görünmez çalıştırıcı bulunamadı: $vbsRunner" }

$action = New-ScheduledTaskAction `
  -Execute "wscript.exe" `
  -Argument ('//nologo "{0}"' -f $vbsRunner) `
  -WorkingDirectory $projectRoot
$matchWindow = 0..16 | ForEach-Object {
  $time = [datetime]::Today.AddHours(16).AddMinutes($_ * 30)
  New-ScheduledTaskTrigger -Daily -At $time
}
$dailyCatchup = New-ScheduledTaskTrigger -Daily -At "10:00"
$atLogon = New-ScheduledTaskTrigger -AtLogOn -User $env:USERNAME
$settings = New-ScheduledTaskSettingsSet `
  -StartWhenAvailable `
  -AllowStartIfOnBatteries `
  -DontStopIfGoingOnBatteries `
  -MultipleInstances IgnoreNew
$principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Interactive -RunLevel Limited

Register-ScheduledTask `
  -TaskName $taskName `
  -Action $action `
  -Trigger (@($matchWindow) + @($dailyCatchup, $atLogon)) `
  -Settings $settings `
  -Principal $principal `
  -Description "Kazanma Lab Süper Lig tarih, oran, ilk 11, eksik ve maç-sonu verilerini ücretsiz kaynaklardan eşitler." `
  -Force | Out-Null

Start-ScheduledTask -TaskName $taskName
Write-Host "Kazanma Lab veri görevi kuruldu. Her gün 10:00'da, 16:00-00:00 arasında 30 dakikada bir ve oturum açılışında çalışacak."
