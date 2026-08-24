$ErrorActionPreference = "Stop"
$taskName = "KazanmaLabDataSync"
$runner = Join-Path $PSScriptRoot "Run-Data-Sync.ps1"
if (-not (Test-Path -LiteralPath $runner)) { throw "Veri çalıştırıcısı bulunamadı: $runner" }

$action = New-ScheduledTaskAction `
  -Execute "powershell.exe" `
  -Argument ('-NoProfile -NonInteractive -WindowStyle Hidden -ExecutionPolicy Bypass -File "{0}"' -f $runner)
$matchWindow = New-ScheduledTaskTrigger -Daily -At "16:00"
$matchWindow.Repetition.Interval = "PT30M"
$matchWindow.Repetition.Duration = "PT8H"
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
  -Trigger @($matchWindow, $dailyCatchup, $atLogon) `
  -Settings $settings `
  -Principal $principal `
  -Description "Kazanma Lab Süper Lig fikstür, ilk 11, eksik ve maç-sonu verilerini ücretsiz API kotası içinde eşitler." `
  -Force | Out-Null

Start-ScheduledTask -TaskName $taskName
Write-Host "Kazanma Lab veri görevi kuruldu. Her gün 10:00'da, 16:00-00:00 arasında 30 dakikada bir ve oturum açılışında çalışacak."
