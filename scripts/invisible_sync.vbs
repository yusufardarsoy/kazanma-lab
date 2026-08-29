Set objShell = CreateObject("Wscript.Shell")
Set objFSO = CreateObject("Scripting.FileSystemObject")
scriptDir = objFSO.GetParentFolderName(Wscript.ScriptFullName)
psScript = objFSO.BuildPath(scriptDir, "Run-Data-Sync.ps1")

cmd = "powershell.exe -NoProfile -NonInteractive -ExecutionPolicy Bypass -File """ & psScript & """"
objShell.Run cmd, 0, True
