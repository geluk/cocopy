Push-Location 'C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build'

cmd /c "vcvars64.bat&set" |
ForEach-Object {
    if ($_ -match "=") {
        $v = $_.split("="); set-item -force -path "ENV:\$($v[0])"  -value "$($v[1])"
    }
}
Pop-Location

Write-Host "Linker flags: " $env:CommonLinkerFlagsMSVC

link out\out.obj /subsystem:console /out:hello.exe legacy_stdio_definitions.lib msvcrt.lib