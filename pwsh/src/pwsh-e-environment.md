# pwsh 环境变量

[about_Environment_Variables](https://learn.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_environment_variables)
[Environment 类](https://learn.microsoft.com/zh-cn/dotnet/api/system.environment)
[Get-Unique](https://learn.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Get-Unique?view=powershell-7.3&viewFallbackFrom=powershell-7)
[拆分和联接运算符](https://learn.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7.3#split-and-join-operators)

windows 系统, 修改环境变量的例子, Linux 类似, 需要修改分隔符为 `:`

```powershell
if ($IsWindows) {
    $paths = 'C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.32.31326\bin\Hostx64\x64',
    'C:\Strawberry\c\bin',
    'C:\Strawberry\perl\site\bin',
    'C:\Strawberry\perl\bin',
    'C:\Program Files\Wolfram Research\WolframScript\',
    'C:\Program Files\Microsoft MPI\Bin\',
    'C:\ProgramData\chocolatey\bin',
    'C:\Program Files\Calibre2\',
    'C:\Program Files\CMake\bin',
    'C:\Program Files\HDF_Group\HDF5\1.12.2\bin\',
    'C:\Users\yd\AppData\Local\Microsoft\WindowsApps',
    'C:\Program Files\Git\bin',
    'C:\Users\yd\AppData\Local\Programs\oh-my-posh\bin',
    'C:\Program Files\7-Zip',
    'C:\rakudo\bin',
    'C:\rakudo\share\perl6\site\bin',
    '%USERPROFILE%\bin',
    'C:\MyProgram\Notepad3',
    'C:\MyProgram\SogouCapture',
    'C:\MyProgram\mpv',
    'C:\Program Files\ParaView 5.11.0\bin\Lib'
    $split = ';'

    # 检查重复
    # $oldPath = $Env:PATH -split ';'
    # foreach ( $p in $paths) {
    #     if ($p -notin $oldPath) {
    #         $Env:PATH += $split + $p
    #     }
    # }

    # 不检查重复
    $Env:PATH += $paths -join $split
}
```
