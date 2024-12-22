# scripts examples

## 探测环境, pwsh 版本, 兼容性

### 兼容性问题

[PowerShell Unexpected Token Error during transliteration](https://superuser.com/questions/1538755/powershell-unexpected-token-error-during-transliteration)

windows 自带的 powershell 5.x,
`$PROFILE` 文件的编码必须是 `UTF-8 with BOM`([byte order mark][]),
否则 它不认 `UTF8` 文件, 例如

```pwsh
D:\PShell\SU\1538755_NoBom.ps1
# At D:\PShell\SU\1538755_NoBom.ps1:2 char:41
# ...
# Unexpected token 'Đ¶Đ·Đ¸Đ' in expression
# or statement.
```

windows 自带的 powershell 5.x, 缺少很多命令和选项

```pwsh
$PSVersionTable.PSVersion
# Major  Minor  Build  Revision
# -----  -----  -----  --------
# 5      1      22621  4391
```

总结为

```pwsh
$IsWindows, $IsLinux, $IsMacOS # 没有 操作系统自动变量
$message = (Test-Path $path) ? "Path exists" : "Path not found" # 没有三元运算符
# select 没有 -CaseInsensitive
$Env:PATH -split ';' | Select-Object -Unique -CaseInsensitive
```

[byte order mark]: https://en.wikipedia.org/wiki/Byte_order_mark

### OS, pwsh version

[PowerTip: Determine your version of PowerShell and host operating system](https://devblogs.microsoft.com/scripting/powertip-determine-your-version-of-powershell-and-host-operating-system/)
[PowerTip: Identify if you are running on PowerShell Core](https://devblogs.microsoft.com/scripting/powertip-identify-if-you-are-running-on-powershell-core/)

只需使用内置变量 `$PSVersionTable`, 它包含了你需要的所有属性.
下面是一个例子:

```pwsh
# 当前 powershell 版本
$PSVersionTable.PSVersion

# 当前 host 操作系统; 如果值是 $null, 则在运行 Windows PowerShell 5.x
# 否则将返回 开源版本(pwsh core)中的主机环境
# else it returns the host environment from the open Source release
$PSVersionTable.OS

# 返回使用的 powershell 版本; Core on pwsh core; Desktop on win PowerShell
$PSVersionTable.PSEdition
```

### `$IsWindows`, `$IsLinux`, `$IsMacOS`

[$IsWindows](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.5#iswindows)
[Powershell 5.1x: about_Automatic_Variables](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-5.1#iswindows)
[Determine the OS version, Linux and Windows from Powershell](https://stackoverflow.com/questions/44703646/determine-the-os-version-linux-and-windows-from-powershell)

`powershell 5.x` 上没有定义这些自动变量, windows 定义了环境变量,
因此可以使用 `$Env:OS` 代替

```pwsh
$Env:OS
#[out] Windows_NT
```

### 三元运算符

[Ternary operator](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators?view=powershell-7.4)

类似的, powershell 5.x 没有三元运算符 `a ? b : c`, PowerShell 7.0 才引入

```pwsh
$message = (Test-Path $path) ? "Path exists" : "Path not found"
# 命令需要用括号包起来
(Test-Path .vscode) ? (Write-Host 'exists') : (Write-Host 'not found')
```

## 文件系统, 文件操作, 路径操作

### 批量重命名

[Rename-Item](https://docs.microsoft.com/zh-cn/powershell/module/Microsoft.PowerShell.Management/Rename-Item?view=powershell-7)

```powershell
Get-ChildItem *.txt | Rename-Item -NewName { $_.Name -replace '.txt','.log' }
```

### ls 查找

使用 `-WhatIf` 预览替换结果, 而不真正执行替换.

### 查找空文件夹

[如何使用Powershell查找空文件夹](如何使用Powershell查找空文件夹)

当你在共享或本地目录上进行清理时, 可用 `Powershell` 寻找空文件夹.
一个很好的例子是, 你要对文件夹进行永久存档, 想做一些清理工作, 以去除文件夹中的冗余, 这将是一个很好的工具.

这个Powershell脚本使用 `Get-ChildItem`, 并支持 `Depth`, `Recurse` 和 `Path` 参数, 使你的查询更灵活.
它还允许你使用 `UNC` 路径, 所以你可以很容易地将其用于网络驱动器.

+ [查找空文件夹](Get-EmptyDirectory.ps1)
+ [合并子文件到上层](Merge-ChildItem.ps1)
+ [批量拉取文件](receive-adb.ps1)

### 查找特定名称的文件夹

```powershell
ls  -Recurse -Filter 'SolvingDomain'
```

### Copy-Item 复制文件

[Copy-Item](https://learn.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Management/Copy-Item)

#### 复制文件, 但不包括自己

默认情况下, 会把 path 复制到 `Destination` 下层.
通过使用 `xxx\*` 的格式, 把 `C:\Logfiles` 目录的内容复制到现有的 `C:\Drawings` 目录.
而不复制 `Logfiles` 目录本身.
如果 `Logfiles` 目录在子目录下有文件, 这些子目录被复制, 其文件树保持不变.
默认情况下, 容器参数被设置为 "True", 它保留了目录结构.

```powershell
Copy-Item -Path "C:\Logfiles\*" -Destination "C:\Drawings" -Recurse
```

#### 单层copy

+ `-Include xxx -Recurse`; `-Include` 只做用到 `-Path` 给出的顶层目录上, 不审核子文件

```powershell
D:\temp\out> Copy-Item -Path D:\temp\tree\* -Include ex* -Recurse
D:\temp\out> (Get-ChildItem -Recurse).FullName
```

#### Merge copy

+ `-Filter -Recurse` 选项; 递归复制满足 通配符模式 的文件, 不保持目录结构; Merge copy

```powershell
Copy-Item -Path C:\temp\tree -Filter *.txt -Recurse -Container:$false
(Get-ChildItem . -Recurse).FullName
```

+ 使用 `Get-ChildItem -Recurse -Filter` 和 `管道`, Merge 复制, 把过滤出的文件复制到同一目录下

```powershell
D:\temp\out> Get-ChildItem -Path D:\temp\tree -Recurse -Filter ex* | Copy-Item
D:\temp\out> (Get-ChildItem -Recurse).FullName
D:\temp\out\examples
D:\temp\out\example_1.txt
D:\temp\out\example_2.txt
D:\temp\out\example.ps1
D:\temp\out\example.txt
```

#### 递归拷贝,过滤,保持目录结构,备份

+ 把 `$false` 改成 `$true` 即可保持目录结构

```powershell
Copy-Item -Path C:\temp\tree -Filter *.txt -Recurse -Container:$true
(Get-ChildItem . -Recurse).FullName
```

### 使用 ffmpeg 合并目录中的 mp4 文件

[合并mp4文件](my-merge-mp4.ps1)

## 环境变量

### pwsh 加载 cmd 脚本中的环境变量

[Running cmd from powershell](https://stackoverflow.com/questions/41399692/running-a-build-script-after-calling-vcvarsall-bat-from-powershell)
[Windows IT Pro: Take Charge of Environment Variables in PowerShell](http://windowsitpro.com/powershell/take-charge-environment-variables-powershell)

问题是, 当你运行 `cmd.exe` 来运行批处理文件时,
变量会在 `cmd.exe` 的实例中设置, 但在该cmd进程终止后就会消失.
要解决这个问题, 可以使用本文中的 `Invoke-CmdScript` 函数:
您可以将此函数添加到 `PowerShell` 配置文件中, 或将其作为脚本文件使用.

```powershell
# Invokes a Cmd.exe shell script and updates the environment.
function Invoke-CmdScript {
    param(
        [String] $scriptName
    )
    $cmdLine = "`"$scriptName`" $args & set"
    &"$Env:SystemRoot\system32\cmd.exe" /c $cmdLine |
    select-string '^([^=]*)=(.*)$' | foreach-object {
        $varName = $_.Matches[0].Groups[1].Value
        $varValue = $_.Matches[0].Groups[2].Value
        Set-Item Env:$varName $varValue
    }
}
```

定义好函数后, 就可以运行命令了:

```powershell
Invoke-CmdScript "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" amd64_x86
C:\buildscript.cmd --build-options
```

这篇文章还介绍了几个可以让你轻松保存和恢复环境变量的函数.

## sdfds

cmd.exe /E:ON /K ""C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022"
cmd.exe /K '"C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022 && powershell'

c:\PROGRA~1
"C:\Program Files\CMake\bin\cmake.exe"
C:\PROGRA~1\CMake\bin\cmake.exe

cd C:\Users\qingz\Downloads\lapack-3.12.0\build

```pwsh
pwsh.exe -NoProfile -Command {C:\Program` Files\CMake\bin\cmake.exe -E __create_def C:\Users\qingz\Downloads\lapack-3.12.0\build\LAPACKE\CMakeFiles\lapacke.dir\.\exports.def C:\Users\qingz\Downloads\lapack-3.12.0\build\LAPACKE\CMakeFiles\lapacke.dir\.\exports.def.objs && cd C:\Users\qingz\Downloads\lapack-3.12.0\build} && Get-Content CMakeFiles\lapacke.rsp && cp CMakeFiles\lapacke.rsp ~\Downloads\ttqqq.rsp && C:\Program` Files\CMake\bin\cmake.exe -E vs_link_dll --msvc-ver=1941 --intdir=LAPACKE\CMakeFiles\lapacke.dir --rc=C:\PROGRA~2\WI3CF2~1\10\bin\100226~1.0\x64\rc.exe --mt=C:\PROGRA~2\WI3CF2~1\10\bin\100226~1.0\x64\mt.exe --manifests -- C:\PROGRA~2\Intel\oneAPI\compiler\latest\bin\icx-cl.exe /nologo @CMakeFiles\lapacke.rsp -LD /Qoption,link,/machine:x64 /Qoption,link,/debug /Qoption,link,/INCREMENTAL /Qoption,link,/DEF:LAPACKE\CMakeFiles\lapacke.dir\.\exports.def -link /out:bin\liblapacke.dll /implib:lib\lapacke.lib /pdb:bin\liblapacke.pdb /version:3.12 && cd .
```