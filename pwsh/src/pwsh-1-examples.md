# scripts examples

## 批量重命名

[Rename-Item](https://docs.microsoft.com/zh-cn/powershell/module/Microsoft.PowerShell.Management/Rename-Item?view=powershell-7)

```powershell
Get-ChildItem *.txt | Rename-Item -NewName { $_.Name -replace '.txt','.log' }
```

## ls 查找

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

## Copy-Item 复制文件

[Copy-Item](https://learn.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Management/Copy-Item)

### 复制文件, 但不包括自己

默认情况下, 会把 path 复制到 `Destination` 下层.
通过使用 `xxx\*` 的格式, 把 `C:\Logfiles` 目录的内容复制到现有的 `C:\Drawings` 目录.
而不复制 `Logfiles` 目录本身.
如果 `Logfiles` 目录在子目录下有文件, 这些子目录被复制, 其文件树保持不变.
默认情况下, 容器参数被设置为 "True", 它保留了目录结构.

```powershell
Copy-Item -Path "C:\Logfiles\*" -Destination "C:\Drawings" -Recurse
```

### 单层copy

+ `-Include xxx -Recurse`; `-Include` 只做用到 `-Path` 给出的顶层目录上, 不审核子文件

```powershell
D:\temp\out> Copy-Item -Path D:\temp\tree\* -Include ex* -Recurse
D:\temp\out> (Get-ChildItem -Recurse).FullName
```

### Merge copy

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

### 递归拷贝,过滤,保持目录结构,备份

+ 把 `$false` 改成 `$true` 即可保持目录结构

```powershell
Copy-Item -Path C:\temp\tree -Filter *.txt -Recurse -Container:$true
(Get-ChildItem . -Recurse).FullName
```

## 使用 ffmpeg 合并目录中的 mp4 文件

[合并mp4文件](my-merge-mp4.ps1)

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
