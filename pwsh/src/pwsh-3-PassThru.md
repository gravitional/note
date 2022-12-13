# PassThru 参数

[PowerShell中的PassThru参数是什么](https://www.pstips.net/powershell-passthru-parameter.html)

首先看下PowerShell内置的命令中, 那些命令包含了PassThru的定义了:

```powershell
Get-Command | where { $_.Definition -like "*PassThru*" } | select Name

Name
----
Add-Computer
Add-Content
...
```

总过有44条命令.
(测试的PowerShell为2.0版本, 操作系统为windows7)那这44条命令主要涉及那些方面呢?可以分析命令的名词后缀:

```powershell
PS C:\> Get-Command | where { $_.Definition -like "*PassThru*" } | select Noun -Unique

Noun
----
Computer
Content
...
```

让我们挑选三条命令进行分析.

## Start-Process

默认通过Start-Process打开记事本, 在PowerShell的控制台上没有输出.
但是如果加上了PassThru参数时, 就会输出新打开的记事本的信息,
并且用户可以将该进程信息保存到变量中, 以进行下一步操作, 例如输出进程的启动时间, 通过管道关闭该进程.

PS C:\> Start-Process notepad
PS C:\> Start-Process notepad -PassThru

Handles  NPM(K)    PM(K)      WS(K) VM(M)   CPU(s)     Id ProcessName
-------  ------    -----      ----- -----   ------     -- -----------
     60       7     2284       6644    77     0.03   3664 notepad

PS C:\> $notepad=Start-Process notepad -PassThru
PS C:\> $notepad.StartTime

2013年1月12日 23:29:03

PS C:\> $notepad | Stop-Process

## Copy-Item

默认通过Copy-Item拷贝文件时, 如果不出错, 在PowerShell控制台上没有输出.
但是如果使用了PassThru参数, 就会输出拷贝后目标文件的文件信息,
并且是一个System.IO.FileInfo, 有了这个对象就可以对目标文件进行下一步操作了.

```powershell
PS C:\> Copy-Item C:\softset.ini d:\softset.ini
PS C:\> Copy-Item C:\softset.ini d:\softset.ini -PassThru

    目录: D:\

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
-a---          2013/1/2      1:51         70 softset.ini

$file=Copy-Item C:\softset.ini d:\softset.ini -PassThru
$file.GetType().FullName
System.IO.FileInfo

$file.FullName
D:\softset.ini

$file.Attributes
Archive

$file | Remove-Item
```

## Import-Module

默认通过Import-Module导入PowerShell模块定义时, 在PowerShell控制台上没有输出.
但是使用了PassThru参数, 就会输出该模块定义的信息, 并且可以将模块保存, 进行下一步操作了.

```powershell
Import-Module AppLocker
Import-Module AppLocker -PassThru

ModuleType Name                      ExportedCommands
---------- ----                      ----------------
Manifest   AppLocker                 {Set-AppLockerPolicy, New-AppLockerPolicy, Test-AppLockerPolicy,

$appLocker=Import-Module AppLocker -PassThru
$appLocker.Description
Powershell AppLocker Module

$appLocker.Guid

Guid
----
9dafd409-67de-4108-8ee9-73cd61f5b7bf
```

好了, 事不过三, 现在稍微总结一下,
使用PowerShell 中的PassThru参数可以将那些新创建的或者经过更新的对象由默认的隐藏变成输出或返回,
以便进行下一步操作, 体现的正是PowerShell的灵活性.
是不是所有的命令都包含了PassThru参数?不是, PassThru参数在PowerShell命令中的定义中是可选的,
不是像ErrorAction那一类通用的参数, 所以倒有几分像Confirm,WhatIf参数.
