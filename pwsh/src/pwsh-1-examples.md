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

## Copy-Item

[Copy-Item](https://learn.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Management/Copy-Item)

### 单层copy

+ `-Include -Recurse`; `-Include` 只做用到 `-Path` 给出的顶层目录上, 不审核子文件

```powershell
D:\temp\out> Copy-Item -Path D:\temp\tree\* -Include ex* -Recurse
D:\temp\out> (Get-ChildItem -Recurse).FullName
D:\temp\out\examples
D:\temp\out\example.ps1
D:\temp\out\example.txt
D:\temp\out\examples\subfolder
D:\temp\out\examples\example_1.txt
D:\temp\out\examples\example_2.txt
D:\temp\out\examples\subfolder\test.txt
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
