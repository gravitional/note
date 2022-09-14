# scripts examples

## 批量重命名

[Rename-Item](https://docs.microsoft.com/zh-cn/powershell/module/Microsoft.PowerShell.Management/Rename-Item?view=powershell-7)

```powershell
Get-ChildItem *.txt | Rename-Item -NewName { $_.Name -replace '.txt','.log' }
```

使用 `-WhatIf` 预览替换结果, 而不真正执行替换.

## 查找空文件夹

[如何使用Powershell查找空文件夹](如何使用Powershell查找空文件夹)

当你在共享或本地目录上进行清理时, 可用 `Powershell` 寻找空文件夹.
一个很好的例子是, 你要对文件夹进行永久存档, 想做一些清理工作, 以去除文件夹中的冗余, 这将是一个很好的工具.

这个Powershell脚本使用 `Get-ChildItem`, 并支持 `Depth`, `Recurse` 和 `Path` 参数, 使你的查询更灵活.
它还允许你使用 `UNC` 路径, 所以你可以很容易地将其用于网络驱动器.

查找空文件夹的脚本如下:

```powershell
Function Get-EmptyDirectory {
    <#
    .SYNOPSIS
        使用底层的 Get-ChildItem cmdlet 获取空目录

    .NOTES
        名称: Get-EmptyDirectory
        作者: TheSysadminChannel
        版本: 1.0
        DateCreated: 2021年10月2日

    .LINK
        https://thesysadminchannel.com/find-empty-folders-powershell/ -

    .EXAMPLE
        Get-EmptyDirectory -Path \\Server\Share\Folder -Depth 2
    #>

    [CmdletBinding()]
    # 参数模板
    param(
        # 装饰器,必须参数, 位置 0
        [Parameter(
            Mandatory = $True,
            Position = 0
        )]
        #字符串类型, 后面使用 $path 调用
        [string] $Path,

        # 非必须参数, 位置 1
        [Parameter(
            Mandatory = $False,
            Position = 1
        )]
        # 开关类型, 即 boolean 类型
        [switch] $Recurse,

        [Parameter(
            Mandatory = $False,
            Position = 2
        )]
        # 验证范围
        [ValidateRange(1, 15)]
        [int] $Depth
    )
    # 初始化段
    BEGIN {}
    # 过程段
    PROCESS {
        try {
            # 准备 Get-ChildItem 的参数
            $ItemParams = @{
                Path      = $Path
                Directory = $True
            }
            # 如果参数包含 Recurse, 递归搜索
            if ($PSBoundParameters.ContainsKey('Recurse')) {
                $ItemParams.Add('Recurse', $True)
            }
            # 如果参数包含 Depth, 就添加深度
            if ($PSBoundParameters.ContainsKey('Depth')) {
                $ItemParams.Add('Depth', $Depth)
            }
            # 使用给定的参数, 调用 Get-ChildItem
            $FolderList = Get-ChildItem @ItemParams | Select-Object -ExpandProperty FullName
            # 对得到的列表进行迭代
            foreach ($Folder in $FolderList) {
                # 如果是空目录
                if (-not (Get-ChildItem -Path $Folder)) {
                    # 自定义 PS 对象
                    [PSCustomObject]@{
                        EmptyDir = $True
                        Path           = $Folder
                    }
                }
                else {
                    [PSCustomObject]@{
                        EmptyDir = $False
                        Path           = $Folder
                    }
                }
            }
        }
        catch {
            Write-Error $_.Exception.Message
        }
    }
    # 善后段
    END {}
}
```

## 合并子文件到上层

```powershell
Function Merge-ChildItem {
    <#
    .SYNOPSIS
        使用底层的 Get-ChildItem, Move-Item 将文件提升到父目录中
    
    .NOTES
        名称: Merge-ChildItem
        作者: Graviton
        版本: 1.0
        DateCreated: 2022年2月15日
    
    .LINK
        https://thesysadminchannel.com/find-empty-folders-powershell/ -
    
    .EXAMPLE
        Merge-ChildItem -Path \\Server\Share\Folder -Depth 2
    #>
    
    [CmdletBinding()]
    # 参数模板
    param(
        [Parameter( # 装饰器,必须参数, 位置 0
            Mandatory = $True,
            Position = 0
        )] 
        [string] $Path, #字符串类型, 后面使用名称 $path 调用

        [Parameter(# 非必须参数
            Mandatory = $False,
            Position = 1
        )] 
        [switch] $Recurse, # 开关类型, 即 boolean 类型
    
        [Parameter(# 递归深度
            Mandatory = $False,
            Position = 2
        )]
        [ValidateRange(1, 15)] #验证范围
        [int] $Depth, 

        [Parameter(# 非必须参数, 是否只打印操作, 不执行动作
            Mandatory = $False,
            Position = 3
        )] 
        [switch] $WhatIf
    )
    # 初始化段 
    BEGIN {}
    # 过程段
    PROCESS {
        try {
            # 准备 Get-ChildItem 的参数
            $ItemParams = @{
                Path = $Path
                File = $True #只搜索文件
            }
            # 如果参数包含 Recurse, 递归搜索
            if ($PSBoundParameters.ContainsKey('Recurse')) {
                $ItemParams.Add('Recurse', $True)
            }
            # 如果参数包含 Depth, 就添加深度
            if ($PSBoundParameters.ContainsKey('Depth')) {
                $ItemParams.Add('Depth', $Depth)
            }
            # Move-Item 使用的参数
            $MoveParams = @{}
            # 如果参数包含 WhatIf, 只打印操作
            if ($PSBoundParameters.ContainsKey('WhatIf')) {
                $MoveParams.Add('WhatIf', $True)
            }
            # 使用给定的参数, 调用 Get-ChildItem
            $subList = Get-ChildItem @ItemParams
            # 对得到的列表进行迭代, 移动到 目录的父目录/目录名_文件名
            foreach ($sub in $subList) {
                $newPath = Join-Path -Path $sub.Directory.Parent.FullName -ChildPath ($sub.Directory.Name + "_" + $sub.Name) 
                Move-Item @MoveParams -Path $sub -Destination $newPath
            }
        }
        catch {
            Write-Error $_.Exception.Message
        }
    }
    # 善后段
    END {}
}
```

### 批量拉取文件,abd

```powershell
function Receive-adb {
    [CmdletBinding()]
    # 参数模板
    param(
        # 非必须参数, 位置 1
        [Parameter(
            Mandatory = $False,
            Position = 0
        )]
        [string] $Paths,
        #字符串类型,
        [Parameter(
            Mandatory = $True,
            Position = 1
        )]

        [string] $Prefix
    )
    # 解析字符串
    $files = @( $Paths -split "\s+")
    foreach ($f in $files) { 
        adb pull ( -join ($Prefix, "/", $f)) 
    }
}
```
