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