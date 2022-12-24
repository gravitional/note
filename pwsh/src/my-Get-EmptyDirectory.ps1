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
                        Path     = $Folder
                    }
                }
                else {
                    [PSCustomObject]@{
                        EmptyDir = $False
                        Path     = $Folder
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