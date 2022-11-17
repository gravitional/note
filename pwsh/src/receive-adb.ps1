function Receive-adb {
    <#
    .SYNOPSIS
    分割 Paths 中的目录, 批量拉取到当前目录下
    .EXAMPLE
    Receive-adb  -Paths "a b c d" -Prefix '/sdcard'
    #>
    [CmdletBinding()]
    # 参数模板
    param(
        # 非必须参数, 位置 1
        [Parameter(
            Mandatory = $True,
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
    # 解析字符串,  去除 $null 元素
    $files = @( $Paths -split "\s+").Where( { $_ })
    foreach ($f in $files) {
        adb pull ( -join ($Prefix, "/", $f))
    }
}