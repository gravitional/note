# powershell 安装

`oh-my-posh`发展到第三版;
[Oh my Posh 3](https://zhuanlan.zhihu.com/p/308481493)
[A prompt theme engine for any shell.](https://ohmyposh.dev/docs/upgrading)

## PSReadLine

`PSReadLine`模块包含让你在`PowerShell`中定制命令行编辑环境的`cmdlet`.在PowerShell 7.1中包含了PSReadLine v2.1.
具体选项设置,如回溯历史时让光标默认移动到尾部,可参考:
[PSReadLine](https://docs.microsoft.com/en-us/powershell/module/psreadline/?view=powershell-7.1)
[Set-PSReadLineOption](https://docs.microsoft.com/en-us/powershell/module/psreadline/set-psreadlineoption?view=powershell-7.1)
[Get-PSReadLineKeyHandler](https://docs.microsoft.com/en-us/powershell/module/psreadline/get-psreadlinekeyhandler?view=powershell-7.1)

下面的命令查看所有绑定和未绑定的按键映射.

```powershell
Get-PSReadLineKeyHandler -Bound -Unbound #查看所有绑定和未绑定的 key mapping
Get-PSReadLineKeyHandler #只查看绑定的 key mapping
Get-PSReadLineKeyHandler -Chord Enter, Shift+Enter # 查看特定按键的 key mapping
```

***
[为 Windows PowerShell 设置 User Alias (命令别名)](https://zhuanlan.zhihu.com/p/74881435)
[给 PowerShell 带来 zsh 的体验](https://zhuanlan.zhihu.com/p/137251716)
[第 9 章 - 函数](https://docs.microsoft.com/zh-cn/powershell/scripting/learn/ps101/09-functions?view=powershell-7.1#parameter-validation)

## 配置文件

下面给出一个 `powershell`配置的例子. `vim $PROFILE`即可打开这个配置文件

```powershell
Import-Module posh-git # 引入 posh-git
Import-Module oh-my-posh # 引入 oh-my-posh
import-Module -Name Terminal-Icons # 引入终端图标

# Set-Theme Agnoster # 第二版设置主题为 Agnoster
# Set-PoshPrompt -Theme agnoster #第三版设置主体命令,设置本地主题
# Set-PoshPrompt -Theme cinnamon
Set-PoshPrompt -Theme craver

# PSReadLine 设置
# 设置预测文本来源为历史记录并将光标移动到末尾
$PSReadLineOptions = @{
    PredictionSource              = "History"
    HistoryNoDuplicates           = $true
    HistorySearchCursorMovesToEnd = $true
}
Set-PSReadLineOption @PSReadLineOptions

Set-PSReadlineKeyHandler -Key Tab -Function Complete # 设置 Tab 键补全
#Set-PSReadLineKeyHandler -Key "Ctrl+d" -Function MenuComplete # 设置 Ctrl+d 为菜单补全和 Intellisense
if ($IsWindows) {
    Set-PSReadLineKeyHandler -Key "Ctrl+a" -Function BeginningOfLine # 设置 Ctrl+a 移动到行首
}
Set-PSReadLineKeyHandler -Key "Ctrl+w" -Function BackwardKillWord # 设置 Ctrl+w 删除word
Set-PSReadLineKeyHandler -Key "Ctrl+z" -Function Undo # 设置 Ctrl+z 为撤销
Set-PSReadLineKeyHandler -Key "Ctrl+u" -Function BackwardKillLine # 删除到行首
Set-PSReadLineKeyHandler -Key "Ctrl+k" -Function DeleteToEnd # 删除到行末
Set-PSReadLineKeyHandler -Key "Ctrl+d" -Function DeleteCharOrExit # 删除字符或者退出
#Set-PSReadLineKeyHandler -Key "Alt+." -Function YankLastArg # 复制上一次输入的末尾
Set-PSReadLineKeyHandler -Key UpArrow -Function HistorySearchBackward # 设置向上键为后向搜索历史记录
Set-PSReadLineKeyHandler -Key DownArrow -Function HistorySearchForward # 设置向下键为前向搜索历史纪录

Set-Alias edit vim #默认编辑器
Set-Alias gh Get-Help # 查看命令帮助

# 常用函数
# 如果是 unix, 就用
if (  $IsLinux ) {
    function la { /usr/bin/env ls --color=tty $args }
    function lb { /usr/bin/env ls --color=tty  -l --human-readable $args }
    function open { xdg-open $args }    # linux open,  MacOs 自带 open
}
if ( $IsMacOS ) {
    function la { /usr/bin/env ls -G $args }
    function lb { /usr/bin/env ls -Glh $args }
}
if ($IsWindows) {
    Set-Alias open Start-Process  # `打开`的别名
}
# 成几列的样式查看文件
function  lc { Get-ChildItem | Format-Wide Name -Column 6 }
# 以MB 为单位查看文件大小
function lh {
    $unit = "MB";
    Get-ChildItem -File $args | Select-Object Name, @{label = "Size($unit)"; expression = { ($_.length / "1$unit").ToString('F2') } }, @{l = "Days"; e = { ((Get-Date) - $_.LastAccessTime).Days } }

}
function .. { Set-Location .. }
function ... { Set-Location ../.. }
function .... { Set-Location ../../.. }
# pwsh 6.2增加了对`-`和`+`作为路径参数值的支持. pwsh 维护了可以用`-`和`+`访问的最后20个位置的历史
# git 常用命令
function gcam {
    param ( [string]$message )
    git commit -a -m  $message
}
function ga { git add $args }
function gaa { git add --all $args }
function grhh { git reset --hard $args }
function gpw { git push $args }
function glw { git pull $args }
function gbr { git branch --remote $args }
function gba { git branch -a $args }
function gcl { git clone --recurse-submodules $args }
function gst { git status $args }
function gd { git diff $args }
function gdca { git diff --cached $args }
function gdcw { git diff --cached --word-diff $args }
function gds { git diff --staged $args }
function gdw { git diff --word-diff $args }
function gfw { git fetch $args }
function gkw { gitk --all --branches $args }
# mathematica
function mma { mathematica $args }
function mms { mathematica -singleLaunch $args }
# 查看文件夹占用体积的函数
function Get-du {
    param ([switch] $total, [switch] $inclueFile, [string] $path = '.', [string] $unit) # 使用 total 开关可以指定求总空间占用
    if ($args) { $path = $args }
    function selectUnit {
        # 根据命令行参数, 以及文件大小选择显示单位
        if ($unit) { $selUnit = $unit }
        elseif ($args -gt '1TB' ) { $selUnit = 'TB' }
        elseif ($args -gt '1GB' ) { $selUnit = 'GB' }
        elseif ($args -gt '1MB' ) { $selUnit = 'MB' }
        elseif ($args -gt '1KB' ) { $selUnit = 'KB' }
        else { $selUnit = 's' }
        return $selUnit
    }
    # 执行文件体积统计任务
    if ($total ) {
        # 如果只统计总量
        $totalSize = (Get-ChildItem -Path $path -Force -Recurse -ErrorAction SilentlyContinue | Measure-Object -Property Length -Sum).Sum
        $selUnit = selectUnit($totalSize)
        write-host -fore green ("{0,8:N2} {1,-2} {2} {3,-20}" -f ($totalSize / "1$selUnit"), $selUnit, ' -- ', ( Resolve-Path $path))
    }
    # 用循环处理子项目, 如果 $inclueFile=True, 除了统计文件夹, 还统计文件
    elseif ($inclueFile) {
        $colItems = (Get-ChildItem $path | Sort-Object)
    }
    else {
        $colItems = (Get-ChildItem $path | Where-Object { $_.PSIsContainer -eq $True } | Sort-Object)
    }
    foreach ($i in $colItems) {
        $subFolderItems = (Get-ChildItem $i.FullName -recurse | Measure-Object -Sum { $_.Length } ) # 计算求和
        $selUnit = selectUnit($subFolderItems.Sum)
        $FileSize = ("{0:N2}" -f ($subFolderItems.Sum / "1$selUnit")) # 格式化字符串
        write-host -fore green ("{0,8:N2} {1,-2} {2} {3,-20}" -f $FileSize, $selUnit, ' -- ', $i.FullName)
    }
}
# 7z 批量压缩
function  to7z {
    if ( -not $args  ) {
        throw 'Please input the file path, such as *'
    }
    else {
        $lst = (Get-ChildItem -Path $args)
        foreach ($f in $lst ) {
            7z a -pxxx -mx=0 ( $f.BaseName + '.7z' ) $f
        }
    }
}
```

## macos, homebrew

在 `homebrew` 中安装

To use Homebrew in PowerShell, set:
  Add-Content -Path $PROFILE.CurrentUserAllHosts -Value '$(/opt/homebrew/bin/brew shellenv) | Invoke-Expression'
