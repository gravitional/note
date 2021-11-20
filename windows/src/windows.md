# windows

## 日常

[你应该知道的 Windows 环境变量](https://zhuanlan.zhihu.com/p/67726501)

+ `ComSpec` 变量: 规定 http://CMD.COM 文件的位置.运行 http://cmd.com 可直接打开 "命令提示符" 窗口.
+ `NUMBER_OF_PROCESSORS` 变量: 代表用户电脑中处理器的数量.
+ `OS` 变量: 表明用户的操作系统.
+ `Path` 变量: 规定操作系统在指定的文件路径中查看可执行文件.
+ `PathExt` 变量: 规定在 Path 变量中所指定的可执行文件的扩展名有哪些.
+ `PROCESSOR_ARCHITECTURE` 变量: 表明用户处理器的架构.
+ `PROCESSOR_IDENTIFIER` 变量: 表明用户处理器.
+ `PROCESSOR_LEVEL` 变量: 表明用户处理器的等级.
+ `PROCESSOR_REVISION` 变量: 表明用户处理器的版本.
+ `TEMP`/`TMP` 变量: 规定系统运行或安装程序时用来存储临时文件的目录.
+ `windir` 变量: 规定操作系统的系统目录的路径.

+ `set` 查看所有环境变量
+ `set xxx` 查看特定环境变量`xxx`的值.
+ `set xxx=yyy` 添加新的环境变量
+ `set xxx=` 删除已经存在的环境变量,不能有空格.

在文件管理器中引用变量,`%xxx%`,比如`%PATH%`

## windos 系统维护

### 启用组策略gpedit.msc

转自大佬:[Win10家庭版启用组策略gpedit.msc](https://blog.csdn.net/u013642500/article/details/80138799)

大家都认为,Windows 10家庭版中并不包含组策略,其实不然,它是有相关文件的,只是不让你使用而已.那么我们让系统允许你使用就好了.
制作一个`bat`脚本,安装组策略包.

1, 首先你需要在桌面上新建一个txt文本文档.然后将以下代码复制到这个新建的txt文本文档中.

```cmd
@echo off
pushd "%~dp0"
dir /b C:\Windows\servicing\Packages\Microsoft-Windows-GroupPolicy-ClientExtensions-Package~3*.mum >List.txt
dir /b C:\Windows\servicing\Packages\Microsoft-Windows-GroupPolicy-ClientTools-Package~3*.mum >>List.txt
for /f %%i in ('findstr /i . List.txt 2^>nul') do dism /online /norestart /add-package:"C:\Windows\servicing\Packages\%%i"
pause
```

把上面的命令保存到一个名为`xxx.cmd`的文件中,右键单击,选择以管理员身份运行,运行完毕,你的电脑就可以使用组策略`gpedit.msc`了.

### 概念解释

`@echo off`

`@` 前缀字符.表示执行时本行在`cmd`里面不显示.如果想要每行都不显示,在文件开头添加`echo off`命令.
`@echo off`不打印命令,包括`echo off`本身,只输出执行结果.

`pushd`: `push directory`

存储当前目录给 `popd` 命令使用,然后更改为指定的目录.

每次使用 pushd 命令时,将存储一个目录供你使用. 但是,可以多次使用 pushd 命令来存储多个目录. 目录按顺序存储在虚拟堆栈中,因此,如果你使用 pushd 命令一次,则使用命令的目录将放置在堆栈的底部. 如果再次使用该命令,第二个目录将置于第一个目录的顶部. 每次使用 pushd 命令时都会重复此过程.
如果使用 `popd` 命令,则会删除堆栈顶部的目录,并将当前目录更改为该目录

`dir /b`  展示目录和文件,不显示额外信息.

***

```powershell
for /f %%i in ('findstr /i . List.txt 2^>nul') do dism /online /norestart /add-package:"C:\Windows\servicing\Packages\%%i"
for /f [<parsingkeywords>] {%%|%}<variable> in ('<command>') do <command> [<commandlinepptions>]
```

`for`的迭代和文件分析:
 使用文件分析处理命令输出, 字符串和文件内容. 使用迭代变量定义要检查的内容或字符串,并使用各种 `parsingkeywords` 选项进一步修改分析.
 使用 `parsingkeywords` 标记选项可指定哪些标记应作为迭代变量传递. 请注意,当不使用令牌选项时, `/f` 将仅检查第一个令牌.

文件分析包括读取输出, 字符串或文件内容,然后将其分解为单独的文本行,并将每一行分析为零个或多个标记. 然后,将调用 for 循环,并将迭代变量值设置为标记. 默认情况下, /f 从每个文件的每一行传递第一个空格分隔标记. 将跳过空白行.

***
`'findstr /i . List.txt 2^>nul`
正则表达式搜索文本,忽略大小写,`^`是转义符号, `2^>`的意思是把标准错误重定向到`nul`

***
`dism /online /norestart /add-package:"C:\Windows\servicing\Packages\%%i"`

***
`dism`

Deployment Image Servicing and Management (部署映像服务和管理,DISM)工具是用于修改 Windows 映像的命令行工具.
您可以使用DISM直接从命令提示符下启用或禁用Windows功能,或通过将应答文件应用于图像.

`/Online` 指定操作在当前正在运行的操作系统上执行.
`/NoRestart` 取消重新启动. 如果不需要重新启动,则此命令不起作用.
`/Add-Package` 将指定的`.cab` 或`.msu` 包安装在映像中. 仅当目标映像处于脱机状态,装载或应用时,才支持`.msu` 包.

### 查看激活密钥

[查看 Windows 系统正版产品密钥的 3 种方法,重装必备!](https://zhuanlan.zhihu.com/p/115403525)

***
执行下面的命令,就能看到自己的原始产品密钥.

```powershell
wmic path softwarelicensingservice get OA3xOriginalProductKey
```

***
注册表 查看

即便没有`原始产品密钥`,还有一个`备份产品密钥`,需要在注册表中查看.`Win+R` 运行 `Regedit` 打开注册表编辑器找到:

`HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\SoftwareProtectionPlatform`
右侧的 `BackupProductKeyDefault` 值就是你的`备份产品密钥`

YNDMB-2QCFC-HTFBP-JF9HC-FX849

***
软件查看

一些软件也可以查看产品密钥,比如硬件检测软件`AIDA64`和专门用来看密钥的`produkey`

### 查看内存硬件信息

`pwsh` 中输入`wmic memorychip`.

### DistributedCOM错误10016

在计算机上右键--管理--事件查看器--windows日志--系统中
找到错误事件的日志,查看详细信息(XML)视图

可以看到相关的注册表项目的ID,以及`processID`,在任务管理器中可以看到对应的进程名称.
这个问题是由于缺少权限导致的.

`win+R`--`regedit`打开注册表编辑器,
在`HKEY_LOCAL_MACHEINE\SYSTEM\controlSet001\Services\EventLog\System\DCOM`
`HKEY_LOCAL_MACHEINE\SYSTEM\controlSet001\Services\EventLog\System\Microsoft-Windows-DistributedDCOM`
找到上面注册表`ID `的项目,右键--权限--添加,添加自己的账户,然后勾选上所有的权限,点击确定.

### windows 权限

[TrustedInstaller SID](https://docs.microsoft.com/en-us/previous-versions/windows/it-pro/windows-server-2008-R2-and-2008/cc731677(v=ws.10)#trustedinstaller-sid)

windows 有一个比 `Administrator`还高的权限,叫做`TrustedInstallers`.但是可以通过更改所有者为自己,然后就可以修改权限.

windows 管理工具: `%windir%\system32\control.exe\ /name Microsoft.AdministrativeTools`

### win UWP 应用目录

在`文件管理取`地址栏输入`shell:AppsFolder`,大小写无所谓.
或者在`Powershell`中输入`explorer shell:appsfolder`,就可以打开UWP 应用的文件夹,可以创建桌面快捷方式.

### powershell 配置

`oh-my-posh`发展到第三版;
[Oh my Posh 3](https://zhuanlan.zhihu.com/p/308481493)
[A prompt theme engine for any shell.](https://ohmyposh.dev/docs/upgrading)

***
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

***
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

### windows api

[如何从Windows命令行打开回收站](https://qastack.cn/superuser/395015/how-to-open-the-recycle-bin-from-the-windows-command-line)

打开回收站:
在命令行上,键入

```powershell
start shell:RecycleBinFolder
```

在`运行`窗口,或者文件管理器窗口输入

```powershell
shell:recyclebinfolder
```

## 常用软件

### scoop 包管理器

[用 Scoop 管理你的 Windows 软件 ](https://sspai.com/post/52496)

***
安装

在 `PowerShell` 管理员 中输入下面内容,保证允许本地脚本的执行:

```powershell
set-executionpolicy remotesigned -scope currentuser
```

然后执行下面的命令安装 `Scoop`:

```powershell
iex (new-object net.webclient).downloadstring('https://get.scoop.sh')
```

静待脚本执行完成就可以了,安装成功后,让我们尝试一下:

```powershell
scoop help
```

***
`Scoop` 使用方法

从上面的命令中,我们可以发现 `Scoop` 命令的语法是`scoop + 动作 + 对象`的语法.其中`对象`是可省略的.

最常用的几个基础动作有这些:
命令     动作

+ `search`     搜索软件名
+ `install`     安装软件
+ `update`     更新软件
+ `status`     查看软件状态
+ `uninstall`     卸载软件
+ `info`     查看软件详情
+ `home`     打开软件主页

举几个栗子,比如:

我们想要搜索一下有没有 `Firefox` 浏览器: `scoop search firefox`
我们想要安装 `aria2` 下载器: `scoop install aria2 `
我们想要看看 `Typora` 的主页: `scoop home typora `

那么现在安装软件的流程就变成了: `scoop search 软件名` -` scoop install 搜索结果中符合条件的那个`,结束.
更多的进阶命令和使用方法可以参考 Scoop Wiki.

`Scoop` 把软件安装在哪儿?

这就是 `Scoop` 设计最为精致的地方所在了,也是我推荐 `Scoop` 超过 Chocolatey 等更知名的 Windows 软件包管理器的原因.
`Scoop` 和 Homebrew 对软件包安装位置有着相同的处理哲学: `下载, 安装在用户文件夹下`.具体来讲:

`Scoop` 在你的用户根目录(一般是 `C:\Users\用户名`)下创建了一个名为 `scoop` 的文件夹,并默认将软件下载安装到这个文件夹下
`Scoop` 将软件安装到一个相对隔离的环境下(Each program you install is isolated and independent),从而保证环境的统一和路径不被污染

可以看到,`scoop` 文件夹下的 apps 存放有安装的所有应用.值得一提的是: `scoop` 是通过 shim 来软链接一些应用,这样的设计让应用之间不会互相干扰,十分方便.

#### 国内镜像

[scoop 国内镜像](https://gitee.com/squallliu)
[win10 安装scoop的正确姿势](https://impressionyang.oschina.io/2021/02/15/win10-install-scoop/)
[给 Scoop 加上这些软件仓库, 让它变成强大的 Windows 软件管理器](https://sspai.com/post/52710)

### aria2 安装配置

[让 aria2 更容易使用的现代 Web 前端](http://ariang.mayswind.net/zh_Hans/)

这里主要介绍`windows`下的安装配置方法,其他平台类似.
`aria2`程序分成前后端, 后端是一个叫做`aria2c.exe`的程序, 在命令行中运行. 前端是一个网页`index.html`, 在浏览器中运行.

+ 从官网地址[aria2/releases](https://github.com/aria2/aria2/releases)下载系统对应版本的程序,
例如`aria2-1.35.0-win-64bit-build1.zip`,解压缩. 放到一个准备好的目录,比如`d:\aria2`.
+ 从[ mayswind/AriaNg ](https://github.com/mayswind/AriaNg/releases)下载网页版界面, 下载那个`AriaNg-xxx-AllInOne.zip`版本,
可以也放到`aria2`文件夹里面,用浏览器打开里面的`index.html`,它默认会监听本地的`6800`端口,后面我们会配置`aria2`,让它连接上后台服务.
可以`ctrl+D`收藏这个页面,方便下次使用.

+ 参考[Aria2 & YAAW 使用说明](http://aria2c.com/usage.html), 编辑`~\.aria2\aria2.conf`文件, 即名称为`aria2.conf `的文本文件.
在我的电脑上,`~`指的是`C:\Users\qingz`. `qingz`是我账户的名字, 完整路径就是`C:\Users\qingz\.aria2\aria2.conf`.
这是`aria2`寻找配置文件的默认路径. 只需配置一次,以后启动时它会自动读取配置.
+ 配置主要参考[Aria2 新手入门](https://zhuanlan.zhihu.com/p/37021947),我自用的轻微修改版也放在下面. 先修改重要的几行,能正常打开网页版界面即可.
其他配置可以用到再修改, `# `开头的行是注释,可以随便修改. 同样, 要让修改后的配置生效, 记得删掉前面的`#`.

+ 在`aria2`的文件夹,按住`shift+右键`, 点击`在此处打开powershell`窗口, 粘贴这个命令`New-Item -Path . -Name "aria2.session" -ItemType "file"`,
将会新建一个`aria2.session`文件, 它用来记录下载状态. 再输入`Resolve-Path .\aria2.session`, 会得到它的绝对路径.
把这个路径粘贴到`input-file=`, `save-session=`这两行配置的等号右边.
+ 默认下载目录: 改成你自己经常用的下载目录,如`D:\Downloads`. 请使用绝对路径, 路径前后不要加引号, 加引号`aria2`会报错.
+ 开启一些`BT`设置,配置文件有详细说明:

``` bash
enable-dht=true
bt-enable-lpd=true
enable-peer-exchange=true
```

+ 添加 `BT rackers`, 可以改善种子下载速度. `BT rackers`可以在[全网热门 BT Tracker 列表](https://github.com/XIU2/TrackersListCollection)获取.
浏览器按`ctrl+f`搜索`Aria2 format`, 点击展开, 复制`BEST Tracker list:https://trackerslist.com/best_aria2.txt`里面的内容,
粘贴到`bt-tracker=`后面. 不过也可以先不管这一步,后面在图形界面修改更方便.

+ 在`aria2`的文件夹,按住`shift+右键`, 点击`在此处打开powershell`窗口, 输入`.\aria2c.exe`即可运行`aria2`程序.
如果配置有错误,会有提示,根据提示查找修改错误就可以了,很简单.
如果不想看输出信息, 可以用`Start-Job -ScriptBlock {./aria2c.exe}`运行`aria2`, 使用` Get-Job`查看运行状态,.

+ 使用`Stop-Job *; Remove-Job *;`关闭所有后台任务. (这样关闭可能会导致`aria2`来不及保存, 还是在网页端`aria2状态`页面点击`关闭aria2`吧)

如果觉得的每次都这样比较麻烦的话,可以考虑把`aria2c.exe`加入环境变量.  这样运行的时候在命令行输入 `aria2c.exe &` 即可.
修改方法: 按下`win+s`打开搜索窗口,输入`环境变量`,依次点击`编辑环境变量`--`环境变量`--`xxx的用户变量`--`Path`--`新建`--`浏览`,
选中`aria2`的存放目录,添加好之后,一路点击确定即可.
如果你嫌麻烦,也可以使用下文的`powershell`命令. 参考[命令行输出和添加系统环境变量](https://blog.csdn.net/amoscn/article/details/108654236)

```powershell
$mypath='你的路径'; # 这里修改成你的 aria2 的文件夹.
echo "查看现在的路径`n---------`n";$target='User';$path=[Environment]::GetEnvironmentVariable('Path', $target); $path -split ';'
echo "查看修改后的路径`n---------`n";$newPath=$path+';'+$mypath;$newPath -split ';'
# 先不要运行下面的命令,检查上面的命令确保无误之后再运行下面这行, 修改之后,可以再用第二行命令查看修改效果
[Environment]::SetEnvironmentVariable("Path",$newPath,$target)
```

+ 最后回到浏览器, 查看或者再次打开之前的`index.html`文件,一切顺利的话,会看到左边`Aria2状态:已连接`.
+ 如果刚才没有设置好`BT-Tracker`的话,现在可以在`Aria2设置`--`BitTorrent设置`--`BT服务器设置`中修改.其他设置类似.

我使用的配置如下:

```bash
## '#'开头为注释内容, 选项都有相应的注释说明, 根据需要修改 ##
## 被注释的选项填写的是默认值, 建议在需要修改时再取消注释  ##
## 文件保存相关 ##
# 文件的保存路径(可使用绝对路径或相对路径), 默认: 当前启动位置
dir=E:\tools\aria2\downloads
# 启用磁盘缓存, 0为禁用缓存, 需1.16以上版本, 默认:16M
#disk-cache=32M
# 文件预分配方式, 能有效降低磁盘碎片, 默认:prealloc
# 预分配所需时间: none < falloc ? trunc < prealloc
# falloc和trunc则需要文件系统和内核支持
# NTFS(windows)建议使用 falloc, EXT3/4(linux)建议trunc, MAC 下需要注释此项
file-allocation=falloc
# 断点续传
continue=true
## 下载连接相关 ##
# 最大同时下载任务数, 运行时可修改, 默认:5
#max-concurrent-downloads=5
# 同一服务器连接数, 添加时可指定, 默认:1
max-connection-per-server=5
# 最小文件分片大小, 添加时可指定, 取值范围1M -1024M, 默认:20M
# 假定size=10M, 文件为20MiB 则使用两个来源下载; 文件为15MiB 则使用一个来源下载
min-split-size=10M
# 单个任务最大线程数, 添加时可指定, 默认:5
#split=5
# 整体下载速度限制, 运行时可修改, 默认:0
#max-overall-download-limit=0
# 单个任务下载速度限制, 默认:0
#max-download-limit=0
# 整体上传速度限制, 运行时可修改, 默认:0
#max-overall-upload-limit=0
# 单个任务上传速度限制, 默认:0
#max-upload-limit=0
# 禁用IPv6, 默认:false
#disable-ipv6=true
# 连接超时时间, 默认:60
#timeout=60
# 最大重试次数, 设置为0表示不限制重试次数, 默认:5
#max-tries=5
# 设置重试等待的秒数, 默认:0
#retry-wait=0
## 进度保存相关 ##
# 从会话文件中读取下载任务
input-file=E:\tools\aria2\aria2.session
# 在Aria2退出时保存`错误/未完成`的下载任务到会话文件
save-session=E:\tools\aria2\aria2.session
# 定时保存会话, 0为退出时才保存, 需1.16.1以上版本, 默认:0
#save-session-interval=60
## RPC相关设置 ##
# 启用RPC, 默认:false
enable-rpc=true
# 允许所有来源, 默认:false
rpc-allow-origin-all=true
# 允许非外部访问, 默认:false
rpc-listen-all=true
# 事件轮询方式, 取值:[epoll, kqueue, port, poll, select], 不同系统默认值不同
#event-poll=select
# RPC监听端口, 端口被占用时可以修改, 默认:6800
rpc-listen-port=6800
# 设置的RPC授权令牌, v1.18.4新增功能, 取代 --rpc-user 和 --rpc-passwd 选项, 新手可以先不管
#rpc-secret=<TOKEN>
# 设置的RPC访问用户名, 此选项新版已废弃, 建议改用 --rpc-secret 选项
#rpc-user=<USER>
# 设置的RPC访问密码, 此选项新版已废弃, 建议改用 --rpc-secret 选项
#rpc-passwd=<PASSWD>
# 是否启用 RPC 服务的 SSL/TLS 加密,
# 启用加密后 RPC 服务需要使用 https 或者 wss 协议连接
#rpc-secure=true
# 在 RPC 服务中启用 SSL/TLS 加密时的证书文件,
# 使用 PEM 格式时,您必须通过 --rpc-private-key 指定私钥
#rpc-certificate=/path/to/certificate.pem
# 在 RPC 服务中启用 SSL/TLS 加密时的私钥文件
#rpc-private-key=/path/to/certificate.key
## BT/PT下载相关 ##
# 当下载的是一个种子(以.torrent结尾)时, 自动开始BT任务, 默认:true
#follow-torrent=true
# BT监听端口, 当端口被屏蔽时使用, 默认:6881-6999
listen-port=51413
# 单个种子最大连接数, 默认:55
#bt-max-peers=55
# 打开DHT功能, 如果是PT, 比如6v,蒲公英等等,需要禁用, 默认:true
enable-dht=true
# 打开IPv6 DHT功能,PT需要禁用
#enable-dht6=false
# DHT网络监听端口, 默认:6881-6999
#dht-listen-port=6881-6999
# 本地节点查找, PT需要禁用, 默认:false
bt-enable-lpd=true
# 种子交换, PT需要禁用, 默认:true
enable-peer-exchange=true
# 每个种子限速, 对少种的PT很有用, 默认:50K
#bt-request-peer-speed-limit=50K
# 客户端伪装, PT需要
peer-id-prefix=-TR2770-
user-agent=Transmission/2.77
peer-agent=Transmission/2.77
# 当种子的分享率达到这个数时, 自动停止做种, 0为一直做种, 默认:1.0
seed-ratio=0
# 强制保存会话, 即使任务已经完成, 默认:false
# 较新的版本开启后会在任务完成后依然保留.aria2文件
#force-save=false
# BT校验相关, 默认:true
#bt-hash-check-seed=true
# 继续之前的BT任务时, 无需再次校验, 默认:false
bt-seed-unverified=true
# 保存磁力链接元数据为种子文件(.torrent文件), 默认:false
# bt-tracker 链接, 记得修改成最新的, 粘贴到等号后面. https://github.com/XIU2/TrackersListCollection
# bt-tracker=
```

### WSL2

[ Win10 Terminal + WSL 2 安装配置指南](https://www.cnblogs.com/willick/p/13924325.html)
[设置从Windows Terminal打开wsl时进入Linux用户主目录](https://blog.csdn.net/baidu_33340703/article/details/106949948)
[适用于 Linux 的 Windows 子系统安装指南 (Windows 10)](https://docs.microsoft.com/zh-cn/windows/wsl/install-win10)
[如何在windows的资源管理器下访问WSL2?](https://www.zhihu.com/question/398307967)

`wsl`目录: 在资源管理器输入:`\\wsl$`

安装适用于 `Linux` 的 `Windows` 子系统 (`WSL`) 时有两个选项:

***
简化安装 (预览版) : `wsl --install`. 要使用 `wsl --install` 简化安装命令,必须先完成以下操作:

+ 加入 Windows 预览体验计划
+ 安装 Windows 10 的预览版(OS 版本 20262 或更高版本).
+ 使用管理员特权打开命令行窗口

满足这些要求后,可通过以下方式安装 `WSL`:

在管理员模式下打开命令行,并输入以下命令: `wsl.exe --install`
重启计算机

首次启动新安装的 `Linux` 分发版时,将打开一个控制台窗口,要求你等待将文件解压缩并存储到电脑上. 未来的所有启动时间应不到一秒.
然后,需要为新的 `Linux` 分发版创建用户帐户和密码.

祝贺你!现已成功安装并设置了与 Windows 操作系统完全集成的 `Linux` 分发!

`--install` 命令执行以下操作:

+ 启用可选的 WSL 和虚拟机平台组件
+ 下载并安装最新 Linux 内核
+ 将 WSL 2 设置为默认值
+ 下载并安装 Linux 分发版(可能需要重启)

默认情况下,安装的 `Linux` 分发版为 `Ubuntu`. 可以使用 `wsl --install -d <Distribution Name>` 进行更改.
(将 `<Distribution Name>` 替换为所需分发版的名称.)初始安装后,可以使用 `wsl --install -d <Distribution Name>` 命令将其他 Linux 分发版添加到计算机.

若要查看可用 `Linux` 分发版的列表,请输入 `wsl --list --online`.

***
手动安装步骤

1. 先启用`适用于 Linux 的 Windows 子系统`可选功能,然后才能在 `Windows` 上安装 `Linux` 分发.

以管理员身份打开 PowerShell 并运行:

```powershell
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
```

建议现在转到步骤 #2,更新到 WSL 2,但如果只想安装 WSL 1,现在可以重新启动计算机,然后继续执行步骤 6 - 安装所选的 Linux 发行版.
若要更新到 WSL 2,请等待重新启动计算机,然后继续执行下一步.

2. 更新到 WSL 2

对于 x64 系统: 版本 1903 或更高版本,采用 内部版本 18362 或更高版本.
对于 ARM64 系统: 版本 2004 或更高版本,采用 内部版本 19041 或更高版本.
低于 18362 的版本不支持 WSL 2. 使用 Windows Update 助手更新 Windows 版本.

若要检查 Windows 版本及内部版本号,选择 `Windows 徽标键 + R`,然后键入`winver`,选择`确定`. (或者在 `Windows` 命令提示符下输入 `ver` 命令).
更新到"设置"菜单中的最新 Windows 版本.

3. 启用虚拟机功能

安装 WSL 2 之前,必须启用`虚拟机平台`可选功能.

以管理员身份打开 PowerShell 并运行:

```powershell
dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart
```

重新启动 计算机,以完成 `WSL` 安装并更新到 `WSL 2`.

4. 下载 Linux 内核更新包

下载最新包:
[适用于 x64 计算机的 WSL2 Linux 内核更新包](https://wslstorestorage.blob.core.windows.net/wslblob/wsl_update_x64.msi)

备注
如果使用的是 ARM64 计算机,请下载 ARM64 包. 如果不确定自己计算机的类型,请打开命令提示符或 PowerShell,并输入: `systeminfo | find "System Type"`.
运行上一步中下载的更新包. (双击以运行 - 系统将提示你提供提升的权限,选择`是`以批准此安装.)
安装完成后,请继续执行下一步 - 在安装新的 Linux 分发时,将 WSL 2 设置为默认版本. (如果希望将新的 Linux 安装设置为 WSL 1,请跳过此步骤.)

5. 将 WSL 2 设置为默认版本

打开 PowerShell,然后在安装新的 Linux 发行版时运行以下命令,将 `WSL 2` 设置为默认版本:

```powershell
wsl --set-default-version 2
```

备注

从 WSL 1 更新到 WSL 2 可能需要几分钟才能完成,具体取决于目标分发版的大小.
 如果从 Windows 10 周年更新或创意者更新运行 WSL 1 的旧(历史)安装,可能会遇到更新错误. 按照这些说明卸载并删除任何旧分发.

如果 `wsl --set-default-version` 结果为无效命令,请输入 `wsl --help`.
如果 `--set-default-version` 未列出,则表示你的 OS 不支持它,你需要更新到版本 `1903`(内部版本 18362)或更高版本.

运行命令后如果看到此消息: `WSL 2 requires an update to its kernel component. For information please visit https://aka.ms/wsl2kernel`.
仍需要安装 MSI Linux 内核更新包.

6. 安装所选的 `Linux` 分发

打开 `Microsoft Store`,并选择你偏好的 `Linux` 分发版.在分发版的页面中,选择"获取".
首次启动新安装的 `Linux` 分发版时,将打开一个控制台窗口,系统会要求你等待一分钟或两分钟,以便文件解压缩并存储到电脑上. 未来的所有启动时间应不到一秒.
然后,需要为新的 `Linux` 分发版创建用户帐户和密码.

7. 安装 Windows 终端(可选)

Windows 终端可启用多个选项卡(在多个 `Linux` 命令行, Windows 命令提示符, PowerShell 和 Azure CLI 等之间快速切换),
创建键绑定(用于打开或关闭选项卡, 复制粘贴等的快捷方式键), 使用搜索功能,以及使用自定义主题(配色方案, 字体样式和大小, 背景图像/模糊/透明度).

8. 将分发版版本设置为 WSL 1 或 WSL 2

可打开 `PowerShell` 命令行并输入以下命令(仅在 Windows 内部版本 18362 或更高版本中可用),检查分配给每个已安装的 `Linux` 分发版的 WSL 版本:

```powershell
wsl --list --verbose
```

若要将分发版设置为受某一 WSL 版本支持,请运行:

```powershell
wsl --set-version <distribution name> <versionNumber>
wsl --set-version Ubuntu20.04 2 # 把 ubuntu 20.04 设置为 wsl2
```

请确保将 `<distribution name>` 替换为你的分发版的实际名称,并将 `<versionNumber>` 替换为数字`1`或`2`.
 可以随时更改回 WSL 1,方法是运行与上面相同的命令,但将`2`替换为`1`.

此外,如果要使 WSL 2 成为你的默认体系结构,可以通过此命令执行该操作:

```powershell
wsl --set-default-version 2
```

这会将安装的任何新分发版的版本设置为 `WSL 2`.

## windows terminal

可以设置的选项非常多,参考
[Windows 终端中的全局设置](https://docs.microsoft.com/zh-cn/windows/terminal/customize-settings/global-settings)
[Windows 终端中的配置文件设置](https://docs.microsoft.com/zh-cn/windows/terminal/customize-settings/profile-settings#unique-identifier)

如果希望将某个设置应用于所有配置文件,可以将其添加到 `settings.json` 文件中的配置文件列表上方的 `defaults` 部分.

```json
"defaults":
{
    // SETTINGS TO APPLY TO ALL PROFILES
},
"list":
[
    // PROFILE OBJECTS
]
```

### 等宽字体

[Cascadia Code](https://docs.microsoft.com/zh-cn/windows/terminal/cascadia-code)

`Cascadia Code` 是 `Microsoft` 提供的一种新的等宽字体,可为命令行应用程序和文本编辑器提供全新的体验.
`Cascadia Code` 是与 `Windows` 终端一起开发的. 建议将此字体与终端应用程序和文本编辑器(如 `Visual Studio` 和 `Visual Studio Code`)一起使用.

编程连字是通过组合字符创建的字形.  它们在编写代码时最有用.  "Code"变体包含连字, 而"Mono"变体不包含连字.

![连字](https://docs.microsoft.com/zh-cn/windows/terminal/images/programming-ligatures.gif)

+ 简体中文等距更纱黑体+Nerd图标字体库:

[laishulu/Sarasa-Mono-SC-Nerd ](https://github.com/laishulu/Sarasa-Mono-SC-Nerd)

国内镜像:
[whjkd / Sarasa-Mono-SC-Nerd ](https://gitee.com/whjkd/Sarasa-Mono-SC-Nerd)

### 添加其他程序

[Adding profiles for third-party tools](https://github.com/microsoft/terminal/blob/main/doc/user-docs/ThirdPartyToolProfiles.md)

Git Bash

假设`Git Bash`安装到了`C:\\Program Files\\Git`:

```json
{
    "name": "Git Bash",
    "commandline": "C:\\Program Files\\Git\\bin\\bash.exe -li",
    "icon": "C:\\Program Files\\Git\\mingw64\\share\\git\\git-for-windows.ico",
    "startingDirectory": "%USERPROFILE%"
}
```

### 创建自己的配色方案

关于配色方案可以参考: [Windows 终端中的配色方案](https://docs.microsoft.com/zh-cn/windows/terminal/customize-settings/color-schemes)
可以在 `settings.json` 文件的 `schemes` 数组中定义配色方案. 它们是使用以下格式写入的:

```json
{
    "name" : "Campbell",
    "cursorColor": "#FFFFFF",
    "selectionBackground": "#FFFFFF",
    "background" : "#0C0C0C",
    "foreground" : "#CCCCCC",
    "black" : "#0C0C0C",
    "blue" : "#0037DA",
    "cyan" : "#3A96DD",
    "green" : "#13A10E",
    "purple" : "#881798",
    "red" : "#C50F1F",
    "white" : "#CCCCCC",
    "yellow" : "#C19C00",
    "brightBlack" : "#767676",
    "brightBlue" : "#3B78FF",
    "brightCyan" : "#61D6D6",
    "brightGreen" : "#16C60C",
    "brightPurple" : "#B4009E",
    "brightRed" : "#E74856",
    "brightWhite" : "#F2F2F2",
    "brightYellow" : "#F9F1A5"
},
```

除 `name` 以外,每个设置都接受十六进制格式(`"#rgb"` 或 `"#rrggbb"`)的字符串形式的颜色. `cursorColor` 和 `selectionBackground` 设置是可选的.

***
`terminal` 自带的配色方案
`Windows` 终端将这些配色方案包含在 `defaults.json` 文件中,可按住 `alt` 并选择设置按钮来访问该文件.
如果要在一个命令行配置文件中设置配色方案,请添加 `colorScheme` 属性,并将配色方案的 `name` 作为值.

```json
"colorScheme": "COLOR SCHEME NAME"
```

可以使用下列颜色方案

```json
Campbell
Campbell Powershell
Vintage
One Half Dark
One Half Light
Solarized Dark
Solarized Light
Tango Dark
Tango Light
```

#### 终端提示

[Windows 终端提示与技巧](https://docs.microsoft.com/zh-cn/windows/terminal/tips-and-tricks)

可以通过按住 `ctrl` 和滚动来缩放 `Windows` 终端的文本窗口. 缩放后,终端会话将保持新的缩放效果.
如果要更改字体大小,可参阅配置文件设置页面.

可以通过按住 `ctrl+shift `和滚动来调整背景的不透明度. 调整后,终端会话将保持新的不透明度.
如果要更改配置文件的 `acrylic` 不透明度,可参阅配置文件设置页面.

### 可执行文件设置

***
命令行

这是在配置文件中使用的可执行文件.
属性名称:  `commandline`
必要性:  可选
接受:  字符串形式的可执行文件名
默认值:  `"cmd.exe"`

***
源

这会存储源自配置文件的配置文件生成器的名称.
此字段没有可发现的值. 有关动态配置文件的其他信息,请访问动态配置文件页.
属性名称:  `source`
必要性:  可选
接受:  字符串

***
起始目录

这是加载 `shell` 时所处的起始目录.
属性名称:  `startingDirectory`
必要性:  可选
接受:  字符串形式的文件夹位置
默认值:  `"%USERPROFILE%"`

备注
在为已安装的 WSL 分发设置打开时的起始目录时,应使用以下格式: `"startingDirectory": "//wsl$/"`,并用分发的名称进行替换.
例如,`"startingDirectory": "//wsl$/Ubuntu-20.04"`.

### 下拉列表设置

***
名称

这是将在下拉菜单中显示的配置文件的名称. 此值还用作在启动时传递给 shell 的"标题".
某些 shell(如 bash)可能会选择忽略此初始值,而其他 shell(Command Prompt, PowerShell)可能会在应用程序的生存期内使用此值.
可以使用 tabTitle 替代此"标题"行为.
属性名称:  `name`
必要性:  必需
接受:  字符串

***
图标

这会设置选项卡和下拉菜单中显示的图标.
属性名称:  `icon`
必要性:  可选
接受:  字符串形式的文件位置

***
隐藏下拉列表中的某个配置文件

如果 `hidden` 设置为 `true`,则配置文件将不会显示在配置文件列表中.
这可用于隐藏默认配置文件和动态生成的配置文件,同时并将它们保留在设置文件中. 若要详细了解动态配置文件,请访问动态配置文件页.
属性名称:  `hidden`
必要性:  可选
接受:  `true`,`false`
默认值:  `false`
这两个`value`都不带`"`

### 文本设置

***
字体

这是配置文件中使用的字体名称. 如果找不到或无效,终端将尝试回退到 `Consolas`.
若要了解默认字体 (`Cascadia Mono`) 的其他变体,请访问 `Cascadia Code` 页.
属性名称:  `fontFace`
必要性:  可选
接受:  字符串形式的字体名称.
默认值:  `"Cascadia Mono"`

***
字体大小

这将设置配置文件的字体大小(以磅为单位).
属性名称:  `fontSize`
必要性:  可选
接受:  整数
默认值:  `12`

***
字体粗细(预览)

此属性设置配置文件字体的粗细(笔画的粗细).
属性名称:  `fontWeight`
必要性:  可选
接受:  `"normal", "thin", "extra-light", "light", "semi-light", "medium", "semi-bold", "bold", "extra-bold", "black", "extra-black"`,或与 `OpenType` 字体粗细的数值表示形式相对应的整数
默认值:  `"normal"`
此功能仅在 Windows 终端预览中可用.

***
消除文本锯齿

此方法控制呈现器中文本的消除锯齿方式. 请注意,更改此设置将需要启动新的终端实例.
`Windows` 终端消除文本锯齿
属性名称:  `antialiasingMode`
必要性:  可选
接受:  `"grayscale", "cleartype", "aliased"`

默认值: `"grayscale"`

***
键盘设置

`AltGr` 别名(预览)
通过它可以控制 `Windows` 终端是否将 `ctrl+alt` 视为 `AltGr` 的别名.
属性名称:  `altGrAliasing`
必要性:  可选
接受:  `true`, `false`
默认值:  `true`
此功能仅在 Windows 终端预览中可用.

### 颜色设置

***
配色方案

这是配置文件中使用的配色方案名称. 配色方案是在 `schemes` 对象中定义的. 可以在配色方案页上找到更详细的信息.
属性名称:  `colorScheme`
必要性:  可选
接受:  字符串形式的配色方案的名称

默认值: `"Campbell"`

### Acrylic 设置

***
启用 `acrylic`

如果设置为 `true`,窗口将使用 `acrylic` 背景. 设置为 `false` 时,窗口将为普通的, 不带纹理的背景. 由于操作系统限制,透明度仅适用于焦点窗口.
属性名称:  `useAcrylic`
必要性:  可选
接受:  `true`, `false`
默认值:  `false`

***
`Acrylic` 不透明度

将 `useAcrylic` 设置为 `true` 时,这会设置该配置文件的窗口透明度. 接受的浮点值为 `0-1`.
属性名称:  `acrylicOpacity`
必要性:  可选
接受:  `0-1` 的浮点值的数字
默认值:  `0.5`

### 背景图像设置

***
设置背景图像

这将设置要在窗口背景上绘制的图像的文件位置. 背景图像可以是 `.jpg`, `.png `或 `.gif` 文件.
属性名称:  `backgroundImage`
必要性:  可选
接受:  字符串形式的文件位置

***
背景图像拉伸模式

这将设置如何调整背景图像的大小以填充窗口.
属性名称:  `backgroundImageStretchMode`
必要性:  可选
接受:  `"none", "fill", "uniform", "uniformToFill"`
默认值:  `"uniformToFill"`

***
背景图像对齐

这会设置背景图像与窗口边界对齐的方式.
属性名称:  `backgroundImageAlignment`
必要性:  可选
接受:  `"center", "left", "top", "right", "bottom", "topLeft", "topRight", "bottomLeft", "bottomRight"`
默认值:  `"center"`

***
背景图像不透明度

这会设置背景图像的透明度.
属性名称:  `backgroundImageOpacity`
必要性:  可选
接受:  `0-1` 的浮点值的数字
默认值:  `1.0`

### 滚动设置

滚动条可见性
这将设置滚动条的可见性.
属性名称:  `scrollbarState`
必要性:  可选
接受:  `"visible", "hidden"`

***
键入时滚动到输入行

如果设置为 `true`,则在键入时,窗口将滚动到命令输入行. 如果设置为 `false`,则在开始键入时,窗口不会滚动.
属性名称:  `snapOnInput`
必要性:  可选
接受:  `true`, `false`
默认值:  `true`

***
历史记录大小

这会设置在窗口显示的内容上方可以回滚的行数.
属性名称:  `historySize`
必要性:  可选
接受:  整数
默认值:  `9001`

### 退出时配置文件的关闭方式

这将设置配置文件如何响应终止或启动失败.
当键入 `exit` 或进程正常退出时,`"graceful"` 将关闭配置文件. `"always"` 将始终关闭配置文件,而 `"never"` 将永远不会关闭配置文件.
`true` 和 `false` 分别被接受为 `"graceful"` 和 `"never"` 的同义词.
属性名称:  `closeOnExit`
必要性:  可选
接受:  `"graceful", "always", "never", true, false`
默认值:  `"graceful"`

### WSL2 参考的对象类型不支持尝试的操作

使用 `VPN` 造成的 WSL2 启动 crash

[failing to startup with code 4294967295](https://github.com/microsoft/WSL/issues/5092#:~:text=Solve%20%22process%20exited%20with%20code%204294967295%22%20%2C%20run,complete%20the%20reset.%20Does%20not%20resolve%20the%20issue.)
[关于使用WSL2出现'参考的对象类型不支持尝试的操作'的解决方法](https://zhuanlan.zhihu.com/p/151392411)

长期解决的方案(推荐),下载此软件: [http://www.proxifier.com/tmp/Test20200228/NoLsp.exe](http://www.proxifier.com/tmp/Test20200228/NoLsp.exe)

因需要梯子访问下载,有些朋友不方便,所以我上传到百度云分享在这里: [https://pan.baidu.com/s/1bVZ0OXZPxEt8l1IHYaFK3A](https://pan.baidu.com/s/1bVZ0OXZPxEt8l1IHYaFK3A) ,提取码: `vjge`

然后在管理员身份运行`CMD`输入:

```pwsh
NoLsp.exe C:\windows\system32\wsl.exe
```

请自行注意`NoLsp.exe`程序的位置,以及你的`wsl.exe`位置.

***
产生原因和解决方法分析:

代理软件和`wsl2`的`sock`端口冲突,使用`netsh winsock reset`重置修复. `Proxifer` 开发人员解释如下:

如果`Winsock LSP DLL`被加载到其进程中,则`wsl.exe`将显示此错误.
最简单的解决方案是对`wsl.exe`使用`WSCSetApplicationCategory WinAPI`调用来防止这种情况.
在后台,该调用在`HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\WinSock2\Parameters\AppId_Catalog`中为`wsl.exe`创建一个条目.
这将告诉`Windows`不要将`LSP DLL`加载到`wsl.exe`进程中.

## 公众号排版

TMD "小可爱" 微信不支持 SM.MS 这种图床,可能是读取速度有点慢就直接拒绝了.码的纸张吧.

转自: [Typora公众号写作与排版 ](https://sspai.com/post/40524#!)

利用 `Markdown` 排版公众号文章. 所需工具如下:

+ `Typora`. [typora 编辑器](https://www.typora.io/)
+ `PicGo` 图床 app [ PicGo](https://picgo.github.io/PicGo-Doc/zh/guide/)
+ `CSS`的基本知识, 稍微了解即可.

>`CSS`, 层叠样式表(英文全称: Cascading Style Sheets)是一种用来表现 `HTML` (准通用标记语言的一个应用)或 `XML`(准通用标记语言的一个子集)等文件样式的计算机语言.
>`CSS` 不仅可以静态地修饰网页, 还可以配合各种脚本语言动态地对网页各元素进行格式化. [MDN: 什么是CSS](https://developer.mozilla.org/zh-CN/docs/Learn/CSS/First_steps/What_is_CSS)

可以解决以下问题:

+ 图片上传: 在编辑器中写好文章, 上传平台的时候还得手动粘贴
+ `Markdown` 编辑器中的文章上传至公众号之前, 还得各种转码.

做好相关配置后, 写作和排版能够同步完成, 直接粘贴至公众号编辑器中即可.

排版的基本思路是:

网页上的`内容`和`排版`是分开的, 内容编辑好以后, 再使用 `CSS` 样式文件完成字号, 行间距, 背景, 颜色等排版.
所以说, 配置好 `CSS` 文件就相当于一劳永逸的完成了排版工作, 下次只要套用 `CSS` 就好了.

我们使用`Typora`编辑器, [让 Markdown 写作更简单, 免费极简编辑器: Typora](https://sspai.com/post/30292)

+ `Typora` 的编辑逻辑是`所见即所得`, 输入 `Markdown` 标记后, 会根据选择的 `主题` 即时渲染成相应格式.
+ `主题` 是使用 `CSS` 文档定义的, 只要修改 `CSS` 文档中的对应参数, 即可修改主题的样式.
+ 所以本质上, `Typora` 就是一个 `HTML/CSS` 的渲染器.
+ 在`Typora`中, 通过按下`Ctrl+/`, 或者`视图->源代码模式`, 可以在渲染视图和源代码视图之间切换.

![所见即所得](https://cdn.sspai.com/2017/08/20/882f2dc69f0223330d31a09a64313c0e.gif)

借助`Typora`, 我们可以非常方便的完成排版:

+ 在 `Markdown` 中写作, 自动上传本地图片到图床;
+ 应用调整好的`CSS` 格式, 粘贴`富文本`格式到微信中.

### 插件配置

+ 使用 `Typora` 编写 `Markdown` , 通过配置`图床`插件, 自动将本地图片转成在线图片.
+ 先安装合适的`Typora`插件.  英文好的同学可以查看 [Typora 的官方参考](https://support.typora.io/Upload-Image/#configuration) :
    + `mac` 用户: [iPic + Typora, 方便快捷地在 Markdown 中插图](https://sspai.com/post/36275).
    + `windows` 用户: 安装 [PicGo app](https://picgo.github.io/PicGo-Doc/zh/guide/), 下载地址在 [PicGo.Github](https://github.com/Molunerfinn/PicGo/releases), 打开链接, 按下`Ctrl+F` 搜索`x64.exe` 安装包.如果网络原因不能访问`Github`, 可以下载 [网盘备份](https://www.aliyundrive.com/s/kSzsKeQRHB5).
    + 下载之后,选择你喜欢的路径安装, 运行. 点击侧栏中的`SM.MS图床`, 下一步 `设定Token`.
+ 注册图床账号, 例如 [sm.ms](https://sm.ms/). 这一步我不是很熟悉,也许图床不注册也能用.
这里以 `sm.ms` 为例, 注册, 验证邮箱之后, 网站右上角点击 `User->Dashboard`, -> 侧边栏选择 `API Token`, 点击 `Generate Secret Token-> 确定`, 文本框中会生成一段乱码,复制到上一步 `PicGo` 的输入栏里.
+ 测试一下: 在 `PicGo` 侧栏选择`上传区`. 随便拖张图片, 到 `PicGo` 的上传提示框, 应该会自动复制 `图片链接` 到 `剪贴板` . 上传框下面的`链接格式`可以更改生成链接的格式, 我们先保持默认的`Markdown` 就好.
把生成的图片链接直接复制到`Typora`编辑界面中, 看能否预览.
图片链接的`Markdown`格式应该类似于:

        ![](https://i.loli.net/2021/10/03/Lz3nNVyDjrGfXkb.png)

+ 在`Typora`的`文件->偏好设置->侧栏:图像->上传服务设定`中设置:

        上传服务 ->PicGo(app), PicGo 路径-> 你的安装路径.

这里有一个`验证图片选项`, 我验证失败了,但是不影响使用, 暂且不管.
现在使用`Typora`编辑`Markdown`的时候,直接把图片拖放到`Typora`中,它会自动上传图片.

我们需要做的只是找到 `CSS` 文件的目录, 修改目标格式, 写好文章然后复制粘贴.
使用`文件->偏好设置->侧边栏:外观->打开主题文件夹`, 即可打开 `CSS` 文件目录.

![a](https://i.loli.net/2021/10/03/y59ekXcoV4fIpt1.png)
![b](https://i.loli.net/2021/10/03/Lz3nNVyDjrGfXkb.png)

参考[sspai老哥](https://sspai.com/post/40524#!),
>在目录中预设了很多主题, 我个人比较喜欢 `Github` 的样式, 所以我的排版样式是基于 `Github` 修改的.
(好吧, 其实是我的水平太低, 让我重写一个 `CSS` 还不如去死)
> 我在目录内新建了名为`WeChat`的 `CSS` 文件. >在电脑上我还是倾向于使用原生 `GitHub` 主题, 在公众号文章中才会选择自定义的格式.`GitHub` 原来的样式已经不错了, 只是在手机端浏览时, 行距, 页边距, 字号等不太合适.
> 另外, 我修改了部分颜色, 看起来不是那么单调.

复制`github.css -> wechat.css`, 然后根据个人喜好调整:

```css
/* 修改正文部分, 页边距为 0.5em, 行高增加至 1.5em. p 表示段落 , 参考
https://developer.mozilla.org/zh-CN/docs/Learn/Getting_started_with_the_web/CSS_basics */
p {
    margin: 0.8em 0.5em;
    line-height: 1.5em;
}
/* 修改标题及引用部分的边线颜色 */
h2 {
   padding-bottom: .3em;
    font-size: 1.5em;
    line-height: 1.225;
    border-bottom: 1px solid #FFBF00;
    text-align: center
}
blockquote {
    border-left: 4px solid #FFBF00;
    padding: 0 15px;
    color: #777777;
}
```

[wechat.css 示例 网盘链接](https://www.aliyundrive.com/s/fsGbvVLZ1zH)

此外, 在公众号文章页面, 按下`F12`进入开发者模式,可以查看相关的`CSS` 样式表

![CSS style sheet](https://i.loli.net/2021/10/03/8jObYiqfgodmxeB.png)

### 查看修改后的样式

`Typora` 可以自行选择用于渲染的 `CSS` 文件, 在电脑写作时, 我会选择`Github`,
在发布前, 我会选择`菜单栏:主题->WeChat`, 然后粘贴到公众号编辑器中.

修改前和修改后的样式如下:

![GithubTheme](https://i.loli.net/2021/10/03/64aM9yRwGPXpE7O.png)

![WeChatTheme](https://i.loli.net/2021/10/03/voMjHNIh48OP7D9.png)

文章写完, 选择好想要的样式, `Ctrl+A,Ctrl+C` 复制到公众号编辑器中就 `OK` 了!
`Typora` 中使用的排版样式, 会完整的复制到公众号文章中, 真正的所见即所得.

![最终样式](https://i.loli.net/2021/10/03/Xkpfw3EGFIsVUtm.png)

### 其他参考

[使用 Markdown + CSS 搞定公众号的排版规范 ](https://sspai.com/post/59091)
[Markdown + CSS 实现微信公众号排版](https://cloud.tencent.com/developer/article/1051711)
[用CSS样式为微信公众号排版](https://zhuanlan.zhihu.com/p/23809384)

[wechat-mp-article](https://github.com/ufologist/wechat-mp-article)
[中文网页重设与排版.css](https://github.com/sofish/typo.css)
[可配置的,更适合阅读的中文文章样式库 ](https://github.com/zmmbreeze/Entry.css)

`typora` 官方安装配置命令行版 `PicGo` 教程.

[Install PicGo-Core via npm](https://support.typora.io/Upload-Image/#image-uploaders)

通过 `node` 包管理器安装 `PicGo-Core`(需要 `NodeJS`运行时).
如果你安装了 `node` 或 `yarn`,你可以在终端运行以下命令.

```bash
npm install picgo -g
# 或
yarn global add picgo
```

然后你可以在终端输入 `which picgo` 来获得它的实际安装位置,
然后选择 `自定义命令` 作为 `图片上传器` 功能,并输入`[ node.js 的路径] [picgo-core路径] upload` 作为命令.
如果安装了 `node` 和 `picgo` 到系统`PATH`,你也可以直接填写 `picgo upload` 作为自定义命令.

#### 配置PicGo-Core

+ 选项1: 编辑配置文件.  请在以下位置编辑 `config.json`

+ Linux / macOS ->  `~/.picgo/config.json`.
+ Windows -> `C:\Users\[您的用户名]\.picgo\config.json`.

细节可以参考, [配置文件](https://picgo.github.io/PicGo-Core-Doc/zh/guide/config.html) (Chinese Only).

`picgo` 需要配置文件来启动. 当你未指定配置文件的时候, `picgo` 将会使用默认配置文件来启动.

通常来说你只需要配置 `Uploader` 即可,所以你可以通过 `picgo set uploader` 来进入交互式命令行,
配置成功后会自动生成配置文件,无需复制粘贴! 其他更多的命令可以参考 `CLI` 命令 一章.
