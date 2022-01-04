# scoop 包管理器

[用 Scoop 管理你的 Windows 软件 ](https://sspai.com/post/52496)
[Scoop 相关技巧和知识](https://www.thisfaner.com/p/scoop/)

## 安装

在 `PowerShell` 管理员 中输入下面内容,保证允许本地脚本的执行:

```powershell
set-executionpolicy remotesigned -scope currentuser
# 或者 (但是它没有上面的命令安全)
set-executionpolicy Unrestricted -scope currentuser
```

安装路径:

+ 用户级别安装的程序和 `Scoop` 本身, 默认安装于 `C:\Users\<user>\scoop`
+ 全局安装的程序(所有用户可用, 使用 `--global` 或 `-g` 选项), 位 于 `C\ProgramData\scoop` 路径中.

可以通过更改对应的环境变量更改这些路径:

+ 将 `Scoop` 安装到自定义目录 :
    打开 `PowerShell` 先配置环境变量 `SCOOP`, 再运行 `iex`

```powershell
$env:SCOOP='D:\Scoop' # 将路径保存在变量中
# 先添加用户级别的环境变量 SCOOP
[environment]::setEnvironmentVariable('SCOOP',$env:SCOOP,'User')
# 然后下载安装 Scoop (如果使用默认安装路径则直接运行下面的命令)
# iex = Invoke-Expression
iex (new-object net.webclient).downloadstring('https://get.scoop.sh')
# 或者使用下面的命令安装. iwr = Invoke-WebRequest,
# -useb = -UseBasicParsing, 这个参数已被弃用. 从 Pwsh 6.0.0开始, 所有Web请求都只使用基本解析. 使用此参数对cmdlet的操作没有影响.
iwr -useb get.scoop.sh | iex
```

静待脚本执行完成就可以了,安装成功后,让我们尝试一下:

```powershell
scoop help
```

### 配置全局安装路径(可选, 建议不改)

```powershell
$env:SCOOP_GLOBAL='D:\GlobalScoopApps'
[environment]::setEnvironmentVariable('SCOOP_GLOBAL',$env:SCOOP_GLOBAL,'Machine')
```

相当于在系统变量中设置:  `SCOOP_GLOBAL=D:\GlobalScoopApps`; 默认是在 `C:\ProgramData\scoop`.

>为什么需要全局安装?
>对于那些需要管理员权限的程序需要进行全局安装.
>我当前遇到的是当使用 Scoop 安装字体时需要使用全局安装, 因为字体需要给所有用户使用.

初次安装 `Scoop` 后, 建议安装的程序:

```powershell
# 在 scoop 进行全局安装时需要使用到 sudo 命令
scoop install sudo
# scoop下载程序时支持使用 aria2 来加速下载
scoop install aria2
```

我们可以发现, 下载的过程中自动下载了依赖 `7-zip`.
在安装方面, 它利用了 `7zip` 去解 压安装包/压缩包, 因此它对绿色软件有天生的友好属性 .
不仅如此, 下载之后的内容会自动将加入到(Path)环境变量中, 十分方便.

补充:  初次安装之后我们可以通过运行 `scoop checkup` 来检测当前潜在问题, 然后根据提示进行修正.

```log
# 检测本人当前环境存在的问题
scoop checkup

WARN  Windows Defender may slow down or disrupt installs with realtime scanning.
  Consider running:
    sudo Add-MpPreference -ExclusionPath 'D:\Scoop\Applications'
  (Requires 'sudo' command. Run 'scoop install sudo' if you don't have it.)
WARN  Windows Defender may slow down or disrupt installs with realtime scanning.
  Consider running:
    sudo Add-MpPreference -ExclusionPath 'C:\ProgramData\scoop'
  (Requires 'sudo' command. Run 'scoop install sudo' if you don't have it.)
WARN  LongPaths support is not enabled.
You can enable it with running:
    Set-ItemProperty 'HKLM:\SYSTEM\CurrentControlSet\Control\FileSystem' -Name 'LongPathsEnabled' -Value 1
ERROR 'dark' is not installed! It's required for unpacking installers created with the WiX Toolset. Please run 'scoop install dark' or 'scoop install wixtoolset'.
WARN  Found 4 potential problems.
```

可以看到存在三个警告(WARN), 一个错误(ERROR), 并给出了解决对应问题的命令:

+ 前两个警告(WARN)提示: 杀毒软件 Windows Defender 有可能会使得下载变慢或阻止安装
+ 第三个警告(WARN)提示: `Windows` 中的 `NTFS` 中默认不允许大于 260 个字符(byte)的文件全路径存在的限制还未解除. (可能需要添加 `sudo` 才能运行给出的命令)
+ 最后一个错误提示(ERROR): 需要安装 `dark` 才能解压使用 `WiX` Toolset 创建的安装包.

`Scoop` 的设计与实现理念 :

+ 分离用户数据: 默认将程序的 `用户数据` 存储到 `persist` 目录中, 这
样当用户日后升级该程序后, 之前的用户配置依然可用. (但是对于部分程序支持的不是很完善)

+ `shim` 软链接:  `scoop` 会自动在 `scoop` 应用安装路径下的 `shims` 文件夹下, 为新安装的程序添加对应的 `.exe` 文件.
而 `shims` 文件夹提前就已被添加到 `PATH` 环境变量中, 所以程序一旦安装就可以直接在命令行中运行.

+ 对于 GUI 程序 , `scoop` 还会自动为其在开始菜单中添加快捷方式 , 路径:

        C:\Users\<user>\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Scoop Apps

## `Scoop` 使用方法

从上面的命令中,我们可以发现 `Scoop` 命令的语法是`scoop + 动作 + 对象`的语法.其中`对象`是可省略的.
最常用的几个基础动作有这些:

```bash
scoop help #查看帮助
scoop help <某个命令> # 具体查看某个命令的帮助
scoop home <app> # 打开软件主页
scoop info  <app> # 查看软件详情

scoop install <app>   # 安装 APP
scoop uninstall <app>  # 卸载 APP

scoop list  # 列出已安装的 APP
scoop search # 搜索 APP
scoop status # 检查哪些软件有更新

scoop update # 更新 Scoop 自身
scoop update <app1> <app2> # 更新某些app
scoop update *  # 更新所有 app (前提是需要在apps目录下操作)

scoop bucket known #通过此命令列出已知所有 bucket(软件源)
scoop bucket add bucketName #添加某个 bucket

scoop cache rm <app> # 移除某个app的缓存
```

举几个栗子,比如:

我们想要搜索下有没有 `Firefox` 浏览器: `scoop search firefox`
我们想要安装 `aria2` 下载器: `scoop install aria2 `

那么现在安装软件的流程就变成了: `scoop search 软件名` -> ` scoop install 搜索结果中符合条件的那个`, 结束.
更多的进阶命令和使用方法可以参考 Scoop Wiki.

>`Scoop` 把软件安装在哪儿?
>这就是 `Scoop` 设计最为精致的地方所在了, 也是我推荐 `Scoop` 超过 Chocolatey 等更知名的 Windows 软件包管理器的原因.
>`Scoop` 和 `Homebrew` 对软件包安装位置有着相同的处理哲学: 下载, 安装在用户文件夹下.
>
>具体来讲:
>`Scoop` 在你的用户根目录(一般是 `C:\Users\用户名`)下创建了名为 `scoop` 的文件夹, 并默认将软件下载安装到此处
>`Scoop` 将软件安装到相对隔离的环境下(Each program you install is isolated and independent), 从而保证环境的统一和路径不被污染
>可以看到, `scoop` 文件夹下的 `apps` 存放有安装的所有应用.
>值得一提的是: `scoop` 是通过 `shim` 来软链接一些应用,这样的设计让应用之间不会互相干扰,十分方便.

### 安装卸载软件

```powershell
# 安装之前, 通过 search 搜索 APP, 确定软件名称
scoop search  xxx

# 安装 APP
scoop install <app>

# 安装特定版本的 APP; 语法 AppName@[version], 示例
scoop install git@2.23.0.windows.1

# 卸载 APP
scoop uninstall <app> #卸载 APP
```

### 更新软件

```powershell
scoop update # 更新 Scoop 自身

scoop update appName1 appName2 # 更新某些app

# 更新所有 app (可能需要在apps目录下操作)
scoop update *

# 禁止某程序更新
scoop hold <app>
# 允许某程序更新
scoop unhold <app>
```

### 清除缓存与旧版本

```powershell
# 查看所有已下载的缓存信息
scoop cache show

# 清除指定程序的下载缓存
scoop cache rm <app>

# 清除所有缓存
scoop cache rm *

# 删除某软件的旧版本
scoop cleanup <app>

# 删除全局安装的某软件的旧版本
scoop cleanup <app> -g

# 删除过期的下载缓存
scoop cleanup <app> -k
```

### 别名

```powershell
# 可用操作
scoop alias add|list|rm [<args>]

## 添加别名, 格式:
scoop alias add <name> <command> <description>

# 示例: (注意: 必须在 Powershell中运行)
scoop alias add st 'scoop status' '检查更新'
# 检查已添加的别名
scoop alias list -v

Name Command      Summary
---- -------      -------
st   scoop status 检查更新
# 测试已添加的别名 st
scoop st

# 另一个示例:
scoop alias add rm 'scoop uninstall $args[0]' '卸载某 app'
```

### 管理软件版本

```powershell
scoop reset [app]@[version]
```

例如:

```powershell
scoop reset idea-ultimate-eap@201.6668.13

scoop reset idea-ultimate-eap@201.6073.9

# 切换到最新版本
scoop reset idea-ultimate-eap
```

对应版本的程序需要已经安装于本地系统中;
所以在你清除某个软件的旧版本时, 可能需要考虑自己是否还会再次使用到此旧版本.

### 其他命令

```powershell
# 显示某个app的信息
scoop info <app>

# 在浏览器中打开某app的主页
scoop home <app>

# 比如
scoop home git
```

### 添加软件源 Bucket

`Scoop` 可安装的软件信息存储在 `Bucket`(翻译为: 桶)中, 也可以称其为软件源.
`Scoop` 默认的 `Bucket` 为 `main` ; 官方维护的另一个 `Bucket` 为 `extras`,
我们需要手动添加:

```powershell
# bucket的用法
scoop bucket add|list|known|rm [<args>]
```

添加 `extras` :

```powershell
scoop bucket add extras
```

我们也可以添加第三方 `bucket` , 示例:

```powershell
scoop bucket add dorado https://github.com/h404bi/dorado
```

并且明确指定安装此 `bucket` (软件源)中的的程序:

```powershell
scoop install dorado/<app_name>
# 下面是dorado中特有的软件, 测试其是否添加成功
scoop search trash
```

### 推荐的 `Bucket`(软件源):

+ `extras`: `Scoop` 官方维护的仓库, 涵盖了大部分因为种种原因不能被收录进主仓库的常用软件(在我看来是必须要添加的). 地址 : [lukesampson/scoop-extras](https://github.com/ScoopInstaller/Extras/tree/master/bucket)
+ `nirsoft`: 是 `NirSoft` 开发的小工具的安装合集. `NirSoft` 制作了大量的小工具, 包括系统工具, 网络工具, 密码恢复等等, 孜孜不倦, 持续更新.
    + `Bucket` 地址 : [kodybrown/scoop-nirsoft](https://github.com/kodybrown/scoop-nirsoft)
    + `NirSoft` 官网地址: [NirSoft](https://www.nirsoft.net/)
+ `dorado`(添加了一些国内的app, 比如 qqplayer)[h404bi/dorado](https://github.com/chawyehsu/dorado)
+ `ash258`: [Ash258/scoop-Ash258](https://github.com/Ash258/scoop-Ash258)
+ `java`: 添加后可以通过它安装各种 `jdk` , `jre`
+ `nerd-fonts` : 包含各种字体

```powershell
# 先添加bucket
scoop bucket add extras
scoop bucket add nirsoft
scoop bucket add dorado https://github.com/h404bi/dorado
scoop bucket add Ash258 'https://github.com/Ash258/Scoop-Ash258.git'
scoop bucket add nerd-fonts
# 对于开发人员, 可添加下面的两个
scoop bucket add java
scoop bucket add versions
```

## bucket 更新时遇到问题

情况是这样: 当我运行 `scoop update` 进行更新时提示:  extras bucket 更新失败. 于是我将其删除后再添加, 提示成功, 但是它却把 main bucket (默认的 bucket) 给删除了. 通过"scoop status"检查状态时出现 "These app manifests have been removed"并且下面列出了已被移除的软件名单.  那么如何解决此问题?

我们需要重新添加 main bucket :

```powershell
scoop bucket add main #添加 main bucket
```

## 网络问题导致app安装失败

示例:

```powershell
scoop install mediainfo
```

当安装 `mediainfo` 时由于网络问题, 安装包无法下载, 从命令行输出信息中可以看到如下 内容

```log
ERROR Download failed! (Error 1) An unknown error occurred
ERROR https://mediaarea.net/download/binary/mediainfo/19.09/MediaInfo_CLI_19.09_Windows_x64.zip
    referer=https://mediaarea.net/download/binary/mediainfo/19.09/
    dir=D:\Scoop\Applications\cache
    out=mediainfo#19.09#https_mediaarea.net_download_binary_mediainfo_19.09_MediaInfo_CLI_19.09_Windows_x64.zip

ERROR & 'D:\Scoop\Applications\apps\aria2\current\aria2c.exe' --input-file='D:\Scoop\Applications\cache\mediainfo.txt'
```

我们可以发现文件的 `下载路径` 和下载后的`文件名称`,
这里 `out=` 后面的 `压缩包` 就是下载后文件的名称,
也可以在 `scoop` 的 `cache` 目录下的 `mediainfo.txt` 文件中找到下载路径与文件名称

然后我们可以尝试在浏览器或其他下载程序中(可以 `fq` 的程序中)下载该程序,
下载完成后再更改文件名并将其放入 `scoop` 的 `cache` 目录,
最后再次运行 `scoop install mediainfo` 即可安装.

## 如何利用 aria2 进行断点续传?

先看具体示例:

```powershell
# 更新 vscode
scoop update vscode-portable
```

`scoop` 更新 `vscode` 时下载到 `80%` 的时候 失败了(安装时处理方法也一样).
我们需要在提示中找到如下内容:

```log
'D:\Scoop\Applications\apps\aria2\current\aria2c.exe' --input-file='D:\Scoop\Applications\cache\vscode-portable.txt'
--user-agent='Scoop/1.0 (+http://scoop.sh/) PowerShell/5.1 (Windows NT 10.0; Win64; x64; Desktop)'
--allow-overwrite=true --auto-file-renaming=false --retry-wait=2
--split=5 --max-connection-per-server=5 --min-split-size=5M
--console-log-level=warn --enable-color=false --no-conf=true
--follow-metalink=true --metalink-preferred-protocol=https
--min-tls-version=TLSv1.2 --stop-with-process=15584 --continue
```

我们从上面的信息中提取出下面的命令; 若要进行断点续传, 只需再次执行下面的命令即可:

```powershell
aria2c.exe --input-file='D:\Scoop\Applications\cache\vscode-portable.txt'
```

当提示下载完成后, 我们需要再次运行 `scoop` 对应的 `app` 更新命令(或 `安装` 命令),
即可完成 `app` 更新(或安装):

```powershell
scoop update vscode-portable
```

## Felix Note

这里记录了 `Github` 上各种 `bucket`,
[rasa/scoop-directory](https://github.com/rasa/scoop-directory)
A directory of buckets for the scoop package manager for Windows,
相当于 Scoop 的第三方软件源.

### 直接官网下载安装

+ Firefox: 使用 scoop 下载的话会出现无法更改语言和添加插件;
+ Chrome 浏览器: 直接在官网下载(是一个安装器), 通过安装器安装的 Chrome 在之后 更新时无需翻墙
+ VSCode : 如果在 scoop 中安装 VS code 便携版会导致版本升级后, 某些插件的配置文 件路径可能会出现问题, 建议直接安装.
+ Rime 输入法: 輸入法是一種有較高權限的系統軟件

### 通过压缩包安装

+ ZoomIt: 用作教鞭. 通过 Scoop 无法安装它.
ZoomIt 是由微软工作人员开发的 [Sysinternals Utilities][] 系列中的一个,
我们也可以选择通过 `scoop` 安装其所有工具

### 通过 `Scoop` 安装 :

+ 各种可在命令行使用的开发相关的程序, 比如 `git`, `jdk`
+ `geekuninstaller`: 著名的卸载工具, 能够完全清理卸载残留
+ `qbittorrent`: BT 下载软件
+ `uGet`: 简洁无广告的下载工具(`Linux` 上可用)
+ `ImageMagick` 也可以安装其替代品 `GraphicsMagick`
+ `curl` 和 `grep`:  如果你安装了 `git` 则无需单独安装它们

[Sysinternals Utilities]: https://docs.microsoft.com/en-us/sysinternals/downloads/

## 国内镜像

[scoop 国内镜像](https://gitee.com/squallliu)
[win10 安装scoop的正确姿势](https://impressionyang.oschina.io/2021/02/15/win10-install-scoop/)
[给 Scoop 加上这些软件仓库](https://sspai.com/post/52710)

## 导出 Scoop 软件列表

[全平台的备份指南](https://sspai.com/post/56272)

备份 `Scoop` 的方式为:

```bash
scoop export > scoop.txt
```

可以对 `Scoop` 的导出列表进行额外处理, 以方便后续安装. 使用 VSCode 打开 `scoop.txt` 文件, 以正则表达式搜索:

```regex
(.*?) .*
```

并全部替换成:

```regex
$1
```

注意正则式中包含 `空格`, 请完整复制.
