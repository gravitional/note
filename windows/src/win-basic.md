# windows

## 日常

[你应该知道的 Windows 环境变量](https://zhuanlan.zhihu.com/p/67726501)
[系统变量 %USERPROFILE%](https://www.cnblogs.com/nio-nio/p/9345750.html)

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

可以用CMD的SET命令来查看现有的系统变量, "="前的部分用%括起来就是.

+ 常用:

```cmd
%USERPROFILE% =C:\Users\用户名
%SystemRoot% =C:\WINDOWS
%SystemDrive% =C:
%APPDATA% =C:\Users\用户名\AppData\Roaming
%LOCALAPPDATA% =C:\Users\用户名\AppData\Local
%windir% =C:\WINDOWS
%Path% =C:\Windows\system32;C:\Windows;
%ProgramData% =C:\ProgramData
%ProgramFiles% =C:\Program Files
%ProgramFiles(x86)% =C:\Program Files (x86)
```

+ 其他:

```cmd
%ALLUSERSPROFILE% =C:\ProgramData
%CommonProgramFiles% =C:\Program Files\Common Files
%CommonProgramFiles(x86)% =C:\Program Files (x86)\Common Files
%CommonProgramW6432% =C:\Program Files\Common Files
%COMPUTERNAME% =MyPC
%ComSpec% =C:\WINDOWS\system32\cmd.exe
%HOMEDRIVE% =C:
%HOMEPATH% =\Users\用户名
%LOGONSERVER% =\\MicrosoftAccount
%OS% =Windows_NT
%ProgramW6432% =C:\Program Files
%PUBLIC% =C:\Users\Public
%TEMP% =C:\Users\用户名\AppData\Local\Temp
%TMP% =C:\Users\用户名\AppData\Local\Temp
%USERDOMAIN% =MyPC
%USERNAME% =用户名
```

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

1, 首先你需要在桌面上新建一个txt文本文档.然后将以下代码复制到这个新建的 `txt` 文本文档中.

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

在 `计算机` 上右键--`管理`--`事件查看器`--`windows日志`--`系统` 中找到错误事件的日志,
查看详细信息(XML)视图;
可以看到相关的注册表项目的 ID,以及`processID`,在任务管理器中可以看到对应的进程名称.
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

## 安装 windows 11

[下载 Windows 11](https://www.microsoft.com/zh-cn/software-download/windows11?)

## 激活Windows

[Windows 激活——MAS](https://www.liyangjie.cn/posts/hobby/windows-activation/)

[microsoft-activation-scripts][https://gitlab.com/massgrave/microsoft-activation-scripts/-/releases]

下载后, 使用图中所示解压密码进行解压, 得到 MAS_1.5 目录. 目录结构如下:

```log
MAS_1.5
├─ All-In-One-Version
└─ Separate-Files-Version
```

其中, `All-In-One-Version` 目录中为集成脚本, 提供了所有激活方法的快捷操作入口.
而 `Separate-Files-Version` 为具体的各个激活方法的独立脚本, 用户可以根据需要自行选择执行.
剩余 `ReadMe.html` 为说明文档,
V`erify_Files-Clear_Zone.Identifier-68.cmd` 校验文件并对时区进行清理,
防止 SmartScreen 警告.

作为小白用户, 这里当然推荐直接使用集成脚本,
进入 `All-In-One-Version` 目录, 直接双击执行该目录下的执行脚本
(这里 1.5 版本为 `MAS_1.5_AIO_CRC32_21D20776.cmd`),
根据提示的选项, 输入具体的数字进行进行激活即可:

+ 1: HWID 激活(永久, 支持 Win 10-11).
+ 2: KMS38 激活(有效期至 2038年, 适用于 Win10-11-Server).
+ 3: Online KMS 激活(有效期 180 天, 支持 Win7 以上和 Office).
+ 4: 查看当前激活状态(vbs).
+ 5: 查看当前激活状态(wmi).
+ 6: 其他功能选项(如预激活镜像制作等).
+ 7: 说明文档, 详细介绍了每种激活方法支持的 Windows 产品, 使用方法等.
+ 8: 退出.

当然, 最重要的就是其中的 `1-3` 这三个选项了, 分别使用 3 种不同的方式进行激活.
对于 `Win10, 11`, 这里推荐使用 `1: HWID` 进行激活,
而对于 `Win7` 只能选择 `3: Online KMS` 进行激活,
`3: Online KMS` 同样支持 `Office` 的激活, 但不推荐使用.
对于 `Office` 这里推荐一个从安装到激活的 Office 一站式部署工具
[Office Tool Plus](https://otp.landian.vip/).

这里以 `1: HWID` 为例进行演示:

+ 首先在主界面输入 1, 即可进入 `HWID` 激活界面:
+ 在上述界面中, 再次输入 1, 开始进行 `Windows` 激活, 完成后, 提示 `Successful`.

激活成功后, 使用如下命令查看激活状态:

```cmd
slmgr.vbs -xpr
```
