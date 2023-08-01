# windows rundll32.exe

[WIN 10进入休眠, 睡眠, 关机的命令](https://blog.csdn.net/qq_18671415/article/details/107866820)
[cmd shutdown命令: 关机, 重启, 休眠](https://blog.csdn.net/qq_21808961/article/details/81180732)

## 进入睡眠状态

简单地说, 执行如下命令即可让 Windows 10 进入睡眠状态:

```cmd
rundll32.exe powrprof.dll,SetSuspendState 0,1,0
```

请大家注意: 如果你的 Windows 10 激活了休眠功能的话, 执行上述命令会直接进入休眠而非睡眠模式.

此时, 你可以使用如下命令来将休眠功能临时关闭掉, 而最后一行表示在系统被唤醒后重新启用休眠功能.

```cmd
powercfg -h off
rundll32.exe powrprof.dll,SetSuspendState 0,1,0
powercfg -h on
```

## SetSuspendState

[SetSuspendState 函数(powrprof.h)](https://learn.microsoft.com/en-us/windows/win32/api/powrprof/nf-powrprof-setsuspendstate)

通过关闭电源暂停(Suspends )系统.
根据 Hibernate 参数, 系统将进入 `挂起`(睡眠)状态或 `休眠`(S4)状态.

### 语法

```cpp
BOOLEAN SetSuspendState(
  [in] BOOLEAN bHibernate,
  [in] BOOLEAN bForce,
  [in] BOOLEAN bWakeupEventsDisabled
);
```

### 参数

`[in] bHibernate`;
如果该参数为 `TRUE`, 系统将 hibernates. 如果该参数为 `FALSE`, 则系统 suspended.

`[in] bForce`; 此参数无效.

`[in] bWakeupEventsDisabled`
如果该参数为 `TRUE`, 系统将禁用所有 唤醒事件.
如果该参数为 `FALSE`, 启用 任何系统唤醒事件.

### 返回值

如果函数执行成功, 返回值为非零.
如果函数执行失败, 返回值为零. 要获取扩展错误信息, 请调用 [GetLastError](https://learn.microsoft.com/en-us/windows/desktop/api/errhandlingapi/nf-errhandlingapi-getlasterror).

### 备注

调用进程必须拥有 `SE_SHUTDOWN_NAME` 权限.
要启用 `SE_SHUTDOWN_NAME` 权限, 请使用 AdjustTokenPrivileges 函数.
更多信息, 请参阅 [更改令牌中的权限](https://learn.microsoft.com/en-us/windows/desktop/SecBP/changing-privileges-in-a-token).

应用程序可使用 `SetSuspendState` 将系统从工作状态过渡到 standby (sleep)状态,
也可选择 hibernate (`S4`)状态. 该函数与 [SetSystemPowerState](https://learn.microsoft.com/en-us/windows/desktop/api/winbase/nf-winbase-setsystempowerstate) 函数类似.

有关使用 `PowrProf.h` 的更多信息, 请参阅 [电源方案](https://learn.microsoft.com/en-us/windows/desktop/Power/power-schemes).
有关可唤醒系统的事件的信息, 请参阅 [系统唤醒事件](https://learn.microsoft.com/en-us/windows/desktop/Power/system-wake-up-events).

### 要求

最低支持客户端 Windows XP, 仅限桌面应用程序
最低支持服务器 Windows Server 2003, 仅限桌面应用程序
目标平台 Windows
头文件 `powrprof.h`
库 `PowrProf.lib`
DLL `PowrProf.dll`

## 启用屏幕保护

cmd下输入, 可以启动屏幕保护功能, 这样要再次使用电脑的时候要输入密码.
可以在短时间离开的时候, 防止别人动你电脑.

```cmd
rundll32.exe user32.dll LockWorkStation
```

## rundll32

[揭秘rundll32中的攻防对抗](https://www.anquanke.com/post/id/263193)

rundll32.exe 可用于执行 DLL 文件, 也能调用该文件中的内部函数
它的历史差不多能追溯到 Windows 95, 几乎是所有 Windows 操作系统的必需组件, 不能轻易地被禁用

在我们日常使用操作系统的过程中,
见得最多的可能是通过 `rundll32.exe` 调用某些 DLL 文件中的特定函数这一行为:

```cmd
rundll32.exe <dllname>,<entrypoint> <optional arguments>
```

譬如, 在我们右键点击某文档, 选择特定的"打开方式",
然后会弹出个窗口供我们指定用于打开的应用程序, 实际上就相当于在后台执行了以下命令:

```cmd
C:\Windows\System32\rundll32.exe C:\Windows\System32\shell32.dll,OpenAs_RunDLL <file_path>

拿修改 hosts 文件举个例子, 通过 WIN+R 执行以下命令, 即可弹出该选择窗口:

```cmd
C:\Windows\System32\rundll32.exe C:\Windows\System32\shell32.dll,OpenAs_RunDLL C:\Windows\System32\drivers\etc\hosts
```

关于 `shell32.dll`, 比较常见的函数还有 `Control_RunDLL` 和 `Control_RunDLLAsUser`,
它们可以用于运行 `.CPL` 文件, 一般主要是控制面板中的小程序.
例如打开防火墙:

```cmd
C:\WINDOWS\System32\rundll32.exe C:\WINDOWS\System32\shell32.dll,Control_RunDLL C:\WINDOWS\System32\firewall.cpl
```

### rundll32.exe 可快速调用的命令清单及其功能含义

[rundll32](https://learn.microsoft.com/zh-cn/windows-server/administration/windows-commands/rundll32)
[List of Rundll32 Commands in Windows 10](https://www.tenforums.com/tutorials/77458-rundll32-commands-list-windows-10-a.html)

Function  Rundll32 command

关于 Windows;   Rundll32.exe shell32.dll,ShellAbout
添加网络位置向导;  Rundll32 %SystemRoot%\system32\shwebsvc.dll,AddNetPlaceRunDll
添加打印机向导; Rundll32.exe shell32.dll,SHHelpShortcuts_RunDLL AddPrinter
添加标准 TCP/IP 打印机端口向导;  Rundll32.exe tcpmonui.dll,LocalAddPortUI

控制面板 ; Rundll32.exe shell32.dll,Control_RunDLL

日期和时间 ;  Rundll32.exe shell32.dll,Control_RunDLL timedate.cpl
日期和时间 - 附加时钟选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL timedate.cpl,,1
桌面图标设置 ; Rundll32.exe shell32.dll,Control_RunDLL desk.cpl,,0
设备安装设置 ; Rundll32.exe %SystemRoot%\System32\newdev.dll,DeviceInternetSettingUi
设备管理器 ; Rundll32.exe devmgr.dll DeviceManager_Execute
显示设置 ; Rundll32.exe shell32.dll,Control_RunDLL desk.cpl

轻松访问中心 ; Rundll32.exe shell32.dll,Control_RunDLL access.cpl
环境变量 ; Rundll32.exe sysdm.cpl,EditEnvironmentVariables

文件资源管理器选项 - 常规选项卡 ; Rundll32.exe shell32.dll,Options_RunDLL 0
文件资源管理器选项 - 搜索选项卡 ; Rundll32.exe shell32.dll,Options_RunDLL 2
文件资源管理器选项 - 视图选项卡 ; Rundll32.exe shell32.dll,Options_RunDLL 7
字体文件夹 ; Rundll32.exe shell32.dll,SHHelpShortcuts_RunDLL FontsFolder
忘记密码向导 ; Rundll32.exe keymgr.dll,PRShowSaveWizardExW
游戏控制器 ; Rundll32.exe shell32.dll,Control_RunDLL joy.cpl
休眠或睡眠 ; Rundll32.exe powrprof.dll,SetSuspendState

索引选项 ; Rundll32.exe shell32.dll,Control_RunDLL srchadmin.dll
Infared ; Rundll32.exe shell32.dll,Control_RunDLL irprops.cpl
Internet Explorer - 删除所有浏览历史记录 ; Rundll32.exe InetCpl.cpl,ClearMyTracksByProcess 255
Internet Explorer - 删除所有浏览历史记录和附加组件历史记录 ; Rundll32.exe InetCpl.cpl,ClearMyTracksByProcess 4351
Internet Explorer - 删除 cookie 和网站数据 ; Rundll32.exe InetCpl.cpl,ClearMyTracksByProcess 2
Internet Explorer - 删除下载历史记录 ; Rundll32.exe InetCpl.cpl,ClearMyTracksByProcess 16384
Internet Explorer - 删除表单数据 ; Rundll32.exe InetCpl.cpl,ClearMyTracksByProcess 16
Internet Explorer - 删除历史记录 ; Rundll32.exe InetCpl.cpl,ClearMyTracksByProcess 1
Internet Explorer - 删除密码 ; Rundll32.exe InetCpl.cpl,ClearMyTracksByProcess 32
Internet Explorer - 删除 Internet 临时文件和网站文件 ; Rundll32.exe InetCpl.cpl,ClearMyTracksByProcess 8
Internet Explorer - 整理收藏夹 ; Rundll32.exe shdocvw.dll,DoOrganizeFavDlg
Internet 属性 - 常规选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL inetcpl.cpl
Internet 属性 - 安全选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL inetcpl.cpl,,1
Internet 属性 - 隐私选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL inetcpl.cpl,,2
Internet 属性 - 内容选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL inetcpl.cpl,,3
Internet 属性 - 连接选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL inetcpl.cpl,,4
Internet 属性 - 程序选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL inetcpl.cpl,,5
Internet 属性 - 高级选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL inetcpl.cpl,,6

键盘属性 ; Rundll32.exe shell32.dll,Control_RunDLL main.cpl @1
锁定电脑 ; Rundll32.exe user32.dll,LockWorkStation
映射网络驱动器向导 ; Rundll32.exe shell32.dll,SHHelpShortcuts_RunDLL Connect
鼠标左键和右键交换功能 ; Rundll32.exe user32.dll,SwapMouseButton
鼠标属性 - 按钮选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL main.cpl
鼠标属性 - 指针选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL main.cpl,,1
鼠标属性 - 指针选项选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL main.cpl,,2
鼠标属性 - 滚轮选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL main.cpl,,3
鼠标属性 - 硬件选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL main.cpl,,4
网络连接 ; Rundll32.exe shell32.dll,Control_RunDLL ncpa.cpl

ODBC 数据源管理员 ; Rundll32.exe shell32.dll,Control_RunDLL odbccp32.cpl
脱机文件(常规选项卡) ; Rundll32.exe Shell32.dll,Control_RunDLL cscui.dll,,0
脱机文件(磁盘使用情况选项卡) ; Rundll32.exe Shell32.dll,Control_RunDLL cscui.dll,,1
脱机文件(加密选项卡) ; Rundll32.exe Shell32.dll,Control_RunDLL cscui.dll,,2
脱机文件(网络选项卡) ; Rundll32.exe Shell32.dll,Control_RunDLL cscui.dll,,3
笔和触控 ; Rundll32.exe shell32.dll,Control_RunDLL tabletpc.cpl
个性化 - 背景设置 ; Rundll32.exe shell32.dll,Control_RunDLL desk.cpl,,2
电源选项 ; Rundll32.exe shell32.dll,Control_RunDLL powercfg.cpl
打印机用户界面 ; Rundll32.exe Printui.dll,PrintUIEntry /?
打印机文件夹 ; Rundll32.exe shell32.dll,SHHelpShortcuts_RunDLL PrintersFolder
空闲任务进程 ; Rundll32.exe advapi32.dll,ProcessIdleTasks
程序和功能 ; Rundll32.exe shell32.dll,Control_RunDLL appwiz.cpl,,0

区域 - 格式选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Intl.cpl,,0
区域 - 位置选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Intl.cpl,,1
区域 - 管理选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Intl.cpl,,2

安全删除硬件 ; Rundll32.exe shell32.dll,Control_RunDLL HotPlug.dll
屏幕保护设置 ; Rundll32.exe shell32.dll,Control_RunDLL desk.cpl,,1
安全和维护 ; Rundll32.exe shell32.dll,Control_RunDLL wscui.cpl
设置程序访问和计算机默认值 ; Rundll32.exe shell32.dll,Control_RunDLL appwiz.cpl,,3
设置网络向导 ; Rundll32.exe shell32.dll,Control_RunDLL NetSetup.cpl
睡眠或休眠 ; Rundll32.exe powrprof.dll,SetSuspendState
声音 - 播放选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Mmsys.cpl,,0
声音 - 录音选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Mmsys.cpl,,1
声音 - 声音选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Mmsys.cpl,,2
声音 - 通信选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Mmsys.cpl,,3
语音属性 - 文本到语音选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL %SystemRoot%\System32\Speech\SpeechUX\sapi.cpl,,1
开始设置 ; Rundll32.exe shell32.dll,Options_RunDLL 3
存储的用户名和密码 ; Rundll32.exe keymgr.dll,KRShowKeyMgr

系统属性 - 计算机名称选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Sysdm.cpl,,1
系统属性 - 硬件选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Sysdm.cpl,,2
系统属性 - 高级选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Sysdm.cpl,,3
系统属性 - 系统保护选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Sysdm.cpl,,4
系统属性 - 远程选项卡 ; Rundll32.exe shell32.dll,Control_RunDLL Sysdm.cpl,,5

任务栏设置 ; Rundll32.exe shell32.dll,Options_RunDLL 1
文本服务和输入语言 ; Rundll32.exe Shell32.dll,Control_RunDLL input.dll,,{C07337D3-DB2C-4D0B-9A93-B722A6C106E2}
用户帐户 ; Rundll32.exe shell32.dll,Control_RunDLL nusrmgr.cpl

Windows 功能 ; Rundll32.exe shell32.dll,Control_RunDLL appwiz.cpl,,2
Windows 防火墙 ; Rundll32.exe shell32.dll,Control_RunDLL firewall.cpl
Windows To Go 启动选项 ; Rundll32.exe pwlauncher.dll,ShowPortableWorkspaceLauncherConfigurationUX

## pstools

[PsShutdown v2.6](https://learn.microsoft.com/zh-cn/sysinternals/downloads/psshutdown)

### 简介

`PsShutdown` 是一种命令行实用程序, 与 Windows 2000 资源包中的关机实用程序类似, 但功能更多.
除了支持关闭或重启本地或远程计算机的相同选项外,
`PsShutdown` 还可以注销控制台用户或锁定控制台(锁定需要 Windows 2000 或更高版本). PsShutdown 无需手动安装客户端软件.

[PSTools下载地址](https://download.sysinternals.com/files/PSTools.zip)

### 安装

只需将 PsShutdown 复制到可执行路径上, 然后输入 psshutdown 和下面定义的命令行选项即可.

### 使用 PsShutdown

请参阅 Windows IT Pro Magazine 2005 年 2 月刊上的 [Mark文章](https://www.windowsitpro.com/article/articleid/44973/44973.html),
其中介绍了 PsKill 的高级用法.

您可以使用 PsShutdown 启动本地或远程计算机的关机, 注销用户, 锁定系统或中止即将进行的关机.

使用方法:

```cmd
psshutdown [[\\computer[,computer[,..] | @file [-u user [-p psswd]]] -s|-r|-h|-d|-k|-a|-l|-o|-x [-f] [-c] [-t nn|h:m] [-n s] [-v nn] [-e [u|p]:xx:yy] [-m "message"]
```

参数 说明

+ `-` 显示支持的选项.
`computer` 在指定的一台或多台远程计算机上执行命令.
如果省略计算机名称, 命令将在本地系统上运行;
如果指定通配符 (`\\*`) , 命令将在当前域中的所有计算机上运行.

+ `@file` 在指定文本文件中列出的每台计算机上运行命令.
+ `-u` 指定用于登录远程计算机的可选用户名.
+ `-p` 为用户名指定可选密码. 如果省略此项, 系统将提示输入隐藏密码.
+ `-a` 终止关机(仅在倒计时进行时可行).
+ `-c` 允许交互式用户终止关机.
+ `-d` 暂停计算机.
+ `-e` 关闭原因代码.
    指定 `u` 表示用户原因代码, `p` 表示计划关机原因代码.
    `xx` 是主要原因代码(必须小于 256).
    `yy` 是次要原因代码(必须小于 65536).

+ `-f`  在关机过程中强制退出所有正在运行的应用程序, 而不是让它们有机会从容保存数据.
+ `-h`  休眠计算机.
+ `-k`  关闭计算机电源(如果不支持关闭电源, 则重新启动).
+ `-l`  锁定计算机.
+ `-m`  该选项可让您指定在关机倒计时开始时向已登录用户显示的信息.
+ `-n`  以秒为单位指定连接远程计算机的超时时间.
+ `-o`  注销控制台用户.

+ `-r`  关机后重新启动.
+ `-s`  关机时不关闭电源.
+ `-t`  以秒为单位指定关机前的倒计时(默认值: 20 秒)或关机时间(以 24 小时为单位).
+ `-x`  关闭显示器(如果支持, 系统将启动现代待机模式)
+ `-v`  在关机前显示指定秒数的信息.
如果省略此参数, 则会显示关机通知对话框, 如果指定的值为 `0`, 则不会显示对话框.
