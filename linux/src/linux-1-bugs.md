# linux 异常处理

## snap 安装软件

[Ubuntu使用snap安装常用软件](https://www.jianshu.com/p/4049b97151a1)

什么是`snap`, `snap`是一种全新的软件包管理方式, 它类似一个容器拥有一个应用程序所有的文件和库, 各个应用程序之间完全独立.
所以使用`snap`包的好处就是它解决了应用程序之间的依赖问题, 使应用程序之间更容易管理. 但是由此带来的问题就是它占用更多的磁盘空间.

`Snap`的安装包扩展名是`.snap`, 类似于一个容器, 它包含一个应用程序需要用到的所有文件和库(`snap`包包含一个私有的`root`文件系统, 里面包含了依赖的软件包).
它们会被安装到单独的目录; 各个应用程序之间相互隔离. 使用`snap`有很多好处, 首先它解决了软件包的依赖问题; 其次, 也使应用程序更容易管理.

现在支持`snap`的应用并不多, `snap`软件包一般安装在`/snap`目录下.

## 开机报错, ubuntu /var/crash

[System program problem detected?](https://askubuntu.com/questions/1160113/system-program-problem-detected)

查看转储到您的磁盘上的崩溃报告.
目录是`/var/crash/`, 它将包含几个文件, 这些文件将您指向它所涉及的软件包以及崩溃的原因.
该目录描述为:

>`/var/crash`: 系统崩溃转储(可选)
>该目录包含系统故障转储.
>自本标准发布之日起, Linux不支持系统故障转储, 但其他可能符合FHS的系统也可能支持系统转储.

`Ubuntu`版本使用此(可选)目录来转储崩溃和执行崩溃的软件包, 称为`apport` (and `whoopsie`).
如果您想获得关于崩溃的真正详细的报告, 请安装`GDB`: `The GNU Project Debugger` with `sudo apt-get install gdb`.

+ 如何摆脱它

取决于您所说的`摆脱`. 理想的解决方法是检查报告中包含的内容, 然后尝试找到解决方法.
如果不需要包装或良性包装, 也可以将其清除. 多数情况下, 它是一项核心功能.

您可以选择以下任意一种来删除崩溃报告,
直到实际删除该软件包为止(如果错误来自于`apport`本身, 那将非常具有讽刺意味):

+ `sudo rm /var/crash/*`将删除旧的崩溃并停止通知您, 直到某些软件包再次崩溃为止.
+ 您可以通过`sudo systemctl disable apport`停止服务(并通过`sudo systemctl enable apport`再次启用它)
+ 如果不想看到崩溃报告, 可以通过`vim /etc/default/apport`将其禁用. 并将`enabled = 1`更改为` enabled = 0`. 反向编辑将再次启用它.
+ 您可以使用`sudo apt purge apport`(使用`sudo apt install apport`再次安装)
+还有一种桌面方法(`问题报告`选项):

[如何阅读和使用崩溃报告](https://askubuntu.com/questions/346953/how-to-read-and-use-crash-reports)有一些有趣的答案.
它有一个示例崩溃报告和一种跟踪崩溃的方法.

## linux windows 双系统时间差异, UTC, 时区设置

[微软Windows中的UTC](https://wiki.archlinux.org/title/System_time#UTC_in_Microsoft_Windows)

要与 Windows 双启动, 建议将 Windows 配置为使用UTC,
而不是将 Linux 配置为使用本地时间.
Windows 默认使用 [本地时间](https://devblogs.microsoft.com/oldnewthing/20040902-00/?p=37983).

这可以通过一个简单的注册表修复来完成.
打开 `regedit`, 在注册表的以下位置, 添加一个十六进制值为 `1` 的 `DWORD` 值.

```reg
HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\TimeZoneInformation\RealTimeIsUniversal
```

等价于用 管理员命令行 运行以下命令:

```cmd
reg add "HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\TimeZoneInformation" /v RealTimeIsUniversal /d 1 /t REG_DWORD /f
```

或者创建具有以下内容的 `*.reg` 文件(在桌面上),
然后双击它, 将其导入注册表中.

```reg
Windows Registry Editor Version 5.00

[HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\TimeZoneInformation]
"RealTimeIsUniversal"=dword:00000001
```

如果Windows由于夏令时的变化而要求更新时钟, 让它来吧.
它将使时钟保持在预期的 `UTC状态`, 只是纠正显示的时间.

设置此值后,  [硬件时钟][] 和 [系统时钟][] 时间可能需要更新.

如果你在时间偏移方面有问题, 可以尝试重新安装 `tzdata`, 然后重新设置你的时区.

```bash
timedatectl set-timezone America/Los_Angeles
```

[硬件时钟]: https://wiki.archlinux.org/title/System_time#Hardware_clock
[系统时钟]: https://wiki.archlinux.org/title/System_time#System_clock

## grub 开机不现实, 按键盘有概率激活

[GRUB is installed but the menu is not shown at boot][def]

GRUB 已安装但启动时未显示 菜单(grub menu)

+ 检查 `/etc/default/grub` 文件中 `GRUB_TIMEOUT` 是否设置为 `0`, 
如果是, 请将其设置为正数: 该参数指定在加载默认 GRUB 条目前等待的秒数. 
+ 同时检查 `GRUB_TIMEOUT_STYLE` 是否设置为 `hidden`, 并将其改为 `menu`, 以便默认显示菜单. 

然后重新生成主配置文件并重启系统以验证是否生效.

如果问题仍未解决, 可能是图形终端存在兼容性问题. 
在 `/etc/default/grub` 中将 `GRUB_TERMINAL_OUTPUT` 设置为 `console` 以禁用 `GRUB` 图形终端.

[def]: https://wiki.archlinux.org/title/GRUB