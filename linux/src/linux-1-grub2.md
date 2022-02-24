# Linux Grub2

[grub2详解(翻译和整理官方手册)](https://www.cnblogs.com/f-ck-need-u/archive/2017/06/29/7094693.html#auto_id_37)
[官方手册原文](https://www.gnu.org/software/grub/manual/html_node/Simple-configuration.html#Simple-configuration)

`grub2-mkconfig`是根据`/etc/default/grub`文件来创建配置文件的.该文件中定义的是`grub`的全局宏,修改内置的宏可以快速生成`grub`配置文件.
实际上在`/etc/grub.d/`目录下还有一些`grub`配置脚本,这些`shell`脚本读取一些脚本配置文件(如`/etc/default/grub`),根据指定的逻辑生成`grub`配置文件.
若有兴趣,不放读一读`/etc/grub.d/10_linux`文件,它指导了创建`grub.cfg`的细节,例如如何生成启动菜单.

```bash
[root@xuexi ~]# ls /etc/grub.d/
00_header  00_tuned  01_users  10_linux  20_linux_xen  20_ppc_terminfo  30_os-prober  40_custom  41_custom  README
```

在`/etc/default/grub`中,使用`key=vaule`的格式,`key`全部为大小字母,如果`vaule`部分包含了空格或其他特殊字符,则需要使用引号包围.

例如,下面是一个`/etc/default/grub`文件的示例:

```bash
[root@xuexi ~]# cat /etc/default/grub
GRUB_TIMEOUT=5
GRUB_DISTRIBUTOR="$(sed 's, release .*$,,g' /etc/system-release)"
GRUB_DEFAULT=saved
GRUB_DISABLE_SUBMENU=true
GRUB_TERMINAL_OUTPUT="console"
GRUB_CMDLINE_LINUX="crashkernel=auto biosdevname=0 net.ifnames=0 rhgb quiet"
GRUB_DISABLE_RECOVERY="true"
```

虽然可用的宏较多,但可能用的上的就几个:
`GRUB_DEFAULT`, `GRUB_TIMEOUT`, `GRUB_CMDLINE_LINUX`和`GRUB_CMDLINE_LINUX_DEFAULT`.

以下列出了部分key.

***
`GRUB_DEFAULT`

默认的菜单项,默认值为`0`.其值可为数值`N`,表示从`0`开始计算的第`N`项是默认菜单,也可以指定对应的`title`表示该项为默认的菜单项.
使用数值比较好,因为使用的`title`可能包含了容易改变的设备名.例如有如下菜单项

```grub
menuentry 'Example GNU/Linux distribution' --class gnu-linux --id example-gnu-linux {
    ...
}
```

如果想将此菜单设为默认菜单,则可设置`GRUB_DEFAULT=example-gnu-linux`.

如果`GRUB_DEFAULT`的值设置为`saved`,则表示默认的菜单项是`GRUB_SAVEDEFAULT`或`grub-set-default`所指定的菜单项.

### UEFI 系统下的安装

[GRand Unified Bootloader](https://wiki.archlinux.org/index.php/GRUB_(简体中文))

安装

本节假设您正在 `x86_64` 系统上安装 `GRUB`. 对于 `IA32` (32 位) `UEFI `系统(不要和 32 位 CPU 相混淆),  将`x86_64-efi`替换成`i386-efi`.
首先安装软件包 `grub` 和 `efibootmgr`. 其中`GRUB`是启动引导器, `efibootmgr`被 `GRUB` 脚本用来将启动项写入 `NVRAM` .

然后按照下列步骤安装 `GRUB`:

挂载 `EFI` 系统分区, 在本节之后的内容里, 把 `esp` 替换成挂载点.
选择一个启动引导器标识, 这里叫做 `new`. 这将在 `esp/EFI/` 中创建一个`new`目录来储存 `EFI` 二进制文件, 而且这个名字还会在 `UEFI` 启动菜单中表示 `new` 启动项.
执行下面的命令来将 `GRUB EFI` 应用 `grubx64.efi` 安装到 `esp/EFI/new/`, 并将其模块安装到 `/boot/grub/x86_64-efi/`.

```
# grub-install --target=x86_64-efi --efi-directory=esp --bootloader-id=new
```

上述安装完成后 GRUB 的主目录将位于 `/boot/grub/`. 注意上述例子中, grub-install 还将在固件启动管理器中创建一个条目, 名叫 `new`.

在配置完成后, 记得生成主配置文件.

提示:  如果你使用了 `--removable` 选项, 那 `GRUB` 将被安装到 `esp/EFI/BOOT/BOOTX64.EFI` (当使用 `i386-efi` 时是 e`sp/EFI/BOOT/BOOTIA32.EFI`),
此时即使 `EFI` 变量被重设或者你把这个驱动器接到其他电脑上, 你仍可从这个驱动器上启动.
通常来说, 你只要像操作 `BIOS` 设备一样在启动时选择这个驱动器就可以了.
如果和 `Windows` 一起多系统启动, 注意 `Windows` 通常会在那里安装一个 `EFI` 可执行程序, 这只是为了重建 `Windows` 的 UEFI 启动项.

注意:
`--efi-directory` 和 `--bootloader-id` 是 `GRUB UEFI` 特有的. -`-efi-directory` 替代了已经废弃的 `--root-directory`.
您可能注意到在 `grub-install` 命令中没有一个 `<device_path>` 选项, 例如 `/dev/sda`. 事实上即使提供了 `<device_path>`, 也会被 `GRUB` 安装脚本忽略, 因为 `UEFI` 启动加载器不使用 MBR 启动代码或启动扇区.
确保 `grub-install` 命令是在你想要用 GRUB 引导的那个系统上运行的. 也就是说如果你是用安装介质启动进入了安装环境中, 你需要在 `chroot` 之后再运行 `grub-install`.
如果因为某些原因不得不在安装的系统之外运行 `grub-install`, 在后面加上 `--boot-directory= `选项来指定挂载 `/boot` 目录的路径, 例如 `--boot-directory=/mnt/boot`.

### grub 修复

[Linux与Windows 10用grub引导教程](https://www.cnblogs.com/jpfss/p/9462792.html)

首先, 在你装了 Windows 之后, Windows 在装机过程中会将硬盘划分出一个约 `100MB` 大小的分区, 称为 `EFI` 分区.
这个分区就是起引导作用的. 在资源管理器中是看不到的这个分区的, 可以在磁盘管理中看到, 管理则需要借助 `DiskGenius` 工具.
可以看到该分区包含 3 个文件夹(如果你没有装 `Linux` 的话, 就只有两个), 分别是 `Boot`, `Microsoft` 和 `Manjaro`, 其中 `Boot` 文件夹就是 `UEFI` 引导所必需的文件.

我们继续打开 `Microsoft/Boot` 文件夹, 将会看到许多文件, 这些文件就是启动 `Windows 10` 所必需的, 包含了语言包, 字体等, `BCD` 包含了 `Windows` 引导开始以后的信息.
其中, `bootmgfw.efi `是 `Windows` 默认引导文件.

```bash
EFI/Boot/bootx64.efi
EFI/Microsoft/Boot/bootmgfw.efi
```

以上是采用 `UEFI` 启动 `Windows 10` 的文件结构, 也就是说, 当你按下开机按钮的时候, 首先 `UEFI` 找到 `EFI` 分区的 `Boot` 文件夹, 然后加载 `bootx64.efi` 文件, 读取文件信息, 找到 `EFI/Microsoft/Boot/bootmgfw.efi`, 按照 `bootmgfw.efi` 的要求, 加载所需的启动信息, 启动 `Windows 10`.

***
准备工作

在正式装系统之前, 我们还需要做一些准备工作:

关闭 `Windows` 的快速启动
这个功能的作用是在于关机的时候不完全断电, 类似将系统处于`休眠`状态, 这样可以让开机更加迅速. 但这也就导致了只能使用 `Windows` 系统.

关闭 `BIOS` 的 `Secure Boot `的功能

在默认情况下, `UEFI` 固件只会加载那些被签名的引导程序. 在缺少 `Secure Boot` 功能的传统 PC 机上, 恶意的后门程序可以加载自身, 进而摇身一变伪装成一个引导程序.
这样的话, `BIOS` 就会在启动的时候加载后门程序, 这样它就可以躲过操作系统, 把自己隐藏得很深.
但是不得不说, 这对我们安装 `Linux` 造成了很大的困扰, 也是直接导致我们重启到 Windows 10 后进不去 `Linux` 的原因.
首先我们要关闭这个功能: 进入 `BIOS` 找到 `Secure Boot`, 选择 `disabled`, 这样就关闭了. 当然, 有些人进入 `BIOS` 会发现 Secure Boot 这个选项是灰色的(比如我的就是), 这时你需要先给你的 `BIOS` 设一个密码, 然后就能关 Secure Boot 了.

***
安装 Linux

所有的准备都已经完成, 这时就可以准备刻录 U 盘了, 推荐`Rufus`和`USBWriter`.
刻录完成后, 重启按 `f12`, 选择从 USB 设备启动, 对于绝大多数发行版来说一路回车就行了, 只需要注意一点: 在选择挂载 `boot` 位置的时候, 一定要挂载在 `efi` 分区, 别的都不行.
重启之后, 不出意外的话, 你会直接进入 Windows 10, 不要担心, 这时 Linux 已经安装成功了, 我们只需要将引导文件替换一下.

***
替换引导文件

先用 `DG` 打开 `EFI` 分区, 你会看到多了一个文件夹, 名称取决于你安装的是哪一个发行版.
我安装的是 `Manjaro Linux`, 名称就是 `Manjaro`, 打开之后会发现里面有一个名为 `grubx64.efi` 的文件, 这就是启动 `Linux` 的引导文件.
和 `Windows 10` 的 `bootmgfw.efi` 类似, 我们想要用 `grubx64.efi` 引导代替掉 `bootmgfw.efi`, 这样就可以用 `GRUB` 引导了. 步骤:

进入管理员命令行. 方法: `win + x`, 再按 `a`
输入 `bcdedit /set {bootmgr} path \EFI\Manjaro\grubx64.efi`. 提示操作成功的话, 就完成了.

注: 如果输入以上命令提示`参数错误`的话, 将`{bootmgr}`改为 `'{bootmgr}'`, 原因是 `PowerShell` 和 `CMD` 语法的差别.

至此, 如果你安装的是除 `Arch` 之外绝大多数发行版, 那么接下来就和你没有啥关系了, 你已经成功了, 好好享受吧!

开机之后会发现进入 `GRUB` 的引导了, 通常会包含至少三个选项(以 `Manjaro` 举例): `Manjaro`, `Manjaro 高级选项`和 `Windows Manager`.
这就代表你已经完美的解决了 Windows 和 Linux 双系统引导的问题.

***
修复 `Windows` 引导

这一点是我安装 Arch Llinux 的时候发现的, Arch Linux 安装过程是手动安装的, 在编写 GRUB 的时候会扫描不到 Windows Manager 所在的分区(当然可能不是所有人都会遇到),
所以在 GRUB 界面可能会看不到 Windows Manager 选项, 导致进不去 Windows 10, 这里就需要手动编辑 GRUB 信息,
我们打开 `/boot/grub/grub.cfg` 文件, 发现里面确实没有 Windows 10 的启动信息, 在后面加上:

```bash
menuentry "Microsoft Windows 10" {
    insmod part_gpt
    insmod fat
    insmod search_fs_uuid
    insmod chain
    search --fs-uuid --set=root $hints_string $fs_uuid
    chainloader /EFI/Microsoft/Boot/bootmgfw.efi
    }
```

注意:

这里的 `$hints_string`, 代表的是终端执行命令:

```bash
sudo grub-probe --target=hints_string /boot/efi/EFI/Microsoft/Boot/bootmgfw.efi
```

后的输出;

而 `$fs_uuid` 代表的是:

```bash
sudo grub-probe --target=fs_uuid /boot/efi/EFI/Microsoft/Boot/bootmgfw.efi
```

的输出. 然后保存. 在终端执行命令: `sudo grub-mkconfig -o /boot/grub/grub.cfg`, 就 `OK` 了.

到此, Arch Linux 和 Windows 10 双系统也配置完毕了.

***
附加问题

+ 在 Windows 10 进行了一个大更新后, 会发现 GRUB 引导界面没有了, 还是直接进入了 Windows 10, 这时只需要按照 替换引导文件 的方法重新输入一遍命令就行.
+ 使用 Linux 某个发行版一段时间之后, 难免会想尝试一下另一个发行版. 这时请务必将之前的发型版的引导文件删除, 否则可能会出现无论怎么设置都无法进入 GRUB 的情况. 例如: 我之前用的是 `Ubuntu`, 我现在换成了 Manjaro, 我就需要用 DG 删除 EFI 分区的 Ubuntu 文件夹.
+ 在我使用 `Manjaro` 更新了一次 `Linux` 的内核后, 进不去 Windows 10 了, 这个时候千万不要直接修复 `Windows 10` 引导, 这会格式化 `EFI` 分区, 只需要按上面修复 `Windows` 引导 的方法编辑一下 `GRUB` 就可以了.

***
[Arch Wiki 上的示例](https://wiki.archlinux.org/index.php/GRUB_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E5%90%AF%E5%8A%A8%E8%8F%9C%E5%8D%95%E6%9D%A1%E7%9B%AE%E7%A4%BA%E4%BE%8B)

这个模式寻找 `Windows` 的启动加载器的位置, 然后当用户选择了相应的菜单条目的时候, 通过链式载入的方法在 `GRUB` 之后加载它.
这里主要的任务是找到 EFI 系统分区然后从上面运行启动加载器. 注意:  这个启动项仅在 `UEFI` 模式下才起作用, 而且 `Windows` 和 `UEFI` 的位数必须相同.

```bash
if [ "${grub_platform}" == "efi" ]; then
    menuentry "Microsoft Windows Vista/7/8/8.1 UEFI/GPT" {
    insmod part_gpt
    insmod fat
    insmod chain
    search --no-floppy --fs-uuid --set=root $hints_string $fs_uuid
    chainloader /EFI/Microsoft/Boot/bootmgfw.efi
    }
fi
```

其中 `$hints_string` 和 `$fs_uuid` 由下述两个命令得到.

`$fs_uuid` 命令检测 EFI 系统分区的 `UUID` :

```bash
# grub-probe --target=fs_uuid esp/EFI/Microsoft/Boot/bootmgfw.efi
1ce5-7f28
```

或者你可以(以 `root` 身份)运行 `blkid` 然后从结果中找到 `EFI` 系统分区的 `UUID` .

`$hints_string` 命令可以确定 EFI 系统分区的位置, 在当前的例子中是 `harddrive 0`:

```bash
# grub-probe --target=hints_string esp/EFI/Microsoft/Boot/bootmgfw.efi
--hint-bios=hd0,gpt1 --hint-efi=hd0,gpt1 --hint-baremetal=ahci0,gpt1
```

这两个命令都是假设 `Windows` 使用的 `ESP` 是挂载在`$esp`上的. 当然,  `Windows` 的 `EFI` 文件路径可能有变,因为这就是 `Windows` ....

### 我使用的 grub 自定义

[Writing your own configuration file](https://www.gnu.org/software/grub/manual/grub/grub.html#search)

+ 编辑 `/etc/default/grub`

```grub
GRUB_DEFAULT=0 #设置默认为第一个
GRUB_TIMEOUT=10 #设置等待时间
GRUB_TIMEOUT_STYLE=menu # 强制显示启动菜单
```

+ 编辑 `/etc/grub.d/40_custom`, 添加 `windows`启动项,

```bash
if [ "${grub_platform}" == "efi" ]; then
    menuentry "Windows UEFI GPT" {
    insmod part_gpt
    insmod fat
    insmod chain
    search --no-floppy --set=root --fs-uuid "C045-C995"
    chainloader /EFI/Microsoft/Boot/bootmgfw.efi
    }
fi
```

然后运行`sudo os-prober ;sudo update-grub`

### grub 命令行手动引导

链式加载`UEFI`模式下安装的 `Windows/Linux`

```bash
insmod fat
set root=(hd1,gpt1)
chainloader /EFI/Microsoft/Boot/bootmgfw.efi
boot
```

### grub 参数

[nomodeset, quiet and splash](https://askubuntu.com/questions/716957/what-do-the-nomodeset-quiet-and-splash-kernel-parameters-mean)

+ `nomodeset` ; 最新的内核已将`video mode setting`移入内核.
So all the programming of the hardware specific clock rates and registers on the video card 发生在内核中,
而不是`X`服务器启动后的`X`驱动中. 如此, 可以在开机时展示高分辨率`splash/boot`画面, 并实现平滑过渡, 不用闪烁一下子.
然而有些显卡无法正常工作, 并且最终会出现黑屏. 添加`nomodeset`参数指示内核在加载`X`之前不加载显卡驱动程序, 而改用`BIOS`模式.

+ `quiet splash`; `splash`(由`/boot/grub/grub.cfg`设置)导致显示启动画面. 同时, 您希望启动过程安静一些, 否则所有类型的消息都会覆盖启动屏幕.
尽管在`GRUB`中指定了这些参数, 但它们是影响内核或其模块加载的内核参数, 而不是改变`GRUB`行为的参数. 来自`GRUB_CMDLINE_LINUX_DEFAULT`的重要部分是`CMDLINE_LINUX`

+ `acpi`, `noapic` and `nolapic`;通常不需要这些参数, 除非你的`BIOS`很古老, 不支持这些标准.

`ACPI`(Advanced Configuration and Power Interface)是用于处理电源管理的标准.
较早的系统可能不完全支持`ACPI`, 因此有时它有助于向内核提示不使用它. `acpi=off`

`APIC`(`Advanced Programmable Interrupt Controller`)是在较新的系统上发行的一种功能.
`"local"`版本称为`LAPIC`. 该控制器可以生成和处理中断, 中断是硬件用来传递消息的信号.
同样, `APIC`的某些实现在较旧的系统上可能会出现问题, 因此禁用它很有用.  `noapic` ,`nolapic`.

有时, `APIC`可以正常工作, 但是传递消息可能会减慢速度, 这可能会干扰音频和视频处理. 人们也可能出于这个原因禁用它.

### ACPI 伪装 windows

[Manjaro折腾记录](https://blog.csdn.net/hustcw98/article/details/81979172)
[how to set acpi_osi parameter in the grub](https://unix.stackexchange.com/questions/246672/how-to-set-acpi-osi-parameter-in-the-grub)

有时从suspend恢复时, 会发现登录后整个桌面一片黑色, 只有一个亮亮的鼠标可以移动.

```bash
vim /etc/default/grub
## 在 GRUB_CMDLINE_LINUX_DEFAUT= 后面添加下面的语句
acpi_osi=! acpi_osi='Windows 2009'
```

`acpi_osi=!`清空表示`acpi_osi`原来的值, 然后假装成`Windows 7, Win Server 2008 R2`,
具体的版本对应可以参考: [如何使用 _OSI 识别 ACPI 中的 Windows 版本](https://docs.microsoft.com/zh-cn/windows-hardware/drivers/acpi/winacpi-osi).

对于`Debian`系列的发行版, 可能需要转义引号, 写成`acpi_osi=\"Windows 2009\"`,修改后, 运行`sudo update-grub`重新生成引导配置.
可以使用`cat /proc/cmdline`检查内核启动的参数, 参考[Linux中proc/cmdline](https://blog.csdn.net/baidu_33879812/article/details/104906774)

***
`Manjaro`在更新内核到`Linux 5.11.14-1`的时候出现问题, 开机提示`NMI watchdog: Watchdog detected hard LOCKUP on cpu x`,删除这个配置之后可以正常进入.

***
20210423  测试:

+ `GRUB_CMDLINE_LINUX_DEFAULT="apparmor=1 security=apparmor resume=UUID=0f163abf-8e60-4626-a4de-54332c64db51 udev.log_priority=3"`: 不设置这个选项, 可以正常启动.
+ 加上`acpi_osi=! acpi_osi='Windows 2009'` : 可以正常启动
+ 加上`acpi_osi=! acpi_osi='Windows 2015'`  : 可以正常启动

三种参数的开机日志基本没啥区别
