# Linux Grub2

[grub2详解(翻译和整理官方手册)](https://www.cnblogs.com/f-ck-need-u/archive/2017/06/29/7094693.html#auto_id_37)
[官方手册原文](https://www.gnu.org/software/grub/manual/html_node/Simple-configuration.html#Simple-configuration)

`grub2-mkconfig` 是根据 `/etc/default/grub` 文件来创建配置文件的.
该文件中定义的是 `grub` 的全局宏, 修改内置的宏可以快速生成 `grub` 配置文件.

实际上在 `/etc/grub.d/` 目录下还有一些`grub`配置脚本,
这些 `shell` 脚本读取一些脚本 配置文件(如`/etc/default/grub`), 根据指定的逻辑生成`grub`配置文件.
若有兴趣, 不放读一读`/etc/grub.d/10_linux`文件,它指导了创建`grub.cfg`的细节, 例如如何生成启动菜单.

```bash
[root@xuexi ~]# ls /etc/grub.d/
00_header  00_tuned  01_users  10_linux  20_linux_xen  20_ppc_terminfo  30_os-prober  40_custom  41_custom  README
```

在 `/etc/default/grub`中, 使用 `键=值` 的格式, `键` 全部为大写字母,
如果 `值` 部分包含了空格或其他特殊字符, 则需要使用引号包围.

例如,下面是一个 `/etc/default/grub` 文件的示例:

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

虽然可用的宏较多, 但可能用的上的就几个:

+ `GRUB_DEFAULT`,
+ `GRUB_TIMEOUT`,
+ `GRUB_CMDLINE_LINUX`和
+ `GRUB_CMDLINE_LINUX_DEFAULT`.

以下详细解释了部分 `key`.

#### GRUB_DEFAULT

默认的菜单项, 默认值为`0`, 其值可为数值`N`, 表示从`0`开始计算的第`N`项是默认菜单,
也可以指定对应的`title`表示该项为默认的菜单项.
使用数值比较好, 因为使用的 `title` 可能包含了容易改变的设备名.
例如有如下菜单项

```grub
menuentry 'Example GNU/Linux distribution' --class gnu-linux --id example-gnu-linux {
    ...
}
```

如果想将此菜单设为默认菜单,则可设置 `GRUB_DEFAULT=example-gnu-linux`.

如果 `GRUB_DEFAULT` 的值设置为 `saved`,
则表示默认的菜单项是 `GRUB_SAVEDEFAULT` 或 `grub-set-default` 所指定的菜单项.

### UEFI 系统下的安装

[GRand Unified Bootloader](https://wiki.archlinux.org/index.php/GRUB_(简体中文))

注意:

+ 建议阅读并理解 [统一可扩展固件界面 (UEFI)][], [Partitioning (简体中文)#GUID 分区表][]
  和 [Arch boot process (简体中文)#UEFI][] 这几个页面.
+ 使用 `UEFI` 安装时, 一定要让安装介质以 `UEFI` 模式启动,
    否则 `efibootmgr` 将无法添加 `GRUB UEFI 启动项`.
    但即使在 `BIOS` 模式工作时, 安装到 [后备启动路径][] 仍然可行, 因为这一过程用不到 `NVRAM`.
+ 要从一个磁盘上使用 `UEFI` 模式启动, 磁盘上必须要先有一个 `EFI 分区`.
    按照 [EFI system partition#Check for an existing partition][] 上说的来查看是否已有 `EFI 分区`,
    如若没有, 就创建一个.

> NVRAM: Non-Volatile Random Access Memory, 是指断电后仍能保持数据的一种 `RAM`.

本节假设您正在 `x86_64` 系统上安装 `GRUB`.
对于 `IA32` (32 位) `UEFI ` 系统(不要和 32 位 `CPU` 相混淆),  将`x86_64-efi`替换成 `i386-efi`.

+ 首先安装软件包 `grub` 和 `efibootmgr`.
其中`GRUB`是启动引导器, `efibootmgr` 被 `GRUB` 脚本用来将启动项写入 `NVRAM` .

+ 确保被安装 `grub` 的系统(运行指令的系统), 正是要用 `GRUB` 引导的系统.
    也就是说, 如果你是用 `安装介质` 启动进入了安装环境中, 你需要在 `chroot` 之后再运行 `grub-install`.
+ 如果因为某些原因, 必须在 `目标系统` 之外运行 `grub-install`,
    在后面加上 `--boot-directory=` 选项来指定挂载 `/boot` 目录的路径, 例如 --boot-directory=/mnt/boot.

然后按照下列步骤安装 `GRUB`:

+ [挂载EFI系统分区][] 到 `esp` 目录, 在本节之后的内容里, 把 `esp` 替换成你的 `挂载点`.
    `挂载点` 通常是 `/efi` 或者 `/boot` 目录, 在 ubuntu 是 `/boot` 目录.
+ 选择 bootloader 标识, 这里假设为 `new`, 这将在 `esp/EFI/` 中创建目录 `new`来储存 `EFI` 二进制文件,
    而且在 `UEFI` 启动菜单中, 会有相应的 `new` 启动项.

+ 管理员模式 执行下面的命令来将 `GRUB EFI` 应用 `grubx64.efi` 安装到 `esp/EFI/new/`, 并将其 `模块` 安装到 `/boot/grub/x86_64-efi/`.

    ```bash
    # grub-install --target=x86_64-efi --efi-directory=esp --bootloader-id=new
    ```

上述安装完成后 `GRUB` 的主目录将位于 `/boot/grub/`.
注意上述例子中, `grub-install` 还将 [在固件启动管理器中创建][] 名为 `new` 的条目.

在配置完成后, 记得 [生成主配置文件][].

[生成主配置文件]: https://wiki.archlinux.org/title/GRUB_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E7%94%9F%E6%88%90%E4%B8%BB%E9%85%8D%E7%BD%AE%E6%96%87%E4%BB%B6

#### 缺省/后备启动路径

一些 `UEFI` 固件在显示 `UEFI NVRAM` 启动条目之前,
需要在一个已知的位置上有一个可启动文件.
如果是这种情况,  `grub-install` 会说明 `efibootmgr` 添加了一个启动 `GRUB` 的条目,
但这个条目不会在 `VisualBIOS` 启动顺序选择器中显示.
解决方法是把 `GRUB` 安装到缺省/后备启动路径当中:

```bash
# grub-install --target=x86_64-efi --efi-directory=esp --removable
```

或者你可以把已经安装好的 `GRUB EFI` 执行文件移动到 `缺省/后备路径`中:

```bash
# mv esp/EFI/grub esp/EFI/BOOT
# mv esp/EFI/BOOT/grubx64.efi esp/EFI/BOOT/BOOTX64.EFI
```

[Arch boot process (简体中文)#UEFI]: https://wiki.archlinux.org/title/Arch_boot_process_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#UEFI
[统一可扩展固件界面 (UEFI)]: https://wiki.archlinux.org/title/Unified_Extensible_Firmware_Interface_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)
[Partitioning (简体中文)#GUID 分区表]: https://wiki.archlinux.org/title/Partitioning_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#GUID_%E5%88%86%E5%8C%BA%E8%A1%A8
[后备启动路径]: https://wiki.archlinux.org/title/GRUB_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E7%BC%BA%E7%9C%81/%E5%90%8E%E5%A4%87%E5%90%AF%E5%8A%A8%E8%B7%AF%E5%BE%84
[EFI system partition#Check for an existing partition]: https://wiki.archlinux.org/title/EFI_system_partition#Check_for_an_existing_partition

#### 在固件启动管理器中创建 GRUB 条目

+ `grub-install` 会自动尝试在启动管理器中创建 `GRUB` 条目.
如果没有成功, 请参考 [Unified Extensible Firmware Interface (简体中文)#efibootmgr][],
里面有如何使用 `efibootmgr` 创建启动目录条目的介绍.
+ 一般来说, 这个问题都是因为你没有以 `UEFI` 模式启动 `CD/USB` 造成的.
请参考 [Unified Extensible Firmware Interface (简体中文)#从 ISO 创建 UEFI 可启动 USB][].

提示:

+ 如果你使用了 `--removable` 选项, 那 `GRUB` 将被安装到 `esp/EFI/BOOT/BOOTX64.EFI`
(当使用 `i386-efi` 时是 `esp/EFI/BOOT/BOOTIA32.EFI`),
此时即使 `EFI` 变量被重设或者你把这个驱动器接到其他电脑上, 你仍可从这个驱动器上启动.
通常来说, 你只要像操作 `BIOS` 设备一样在启动时选择这个驱动器就可以了.
+ 如果和 `Windows` 一起多系统启动, 注意 `Windows` 通常会在那里安装一个 `EFI` 可执行程序,
这只是为了重建 `Windows` 的 `UEFI` 启动项.

 注意:

+ `--efi-directory` 和 `--bootloader-id` 是 `GRUB UEFI` 特有的.
`--efi-directory` 替代了已经废弃的 `--root-directory`.
+ 您可能注意到在 `grub-install` 命令中没有 `<device_path>` 选项, 例如 `/dev/sda`.
事实上即使提供了 `<device_path>`, 也会被 `GRUB` 安装脚本忽略,
因为 `UEFI` 启动加载器不使用 `MBR` 启动代码或启动扇区.

[挂载EFI系统分区]: https://wiki.archlinux.org/title/EFI_system_partition#Mount_the_partition
[在固件启动管理器中创建]: https://wiki.archlinux.org/title/GRUB/Tips_and_tricks_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E5%9C%A8%E5%9B%BA%E4%BB%B6%E5%90%AF%E5%8A%A8%E7%AE%A1%E7%90%86%E5%99%A8%E4%B8%AD%E5%88%9B%E5%BB%BA_GRUB_%E6%9D%A1%E7%9B%AE
[Unified Extensible Firmware Interface (简体中文)#efibootmgr]: https://wiki.archlinux.org/title/Unified_Extensible_Firmware_Interface_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#efibootmgr
[Unified Extensible Firmware Interface (简体中文)#从 ISO 创建 UEFI 可启动 USB]: https://wiki.archlinux.org/title/USB_flash_installation_medium#Using_the_ISO_as_is_(BIOS_and_UEFI)

### 生成主配置文件

安装后,需要生成 `主配置文件 /boot/grub/grub.cfg`.
`配置文件` 的生成过程受到 `/etc/default/grub` 中的选项和 `/etc/grub.d/` 下脚本的影响.

如果你没有进行额外配置,
`自动生成程序` 会在当前启动的系统的 `根文件系统` 中侦测 `配置文件`.
所以请确保系统已经启动, 或者已经通过 `chroot` 进入.

>注意:
>
+ 请记住, 每当修改 `/etc/default/grub` 或者 `/etc/grub.d/` 中的文件之后, 都需要再次生成 `/boot/grub/grub.cfg`.
+ 默认的文件路径是 `/boot/grub/grub.cfg`, 而非 `/boot/grub/i386-pc/grub.cfg`.
+ 如果你是在 `chroot` 或者 `systemd-nspawn` 容器中运行 `grub-mkconfig`,
可能会报 `grub-probe` 无法获取 `"canonical path of /dev/sdaX"` 错误而无法正常执行.
此时可以尝试使用 `arch-chroot`, 参见 [BBS post](https://bbs.archlinux.org/viewtopic.php?pid=1225067#p1225067).
+ 如果你在使用了 `LVM` 的 `chroot` 环境中安装 `GRUB`, `grub-mkconfig` 会无限期挂起.
参见 [#Device /dev/xxx not initialized in udev database even after waiting 10000000 microseconds](https://wiki.archlinux.org/title/GRUB_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#Device_/dev/xxx_not_initialized_in_udev_database_even_after_waiting_10000000_microseconds).

使用 `grub-mkconfig` 工具来生成 `/boot/grub/grub.cfg`:

```bash
# grub-mkconfig -o /boot/grub/grub.cfg
```

ubuntu 上也可以用更简单的命令

```bash
sudo update-grub
```

`自动生成脚本` 默认将在生成的 `配置文件` 中为所有已安装的 `Arch Linux` [内核][] 添加一个条目.

[内核]: https://wiki.archlinux.org/title/Kernels_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)

> 提示:
>
>+ 每次安装或者移除一个内核后, 你都需要重新运行一次 `grub-mkconfig` 命令.
>+ 若要管理多个 `GRUB` 条目, 比如既使用 [linux][] 又使用 [linux-lts][] 内核,
>相关的提示可以参见 [GRUB/Tips and tricks (简体中文)#多个启动条目][].

如果想要自动为其他操作系统添加条目, 请见下文的 `探测其他操作系统`.

如果想要添加 `自定义条目`, 你可以编辑 `/etc/grub.d/40_custom` 文件,
然后重新生成 `/boot/grub/grub.cfg`.
或者你可以创建 `/boot/grub/custom.cfg` 文件然后把条目添加进这里面.

修改 `/boot/grub/custom.cfg` 文件后不用再运行` grub-mkconfig` 程序,
因为 `/etc/grub.d/41_custom` 文件已经在生成的 `主配置文件` 中,
添加了相关的 `source` 语句来引用 `/boot/grub/custom.cfg`.

>提示:  `/etc/grub.d/40_custom` 可以用做创建 `/etc/grub.d/nn_custom` 文件的模板,
>其中 `nn` 为优先级, 规定脚本文件的 `执行顺序`.
>而脚本文件的执行顺序决定了, 其所添加的条目在 `GRUB` 启动菜单中的位置.
>`nn` 应当比 `06` 大, 以此保证重要的脚本能够优先执行.

如要参考自定义菜单条目的例子, 请看[#启动菜单条目示例](https://wiki.archlinux.org/title/GRUB_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E5%90%AF%E5%8A%A8%E8%8F%9C%E5%8D%95%E6%9D%A1%E7%9B%AE%E7%A4%BA%E4%BE%8B).

[linux]: https://archlinux.org/packages/?name=linux
[linux-lts]: https://archlinux.org/packages/?name=linux-lts
[GRUB/Tips and tricks (简体中文)#多个启动条目]: https://wiki.archlinux.org/title/GRUB/Tips_and_tricks_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E5%A4%9A%E4%B8%AA%E5%90%AF%E5%8A%A8%E6%9D%A1%E7%9B%AE

#### 探测其他操作系统

想要让 `grub-mkconfig` 探测其他 `已经安装的系统`, 并自动把他们添加到 `启动菜单` 中,
[安装][] 软件包 [os-prober][] 并 [挂载][] 包含其它系统的 `磁盘分区`.
然后重新运行 `grub-mkconfig`. 如果你得到以下输出:

    Warning: os-prober will not be executed to detect other bootable partitions,

你需要编辑 `/etc/default/grub` 并取消下面这一行的注释, 如果没有这一行的话就在文件末尾手动添加:

    GRUB_DISABLE_OS_PROBER=false

然后运行 `grub-mkconfig` 再试一次.

注意:  记得每次运行 `grub-mkconfig` 之前都把包含其他操作系统的 `分区` 挂载上,
以免忽略了这些操作系统的启动项.

>提示:  你可能想让 `grub` 记住你上次选择的启动项,
参见 [GRUB/Tips and tricks (简体中文)#调用之前的启动条目](https://wiki.archlinux.org/title/GRUB/Tips_and_tricks_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E8%B0%83%E7%94%A8%E4%B9%8B%E5%89%8D%E7%9A%84%E5%90%AF%E5%8A%A8%E6%9D%A1%E7%9B%AE)

#### 调用之前的启动条目

`GRUB` 能够记住你最近一次使用的启动项, 并且在下次启动时将其作为默认项.
当你使用多个内核或操作系统时(比如当前的 `Arch` 和一个用来做后备选项的 LTS 内核), 这个特性很有用.
要开启这个功能, 编辑 `/etc/default/grub` 中的 `GRUB_DEFAULT` 选项:

    GRUB_DEFAULT=saved

上面的命令会告诉 `GRUB` 使用记住的启动项为默认启动项.
要想让 `GRUB` 记住当前选择的启动项, 将下面的行添加到 `/etc/default/grub`:

    GRUB_SAVEDEFAULT=true

仅当 `/boot` 不是 `btrfs` 文件系统的时候才能用, 因为 `GRUB` 没法对 `btrfs` 进行写入操作.
但它会生成一个容易误导人的错误信息: `sparse file not allowed. Press any key to continue.`

>注意:  手动添加启动项到 `/etc/grub.d/40_custom` 或 `/boot/grub/custom.cfg` 中,
比如添加 `Windows` 启动项, 需要先添加 `savedefault` 选项.

#### MS Windows

[os-prober][] 通常能自动发现包含 `Windows` 的分区.
当然在载入默认的 `Linux` 驱动的情况下, `NTFS` 分区也不是总能够被探测到.
如果 `GRUB` 没能发现它, 尝试安装 [NTFS-3G][], 然后重新挂载这个分区再试一次.

加密的 `Windows` 分区需要在解密之后才能挂载.
对于 `BitLocker`, 可以使用 [dislockerAUR]. 这足够 [os-prober][] 来添加正确的启动条目了.

[安装]: https://wiki.archlinux.org/title/Install
[os-prober]: https://archlinux.org/packages/?name=os-prober
[挂载]: https://wiki.archlinux.org/title/File_systems_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E6%8C%82%E8%BD%BD%E6%96%87%E4%BB%B6%E7%B3%BB%E7%BB%9F
[NTFS-3G]: https://wiki.archlinux.org/title/NTFS-3G
[dislockerAUR]: https://aur.archlinux.org/packages/dislocker/

#### 额外的参数

如想为 `Linux` 镜像添加额外的参数,
你可以在 `/etc/default/grub` 中设置 `GRUB_CMDLINE_LINUX` 和 `GRUB_CMDLINE_LINUX_DEFAULT` 变量.
生成 `普通启动项` 时, 这两个参数的值会合并在一起传给内核.
生成 `recovery` 启动项时, 仅使用 `GRUB_CMDLINE_LINUX` 参数.

两个参数不是一定要一起用. 例如要系统支持 [休眠][] 后恢复,
可以使用 `GRUB_CMDLINE_LINUX_DEFAULT="resume=UUID=uuid-of-swap-partition quiet"`,
其中 `uuid-of-swap-partition` 是你的 `交换分区` 的 `UUID`.
这样在生成 `recovery` 启动项时, 将不会启用 `resume` 功能, 也不会有 `quiet` 参数来省略启动时的内核信息.
而其他的 `普通启动项` 会包含它们.

`grub-mkconfig` 默认使用根文件系统的 [UUID][], 要禁用此设置,
去掉 `GRUB_DISABLE_LINUX_UUID=true` 前的注释符.

要生成 `GRUB recovery 启动项`,
需要确保在 `/etc/default/grub` 中 `GRUB_DISABLE_RECOVERY` 没设置为 `true`.

更多信息请参考 [Kernel parameters (简体中文)](https://wiki.archlinux.org/title/Kernel_parameters_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)).

[休眠]: https://wiki.archlinux.org/title/%E4%BC%91%E7%9C%A0
[UUID]: https://wiki.archlinux.org/title/UUID

### 挂载EFI分区

`内核`, `initramfs文件`, 以及在大多数情况下 `处理器的 microcode`,
都需要被 `boot loader` 或 `UEFI` 本身所访问, 才能成功启动系统.
因此, 如果你想保持简单的设置, 你选择的 `Boot Loader` 会限制 `EFI 系统分区` 的可用挂载点.

ESP: EFI system partition. EFI 系统分区是不依赖具体操作系统的,  它用来存储 EFI bootloaders,  applications 和驱动, 这些东西被 UEFI 固件启动, ESP 对于 UEFI 启动是必须的.

#### 典型的挂载点

挂载 `EFI系统分区` 的最简单方案是:

+ 将 `ESP` [挂载][] 到 `/efi`, 并且使用的 [boot loader][]
能访问存储在别处的 `kernel` 和 `initramfs镜像` (通常在 `/boot` 下面).
请参阅 [Arch boot process#Boot loader][] 以了解更多关于 boot loader 需求和功能的信息.

+ 将 `ESP` 挂载到 `/boot`. 这是直接从 UEFI 启动 [EFISTUB][] 内核,
或通过 [systemd-boot][] 等启动管理器启动时的首选方法.

+ 将 `ESP` 挂载到 `/efi`, 并将 `Extended Boot Loader Partition`(XBOOTLDR)挂载到 `/boot`.
当之前创建的 `ESP` 太小, 无法容纳多个 ` boot loaders` 或 `kernels `,
但 `ESP` 又不容易调整大小时(比如在Windows之后安装Linux进行双启动时), 这种方法就很有用.
这种方法至少被 [systemd-boot][] 所支持.

[boot loader]: https://wiki.archlinux.org/title/Boot_loader
[Arch boot process#Boot loader]: https://wiki.archlinux.org/title/Arch_boot_process#Boot_loader
[EFISTUB]: https://wiki.archlinux.org/title/EFISTUB
[systemd-boot]: https://wiki.archlinux.org/title/Systemd-boot
[挂载]: https://wiki.archlinux.org/title/Mount
[替代品]: https://github.com/systemd/systemd/pull/3757#issuecomment-234290236
[mkdir]: https://man.archlinux.org/man/mkdir.1
[Microcode]: https://wiki.archlinux.org/title/Microcode

>提示:
>`/efi` 是以前流行的 ESP挂载点 `/boot/efi` 的 [替代品][] (也可能仍然被其他 `Linux` 发行版使用).
>默认情况下, `/efi` 目录是不可用的, 你需要先用 [mkdir][] 创建它, 然后再把 `ESP` 挂载到上面.

#### 替代挂载点

如果你不使用上面的 `典型挂载点`, 你将需要把你的 ` boot files` 复制到 `ESP`(以下简称 `esp`).

```bash
# mkdir -p esp/EFI/arch
# cp -a /boot/vmlinuz-linux esp/EFI/arch/
# cp -a /boot/initramfs-linux.img esp/EFI/arch/
# cp -a /boot/initramfs-linux-fallback.img esp/EFI/arch/
```

>注意: 你可能还需要把 [Microcode][] 复制到 boot-entry 的位置.

此外, 你需要保持 `ESP` 上的文件, 与之后的内核更新同步.
如果不这样做, 可能会导致系统无法启动.
下面的章节讨论了几种自动化的机制.

>注意: 如果 `ESP` 没有挂载到 `/boot`,
请确保不要依赖 [systemd的自动挂载机制][](包括 [systemd-gpt-auto-generator(8)][]).
在更新系统或内核之前, 一定要手动挂载, 否则更新后可能无法挂载,
从而将你锁定在当前运行的内核中, 无法更新 `ESP` 上的内核副本.

或者, 在 [启动时预载所需的内核模块][], 例如.

```conf
/etc/modules-load.d/vfat.conf
vfat
nls_cp437
nls_ascii
```

[systemd的自动挂载机制]: https://wiki.archlinux.org/title/Fstab#Automount_with_systemd
[systemd-gpt-auto-generator(8)]: https://man.archlinux.org/man/systemd-gpt-auto-generator.8
[启动时预载所需的内核模块]: https://wiki.archlinux.org/title/Kernel_module#Automatic_module_loading_with_systemd

#### 使用bind mount

你可以使用 bind mount ([mount(8)][]) 将 `ESP` 下的目录挂载到 `/boot`,
而不是将 `ESP本身` 挂载到 `/boot`.
这允许 [pacman][] 直接更新内核, 同时保持ESP按照你的喜好组织.

注意: 这需要一个与 `FAT32` 兼容的 [内核][] 和 [引导程序][].
这对普通的 Arch安装 来说不是问题,
但对其他发行版(即那些需要在 `/boot/` 中设置符号链接的发行版)来说可能会有问题.
见 [论坛帖子][].

就像在 `替代挂载点` 中说的一样,
把所有的 boot files 复制到 `ESP` 上的目录, 但是在 `/boot` 之外的目录挂载 `ESP`.
然后 bind mount 这个目录.

```bash
# mount --bind esp/EFI/arch /boot
```

在验证成功后, 编辑你的 [Fstab][], 使这些变化持久化.

```conf
vim /etc/fstab
esp/EFI/arch /boot none defaults,bind 0 0
```

[mount(8)]: https://man.archlinux.org/man/mount.8
[pacman]: https://wiki.archlinux.org/title/Pacman
[内核]: https://wiki.archlinux.org/title/FAT#Kernel_configuration
[引导程序]: https://wiki.archlinux.org/title/Bootloader
[论坛帖子]: https://bbs.archlinux.org/viewtopic.php?pid=1331867#p1331867
[Fstab]: https://wiki.archlinux.org/title/Fstab

### grub 修复

[ubuntu 21.10 grub引导丢失修复](https://www.cnblogs.com/anice520/p/15746079.html)

适用于不小心损坏了 `grub` 配置文件, 例如 `grub.cfg` 文件被删除,
或者被 Windows 更新破坏等等.

#### 修复

关机之后重启直接进入 `grub` 命令行界面, 显示

    grub>

对 `grub` 进行操作, 使用 `ls` 命令查看分区信息, 使用 `ls -l` 命令查看分区详细信息

```bash
grub> ls
(proc) (hd0) (hd0,gpt1) (hd0,gpt2) # 显示的是我电脑中的分区, 执行结果以实际情况为准
grub> ls -l
xxxxxxxxxxxxxxxxxxxxxxxx # 分区的详细信息, 执行结果以实际情况为准
# 在输入命令时按下Tab键可以补全代码
```

执行命令之后会显示 `分区` 相关信息,
我的 `linux` 系统分区的文件格式是 `ext4` , 对应的分区是 `(hd0,gpt2)`. 执行以下代码:

```bash
grub> set root=/'hd0,gpt2'
grub> linux /boot/vmlinuz
grub> initrd /boot/initrd.img
grub> boot
```

但是重启之后并没有按照预期进入系统, 而是出现了

```bash
(initramfs)
```

这是因为前面 `grub` 命令行中的 `linux` 命令没有指定 `root` 参数.

+ 可以通过查看 `linux` 系统的 `/etc/fstab` 文件, 了解系统启动时 `root` 对应哪个设备路径 `/dev/sdX`

+ 也可以在 `(initramfs)` 环境中执行 `blkid` 命令, 查看分区信息, 显示如下

```bash
(initramfs) blkid
/dev/nvme0n1p2: UUID="25b94685-6eee-4c01-a1e2-e6935f53d566" BLOCK_SIZE="4096" TYPE="ext4" PARTUUID="ce803dfe-79f9-4c65-b684-83241be58218" #输出结果仅供参考, 请以实际执行结果为准
```

记下分区路径 `/dev/nvme0n1p2`, 关机.

```bash
(initramfs) poweroff
```

开机, 在 `grub` 环境下执行如下命令:

```bash
grub> set root='hd0,gpt2'
grub> linux /boot/vminuz-5.13.0-22-generic ro root=/dev/nvme0n1p2 #根据实际情况填写, 用 tab 补全
grub> initrd /boot/initrd-5.13.0-22-generic.img #根据实际情况填写
grub> boot
```

发现系统已经被正确引导了, 进入系统后, 在终端执行以下代码:

```bash
sudo grub-mkconfig -o /boot/grub/grub.cfg
```

重建 `grub.cfg` 引导文件, 重启验证结果, 再次成功进入系统.

### 我的 grub 自定义

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
