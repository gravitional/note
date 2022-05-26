# 修复grub2 教程2

[Linux与Windows 10用grub引导教程](https://www.cnblogs.com/jpfss/p/9462792.html)

首先, 在你装了 `Windows` 之后, `Windows` 在装机过程中,
会将硬盘划分出一个约 `100MB` 大小的分区, 称为 `EFI` 分区, 这个分区就是用来 boot 的. 
在资源管理器中是看不到的这个分区的, 可以在磁盘管理中看到, 管理则需要借助 `DiskGenius` 工具.
可以看到该分区包含 3 个文件夹(如果你没有装 `Linux` 的话, 就只有两个), 
分别是 `Boot`, `Microsoft` 和 `Manjaro`, 其中 `Boot` 文件夹就是 `UEFI` 引导所必需的文件.

我们继续打开 `Microsoft/Boot` 文件夹, 将会看到许多文件, 
这些文件就是启动 `Windows 10` 所必需的, 包含了语言包, 字体等, 
`BCD` 包含了 `Windows` 引导开始以后的信息.
其中, `bootmgfw.efi ` 是 `Windows` 的默认引导文件.

```bash
EFI/Boot/bootx64.efi
EFI/Microsoft/Boot/bootmgfw.efi
```

以上是采用 `UEFI` 启动 `Windows 10` 的文件结构, 
也就是说当你按下开机按钮的时候, 首先 `UEFI` 找到 `EFI` 分区的 `Boot` 文件夹,
然后加载 `bootx64.efi` 文件, 读取文件信息, 
找到 `EFI/Microsoft/Boot/bootmgfw.efi`, 
按照 `bootmgfw.efi` 的要求, 加载所需的启动信息, 启动 `Windows 10`.

## 准备工作

在正式装系统之前, 我们还需要做一些准备工作:

+ 关闭 `Windows` 的快速启动
这个功能的作用是在于关机的时候不完全断电, 类似将系统处于`休眠`状态,
这样可以让开机更加迅速, 但这也就导致了只能使用 `Windows` 系统.

+ 关闭 `BIOS` 的 `Secure Boot `的功能

在默认情况下, `UEFI` 固件只会加载那些被签名的引导程序. 
在缺少 `Secure Boot` 功能的传统 PC 机上, 恶意的后门程序可以加载自身, 进而摇身一变伪装成 `引导程序`.

这样的话, `BIOS` 就会在启动的时候加载后门程序, 这样它就可以躲过操作系统, 把自己隐藏得很深.
但是不得不说, 这对我们安装 `Linux` 造成了很大的困扰, 也是直接导致我们重启到 Windows 10 后进不去 `Linux` 的原因.

首先我们要关闭这个功能: 进入 `BIOS` 找到 `Secure Boot`, 选择 `disabled`, 这样就关闭了. 
当然, 有些人进入 `BIOS` 会发现 Secure Boot 这个选项是灰色的(比如我的就是), 
这时你需要先给你的 `BIOS` 设一个密码, 然后就能关闭 `Secure Boot` 了.

## 安装 Linux

所有的准备都已经完成, 这时就可以准备刻录 U 盘了, 推荐`Rufus`和`USBWriter`.
刻录完成后, 重启按 `f12`, 选择从 USB 设备启动, 对于绝大多数发行版来说一路回车就行了, 
只需要注意一点: 在选择挂载 `boot` 位置的时候, 一定要挂载在 `efi` 分区, 别的都不行.
重启之后, 不出意外的话, 你会直接进入 Windows 10, 
不要担心, 这时 Linux 已经安装成功了, 我们只需要将引导文件替换一下.

## 替换引导文件

先用 `DG` 打开 `EFI` 分区, 你会看到多了一个文件夹, 名称取决于你安装的是哪一个发行版.
我安装的是 `Manjaro Linux`, 名称就是 `Manjaro`, 
打开之后会发现里面有一个名为 `grubx64.efi` 的文件, 这就是启动 `Linux` 的引导文件.
和 `Windows 10` 的 `bootmgfw.efi` 类似, 
我们想要用 `grubx64.efi` 引导代替掉 `bootmgfw.efi`, 这样就可以用 `GRUB` 引导了. 步骤:

进入管理员命令行. 方法: `win + x`, 再按 `a`
输入 

```cmd
bcdedit /set {bootmgr} path \EFI\Manjaro\grubx64.efi
```

提示操作成功的话, 就完成了.

>注: 如果输入以上命令提示 `参数错误` 的话, 将 `{bootmgr}` 改为 `'{bootmgr}'`, 
>原因是 `PowerShell` 和 `CMD` 语法的差别.

至此, 如果你安装的是除 `Arch` 之外绝大多数发行版, 
那么接下来就和你没有啥关系了, 你已经成功了, 好好享受吧!

开机之后会发现进入 `GRUB` 的引导了, 通常会包含至少三个选项(以 `Manjaro` 举例):
`Manjaro`, `Manjaro 高级选项`和 `Windows Manager`.
这就代表你已经完美的解决了 Windows 和 Linux 双系统引导的问题.

## 修复 Windows 引导

>一般不建议直接编辑 `/boot/grub/grub.cfg`, 更简单且可靠的方法是使用 [os-prober][].

这一点是我安装 Arch Llinux 的时候发现的, Arch Linux 安装过程是手动安装的, 
在编写 `GRUB` 的时候会扫描不到 `Windows Manager` 所在的分区(当然可能不是所有人都会遇到),
所以在 `GRUB` 界面可能会看不到 `Windows Manager` 选项, 
导致进不去 Windows 10, 这里就需要手动编辑 `GRUB` 信息,
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

然后保存. 
>注意: 这里的 `$hints_string`, 代表的下面命令的输出:

```bash
sudo grub-probe --target=hints_string /boot/efi/EFI/Microsoft/Boot/bootmgfw.efi
```

而 `$fs_uuid` 代表的是下面命令的输出:

```bash
sudo grub-probe --target=fs_uuid /boot/efi/EFI/Microsoft/Boot/bootmgfw.efi
```

在终端执行命令: 

```bash
sudo grub-mkconfig -o /boot/grub/grub.cfg
```

就 `OK` 了. 到此,  `Arch Linux` 和 `Windows` 10 双系统也配置完毕了.

#### 附加问题

+ 在 Windows 10 进行了一个大更新后, 会发现 GRUB 引导界面没有了, 
还是直接进入了 Windows 10, 这时只需要按照 替换引导文件 的方法重新输入一遍命令就行.

+ 使用 Linux 某个发行版一段时间之后, 难免会想尝试一下另一个发行版. 
这时请务必将之前发行版的 `引导文件` 删除, 否则可能会出现无论怎么设置都无法进入 GRUB 的情况. 
例如: 我之前用的是 `Ubuntu`, 我现在换成了 Manjaro, 我就需要用 DG 删除 EFI 分区的 Ubuntu 文件夹.

+ 在我使用 `Manjaro` 更新了一次 `Linux` 的内核后, 进不去 `Windows 10` 了, 
这个时候千万不要直接修复 `Windows 10` 引导, 这会格式化 `EFI` 分区, 
只需要按上面修复 `Windows` 引导 的方法编辑一下 `GRUB` 就可以了.

## Arch Wiki

[Arch Wiki 上的示例](https://wiki.archlinux.org/index.php/GRUB_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E5%90%AF%E5%8A%A8%E8%8F%9C%E5%8D%95%E6%9D%A1%E7%9B%AE%E7%A4%BA%E4%BE%8B)

这个模式寻找 `Windows` 的启动加载器的位置, 
然后当用户选择了相应的菜单条目的时候, 通过链式载入的方法在 `GRUB` 之后加载它.
这里主要的任务是, 找到 `EFI 系统分区` 然后从上面运行 `启动加载器`. 
注意:  这个启动项仅在 `UEFI模式`下才起作用, 而且 `Windows` 和 `UEFI` 的位数必须相同.

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

这两个命令都是假设 `Windows` 使用的 `ESP` 是挂载在`$esp`上的. 
当然,  `Windows` 的 `EFI` 文件路径可能有变,因为这就是 `Windows` ...
