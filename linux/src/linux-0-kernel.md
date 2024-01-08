# linux kernel

## 硬件驱动异常, 例如 WIFI 丢失

[新安装的Ubuntu20.04 5.13上没有WIFI 看这一篇就够了](https://blog.csdn.net/Jack_Sparrow33/article/details/127525441)

可能是内核版本不对等原因造成的.
开机界面上使用方向键选择 `Advanced options for Ubuntu`,
选择其他内核版本, 有可能解决问题, 可以选取上一个正常的内核版本, 例如
`Ubuntu, with Linux 6.5.0-060500-generic`

## 查看, 切换内核

[ubuntu---查看, 安装, 切换内核](https://www.cnblogs.com/carle-09/p/12377128.html)

首先可以查看一下内核列表:

```bash
sudo dpkg --get-selections | grep linux-image
```

查看Linux中安装了哪些内核:

## dpkg selection

`--get-selections [package-name-pattern...]`
获取 package selections 列表, 并将其写入标准输出.
如果没有 pattern, 未安装的包(例如已被purged)将不会显示.

`--set-selections`
使用从stdin读取的文件设置 pacage selections.
该文件的格式应为 "package state",  其中state为 `install`, `hold`, `deinstall` or `purge`之一.
也允许使用空白行和以 `#` 开头的注释行.

`available file` 必须是最新的, 否则未知的包将被忽略 附带一个警告.
有关更多信息, 请参见 `--update-avail` 和 `--merge-avail` 命令.

`--clear-selections`
设置每个 `non-essential`包 的请求状态为 `deinstall`(从dpkg 1.13.18开始).
这是打算紧接在 `--set-selections` 之前使用的,
卸载不在`--set-selections` 参数列表中的任何包.

需要知道的是, 内核映像文件主要包括以下类型:
linux-image-版本号: 内核映像文件
linux-headers-版本号: 内核头文件
linux-image-extra-版本号: 内核扩展文件

```bash
# 删除多余内核文件:
sudo apt-get purge linux-image-<版本号> 命令

# 更新grub文件:
sudo update-grub
```

然后安装自己想要的内核版本:

```bash
sudo apt install linux-image-6.5.0-14-generic linux-headers-6.5.0-14-generic linux-modules-extra-6.5.0-14-generic
```

安装好以后, 查看启动顺序

```bash
cat /boot/grub/grub.cfg| grep menuentry
```

如果需要修改启动顺序:  `sudo gedit /etc/default/grub`
把 `GRUB_DEFAULT=0` 修改为类似下面的

```bash
GRUB_DEFAULT="Ubuntu, with Linux 6.6.0-060600-generic"
```

当然内核版本应该是你想要的.
然后使用`sudo update-grub`更新启动项
然后修改 sudo gedit /boot/grub/grub.cfg

## 查看内核版本

```bash
cat /proc/version
# or
uname -a
# or
lsb_release -a
```
