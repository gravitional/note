# Windows SubLinux2

[ Win10 Terminal + WSL 2 安装配置指南](https://www.cnblogs.com/willick/p/13924325.html)
[设置从Windows Terminal打开wsl时进入Linux用户主目录](https://blog.csdn.net/baidu_33340703/article/details/106949948)
[适用于 Linux 的 Windows 子系统安装指南 (Windows 10)](https://docs.microsoft.com/zh-cn/windows/wsl/install-win10)
[如何在windows的资源管理器下访问WSL2?](https://www.zhihu.com/question/398307967)

## WSL目录, 资源

`wsl`目录: 在资源管理器输入:`\\wsl$`, 必须是反斜杠.

## WSL常用命令

[WSL常用命令](https://zhuanlan.zhihu.com/p/517525867)

查看帮助信息:

```bash
wsl --help # --help 可简写为 -h
```

安装子系统(必须是微软官方提供的子系统):

```bash
wsl --install --distribution <Distro>  # --distribution 可简写为 -d
# 例如, 安装 Debian:
wsl --install -d Debian
```

查看微软官方提供的子系统:

```bash
wsl --list --online # 可简写为 wsl -l -o
```

官方提供的发行版只能安装在 C 盘, 如果不想安装到 C 盘, 那你可以自制子系统并导入.
微软官方教程: [Import any Linux distribution to use with WSL | Microsoft Docs](https://link.zhihu.com/?target=https%3A//docs.microsoft.com/en-us/windows/wsl/use-custom-distro)
这篇教程以 CentOS 为例.

设置默认的子系统:

```bash
wsl --set-default <Distro>  # --set-default 可简写为 -s
# 例如, 将 Ubuntu 设为默认子系统:
wsl -s ubuntu
```

设置 wsl 的默认版本(wsl 有两个版本: wsl1 和 wsl2):

```bash
wsl --set-default-version <Version>
# 例如, 将 wsl2 设为默认版本:
wsl --set-default-version 2
```

将某一子系统设为 wsl1 或 wsl2:

```bash
wsl --set-version <Distro> <Version>
# 例如, 将 Ubuntu 设为 wsl2:
wsl --set-version ubuntu 2
```

查看状态, 包括当前的默认子系统是哪个, wsl 的版本是 1 还是 2, 内核版本等:

```bash
wsl --status
```

查看详细信息, 包括安装了哪些子系统, 子系统的运行状态, wsl1 还是 wsl2:

```bash
wsl --list --verbose  # 可简写为 wsl -l -v, 非常常用!
```

进入默认的子系统:

```bash
wsl
```

进入某个子系统, 以 `ubuntu` 为例
(只对微软官方提供的子系统有效, 对自己导入的子系统无效, 如 CentOS):

```bash
ubuntu
```

默认以非 `root` 用户登录.
如果想默认使用 `root` 用户, 需执行命令: `ubuntu config --default-user root`
查看更多选项可执行:  `ubuntu help`.

进入某个子系统的另一种方式:

```bash
wsl -d <Distro>  // -d 是 --distribution 的简写
# 例如, 进入 Debian
wsl -d debian
```

进入子系统后, 可以通过 exit 命令, 或 Ctrl + d 退出.
直接运行子系统命令:

```bash
wsl -d <Distro> <command>  // -d 是 --distribution 的简写

# 例如, 在 Ubuntu 中运行 ls 命令:
wsl -d ubuntu ls
# 如果是在默认子系统中运行命令, 可省略 -d <Distro>, 例如:
wsl ls
```

可以在运行命令时指定用户:

```bash
wsl -d <Distro> --user <Username> <command>  # --user 可以简写为 -u
# 例如, 在 Ubuntu 中以 root 用户的身份来运行 whoami 命令:
wsl -d ubuntu -u root whoami
```

省略用户时, 使用默认用户.

+ 一旦进入过某个子系统, 或直接运行过某个子系统的命令, 那该子系统就会变为运行状态.
关闭某个子系统:

```bash
wsl --terminate <Distro> // --terminate 可简写为 -t, <Distro> 不能省.

# 例如, 关闭 Ubuntu:
wsl -t ubuntu

关闭所有子系统:
wsl --shutdown
```

一个命令行窗口只能有一个运行状态的子系统, 运行一个新的, 就会自动停止一个旧的.
想要同时运行多个子系统, 需要打开多个命令行窗口(关闭命令行窗口不会关停子系统).

+ 卸载某个子系统:

```bash
wsl --unregister <Distro>
# 例如, 卸载 Ubuntu:
wsl --unregister ubuntu
```

另外, 通过微软应用商店安装的子系统, 可以像普通应用一样卸载.

## WSL 设置代理

[为 WSL2 一键设置代理](https://zhuanlan.zhihu.com/p/153124468)
[WSL2配置代理](https://www.cnblogs.com/tuilk/p/16287472.html)

### Windows下打开代理

+ 允许局域网
以Clash为例, 选择主页选项卡, 开启 `允许局域网`:
![img](https://img2022.cnblogs.com/blog/2026333/202205/2026333-20220519095954875-534125057.png)

+ 开启防火墙
打开Windows Defender 防火墙,
选择允许应用或功能通过 Windows Defender 防火墙,
点击更改设置, 找到Clash for Windows, 然后勾选专用和公用.
如果找不到, 点击下方的允许其他应用,
然后找到安装路径, 将 `Clash for Windows.exe` 文件加入进来.

![img2](https://img2022.cnblogs.com/blog/2026333/202205/2026333-20220519100052696-2100542151.png)

### 单次配置

这种配置方法适用于单次配置, 也就是在重启终端后会失效.
在终端中输入如下语句:
如果是采用HTTP协议:

```bash
export hostip=$(cat /etc/resolv.conf |grep -oP '(?<=nameserver\ ).*')
export https_proxy="http://${hostip}:7890";
export http_proxy="http://${hostip}:7890";
```

其中后两行的7890需要更换为自己代理服务器的端口号,
在Clash的主页选项卡中可以查看.

如果采用`socket5`协议:

```bash
export hostip=$(cat /etc/resolv.conf |grep -oP '(?<=nameserver\ ).*')
export http_proxy="socks5://${hostip}:7890"
export https_proxy="socks5://${hostip}:7890"
```

如果端口号一样则可以合并成为一句话:

```bash
export all_proxy="socks5://${hostip}:7890"
```

使用`curl`即可验证代理是否成功, 如果有返回值则说明代理成功.

```bash
curl www.google.com
```

### 长期配置

这种配置方法适用于长期配置, 也就是写一个脚本, 然后可以通过命令启动代理.
新建 `proxy.sh` 脚本如下:

```bash
#!/bin/sh
hostip=$(cat /etc/resolv.conf | grep nameserver | awk '{ print $2 }')
wslip=$(hostname -I | awk '{print $1}')
port=7890

PROXY_HTTP="http://${hostip}:${port}"

set_proxy(){
  export http_proxy="${PROXY_HTTP}"
  export HTTP_PROXY="${PROXY_HTTP}"

  export https_proxy="${PROXY_HTTP}"
  export HTTPS_proxy="${PROXY_HTTP}"

  export ALL_PROXY="${PROXY_SOCKS5}"
  export all_proxy=${PROXY_SOCKS5}

  git config --global http.https://github.com.proxy ${PROXY_HTTP}
  git config --global https.https://github.com.proxy ${PROXY_HTTP}

  echo "Proxy has been opened."
}

unset_proxy(){
  unset http_proxy
  unset HTTP_PROXY
  unset https_proxy
  unset HTTPS_PROXY
  unset ALL_PROXY
  unset all_proxy
  git config --global --unset http.https://github.com.proxy
  git config --global --unset https.https://github.com.proxy

  echo "Proxy has been closed."
}

test_setting(){
  echo "Host IP:" ${hostip}
  echo "WSL IP:" ${wslip}
  echo "Try to connect to Google..."
  resp=$(curl -I -s --connect-timeout 5 -m 5 -w "%{http_code}" -o /dev/null www.google.com)
  if [ ${resp} = 200 ]; then
    echo "Proxy setup succeeded!"
  else
    echo "Proxy setup failed!"
  fi
}

if [ "$1" = "set" ]
then
  set_proxy

elif [ "$1" = "unset" ]
then
  unset_proxy

elif [ "$1" = "test" ]
then
  test_setting
else
  echo "Unsupported arguments."
fi
```

注意: 其中第4行的`<PORT>`更换为自己的代理端口号.

```bash
source ./proxy.sh set: 开启代理
source ./proxy.sh unset: 关闭代理
source ./proxy.sh test: 查看代理状态
```

#### 任意路径下开启代理

可以在 `~/.bashrc` 中添加如下内容, 并将其中的路径修改为上述脚本的路径:

```bash
alias proxy="source /path/to/proxy.sh"
```

然后输入如下命令:

```bash
source ~/.bashrc
```

那么可以直接在任何路径下使用如下命令:

```bash
proxy set: 开启代理
proxy unset: 关闭代理
proxy test: 查看代理状态
```

### 自动设置代理

也可以添加如下内容, 即在每次shell启动时自动设置代理,
同样的更改其中的路径为自己的脚本路径:

```bash
. /path/to/proxy.sh set
```

## WSL2 errro: 参考的对象类型不支持尝试的操作

使用 `VPN` 造成的 WSL2 启动 crash

[failing to startup with code 4294967295](https://github.com/microsoft/WSL/issues/5092#:~:text=Solve%20%22process%20exited%20with%20code%204294967295%22%20%2C%20run,complete%20the%20reset.%20Does%20not%20resolve%20the%20issue.)
[关于使用WSL2出现'参考的对象类型不支持尝试的操作'的解决方法](https://zhuanlan.zhihu.com/p/151392411)

临时解决

```pwsh
netsh winsock reset
```

长期解决的方案(推荐),下载此软件: [http://www.proxifier.com/tmp/Test20200228/NoLsp.exe](http://www.proxifier.com/tmp/Test20200228/NoLsp.exe)

因需要梯子访问下载,有些朋友不方便,所以我上传到百度云分享在这里:
[https://pan.baidu.com/s/1bVZ0OXZPxEt8l1IHYaFK3A](https://pan.baidu.com/s/1bVZ0OXZPxEt8l1IHYaFK3A),
提取码: `vjge`

然后在管理员身份运行`CMD`输入:

```pwsh
NoLsp.exe C:\windows\system32\wsl.exe
```

请自行注意`NoLsp.exe`程序的位置,以及你的`wsl.exe`位置.

***
产生原因和解决方法分析:

代理软件和`wsl2`的`sock`端口冲突,使用`netsh winsock reset`重置修复. `Proxifer` 开发人员解释如下:

如果`Winsock LSP DLL`被加载到其进程中, 则`wsl.exe`将显示此错误.
最简单的解决方案是对`wsl.exe`使用`WSCSetApplicationCategory WinAPI`调用来防止这种情况.
在后台,该调用在`HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\WinSock2\Parameters\AppId_Catalog` 中为`wsl.exe`创建一个条目.
这将告诉`Windows`不要将`LSP DLL`加载到`wsl.exe`进程中.

## WSL error: 检测到 localhost 代理配置, 但未镜像到 WSL. NAT 模式下的 WSL 不支持 localhost 代理

[检测到 localhost 代理配置](https://www.cnblogs.com/hg479/p/17869109.html)
[Windows 11 + WSL + VS Code + Rust 环境配置](https://www.cnblogs.com/LLW-NEU/p/17857553.html)

在Windows中的`C:\Users\<your_username>`目录下创建一个`.wslconfig`文件,
然后在文件中写入如下内容

```toml
[experimental]
autoMemoryReclaim=gradual
networkingMode=mirrored
dnsTunneling=true
firewall=true
autoProxy=true
```

然后用 `wsl --shutdown` 关闭WSL, 之后再重启, 提示就消失了.

## [跨 Windows 和 Linux 文件系统工作](https://learn.microsoft.com/zh-cn/windows/wsl/filesystems#file-storage-and-performance-across-file-systems)

建议不要跨操作系统使用文件, 除非有这么做的特定原因.
若想获得最快的性能速度, 请将文件存储在 WSL 文件系统中,
前提是在 Linux 命令行(Ubuntu, OpenSUSE 等)中工作.
如果使用 Windows 命令行(PowerShell, 命令提示符)工作, 请将文件存储在 Windows 文件系统中.

例如, 在存储 WSL 项目文件时:

+ 使用 Linux 文件系统根目录: `/home/<user name>/Project`
+ 而不使用 Windows 文件系统根目录: `/mnt/c/Users/<user name>/Project$` 或 `C:\Users\<user name>\Project`

在 WSL 命令行的文件路径中看到 `/mnt/` 时, 表示你正在使用已装载的驱动器.
因此, Windows 文件系统 `C:/drive` (`C:\Users\<user name>\Project`)
在 WSL 命令行中装载时将如下所示: `/mnt/c/Users/<user name>/Project$`.
可以将项目文件存储在装载的驱动器上, 但如果将其直接存储在 `\\wsl$` 驱动器上, 性能速度会提高.
