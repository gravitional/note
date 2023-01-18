# vnc 虚拟网络计算机

## archwiki

[TigerVNC](https://wiki.archlinux.org/title/TigerVNC_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))

`TigerVNC` 是 `Virtual Network Computing` (VNC) 的一种实现. 它向用户提供一些远程功能, 包括:

+ 直接控制本地 `X` 会话.
+ 在一台机器上的后台并行 `X` 会话, 即并不显示在物理显示器上而是虚拟显示器. 即使用户断开连接, 在服务器上运行的所有程序依旧可以运行.

***
[run KDE Plasma 5 in VNC](https://unix.stackexchange.com/questions/315247/what-commands-are-needed-in-the-vnc-xstartup-file-to-run-kde-plasma-5-in-vn)

在`manjaro`上使用时, 连接到`vnc`服务器之后, 桌面是黑的, 可以打开终端, 这时执行`dbus-launch startplasma-x11 # or startplasma-wayland`即可打开桌面.

***
[D-Bus](https://wiki.archlinux.org/title/D-Bus)

`D-Bus` 是一个提供简便进程间通信的消息总线系统. 包含一个能以全系统或者针对一个用户会话运行的守护进程, 和一系列提供与 `D-Bus` 通信的库.
`dbus-launch`: 从 `shell` 脚本启动消息总线的程序

[Linux Device Model](https://linux-kernel-labs.github.io/refs/heads/master/labs/device_model.html)

`bus`是处理器和输入/输出设备之间的通信通道.  为了确保模型是通用的, 所有输入/输出设备都通过`bus`连接到处理器(它可以是没有对应的物理硬件的虚拟设备).

添加系统总线时, 它会出现在 `sysfs` 文件系统中的`/sys/bus`中.  与 `kobjects` 一样, 总线可以组织成层次结构, 并在 `sysfs` 中表示.

### 为虚拟(无界面)会话运行 vncserver

***
初次设置
注意:  在物理内存允许的条件下, `Linux`系统可以拥有任意数量的VNC服务器 -- 它们同时并行运行, 互不干扰.

1. 用`vncpasswd`创建密码, 它会将哈希处理之后的密码存储在`~/.vnc/passwd`.
2. 编辑`/etc/tigervnc/vncserver.users`来定义用户映射. 这文件中定义的用户都会拥有独有的端口来运行它的会话.
这文件中的数字对应的是`TCP`端口. 默认情况下, `:1` 是`TCP`端口`5901`(`5900+1`).
如果需要运行一个并行的服务器, 第二个实例可以运行在下一个最大的, 未被占用的端口, 即`5902`(`5900+2`).
3. 创建`~/.vnc/config`, 至少要定义会话的类型, 比如`session=foo`(将`foo`替换为你想要运行的桌面环境).
你可以通过查看`/usr/share/xsessions/`里的`.desktop`文件来知道有哪些桌面环境在当前系统上可以使用. 比如:

~/.vnc/config

```bash
session=lxqt
geometry=1920x1080
localhost #这个配置会导致只能本地访问 vnc
alwaysshared
```

***
权限: 像对待 `~/.ssh` 一样保护 `~/.vnc` 是很好的做法, 虽然并非必须. 执行下面的命令来达到该目的:

```bash
$ chmod 700 ~/.vnc
```

***
启动与停止`tigervnc`

`systemctl start  vncserver@.service`, 如果需要让它随系统启动, `enable`它.
注意`/etc/tigervnc/vncserver.users`中定义的编号需要在`@`符号后面指定, 比如启动`:1`的命令是:

```bash
# systemctl start vncserver@:1
```

Note: 已经不再支持直接调用`/usr/bin/vncserver`了, 因为这样做不会建立完整可用的会话环境.
`systemd`服务是唯一受支持的使用`TigerVNC`的方式. 参见Issue #1096.

## 在物理显示器上(5900端口)运行VNC服务:

***
使用 `TigerVNC 的 x0vncserver` (推荐)

`TigerVNC` 提供名为 `x0vncserver` 的二进制文件, 它具有和 `x11vnc` 相类似的功能. 例如:

```bash
x0vncserver -display :0 -passwordfile ~/.vnc/passwd
```

欲获取更多信息, 执行`man x0vncserver`

***
使用 `x11vnc` (推荐)

如果需要远程控制物理显示, 使用 `x11vnc`. 参见 `X11vnc` 获取更多信息.

## 连接 VNC 服务

一个 `VNC` 服务可允许任意数量的客户端来连接. 下面给出一个简单例子, 其中 `VNC` 服务运行在 `10.1.10.2` 的 `5901`(`:1`, 使用速记法)端口:

```bash
$ vncviewer 10.1.10.2:1
```

### 无密码验证

`-passwd` 开关允许我们定义服务器上 `~/.vnc/passwd` 文件的位置. 在服务器方面(无论是通过 `SSH` 还是物理接触), 用户需要有权访问该文件.
在任一情况下, 都应将该文件放在客户端文件系统的一个安全位置(例如一个仅给期望用户`read`权限的位置).

```bash
$ vncviewer -passwd /path/to/server-passwd-file
```

图形界面客户端示例

+ gtk-vnc
+ krdc
+ rdesktop
+ vinagre
+ remmina
+ vncviewer-jarAUR

## 使用 SSH 隧道加密 VNC 服务

### 服务端配置

若希望从 `LAN` 保护之外访问 `VNC` 服务, 你需要考虑明文密码及客户端与服务端之间未加密通信的问题.
`VNC` 服务可以很简单地使用 `SSH` 隧道进行加密. 另外, 不要使用此方法对外界打开另一个端口, 因为通信会沿用户之前对 `WAN` 打开的 `SSH` 端口在隧道中依次进行.
在这种情况下, 强烈推荐使用 `-localhost` 开关运行 `vncserver`.
该开关仅允许接收从本机发起的连接, 并顺理成章地仅允许物理 `SSH` 连接并认证到机器上的用户.

```bash
$ vncserver -geometry 1440x900 -alwaysshared -dpi 96 -localhost :1
```

### 客户端配置

既然服务器现在只接受本机的连接, 使用 `-L` 开关通过 `SSH` 连接到该机器来打开隧道. 例如:

```bash
$ ssh 目标机器IP -L 8900:localhost:5901
```

该命令将服务器的 `5901` 端口转发到客户机的 `8900` 端口. 一旦已通过 `SSH` 连接, 请保持该 `xterm` 或 `shell` 窗口开启 -- 它会作为和服务器通信的加密隧道.
为了通过 `VNC` 连接, 打开第二个 `xterm` 并连接到客户机的加密隧道上(而不是远程服务器的 `IP` 地址):

```bash
$ vncviewer localhost::8900
```

***
以下来自 `SSH` 的手册页面:

    -L [bind_address:]port:host:hostport
    -L [bind_address:]port:remote_socket
    -L local_socket:host:hostport
    -L local_socket:remote_socket

指定到本地`(client)`主机上给定 TCP 端口或 Unix 套接字的连接将被转发到给定远程端的`host`和`port`, 或 `Unix socket`.
通过分配一个`socket`来侦听本地端的 `TCP` 端口, 可选择绑定到指定的 `bind_address` 或 `Unix socket`.
每当`local port`或`socket`建立连接时, 连接通过安全通道转发(`forwarded`), 并连接到远程计算机上的口 `hostport` 或 `remote_socket`.

端口转发也可以在配置文件中指定. 只有超级用户才能转发特权端口.  `IPv6` 地址可以通过将地址括在方括号中来指定.

默认情况下, 根据`GatewayPorts`设置绑定本地端口. 显式的 `bind_address` 可用于将连接绑定到特定地址.
`bind_address`若为`localhost`, 表示绑定监听端口仅供本地使用, 而空地址或`*`表示该端口可以从所有`interfaces`使用.

## 在 Android 设备上通过 SSH 连接 VNC 服务器

为了在 `Android` 设备上通过 `SSH` 连接到 `VNC` 服务器, 需要具备以下条件:

1. 在要连接的机器上运行 `SSH` 服务
2. 在要连接的机器上运行 `VNC` 服务. (使用上面提到的 `-localhost `标志来运行服务)
3. 在 `Android` 设备上安装 `SSH` 客户端. (`ConnectBot` 是一个常见选择, 并在本向导中用作例子. )
4. 在 `Android` 设备上安装 `VNC` 客户端(`androidVNC`)

考虑为目标使用一些动态 `DNS` 服务, 来解决无固定 `IP` 地址问题.
在 `ConnectBot` 中, 键入 `IP` 并连接到所期望的机器. 按选项键, 选择端口转发, 并增加一个新端口然后保存之:

    Nickname: vnc
    Type: Local
    Source port: 5901
    Destination: 127.0.0.1:5901

在 `androidVNC` 中:

    Nickname: nickname
    Password: 设置 VNC 服务器时使用的密码
    Address: 127.0.0.1 (通过 ssh 连接之后, 我们在远程机器的"本地")
    Port: 5901

连接即可.

## 在开关机时启动关闭 VNC 服务

如果是`arch`, `vncserver.service`可能安装在`/usr/lib/systemd/system/vncserver.service`.
如果是`ubuntu`, 默认似乎没有安装`vncserver.service`, 自己创建一个: `sudo vim /etc/systemd/system/vncserver@.service`,

```.service
# nolisten=tcp 选项可以禁止使用`TCP`连接
# localhost 选项表示只能使用`ssh`隧道连接
# 详细查看 man vncviewer 的 "-via" 选项.

[Unit]
Description=Remote desktop service (VNC)
After=syslog.target network.target

[Service]
Type=forking
User=

# Clean any existing files in /tmp/.X11-unix environment
ExecStartPre=-/usr/bin/vncserver -kill %i
ExecStart=/usr/bin/vncserver %i
ExecStop=/usr/bin/vncserver -kill %i

[Install]
WantedBy=multi-user.target
```

## 复制远程机器剪贴板内容到本地

如果从远程机器到本机的复制不工作, 如该参考下面所述, 在服务器上执行 `autocutsel`:

```bash
$ autocutsel -fork
```

现在, 按下 `F8` 键显示 `VNC` 弹出菜单, 选择 `Clipboard: local -> remote` 选项.

你可以将上述命令放在 `~/.vnc/xstartup` 中, 以使其在 `VNC` 服务启动时自动运行.

## 解决无光标问题

如果使用 `x0vncserver` 时光标无法显示, 那么像下面这样启动 `vncviewer`:

```bash
$ vncviewer DotWhenNoCursor=1 <server>
```

或者将 `DotWhenNoCursor=1` 写在 `tigervnc` 的配置文件(默认在 `~/.vnc/default.tigervnc`)中.

## ubuntu 20.04

[tigervncserver crashes unless started with sudo](https://stackoverflow.com/questions/59709214/tigervncserver-crashes-unless-started-with-sudo)

`ubuntu` 上安装完`tigervncserver`, 用`systemctl`启动时失败, 原因可能是没有` ~/.vnc/xstartup`文件. 创建`~/.vnc/xstartup`, 内容如下:

```bash
#!/bin/bash

PATH=/usr/bin:/usr/sbin
unset SESSION_MANAGER
unset DBUS_SESSION_BUS_ADDRESS
exec cinnamon-session-cinnamon &
```

`exec`后面的值可以查看`/usr/share/xsessions/`中`.desktop`文件内的写法, 例如`cinnamon.desktop`中的值是`cinnamon-session-cinnamon`.

之后启动`vncserver`就不需要`sudo`了, 也不会`crash`.
我猜缺少可执行文件会阻止 `vncserver` 启动, 但是使用 `sudo` 权限,
它设法从某些文件中提取的默认设置开始, 这些文件只能由 `sudoers` 访问.
但在`ubuntu 20.04`上使用`gnome-session`能连接, 但是会有黑屏问题.
