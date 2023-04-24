# windows 代理

[windows完全代理配置(包括UWP,应用商店,系统更新)](https://zhuanlan.zhihu.com/p/113108221)

首先基本的代理设置,对于常规程序,在win10下,可以通过:

设置 -> 网络和Internet -> 代理

在其他常规版本的windows下,可以通过:

控制面板 -> 网络和Internet -> Internet选项 -> 连接 -> 局域网设置

对于wget,curl及其他会自动引用环境变量的程序,可以通过:

win+R -> cmd -> "setx http_proxy http://127.0.0.1:[端口号]" -> "setx https_proxy http://127.0.0.1:[端口号]"

例如: setx http_proxy http://127.0.0.1:1080
此外,wget亦可通过如 "wget --config='./wgetrc' [url]"的方式指定配置文件以设置代理,wgetrc文件内容可如下:

```yaml
https_proxy = http://127.0.0.1:[端口号]/
http_proxy = http://127.0.0.1:[端口号]/
ftp_proxy = http://127.0.0.1:[端口号]/
use_proxy = on
```

对于UWP应用程序,可以通过:

下载安装 Fiddler -> 运行 -> WinConfig -> Exempt All -> Save Changes

前提是你已经完成了基本的代理设置

对于应用商店更新,系统更新,可以通过:

win+R -> cmd(管理员) -> netsh winhttp import proxy source=ie

前提是你已经完成了基本的代理设置,即IE配置,因为这条命令是以IE为源进行导入

对于其他不自动引用的软件,需要因软件而异人为设置代理,例如:

Jetbrains系: File -> Settings -> Appearance & Behavior -> System Settings -> HTTP Proxy

Anaconda: "C:\Users\[你的用户目录]" -> 编辑".condarc"添加如下:

```yaml
proxy_servers:
  http: http://127.0.0.1:[端口号]
  https: http://127.0.0.1:[端口号]
```

git: "C:\Users\[你的用户目录]" -> 编辑".gitconfig"添加如下:

```yaml
[http]
    proxy = http://127.0.0.1:[端口号]
[https]
    proxy = http://127.0.0.1:[端口号]
```

对于游戏使用代理加速,可以通过:https://github.com/NetchX/Netch

至于ping命令,由于它是直连的,虽然工作于应用层,但使用网络层的ICMP协议,而非传输层的TCP/UDP协议,而代理往往基于TCP/UDP协议,因此不能设置ping的代理
