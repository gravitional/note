# aria2 安装配置

[让 aria2 更容易使用的现代 Web 前端](http://ariang.mayswind.net/zh_Hans/)

这里主要介绍`windows`下的安装配置方法,其他平台类似.
`aria2`程序分成前后端, 后端是一个叫做`aria2c.exe`的程序, 在命令行中运行.
前端是一个网页`index.html`, 在浏览器中运行.

## ariaNg前端

[mayswind/AriaNg]: https://github.com/mayswind/AriaNg/releases

从[mayswind/AriaNg][]下载网页版界面, 下载 `AriaNg-xxx-AllInOne.zip`,
可以也放到 `aria2` 文件夹里面, 用浏览器打开里面的`index.html`,
它默认会监听本地的`6800`端口, 后面我们会配置`aria2`, 让它连接上后台服务.
可以`ctrl+D`收藏这个页面, 方便下次使用.

如果连接不上, 记得查看配置文件中的选项

```conf
# RPC监听端口, 可修改, 默认:6800; AriaNg设置->右边->RPC地址
rpc-listen-port=6800 端口
# 设置的RPC授权令牌, 网页端需要对应; AriaNg设置->右边->RPC密钥
rpc-secret=abcd
# 启用加密后RPC服务, 需要使用https或者wss协议连接; AriaNg设置->右边->RPC协议
rpc-secure=true
```

## aria2c.exe

+ 从官网地址[aria2/releases](https://github.com/aria2/aria2/releases)下载系统对应版本的程序,
例如`aria2-1.35.0-win-64bit-build1.zip`,解压缩. 放到一个准备好的目录,比如`d:\aria2`.

## aria2c 配置文件

参考[Aria2 & YAAW 使用说明](http://aria2c.com/usage.html),
编辑`~\.aria2\aria2.conf`文件, 在我的电脑是

```powershell
C:\Users\qingz\.aria2\aria2.conf
```

只需配置一次,以后启动时它会自动读取配置.

+ 配置主要参考[Aria2 新手入门](https://zhuanlan.zhihu.com/p/37021947),
我自用的轻微修改版也放在下面. 先修改重要的几行, 能正常打开网页版界面即可.

其他配置可以用到再修改, `#`开头的行是注释, 可以随便修改.
同样, 要让修改后的配置生效, 记得删掉前面的`#`.

+ 在`aria2`的文件夹, 打开 `powershell`窗口, 新建文件

```powershell
New-Item -Path . -Name "aria2.session" -ItemType "file"
```

将会新建 `aria2.session`文件, 它用来记录下载状态, 获取它的绝对路径.

```powershell
Resolve-Path .\aria2.session
```

把这个路径粘贴到 `input-file=`, `save-session=` 这两行配置的等号右边.

+ 默认下载目录: 改成你自己经常用的下载目录, 如`C:/Users/xxx/Downloads`.
请使用绝对路径, 路径前后不要加引号, 加引号 `aria2` 会报错.
+ 开启一些`BT`设置,配置文件有详细说明:

``` bash
enable-dht=true
bt-enable-lpd=true
enable-peer-exchange=true
```

+ 添加 `BT rackers`, 可以改善种子下载速度.
`BT rackers`可以在[全网热门BTTracker 列表](https://github.com/XIU2/TrackersListCollection)获取.

浏览器按 `Ctrl+F` 搜索 `Aria2 format`, 点击展开,
复制 `BEST Tracker list:https://trackerslist.com/best_aria2.txt` 里面的内容,
粘贴到 `bt-tracker=` 后面.
不过也可以先不管这一步, 后面在图形界面修改更方便.

+ 在`aria2`的文件夹, 在 powershell 窗口输入 `.\aria2c.exe` 即可运行 `aria2` 程序.
如果配置有错误会有提示, 根据提示查找修改错误就可以了,很简单.
如果不想看输出信息, 可以用 `Start-Job -ScriptBlock {./aria2c.exe}`运行`aria2`,
使用` Get-Job`查看运行状态.

+ 使用 `Stop-Job *; Remove-Job *;` 关闭所有后台任务.
(这样关闭可能会导致`aria2`来不及保存, 还是在网页端`aria2状态`页面点击`关闭aria2`吧)

### 添加环境变量

如果觉得的每次都这样比较麻烦的话, 可以考虑把`aria2c.exe`加入环境变量.
这样运行的时候在命令行输入 `aria2c.exe &` 即可.
修改方法: 按下`win+s`打开搜索窗口,输入`环境变量`,
依次点击`编辑环境变量`--`环境变量`--`xxx的用户变量`--`Path`--`新建`--`浏览`,
选中`aria2`的存放目录,添加好之后,一路点击确定即可.

如果你嫌麻烦,也可以使用下文的`powershell`命令.
参考[命令行输出和添加系统环境变量](https://blog.csdn.net/amoscn/article/details/108654236)

```powershell
$mypath='你的路径'; # 这里修改成你的 aria2 的文件夹.
echo "查看现在的路径`n---------`n";
$target='User';
$path=[Environment]::GetEnvironmentVariable('Path', $target); $path -split ';'
echo "查看修改后的路径`n---------`n";
$newPath=$path+';'+$mypath;$newPath -split ';'

# 先不要运行下面的命令, 先检查上面的命令确认无误
[Environment]::SetEnvironmentVariable("Path",$newPath,$target)
```

+ 最后回到浏览器, 查看或者再次打开之前的`index.html`文件,
一切顺利的话,会看到左边`Aria2状态:已连接`.

+ 如果刚才没有设置好`BT-Tracker`的话,
现在可以在`Aria2设置`--`BitTorrent设置`--`BT服务器设置`中修改.其他设置类似.

## 配置备份

我使用的配置如下:

```bash
# '#'开头为注释内容, 选项都有相应的注释说明, 根据需要修改
# 被注释的选项填写的是默认值, 建议在需要修改时再取消注释
# 文件保存相关
# 文件的保存路径(可使用绝对路径或相对路径), 默认: 当前启动位置
dir=C:/Users/qingz/Downloads
# 启用磁盘缓存, 0为禁用缓存, 需1.16以上版本, 默认:16M
#disk-cache=32M
# 文件预分配方式, 能有效降低磁盘碎片, 默认:prealloc
# 预分配所需时间: none < falloc ? trunc < prealloc
# falloc和trunc则需要文件系统和内核支持
# NTFS(windows)建议使用 falloc, EXT3/4(linux)建议trunc, MAC 下需要注释此项
file-allocation=falloc
# 断点续传
continue=true
## 下载连接相关
# 最大同时下载任务数, 运行时可修改, 默认:5
max-concurrent-downloads=20
# 同一服务器连接数, 添加时可指定, 默认:1
max-connection-per-server=5
# 最小文件分片大小, 添加时可指定, 取值范围1M -1024M, 默认:20M
# 假定size=10M, 文件为20MiB 则使用两个来源下载; 文件为15MiB 则使用一个来源下载
min-split-size=10M
# 单个任务最大线程数, 添加时可指定, 默认:5
#split=5
# 整体下载速度限制, 运行时可修改, 默认:0
#max-overall-download-limit=0
# 单个任务下载速度限制, 默认:0
#max-download-limit=0
# 整体上传速度限制, 运行时可修改, 默认:0
#max-overall-upload-limit=0
# 单个任务上传速度限制, 默认:0
#max-upload-limit=0
# 禁用IPv6, 默认:false
#disable-ipv6=true
# 连接超时时间, 默认:60
#timeout=60
# 最大重试次数, 设置为0表示不限制重试次数, 默认:5
#max-tries=5
# 设置重试等待的秒数, 默认:0
#retry-wait=0
## 进度保存相关 ##
# 从会话文件中读取下载任务
input-file=aria2.session
# 在Aria2退出时保存`错误/未完成`的下载任务到会话文件
save-session=aria2.session
# 定时保存会话, 0为退出时才保存, 需1.16.1以上版本, 默认:0
save-session-interval=100
## RPC相关设置 ##
# 启用RPC, 默认:false
enable-rpc=true
# 允许所有来源, 默认:false
rpc-allow-origin-all=true
# 允许非外部访问, 默认:false
rpc-listen-all=true
# 事件轮询方式, 取值:[epoll, kqueue, port, poll, select], 不同系统默认值不同
#event-poll=select
# RPC监听端口, 可修改, 默认:6800; AriaNg设置->右边->RPC地址
rpc-listen-port=6800
# 设置的RPC授权令牌, v1.18.4新增功能, 取代 --rpc-user 和 --rpc-passwd 选项, 新手可以先不管 网页端需要对应; AriaNg设置->右边->RPC密钥
rpc-secret=abcd
# 设置的RPC访问用户名, 此选项新版已废弃, 建议改用 --rpc-secret 选项
#rpc-user=<USER>
# 设置的RPC访问密码, 此选项新版已废弃, 建议改用 --rpc-secret 选项
#rpc-passwd=<PASSWD>
# 是否启用 RPC 服务的 SSL/TLS 加密; 启用加密后RPC服务, 需要使用https或者wss协议连接; AriaNg设置->右边->RPC协议
# rpc-secure=true
# 在 RPC 服务中启用 SSL/TLS 加密时的证书文件,
# 使用 PEM 格式时,您必须通过 --rpc-private-key 指定私钥
#rpc-certificate=/path/to/certificate.pem
# 在 RPC 服务中启用 SSL/TLS 加密时的私钥文件
#rpc-private-key=/path/to/certificate.key
## BT/PT下载相关 ##
# 当下载的是一个种子(以.torrent结尾)时, 自动开始BT任务, 默认:true
follow-torrent=true
# BT监听端口, 当端口被屏蔽时使用, 默认:6881-6999
listen-port=51413
# 单个种子最大连接数, 默认:55
#bt-max-peers=55
# 打开DHT功能, 如果是PT, 比如6v,蒲公英等等,需要禁用, 默认:true
enable-dht=true
# 打开IPv6 DHT功能,PT需要禁用
#enable-dht6=false
# DHT网络监听端口, 默认:6881-6999
#dht-listen-port=6881-6999
# 本地节点查找, PT需要禁用, 默认:false
bt-enable-lpd=true
# 种子交换, PT需要禁用, 默认:true
enable-peer-exchange=true
# 每个种子限速, 对少种的PT很有用, 默认:50K
#bt-request-peer-speed-limit=50K
# 客户端伪装, PT需要
peer-id-prefix=-TR2770-
user-agent=Transmission/2.77
peer-agent=Transmission/2.77
# 当种子的分享率达到这个数时, 自动停止做种, 0为一直做种, 默认:1.0
seed-ratio=0
# 强制保存会话, 即使任务已经完成, 默认:false
# 较新的版本开启后会在任务完成后依然保留.aria2文件
# force-save=true
# BT校验相关, 默认:true
# bt-hash-check-seed=true
# 继续之前的BT任务时, 无需再次校验, 默认:false
bt-seed-unverified=true
# 保存磁力链接元数据为种子文件(.torrent文件), 默认:false
bt-save-metadata=true
# bt-tracker 链接, 记得修改成最新的, 粘贴到等号后面. search Aria2
# https://github.com/XIU2/TrackersListCollection
```
