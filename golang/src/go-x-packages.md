# golang packages

## qrcp

[claudiodangelis/qrcp](https://github.com/claudiodangelis/qrcp)
[使用 qrcp 在你的手机和 Linux 之间传输文件 ](https://linux.cn/article-13999-1.html)

Quick Response Code

### 安装

用 `go` 安装最新开发版:

```bash
go get github.com/claudiodangelis/qrcp
```

### 设置 qrcp

你可以通过使用 `--help` 选项查看所有可用的 `qrcp` 选项:

```bash
$ qrcp --help
$ ./qrcp --help
```

默认配置文件位于 `~/.config/qrcp/config.json`, 你可以使用你喜欢的编辑器编辑, 或从命令行调用配置向导来配置应用.

```bash
$ qrcp config
```

第一步是创建一个配置文件. `qrcp config` 命令将带你完成这个过程, 但会问你几个问题.

+ 第一个问题是要求你提供一个"完全限定域名".
    如果你在一个不使用完全限定域名的本地网络上使用 `qrcp`(或者你不知道哪种方式), 那么就把这个留空.
    `qrcp` 命令将使用你的本地 IP 地址代替.
+ 下一个问题是提示你选择端口. 大多数防火墙会阻止非标准的端口, 但会将 `8080` 端口作为互联网流量的情况并不少见.
    如果你的防火墙屏蔽了 `8080` 端口, 那么你还是要添加一个例外.
    假设你的系统使用 `firewalld`, 你可以用这个命令允许 `8080` 端口的流量:

    ```bash
    $ sudo firewall-cmd --add-port 8080/tcp --permanent
    ```

+ 拒绝在 `传输完成后保持网络连接` 的选项, 让 qrcp 生成一个随机路径.
+ 假设你在一个可信的网络上, 使用 HTTP(而不是 HTTPS)连接, 那么你不必配置 TLS.
+ 配置保存在 `~/.config/qrcp/config.json` 中, 并且之后可以编辑, 所以如果你想改变设置, 它很容易更新.

+ 用 qrcp 传输文件

现在你已经准备好从你的 `Linux` 电脑向你的移动设备发送一个文件.
在这个例子中, 我使用了我的 `iPhone`, 它完全不支持 `Linux`, 这是臭名昭著的.
这个过程在安卓设备上是完全一样的.

使用 `send` 子命令将文件从 Linux 电脑发送到我的手机:

```bash
$ qrcp send example.txt
```

打开相机应用, `iPhone` 扫描二维码并启动 `Safari` 浏览器. 最后, 点击"下载"按钮.

![下载电脑上的文件](https://img.linux.net.cn/data/attachment/album/202111/19/114133xdpd6iad56sairah.png)

+ 用 `qrcp` 接收文件

接收文件也一样简单, 只是命令略有不同:

```bash
$ qrcp receive
```

扫描二维码, 并选择要发送的文件:

![选择要传输的文件](https://img.linux.net.cn/data/attachment/album/202111/19/114133zll8zq77s5sosubn.jpg)

文件将存放在, 配置中指定的默认位置.

### README

+ 当一次发送多个文件时, qrcp会创建一个你要传输的文件或文件夹的压缩包, 并在传输完成后删除该压缩包.

```bash
# 多个文件
qrcp MyDocument.pdf IMG0001.jpg
# 一个完整的文件夹
qrcp Documents/
```

+ 你可以选择在传输前压缩一个文件.

```bash
qrcp --zip LongVideo.avi
```

+ 接收文件

接收文件时, `qrcp` 会提供一个 "上传页面", 你可以通过它选择手机中的文件.
接收文件到一个特定的目录

```bash
# 注意: 该文件夹必须存在
qrcp receive --output=/tmp/dir
```

+ 选项

`qrcp` 工作时不需要任何事先配置, 但是, 你可以选择配置使用特定的值.
config命令可以启动一个向导, 让你配置接口, 端口, 全称域名和保持存活等参数.

```bash
qrcp config
```

注意: 如果某些网络接口没有显示出来, 使用 `--list-all-interfaces` 标志来抑制接口的过滤.

```bash
qrcp --list-all-interfaces config
```

### 配置文件

默认的配置文件存储在 `$XDG_CONFIG_HOME/qrcp/config.json`, 但是, 你可以通过 `--config` 标志来指定配置文件的位置.

```bash
qrcp --config /tmp/qrcp.json MyDocument.pdf
```

+ 端口

默认情况下, `qrcp` 监听的是一个随机端口. 传递 `--port`(或-p)标志来选择一个特定的端口.

```bash
qrcp --port 8080 MyDocument.pdf
```

+ 网络接口

qrcp将尝试自动找到合适的网络接口用于传输. 如果发现有多个合适的接口, 它会要求你选择一个.

如果你想使用一个特定的接口, 请传递 `--interface`(或`-i`)标志.

```bash
# webserver将被
# tun0的接口网络上的所有计算机都能看到
qrcp -i tun0 MyDocument.dpf
```

你也可以使用一个特殊的接口名称, 即 `any`,
它将网络服务器绑定在 `0.0.0.0`上, 使得任何网络上的人都能看到网络服务器, 甚至是从外部网络上看到.

当你想从你的 Amazon EC2, Digital Ocean Droplet, Google Cloud Platform Compute Instance 或任何其他VPS传输文件时, 这很有用.

```bash
qrcp -i any MyDocument.pdf
```

+ 网址

`qrcp` 对 `URL` 使用两种模式.

+ 发送: `http://{ip address}:{port}/send/{random path}`
+ 接收: `http://{ip address}:{port}/receive/{random path}`

有几个选项可以覆盖这些模式.

传递 `--path` 标志以使用特定的URL路径, 例如.

```bash
# 产生的URL将是
# http://{ip address}:{port}/send/x
qrcp --path=x MyDocument.pdf
```

通过 `--fqdn` (或 `-d`) 来使用完全合格的域名而不是`IP`.
这在与 `-i any` 结合使用时非常有用, 如果你从远程位置使用它.

```bash
# 产生的URL将是
# http://example.com:8080/send/xYz9
qrcp --fqdn example.com -i any -p 8080 MyRemoteDocument.pdf
```

+ HTTPS

`qrcp` 支持 HTTPS 的安全文件传输. 要启用安全传输, 你需要一个TLS证书和相关的密钥.

你可以在 `qrcp config` 向导中选择TLS证书和密钥的路径, 或者, 如果你愿意, 你可以通过 `--tls-cert`和 `--tls-key`.

```bash
qrcp --tls-cert /path/to/cert.pem --tls-key /path/to/cert.key MyDocument
```

也有一个 `--secure` 标志, 你可以用它来覆盖默认值.

### 默认的输出目录

在浏览器中打开; 如果你需要在终端之外打印QR, 你可以通过`--browser`标志.
有了这个标志, `qrcp` 仍然会将QR码打印到终端, 但它也会打开一个默认浏览器的新窗口来显示QR码.

```bash
qrcp --browser MyDocument.pdf
```

+ 保持服务器存活

在传输文件后保持服务器存活可能很有用, 例如, 当你想把同一个文件传输到多个设备上时.
你可以使用 `--keep-alive` 标志来做到这一点.

```bash
# 服务器不会自动关闭
# 在第一次传输后
qrcp --keep-alive MyDocument.pdf
```

### shell 补全脚本

qrcp带有一个内置的`completion`命令, 可以生成shell完成脚本.

+ Bash

    ```bash
    $ source <(qrcp completion bash)
    ```

    要在每个会话中加载补全, 请执行一次.

    + Linux:

    ```bash
    $ qrcp completion bash > /etc/bash_completion.d/qrcp
    ```

    注意: 如果你不想在全系统范围内安装补全脚本,
    请参考 [Bash补全FAQ](https://github.com/scop/bash-completion/blob/master/README.md).

    + MacOS:

    ```bash
    $ qrcp completion bash > /usr/local/etc/bash_completion.d/qrcp
    ```

+ Zsh:

    如果你的环境中还没有启用 `shell 补全`, 你需要启用它. 你可以执行一次下面的操作.

    ```bash
    $ echo "autoload -U compinit; compinit" >> ~/.zshrc
    ```

    要在每个会话中加载补全, 请执行一次.

    ```bash
    $ qrcp completion zsh > "${fpath[1]}/_qrcp"
    ```

    你需要启动一个新的shell来使这个设置生效.

+ Fish:

    ```bash
    $ qrcp completion fish | source
    ```

    要在每个会话中加载补全, 请执行一次.

    ```bash
    $ qrcp completion fish > ~/.config/fish/completions/qrcp.fish
    ```
