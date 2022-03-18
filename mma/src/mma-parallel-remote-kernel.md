# MMA远程内核

`远程内核`用于在 `异于主Wolfram语言所在计算机` 上运行 `并行worker`s.
它依赖于使用远程 shell 调用技术来启动, 通常更难配置和维护.

远程内核连接方法的配置是通过 `并行首选项` 完成的.
配置面板应该类似于下面的样子.
在这个配置中, 两台远程机器被配置为各自提供四个并行内核.

在Windows上, 默认设置是使用 `rsh` 来启动远程机器上的内核; 其他平台使用 `ssh`.
这是因为 `ssh` 通常不适用于 `Windows`.
当然, 任何机器都必须被配置为允许从 `主机器上` 进行 `远程shell调用`.

你可以修改用于启动内核的启动命令.
在命令中, 你可以使用一些 `参数`, 这些参数在命令实际使用之前, 会被 `填充`.
(也就是字符串模板, `StringTemplate`)
例如, 参数 `` `1 `` 是用来指正在使用的远程机器的名称.

```mathematica
`1` ;   远程机器的名称
`2` ;   在本地机器上创建的链接的名称(当远程机器正在连接时
`3` ;   执行远程 shell 的用户名称
`4` ;   用于 WSTP 连接的 链接协议, 详见 LinkProtocol 的帮助.
```

+ 用户名称的默认值是使用 `$Username`, 即运行 Wolfram Language 本地拷贝的用户.

## Linux 上默认的远程内核命令

可通过查看 MMA 默认变量 `$RemoteCommand` 的值获取.

```bash
ssh -x -f -l `3` `1` wolfram -wstp -linkmode Connect `4` -linkname '`2`' -subkernel -noinit -pacletreadonly
```

参数例如: {'myLinux', 'mmaR', 'tom', 'TCPIP'}

## Windows 默认的远程内核命令

```bash
rsh `1` -n -l `3` "wolfram -wstp -linkmode Connect `4` -linkname `2` -subkernel -noinit -nopaclet >& /dev/null &"
```

## LinkProtocol

`LinkProtocol` 是 `LinkLaunch`, `Install` 和相关函数的 `选项`,
它指定了新的 `WSTP链接` 要使用的 `底层数据传输协议`.

+ `LinkProtocol` 的可能设置是 `"SharedMemory"` 和 `"TCPIP"`.
+ 在 `Unix` 和 `Macintosh` 上, 也支持 `"Pipes"`;
+ 在 `32位Windows` 系统上, 支持 `"FileMap"`.
+ 也支持其他传统的设置, 如 `"TCP"`.

## 手动启动

如果你想手动启动内核, 避免使用配置机制, 你可以直接向 `LaunchKernels` 传递参数.
如果您是直接从命令行以批处理模式运行 Wolfram Language, 这可能很有用.

+ `LaunchKernels[RemoteMachine[host]]`  ;在机器主机上启动一个远程内核
+ `LaunchKernels[RemoteMachine[host,num]]`  ;在机器主机上启动数个远程内核.
+ `LaunchKernels[RemoteMachine[host,command,num]]`; 使用 `command` 作为远程命令在机器 `host` 上启动`n`个远程内核.

远程内核连接方法支持 `LaunchKernels` 直接启动远程内核.
要做到这一点, 你必须首先加载 `` RemoteKernels` `` 包, 如下所示.

```mathematica
Needs["SubKernels`RemoteKernels`"]
```

为了启动, 你需要向 `RemoteMachine` 传递参数.
下面演示如何在地址为 `remote1.wolfram.com` 的机器上启动 `远程内核`,
可以把此地址替换成你的远程电脑的 `IP`.

```mathematica
LaunchKernels[RemoteMachine["remote1.wolfram.com"]]
OutPut[2]= KernelObject[1, "remote1.wolfram.com"]
```

通常需要自定义用于启动 `远程内核` 的命令.

默认命令是由 `$RemoteCommand` 设置的, 它的典型设置如下.
注意, `rsh` 只在 `Windows` 上使用, 在其他平台上使用 `ssh`.

```mathematica
$RemoteCommand

Out[19]= "ssh -x -f -l `3` `1` wolfram -wstp -linkmode Connect `4` -linkname '`2`' -subkernel -noinit -pacletreadonly"
```

注意, `远程命令` 的参数在上一节中已经描述过了.
`用户名` 参数是由 `$RemoteUsername` 设置的, 它默认设置为 `$Username`.

```mathematica
$RemoteUsername
Out[18]= "user"
```

你可以将命令直接传入 `RemoteMachine`.
下面的内容适合于 Wolfram System 的非标准安装.

```mathematica
LaunchKernels[RemoteMachine[
  "remote1.wolfram.com",
  "rsh `1` -n -l `3` \"D:\\Mathematica\\math -mathlink -linkmode Connect `4` -linkname `2` -subkernel -noinit >& /dev/null &\"" , 4]]

Out[5]=KernelObject[2, "remote1.wolfram.com"]
```

另一种方法是在启动任何内核之前修改 `$RemoteKernel` 的值.

一旦你启动了并行内核, 那么就可以使用 Wolfram 语言的所有并行功能.
