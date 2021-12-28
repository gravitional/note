# Wolframscripts

## 命令行二进制程序

```bash
file -L `which wolframscript`
wolframscript --help
```

`--help` 的输出是:

### 选项:

+ `-h, -help` ; 打印帮助文本.
+ `-c, -code WL`; 给出要执行的 Wolfram Language 代码.
+ `-f, -file PATH`; 给出要执行的包含 Wolfram 语言代码的 `文件`.
+ `-a, -api URL|UUID`; 使用指定 `URL` 的 `API`, 或来自指定 `UUID` 的云或本地对象. 在'-args'之后, 以 `key=value` 的形式提供参数.

+ `-fun, -function WL`; 使用函数, 参数为使用 `-args` 给出的 `字符串`
并将参数解析为 `-signature` 给出的类型.
+ `-o, -cloud [CLOUDBASE]`; 在 `云` 中执行代码, 使用指定的 `cloud base`.
默认情况下, 云基地是 [https://wolframcloud.com](https://wolframcloud.com)
+ `-l, -local [KERNELPATH]`; 在本地执行代码, 使用指定路径的 Wolfram Engine kernel.
默认情况下, `KernelPath` 使用本地系统中最新版本的Wolfram 语言
+ `--, -args ARGS...`; 与 `-api` 或 `-function` 一起使用, 以提供参数.
+ `-s, -signature TYPE...`; 与 `-function` 和 `-args` 一起使用, 为提供的参数指定解释器类型.

+ `-v, -verbose`; 在执行过程中打印附加信息.

+ `-activate [KEY]`; 通过 `云端` 或钥匙激活 Wolfram Engine.
+ `-entitlement ID`;  使用按需许可, 用给定的许可权限 ID 激活Wolfram Engine.
+ `-authenticate [ID PASS]`; 通过 `云端` 认证, 指定特定的 Wolfram ID 和密码, 如果没有给出, 则提示. 可以为不同的云指定不同的认证.
+ `-disconnect` 断开与云的连接, 删除认证信息.
+ `-configure [KEY=VAL...]`;  通过指定特定 `配置变量键` 的值来配置WolframScript. 如果没有给出 `keys`, 这将打印出当前的配置.
+ `-version`; 打印 WolframScript 的版本.
+ `-username USERNAME` 给出用于认证的用户名.
+ `-password PASSWORD`; 给出用于认证的密码.
+ `-permissionskey KEY`; 给出`权限密钥`, 用于授权访问云端部署的API.
+ `-k, -noverifypeer`; 在与云端互动时禁用对等的证书验证(peer certificate verification).
+ `-timeout SECONDS [VALUE]` 指定允许执行的`秒数`. 如果超过时间, 则返回`VALUE`.

+ `-charset ENCODING`; 使用`ENCODING`进行输出.
编码可以是 `None`, 用于输出原始字节,
或是 `$CharacterEncodings` 中的任何条目, 除了 `Unicode`.
默认情况继承终端的 `语言设置` 中的值.
+ `-format TYPE`; 指定输出的格式. 可以使用 `Export` 所理解的任何格式.

+ `-print [all]`; 当运行 `脚本` 时, 打印脚本 `最后一行` 的结果. 如果给了 `all` 参数, 则打印 `每一行`.
+ `-linewise`; 执行读取到的 `标准输入` 中的每行代码.
+ `-script ARGS...`; 与 `wolfram -script` 相对应, 另外设置了 `$ScriptCommandLine`.

常用的调用方式为:

```bash
wolframscript -script ./test.wl --abs 1 --box  3 apple banana
```

注意 `$ScriptCommandLine` 中的参数, 类型均为 `String`.
也就是命令行接收到的参数, 会被强制转码为 `字符串`. 所以上面收到的参数列表为:

```mathematica
{"./test.wl", "--abs", "1", "--box", "3", "apple", "banana"}
```

如果需要使用 `数值` 或其他类型的值, 需要在脚本内部自行 `ToExpression`.

## OS covery

总的来说,

windows: 建立后缀名为 `.wl`/`.wls` 的文件, 然后按正常的方法去写 `mma` 笔记本,
运行的时候用`wolframscript.exe`
用 `-print all` 指定输出所有没被`;` 抑制输出的表达式.
用 `para1 para2 ...` 传递参数.

参数用下面的变量调用

+ `$CommandLine` -- 一系列字符串, 给出使用的 `完整的` 命令行.
+ `$ScriptCommandLine` -- `字符串` 的列表, 给出了调用 `独立 Wolfram System 脚本` 时, 使用的命令行参数. 这些参数出现在 `-option` 给出的选项之后.
+ `$ScriptInputString` -- 通过 `标准输入` 给出脚本输入的字符串. 在脚本的每次迭代中, 选项 `-linewise` 用一行标准输入载入该变量.

```powershell
wolframscript.exe -print all -file .\test.wl para1 para2
```

***
`unix`: 通过加上`#!/usr/bin/env wolframscript -print all`,运行的时候, 不用输入`wolframscript`,
传递参数的方法不变.

```bash
./test.wl para1 para2
wolframscript -file ./init.wl &> ~/test/log.txt & # 在后台运行, 把输出重定向到日志文件 log.txt
```

经常用到的`mma`系统变量,参考

`guide/SystemInformation` : mma 系统信息
`guide/WolframSystemSetup`: 更一般的系统设置

`$InputFileName`: 脚本的绝对路径.
`$Notebooks`: 如果是用前端运行的, 则为`True`.
`$BatchInput`: 输入是否来自批处理
`$BatchOutput`:如果在命令行中输出, 则为`True`.
`$CommandLine`: 唤醒环境变量所使用的命令行,
`$ProcessID`:进程ID
`$ParentProcessID`:
`$Username`: 用户的登陆名
`Environment["var"]`:操作系统的环境变量, 如`Environment["HOME"]`

## 脚本文件

Wolfram 语言脚本是一个包含 Wolfram 语言命令的文件, 用户通常可以在一个 Wolfram 语言会话中按顺序计算这些命令.
如果这些命令需要多次重复, 编写一个脚本就是非常有用的. 把这些命令收集在一起确保它们按照特定的顺序计算, 并且没有忽略任何命令. 如果你运行复杂的, 很长的计算, 这么做是很重要的.

当您交互式地使用 Wolfram 语言时, 包含在脚本文件中的命令可以利用 `Get` 计算.
这个函数也可以通过编程在代码或者其它 `.wl` 文件中使用.

***
从一个脚本文件读取命令.

+ `Get["file"]` ; 读入一个文件, 并且运行其中的命令
+ `<<file`; `Get` 的简写形式

对脚本文件的结构没有任何要求. 在该文件中给出的任何 Wolfram 语言命令序列都会按照顺序读入并计算.
如果你的代码比一个普通命令列表更复杂, 你可能可以考虑编写一个更为结构化的程序包, 如"建立 Wolfram 语言程序包"中所述.

当我们不需要用一个交互式的会话时, 即你的脚本封装了需要执行的简单计算时, Wolfram 语言脚本就更有用了.
例如, 你的计算涉及大量计算任务, 如线性代数, 最优化, 数值积分或微分方程的解, 并且当你不使用排版功能, 动态交互或笔记本的时候.

脚本可以存储在一般的 `.wl` 程序包文件或专门的 `.wls` 脚本文件.
两种文件的概念是一样的: Wolfram 语言表达式系列, 起始处带有可选的 "shebang" 行一般用于类 Unix 操作系统(参见 Unix 脚本可执行文件).

文件类型中的唯一不同是它们双击的行为.
双击程序包文件会在笔记本程序包编辑器中打开文件;
双击脚本文件会执行文件, 如果操作系统支持的话.
脚本文件可以在笔记本界面上编辑, 但是必须使用 `文件--打开` 进行开启.

## 在 Local 内核中运行脚本

当从命令行调用 Wolfram 语言内核时, 可以使用脚本文件,  对于内核可执行文件, 一般使用以下位置.

运行 Windows 上的脚本文件.

```mathematica
"%ProgramFiles%\Wolfram Research\Mathematica\12.0\wolfram" -script file.wl
```

```mathematica
D:\mathematica\wolfram -script file.wl
```

在 Mac 上运行脚本文件.

```mathematica
/Applications/Mathematica.app/Contents/MacOS/wolfram -script file.wl
```

在 Linux 上运行脚本.

```mathematica
wolfram -script file.wl
```

`-script` 命令行选项指定 Wolfram 语言内核在一个特殊的脚本上运行, 或者以批处理模式运行.
在这种模式下, 内核读入指定的文件, 并且按顺序计算命令. 通过把输出函数的 `PageWidth` 选项设为 `Infinity` , 内核关闭默认的换行功能, 并且不显示 `In[]` 和 `Out[]` 标签.
当在该模式下运行时, 标准的输入和输出通道 `stdin`, `stdout` 和 `stderr` 不会被重定向, 数值按 `InputForm` 进行格式化.

运行带有 `-script` 选项的 wolfram 等价于利用 `Get` 命令读入文件, 唯一的不同之处是: **在计算文件中最后一个命令之后, 内核停止运行**.
这种行为可能会影响 `Wolfram Symbolic Transfer Protocol (WSTP)` 链接或者通过运行脚本创建的外部进程.

## 使用 WolframScript 运行脚本

脚本可以使用如下的 WolframScript 诠释器进行运行. `-file` 标志是可选的.

使用 WolframScript 运行脚本.

```mathematica
wolframscript -file file.wl
```

wolframscript 已经加入环境变量

WolframScript 会找到最佳的本地内核运行脚本.
如果没有找到任何本地内核, 它会连接至云端并在那里运行脚本.
程序接受任何标志以便控制使用本地还是云端的进行计算.
它还设置 **脚本参数**, 其允许脚本基于启动的形式或收到的输入来改变行为.
使用 WolframScript 的另一个好处是, 输入和输出是完全缓存的, 允许应用各种变换.
在 `WolframScript` 页面(`ref/program/wolframscript`)有对这些额外选项以及范例的详细说明.

在 Windows 和 Linux 上,  `WolframScript` 一般与 Wolfram 系统一起安装.
在 `Mac` 上, 有必要安装与 Wolfram 系统一起绑定的 "Extras" 安装器以便获取 `WolframScript`.
默认情况下, 这些安装器会把 wolframscript 放在 `PATH` 中.

## Unix 脚本可执行文件

类 `Unix` 的操作系统--以及 Windows 中的 `Unix` 环境, 例如 `cygwin` 和 `MinGW` 允许编写可执行的脚本文件, 和普通的可执行程序那样运行.
这可以通过在文件开头放上一个"解释器"行实现.
对于包含 Wolfram 语言命令的脚本也一样.

"解释器"行包括两个字符 `#!`, 它必须是文件中的首两个字符, 然后是执行文件的绝对路径以及其他参数.
为了达到跨平台和机器的最大的兼容性, 建议通过如下所示的帮助器 `/usr/bin/env` 启动WolframScript.
`env` 程序会正确找到 `PATH` 中的 `wolframscript` 并启动之.

***
一个脚本文件的范例.

```mathematica
#!/usr/bin/env wolframscript

(* generate high-precision samples of a mixed distribution *)
Print /@ RandomVariate[MixtureDistribution[
    {1,2},
    {NormalDistribution[1,2/10],
     NormalDistribution[3,1/10]}],
    10,  WorkingPrecision -> 50]
```

如果要把脚本编译成可执行文件, 你需要设置可执行权限.
之后, 就可以通过在一个 shell 提示输入脚本名称, 运行脚本.
***
使脚本可执行, 并且运行它.

```mathematica
chmod a+x script.wls
./script.wls
```

解释器行可能另外包含解释器的其他参数. 可能的参数在 WolframScript 页(`ref/program/wolframscript`)被指定.

***
使用额外参数的解释器行.

```mathematica
#!/usr/bin/env wolframscript -linewise -format XML
```

Wolfram 语言脚本无需含有 `.wl` 或 `.wls` 扩展名.
一个可执行脚本等价于在一个 Unix 操作系统中的任何其它程序的一个全功能程序, 所以它可用于其它脚本中, 在管道中, 根据任务控制的方运行等等.
每个 Wolfram 语言脚本启动自己的 `WolframKernel` 拷贝, 并且不共享变量或者定义.
注意: 同步运行 Wolfram 语言脚本可能受同时运行的内核数上的许可证限制所影响.

在一个交互式的 Wolfram 语言会话中, 可以显式地读入可执行脚本文件并且对其进行计算.
如果第一行以 `#!` 字符开始的话, `Get` 命令会正常忽略脚本的第一行.

可以避免使用 `env` 程序, 但是到 `wolframscript` 的路径必须是绝对路径.
启动脚本的操作系统机制没有使用 `PATH` 或其他方法找到文件. 而且, 到诠释器的路径不能包含空格.

## Windows 上的脚本

独立的脚本也可以用在 `Windows` 上.
不像 `Unix` 类的操作系统, 这些脚本必须含有扩展名 `.wls` 以便被识别为 `Wolfram` 语言脚本.
它们可以通过双击从 Windows 浏览器中启动, 以及在命令提示符中敲入名称启动.
Unix 诠释器行, 如果出现的话, 会被机制忽略.

***
从命令提示符中启动脚本, 等价于双击.

```mathematica
> file.wls
```

在命令提示符中, 其他参数加在文件名后传递.
WolframScript 本身看不见这些参数, 但是会以参数形式传递给脚本, 下一章节会详细描述.

***
从命令提示符中启动带有两个额外参数的脚本.

```mathematica
> file.wls arg1 arg2
```

## 脚本参数

当运行一个 Wolfram 语言脚本时, 你可能常常想要通过指定命令行参数, 修改脚本行为.
Wolfram 语言代码可以通过 `$ScriptCommandLine` 访问传递给 Wolfram 语言脚本的参数.
另外, 标准输入的内容可被用为变量 `$ScriptInputString` 中的字符串.

***
变量, 给出关于脚本如何运行的信息.

+ `$ScriptCommandLine`    启用脚本的命令行
+ `$ScriptInputString`    给予脚本的标准输入的内容

***
利用一个命令行参数的脚本文件 `file.wls` 的一个范例.

```mathematica
#!/usr/bin/env wolframscript

(* generate "num" samples of a mixed distribution *)
num  = ToExpression[$ScriptCommandLine[[2]]];
Print /@ RandomVariate[
  MixtureDistribution[
    {1, 2},
    {NormalDistribution[1, 0.2],
     NormalDistribution[3, 0.1]}
  ], num,  WorkingPrecision -> 50]
```

***
运行脚本, 并且指定在 Unix 环境下的样本数.

```mathematica
./file.wls 10
```

***
运行脚本, 并且指定 Windows 中的样本数.

```mathematica
> file.wls 10
```

当在脚本中访问时, `$ScriptCommandLine` 是以脚本名称为第一个元素的一个列表, 列表的其余元素为命令行变量.
`$ScriptCommandLine` 遵循标准的 `argv[]` 习惯.
注意这完全隐藏了到解释器的路径或 `#!` 行传递的任何参数.

因为类 Unix 操作系统执行脚本的方式, `$ScriptCommandLine` 仅仅当 Wolfram 语言内核经过 `wolframscript` 调用时才设置为非空列表.

如果该脚本应该以批处理模式和标准 `Unix` 脚本模式运行, `$ScriptCommandLine` 可以用于决定当前执行模式.
那么, `$ScriptCommandLine` 和 `$CommandLine` 都应该用于访问命令行参数.

## 标准输入, 标准输出

[什么是标准输入, 标准输出(stdin, stdout)](https://blog.csdn.net/sinat_17700695/article/details/91491472)

***
要弄清什么是标准输入输出. 首先需要弄懂什么是IO.

`IO` 的 `I` 是 `Input` 的意思, `O` 是 `output` 的意思.
意味着输入和输出.

更确切的含义是:

+ `I`: 从外部设备输入到内存
+ `O`: 从内存输出到外部设备

而标准输入和标准输出是干什么的?它们是用于 `IO` 的.
那么它们属于 `IO` 的哪个部分?
内存?还是外部设备?

答案显然是外部设备(逻辑上的外部设备, 为什么?接着看).

***
更具体的含义?

在 linux 操作系统中, 外部设备用什么表示?是用文件.
linux 中一切设备皆是文件!
因此标准输入和输出更具体的含义是文件.

它们是哪两个文件?
它们是 `/dev/stdin` 这个文件和 `/dev/stdout` 这个文件.
也就是说所谓的标准输入和标准输出其实就是两个 linux 下的文件.

***
linux 的文件类型有:

1. 普通文件
1. 字符设备文件
1. 块设备文件
1. 目录文件
1. 链接文件
1. 管道文件
1. 套接字文件

思考一下?它们是什么文件?它们在 `/dev` 目录下, 它们是设备文件吗?

那么所谓的从标准输入读是什么意思?
逻辑上来看:
就是打开 `/dev/stdin` 这个文件, 然后把这个文件里的内容读进来.
输出到标准输出是什么意思?
逻辑上来看:
就是打开 `/dev/stdout` 这个文件, 然后把内容输出到这个文件里去.

为什么是从逻辑上来看?因为它们不是设备文件,所以它们不代表一个设备.
linux里一切皆是文件, 设备是文件, 但是文件不一定是设备!

***
那它们是什么文件?他们是`链接文件`. (可以用` ls -l /dev` 来查看 `l` 开头的就是链接文件. )

什么是链接文件?**文件内容是另一个文件的地址的文件称为链接文件**.

因此, 打开, 读或者写 `/dev/stdin` 和` /dev/stdout` 实际上是打开, 读或者写这两个文件中存放的地址对应的设备文件.

## WolframScript

ref/program/wolframscript
***
名称
wolframscript -- Wolfram 语言命令行脚本解释器

### 概要

+ `wolframscript -code code [-cloud [cloudbase] | -local [kernelpath]] arg1 ...`
+ `wolframscript -file file|url [-cloud [cloudbase] | -local [kernelpath]] [Subscript[arg, 1] ...]`
+ `wolframscript -api url|uuid|file [-cloud [cloudbase] | -local [kernelpath]] [-args key=value ...]`
+ `wolframscript -function code [-cloud [cloudbase] | -local [-kernelpath]] [-signature type ...] [-args values ...]`

### 说明

`Wolframscript` 在本地或云端运行 `Wolfram` 语言代码, 函数和已部署的 API,  接受
标准输入
命令行参数
文件
URL
等形式的输入.

### 命令行代码

在本地 Wolfram Engine 上执行 Wolfram 语言代码 `2+2`:

```mathematica
wolframscript -code 2+2
4
```

在 Wolfram 云中执行 Wolfram 语言代码 `2+2`, 需要时提示输入许可验证:

```mathematica
wolframscript -cloud -code 2+2
4
```

在本地执行 Wolfram 语言代码, 对shell的输入进行转义:

```mathematica
wolframscript -code 'StringReverse["hello"]'
olleh
```

执行代码并把结果放入文件中:

```mathematica
wolframscript -code 'Graphics3D[Sphere[ ]]' -format PNG > file.png
```

### 文件代码

从文件中执行 Wolfram 语言代码, 返回产生的最终结果:

```mathematica
$ wolframscript -file test.wl
12345
```

从本地文件中获取代码, 但是在云端运行代码:

```mathematica
$ wolframscript -cloud -file test.wl
12345
```

### 脚本文件

设置为在本地执行 Wolfram 语言代码的文件:

```mathematica
#!/usr/bin/env wolframscript
Print[2+2]
$ ./file.wls
4
```

在 Wolfram 云中执行 Wolfram 语言代码的文件:

```mathematica
#!/usr/bin/env wolframscript -cloud
Print[2+2]
$ ./file.wls
4
```

使用命令行参数的文件:

```mathematica
#!/usr/bin/env wolframscript
Print[ToExpression[$ScriptCommandLine[[2]]]^2]
$ ./file.wls 5
25
```

一个能给出函数的文件, 其参数从命令行中来:

```mathematica
#!/usr/bin/env wolframscript -function -signature City City
GeoDistance[#1, #2]&
$ ./file.wls "New York" London
Quantity[3453.7070027090986, Miles]
```

### 交互操作

```mathematica
在交互 `REPL` 中运行 Wolfram 语言:

$ wolframscript
Wolfram Language 12.0.0 for Microsoft Windows (64-bit)
Copyright 1988-2019 Wolfram Research, Inc.

In[1]:= 2+2

Out[1]= 4

In[2]:=
```

### API

### 运行云端 API

```mathematica
$ wolframscript -api https://wolfr.am/bNvKWq2U -args x=1 y=2
3
```

从云中获取 API 代码, 但在本地运行 API:

```mathematica
$ wolframscript -api https://wolfr.am/bNvKWq2U -local -args x=1 y=2
3
```

### 更多范例

登录不同的云账户:

```mathematica
$ wolframscript -authenticate
Enter WolframID: example-user@wolfram.com
Password:

Success. Saving connection data.
```

提供证书而不使用提示:

```mathematica
$ wolframscript -username example-user@wolfram.com -password XXXXXX
Success. Saving connection data.
```

断开云端, 清除连接信息:

```mathematica
$ wolframscript -disconnect
```

颠倒输入文件中每行字符串的顺序, 并把结果写入另一个文件中:

```mathematica
$ wolframscript -code 'StringReverse[$ScriptInputString]' -linewise < file1 > file2
```

使用超时限制计算:

```mathematica
$ wolframscript -code 'Do[Print[i];Pause[1], {i,10}]' -timeout 3
1
2
3
$TimedOut
```

输出使用特殊字符:

```mathematica
$ wolframscript -code 'Alphabet["Greek"]' -charset UTF8
```

使用脚本的选项 `-print` 和 `-format` 产生一幅图像:

```mathematica
#!/usr/bin/env wolframscript -print -format PNG
ListLinePlot[RandomFunction[WienerProcess[],{0,10,0.01},10]]
```

```powershell
$ ./file.wls > plot.png
```

使用 `-print All` 选项, 打印执行脚本时产生的每个结果:

```mathematica
#!/usr/bin/env wolframscript -print All
"Using -print All print will each result"
a = 2+2; (* This line won't print because the ; suppresses output *)
a
$ ./file.wls
Using -print All will each result
4
```

创建一个由 `PermissionsKey` 保护的 `API` , 并把密钥传给 `WolframScript` , 以便访问:

```mathematica
In[1]:= CloudDeploy[APIFunction[{"n"->Integer},#n^2&],Permissions->{PermissionsKey["thekey"]->"Execute"}]
Out[1]= CloudObject[https://www.wolframcloud.com/objects/83aa0bc2-8e0c-4ef6-b314-48e0bf283196]
$ wolframscript -api 83aa0bc2-8e0c-4ef6-b314-48e0bf283196 -args n=5 -permissionskey thekey
25
```

检查 WolframScript 的版本:

```mathematica
$ wolframscript -version
WolframScript 1.2.0 for MacOSX-x86-64
```

配置使用特别的 `WolframEngine` :

```mathematica
$ wolframscript -config WOLFRAMSCRIPT_KERNELPATH=/Applications/Mathematica.app/MacOS/WolframKernel
Configured:WOLFRAMSCRIPT_KERNELPATH=/Applications/Mathematica.app/MacOS/WolframKernel
```

### WolframScript选项

+ 代码选项

+ `-c|-code code` -- 给出要执行的 Wolfram 语言代码.
+ `-f|-file file` -- 给出含有要执行的 Wolfram 语言代码的文件.
+ `-api url|uuid|file` -- 在指定的 URL 使用 API, 或来自于有指定 UUID 的云端或本地对象, 也可以来自于指定的本地文件. 使用参数 key=value .......
+ `-fun|-function code [-s|-signature type ...] [-args|-- value ...]` --  使用参数为字符串 `value` ...... 的函数, 将其解释为类型 `type` ....... 如果没有给出标记 (signature), 则假设所有参数都是字符串. 标记类型可以是 `$InterpreterTypes` 的任意一种.

+ 执行选项

+ `-o|-cloud [cloudbase]` -- 在云端执行代码, 使用指定的云基 (cloud base). 缺省情况下, cloudbase 为 https://wolframcloud.com.
+ `-l|-local [kernelpath]` -- 在本地执行代码, 使用到 Wolfram Engine 核的指定路径. 缺省情况下, kernelpath 使用本地系统中 Wolfram 语言的最新版本.
+ `-format type` -- 指定输出的格式. 可以使用任意可被 Export 接受的格式.
+ `-charset encoding` -- 对输出使用 encoding. 如果想输出原始字节, 编码可设为 None, 或  $CharacterEncodings 中的任意一项, 除了 "Unicode". 默认情况下, 从终端的语言设置中推断.
+ `-linewise` -- 执行从标准输入读入的每行代码.
+ `-print [all]` -- 当运行脚本时, 将执行脚本最后一行的结果打印出来, 在指定 all 时, 打印所有行的结果.
+ `-timeout seconds [value]` -- 指定执行可以使用的秒数. 如果超出指定时间, 返回 value 的值.
+ `-v|-verbose` --  在执行中打印额外信息.

+ 实用选项

`-h|-help` -- 打印帮助信息.
`-version` -- 打印 WolframScript 版本.
`-auth|-authenticate [wolframid [password]] [-cloud cloudbase]` -- 执行云端许可验证, 指定特定的 Wolfram ID 和密码, 没有给出的情况下提示输入. 对不同的云可以指定不同的验证.
`-username [wolframid]` -- 指定在云端认证使用的 Wolfram ID.
`-password [password]` -- 指定在云端认证使用的密码.
`-permissionskey key` -- 使用权限密钥访问云资源.
`-config|-configure [key=value ...]` -- 通过指定特定配置变量的值对 WolframScript 进行配置.
`-disconnect [-cloud cloudbase]` -- 从云端断开, 并移除验证信息.

### 详细信息

+ Wolfram 语言脚本

在 `#!wolframscript` 脚本中可以使用所有标准选项.
当设置为 `#!wolframscript -function` ...... 时, 可以在命令行脚本中给出函数的每个参数.
当设置为 `#!wolframscript -api` ...... 时, 可以按 `-key value` ...... 形式在命令行脚本中给出 API 的参数.
可以用 `Exit[code]`指定在执行脚本时退出的代码.
没有设置 `-print` 时, 不传送任何输出到 `stdout`, 除非使用 `Print[expr]` 命令明确指出.
当有选项 `-print` 时, 把脚本最后一行的结果传送到 `stdout`.
当有选项 `-print all` 时, 脚本中每一行命令的结果产生后都被传送到 `stdout`.
`-linewise` 选项可用来多次运行脚本, 每次取一行 `stdin` 作为输入.

+ 命令行输入;  在 Wolfram 语言代码中, 可以用 `$ScriptInputString` 获取按标准输入形式给出的脚本输入. 可以用 `$ScriptCommandLine` 获取命令行中给出的参数.
+ 输出格式; `TotalWidth` 的缺省设置为 `Infinity`.
+ API 参数; 如果 API 支持多个参数, 如 `x-url`, `x-format` 和 `_timeout`, 可将它们用在 `wolframscript -api` 命令中.
+ 代码位置; 在 `wolframscript -api uuid` 中, 如果 `LocalObject["uuid"]` 存在, 就用 `LocalObject["uuid"]`, 否则使用 `CloudObject["uuid"]`.

## 配置和环境变量

### 缺省配置文件

+ `%APPDATA%\Wolfram\WolframScript\WolframScript.conf` Windows
+ `~/Library/Application\ Support/Wolfram/WolframScript/WolframScript.conf` Macintosh
+ `~/.config/Wolfram/WolframScript/WolframScript.conf` Unix

### 缺省认证文件夹

+ `\%LOCALAPPDATA%\Wolfram\WolframScript\` Windows
+ `~/Library/Caches/Wolfram/WolframScript/` Macintosh
+ `~/.cache/Wolfram/WolframScript/` Unix

### WOLFRAM 语言变量

下列变量会在 WolframScript 开始执行时被设置.

+ `$CommandLine` -- 一系列字符串给出使用的完整的命令行.
+ `$ScriptCommandLine` -- 为正在运行的脚本准备的一系列命令行参数. 这些参数出现在 `-option` 给出的选项之后.
+ `$ScriptInputString` -- 一个通过标准输入给出脚本输入的字符串. 在脚本的每次迭代中, 选项 `-linewise` 用一行标准输入载入该变量.

### 环境变量

+ `WOLFRAMSCRIPT_AUTHENTICATIONPATH` -- 存储验证信息的文件夹.
+ `WOLFRAMSCRIPT_CONFIGURATIONPATH` -- 存储永久配置信息的文件.
+ `WOLFRAMSCRIPT_CLOUDBASE` --  WolframScript 中使用的缺省云基 (cloud base) .
+ `WOLFRAMSCRIPT_KERNELPATH` -- 到缺省的本地可执行 Wolfram Engine 核的路径.

### 配置变量

+ `WOLFRAMSCRIPT_AUTHENTICATIONPATH` -- 同名的环境变量尚未被设置的情况下, 到存储验证信息的文件的路径.
+ `WOLFRAMSCRIPT_CLOUDBASE` -- 同名的环境变量尚未被设置的情况下, WolframScript 中使用的缺省云基.
+ `WOLFRAMSCRIPT_KERNELPATH` -- 同名的环境变量尚未被设置的情况下, 到缺省的本地可执行 Wolfram Engine 核的路径.
