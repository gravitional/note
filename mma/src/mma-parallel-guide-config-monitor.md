# parallel guide

ParallelTools/tutorial/Overview
ParallelTools/tutorial/ConcurrencyManagingParallelProcesses

Wolfram 语言为并行计算提供了一个强大而独特的环境.
大部分的功能都可以用最少的精力来使用, 而且不需要对并行系统的低级内部结构付出太多细节.
然而, 如果你想获得有关并行系统功能的更详细的描述, 本用户指南是非常有用的.

## introduction

### 使用 Wolfram 进行并行计算

Wolfram语言的 `并行计算` 是基于, 从一个 `主Wolfram语言`(主核) 中启动和控制多个Wolfram语言内核(`worker`)进程,
为并行编程提供一个分发式内存环境. (distributed-memory environment)

`wolfram系统` 的每一份拷贝, 都带有所有的 `组件` 和 `工具`, 来运行和创建并行应用程序.
并行计算功能几乎完全由Wolfram语言编写, 因此是独立于机器的.
它们已经在 `Unix`, `Linux`, `Windows` 和 `Macintosh` 平台上进行了测试,
并且非常适合在多核机器上, 机器网格中(grid of machines), 或异质网络(heterogeneous network)中进行本地工作.

所有的 `客户端` 和 `应用程序代码` 都是分发式的, 所以不需要 `公共文件系统`.
从而促进了并行编程技术的使用. 这些技术可以从许多技术进步中受益,
例如, 多核机器, 使用专门的硬件进行并行计算, 以及快速网络技术(fast network technology), 帮助运行连接的计算机网格(grid).

为了进行 `并行计算`, 你需要能够执行以下任务.

+ 启动 `进程`(processes) 并等待进程完成
+ 在可用的 `处理器`(processors)上安排进程
+ 在进程之间交换数据, 并同步(synchronize)对公共资源的访问

在 Wolfram Language 环境中, 术语 `处理器`(processor)是指正在运行的 Wolfram Language 内核,
而进程(process)是指要计算的表达式.

### WSTP

Wolfram语言的`并行计算`的基础是, Wolfram符号传输协议(`WSTP`).
这是一个通用而灵活的接口, 用于各种程序与Wolfram语言进行通信.
它是Wolfram语言的一个基本组成部分, 在Wolfram语言本身中被广泛使用.
一种常见的操作模式是Wolfram语言的不同组件使用WSTP进行通信,
这正是并行计算使用WSTP的方式.

WSTP提供了一些核心功能, 它是独立于平台和架构的, 它既可以在本地工作, 也可以在网络上工作, 它能传输 `Wolfram` 语言可以表示的任何东西,
并且它提供了一个控制Wolfram语言的接口.
这些都是并行计算的关键特征, 允许在同质和异质网络之间进行本地和网络通信, 并允许数据和程序的传输.

### 连接方法

Wolfram语言可以通过多种不同的方式运行` 并行 workers`, 在同一台本地机器上, 或网络上的远程机器上.
此外, 网络可能是一个由一些专门的管理应用程序控制的同质网格(homogeneous grid), 也可能是一个异质网格(heterogeneous grid).
为了在这些情况下运行, Wolfram Language需要启动 `并行 workers`, 然后与他们进行通信.

为了在这些不同的情况下进行并行计算, Wolfram Language 提供了许多不同的连接方法, 其中每一种都是专门针对特定的使用风格. 这里将介绍一些主要的连接方法.

#### 本地内核,Local Kernels

`本地内核连接方法`, 用于在与 `主Wolfram语言` 相同的计算机上运行并行的`worker`s.
它适用于多核环境, 是启动和运行并行计算的最简单方法.

只要你的处理器有一个以上的内核, 你就可以使用`本地内核`进行并行计算.

```mathematica
$ProcessorCount
Out[25]=2
```

### 轻量级网格, LightWeight Grid

`轻量级网格连接`方法用于, 从运行 `主Wolfram语言` 之外的计算机上运行 `并行的 worker`s.
它使用 Wolfram 轻量级网格技术, 来启动远程机器上的 Wolfram 语言.
它适用于异质网络(heterogeneous network), 且没有管理技术的地方.

#### 集群集成,Cluster Integration

`集群集成连接方法` 用于, 从运行 `主Wolfram语言` 之外的计算机上运行 `并行的 worker`s.
它与大量的 `第三方集群管理技术` 相整合.

#### 远程内核, Remote Kernels

`远程内核连接方法`用于, 在 `主Wolfram Language` 之外的计算机上 `运行并行 worker`s.
它依赖于使用 `远程 shell 调用` 技术来启动, 通常更难配置和维护.

### 特点

Wolfram语言中 `并行计算` 的主要特点是.

+ 分发式内存, 主从式并行(master/slave parallelism)
`m```mathematica
用Wolfram语言编写
+ 独立于机器型号
+ 与远程内核的WSTP通信
+ 与远程内核交换符号表达式和程序, 而不仅仅是 `数字` 和 `数组`
+ 异构网络, 多处理器机器, LAN和WAN
+ 虚拟进程调度, 或显式将进程分发给可用的处理器
virtual process scheduling or explicit process distribution to available processors
+ 虚拟共享内存, 同步, 锁定; virtual shared memory, synchronization, locking
+ 延迟隐藏; latency hiding
+ 支持并行函数式编程, 和自动并行化
+ 故障恢复, 在故障的远程计算机上自动重新分发搁浅的进程
failure recovery, automatic reassignment of stranded processes on failed remote computers

#### 较低级别的功能

在 `` Parallel`Developer` ``上下文中还有其他的并行编程功能.
它们允许你获取和设置 `并行内核` 的属性, 并实现你自己的 `并发调度器`(scheduler for concurrency).

低级别的功能最好通过, 在你的 `$ContextPath` 中添加其`上下文`来实现.

```mathematica
Needs["Parallel`Developer`"]
```

对于单次使用, 用 `全名` 来引用开发者函数就足够了, 比如

```bash
Parallel`Developer`ClearKernels[].
```

## 开始使用

Wolfram语言自带了所有的工具和配置, 使您可以立即进行并行计算.
请注意, 要利用并行计算的优势, 通常最好是有一台多核机器, 或访问Wolfram Language并行内核的网格.
幸运的是, 多核机器在许多类型的配置中, 已经开始普及开来.

第一步可能只是证明系统能够运行, 使用 `ParallelEvaluate`.
如果这是会话中的第一次 `并行计算`, 它将启动`配置的`并行内核.

下面的例子应该返回每个并行内核的 `进程ID`.

```mathematica
ParallelEvaluate[$ProcessID]
Out[10]= {1648, 1649, 1657, 1669}
```

这将返回每个内核的机器名称; 例如它表明所有东西都在同一台计算机上运行.

```mathematica
ParallelEvaluate[$MachineName]
Out[11]= {"arcturus", "arcturus", "arcturus", "arcturus" }
```

你可能会发现打开 `并行内核状态监视器` 是很有用的, 它看起来像下面这样.

    计算-> 并行内核状态; Alt-V-K

现在你可以进行一次实际的计算了. 一个非常简单的并行程序类型是做一个搜索.
在下面的例子中, 将`1`加到`阶乘`上, 然后测试结果是否为`素数`.
通过在 `Parallelize` 中包装常规的 `Wolfram` 语言计算, 即可完成.

```mathematica
data = Parallelize[Table[ PrimeQ[n! + 1], {n, 400, 550}]];
```

下面的表达式告诉我们这些数字中有一些是质数.

```mathematica
Or @@ data
Out[4]= True
```

另一个例子是寻找梅森素数(Mersenne prime).
这是用下面的方法完成的, 同样用 `Parallelize` 包装计算.

```mathematica
Parallelize[Select[ Range[2000], PrimeQ[2^# - 1] &]]
Out[5]= {2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607, 1279}
```

这表明前15个梅森素数已经被找到.

当你到了这个阶段, 你应该已经准备好开始在Wolfram语言中进行并行计算了.

### 在并行计算中使用你自己的函数

前面的例子是通过简单地将一个可并行的表达式包裹在 `Parallelize[...]` 中来实现的.
如果表达式不仅涉及 `内置函数`, 而且涉及你自己定义的函数, 那么就需要做一些准备工作了.

除了 `内置的符号` 外, 要在`并行内核`上被计算的符号的`定义`, 需要在使用前`分发`到所有内核:

定义一个测试 `2^n-1` 是否为素数的谓词(predicate).

```mathematica
mersenneQ[n_] := PrimeQ[2^n - 1]
```

将该定义分发到所有的并行内核.

```mathematica
DistributeDefinitions[mersenneQ]
```

现在它可以作为并行计算的一部分使用.

```mathematica
Parallelize[Select[Range[1000], mersenneQ]]
Out[14]= {2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607}
```

如果你忘了为`并行计算`分发定义会发生什么?
这个定义与上面的 `mersenneQ` 相同.

```mathematica
m2[n_] := PrimeQ[2^n - 1]

Parallelize[Select[Range[1000], m2]]
Out[18]= {2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607}
```

在很多情况下, 无论如何, 这个计算似乎是有效的, 但是如果你分析它的性能, 你应该看到它实际上没有被计算的那么快.

这种计算给出了正确的结果, 但它并不比正常的`Table `快.

```mathematica
ParallelTable[m2[i], {i, 10000, 10020}]
```

它看起来有效的原因是, 未知函数 `m2` 不在 `并行内核`上求值,
所以表达式 `m2[10000], m2[10001], ...` 被送回, 然后它们在主内核上求值, 那里的定义是已知的.

## 配置和监控

Wolfram Language 提供了许多配置和监控并行计算的工具.
其中一些是通过 Wolfram Language 笔记本前端的菜单来访问的.
本节将介绍这些工具, 并说明它们的作用.

常用的管理多核的函数及变量为:

+ `LaunchKernels`: 启动子核
+ `AbortKernels`-- 停止所有子核上的运算
+ `CloseKernels`: 关闭所有子核
+ `$KernelCount`:目前运行的子核数量.
+ `Kernels`: 列出正在运行的子核
+ `$KernelID`: 每个子核的特有`ID`
+ `$ConfiguredKernels`:子核的默认启动列表

### 并行首选项,Preferences

Wolfram Language的默认设置会自动配置一些并行内核, 用于并行计算.
通常情况下, 在多核机器上, 你会得到与内核数量相匹配的`worker`内核的数量(最多到许可进程限制).
这个默认设置意味着, 当你在会话中第一次进行`并行计算`时, 系统会被初始化.

你可以通过`并行首选项`看到这些设置.
这些可以从前端菜单栏中选择 `Evaluation > Parallel Kernel Configuration`来打开.
它应该能打开 `并行偏好设置`, 看起来像下面这样.
从并行首选项中, 你可以设置有关如何启动并行内核的细节;
默认设置是在需要时加载内核.
此外, 你还可以配置连接方法.

#### 常规首选项

常规首选项控制何时应启动并行内核.

+ `Manual` ; 不自动启动内核
+ `When needed` ; 在启动第一个并行计算时启动内核
+ `At startup` ; 在主内核启动时启动内核

Wolfram语言中的 `并行函数`, 通常只在任何一个函数被首次引用时才会被加载,
所以启动内核不会减慢你的 `主内核` 的启动.

`Evaluation failure handling `, 控制在并行计算过程中, 如果并行内核失败(由于硬件故障, 网络问题或崩溃), 如何处理正在进行的计算的.

`Retry`(重试)导致正在运行的计算在失败时被重新分发给另一个内核;
`Abandon`(放弃)意味着将其标记为失败, 并返回 `$Failed` 作为其结果.
在默认的 `Retry` 设置下, 即使内核在计算过程中失败, 并行计算也可以正常完成.

如果启用 `Try to relaunch failed kernels`, Wolfram Language将定期尝试重新启动任何失败的内核.
这可能并不总是可能的, 这取决于所使用的 `内核的类型`.
对于本地内核, 它应该总是成功的.

选择启用 `Enable parallel monitoring tools` 有助于开发并行程序, 因为它允许你监控性能, 也可以选择观察主内核和并行内核之间的通信.
这些工具会造成很小的性能损失, 但是除了较大的 `面向批处理`的计算之外, 一般都推荐使用.

#### 并行内核配置

`Parallel Kernel Configuration`选项卡控制 Wolfram Language 如何启动并与并行内核进行通信.

默认的连接方法是使用本地内核, 在与 Wolfram Language 主内核相同的计算机上运行内核.
默认情况下, 这将启动与你的计算机上的内核数量相同的内核;
这是由 `$ProcessorCount` 指定的.

还提供了其他的连接方法, 这些方法与在某种类型的网格或网络中的远程计算机上运行的内核一起工作.

你可以在 "连接方法 "中找到更多信息.
其中一些方法需要在你的本地计算机上安装额外的软件, 通常需要在你想使用的远程计算机上安装Wolfram System.

### 状态

您可以通过 `状态显示` 监控您的并行内核的运行情况.
这可以从前端菜单栏中选择 `Evaluation > Parallel Kernel Status` 打开.
它看起来应该像下面这样.

这显示了一个主核和两个工作核都在同一台计算机上运行.
只有在偏好设置中启用了`并行监控工具`, `性能监控组`的栏目才可用.

### 系统信息

要想获得你的系统用于`并行计算`的配置的详细清单,
你可以使用 `SystemInformation` 的并行标签. 下面是一个例子.

`SystemInformation` 的文本表示可以通过 `Copy` 按钮获得.

## 启动和连接

Wolfram Language 可以以多种不同的方式运行并行内核: 在同一台机器上的本地或在网络中连接的其他机器上的远程.
此外, 网络可能是一个由一些专门的管理应用程序控制的同质网格, 也可能是一个异质的网格. 为了使用所有这些不同的配置, Wolfram Language需要启动并行内核, 然后与它们进行通信.
当与远程机器一起工作时, 启动并行内核是很复杂的.
不管并行内核是如何启动的, 通信总是使用 `WSTP`.

通常情况下, Wolfram Language 会在需要时自动启动并行内核.
这使用了 `并行首选项`(Parallel Preferences) 的设置, `但你也可以用LaunchKernels` 命令手动启动内核.
如果你在批处理模式下运行, 这可能是有用的.

本节将讨论与启动和连接到并行内核有关的问题.
它将涵盖所提供的不同连接方法, 每一种方法都专门针对 Wolfram Language 可能运行的不同环境类型.

### 启动内核

Wolfram语言的并行计算, 通常会在执行并行命令时自动启动内核.
启动的内核是那些在并行首选项中设置的内核.
默认设置是启动在同一台计算机上运行的内核, 使用本地内核连接方法;
对于双核机器, 将有两个并行内核.
你可以通过 `$ConfiguredKernels` 看到将自动启动的内核;

```mathematica
$ConfiguredKernels
{StringForm["<<`1` local kernels>>", 6]}
```

如果你使用没有参数的 `LaunchKernels`, 这将启动已经配置好的内核.
这里, 两个内核被启动.

```mathematica
LaunchKernels[]
...
```

注意, 如果你使用了 `ParallelTable` 这样的并行命令, 你不需要使用 `LaunchKernels`, 并行内核会自动启动.

在任何时候, 你都可以通过 `Kernels` 看到正在运行的内核.

```mathematica
Kernels[]
```

你也可以使用 `LaunchKernels` 来手动启动内核, 而不用在偏好设置中配置它们.
每种连接方式的细节都不一样, 在下面的章节中会介绍.

本节的其余部分现在将描述每种不同连接方式的配置和启动.

### 本地内核

`本地内核连接方法`用于, 在与 `主Wolfram语言` 相同的计算机上运行并行内核.
它适用于多核环境, 也是为并行计算而设置的最简单方法.

`本地内核连接`方式的配置是通过`并行首选项`完成的.
配置面板看起来类似于下面的内容.
由于并行内核都是在与主内核相同的计算机上启动的, 这是最容易配置的.
主要的问题是要启动的内核的数量;
自动设置是使用处理器内核的数量和并行内核的许可数量.

处理器核的数量是由 `$ProcessorCount` 给出的;
如果你想改变它的值, 你可以取消保护并指定一个新的值.

```mathematica
$ProcessorCount
6
```

并行内核许可证的数量由 `$MaxLicenseSubprocesses` 给出, 它由 `Wolfram System` 许可证控制.

```mathematica
$MaxLicenseSubprocesses
\[Infinity]
```

设置为`\[Infinity]`意味着 最多数量不限.

### 手动启动

如果你想手动启动内核, 避免配置机制, 你可以直接向 `LaunchKernels` 传递参数.
如果你在 `批处理模式`下, 直接从命令行运行 Wolfram Language, 这可能很有用.

+ `LaunchKernels[num]`  ;   启动 `num` 个本地内核
+ `LaunchKernels["localhost"]`  ;   启动一个本地内核
+ `LaunchKernels[LocalMachine[num]]`    ;   启动 `n`个本地内核
+ `LaunchKernels[LocalMachine[cmd]]`    ;   使用操作系统命令 `cmd` 启动一个本地内核.

`本地内核连接方法`支持直接使用 `LaunchKernels` 启动本地内核,
通过传递一个整数(设置内核的数量)或指定字符串 `localhost`.
此外, 你可以在 `LocalMachine` 里面给出更详细的设置.
要做到这一点, 你必须首先加载`` LocalKernels` ``包, 如下所示.

```mathematica
Needs["SubKernels`LocalKernels`"]
```

现在你可以向 `LocalMachine` 传递参数. 以下是设置要启动的本地内核的数量.

```mathematica
LaunchKernels[LocalMachine[1]]
...
```

改变用于启动远程内核的命令可能更有用.
默认命令是由 `$mathkernel` 设置的, 它的典型设置如下.

```mathematica
$mathkernel

Out[12]="/home/ws-dist/1220-7150061/Executables/wolfram -subkernel -noinit \
-pacletreadonly -wstp"
```

你可以将命令直接传入 `LocalMachine`.
下面的内容适合于 Wolfram 系统的非标准安装.

```mathematica
LaunchKernels[
 LocalMachine[
  "'/Applications/Mathematica.app/Contents/MacOS/MathKernel' \
-subkernel -noinit -mathlink"]]
Out[3]=KernelObject[2, "local"]
```

另一种方法是在启动任何内核之前修改 `$mathkernel` 的值.

一旦你启动了并行内核, 那么就可以使用 Wolfram语言 的所有 `并行函数`.

### 轻量级网格

`轻量级网格连接方法`用于, 在异于 `主Wolfram语言所在的计算机` 上, 运行 `并行workers`.

它使用 Wolfram 轻量级网格技术在远程机器上启动 Wolfram 语言.
它适用于 `异质网络` 和没有管理技术的地方.

需要安装 gridMathematica Server 以及 Lightweight Grid Manager.

`轻量级网格连接` 方式的配置是通过 `并行首选项` 完成的.
配置面板看起来应该与下面类似.
在这个设置中, 主内核找到了另外三台运行轻量级网格管理器的计算机,
每台都被配置为运行两个内核.

更多信息可以在轻量级网格文档中找到.
LightweightGridClient/guide/LightweightGridClient

#### 手动启动

如果你想手动启动内核, 避免使用配置机制, 你可以直接向 `LaunchKernels` 传递参数.
如果你直接从命令行以批处理模式运行 Wolfram Language, 这可能很有用.

+ `LaunchKernels[url]`  ;   使用服务器`url`上的轻量级网格启动一个内核.
+ `LaunchKernels[LightweightGrid[url]]` ;   使用服务器`url`上的轻量级网格启动一个内核.

轻量级网格连接方法支持直接用 `LaunchKernels` 启动内核, 只需将轻量级网格管理器的名称传给它.
此外, 你可以在 `LightweightGrid` 里面给出更详细的设置.
要做到这一点, 你必须首先加载`` LightweightGridClient` ``包, 如下图所示.

```mathematica
Needs["LightweightGridClient`"]
```

现在, 当你启动内核时, 你在 `LightweightGrid` 中使用参数. 下面是一个例子.

```mathematica
LaunchKernels[LightweightGrid["octet"]]
Out[2]= {KernelObject[2, "octet"]}.
```

一旦你启动了并行内核, 那么就可以使用Wolfram语言的所有并行功能.

更多信息可以在轻量级网格文档中找到.

### 集群集成,Cluster Integration

`集群集成连接` 方法用于在异于 `主Wolfram语言所在计算机` 上运行 `并行 worker`s.
它与大量的第三方集群管理技术相整合.

集群集成连接方法的配置是通过并行首选项完成的.
配置面板应该类似于下面的样子.
在这个设置中, 一个集群被配置为使用 `Windows Computer Cluster Server`, 头节点为 `clusterboss`.
每台机器都被设置为运行两个内核.

`Wolfram Language` 支持以下集群管理技术.

+ Windows Computer Cluster Server
+ Windows HPC Server 2008
+ Platform TM LSF®
+ Altair TM PBS Professional®
+ Sun Grid Engine

更多信息可以在集群集成文档中找到.
ClusterIntegration/guide/ClusterIntegration

#### 手动启动

如果你想手动启动内核, 避免配置机制, 你可以直接向 `LaunchKernels` 传递参数.
如果您直接从命令行以批处理模式运行 Wolfram Language, 这可能很有用.

`LaunchKernels[cluster[args]]`  ; 在 cluster 上使用 Cluster Integration 启动一个内核.

集群集成的连接方法支持直接从 `LaunchKernels` 启动内核.
要做到这一点, 你必须首先加载`` ClusterIntegration` ``包, 如下图所示.

```mathematica
Needs["ClusterIntegration`"]
```

要在一个特定的集群上启动, 你必须把该集群的名称传给 `LaunchKernels`.
例如, 要使用 `Windows Compute Cluster Server`, 你可以使用 `CCS`.
下面是一个启动两个内核的例子.

```
LaunchKernels[CCS["winccs"], 2]
Out[2]= {KernelObject[1, "winccs"], KernelObject[2, "winccs"] }
```

请注意, 要使这一方法奏效, 你需要配置 `Windows Compute Cluster Server` 才行.

一旦你启动了并行内核, 那么就可以使用 Wolfram Language 的所有并行功能.

更多信息可以在集群集成文档中找到.

### 远程内核

`远程内核`用于在 `异于主Wolfram语言所在计算机` 上运行 `并行worker`s.
它依赖于使用远程 shell 调用技术来启动, 通常更难配置和维护.

远程内核连接方法的配置是通过 `并行首选项` 完成的.
配置面板应该类似于下面的样子.
在这个配置中, 两台远程机器被配置为各自提供四个并行内核.

在Windows上, 默认设置是使用 `rsh` 来启动远程机器上的内核; 其他平台使用 `ssh`.
这是因为 `ssh` 通常不适用于 `Windows`.
当然, 任何机器都必须被配置为允许从 `主机器上` 进行 `远程shell调用`.

你可以修改用于启动内核的启动命令.
在命令中, 你可以使用一些参数, 这些参数在命令实际使用之前, 会被填充.
例如, 参数 `` `1` `` 是用来指正在使用的远程机器的名称.

```mathematica
`1` ;   远程机器的名称
`2` ;   在本地机器上创建的链接的名称(当远程机器正在连接时
`3` ;   执行远程shell的用户名称
`4` ;   用于WSTP连接的链接协议.
```

用户名称的默认值是使用 `$Username`, 即运行Wolfram Language 本地拷贝的用户.

#### 手动启动

如果你想手动启动内核, 避免使用配置机制, 你可以直接向 `LaunchKernels` 传递参数.
如果您是直接从命令行以批处理模式运行 Wolfram Language, 这可能很有用.

+ `LaunchKernels[RemoteMachine[host]]`  ;在机器主机上启动一个远程内核
+ `LaunchKernels[RemoteMachine[host,num]]`  ;在机器主机上启动数个远程内核.
+ `LaunchKernels[RemoteMachine[host,command,num]]`; 使用 `command` 作为远程命令在机器 `host` 上启动`n`个远程内核.

远程内核连接方法支持 `LaunchKernels` 直接启动远程内核.
要做到这一点, 你必须首先加载`` RemoteKernels` ``包, 如下所示.

```mathematica
Needs["SubKernels`RemoteKernels`"]
```

为了启动, 你在 `RemoteMachine` 中传递参数.
下面是在 `remote1.wolfram.com` 机器上启动一个远程内核.

```mathematica
LaunchKernels[RemoteMachine["remote1.wolfram.com"]]
OutPut[2]= KernelObject[1, "remote1.wolfram.com"]
```

改变用于启动 `远程内核` 的命令可能更有用.
默认命令是由 `$RemoteCommand` 设置的, 它的典型设置如下.
注意, `rsh` 只在 `Windows` 上使用, 在其他平台上使用 `ssh`.

```mathematica
$RemoteCommand

Out[19]= "ssh -x -f -l `3` `1` wolfram -wstp -linkmode Connect `4` -linkname \
'`2`' -subkernel -noinit -pacletreadonly"
```

注意, `远程命令` 的参数在上一节中已经描述过了.
用户名参数是由 `$RemoteUsername` 设置的, 它默认设置为 `$Username`.

```mathematica
$RemoteUsername
Out[18]= "user"
```

你可以将命令直接传入 `RemoteMachine`. 下面的内容适合于 Wolfram System 的非标准安装.

```mathematica
LaunchKernels[RemoteMachine[
  "remote1.wolfram.com",
  "rsh `1` -n -l `3` \"D:\\Mathematica\\math -mathlink -linkmode \
Connect `4` -linkname `2` -subkernel -noinit >& /dev/null &", 4]]

Out[5]=KernelObject[2, "remote1.wolfram.com"]
```

另一种方法是在启动任何内核之前修改 `$RemoteKernel` 的值.

一旦你启动了并行内核, 那么就可以使用 Wolfram 语言的所有并行功能.

### WSTPServer Kernels

`远程内核连接方法`用于在与主Wolfram Language不同的计算机上运行并行`workers`.
它可以连接到 `WSTPServer` 的运行实例, 并向这些服务器请求`内核`.

启用 `WSTPServer内核连接方法`是通过 `并行首选项` 完成的.
配置面板应该类似于下面的样子. 内核需要手动配置.

#### 手动启动

`WSTPServer` 内核可以通过 `wstp://server:port` 形式的 `URL` 方便地指定.
端口号是可选的.

```mathematica
Needs["SubKernels`WSTPServerKernels`"]
```

你可以直接指定 `wstp: URLs` 作为 `LaunchKernels` 的一个参数.

这里, 我们要求在名为 `"raspi4b"` 的机器, 监听请求的 `WSTPServer` 中获取2个内核.

```mathematica
LaunchKernels["wstp://raspi4b", 2]
Out[2]= {KernelObject[...
ParallelEvaluate[{$MachineName, $Version}]
Out[3]= {{"raspi4b",...
CloseKernels[];
```

`WSTPServer` 内核也可以包含在 `$ConfiguredKernels` 中.
它们需要作为 `WSTPServerKernel[]` 项加入.

```mathematica
$ConfiguredKernels = {WSTPServerKernel["wstp://raspi4b", 2]}
OutPut: {StringForm["<<`1` kernels each on `2` nodes>>", 2, 1]}
LaunchKernels[]
```

一旦你启动了并行内核, Wolfram语言的所有并行功能都可以使用.

### 原始WSTP连接

原则上也可以 attach 到, 通过手动设置的 `WSTP链接`连接的内核.

在下面的例子中, 加载了对应于 `raw link connections` 的本地应用程序.
然后将 `-subkernel` 和 `-mathlink` 作为命令行选项, 启动了 `Wolfram Language` 的另一个拷贝, .

最后, 这个链接被传递给 `LaunchKernels`, 使远程内核成为`worker`内核.

```mathematica
Needs[ "SubKernels`LinkKernels`"]
kernel = "/Applications/Mathematica.app/Contents/MacOS/MathKernel";
link = LinkLaunch[kernel <> " -subkernel -mathlink"]
Out[52]= LinkObject[...]
LaunchKernels[link]
Out[4]= KernelObject[...]
```
