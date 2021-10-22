# parallel guide

ParallelTools/tutorial/Overview
ParallelTools/tutorial/ConcurrencyManagingParallelProcesses

Wolfram 语言为并行计算提供了一个强大而独特的环境.
大部分的功能都可以用最少的精力来使用, 而且不需要对并行系统的低级内部结构付出太多细节.
然而, 如果你想获得有关并行系统功能的更详细的描述, 本用户指南是非常有用的.

## introduction

### 使用 Wolfram 进行并行计算

Wolfram语言的 `并行计算` 是基于, 从一个 `主Wolfram语言` 中启动和控制多个Wolfram语言内核(`worker`)进程,
为并行编程提供一个分发式内存环境. (distributed-memory environment)
`m```mathematica
olfram系统的每一份拷贝都带有所有的组件和工具来运行和创建并行应用程序.

并行计算功能几乎完全由Wolfram语言编写, 因此是独立于机器的.
它们已经在 `Unix`, `Linux`, `Windows` 和 `Macintosh` 平台上进行了测试, 并且非常适合在多核机器上, 机器网格中(grid of machines),
或异质网络(heterogeneous network)中进行本地工作.
所有的 `客户端` 和 `应用程序代码` 都是分发式的, 所以不需要 `公共文件系统`.

```mathematica
 从而促进了并行编程技术的使用.
这些技术可以从许多技术进步中受益, 例如, 多核机器, 使用专门的硬件进行并行计算, 以及快速网络技术(fast network technology), 帮助运行连接的计算机网格(grid).

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

#### 局部内核

`本地内核连接方法`, 用于在与 `主Wolfram语言` 相同的计算机上运行并行的`worker`s.
它适用于多核环境, 是启动和运行并行计算的最简单方法.

只要你的处理器有一个以上的内核, 你就可以使用`本地内核`进行并行计算.

```mathematica
$ProcessorCount
Out[25]=2
```

### 轻量级网格

`轻量级网格连接`方法用于, 从运行 `主Wolfram语言` 之外的计算机上运行并行的`worker`s.
它使用 Wolfram 轻量级网格技术, 来启动远程机器上的 Wolfram 语言.
它适用于异质网络(heterogeneous network), 且没有管理技术的地方.

#### 集群集成

`集群集成连接方法`用于, 从运行 `主Wolfram语言` 之外的计算机上运行并行的`worker`s.
它与大量的 `第三方集群管理技术` 相整合.

#### 远程内核

`远程内核连接方法`用于, 在 `主Wolfram Language` 之外的计算机上运行并行`worker`s.
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

`轻量级网格连接方法`用于在不同于主Wolfram语言所在的计算机上, 运行并行`workers`.

它使用 Wolfram 轻量级网格技术在远程机器上启动 Wolfram 语言.
它适用于异质网络和没有管理技术的地方.

`轻量级网格连接`方式的配置是通过`并行首选项`完成的.
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

`集群集成连接`方法用于在不同于 `主Wolfram语言` 所在的计算机上运行并行`workers`.
它与大量的第三方集群管理技术相整合.

集群集成连接方法的配置是通过并行首选项完成的.
配置面板应该类似于下面的样子.
在这个设置中, 一个集群被配置为使用 `Windows Computer Cluster Server`, 头节点为 `clusterboss`. 每台机器都被设置为运行两个内核.

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

`轻量级网格连接方法`用于在不同于主Wolfram语言所在的计算机上, 运行并行`workers`.
它依赖于使用远程 shell 调用技术来启动, 通常更难配置和维护.

远程内核连接方法的配置是通过`并行首选项`完成的.
配置面板应该类似于下面的样子.
在这个配置中, 两台远程机器被配置为各自提供四个并行内核.

在Windows上, 默认设置是使用 `rsh` 来启动远程机器上的内核;其他平台使用 `ssh`.
这是因为 `ssh` 通常不适用于 `Windows`.
当然, 任何机器都必须被配置为允许从`主机器上`进行 `远程shell调用`.

你可以修改用于启动内核的启动命令.
在命令中, 你可以使用一些参数, 这些参数在命令实际使用之前, 会被填充.
例如, 参数 `` `1` ``是用来指正在使用的远程机器的名称.

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
+ `LaunchKernels[RemoteMachine[host,command,num]]`  ;使用command作为远程命令在机器主机上启动`n`个远程内核.

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

改变用于启动远程内核的命令可能更有用.
默认命令是由 `$RemoteCommand` 设置的, 它的典型设置如下.
注意, `rsh` 只在 `Windows` 上使用, 在其他平台上使用 `ssh`.

```mathematica
$RemoteCommand
"ssh -x -f -l `3` `1` wolfram -wstp -linkmode Connect `4` -linkname \
'`2`' -subkernel -noinit -pacletreadonly"
```

注意, 远程命令的参数在上一节中已经描述过了.
用户名参数是由 `$RemoteUsername` 设置的, 它默认设置为 `$Username`.

```mathematica
$RemoteUsername
Out[18]= "user"
```

你可以将命令直接传入 `RemoteMachine`. 下面的内容适合于 Wolfram System 的非标准安装.

```mathematica
LaunchKernels[
 RemoteMachine["remote1.wolfram.com",
  "rsh `1` -n -l `3` \"D:\\Mathematica\math -mathlink -linkmode \
Connect `4` -linkname `2` -subkernel -noinit >& /dev/null &", 4]]
Out[5]=KernelObject[2, "remote1.wolfram.com"]
```

另一种方法是在启动任何内核之前修改 `$RemoteKernel` 的值.

一旦你启动了并行内核, 那么就可以使用 Wolfram 语言的所有并行功能.

### WSTPServer 内核

`远程内核连接方法`用于在与主Wolfram Language不同的计算机上运行并行`workers`.
它可以连接到 `WSTPServer` 的运行实例, 并向这些服务器请求`内核`.

启用 `WSTPServer内核连接方法`是通过 `并行首选项`完成的.
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

## 并行计算,Parallel Evaluation

回顾一下, 与远程内核的连接, 如 `LaunchKernels` 所打开的, 被表示为 `内核对象`.
详见连接方法.
本节中的命令将 `并行内核` 作为参数, 并使用它们来进行计算.

### 底层的并行计算

+ `ParallelEvaluate[cmd,kernel]`; 将 `cmd` 发送给并行内核 `kernel`, 以进行计算, 然后等待结果并返回
+ `ParallelEvaluate[cmd,{kernels}]` ;将 `cmd` 发送到给定的并行内核`kernels`进行计算, 然后等待结果并返回
+ `ParallelEvaluate[cmd]` ; 将 `cmd` 发送到所有并行内核进行计算, 并返回结果列表;相当于 `ParallelEvaluate[cmd,Kernels[]]`

`ParallelEvaluate` 有属性 `HoldFirst`.

当涉及 `ParallelSubmit` 或 `WaitAll` 的`并发计算`(concurrent)正在进行时, 你不能使用 `ParallelEvaluate`.
请参阅 "并发性. 管理并行进程 "了解详情.
ParallelTools/tutorial/ConcurrencyManagingParallelProcesses#814127527

### 变量的值

在`本地主内核`上定义的`变量值`通常对`远程内核`不可用.
如果你发送的计算命令提到了一个变量, 它通常不会像预期那样工作.

在这些例子中, 一个命令总是被发送到单个内核上.

```mathematica
mykernel = First[Kernels[]]
Out[15]= KernelObject[1, "local"]
```

下面的程序将返回 `False`, 因为符号`a`很可能在远程内核上根本没有任何值.

```mathematica
a = 2;
ParallelEvaluate[a === 2, mykernel]
Out[17]= False
```

在`unevaluated`的命令中插入`变量值`的一个方便的方法是使用 `With`, 如下面的命令所示.
符号`a`被`2`替换, 然后表达式`2 === 2`被发送到远程内核, 在那里它被计算为 `True`.

```mathematica
With[{a = 2}, ParallelEvaluate[a === 2, mykernel] ]
Out[18]= True
```

如果你需要把`变量值`和`定义`送到远程内核中, 请使用 `DistributeDefinitions` 或`shared variables`.

`迭代器`(Iterators), 如 `Table` 和 `Do`, 在处理`迭代器变量`方面的工作方式是一样的.
因此, 像下面这样的语句将不会做预期的事情.

变量 `i` 在远程内核上没有`值`.

```mathematica
Table[ParallelEvaluate[IntegerQ[i], mykernel], {i, 1, 10}]
Out[20]= {False, False, False, False, False, False, False, False, False}.
```

你可以使用下面的命令来完成对远程内核的预期迭代.
这将`i`的值填入 `ParallelEvaluate` 参数中的 i.

```mathematica
Table[With[{i = i}, ParallelEvaluate[IntegerQ[i], mykernel]], {i, 1,  10}]
Out[21]={True, True, True, True, True, True, True, True, True, True}
```

模式变量, 常量和纯函数变量将在远程内核上如期工作.
下面三个例子中的每一个都会产生预期的结果.

在表达式被发送到`并行内核`之前, `纯函数`的`形式参数`将被插入函数体中.

```mathematica
Function[i, ParallelEvaluate[IntegerQ[i]]][5]
Out[22]= {True, True}
```

模式变量也被插入到定义的右侧:

```mathematica
f[i_] := ParallelEvaluate[IntegerQ[i]]
In[24]:= f[5]
Out[24]= {True, True}
```

常量也被插入:

```mathematica
With[{i = 5}, ParallelEvaluate[IntegerQ[i]]]
Out[25]= {True, True}
```

### 表达式的并行计算

Basic parallel dispatch of evaluations.

+ `ParallelCombine[f,h[e1,  e2, ...,en ],comb]`
并行计算`f[h[e1,   e2,  ...,en ]`, 将 `f[h[ei, e{i+1},...,e{i+k}]]]`分布到所有内核中, 然后用  `comb[]` 把结果合并
+ `ParallelCombine[f,h[e1, e2,...,en ]]` ; 如果 `h` 有 `Flat` 属性, 则默认组合器为 `h`, 否则为 `Join`.

`ParallelCombine[f,h[e1, e2, ...,en ],comb]` 将 `h[e1,   e2, ...,en` 分成 `h[ei,e{i+1}, ... . ]`,
然后并行计算 `f[h[ei,e{i+1},...,e{i+k}]`,
然后用 `comb[r1,r2,...,rm]` 合并结果.

`ParallelCombine` 的属性是 `HoldFirst`,
所以 `h[e1, e2,..., en]` 在并行化之前, 不会在主核上被计算.

#### ParallelCombine

`ParallelCombine`是一个通用的, 强大的命令,
其参数的默认值适用于计算容器的元素, 如列表和关联的函数(associative functions).

##### 计算列表类容器

如果将函数 `f` 应用于 `列表`的结果又是一个列表,
`ParallelCombine[f,h[[e, 1],[e, 2],...,[e, n]],comb]`只是将 `f`应用于输入列表的片段, 并将部分结果连在一起.

```mathematica
ParallelCombine[Prime, {1, 2, 3, 4, 5, 6, 7, 8, 9}]
Out[2]= {2, 3, 5, 7, 11, 13, 17, 19, 23}
```

结果与 `Prime[{1,2,3,4,5,6,7,8,9}]` 相同, 但计算是并行进行的.

如果函数是 `Identity`, `ParallelCombine` 只是并行地计算元素 `[e, i]`.

```mathematica
ParallelCombine[Identity, {1 + 2, 2 + 3, 3 + 4, 4 + 5, 5 + 6}]
Out[14]= {3, 5, 7, 9, 11}
```

如果将函数 `f`应用于列表的结果不是一个列表, 必须选择一个自定义的组合器.

函数 `Function[li,Count[li,_?OddQ]]` 计算一个列表中奇数元素的数量.
为了找到奇数元素的总数, 将部分结果加在一起.

```mathematica
ParallelCombine[
 Function[li, Count[li, _?OddQ]], {1, 2, 3, 4, 5, 6, 7, 8, 9}, Plus]
输出[17]=5
```

##### 计算关联操作

如果`h[e1,e2,...,en]`中的操作`h` 是associative(有属性Flat), 那么 identity

```mathematica
h[[e, 1],[e, 2],...,[e, n]]==h[h[[e, 1],[e, 2],...,[e, i]],h[[e, i+1],[e, i+2],...,[e, n]]]
```

holds;
由于默认的组合器是 `h` 本身, 该操作以一种自然的方式被并行化.
这里所有的数字都是并行添加的.

```mathematica
ParallelCombine[Identity, Unevaluated[1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9]]
输出[27]= 45

ParallelCombine[GCD, Unevaluated[GCD[4, 6, 8, 10]]]
Out[28]= 2
```

### 并行映射和迭代器

本节中的命令是 Wolfram 语言中并行编程的基础.

+ `ParallelMap[f,h[[e, 1],[e, 2],...]` ; 并行计算 `h[f[[e, 1]],[f[[e, 2]],...]`
+ `ParallelTable[expr,{i,[i, 0],[i, 1],di},{j,[j, 0],[j, 1],dj},...]]` ;
并行构建 `Table[expr,{i,[i, 0],[i, 1],di},{j,[j, 0],[j, 1],dj},...]`; 并行化是沿着第一个(最外层) `迭代器{i,[i, 0],[i, 1],di}`进行的
+ `ParallelSum[...]`, `ParallelProduct[...]` ;  并行计算和与乘.

并行 evaluation, mapping 和 tables.

`ParallelMap[f,h[e1,e2,...]]` 是 `f/@h[e1,e2, ...]` 的并行版本,
它以并行方式而不是 `顺序地` 计算单个`f[ei] `.

#### 副作用

除非你使用共享变量, 否则所进行的并行计算是完全独立的, 不能相互影响.
此外, 任何副作用, 例如对变量的赋值, 将会丢失, 当作为并行 `evaluations` 的一部分时.

并行求值的唯一影响是, 它在最后返回的结果.

### 例子

首先, 启动几个远程内核.

```mathematica
ParallelEvaluate[$ProcessID]
Out[1]={13952, 13600}
```

正弦函数被应用于给定的参数. 每次计算都是在一个远程内核上进行的.

```mathematica
ParallelMap[Sin, {0, \[Pi], 1.0}]
Out[2]= {0, 0, 0.841471}
```

这个特殊的计算几乎可以肯定是太琐碎了, 无法从并行计算中获益.
将`Sin[0], Sin[\[Pi]]`等表达式发送到远程内核, 并收集结果所需的开销, 将大于并行化带来的收益.

对`111...` 形式的整数进行`因式分解`需要更多的时间, 所以这种计算可以从并行化中受益.

```mathematica
ParallelMap[FactorInteger, (10^Range[20, 30] - 1)/9]
```

另外, 你也可以使用 `ParallelTable`.
这里产生了一个 `11...1/i` 的因子数的列表.

```mathematica
ParallelTable[{i,  Plus @@ (#[[2]] &) /@ FactorInteger[(10^i - 1)/9]}, {i, 20,  30}] // TableForm
```

### 自动分发定义

`ParallelTable` 等并行命令会自动分发所需的值和函数, `有效地使用DistributeDefinitions`.

对于这个 `parallel table`, 函数 `f` 和迭代器的边界 `n` 将在子核上求值, 所以它们的定义需要被`分发`以使其工作.

```mathematica
f[x_, y_] := x^y
{m, n} = {4, 2};
ParallelTable[f[i, j], {i, m}, {j, n}]
Out[3]= {{1, 1}, {2, 4}, {3, 9}, {4, 16}}
```

这种自动分发发生在你以交互方式定义的任何函数和变量上, 在同一个笔记本中(严格地说, 在`默认上下文中`的所有符号).
来自其他`contexts`的定义, 如来自`包`的函数, 不会自动分发.

要获得对`定义分发`的更多控制, 可以用以下方法禁用自动分发.

```mathematica
$DistributedContexts = None;
```

然后明确地分发所需符号的定义.

```mathematica
kid := $KernelID
DistributeDefinitions[kid]
ParallelEvaluate[kid]
Out[18]= {1, 2, 3, 4}
```

如果我们不分发一个符号的定义, 它通常会从并行的子内核中未被计算而返回, 但随后在主内核上被进一步计算.
这段代码看起来是有效的, 但实际上是按顺序计算的.

```mathematica
mp1[i_] := PrimeQ[2^i - 1]
AbsoluteTiming[ParallelTable[mp1[i], {i, 60000, 60020}]]
Out[44]={60.722,...}
```

这个等价代码是`分发过的`, 因此计算是并行发生的.

```mathematica
mp2[i_] := PrimeQ[2^i - 1];
DistributeDefinitions[mp2];
AbsoluteTiming[ParallelTable[mp2[i], {i, 60000, 60020}]]
Out[45]= {28.5367, ...}
```

### 自动并行化

`Parallelize[cmd[list,arguments ...]]` 能识别 `cmd` 是否为某类 Wolfram Language 函数, 这类函数对`列表`或其他`长表达式`进行操作, 能够以容易的方式并行化. 如果识别成功将自动执行`并行化`.

```mathematica
Parallelize[Count[{1, 2, 3, 4, 5, 6, 7}, _?PrimeQ]]
Out[34]= 4
Parallelize[Map[f, {a, b, c, d, e, f}]]
Out[35]= {f[a], f[b], f[c], f[d], f[e], f[f] }
Parallelize[{a, b, c, d} . ( {
    {w1, w2},
    {x1, x2},
    {y1, y2},
    {z1, z2}
   } )]
Out[36]= {a w1 + b x1 + c y1 + d z1, a w2 + b x2 + c y2 + d z2}
```

并非所有这些命令的使用都能被并行化.
如果有必要, 会产生一个信息, 并在主内核上按顺序进行计算.

```mathematica
Parallelize[Apply[f, g[a, b, c, d], 0]]
Parallelize::nopar1: Apply[f,g[a,b,c,d],0] cannot be parallelized; proceeding with sequential evaluation.
Out[39]= g[a, b, c, d]
```

## 并发:管理并行进程,Concurrency: Managing Parallel Processes

ParallelTools/tutorial/ConcurrencyManagingParallelProcesses

### 进程和处理器

一个`进程`(`process`)只是一个正在计算的 `Wolfram Language 表达式`.
`处理器`(`processor`)是一个执行这种计算的并行内核.

在 `并行计算` 教程中讨论的 `ParallelEvaluate` 命令将把计算发送到一个明确给定的处理器, 这就要求你自己跟踪可用的处理器和进程.

本教程中讨论的`调度函数`(scheduling)为你执行这些功能.
你可以创建任何数量的进程, 远超过可用处理器的数量.
如果创建的`进程`多于`处理器`的数量, 剩余的进程将被排队, 并在有`处理器可用时` 再提供服务.

### 启动和等待进程

两个基本命令是 `ParallelSubmit[expr]` 和 `WaitAll[pid]`,
前者用于在任何可用的`处理器`上, 排队(line up)计算一个表达式, 后者用于等待某个`进程`结束.

队列中的每个`进程`都由其唯一的`计算 ID`(即 `eid`)来识别.

+ `ParallelSubmit[cmd]` ; 将 `cmd` 提交给远程内核进行计算, 并返回队列中作业的 `eid`.
+ `ParallelSubmit[{vars ...},cmd]` ; 在将 `cmd` 发送到远程内核之前, 使用`vars`的本地值建立`闭包`.
+ `WaitAll[eid]` ; 等待给定的`进程`结束, 并返回其结果.
+ `WaitAll[{eid1, eid2,...}]` ; 等待所有给定的进程, 并返回`结果的列表`.
+ `WaitAll[expr]` ; 等待 `expr` 中包含的所有 `evaluation IDs` 完成,
并将各自进程的结果代入`expr`.
+ `WaitNext[{eid1, eid2,...}]` ; 等待给定进程中的某一个完成.
然后返回 `{res,id,ids}`, 其中 `id` 是已完成进程的 `eid`, `res` 是其结果,
`ids` 是剩余 `eid` 的列表.

`WaitNext` 不是确定的. 它返回任意一个已经结束的进程.
如果没有进程完成, 它就等待, 直到有一个结果出现.
`WaitNext` 的结果中的第三个元素, 适合作为另一个 `WaitNext` 调用的参数.

函数 `ParallelSubmit` 和 `WaitAll` 实现了`并发`(concurrency).
你可以启动任意多的 `进程`, 它们最终都会在任何可用的 `远程处理器` 上被计算.
当你需要一个特定的结果时, 你可以等待任何特定的 `eid`, 或者你可以重复调用 `WaitNext` 来等待所有的结果.

## Basic Usage

为了尝试这里的例子, 请启动几个并行的内核.

```mathematica
$KernelCount
Out[2]= 4
```

将计算 `1+1` 进行排队, 以便在远程内核上处理.
注意 `Queue` 有一个属性 `HoldAll`, 以防止在`排队前` 表达式被计算.
`Queue` 返回的值是被排队进程的 `evaluation ID(eid)`.

```mathematica
j1 = ParallelSubmit[1 + 1]
Out[2]= ...
```

对一个进程进行排队后, 可以进行其他计算, 并最终收集结果.
如果结果还没有出来, `WaitAll` 将等待它.

```mathematica
WaitAll[j1]
Out[3]=2
```

你可以将好几个`进程`加入队列.
在这里, 表达式 `1^2`, `2^2`, ..., `5^2` 被排队等待计算.

```mathematica
pids = Function[i, ParallelSubmit[i^2]] /@ {1, 2, 3, 4, 5}
Out[4]= {...}
```

接下来, 等待任何一个`进程`完成.

```mathematica
{res, pid, pids} = WaitNext[pids]
{...}
```

注意 `pids` 被重新分配到, 剩余 `evaluation ID` 的列表中.
重复之前的计算, 直到 `pids` 列表变空:

```mathematica
{res, pid, pids} = WaitNext[pids]
{...}
```

你也可以等待所有剩余的进程完成.

```mathematica
WaitAll[pids]
Out[7]= {9, 16, 25}
```

### 关于使用变量的说明

如果 `ParallelSubmit[e]` 中的表达式 `e` 涉及到 `被赋值` 的变量,
必须注意确保远程内核有相同的变量值定义.

除非你使用 `DistributeDefinitions` 或 `共享变量`, 否则本地定义的变量将无法提供给远程内核.
更多信息请参见教程 `并行计算` 中的 `Values of Variables`.

下面是一些可能出现问题的常见情况.
给`本地主内核`中的变量 `a` 赋值 `2`.

```mathematica
a = 2;
```

你想在一个远程内核上计算表达式 `Head[a]`.
结果不是 `Integer`, 就像在本地内核上一样, 因为在远程内核上, 符号 `a` 没有`值`.

```mathematica
Head[a]
Integer
WaitAll[ParallelSubmit[Head[a]]]
Symbol
```

你可以使用`局部常量` 记录 `a` 的值,`常量` 甚至可以也叫做`a`,
`a` 的值 将以文本方式插入 `ParallelSubmit` 的参数中.

```mathematica
With[{a = a}, WaitAll[ParallelSubmit[Head[a]]]]
Out[4]= Integer
```

为了使这种常见的情况更简单, 你可以在 `ParallelSubmit` 中使用可选的首参数来声明变量.
然后这些变量将被插入第二个参数的表达式中.

```mathematica
WaitAll[ParallelSubmit[{a}, Head[a]]]
Out[5]= Integer
```

`ParallelSubmit[{vars ...},expr]` 这个语法在设计上类似于 `Function[{vars ...},expr]`.
两者都形成`闭包`(closures), `变量`被绑定到它们的`值`上.

`迭代器`变量的行为也是如此.
在下面两个输出中, 并行计算没有给出正确的结果.

```mathematica
Table[i^2, {i, 1, 10}]
Out[6]= {1, 4, 9, 16, 25, 36, 49, 64, 81, 100}
WaitAll[Table[ParallelSubmit[i^2], {i, 1, 10}]]
Out[7]= {i^2, i^2, i^2, i^2, i^2, i^2, i^2, i^2, i^2, i^2}
```

将 `迭代器变量`插入并行计算, 以 `局域常量` 的方式,
或者声明一个`闭包`, 以确保得到正确的结果, 如下面的命令.

```mathematica
WaitAll[Table[ParallelSubmit[{i}, i^2], {i, 1, 10}]]
Out[8]= {1, 4, 9, 16, 25, 36, 49, 64, 81, 100}.
```

请注意, `ParallelTable[]` 正确处理了 `迭代器变量`.

```mathematica
ParallelTable[i^2, {i, 1, 10}]
Out[9]= {1, 4, 9, 16, 25, 36, 49, 64, 81, 100}.
```

### 底层函数

`` Parallel`Developer` ``上下文包含控制`进程队列`(process queue)的函数.

+ `$Queue`  ;   用 `ParallelSubmit` 提交, 但尚未分配给可用内核的计算列表
+ `$QueueLength` ; 给出输入队列 `$Queue` 的长度
+ `ResetQueues[]`   ; 等待所有 `正在运行` 的进程, 并放弃任何排队的进程
+ `QueueRun[]`  ; 从所有内核收集完成的`evaluations`, 并从队列中分配新的计算到子核上.

如果至少向内核提交了一个计算, 或从内核收到一个结果,  则 `QueueRun[]` 返回 `True`, 否则返回 `False`.

通常情况下, 你不应该自己去运行 `QueueRun`. 它在 `WaitAll` 和其他函数的适当位置被调用.
只有当你为一个`并发程`序实现自己的`主循环`(main loop)时, 你才需要它.

访问开发者函数:

```mathematica
Needs["Parallel`Developer`"]
```

提交一些计算结果:

```mathematica
eids = Table[ParallelSubmit[{i}, PrimeQ[i]], {i, 9}]
Out[34]= {...}
```

每次你调用 `QueueRun[]` 时, 你可以看到上面显示的计算状态的变化:

```mathematica
QueueRun[]
True

QueueRun[]
True
```

这是仍在等待被服务(serviced)的 `evaluations` 的数量:

```mathematica
$QueueLength
1
```

最后, 让它们运行到完成.

```mathematica
WaitAll[eids]
{False, True, True, False, True, False, True, False, False}.
```

### 使用 Evaluation ID

`WaitAll[{pid1, pid2,...}]` 仅仅是并行计算的 `一般机制` 的简单形式.

`WaitAll` 可以接受任意表达式, 后者的参数中包含`evaluation IDs`,
并将等待所有相关进程完成.
然后, 这些 `pids` 将被相应`进程`的结果所取代.

你可以把 `WaitAll` 看作是 `ParallelSubmit` 的逆过程;

也就是说, `WaitAll[ParallelSubmit[expr]]` 给出使用远程内核计算的 `expr`,
结果和 `expr` 在本地的计算相同.
此外, `WaitAll[... ParallelSubmit[e1]... ParallelSubmit[en]...]` 等同于 `...e1...en...`, 其中每个 `ei` 被并行地计算.
这里的省略号代表一个任意的周边计算.

`ParallelSubmit` 的`实例`所产生的 `pid` 应该被保持原样,
在 `WaitAll` 执行其任务之前, 既不应该被销毁, 也不应该被重复.
原因是它们中的每一个都代表一个正在进行的并行计算, 其结果应该被`准确地收集一次`.

下面是保存 `pids` 的表达式的例子.

+ 列表中的 `pids` 是安全的, 因为 `列表操作` 不对其参数做任何处理;
它只是将它们放在一起. 出于同样的原因, 嵌套的列表也是安全的.

    ```mathematica
    WaitAll[{ParallelSubmit[e1], ...,ParallelSubmit[en]}]
    ```

+ `Pids` 是符号对象(symbolic objects), 不受 `Plus` 的影响.
它们可能被重新排序, 但这并不重要. 大多数算术操作是安全的.

    ```mathematica
    WaitAll[ParallelSubmit[e1]+...+ParallelSubmit[en]]
    ```

+ 将涉及 `ParallelSubmit` 的函数`Mapping` 到列表上是安全的, 因为其结果将包含 `pids` 的列表.

    ```mathematica
    WaitAll[Map[ParallelSubmit[...# ...]&, {e1, ..., en}]]
    ```

+ `Table` 返回 `pids` 的列表, 因此是安全的.

    ```mathematica
    WaitAll[Table[ParallelSubmit[{i},expr],{i,1,10}]]
    ```

不安全的表达式的例子包括如下:

+ `Head` 操作会破坏符号化的 `pid`, 其他结构性操作如 `Length`, `ByteCount` 等也是如此.

    ```mathematica
    WaitAll[Head[ParallelSubmit[e]]]
    ```

+ 将一个 `pid` 乘以 `0` 会破坏它.

```mathematica
WaitAll[0*ParallelSubmit[e]]
```

+ `Do` 不返回任何东西, 所以所有的 `pid` 都会丢失. 类似的情况是`Scan`.

```mathematica
WaitAll[Do[ParallelSubmit[{i},expr],{i,1,10}]].
```

如果 `pid` 被破坏或重复, 要从这类计算中恢复, 可以使用 `AbortKernels[]` 命令.

进程ID的属性(在开发者上下文中).

+ `ProcessID[pid]`  ; 识别进程的唯一整数
+ `Process[pid]` ; 代表进程的表达式
+ `Scheduling[pid]` ; 分配给进程的优先级
+ `ProcessState[pid]` ; 进程的状态: 排队, 运行, 完成

```mathematica
Needs["Parallel`Developer`"]
```

这里, 有几个进程在排队.

```mathematica
ids = ParallelSubmit[2 + #, Scheduling -> #] & /@ Range[6]
{...}
```

因为`调度器`(scheduler)还没有运行, 所有的任务都在排队等待计算.

```mathematica
TableForm[
 Function[pid,
   Through[{OutputForm, ProcessID, Process, Scheduling, ProcessState}[
     pid]]] /@ ids,
 TableHeadings -> {None, {"pid", "ID", "expr", "prio", "state"}}]
```

为了演示它是如何工作的, 手动调用调度器.

```mathematica
QueueRun[]; QueueRun[]
True
```

现在一些进程正在可用的`处理器`上运行; 有些可能已经完成.
请注意, 这些信息中的某些部分, 在默认的`evaluations` 的输出形式也能得到.
我们再次查看

```mathematica
TableForm[
 Function[pid,
   Through[{ProcessID, Process, Scheduling, ProcessState}[pid]]] /@
  ids, TableHeadings -> {None, {"ID", "expr", "prio", "state"}}]
```

`WaitAll[]` 调用`调度器`(scheduler), 直到所有进程结束并返回其结果.
注意, 默认的队列类型时不使用 `优先级`(`priorities`).

```mathematica
WaitAll[ids]
{3, 4, 5, 6, 7, 8}
```

### Examples

在运行这些例子之前, 请确保你可以运行并行内核.

```mathematica
$KernelCount
Out[55]=4
```

#### 无限计算

如果你想验证多项式 $sum_{i=1}^{n+1} i x^{i-1}$, 对于 $n=1,2,cdots$
都是不可约的(irreducible, 一个开放猜想), 你可以用 `IreduciblePolynomialQ` 来测试它们.
这个计算将永远持续下去.
要停止它, 按 `CMD+.` 或选择 `Kernel>Abort Evaluation` 来中止本地计算.
终止后, 收集任何等待的结果, 如下所示.

下面是 degree 为 `n` 的, `x` 的多项式的定义.

```mathematica
poly[n_, x_] := Sum[i x^(i - 1), {i, 1, n + 1}]
```

然后你将定义告知所有远程内核:

```mathematica
DistributeDefinitions[{poly}];
```

现在你可以开始计算了, 要求它在进行时打印每个结果.
要停止计算, 可以选择`Kernel \[FilledRightTriangle] Abort Evaluation` 或按 `CMD+.`.

在这样的例子中, 明确调用 `QueueRun[]` 是必要的, 因为你要对自己的`调度细节`(scheduling)进行编程.

```mathematica
d = 100; ids = {}; (*eids 初始化*)
While[True,
 While[$QueueLength == 0,
  AppendTo[ids,
   ParallelSubmit[{d}, {d, IrreduciblePolynomialQ[poly[d, x]]}]];
    d++;
  If[! QueueRun[], Break[]]; (*如果*)
  ];
 If[Length[ids] > 0, (*如果队列长度大于0*)
  {res, id, ids} = WaitNext[ids]; (*收集这一轮的结果*)
  Print[res]
  ];
 ]

{100,True}
{101,True}
...
{135,True}{135,True}
$Aborted
```

不要忘记在中断后收集`孤儿进程`(orphaned), 或者使用 `AbortKernels[]`.

```mathematica
WaitAll[ids]
Out[60]= {{134, True}, {136, True}, {137, True}, {138, True}, {139, True}}
```

### 自动生成进程, Automatic Process Generation

并行化很多种 `computation`的一般方法是,
用 `Composition[ParallelSubmit,g]` 代替泛函(functional)操作中出现的函数 `g`.
这个新的操作, 将导致调用函数 `g` 的所有实例被排队进行`并行计算`.

这个`组合`的结果是一个 `进程ID`(pid), 它将嵌入, `g`所在的外部计算所构建的结构中.
为了把 `g` 的计算结果放回去, 把整个表达式包在 `WaitAll` 中.
这将用相应进程返回的结果, 替换其表达式内的任何 `pid`.

下面是这种`functional compositions`的几个例子.

#### 并行映射

并行版本的 `Map` 很容易开发.
顺序(sequential)的 `Map` 将函数 `f` 包裹到列表的每个元素上.

```mathematica
Map[f, {a, b, c, d}]
Out[1]= {f[a], f[b], f[c], f[d]}
```

只需使用 `Composition[ParallelSubmit,f]` 而不是 `f`, 来把所有 mapping 安排为并行执行.
其结果是`pids` 的列表.

```mathematica
Map[Composition[ParallelSubmit, f], {a, b, c, d}]
```

最后, 简单地`等待`进程结束. 每个 `pid` 都将被替换成其相关进程的结果.

```mathematica
WaitAll[%]
Out[3]= {f[a], f[b], f[c], f[d]}
```

#### 并行内积

为了观察并行对 `符号内积` 的实现, 假设你想要一个广义的内积,
其中 `d` 是 `a` 的末维度, 同时是`b`的首维度; 把 `p` 想象成 `Plus`, 把 `t` 想象成 `Times`.

$$(a.b)_{i_1 i_2 \cdots i_{n-1} k2 k3 \cdots k_m}= p[
    t[a_{i_1 i_2 \cdots  i_{n-1}}, b_{1 k_2 k_3 \cdots k_m}],
    t[a_{i_1 i_2 \cdots  i_{n-1}d}, b_{k_2 k_3 \cdots k_m}]]$$

下面是一个 `d=2` 的例子.

```mathematica
Inner[t, Array[a, {2, 2}], Array[b, {2, 2}], p]
```

你可以用 `Composition[ParallelSubmit,p]` 代替前面表达式中的 `p`,
来排队 `p` 的所有计算, 以便并发执行. 其结果是 `进程ID` 组成的张量.

```mathematica
Inner[t, Array[a, {2, 2}], Array[b, {2, 2}],  Composition[ParallelSubmit, p]]
```

现在, 简单地等待这个表达式中的所有进程.

```mathematica
WaitAll[%]
```

#### 并行Tables与Sums

这段代码生成了一个 `5x5`的随机数矩阵, 其中每一行都是并行计算的.

```mathematica
WaitAll[Table[ParallelSubmit[RandomReal[{0, 1}, {5}]], {5}]]
```

一个`sum`, 其元素是并行计算的. 该`sum`的每个元素都是一个数值积分.

```mathematica
WaitAll[Sum[ ParallelSubmit[{k}, NIntegrate[x^(-1 - 1/k), {x, 1, \[Infinity]}]], {k, 1, 8}]]
Out[8]=36.
```

下面是相应的并行精确结果表. $\int_0^\infty \frac{1}{x^{1/k+1}}$ 的值是`k`.

```mathematica
TableForm[ WaitAll[Table[ParallelSubmit[{k}, {k,Integrate[x^(-1 - 1/k),
{x, 1, \[Infinity]}]}], {k, 1, 8}]],  TableDepth -> 2]
```

#### 与Parallelize的比较

并行 `mapping`, `tables` 和 `products` 在 `并行计算` 中已经介绍过了.

这些函数在`方法选项`的控制下, 将任务分成`几批`, 每批有几个子问题.
本节中的函数为每个子问题生成一个`evaluation`.
这种划分相当于设置`Method->"FinestGrained"`.

如果所有子问题花费的时间相同, 那么 `ParallelMap[]` 和 `ParallelTable[]`等函数会更快.
但是, 如果子问题的计算时间不同, 而且不容易事先估计,
那么使用本节所述的 `WaitAll[... ParallelSubmit[] ...]`或者使用等效的`方法选项`设置可能会更好.

如果产生的进程数量大于远程内核的数量, 这个方法会自动执行`负载均衡`(load balancing),
因为一旦前一个作业完成, 新的作业就会被分配到此内核, 所有的内核都会一直保持忙碌.

### Tracing

为了观察进程是如何`scheduled`的, 你可以使用 `tracing`.
为了使用这些功能, 你必须在加载`toolkit`和 `启动并行内核`之前, 加载`调试包`(debugging package).

```mathematica
Needs["Parallel`Debug`"]
```

现在你可以启用 `队列跟踪`(Queueing tracing)功能.

```mathematica
SetOptions[$Parallel, Tracers -> {Queueing}]
Out[6]= {Tracers -> {Queueing}}
```

这里有几个进程被排队, 显示了队列的大小是如何增长的.

```mathematica
ids = ParallelSubmit[2 + #] & /@ Range[4]
Queueing: Subscript[eid, 186][2+1] queued (20)
```

`WaitAll[]` 调用调度器, 调度器将排队的作业发送到空闲的处理器, 收集结果, 并将其交还给应用程序.

```mathematica
WaitAll[ids]
Queueing: Subscript[eid, 1][2+1] sent to kernel 4
...
Queueing: Subscript[eid, 1][2+1] received from kernel 4
...
Queueing: Subscript[eid, 1][2+1] dequeued
...
```

要在完成后关闭 `tracing`, 请使用

```mathematica
SetOptions[$Parallel, Tracers -> {}]
Out[9]= {Tracers -> {}}
```

## 虚拟共享内存

### 同步

一个简单的实现`deadlock`的例子.

```mathematica
Parallelize[
 {
      (*获得锁a*)
  CriticalSection[{lcka},
   CriticalSection[{lckb},
    Pause[Random[]];
    Print["A done"]
    ]
   ],
   (*获得锁b*)
  CriticalSection[{lckb},
   CriticalSection[{lcka},
    Pause[Random[]];
    Print["B done"]
    ]
   ]
  }
 ]
```

## 推送定义到远程

## 并行计算

## 并发: 管理多进程

每个要计算的表达式就是一个进程(`process`),

并发控制的常用函数:

+ `ParallelSubmit`; 提交表达式进行并发运算
+ `WaitAll`;  等待所有并发计算完成, 相当于并发任务的启动按钮
+ `WaitNext`; 运行下一个并发任务, 相当于并发任务的步进按钮

+ 还有并发任务的开发者工具,导入工具包;

```mathematica
Needs["Parallel`Developer`"]
```

可以使用底层函数:

`ProcessID[pid]`:表示进程的整数
`Process[pid]`: 对应进程的表达式
`Scheduling[pid]`:分发给进程的优先级
`ProcessState[pid]`进程的运行状态: `queued`, `running`, `finished`
