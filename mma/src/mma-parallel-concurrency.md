# 并发:管理并行进程

ParallelTools/tutorial/ConcurrencyManagingParallelProcesses

## 进程和处理器

`进程`(`process`)只是一个正在计算的 `Wolfram Language 表达式`.
`处理器`(`processor`)是一个执行这种计算的 `并行内核`.

在 `并行计算` 教程中讨论的 `ParallelEvaluate` 命令将把计算发送到一个明确给定的处理器, 这就要求你自己跟踪可用的处理器和进程.

本教程中讨论的`调度函数`(scheduling)为你执行这些功能.
你可以创建任何数量的进程, 远超过可用处理器的数量.
如果创建的`进程`多于`处理器`的数量, 剩余的进程将被排队, 并在有`处理器可用时` 再提供服务.

## 启动和等待进程

两个基本命令是 `ParallelSubmit[expr]` 和 `WaitAll[pid]`,
前者用于在任何可用的`处理器`上, 排队(line up)计算一个表达式, 后者用于等待某个`进程`结束.

队列中的每个`进程`都由其唯一的`计算 ID`(即 `eid`)来识别.

+ `ParallelSubmit[cmd]` ; 将 `cmd` 提交给远程内核进行计算, 并返回队列中作业的 `eid`.
+ `ParallelSubmit[{vars ...},cmd]` ; 在将 `cmd` 发送到远程内核之前, 使用`vars`的本地值建立`闭包`.
+ `WaitAll[eid]` ; 等待给定的`进程`结束, 并返回其结果.
+ `WaitAll[{eid1, eid2,...}]` ; 等待所有给定的进程, 并返回`结果的列表`.
+ `WaitAll[expr]` ; 等待 `expr` 中包含的所有 `evaluation IDs` 完成, 并将各进程的结果代入`expr`.
+ `WaitNext[{eid1, eid2,...}]` ; 等待给定进程中的某一个完成.
    然后返回 `{res,id,ids}`, 其中 `id` 是已完成进程的 `eid`, `res` 是其结果, `ids` 是剩余 `eid` 的列表.
    + `WaitNext` 不是确定的. 它返回任意一个已结束的进程.  如果没有进程完成, 它就等待, 直到有一个结果出现.
    + `WaitNext` 的结果中的第三个元素, 适合作为参数传递给另一个 `WaitNext` 调用.

函数 `ParallelSubmit` 和 `WaitAll` 实现了`并发`(concurrency).
你可以启动任意多的 `进程`, 它们最终都会在任何可用的 `远程处理器` 上被计算.
当你需要一个特定的结果时, 你可以等待任何特定的 `eid`, 或者你可以重复调用 `WaitNext` 来等待所有的结果.

```mathematica
ParallelSubmit[{var1, var2, ... }, expr]; 在提交 `expr` 进行计算之前, 将 `var_i` 的 `当前值` 代入 `expr`
```

`ParallelSubmit` 有属性 `HoldAllComplete`, `Evaluate` 也无法强迫它计算.
同理, `Association` 也有属性 `HoldAllComplete`, 放入`Association`容器的并行运算表达式, 无法被启动,
需要解开 `Association` 封装才可以调用 `WaitAll` 进行计算.

## 基本例子

为了尝试这里的例子, 请启动几个 `并行内核`.

```mathematica
$KernelCount
Out[2]= 4
```

将计算 `1+1` 进行排队, 以便在 远程内核 上处理.
注意 `Queue` 有一个属性 `HoldAll`, 以防止在 `排队前` 表达式被计算.
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

## 关于使用变量的说明

如果 `ParallelSubmit[e]` 中的表达式 `e` 涉及到 `被赋值` 的变量,
必须注意确保 `远程内核` 具有相同的变量值定义.

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

## 底层函数

`` Parallel`Developer` ``上下文包含控制`进程队列`(process queue)的函数.

+ `$Queue`;   用 `ParallelSubmit` 提交, 但尚未分配给可用内核的计算列表
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

## 使用 Evaluation ID

`WaitAll[{pid1, pid2,...}]`, 仅仅是并行计算的 一般机制的简化形式.

`WaitAll` 可以接受任意表达式, 后者的参数中包含`evaluation IDs`, 并将等待所有相关进程完成.
然后, 这些 `pids` 将被相应`进程`的结果所取代.
你可以把 `WaitAll` 看作是 `ParallelSubmit` 的逆过程;

也就是说, `WaitAll[ParallelSubmit[expr]]` 给出使用远程内核计算的 `expr`, 结果和 `expr` 在本地的计算相同.
此外, `WaitAll[... ParallelSubmit[e1]... ParallelSubmit[en]...]` 等同于 `...e1...en...`, 其中每个 `ei` 被并行地计算.
这里的省略号代表任意的周边计算(surrounding computation).

`ParallelSubmit` 的`实例`所产生的 `pid` 应该被保持原样,
在 `WaitAll` 执行其任务之前, 既不应该被销毁, 也不应该被重复.
原因是它们中的每一个都代表一个正在进行的并行计算, 其结果应该被`准确地收集一次`.

下面是保持 `pids` 完整的表达式的例子:

+ `列表` 中的 `pids` 是安全的, 因为 `列表操作` 不对其参数做任何处理;
它只是将它们放在一起. 出于同样的原因, `嵌套的列表` 也是安全的:

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

## 开发者工具

可以使用 `底层函数`, `进程ID` 的属性有(在 `开发者上下文` 中):

+ `ProcessID[pid]`  ; 进程的身份证, 唯一整数
+ `Process[pid]` ; 描写 `进程` 的具体表达式
+ `Scheduling[pid]` ; 分配给进程的 `优先级`
+ `ProcessState[pid]` ; 进程的运行状态: `queued`, `running`, `finished`

先导入工具包;

```mathematica
Needs["Parallel`Developer`"]
```

这里, 有几个进程在`排队`.

```mathematica
ids = ParallelSubmit[2 + #, Scheduling -> #] & /@ Range[6]
{...}
```

因为 `调度器`(scheduler)还没有运行, 所有的任务都在排队等待计算.

```mathematica
TableForm[
 Function[pid,
   Through[{OutputForm, ProcessID, Process, Scheduling, ProcessState}[
     pid]]] /@ ids,
 TableHeadings -> {None, {"pid", "ID", "expr", "prio", "state"}}]
```

为了演示它的工作机制, 我们手动调用 `调度器`.

```mathematica
QueueRun[]; QueueRun[]
True
```

现在一些进程正在可用的 `处理器` 上运行; 有些可能已经完成.
请注意, 这些信息中的某些部分, 在默认的 `evaluations` 的输出形式也能得到. 我们再次查看:

```mathematica
TableForm[
 Function[pid,
   Through[{ProcessID, Process, Scheduling, ProcessState}[pid]]] /@
  ids, TableHeadings -> {None, {"ID", "expr", "prio", "state"}}]
```

`WaitAll[]` 调用 `调度器`(scheduler), 直到所有进程结束, 并返回其结果.
注意, 默认的队列类型时不使用 `优先级`(`priorities`).

```mathematica
WaitAll[ids]
{3, 4, 5, 6, 7, 8}
```

## Examples

在运行这些例子之前, 请确保你可以运行并行内核.

```mathematica
$KernelCount
Out[55]=4
```

## 无限计算

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

## 自动生成进程, Automatic Process Generation

并行化很多种 `computation`的一般方法是,
用 `Composition[ParallelSubmit,g]` 代替泛函(functional)操作中出现的函数 `g`.
这个新的操作, 将导致调用函数 `g` 的所有实例被排队进行`并行计算`.

这个`组合`的结果是一个 `进程ID`(pid), 它将嵌入, `g`所在的外部计算所构建的结构中.
为了把 `g` 的计算结果放回去, 把整个表达式包在 `WaitAll` 中.
这将用相应进程返回的结果, 替换其表达式内的任何 `pid`.

下面是这种`functional compositions`的几个例子.

## 并行映射

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

## 并行内积

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

## 并行Tables与Sums

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

## 与Parallelize的比较

并行 `mapping`, `tables` 和 `products` 在 `并行计算` 中已经介绍过了.

这些函数在`方法选项`的控制下, 将任务分成`几批`, 每批有几个子问题.
本节中的函数为每个子问题生成一个`evaluation`.
这种划分相当于设置`Method->"FinestGrained"`.

如果所有子问题花费的时间相同, 那么 `ParallelMap[]` 和 `ParallelTable[]`等函数会更快.
但是, 如果子问题的计算时间不同, 而且不容易事先估计,
那么使用本节所述的 `WaitAll[... ParallelSubmit[] ...]`或者使用等效的`方法选项`设置可能会更好.

如果产生的进程数量大于远程内核的数量, 这个方法会自动执行`负载均衡`(load balancing),
因为一旦前一个作业完成, 新的作业就会被分配到此内核, 所有的内核都会一直保持忙碌.

## Tracing

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
