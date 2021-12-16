# 并行计算

ParallelTools/tutorial/ParallelEvaluation

回顾一下, 与远程内核的连接, 如 `LaunchKernels` 所打开的, 被表示为 `内核对象`. 详见连接方法.
本节中的命令将 `并行内核` 作为参数, 并使用它们来进行计算.

## 底层的并行计算

+ `ParallelEvaluate[cmd,kernel]`; 将 `cmd` 发送给并行内核 `kernel`, 以进行计算, 然后等待结果并返回
+ `ParallelEvaluate[cmd,{kernels}]` ;将 `cmd` 发送到给定的并行内核`kernels`进行计算, 然后等待结果并返回
+ `ParallelEvaluate[cmd]` ; 将 `cmd` 发送到所有并行内核进行计算, 并返回结果列表;相当于 `ParallelEvaluate[cmd,Kernels[]]`

`ParallelEvaluate` 有属性 `HoldFirst`.

当涉及 `ParallelSubmit` 或 `WaitAll` 的`并发计算`(concurrent)正在进行时, 你不能使用 `ParallelEvaluate`.
请参阅 "并发性. 管理并行进程 "了解详情.
ParallelTools/tutorial/ConcurrencyManagingParallelProcesses#814127527

## 变量的值

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

## 表达式的并行计算

Basic parallel dispatch of evaluations.

+ `ParallelCombine[f,h[e1,  e2, ...,en ],comb]`
并行计算`f[h[e1,   e2,  ...,en ]`, 将 `f[h[ei, e{i+1},...,e{i+k}]]]`分布到所有内核中, 然后用  `comb[]` 把结果合并
+ `ParallelCombine[f,h[e1, e2,...,en ]]` ; 如果 `h` 有 `Flat` 属性, 则默认组合器为 `h`, 否则为 `Join`.

`ParallelCombine[f,h[e1, e2, ...,en ],comb]` 将 `h[e1,   e2, ...,en` 分成 `h[ei,e{i+1}, ... . ]`,
然后并行计算 `f[h[ei,e{i+1},...,e{i+k}]`,
然后用 `comb[r1,r2,...,rm]` 合并结果.

`ParallelCombine` 的属性是 `HoldFirst`,
所以 `h[e1, e2,..., en]` 在并行化之前, 不会在主核上被计算.

## ParallelCombine

`ParallelCombine` 是一个通用的, 强大的命令,
其参数的默认值适用于计算容器的元素, 如列表和关联的函数(associative functions).

### 计算列表类容器

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

### 计算关联操作

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

## 并行映射和迭代器

本节中的命令是 Wolfram 语言中并行编程的基础.

+ `ParallelMap[f,h[[e, 1],[e, 2],...]` ; 并行计算 `h[f[[e, 1]],[f[[e, 2]],...]`
+ `ParallelTable[expr,{i,[i, 0],[i, 1],di},{j,[j, 0],[j, 1],dj},...]]` ;
并行构建 `Table[expr,{i,[i, 0],[i, 1],di},{j,[j, 0],[j, 1],dj},...]`; 并行化是沿着第一个(最外层) `迭代器{i,[i, 0],[i, 1],di}`进行的
+ `ParallelSum[...]`, `ParallelProduct[...]` ;  并行计算和与乘.

并行 evaluation, mapping 和 tables.

`ParallelMap[f,h[e1,e2,...]]` 是 `f/@h[e1,e2, ...]` 的并行版本,
它以并行方式而不是 `顺序地` 计算单个`f[ei] `.

## 副作用

除非你使用共享变量, 否则所进行的并行计算是完全独立的, 不能相互影响.
此外, 任何副作用, 例如对变量的赋值, 将会丢失, 当作为并行 `evaluations` 的一部分时.

并行求值的唯一影响是, 它在最后返回的结果.

## 例子

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

## 自动分发定义

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

## 自动并行化

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

