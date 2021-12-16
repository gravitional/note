# 并行计算--内置函数

## Parallelize

```mathematica
Parallelize[expr]; 使用 `自动并行化` 来运算 `expr`.
```

`Parallelize`是偏底层的函数, `ParallelTable`,`ParallelMap`是偏上层的函数.
`Parallelize`的默认选项, `DistributedContexts:>$Context`,它分配当前`context`中所有符号的定义, 但不分配`packages`中符号的定义.

大多数并行函数, 如`ParallelTable`的默认选项是`DistributedContexts:>$DistributedContexts`,
而`$DistributedContexts`的初始值是`$Context`.
同样不会默认分配`packages`中的符号.

使用`DistributeDefinitions[s1,s2] `或者`` DistributeDefinitions["context`"] ` `, 将`s1,s2`的定义, 或者某个上下文分配到所有并行计算中,
包括`ownvalues`, `downvalues`, `upvalues`,以及其他类型的`values`.
`DistributeDefinitions`会递归的运行,  `s1,s2`依赖的符号也会被分配.

`DistributeDefinitions[expr]`会分配`expr`中所有符号的定义.

### 细节

`Parallelize[expr]` 自动将 `expr` 的不同部分的求值, 分配给不同的 `可用内核`(kernels) 和 `处理器`.

+ `Parallelize[expr]` 通常会给出与计算 `expr` 相同的结果, 除了计算过程中的副作用(side effects).
+ `Parallelize` 具有 `HoldFirst` 属性, 因此表达式在并行化之前不会被计算.
+ `Parallelize的Method` 选项 指定了要使用的并行化方法. 可能的设置包括.
    + `"CoarsestGrained"` ; 将计算分成与 `可用内核` 相同数量的部分.
    + `"FinestGrained"` ; 将计算分成 `尽可能小` 的子单元.
    + `"EvaluationsPerKernel"->e` ; 将计算分成每个内核最多 `e 个部分`.
    + `"ItemsPerEvaluation"->m` ; 将`computation`计成最多 `m` 个子单元的`evaluation`.
    + `Automatic`   ; 在`开销`(overhead) 和 `负载均衡`(load-balancing) 之间自动平衡

+ `Method->"CoarsestGrained"` ; 适用于涉及许多子单元的计算, 所有这些子单元需要`大致相同的时间`.
它使开销最小化, 但不提供任何`负载均衡`.
+ `Method->"FinestGrained"` ;适用于涉及少数子单元的计算, 这些子单元的计算需要不同的时间.
它导致了更高的`通信开销`, 但最大限度地提高了`负载均衡`.
+ `Parallelize` 的 `DistributedContexts` 选项指定了 `expr` 中出现的哪些符号在计算前, 被自动分配到所有可用的内核中.
`默认值`是 `DistributedContexts:>$Context`, 它分发`当前上下文`中所有符号的定义, 但不分发`包`(package)中的符号定义.

### 例子

以交互方式定义的函数可以立即被并行使用.

```mathematica
f1[n_] := Length[FactorInteger[(10^n - 1)/9]]
Parallelize[Map[f1, Range[50, 60]]]
```

### 范围

#### listable 函数

所有有一个参数的可列表函数在应用于列表时将自动并行化:
隐式定义的列表:

```mathematica
Parallelize[Prime[Range[10]]]
```

### 结构保持函数

许多保持列表结构的, `函数式编程`的 constructs 都是并行的:

```mathematica
Parallelize[Map[f, {a, b, c, d}]]
Parallelize[MapIndexed[List, {a, b, c, d}]]
```

结果不需要与输入的长度相同:

```mathematica
Parallelize[Cases[Range[100], _?PrimeQ]]
```

在没有明显的可并行 `函数` 的情况下, Parallelize只是对元素进行并行计算:

```mathematica
Parallelize[{1 + 2, Sin[1.0], Print[3], $KernelID}]
```

#### Reductions

数出100万以内的 `素数`:

```mathematica
Parallelize[Count[Range[10^6], _?PrimeQ]]
```

#### 内积和外积

内积,外积都可以自动并行化:

```mathematica
Parallelize[ Inner[f, {{a1, a2}, {b1, b2}, {c1, c2}, {d1, d2}}, {x1, x2}]]
Parallelize[Outer[StringJoin, {"", "re", "un"}, {"cover", "draw", "wind"}, {"",  "ing", "s"}]]
```

#### 迭代,Iterators

不管有或没有迭代器变量, 都可以并行计算 `Table`:

```mathematica
Parallelize[Table[i, {i, 2, 21, 2}]]
Parallelize[Table[Random[], {10}, {2}]]
```

并行计算`sum`s 与 `product`s:

```mathematica
Parallelize[Product[i, {i, 100}]]
```

函数的计算是平行进行的:

```
Parallelize[Sum[Pause[1]; i, {i, 4}]] // AbsoluteTiming
{1.01192,10}
```

文件名列表在子核上进行本地展开:

```mathematica
Parallelize[ Count[FileNames["*.m", $InstallationDirectory, Infinity],  f_ /; FileByteCount[f] > 500000]]
```

#### Associative 函数

具有 `Flat` 属性的函数会自动并行化:

```mathematica
Parallelize[a + b + c + d + e + f]
Parallelize[a*b*c*d]
Parallelize[LCM[1, 2, 3, 4, 5, 6]]
```

### 推广

在赋值中, 只有右侧的表达式被并行化:

```mathematica
Parallelize[primes = Select[Range[10^10, 10^10 + 100], PrimeQ]]
```

复合表达式的元素, 一个接一个地被并行化:

```mathematica
Parallelize[ primes = Table[Prime[10^i], {i, 11}]; IntegerLength /@ primes]
```

将视频帧的生成并行化:

```mathematica
Parallelize[ VideoGenerator[
     Plot[Sin[# x], {x, 0, 10},
    PlotLabel -> ("Plot[Sin[a x]]\na = " <> ToString[N@#]),
    ImageSize -> {320, 240}] &, 5]]
```

### 选项

默认情况下, 当前上下文中的定义会自动分发.

```mathematica
remote[x_] := {$KernelID, x^3}
Parallelize[Table[remote[i], {i, 4}]]
```

手动设置不分发任何函数的定义:

```mathematica
local[x_] := {$KernelID, x^2}
Parallelize[Table[local[i], {i, 4}], DistributedContexts -> None]
```

为并行计算中出现的, 所有`上下文`中的所有符号分发定义:

```mathematica
a`f[x_] := {$KernelID, x}
b`f[x_] := {$KernelID, -x}
Parallelize[{a`f[1], b`f[1]}, DistributedContexts -> Automatic]
```

#### method

将`computation`分成最多五个元素的`evaluations`:

```mathematica
Parallelize[Table[Labeled[Framed[i], $KernelID], {i, 18}], Method -> "ItemsPerEvaluation" -> 5]
```

对于运行时间相差悬殊的计算, 应该尽可能`精细地`进行并行化:

```mathematica
Parallelize[Select[Range[4000, 5000], PrimeQ[2^# - 1] &],Method -> "FinestGrained"]
```

大量的简单计算应该被分配到尽可能少的 `batches` 中:

```mathematica
BinCounts[ Parallelize[Map[Mod[Floor[#*Pi], 10] &, Range[10000]],  Method -> "CoarsestGrained"], {0, 10}]
```

### Applications

当找到结果时, 进行展示:

```mathematica
SetSharedVariable[primes]
primes = {};
Monitor[Parallelize[
    Scan[If[PrimeQ[2^# - 1], AppendTo[primes, #]] &,
    Range[1000, 5000]], Method -> "FinestGrained"], primes];
primes
```

### Properties

对于`数据`并行函数, `Parallelize` 是以 `ParallelCombine` 的方式实现的.
下面的表达式具有相同的效果:

```mathematica
Parallelize[Select[Range[100], PrimeQ]]
ParallelCombine[Select[#, PrimeQ] &, Range[100]]

Parallelize[Count[Range[100], _?PrimeQ]]
ParallelCombine[Count[#, _?PrimeQ] &, Range[100], Plus]
```

`并行`的加速效果, 可以用一个已知时间的`计算`来衡量:

```mathematica
AbsoluteTiming[Parallelize[Table[Pause[1]; $KernelID, {8}]]]
```

对于来自`package`的函数, 使用 `ParallelNeeds` 而不是 `DistributeDefinitions`:

```mathematica
Needs["FiniteFields`"]
Table[GF[7][{3}]^i, {i, 7}]
ParallelNeeds["FiniteFields`"]
Parallelize[Table[GF[7][{3}]^i, {i, 7}]]
```

设置一个适合并行使用的随机数发生器, 并初始化每个内核:

```mathematica
ParallelEvaluate[SeedRandom[1,Method -> {"ParallelMersenneTwister", "Index" -> $KernelID}]];
Join @@ ParallelEvaluate[RandomReal[1, 10]]
```

### 可能的问题

不能被`并行化`的表达式, 会进行普通计算.

```mathematica
Parallelize[Integrate[1/(x - 1), x]]
Parallelize::nopar1:.... cannot be parallelized; proceeding with sequential evaluation.

Parallelize[Map[f, {a, b, c}, 0]]
Parallelize::nopar1: Map[f,{a,b,c},0] cannot be parallelized; proceeding with sequential evaluation.
```

并行计算的函数不能使用副作用:
Side effects cannot be used in the function mapped in parallel:

```mathematica
primes = {};
Parallelize[
  Scan[If[PrimeQ[2^# - 1], AppendTo[primes, #]] &, Range[1000, 4000]]];
primes
```

使用 `shared variable` 来实现副作用:

```mathematica
SetSharedVariable[primes]
primes = {};
Parallelize[
  Scan[If[PrimeQ[2^# - 1], AppendTo[primes, #]] &, Range[1000, 4000]]];
primes
```

如果没有`子核`可用, 结果就在`主核`上计算:

```mathematica
CloseKernels[];
Parallelize[Map[f, {a, b, c}]]
```

如果使用的函数没有预先 `distributed`, 结果可能仍然看起来是正确的:
但只有当函数被`分发`之后, 结果才会在可用的内核上实际计算.

当前上下文中的函数定义是自动分布的.
来自默认上下文以外的上下文的定义不会被自动分发.
使用 `DistributeDefinitions` 来分发这些定义:

```mathematica
DistributeDefinitions[ctx`gtest];
Parallelize[Map[ctx`gtest, Range[1275, 1285]]]
```

或者, 将 `DistributedContexts` 选项设置为包括所有上下文, 通过 `Automatic` :

```mathematica
cty`gtest[n_] := Labeled[Framed[PrimeQ[2^n - 1]], $KernelID]
Parallelize[Map[cty`gtest, Range[1275, 1285]],  DistributedContexts -> Automatic]
```

只在`子核`上定义的符号不会自动分发:

```mathematica
ParallelEvaluate[h[i_] := {i, $KernelID}];
Parallelize[Map[h, Range[8]]]
```

+ `Parallelize` 中不使用 `$DistributedContexts` 的值:

+ 将所有设置恢复为默认值:

```mathematica
$DistributedContexts := $Context
SetOptions[Parallelize, DistributedContexts :> $Context]
```

琐碎的操作在并行化时, 可能反而需要更长的时间:

```mathematica
AbsoluteTiming[Parallelize[Table[N[Sin[x]], {x, 0, 1000}]];]
AbsoluteTiming[Table[N[Sin[x]], {x, 0, 1000}];]
```

### Neat examples

在发现非平庸(nontrivial)自动机时显示它们:

```mathematica
Module[{auto = {}}, SetSharedVariable[auto];
 (* Progress *)

 PrintTemporary@
  Dynamic[GraphicsGrid[Partition[auto, 3, 3, 1, {}], Frame -> All,
    ImageSize -> 500]];
 (* Compute *)

 Parallelize[
  Scan[(out =
      Position[
       CellularAutomaton[{#, {2, 1}, {1, 1, 1}}, {{{{1}}},
         0}, {{{8}}}], 1];
     If[Length[out] > 50,
      AppendTo[auto,
       Graphics3D[{Cuboid /@ out}, Boxed -> False]]];) &,
   Range[2, 50, 2]]];
 (* Output *)

 GraphicsGrid[Partition[auto, 3, 3, 1, {}], Frame -> All,
  ImageSize -> 500]]
```

## ParallelNeeds

`ParallelNeeds`可以在所有`子核`加载`packages`, 新开的`子核`也会自动加载.

在主核加载:`Needs["ComputerArithmetic`"]`
在所有子核加载: `ParallelNeeds["ComputerArithmetic`"]`

## 共享变量,SetSharedVariable

    SetSharedVariable[s1, s2,  ...]

声明符号`s_i`, 其值在所有的`并行内核`中是同步的.

`共享变量`的`唯一值`由`主内核`维护, 在并行`子内核上`的每一次访问都通过主内核进行同步.
没有值的`共享变量` 运算结果为`Null`.

+ 每个子内核都会增加(全局)共享变量的值:

```mathematica
xs = 0; SetSharedVariable[xs];
ParallelEvaluate[xs++]
Out[2]= {0, 1, 2, 3}
xs
Out[3]= 4
```

+ 高效地访问共享列表中的单个元素, 注意`Unevaluated`的使用

```mathematica
sl = {a, b, c, d}; SetSharedVariable[sl]
ParallelEvaluate[Part[Unevaluated[sl], $KernelID]]
Out[4]= {a, b, c, d}
```

+ 更新一个 `共享变量` 的值:

```mathematica
sx = 0; SetSharedVariable[sx]
ParallelEvaluate[sx++]
Out[2]= {0, 1, 2, 3}
ParallelEvaluate[sx *= 2]
Out[3]= {32, 8, 64, 16}
sx
Out[4]= 64
```

+ 附加到一个`共享列表`:

```mathematica
sl = {}; SetSharedVariable[sl]
ParallelEvaluate[AppendTo[sl, $KernelID]]
Out[6]= {{3, 1}, {3, 1, 2}, {3}, {3, 1, 2, 4}}
sl
Out[7]= {3, 1, 2, 4}
```

+ 附加到一个共享列表的一个元素上:

```mathematica
sll = Table[{}, {3}]; SetSharedVariable[sll]
ParallelDo[AppendTo[sll[[i]], $KernelID], {i, Length[sll]}]
sll
Out[3]= {{4}, {3}, {2}}
```

### 可能的问题

+ 分开的读和写操作不是线程安全的. 应该使用`原子操作`进行同步. 例如:

        ++, --, AppendTo, 等函数

```mathematica
n = 0; SetSharedVariable[n]
ParallelEvaluate[n++]
Out[5]= {0, 1, 2, 3}
n
Out[6]= 4
```

或者, 使用 `CriticalSection` 来使 `某部分代码` 成为原子操作;

```mathematica
n = 0; SetSharedVariable[n]; Clear[nlock]
ParallelEvaluate[CriticalSection[{nlock}, n = n + 1]]
Out[8]= {1, 2, 3, 4}
n
Out[9]= 4
```

+ 创建一个共享变量, 其值是一个`大矩阵`;

```mathematica
list = RandomReal[1, {10^4, 10^4}];
SetSharedVariable[list]
```

访问它的元素, 会导致整个矩阵被重复传输到 `子核`;

```mathematica
AbsoluteTiming[ParallelEvaluate[Table[list[[i, $KernelID]], {i, 4}]]]
Out[2]= {13.0841, {{0.310021, 0.500236, 0.749769, ...}}}
```

使用 `Unevaluated` 来允许, 提取变量slice 的那部分代码, 可以看到共享变量 `list`;

```mathematica
AbsoluteTiming[
    ParallelEvaluate[
        Table[Part[Unevaluated[list], i, $KernelID], {i, 4}]]]
Out[3]= {0.055431, {{0.310021, 0.500236, 0.749769, 0.408784},...}}
```

## 共享函数,SetSharedFunction

```mathematica
SetSharedFunction[f1,   f2, ...];
声明各符号 `f_i` 为共享函数, 它们的 `下值`(downvalues) 在`所有并行内核`中同步(synchronized).
```

+ 在`任何内核`上(主核或子核) 定义的`共享函数`的 `下值`, 都由 `主内核`负责维护,
从 `并行子内核`  中每一次对`共享函数`的访问, 都通过主内核进行`同步`.
+ `f[...]` 形式的表达式如果计算不出值, 就会返回 `Null`.

`共享函数`在所有内核中, 均有相同的`定义`(`Definition`), 普通函数在每个内核中有一份自己的拷贝.

### scope

定义并 `share` 函数

```mathematica
next = 1;
getnext[] := next++
SetSharedFunction[getnext]
```

从 `subkernel` 调用时, 函数更新 `master kernel` 上的 `next` 变量:

```mathematica
ParallelEvaluate[getnext[]]
ParallelEvaluate[getnext[]]
```

+ 定义一个 `共享函数`, 来操作 `主内核`上的局部变量`results`(local variable).

```mathematica
append[res_] := (AppendTo[results, res];)
SetSharedFunction[append]
```

从`子核`中调用该函数:

```mathematica
results = {};
ParallelDo[If[PrimeQ[2^i + 1], append[i]], {i, 5000}]
```

主核上的变量已被修改:

```mathematica
result
Out[3]= {1, 2, 4, 8, 16}
```

### 推广

+ 在 `主内核` 上, 给 `共享函数` 添加`延迟定义`(delayed definition):

```mathematica
SetSharedFunction[f1]
f1[n_] := {n,$KernelID}
```

这样的 `定义` 总是返回到 `主内核` 中计算, 无论在哪个`子核`中调用它:

```mathematica
ParallelMap[f1, Range[4]]
Out[3]= {0, 0, 0, 0}
Definition@f1
Out: f1[n_] := {n, $KernelID} (* 主核函数的定义 *)
```

+ 在某个并行 `子内核` 上, 给共享函数添加定义:

```mathematica
SetSharedFunction[f2]
(*例如在第一个子核中添加 延迟定义*)
ParallelEvaluate[f2[n_] := {n,$KernelID}, First[Kernels[]]]
```

这样添加的定义, 总是`子核`自己进行计算. 也就是哪个`子核`请求返回值, 就用哪个`子核`计算, 所有的`子核`得到自己的本地值.

```mathematica
ParallelMap[f2, Range[4]]
Out[6]= {4, 6, 6, 4}
Definition@f2
f2[n_] := Parallel`Developer`SendBack[{n, $KernelID}]
```

查看 `子核` 函数的定义, 由名称 SendBack 可以推测`主核` 将把计算遣返回`子核`, 让子核 自行计算.

### 应用

`共享函数`可用于 `同步`(synchronization):

```mathematica
mlist = {};
include[e_] := (mlist = Union[mlist, {e}];)
SetSharedFunction[include]
(*在 主核 定义的函数, 将由主核负责计算*)
ParallelDo[include[RandomInteger[10]], {10}]
mlist
Out[3]= {2, 4, 5, 6, 8, 9}
```

若使用 `共享变量` 以及 `CriticalSection` 实现, 而不使用上述 `共享函数`,  会比较绕:

```mathematica
slist = {}; SetSharedVariable[slist]; Clear[lock] (* 使用共享变量, 并加锁*)
ParallelDo[e = RandomInteger[10];
    CriticalSection[{lock}, slist = Union[slist, {e}]], {10}]
    (* 使用 CriticalSection 对 lock  上锁, 其他进程无法使用, 计算完成后才释放 lock *)
slist
Out[6]= {1, 2, 3, 5, 7, 8, 9}
```

+ 简单 `队列数据类型`(`先进先出`) 的构造函数(constructor):

    ```mathematica
    newList[list_Symbol] := Module[(*局部变量 data 存储列表的元素, 初始化为空列表*){data = {}},
      (*push 方法,在队列末尾压入数据*)
      list[push, e_] := (AppendTo[data, e];);
      (*pop 方法弹出数据,如果到达队列末尾,则返回 $Failed*)
      list[pop] :=   If[Length[data] == 0, $Failed, With[{e = First[data]}, data = Rest[data]; e]];
      (*相当于 get 方法,返回整个队列的数据*)
      list[] := data;
      (*最后返回构造的队列实例*)
      list ]
    ```

    创建两个 `共享队列 `(queues):

    ```mathematica
    newList[input]; SetSharedFunction[input]
    newList[output]; SetSharedFunction[output]

    (*填充输入队列 *)
    Scan[input[push, #] &, Range[25, 45]]; input[]

    (*在输入队列的元素上并行工作, 并将结果放入输出队列. *)
    numberOfPrimes[n_] := Total[FactorInteger[e! + 1][[All, 2]]];
    (*将定义发送到各个子核*)
    DistributeDefinitions[numberOfPrimes]

    (*当 input 队列不到末尾, 就持续弹出队列数据*)
   ParallelEvaluate[While[(e = input[pop]) =!= $Failed,
   output[push, {e, numberOfPrimes[e! + 1]}]]];

   (* 查看 output 队列 收集到的结果 *)
     output[]
    ```

+ 使用单个 `共享函数`, 来交流(communicate) input 和 result:

    ```mathematica
    record[0, _] := next++;
    (*记录函数, 将结果保存到 results 的下值, 并返回迭代指标++*)
    record[n_, nf_] := (results[n] = nf; next++)
    SetSharedFunction[record]

    (* 建立一个搜索, 并显示其进度, 直到它被手动中止: Alt+. *)
    first = next = 100; Clear[results];
    results[_] := "\[WatchIcon]";
    (*暂时输出, 计算完成后删除*)
    PrintTemporary[
    (*动态展示表达式, Array[f,  length,origin] *)
    Dynamic[{next, Array[results, next - first, first]}]];
    (* 如果 CheckAbort 检测到终止, 返回 null, 但不 abort,继续运行 *)
    CheckAbort[
     ParallelEvaluate[Module[{next = 0, res},
        While[True, next = record[next, res]; (* 记录 next 步的结果 res*)
       res = Total[FactorInteger[2^next + 1][[All, 2]]]] (* 计算 next+1 步的结果 *)
       ]], Null]

    (* 2^n+1的因子数的列表, 目前得到的结果:*)
    Style[Table[{n, results[n]}, {n, first, next}], Small]
    ```

### 性质和关系

自定义共享函数 `append`, 并把它作用在 `local 变量`上,  来收集结果:

```mathematica
append[res_] := (AppendTo[localres, res];)
SetSharedFunction[append]
localres = {};
(*并行计算, 更新主核上的局域变量*)
ParallelDo[If[PrimeQ[2^i + 1], append[i]], {i, 1000}];
localres
```

+ 定义一个 `共享变量`, 并对其使用 `AppendTo`  函数(原子操作), 以达到相同的效果:

```mathematica
SetSharedVariable[sharedres]
sharedres = {};
ParallelDo[If[PrimeQ[2^i + 1], AppendTo[sharedres, i]], {i, 1000}];
sharedres
```

### 可能的问题

对于单纯的将代码分发到`子核`(code distribution)来说, 使用 `共享函数` 并不高效, 结果是单纯的顺序计算(sequential evaluation):

```mathematica
(*定义并共享函数代码*)
nfs[n_] := Total[FactorInteger[2^n + 1][[All, 2]]];SetSharedFunction[nfs];
(* 使用 并行子核 进行计算*)
AbsoluteTiming[ ParallelMap[nfs, Range[140, 160], Method -> "FinestGrained"]]
Out[2]= {3.136468, {4, 7, 6, 7, 7, 6, 5, 7, 2, 5, 14, 3, 4, 11, 9, 6, 5, 5, 4,   6, 4}}
```

+ 简单粗暴地把任何需要的函数定义, 派发到并行子核上, 这样计算速度更快:

```mathematica
(* 使用 DistributeDefinitions 派发到子核, 相当于拷贝函数代码 *)
nfu[n_] := Total[FactorInteger[2^n + 1][[All, 2]]];DistributeDefinitions[nfu]
AbsoluteTiming[ ParallelMap[nfu, Range[140, 160], Method -> "FinestGrained"]]
Out[4]= {1.961938, {4, 7, 6, 7, 7, 6, 5, 7, 2, 5, 14, 3, 4, 11, 9, 6, 5, 5, 4,   6, 4}}
```

+ 对某个`共享变量` 进行分离的`读写`(Separate read/write)操作, 不是`线程安全的`(thread-safe).

```mathematica
ns = 0; SetSharedVariable[ns]
ParallelEvaluate[ns = 2 ns + $KernelID]
Out[2]= {1, 2, 3, 4}
ns
Out[3]= 4
```

+ 解决方法是: 使用 `共享函数` 来 `同步访问`(synchronize access) 某个`变量`(非共享的):

```mathematica
(*定义局部变量, 定义 主核 共享函数*)
nu = 0;
update[kid_] := (nu = 2 nu + kid);
SetSharedFunction[update]
(* 主核将维护下值, 同步各个子核的调用 *)
ParallelEvaluate[update[$KernelID]]
Out[5]= {1, 4, 11, 26}
nu
Out[6]= 26
```

或者, 使用 `CriticalSection` 来使这段代码 变成 `原子的`(atomic, 即必须作为单元执行, 不能被干扰)

```mathematica
ns = 0; SetSharedVariable[ns]; Clear[nlock] (* 共享, 加锁 *)
ParallelEvaluate[CriticalSection[{nlock}, ns = 2 ns + $KernelID]]
Out[8]= {1, 4, 11, 26}
In[9]:= ns
Out[9]= 26

并行的动态编程(Parallel dynamic programming):

```mathematica
(* 在 子核定义函数, 主核会把函数下值进行同步 *)
SetSharedFunction[fib];fib[1] = fib[2] = 1;
ParallelEvaluate[fib[n_] := fib[n] = fib[n - 1] + fib[n - 2], First[Kernels[]]]

(* 并行 计算 Fibonacci 数列 *)
ParallelTable[fib[i], {i, 20}]
Out[3]= {1, 1, 2, 3, 5, 8, 13, 21, 34, 55}
Definition[fib]

Out:
fib[1] = 1,
fib[2] = 1,
fib[3] = 2,
fib[4] = 3,
fib[5] = 5,
...
fib[n_] := Parallel`Developer`SendBack[fib[n] = fib[n - 1] + fib[n - 2]]
```

### Neat 例子

`Sow` 的 并行版本:

```mathematica
sow[e_] := Sow[e];
SetSharedFunction[sow]
Reap[ParallelDo[sow[$KernelID], {10}]]
Out[2]= {Null, {{4, 3, 2, 1, 4, 3, 2, 1, 4, 3}}}
```

## DistributeDefinitions

    [s1,s2,...]

将符号 `s_i` 的所有定义分配给所有并行内核.

    ["context`"]

分发 `指定上下文`中所有符号的定义.

+ `DistributeDefinitions`  将 `ParallelEvaluate` 应用于涉及符号 `s_i` 的赋值和 `属性`, 不仅包括`ownvalues`, 还包括`downvalues`, `upvalues`和其他类型的`值`.
+ `DistributeDefinitions` 将自己递归地应用于出现在 符号 `s_i` 的定义中的任何符号.
+ `DistributeDefinitions` 具有 `HoldAll` 属性.
+ `DistributeDefinitions` 实际上是 `注册`(registers) 了符号 `s_i` 的定义, 这样它们就会自动分发到每个被启动的`新的`并行内核.
+ 对于一个任意的表达式 `expr`, `DistributeDefinitions[expr]` 分发 `expr` 中出现的所有符号的定义.

### 例子

确保启动了 `并行的子内核`:

```mathematica
LaunchKernels[]; $KernelCount
Out[1]= 4
(*在子核中使用的 `值` 需要先进行分发. *)
ctx`y = 42;
ParallelEvaluate[ToString[ctx`y]]
Out[3]= {"ctx`y", "ctx`y", "ctx`y", "ctx`y"}
DistributeDefinitions[ctx`y]
{ctx`y}
ParallelEvaluate[ToString[ctx`y]]
Out[5]= {"42", "42", "42", "42"}
```

默认上下文中的符号会自动分发:

```mathematica
y = 43;
ParallelEvaluate[ToString[y]]
Out[2]= {"43", "43", "43", "43"}
```

### 范围

变量的值:

```mathematica
ni = 5; nd := $KernelID
DistributeDefinitions[ni, nd]
{ni,nd}
ParallelEvaluate[{ni, nd}]
Out[2]= {{5, 1}, {5, 2}, {5, 3}, {5, 4}}
```

函数:

```mathematica
nfactors[n_] := Total[FactorInteger[(10^n - 1)/9][[All, 2]]]
DistributeDefinitions[nfactors]
{nfactors}
ParallelMap[nfactors, Range[50, 70]]
Out[2]= {10, 8, 9, 4, 14, 8, 12, 6, 8, 2, 20, 7, 5, 14, 15, 7, 15, 3, 10, 6, 12}
```

上值:

```mathematica
g /: f[g[x_]] := fg[x, $KernelID]
DistributeDefinitions[g]
{g}
ParallelMap[f, {g[1], h[2], g[3]}]
Out[2]= {fg[1, 4], f[h[2]], fg[3, 2]}
```

属性, Attributes:

```mathematica
np = 5; Protect[np]
DistributeDefinitions[np]
{"np"}
ParallelEvaluate[np = $KernelID, First[Kernels[]]]
During evaluation of Set::wrsym: Symbol Cell$$4410`np is Protected.
Out[2]= 1
```

分发当前`上下文`中所有符号的定义.

```mathematica
f[x_] := N[Pi^x, $prec]; $prec = 50;
DistributeDefinitions[Evaluate[Context[]]];
ParallelEvaluate[f[$KernelID]]
Out[3]= {3.14159265...}
```

### 推广

将要被`分布`的定义, 它所依赖的辅助定义也会`自动分布`:

```mathematica
prec = 18;(* prec 将被自动分布, 它是 mtest 的依赖*)
mtest[n_] :=  Timing[Inverse[RandomReal[{-1, 1}, {n, n}, WorkingPrecision -> prec]]][[1]]
DistributeDefinitions[mtest]
{mtest,prec}

ParallelTable[mtest[i], {i, 10, 100, 10}]
Out[3]= {0.004000, 0.006000...}
```

### 性质和关系

`DistributeDefinitions` 会覆盖之前存在的任何`values`和`attributes`:

```mathematica
ParallelEvaluate[f[0] = "Zero"; f[x_] := x^2,
  DistributedContexts -> None];
ParallelMap[f, {0, 1, 2, 3, 4}, DistributedContexts -> None]
Out[2]= {"Zero", 1, 4, 9, 16}
{f}

ParallelMap[f, {0, 1, 2, 3, 4}, DistributedContexts -> None]
Out[4]= {0, 1, 4, 9, 16}
```

通过`清除`函数, 并再次分发来删除被`分发过`的定义:

```mathematica
f[x_] := Labeled[Framed[x^2], $KernelID]
DistributeDefinitions[f];
ParallelMap[f, {1, 2, 3, 4}]
(* 清除定义, 重新分发 *)
ClearAll[f]; DistributeDefinitions[f];
ParallelMap[f, {1, 2, 3, 4}]
Out[4]= {f[1], f[2], f[3], f[4]}
```

`DistributeDefinitions` 使用 `ParallelEvaluate` 向所有内核发送定义:

```mathematica
f[n_] := PrimeQ[2^n - 1]
DistributeDefinitions[f];

(*一个显式的 ParallelEvaluate 也能达到效果*)
ParallelEvaluate[g[n_] := PrimeQ[2^n + 1]];
ParallelEvaluate[Through[{Identity, f, g}[$KernelID]]]
Out[4]= {{1, False, True}, {2, True, True}, {3, True, False}, {4, False, True}}

(* 新内核 将会记住 `Distributed 定义`; 而 ParallelEvaluate 则无此效果 *)

CloseKernels[]; LaunchKernels[];
In[6]:= ParallelEvaluate[Through[{Identity, f, g}[$KernelID]]]
Out[6]= {{5, True, g[5]}, {6, False, g[6]}, {7, True, g[7]}, {8, False, g[8]}}
```

+ 对于`上层的`并行命令, 以交互方式定义的函数会`自动分发`.

```
ftest[n_] := Labeled[Framed[PrimeQ[2^n - 1]], $KernelID]
ParallelMap[ftest, Range[1275, 1285]]
...
```

也可以选择`手动`分发定义, 禁用`自动分发`, 运行结果是相同的:

```mathematica
gtest[n_] := Labeled[Framed[PrimeQ[2^n - 1]], $KernelID]
DistributeDefinitions[gtest];
ParallelMap[gtest, Range[1275, 1285], DistributedContexts -> None]
```

+ 只在`子核`上`values` 的符号不会被分发:

```mathematica
ParallelEvaluate[xr = 5]
Out[1]= {5, 5, 5, 5}
DistributeDefinitions[xr] (* 结果为空, 表示没有分发*)
Out[2]= {}
(* 子核上的值不会被修改 *)
ParallelTable[Sqrt[i]/xr, {i, 5}]
Out[3]= {1/5, Sqrt[2]/5, Sqrt[3]/5, 2/5, 1/Sqrt[5]}

(*只要`符号`得到`局部值`, 它就会随着下一次的`并行计算`而被分发:*)
xr = 6;
ParallelTable[Sqrt[i]/xr, {i, 5}]
Out[5]= {1/6, 1/(3 Sqrt[2]), 1/(2 Sqrt[3]), 1/3, Sqrt[5]/6}
```

+ 使用 `ParallelNeeds` 在所有并行内核上`设置`一个`package`:

```mathematica
Needs["GraphUtilities`"]
ParallelNeeds["GraphUtilities`"]
ParallelEvaluate[HamiltonianCycles[CompleteGraph[3], All]]
Out[2]= {{{1, 2, 3}, {1, 3, 2}}, {{1, 2, 3}, {1, 3, 2}}, {{1, 2, 3}, {1, 3, 2}}, {{1, 2, 3}, {1, 3, 2}}}

使用 DistributeDefinitions 来设置你自己的定义:

hcg[n_] := HamiltonianCycles[CompleteGraph[n], All]
DistributeDefinitions[hcg];
ParallelMap[hcg, Range[4]]
Out[5]= {{}, {}, {{1, 2, 3}, {1, 3, 2}}, {{1, 2, 3, 4}, {1, 2, 4, 3}, {1, 3,2, 4}, {1, 3, 4, 2}, {1, 4, 2, 3}, {1, 4, 3, 2}}}
```

### 可能的问题

在`并行子核` 上使用未知函数可能会导致`顺序计算`(sequential evaluation):

```mathematica
ftest[n_] := Labeled[Framed[PrimeQ[2^n - 1]], $KernelID]
ParallelEvaluate[ftest[$KernelID], DistributedContexts -> None]
```

解决方法是在所有子核上定义这个函数:

```mathematica
DistributeDefinitions[ftest];
The function is now evaluated on the parallel kernels:
ParallelEvaluate[ftest[$KernelID], DistributedContexts -> None]
```

使用 `DistributeDefinitions` 并不能抑制定义的自动分发.

The use of DistributeDefinitions does not suppress automatic distribution of definitions:

```mathematica
f[i_] := {i, $KernelID}
DistributeDefinitions[f];

(* 修改定义 *)
Clear[f]
(* 修改过的定义会被自动分发 *)
Parallelize[Map[f, Range[8]]]
Out[3]= {f[1], f[2], f[3], f[4], f[5], f[6], f[7], f[8]}

(* 抑制自动分法行为 *)
g[i_] := {i, $KernelID}
DistributeDefinitions[g];
Clear[g]
In[6]:= Parallelize[Map[g, Range[8]], DistributedContexts -> None]
Out[6]= {{1, 4}, {2, 4}, {3, 3}, {4, 3}, {5, 2}, {6, 2}, {7, 1}, {8, 1}}
```

只在子核上定义的`符号`不会被自动分发:

```mathematica
In[7]:= ParallelEvaluate[h[i_] := {i, $KernelID}];
In[8]:= Parallelize[Map[h, Range[8]]]
Out[8]= {{1, 4}, {2, 4}, {3, 3}, {4, 3}, {5, 2}, {6, 2}, {7, 1}, {8, 1}}
```

+ 具有`内部状态`的某些对象, 在分布后, 可能会造成效率损失:

```mathematica
data = RandomReal[{-1, 1}, 1000];
fdata = ListInterpolation[data]
Out[2]= {0.044, Null}

DistributeDefinitions[fdata];
ParallelEvaluate[Timing[Table[fdata[r], {r, 1.0, 1000, 0.1}];], First[Kernels[]]]
Out[4]= {0.16, Null}
```

在子核上重新计算, 来改善速度:

```mathematica
DistributeDefinitions[data, fdata]
ParallelEvaluate[fdata = ListInterpolation[data];];
ParallelEvaluate[Timing[Table[fdata[r], {r, 1.0, 1000, 0.1}];],  First[Kernels[]]]
Out[7]= {0.044, Null}
```

或者在计算之前, 在所有 `子核` 上, 先重新计算所有数据:

```mathematica
fdata1 = fdata;
DistributeDefinitions[fdata1]
ParallelEvaluate[fdata1 = fdata1];

ParallelEvaluate[Timing[Table[fdata1[r], {r, 1.0, 1000, 0.1}];],
 Last[Kernels[]]]
Out[10]= {0.044, Null}
```

如果`符号`具有 `ReadProtected`, 则无法对它的定义进行分发:

```mathematica
ftest[n_] := Labeled[Framed[PrimeQ[2^n - 1]], $KernelID];
SetAttributes[ftest, ReadProtected]
DistributeDefinitions[ftest]

ParallelMap[ftest, Range[$KernelCount]]
```

### Neat 例子

快速可视化的高斯素数:

```mathematica
test[x_] := Boole[PrimeQ[x, GaussianIntegers -> True]]
data = With[{n = 100}, (* 适用 with 初始化局域常量, 便于修改变量的值, 并且比 Module 效率高*)
ParallelTable[test[x + y I], {x, -n, n}, {y, -n, n}]];
ArrayPlot[data]
```

## With

```mathematica
With[{x=x0, y=y0,   ...},expr]; 指定在 `expr` 中出现的所有符号 `x, y, ...` 应该替换成 `x0`, `y0`, ...
```

+ `With`允许你定义 `局部常量`.
+ 只有当 `expr` 中的符号不处在 深层嵌套 `scoping` 构造内的情况下, `with` 才替换符号.
+ 你可以使用 `With[{vars},body/;cond]` 作为 transformation 规则的右侧表达式, 可以附带一个`condition`.
+ `With` 具有 `HoldAll` 属性.
+ `With` 构造可以任意嵌套, 如果需要的话, 它会对内部变量进行重命名.
+ `With` 是一个 `scoping` 结构, 它实现了 `只读` 词法变量( read-only lexical variables).

### 可能的问题

`With` 是一个作用域结构; 嵌套作用域中的变量将被重新命名.

```mathematica
With[{e = Expand[(1 + x)^5]}, Function[x, e]]
(*Functions 也是scope, x 将被重命名, 返回的函数不能正常使用 *)
Out[1]= Function[x$, 1 + 5 x + 10 x^2 + 10 x^3 + 5 x^4 + x^5]
 %[10]
Out[2]= 1 + 5 x + 10 x^2 + 10 x^3 + 5 x^4 + x^5
```

+ 从函数的元素开始建立函数, 以避免重命名.

```mathematica
With[{e = Expand[(1 + x)^5]}, Function @@ {x, e}]
Out[3]= Function[x, 1 + 5 x + 10 x^2 + 10 x^3 + 5 x^4 + x^5]
 %[10]
Out[4]= 161051
```

### Neat 例子:

+ 用 `牛顿法` 找到任意函数的零点:

```mathematica
newton[f_, x0_] := With[{fp = f'}, FixedPoint[# - f[#]/fp[#] &, x0]]

newton[Cos, 1`20]
Out[2]= 1.570796326794896619
```

找到一个固定点(fixed point).

```mathematica
newton[Cos[#] - # &, 1`20]
Out[3]= 0.739085133215160642
```

+ 另一个版本的 `With`, `initializer`(即下面的`val`) 处在`局部变量`的作用域中.
`letrec` 即 `let recursive`, 递归赋值.
A version of With where the initializer is within the scope of the local variable

```mathematica
SetAttributes[letrec, HoldAll];(* HoldAll 属性, 要求函数的所有参数保持不计算的形式 *)
letrec[{var_ = val_}, body_] := Module[{var},
(* letrec 接收参数, 下文中 var 的实参为 f ;
Module 对 f 进行局域化, 而 val 中的 f 将被 Module 替换成相同的局部变量,
所以处在相同的作用域中, 可以进行递归计算 *)
var = val;
body]

letrec[{f = Function[n, If[n == 0, 1, n*f[n - 1]]]}, f[10]]
Out[2]= 3628800
```

在下面的定义中, `Function[]`中的 `f`  , 跟外面 `f=` 中的 `f`, 处在不同的作用域,
由于 `With` 会重命名内层作用域的`f`.  总的效果是无法实现递归定义:

```mathematica
With[{f = Function[n, If[n == 0, 1, n*f[n - 1]]]}, f[10]]
Out[3]= 10 f[9]
```

## Moudles

如果不需要`赋值`给局部变量, 应该使用`常量`来代替:

```mathematica
With[{x=2.0},Sqrt[x]+1]
Out[1]= 2.41421
(*With 比 Module 速度快:*)
Timing[Do[Module[{x=5},x;],{10^5}]]
Out[2]= {0.312,Null}
Timing[Do[With[{x=5},x;],{10^5}]]
Out[3]= {0.093,Null}
```

`Block`只对`values`进行居域化, 它不创建新的符号.

```mathematica
x = 7;
Block[{x = 5}, Print[x]]; x
5
7
```

`Unique` 以类似于 `Module` 的方式创建新的变量.

```mathematica
{Unique[x], Module[{x}, x]}
Out[1]= {x$949,x$950}.
```

`本地变量`不受 `全局变量` 的影响, 反之亦然.

```mathematica
x = 17;
Module[{x = x}, x = x + 1; x]
x
输出[2]=17
```

带入 `Module` 范围的`symbol`不受命名冲突的影响.

```mathematica
y = x^2;
Module[{x = 5}, x + y]
Out[2]= 5+x^2
```

### 可能的问题

`Module` 是一个 `scoping`结构; `内部局部变量` 屏蔽 `外部变量`.

```mathematica
Module[{x}, Print[x]; Module[{x}, Print[x]]]
x$119
x$120
```

+ `嵌套作用域`中的变量被重新命名.

```mathematica
Module[{e = Expand[(1 + x)^5]}, Function[x, e]]
Out[1]:= Function[x$,e$100477] .
%[10]
Out[2]= 1+5 x+10 x^2+10 x^3+5 x^4+x^5
```

从它的组成部分建立函数以避免重命名.

```mathematica
Module[{e = Expand[(1 + x)^5]}, Function @@ {x, e}]
Out[3]= Function[x,1+5 x+10 x^2+10 x^3+5 x^4+x^5] .
%[10]
Out[4]= 161051
```

+ 并行赋值, 对`Module`变量是不可用的:

```mathematica
v = {a, b};
Module[{{x, y} = v}, x^2 + y^2]
在运算Module::lvset的过程中...

(* 进行逐个赋值 *)
Module[{x = v[[1]], y = v[[2]]}, x^2 + y^2]
Out[3]= a^2+b^2
```

## 与 Parallelize 的比较

`Parallel Evaluation` 中介绍了 Parallel mapping, tables, and inner product .
这些函数在`method`选项的控制下, 将任务分为若干个子问题.
本节中的功能为每个子问题生成一个计算.  此划分等效于设置`Method->"FinestGrained"`.

如果所有子问题花费相同的时间, 则诸如`ParallelMap[]`和`ParallelTable[]`之类的功能会更快.
但是, 如果子问题的计算时间不同, 并且不容易预先估计, 则最好使用本节中所述的`WaitAll [... ParallelSubmit [] ...]`或等效的`Method->"FinestGrained"`.
如果生成的进程数大于远程内核数, 则此方法将执行自动负载均衡, 一旦完成前一个作业, 便将作业分配给内核, 使所有内核始终保持忙碌状态.
