# 并行计算

ParallelTools/tutorial/Overview
ParallelTools/tutorial/ConcurrencyManagingParallelProcesses

`Parallelize`是偏底层的函数, `ParallelTable`,`ParallelMap`是偏上层的函数.
`Parallelize`的默认选项, `DistributedContexts:>$Context`,它分配当前`context`中所有符号的定义, 但不分配`packages`中符号的定义.

大多数并行函数, 如`ParallelTable`的默认选项是`DistributedContexts:>$DistributedContexts`,
而`$DistributedContexts`的初始值是`$Context`.
同样不会默认分配`packages`中的符号.

使用`DistributeDefinitions[s1,s2] `或者`` DistributeDefinitions["context`"] ` `, 将`s1,s2`的定义, 或者某个上下文分配到所有并行计算中,
包括`ownvalues`, `downvalues`, `upvalues`,以及其他类型的`values`.
`DistributeDefinitions`会递归的运行,  `s1,s2`依赖的符号也会被分配.

`DistributeDefinitions[expr]`会分配`expr`中所有符号的定义.

***
`ParallelNeeds`可以在所有`从核`加载`packages`, 新开的`从核`也会自动加载.

在主核加载:`Needs["ComputerArithmetic`"]`
在所有从核加载: `ParallelNeeds["ComputerArithmetic`"]`

## 配置和监控运行

常用的管理多核的函数及变量为:

+ `LaunchKernels`: 启动从核
+ `AbortKernels`-- 停止所有从核上的运算
+ `CloseKernels`: 关闭所有从核
+ `$KernelCount`:目前运行的从核数量.
+ `Kernels`: 列出正在运行的从核
+ `$KernelID`: 每个从核的特有`ID`
+ `$ConfiguredKernels`:从核的默认启动列表

## 启动并链接从核

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
`Scheduling[pid]`:分配给进程的优先级
`ProcessState[pid]`进程的运行状态: `queued`, `running`, `finished`

## 与 Parallelize 的比较

`Parallel Evaluation` 中介绍了 Parallel mapping, tables, and inner product .
这些函数在`method`选项的控制下, 将任务分为若干个子问题.
本节中的功能为每个子问题生成一个计算.  此划分等效于设置`Method->"FinestGrained"`.

如果所有子问题花费相同的时间, 则诸如`ParallelMap[]`和`ParallelTable[]`之类的功能会更快.
但是, 如果子问题的计算时间不同, 并且不容易预先估计, 则最好使用本节中所述的`WaitAll [... ParallelSubmit [] ...]`或等效的`Method->"FinestGrained"`.
如果生成的进程数大于远程内核数, 则此方法将执行自动负载平衡, 一旦完成前一个作业, 便将作业分配给内核, 使所有内核始终保持忙碌状态.

## SetSharedVariable

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

## SetSharedVariable

    SetSharedFunction[Subscript[f, 1],Subscript[f, 2],...]

声明符号 `f_i` 为共享函数, 其 `下值`(downvalues), 在所有并行内核中同步.

在`任何内核上`(主核,或从核)定义的`共享函数`的 `下值`都由 `主内核`维护, 在 `并行子内核`上的每一次访问都通过主内核进行`同步`.
`f[...]` 形式的表达式如果计算不出值, 就会返回 `Null`.

### scope

+ 定义一个 `共享函数`, 来操作 `主内核`上的局部变量`results`.

```mathematica
append[res_] := (AppendTo[results, res];)
SetSharedFunction[append]
```

从子核中调用该函数:

```mathematica
results = {};
ParallelDo[If[PrimeQ[2^i + 1], append[i]], {i, 5000}]
```

主核上的变量已被修改:

```mathematica
result
Out[3]= {1, 2, 4, 8, 16}
```

### Generalizations & Extensions

在主内核上, 为一个共享函数作`延迟定义`(delayed definition):

```mathematica
SetSharedFunction[f1]
f1[n_] := n*$KernelID
```

这样的定义总是在主内核上被计算, 无论在哪个子内核计算它:

```bash
ParallelMap[f1, Range[4]]
Out[3]= {0, 0, 0, 0}
```

在某个并行`子内核`上, 作共享函数的定义:

```mathematica
SetSharedFunction[f2]
ParallelEvaluate[f2[n_] := n*$KernelID, First[Kernels[]]]
```

这样的定义, 总是在请求返回值的那个内核上被计算, 也就是子核自己计算, 所有的子核都有自己的计算.

```mathematica
ParallelMap[f2, Range[4]]
Out[6]= {4, 6, 6, 4}
```

### Applications

Shared functions can be used for synchronization:

```mathematica
In[1]:= mlist = {};
include[e_] := (mlist = Union[mlist, {e}];)
SetSharedFunction[include]
In[2]:= ParallelDo[include[RandomInteger[10]], {10}]
In[3]:= mlist
Out[3]= {2, 4, 5, 6, 8, 9}
Less efficient is the use of a shared variable and critical section:
In[4]:= slist = {}; SetSharedVariable[slist];
Clear[lock]
In[5]:= ParallelDo[e = RandomInteger[10];
 CriticalSection[{lock}, slist = Union[slist, {e}]], {10}]
In[6]:= slist
Out[6]= {1, 2, 3, 5, 7, 8, 9}
```

+ 简单数据类型(栈)的构造函数(constructor):

```mathematica
newList[list_Symbol] := Module[{data = {}},  list[push, e_] := (AppendTo[data, e];);(* 压入数据*)

  list[pop] :=  If[Length[data] == 0, $Failed,   With[{e = First[data]}, data = Rest[data]; e]];
  (*弹出数据, 如果末尾则返回失败*)
  list[] := data;(*提取出数据*)
    list](*返回构造的队列数据, 以及方法*)

(*创建两个共享的队列 queues *)
newList[input]; SetSharedFunction[input]
newList[input]; SetSharedFunction[input]
(*填充输入队列 queue*)
newList[input]; SetSharedFunction[input]

(*在输入队列的元素上并行工作, 并将结果放入输出队列. *)
numberOfPrimes[n_] := Total[FactorInteger[e! + 1][[All, 2]]];
DistributeDefinitions[numberOfPrimes](*将定义发送到各个从核*)
(*当 input 队列没有穷尽, 就持续弹出栈中数据*)

ParallelEvaluate[   While[(e = input[pop]) =!= $Failed,
 output[push, {e, numberOfPrimes[e! + 1]}]]];
 output[]
```

+ asdfsadfdas

```mathematica
(*Use a single shared function to communicate both input and result:*)

record[0, _] := next++;
record[n_, nf_] := (results[n] = nf; next++)
SetSharedFunction[record]

(*Set up a search and display its progress until it is manually aborted:*)
first = next = 100; Clear[results];
results[_] := "\[WatchIcon]";
PrintTemporary[(*暂时输出, 计算完成后删除*)

Dynamic[{next, Array[results, next - first, first]}](* Array[f,  length,origin] *)];(*动态展示表达式*)
CheckAbort[(* 如果检测到终止, 返回 null, 但不 abort,继续运行 *)
 ParallelEvaluate[Module[{next = 0, res},
   While[True, next = record[next, res];    res = Total[FactorInteger[2^next + 1][[All, 2]]]]
   ]], Null]

(*The results found so far\[LongDash]a list of the number of factors of 2^n+1:*)
Style[Table[{n, results[n]}, {n, first, next}], Small]
```

### properties & relations

```mathematica
(*Use a shared append function for a local variable to collect results:*)
append[res_] := (AppendTo[localres, res];)
SetSharedFunction[append]
localres = {};
ParallelDo[If[PrimeQ[2^i + 1], append[i]], {i, 1000}];
localres

(*Using AppendTo on a shared variable has the same effect:*)
SetSharedVariable[sharedres]
sharedres = {};
ParallelDo[If[PrimeQ[2^i + 1], AppendTo[sharedres, i]], {i, 1000}];
sharedres
```

### Possible Issues  

A shared function is inefficient for mere code distribution and leads to sequential evaluation:

```
In[1]:= nfs[n_] := Total[FactorInteger[2^n + 1][[All, 2]]];SetSharedFunction[nfs]
In[2]:= AbsoluteTiming[ ParallelMap[nfs, Range[140, 160], Method -> "FinestGrained"]]
Out[2]= {3.136468, {4, 7, 6, 7, 7, 6, 5, 7, 2, 5, 14, 3, 4, 11, 9, 6, 5, 5, 4,   6, 4}}
(*Simply distribute the definitions of any function needed on the parallel kernels:*)
In[3]:= nfu[n_] := Total[FactorInteger[2^n + 1][[All, 2]]];
DistributeDefinitions[nfu]
In[4]:= AbsoluteTiming[ ParallelMap[nfu, Range[140, 160], Method -> "FinestGrained"]]
Out[4]= {1.961938, {4, 7, 6, 7, 7, 6, 5, 7, 2, 5, 14, 3, 4, 11, 9, 6, 5, 5, 4,   6, 4}}
```

Separate read and write operations on a shared variable are not thread-safe:

In[1]:= ns = 0; SetSharedVariable[ns]
In[2]:= ParallelEvaluate[ns = 2 ns + $KernelID]
Out[2]= {1, 2, 3, 4}
In[3]:= ns
Out[3]= 4

Use a shared function to synchronize access to an (unshared) variable:

In[4]:= nu = 0;
update[kid_] := (nu = 2 nu + kid);
SetSharedFunction[update]
In[5]:= ParallelEvaluate[update[$KernelID]]
Out[5]= {1, 4, 11, 26}
In[6]:= nu
Out[6]= 26

Alternatively, use CriticalSection to make a whole code section atomic:

In[7]:= ns = 0; SetSharedVariable[ns]; Clear[nlock]
In[8]:= ParallelEvaluate[CriticalSection[{nlock}, ns = 2 ns + $KernelID]]
Out[8]= {1, 4, 11, 26}
In[9]:= ns
Out[9]= 26

Parallel dynamic programming:

```mathematica
In[1]:= SetSharedFunction[fib];
fib[1] = fib[2] = 1;
In[2]:= ParallelEvaluate[fib[n_] := fib[n] = fib[n - 1] + fib[n - 2],
 First[Kernels[]]]
In[3]:= ParallelTable[fib[i], {i, 10}]
Out[3]= {1, 1, 2, 3, 5, 8, 13, 21, 34, 55}
In[4]:= Definition[fib]
Out[4]= Definition[fib]
```

### Neat Examples

A parallel version of Sow:

```
In[1]:= sow[e_] := Sow[e];
SetSharedFunction[sow]
In[2]:= Reap[ParallelDo[sow[$KernelID], {10}]]
Out[2]= {Null, {{4, 3, 2, 1, 4, 3, 2, 1, 4, 3}}}
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

## Wolfram Engine

[Mathematica 激活指南](https://tiebamma.github.io/InstallTutorial/)
[free Wolfram Engine](https://mathematica.stackexchange.com/questions/198839/how-to-add-a-front-end-to-the-free-wolfram-engine)

2019 年 5 月, Wolfram 推出了免费的 Wolfram Engine for Developers.
此软件实质上是一个没有笔记本界面, 也没有本地自带帮助的 Mathematica. 但是, 它是免费的!并且, 虽然没有自带笔记本, 但你可以用 `Jupyter` 笔记本.

## Windows 平台

+ 下载安装 `Python`:[https://www.python.org/](https://www.python.org/).
不要忘了勾选`"add python environment variables" / "add to PATH"`,否则需要自己添加`python.exe`到环境变量.
+ 从[github : WLforJupyter > releases](https://github.com/WolframResearch/WolframLanguageForJupyter/releases)下载`.paclet`文件.
+ 在`cmd(管理员)`执行:

```bash
pip install jupyter ## 等待安装jupyter笔记本
wolframscript ## 运行wolfram内核,下面的命令将在Wolfram 内核运行
PacletInstall @ ".paclet文件的路径" # 安装.paclet文件
<< WolframLanguageForJupyter`## 导入这个包
ConfigureJupyter["Add"] ## 配置Jupyter笔记本
```

然后输入`Quit`退出, 重新运行`CMD`, 输入`jupyter notebook`就会打开浏览器, 选择`New -> Wolfram Language`就可以运行`Wolfram`语言.

## Linux安装

首先安装好`wolframscript`和`Jupyter`

Clone [WolframLanguageForJupyter](https://github.com/WolframResearch/WolframLanguageForJupyter)仓库:
在终端中进入仓库, 运行`git clone https://github.com/WolframResearch/WolframLanguageForJupyter.git`
在克隆好的仓库中运行`./configure-jupyter.wls add`. 如果报错`Jupyter installation on Environment["PATH"] not found`, 可以尝试指定具体的路径:

```bash
configure-jupyter.wls add "Wolfram Engine二进制程序的绝对路径" "Jupyter二进制程序的绝对路径"
```

`Jupyter`二进制的目录一般在`~/.local/bin/jupyter `,` Wolfram binary`的位置可以在`wolframscript`中得到:

```bash
wolframscript
FileNameJoin[{$InstallationDirectory, "Executables", "WolframKernel"}]
```
