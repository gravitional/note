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
