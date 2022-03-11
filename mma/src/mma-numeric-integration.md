# mma 数值积分

tutorial/NIntegrateOverview

## Overview

Wolfram 语言函数 `NIntegrate` 是通用的 数值积分器. 它可以处理一般的一维和多维积分.

在 `区域 [a1, b1]x[a2,b2]x...x[an,bn]` 内找到函数 `f` 的数值积分.

```mathematica
NIntegrate[f[x1, x2, ... , xn ,{x1, a1, b1} ,{x2, a2, b2} ,... ,{xn, an, bn} ]
```

一般来说, `NIntegrate` 通过对 `积分区域内` 的积分值进行 `采样`(sampling), 来估计积分.
各种数值积分方法规定了 `初始采样步骤` 和 `采样` 的演变方式(evolve).

以下数值计算积分 $\int_0^1 \frac{1}{\sqrt{x}}d x$ :

```mathematica
NIntegrate[1/Sqrt[x], {x, 0, 1}]
Out[1]= 2.
```

该图绘制了计算被积函数点 `x` 值的序列. 靠近 `x=0` 时的采样更密集, 其中被积函数变化更快:

```mathematica
ListPlot[Last[ Reap[NIntegrate[1/Sqrt[x], {x, 0, 1}, EvaluationMonitor :> Sow[x]]]]]
```

`NIntegrate` 使用的算法被称作 "积分策略"(`integration strategies`),
尝试计算积分估计, 使之满足用户指定的精度和准确度的目标.
`积分策略` 运用 `积分规则`(integration rules) 使用 `加权和` 计算 `被积函数` 值的 `积分估计`.
使用特殊的 `积分策略` 和 `规则` 计算数值积分:

```mathematica
NIntegrate[1/(2 + Sin[x]), {x, 0, 5}, Method -> {"GlobalAdaptive", Method -> "ClenshawCurtisRule"}]
Out[3]= 2.70216
```

## 符号预处理

`NIntegrate` 使用 `符号预处理` 简化特殊结构的积分, 且自动选择积分方法.
当被积函数是 `偶函数` 或 `奇函数`, 或包含 `分段函数`,
可能导致 `积分区域`被转换成或分成多个不同的积分区域.

+ 对分段函数在区间 `[0,2]` 上进行积分. 积分区域自动在奇异点 `x=1` 分开:

```mathematica
NIntegrate[1/Sqrt[Abs[x - 1]], {x, 0, 2}]
Out[4]=4.
```

+ 高振荡(oscillatory)的被积函数将被识别, 且会应用指定的积分规则.

这里对一个高度振荡函数在区间  `[2,3]`上进行积分.
自动选择适于傅立叶正弦积分的一种专用方法:

```mathematica
NIntegrate[(x - 2)^2 Sin[4000 x], {x, 2, 3}]
Out[5]=-0.000158625
```

+ 这是先前的被积函数在积分区域的 `1/50`子集 上的图形:

```mathematica
Plot[(x - 2)^2 Sin[4000 x], {x, 2 + 2/50, 2 + 3/50}]
```

+ `符号预处理` 允许自动计算包括 `不连续性`(discontinuities) 和快速变化(rapid variation)区域的各种积分.

这里对在某区域有 `高度振荡` 的 `分段函数` 进行积分.
积分区域自动在 `x=1` 和 `x=2` 处分开, 特殊的傅立叶方法将应用于振荡区域 :

```mathematica
NIntegrate[
Piecewise[{{1/Sqrt[Abs[x - 1]], x < 2}, {(x - 2)^2 Sin[4000 x], 2 < x < 3}}], {x, 0, 3}]

Out[7]=3.99984
```

### 设计

`NIntegrate` 框架的主要特征是:

+ 面向对象(方法属性规范和交流); Object orientation (method property specification and communication)
+ 方法初始化阶段和 `运行时计算` 的分离; Separation of method initialization phase and runtime computation
+ `分层` 和 `折返数值` 方法; Hierarchical and reentrant numerical methods
+ `类型动态` 及 `精度动态` 方法; Type- and precision-dynamic methods
+ 通过 `插件` 功能的用户可扩展性和原型设计; User extensibility and prototyping through plugin capabilities
+ 专业化数据结构; Specialized data structures

策略, 规则和预处理器

NIntegrate 积分策略是根据它们如何在积分区域采样, 如何应用被积函数的类, 它们是否是"基于规则"的策略进行分类.
"GlobalAdaptive"    任何被积函数, 自适应采样, 基于规则
"LocalAdaptive"    任何被积函数, 自适应采样, 基于规则
"DoubleExponential"    任何被积函数, 均匀采样
"Trapezoidal"    任何被积函数, 均匀采样
"MultiPeriodic"    多维被积函数, 均匀采样
"MonteCarlo"    任何被积函数, 均匀随机采样
"QuasiMonteCarlo"    任何被积函数, 均匀准随机采样
"AdaptiveMonteCarlo"    任何被积函数, 自适应随机采样
"AdaptiveQuasiMonteCarlo"    任何被积函数, 自适应准随机采样
"DoubleExponentialOscillatory"    一维无限范围振荡被积函数
"ExtrapolatingOscillatory"    一维无限范围振荡被积函数

## NIntegrate 积分策略

自适应采样策略是通过在子区域使用更大的错误估计进行更频繁的采样来尝试改善积分估计, 一般是细分那些子区域. 均匀采样策略是通过在整个积分区域均匀增加采样密度来尝试改善积分估计.

基于规则策略将一个给定的积分规则应用于每个子区域, 以获得该区域的积分和错误估计. 积分规则可以使用设置 Method->{"strategy",Method->"rule"} 来指定.
以下是如何在每个子区域用具有 Clenshaw-Curtis 求积的全局自适应细分指定一个积分:

NIntegrate 积分规则可以根据它们是应用于一维或多维区域, 以及积分规则的类型进行分类.
"BooleRule"    一维, 加权和
"ClenshawCurtisRule"    一维, 加权和
"GaussBerntsenEspelidRule"    一维, 加权和
"GaussKronrodRule"    一维, 加权和
"LobattoKronrodRule"    一维, 加权和
"LobattoPeanoRule"    一维, 加权和
"NewtonCotesRule"    一维, 加权和
"TrapezoidalRule"    一维, 加权和
"ClenshawCurtisOscillatoryRule"    一维, 专用振荡规则
"LevinRule"    一维或多维, 一般振荡规则
"MultipanelRule"    一维, 加权和, 一维规则的组合
"CartesianRule"    多维, 加权和, 一维规则的乘积
"MultidimensionalRule"    多维, 加权和

可以与基于规则策略 "GlobalAdaptive" 和"LocalAdaptive" 一起使用的积分规则.

经典的"加权和"类型规则估计点集的函数值的预设线性组合的积分. 振荡规则使用求积加权估计积分, 其依赖于被积函数的特殊振荡"内核".

组合规则从一个或多个子规则构建求积规则. 它们由设置 Method->{"rule",Method->{"subrule1",... }} 指定.
以下是如何制定两个一维子规则的笛卡尔积的多维规则:

In[34]:=
NIntegrate[Log[1 + x^2 + y], {x, 0, 10}, {y, 0, 10},
 Method -> {"CartesianRule",
   Method -> {"GaussKronrodRule", "ClenshawCurtisRule"}}]
Out[34]=

所有策略的能力通过被积函数的符号预处理得到延伸. 预处理是由预处理策略控制的, 首先变换或分析积分, 然后把积分托付给另一个策略(经常是另一个预处理策略).
"SymbolicPreprocessing"    全局预处理控制器
"EvenOddSubdivision"    简化奇偶被积函数
"InterpolationPointsSubdivision"    细分包含内插函数的被积函数
"OscillatorySelection"    检测振荡被积函数并选择合适的方法
"SymbolicPiecewiseSubdivision"    细分包含分段函数的被积函数
"UnitCubeRescaling"    重新调整多维被积函数到单位立方体
"DuffyCoordinates"    多维奇点消除变换
"PrincipalValue"    等价于柯西主值的数值积分

## NIntegrate 预处理器策略

预处理策略是由设置 Method->{"preprocessor",Method->m} 来指定的, 其中 m 是预处理完成后积分被托付的策略或规则.
以下是如何明确应用预处理策略于一个关于两个变量的被积函数. 预处理后, 积分被托付给 "LocalAdaptive" 策略:

 Out[35]=

预处理策略经常减小最后积分策略所需要的工作量.
以下是以前积分的采样点. 没有预处理, 需要四倍的采样点来覆盖整个积分区域:
In[36]:=
Graphics[{PointSize[0.005],
  Point[Reap[
     NIntegrate[
      Boole[x^2 + y^2 < 1]*(1 - Sqrt[x^2 + y^2]), {x, -1, 1}, {y, -1,
       1}, Method -> {"EvenOddSubdivision",
        Method -> "LocalAdaptive"},
      EvaluationMonitor :> Sow[{x, y}]]][[2, 1]]]}, Axes -> True,
 PlotRange -> {{-1, 1}, {-1, 1}}]

Out[36]=

## 用户可扩展性

内置方法可作为对专用积分进行高效构建的构件块, 并可以添加用户定义的积分规则, 积分策略和预处理策略.
