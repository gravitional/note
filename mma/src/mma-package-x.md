# package-X

根据`package-X`的约定, 圈积分的表达式不需要带上$1/(2\pi)^4$.

## 并行计算error

如果`.wl`脚本中引用了 `package-X` 包, 在使用 `wolframscript` 对脚本进行并行计算时,
如果使用了 `-print all` 选项, 可能会导致脚本出现如下报错, 即出现符号`Context`冲突, 导致脚本解析失败.
详细原因暂时未知, 但是对于导入其他包也同样存在.
报错示例如下:

```log
Contract::shdw: Symbol Contract appears in multiple contexts {X`, Global`}; definitions in context X` may shadow or be shadowed by other definitions.
LDot::shdw: Symbol LDot appears in multiple contexts {X`, Global`}; definitions in context X` may shadow or be shadowed by other definitions.
...
From KernelObject[1, Local kernel]:
Global`LDot::shdw: Symbol LDot appears in multiple contexts {Global`, X`}; definitions in context Global` may shadow or be shadowed by other definitions.
```

似乎开启 `-print all` 选项, 会导致对脚本中定义的`提前检查`, 并且时间早于导入 `Package` .
即使语句声明 ``<<Package` `` 的顺序在前, 调用在后.
解决方法是去掉`-print all` 选项, 即可正常运行.

+ `wolframscript -print [all]` ;  当运行一个脚本时, `打印`执行该脚本最后一行的结果, 如果给定的是`all`, 则打印每一行.

## 费米子代数

### FermionLineExpand

[FermionLineExpand](https://packagex.hepforge.org/Documentation/HTML/X/ref/FermionLineExpand.html)

除了使用`狄拉克`代数, `FermionLineExpand`还会使用旋量的运动方程
$$\gamma\cdot p u(p)=m u(p), \quad \gamma\cdot v u(p)= -m v(p) $$
将狄拉克矩阵的乘积展开为标准的 `SVTAP` 形式:
$$\gamma^{\mu 1} \gamma^{\mu 2} \gamma^{\mu 3}\cdots =
A^{\mu 1 \mu 2 \mu 3 \cdots} 1+(B^{\mu 1 \mu 2 \mu 3 \cdots})_\nu \gamma^\nu +
(C^{\mu 1 \mu 2 \mu 3 \cdots})_{\nu\rho} \sigma^{\nu\rho} +\\
(D^{\mu 1 \mu 2 \mu 3 \cdots})_{\nu} \gamma^{\nu} \gamma_5+
E^{\mu 1 \mu 2 \mu 3 \cdots}\gamma_5 $$

如果可能的话, 还会使用高登`Gordon`恒等式,
$$\bar u(p^\prime)(p^\prime+p)^\mu u(p)=2m\;\bar{u}(p^\prime) \gamma^\mu u(p) -
i \bar{u}(p^\prime) \sigma^{\mu\nu} (p^\prime-p)_\nu u(p) $$

这些操作都是`LoopIntegrate` 自动进行的, 如果分子里存在`FermionLine`.

## 圈积分,LoopIntegrate

+ 给`LoopIntegrate`提供分子的列表, 能获得对应圈积分的列表
+ `LoopRefine`能自动线性作用于列表.

如果外线动量不是线性独立的(例如, 零转移动量时的形状因子), 分子分母中的公共因子不会自动消除.
输出会与`Cancel->False`情形的结果相同.

![LoopIntegrate](https://packagex.hepforge.org/Documentation/HTML/X/ref/Files/LoopIntegrate/61.png)

```mathematica
int1 = LoopIntegrate[k.k k.p, k, {k, m}, {k, 0}, {k + p, 0}, Cancel -> True]
```

设置选项`Apart->True`将分母用 部分 分式展开, 以使得分母上的因子能够和标量积相消:

```mathematica
int2 = LoopIntegrate[k.k k.p, k, {k, m}, {k, 0}, {k + p, 0},  Cancel -> True, Apart -> True]
```

## LoopRefineSeries

```mathematica
LoopRefineSeries[f,{s,  s0, n}]; 以点 s=s0 为中心, 将 f 展开为泰勒级数, 到 order  (s-s0)^n, 其中 f 包含 Passarino-Veltman函数

LoopRefineSeries[f, {s, s0, ns}, {t, t0, ts}, ...]; 依次展开, 先按照 `s` 的泰勒级数展开, 然后是 `t`, 等等.
```

+ `LoopRefineSeries` 通过计算 `Passarino-Veltman` 函数 `PVA`, `PVB`, `PVC` 和 `PVD` 的必要的导数, 并使用 `LoopRefine` 的内部版本进行转换, 有效地生成了展开式.
+ `LoopRefineSeries` 无法在(Landau)奇异点附近构建级数展开, 例如在 normal thresholds附近, 或在内部质量为零时
+ 设置选项 `Analytic->True`, 将`epsilon` 延拓到`大值`(和`负值)`, 使所有 `Landau` 奇异点充分可微, 允许进行级数展开.
`LoopRefineSeries` 将时空维度`d`替换为`4-2 epsilon`, 并保留阶数为`O(epsilon^0)`的项.
+ 所有`对数紫外发散`(logarithmic), 和质量奇点都明确显示为`1/epsilon` 或`1/epsion^2`极点.
若要使用规范的积分测度 $\mu^{2\epsilon} \int \frac{\mathrm{d}^4d k}{(2\pi)^d}$, 解释 `LoopRefineSeries` 的输出, 按照以下步骤操作:
    + 将结果乘以 $i/(16\pi^2)$
    + 将$\epsilon\sim 0$ 的极点解释为:
    输出->     含义:
     $\frac{1}{\epsilon} \quad\to\quad  \frac{1}{\epsilon}-\gamma_E + \ln(4\pi)$
     $\frac{1}{\epsilon^2} \quad \to \quad \frac{1}{\epsilon^2}+
     \frac{1}{\epsilon}(-\gamma_E +\ln(4\pi)) +\frac{\gamma_E^2}{2}
      -\gamma_E\ln(4\pi)+\frac12 \ln^2(4\pi)$
+ 除了重新组织结果, `LoopRefineSeries` 并没有进行任何实质性的`张量代数`操作.
+ `LoopRefineSeries` 产生的表达式对`实数`(正数或负数)的外部运动学不变量, 和半正定的内部质量有效.
+ `LoopRefineSeries` 的选项与 `LoopRefine` 相同.
+ `LoopRefineSeries` 生成一个 `SeriesData` 对象.

对于已经用`LoopRefine` 替换过的结果, 也可以使用.

### 选项

+ 默认情况下, `LoopRefineSeries` 不会在 `Landau` 奇点附近构造 级数展开.

```mathematica
LoopRefineSeries[PVB[0, 0, s, m, M], {m, 0, 2}]
```

设置选项 `Analytic->True`, 通过解析地把 `epsilon` 延续到`大值`(和负值), 使所有奇点充分可微, 从而产生一个结果.
这使得 `LoopRefineSeries` 能够在任何运动学点上进行 级数展开. 见下面的例子:

+ `对数奇点`(Logarithmic singularities, 那些与接近零的内部质量有关的奇点)显示为`1/epsilon` poles:

```mathematica
LoopRefineSeries[ PVC[0, 0, 0, m^2, 0, m^2, \[Lambda], M, M], {\[Lambda], 0, 2},  Analytic -> True, TargetScale -> M]
```

与由`Series`产生的正确的渐进展开(asymptotic expansion)进行比较:

```mathematica
LoopRefine[PVC[0, 0, 0, m^2, 0, m^2, \[Lambda], M, M]];Series[%, {\[Lambda], 0, 2}]
```

由 `LoopRefineSeries` 生成的泰勒级数展开的系数, 如果没有`1/epsilon`极点, 将与渐进展开中的相应项一致.

+ 由 `LoopRefineSeries` 生成的表达式中, 不存在代数奇点 (Algebraic singularities, normal thresholds).

```mathematica
LoopRefineSeries[PVB[0, 0, s, m, m], {s, 4 m^2, 2}, Analytic -> True]
```

与 `Series` 生成的正确渐近展开进行比较, 观察 `LoopRefineSeries` 的输出中没有`(s-4m^2)`的半数整次幂:

```mathematica
LoopRefine[PVB[0, 0, s, m, m]];
Series[%, {s, 4 m^2, 2}]
```
