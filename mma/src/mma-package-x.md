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

```mathematica
LoopIntegrate[num, k, {q0, m0 }, {q1, m1 }, ... ] ;
用 Passarino-Veltman 系数函数, 表示对 k 的单圈张量积分, 分子为 num, 分母为 (q0^2-m0^2)(q1^2-m1^2)

LoopIntegrate[num, k, {q0, m0, w0 }, {q1, m1,w1 }, ... ];
用 Passarino-Veltman 系数函数, 表示单圈张量积分, 并且分母的权重分别为 w0, w1, ...
```

### 详细

+ `LoopIntegrate` 生成用系数函数 `PVA`, `PVB`, `PVC`, `PVD` 和 `PVX` 表示的洛伦兹协变积分, 它对任何运动学参数值, 以及所有阶的 `Epsilon`有效.
+ 应用 `LoopRefine` 将 `PVA`, `PVB`, `PVC` 和 `PVD` 转换为解析表达式.
+ `分母指定` 的通常形式为: `{qn,mn}`, 代表被积式中的因子 $1/(q_n^2-m_n^2+i\epsilon)$.
其中 `qn` 和 `mn` 分别对应于 `内部动量`和 `质量` 变量.
+ `分母指定`可以带有附加参数, 也就是`{qn,mn,wn}` 的形式, 其中整数`wn`表示分母的权重,
$1/(q_n^2-m_n^2+i\epsilon)^{w_n}$. 默认的权重为 `1`.
+ 与积分变量`k`独立的分母, 将从积分中移出, 相同的分母被合并, 表示成带权重的形成.
+ 给予 `LoopIntegrate` 的独立的`分母指定`的数量, 决定了相应单圈图的拓扑结构:

$$ \text{LoopIntegrate}[ \mathrm{num},k, {k+p_0, m_0}, {k+p_1, m_1}, ... ] \\
=\frac{1}{C_\epsilon} \mu^{2\epsilon} \int \frac{\mathrm{d}^d k}{(2\pi)^d}
\frac{\mathrm{num}}{ [(k+p_0)^2-m_0^2+ i\epsilon] [(k+p_1)^2-m_1^2+i\epsilon]} $$

$$ \text{LoopIntegrate}[ \mathrm{num},k, {k+p_0, m_0, w_0}, {k+p_1, m_1, w_1}, ... ] \\
=\frac{1}{C_\epsilon} \mu^{2\epsilon} \int \frac{\mathrm{d}^d k}{(2\pi)^d}
\frac{\mathrm{num}}{ [(k+p_0)^2-m_0^2+ i\epsilon]^{w^0} [(k+p_1)^2-m_1^2+i\epsilon]^{w^1} }$$

+ 尽管在典型的单圈计算中出现的标准积分度量是$\mu^{2\epsilon}\int \frac{\mathrm{d}^d k}{(2\pi)^d}$,
但 `LoopIntegrate` 给出的结果是将 $C_\epsilon= \frac{ie^{-\gamma_E \epsilon}}{(4\pi)^{d/2}}$ 这个组合因子提取并省略.
要从 `LoopIntegrate` 的输出得到 `4`维时空的结果,
    + 将 `LoopRefine` 的输出乘以 $i/(16\pi^2)$,
    + 按照与下面 `LoopRefineSeries` 中相同的对应规则去解释, $\epsilon\sim0$ 处的极点.

+ 选项 `Apart` 控制: 是否在进行洛伦兹协变分解之前, 将被积式进行部分分式展开(partial fraction expansion). 可能的值为:
    + `False` ; 默认, 不进行部分分式展开
    + `True`; 进行部分分式展开

+ 选项 `Cancel` 控制: 是否在进行洛伦兹协变分解之前, 约掉分子和分母中的公因子. 可能的值是:
    + `Automatic` ; 默认, 如果分子包含运动学奇异点, 则不约掉`公因子`, 其他情况则约掉.
    + `True` ; 约掉公因子
    + `False` ; 不约掉公因子

+ 选项 `Dimensions->n` 将积分测量的维度设置为 $n-2\epsilon$. 默认设置为`4`, 可以设置为任何偶数整数.
+ 选项 `DiracAlgebra` 指定是否应用 `Dirac 代数`来简化`分子`. 可能的值是:
    + `True`;   应用狄拉克代数.
    + `False`;  不应用狄拉克代数, 只使用`linearity`来分解积分.
+ 在设置 `DiracAlgebra->True` 的情况下, `LoopIntegrate` 遵守 `FermionLineExpand` 的默认选项设置, 可以通过 `SetOptions` 进行更改.
+ 选项 `Organization` 规定了输出结果的整理方式.  可能的值是:
    + `Automatic`; 默认, 自动选择组织方案
    + `LTensor`; 按洛伦兹张量结构组织结果
    + `Function`; 用 Passarino--Veltman 函数组织结果(比较快)
    + `None` ; 不整理结果(运行速度最快).

+ 如果在`LoopIntegrate` 的`num`参数中提供分子的列表, 能获得对应圈积分的列表.
`LoopRefine` 也能自动线性作用于列表.

### 例子

计算单`传播子`的标量一圈积分(蝌蚪图):

$$\mu^{2\epsilon} \int \frac{\mathrm{d}^d k}{(2\pi)^d} \frac{1}{ k^2-m^2} $$

```mathematica
tea=LoopIntegrate[1, k, {k, m}]
Out[1]= PVA[0, m]
```

应用 `LoopRefine` 来获得显式表达式:

```mathematica
LoopRefine[tea]
```

为了解释这个结果:

1. 恢复整体系数 $i/(16\pi^2)$;
2. 把$1/\epsilon$ 展开成 $\frac{1}{\epsilon}- \gamma_E +\ln(4\pi)$

所以最终结果是:

$$\mu^{2\epsilon} \int \frac{\mathrm{d}^d k}{(2\pi)^d} \frac{1}{ k^2-m^2}=
\frac{i}{16\pi^2}[m^2+m^2(\frac{1}{\epsilon}-\gamma_E+\ln(4\pi)+\ln(\frac{\mu^2}{m^2}))] $$

### 可能的问题

+ 如果外线动量不是线性独立的(例如, 零转移动量时的形状因子), 分子分母中的公共因子不会自动消除.
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

## PVX

```mathematica
PVX[r,n1,n2,..., s01, s12, s23, ... , m0, m1, m2, ... ]
是一般的 Passarino-Veltman 系数函数, 如下:
```

$$X_{ \underbrace{{0\cdots0}}_{2r}
\underbrace{{1\cdots1}}_{n_1} \underbrace{{2\cdots2}}_{n_2} }
(s_{01},s_{12},s_{23}, \cdots ; m_0, m_1, m_2, \cdots )$$

### 详细

+ PaVe 系数函数将乘上一个洛伦兹协变的张量结构, 即全对称张量:

$$\{ [p_1]^{n_1} [p_2]^{n_2} \cdots [g]^r\}^
{\mu_1 \cdots \mu_{2r+n_1+n_2+\cdots} }$$

张量的指标一共有 $2r+n_1+n_2+\cdots$ 个, 其中:

+ $2r$属于`r`个度规张量$g^{\mu\nu}$,
+ $n_1$ 个指标属于第一个独立动量 $p_1$, 它们组成 $p_1^{a}p_1^{b}p_1^{c}\cdots$
+ $n_2$ 个指标属于第二个独立动量 $p_2$, 它们组成 $p_2^{d}p_2^{e}p_2^{f}\cdots$

等等, 依次类推. 由于整体的动量守恒, `n`点单圈图最多有 `n-1` 个线性独立的外动量,
所以分子中最多出现`n-1`个独立的外动量 $p_1\cdots p_n$, 再考虑到度规张量 $g^{\mu\nu}$,
这些洛伦兹结构的不同幂次组合, 形成每个圈积分特有的张量结构.

+ `PVX[...]` 隐式地依赖于时空维数`d`.
+ `PVX` 不直接求值, `LoopRefine` 也不会用它的显式表达式代替它.
+ N点系数函数的 `PVX` 表示中, 其参数的含义如下:
    + 前 `N` 个参数是整数, 并指定线性独立动量或度规的 `指标重数`;
    + 接下来的 `N(N-1)/2` 个参数是外部动量不变量;
    + 最后 `N` 个参数是内部质量.
    + 所以参数总共有 `N(N+3)/2` 个, 例如 `5点` 图有 `20` 个参数.
+ 对于 `N<=4 点` 的系数函数, `PVX` 被自动替换为 `PVA`, `PVB`, `PVC` 或 `PVD`.

### 需要注意的问题

对于 ChPT 费曼图, 设圈积分变量为 `k`, KR addition 图中, 非定域正规子的标量部分满足 `R(q-k)=R(k-q)`,
这里可能会有个 "bug", 与 `Package-X` 的实现有关系:

以下两个积分的结果应该是相同的, 因为传播子是动量的偶次幂, 与正负号无关.
但 Package-X 却不会自动给出相同的结果, 需要手动把 `-k` 换成 `+k`.
貌似这里 Package-X 没有自动整理动量, 把 `k`, `-k` 当成独立的动量, 它认为第二行有`5`个独立的动量:
`PVD` 的帮助页面也提到: `PVD` 不会自动整理它的参数, 猜测 `PVX` 也是如此.

```mathematica
LoopIntegrate[1, k, {k, m2}, {k, m2}, {k - p1 + p2, m2}, {k,m1}, {k - p1, mo1}]
LoopIntegrate[1, k, {k, m2}, {-k, m2}, {k + p1 - p2, m2}, {k, m1}, {k - p2, mo1}]
```
