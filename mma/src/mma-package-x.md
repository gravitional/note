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

## 圈积分

### LoopIntegrate

+ 给`LoopIntegrate`提供分子的列表, 能获得对应圈积分的列表
+ `LoopRefine`能自动线性作用于列表.

如果外线动量不是线性独立的(例如, 零转移动量时的形状因子), 分子分母中的公共因子不会自动消除.
输出会与`Cancel->False`情形的结果相同.

![LoopIntegrate](https://packagex.hepforge.org/Documentation/HTML/X/ref/Files/LoopIntegrate/61.png)

```mathematica
int1 = LoopIntegrate[k.k k.p, k, {k, m}, {k, 0}, {k + p, 0}, Cancel -> True]
```

设置选项`Apart->True`将分母用 部分 分式展开, 以使得分母上的因子能够和标量积相消:

```mathe matica
int2 = LoopIntegrate[k.k k.p, k, {k, m}, {k, 0}, {k + p, 0},  Cancel -> True, Apart -> True]
```
