# packageX

根据`package-X`的约定，圈积分的表达式不需要带上$1/(2\pi)^4$.

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

如果可能的话，还会使用高登`Gordon`恒等式,
$$\bar u(p^\prime)(p^\prime+p)^\mu u(p)=2m\;\bar{u}(p^\prime) \gamma^\mu u(p) -
i \bar{u}(p^\prime) \sigma^{\mu\nu} (p^\prime-p)_\nu u(p) $$

这些操作都是`LoopIntegrate` 自动进行的，如果分子里存在`FermionLine`.

## 圈积分

### LoopIntegrate

+ 给`LoopIntegrate`提供分子的列表，能获得对应圈积分的列表
+ `LoopRefine`能自动线性作用于列表.

如果外线动量不是线性独立的(例如，零转移动量时的形状因子), 分子分母中的公共因子不会自动消除.
输出会与`Cancel->False`情形的结果相同.

![LoopIntegrate](https://packagex.hepforge.org/Documentation/HTML/X/ref/Files/LoopIntegrate/61.png)

```mathematica
int1 = LoopIntegrate[k.k k.p, k, {k, m}, {k, 0}, {k + p, 0}, Cancel -> True]
```

设置选项`Apart->True`将分母用 部分 分式展开, 以使得分母上的因子能够和标量积相消:

```mathematica
int2 = LoopIntegrate[k.k k.p, k, {k, m}, {k, 0}, {k + p, 0},  Cancel -> True, Apart -> True]
```
