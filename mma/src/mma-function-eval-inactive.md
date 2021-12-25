# 激活和闲置形式

## Inactivate

+ `Inactivate[expr]` ; 用 `Inactive[f]` 替换`f`,  其中 `f` 为 `expr` 中任意作为头部的符号.
+ `Inactivate[expr,patt]` ; 闲置 `expr` 中所有与模式 `patt` 匹配的符号.

### 细节

+ `Inactivate` 具有 `HoldFirst` 属性, `expr` 中的符号在`evaluation` 前被 `闲置`.
+ 通过选项设置 `Heads->False`, `Inactivate` 不进入表达式的头部, 只 `inactivate` 它们的`parts`.
+ 默认情况下, 某些语义上(semantically)重要的头部不会 `inactivated`. 常见的例子包括`List`, `Rule`和`Blank`.

### 例子

+ Inactivate 表达式:

```mathematica
Inactivate[Length[{a, b, c}]]
Out[1]= Inactive[Length][{a, b, c}]
```

Activate 它:

```mathematica
Activate[%]
Out[2]= 3
```

+ 激活有多个项的表达式:

```mathematica
expr = Inactivate[2 + 2 + 3^2]
2+2+3^2
```

激活表达式的不同部分:

```mathematica
Activate[expr, Plus]
4+3^2

Activate[expr, Power]
2+2+9

Activate[expr]
13
```

+ 闲置 `表达式` 中的一个 `符号`:

```mathematica
Inactivate[Sum[k^2, {k, 1, n}], Sum]

Inactive[Sum][k^2, {k, 1, n}]
```

计算表达式:

```mathematica
Activate[%]
1/6 n (1 + n) (1 + 2 n)
```

### 范围

闲置表达式:

```mathematica
expr = Inactivate[Cos[Pi Sin[Pi]]]
Cos[Pi*Sin[Pi]]
```

计算表达式:

```mathematica
Activate[expr]
1
```

只闲置符号 `g` :

```mathematica
Inactivate[f[g[h[x]], y, g[z]], g]
f[Inactive[g][h[x]], y, Inactive[g][z]]
```

激活 `g`:

```mathematica
Activate[%]
f[g[h[x]], y, g[z]]
```

闲置 `g` 和 `h`:

```mathematica
Inactivate[f[g[h[x]], y, g[z]], g | h]

f[Inactive[g][Inactive[h][x]], y, Inactive[g][z]]
```

只激活 `h`:

```mathematica
Activate[%, h]
f[Inactive[g][h[x]], y, Inactive[g][z]]
```

激活表达式中除 `Integrate` 以外的所有符号:

```mathematica
Inactivate[Integrate[Sin[x] + Cos[x], x], Except[Integrate]]
```

激活表达式:

```mathematica
Activate[%]
-Cos[x] + Sin[x]
```

防止数字函数被闲置:

```mathematica
Inactivate[2 + Integrate[Sin[x], x], Except[_?(MemberQ[Attributes[#], NumericFunction] &)]]

2 + Inactive[Integrate][Sin[x], x]
```

通常情况下,  `Plus` 和 `Sin` 也会被闲置:

```mathematica
Inactivate[2 + Integrate[Sin[x], x]]

2 + Inactive[Integrate][Inactive[Sin][x], x]
```

### 选项

+ 默认情况下,  `Inactivate` 使 `heads` 闲置, 即使在 `复合头部`(compound heads) 中:

```mathematica
Inactivate[f[x][y] + g[z]]

Out[1]:=Inactive[Plus][Inactive[f][x][y], Inactive[g][z]]
```

+ 若使用 `Heads->False` 选项, 则 `inactivation` 不会深入到 `复合头部`:

```mathematica
Inactivate[f[x][y] + g[z], Heads -> False]

Out[2]:=Inactive[Plus][f[x][y], Inactive[g][z]]
```

### 性质和关系

`Activate`  是 `Inactivate` 的逆:

```mathematica
Inactivate[f[x]]
Inactive[f][x]

Activate[%]
f[x]
```

+ `Inactivate` 将 `特定符号` 替换成 `闲置形式`:

```mathematica
Inactivate[f[x] + g[x] + h[x], f | g] // FullForm

Plus[h[x], Inactive[f][x], Inactive[g][x]]
```

`Activate` 将所有 `闲置符号` 实例替换为 `活动形式`:

```mathematica
Activate[%]
f[x] + g[x] + h[x]
```

+ `Inactivate`  维护符号的 `不激活形式`, 并允许指定表达式的某些 `部分` 不激活:

```mathematica
isin = Inactivate[Sin[ArcTan[1]], Sin] (*这里指定了 Sin*)
Out[1]= Inactive[Sin][\[Pi]/4]

Activate[isin]
Out[2]= 1/Sqrt[2]
```

`Hold` 保持表达式的 `未计算形式`, 表达式的 `所有部分` 都 `inactive`:

```mathematica
esin = Hold[Sin[ArcTan[1]]]
Out[3]= Hold[Sin[ArcTan[1]]]

ReleaseHold[esin]
Out[4]= 1/Sqrt[2]
```

将 `inactive` 表达式与相应的 `FullForm` 进行比较:

```mathematica
Inactivate[Cos[x Sin[y]]]
Cos[x*Sin[y]]

% // FullForm
Inactive[Cos][Inactive[Times][x,Inactive[Sin][y]]]

FullForm[Cos[x Sin[y]]]
Cos[Times[x,Sin[y]]]
```

Inactivate是幂等(idempotent) 算符:

```mathematica
Inactivate[Inactive[Sin][x], Sin] // InputForm

Inactive[Sin][x]
```

某些 `heads` 在默认情况下不会被 `闲置`, 包括 `List`, `Rule`(->)和 `Blank`(_):

```mathematica
Inactivate[{f[x], a -> b, x_}]
{Inactive[f][x], a -> b, x_}
```

在 全级别 使用 `Replace` 可以使表达式中的所有 `heads` 不被激活:

```mathematica
Replace[{f[x], a -> b, x_}, h_[args___] :> Inactive[h][args], {0, -1}]
List[f[x],a->b,Pattern[x,Blank[]]]
```

### 巧妙范例

创建定积分的图库:

```mathematica
defint =Inactivate[{
    Integrate[1/(x^4 + x^2 + 2), {x, 0, Infinity}], Integrate[Cos[Sin[x]^2], {x, 0, Pi/2}],
    Integrate[AiryAi[x]^2, {x, 0, Infinity}], Integrate[Ceiling[x^2 + Abs[3*x - 1]], {x, 0, 1}],
    Integrate[Floor[x^2], {x, 0, 2}], Integrate[Log[(1/2)*(1 + Sqrt[1 + 4*x])]/x, {x, 0, 1}],
    Integrate[Product[Sin[x/(2*k + 1)], {k, 0, 5}]/x^6,
    {x, 0, Infinity}]}, Product | Integrate];

FormulaGallery[forms_List] := Module[{vals = ParallelMap[Activate, forms]},
    Text[Grid[Table[{forms[[i]], "=", vals[[i]]}, {i, Length[forms]}],
        Dividers -> {{True, False, False, True}, All},
        Alignment -> {{Right, Center, Left}, Baseline},
        Background -> LightYellow, Spacings -> {{6, {}, 6}, 1}]]];

FormulaGallery[defint] // TraditionalForm
```

## Inactive(闲置)

### 细节

+ `Inactive[f][args]` 实际上是 `f[args]` 的 `纯符号`形式, 在这种形式中不做与 `f` 相关的计算.
+ 使用 `Inactivate` 可以方便地将 `Inactive` 插入表达式中.
+ `Inactive[f]` 显示在 `StandardForm` 中, `f` 或任何与 `f` 相关的特殊输出形式显示为灰色.
+ `Inactive` 不影响 `TraditionalForm`.
+ `Inactive` 具有 `HoldFirst` 属性, 并且不计算其 `首参数`.
+ `Inactive[atom]` 给出 `atom`, 对 `symbols` 以外的原子表达式.

### 例子

Inactive `Length`:

```mathematica
Inactive[Length][{a, b, c}]
```

计算表达式:

```mathematica
Activate[%]
3
```

闲置 `Plus`:

```mathematica
Inactivate[2 + 2]
2+2
```

显示 `激活` 和 `失活`形式的等式:

```mathematica
% == Activate[%]
2+ 2 == 4
```

不活跃的对象在 `StandardForm` 中是灰色的:

```mathematica
Inactive[Sin][0]
```

但在 `TraditionalForm` 中不是这样:

```mathematica
TraditionalForm[%]
sin(0)
```

### 范围

#### 基本用例

定义 `inactive` 表达式:

```mathematica
expr = Inactive[Sin][\[Pi]/2]
Inactive[Sin][\[Pi]/2]
```

使用 `Activate` 计算该表达式:

```mathematica
Activate[expr]
1
```

使用 `Inactivate` 创建 `inactive` 的表达式:

```mathematica
expr = Inactivate[Sin[Pi/2], Sin]
Inactive[Sin][\[Pi]/2]
```

计算表达式

```mathematica
Activate[expr]
1
```

带有两个 `非活动项` 的表达式:

```mathematica
expr = Inactive[Cos][\[Pi]/3] + Inactive[Sin][\[Pi]/3]
Inactive[Cos][\[Pi]/3] + Inactive[Sin][\[Pi]/3]
```

激活表达式的不同部分:

```mathematica
Activate[expr, Sin]
Sqrt[3]/2 + Inactive[Cos][\[Pi]/3]

Activate[expr, Cos]
1/2 + Inactive[Sin][\[Pi]/3]

Activate[expr]
1/2 + Sqrt[3]/2
```

仅仅 `Inactivate` 符号 `g`:

```mathematica
Inactivate[f[g[h[x]], y, g[z]], g]

f[Inactive[g][h[x]], y, Inactive[g][z]]
```

Activate g:

```mathematica
Activate[%]
f[g[h[x]], y, g[z]]
```

Inactivate `g` 和 `h`:

```mathematica
Inactivate[f[g[h[x]], y, g[z]], g | h]

f[Inactive[g][Inactive[h][x]], y, Inactive[g][z]]
```

Activate `h`:

```mathematica
Activate[%, h]
f[Inactive[g][h[x]], y, Inactive[g][z]]
```

#### 形式操作, Formal Operations

积分的 `非活动形式`:

```mathematica
Inactive[Integrate][1/(1 + x), x]

Inactive[Integrate][1/(1 + x), x]
```

微分 `inactive` 的形式:

```mathematica
D[%, x]
1/(1 + x)
```

+ 对 `拉普拉斯变换` 形式微分:

```mathematica
Inactive[LaplaceTransform][a t^2, t, s]
Inactive[LaplaceTransform][a t^2, t, s]

D[%, s]
Inactive[LaplaceTransform][-a t^3, t, s]

Activate[%]
-((6 a)/s^4)

D[LaplaceTransform[a t^2, t, s], s]
-((6 a)/s^4)
```

同样地, 对 `t` 和 `a` 进行微分:

```mathematica
e = Inactive[LaplaceTransform][a t^2, t, s]
Inactive[LaplaceTransform][a t^2, t, s]

D[e, t]
0

D[e, a]
Inactive[LaplaceTransform][t^2, t, s]
```

+ 微分算子, 包括 `积分`:

```mathematica
Inactive[Integrate][f[x], {x, a[x], b[x]}]

D[%, x]
-f[a[x]] Derivative[1][a][x] + f[b[x]] Derivative[1][b][x]
```

`FourierTransform`:

```mathematica
Inactive[FourierTransform][f[t], t, \[Omega]]

D[%, \[Omega]]
Inactive[FourierTransform][I t f[t], t, \[Omega]]
```

`FourierSinTransform`:

```mathematica
Inactive[FourierSinTransform][f[t], t, \[Omega]]

D[%, \[Omega]]
Inactive[FourierCosTransform][t f[t], t, \[Omega]]
```

`Convolve`:

```mathematica
Inactive[Convolve][f[t], g[t], t, s]

D[%, s]
Inactive[Convolve][f[t], Derivative[1][g][t], t, s]
```

`Sum`:

```mathematica
Inactive[Sum][f[k, y], {k, a, b}]

D[%, y]
```

`ZTransform`:

```mathematica
Inactive[ZTransform][f[k], k, z]

D[%, z]
-(Inactive[ZTransform][k f[k], k, z]/z)
```

+ 差分算子, 包括 `Sum`:

```mathematica
Inactive[Sum][f[k], k]

DifferenceDelta[%, k]
f[k]
```

`DiscreteConvolve`:

```mathematica
Inactive[DiscreteConvolve][f[m], g[m], m, n]

DifferenceDelta[%, n]
Inactive[DiscreteConvolve][f[m], Inactive[DifferenceDelta][g[m], m], m, n]
```

Integrate:

```mathematica
Inactive[Integrate][f[x], {x, a, b}]

DifferenceDelta[%, b]
```

LaplaceTransform:

```mathematica
Inactive[LaplaceTransform][f[t], t, s]

DifferenceDelta[%, s]
```

其他形式算子, 包括 `Product`:

```mathematica
Inactive[DiscreteRatio][f[k], k]

Product[%, k]
```

ZTransform:

```mathematica
Inactive[InverseZTransform][F[z], z, k + 1]

ZTransform[%, k, z]
z F[z] - z Inactive[InverseZTransform][F[z], z, 0]

Inactive[InverseZTransform][F[z], z, k]
Inactive[InverseZTransform][F[z], z, k]

ZTransform[k %, k, z]
-z Derivative[1][F][z]
```

#### 代码转换

`Inactivate` 函数的定义:

```mathematica
Inactivate[forSquares[f_, max_] := For[x = 1, x < max, x++, Print[f[x^2]]]]
```

将 `For` 语句转化为 `Do` 语句:

```mathematica
% //. Inactivate[For[i_ = init_, i_ < max_, i_++, body_] :> Do[body, {i, init, max}]]

forSquares[f_,max_]:=Do[Print[f[x^2]],{x,1,max}]
```

`Activate` 并使用 `定义`:

```mathematica
Activate[%]

forSquares[Range, 3]

{1}
{1,2,3,4}
{1,2,3,4,5,6,7,8,9}
```

用迭代的版本取代 `With`, 这样 `后面的变量` 就可以引用 `前面的变量`, 即 racket 中的 `letrec` :

```mathematica
(* HoldAll 保持参数不计算 *)
SetAttributes[IterateWith, HoldAll]
(* 对 scoped 变量列表进行迭代, 后面的变量将引用前面的变量, 一直递归到 首参量 *)
IterateWith[expr_] := Activate[Inactivate[expr] //.
    Inactive[With][{first_, rest__}, body_] :>  Inactive[With][{first}, Inactive[With][{rest}, body]]]

With[{a = 3, b = a + 2, c = a + b}, {a, b, c}]
{3, 2 + a, a + b}

IterateWith[With[{a = 3, b = a + 2, c = a + b}, {a, b, c}]]
{3, 5, 8}
```

Inactivate `Length`:

```mathematica
Inactivate[{Length[{a, b, c}], Length[{b, c}],
    Length[{Length[{a}], Length[{b}], Length[{c}], 7}]}, Length]
```

将 `measure` 改成长度的平方:

```mathematica
% //. Inactive[Length][x_] :> Length[x]^2
{9, 4, 16}
```

### 应用

#### 基本等式

展示求和等式:

```mathematica
Block[{e = Inactivate[23 + 11]}, e == Activate[e]]
23 + 11 == 34
```

做一个完整的等式列表:

```mathematica
Table[Block[{e = Inactivate[n + m]}, e == Activate[e]], {n, 0, 3}, {m,0, 3}] // Grid // TraditionalForm

0+0==0    0+1==1    0+2==2    0+3==3
1+0==1    1+1==2    1+2==3    1+3==4
2+0==2    2+1==3    2+2==4    2+3==5
3+0==3    3+1==4    3+2==5    3+3==6
```

显示乘法等式:

```mathematica
Block[{e = Inactivate[3 5]}, e == Activate[e]]

3*5==15
```

做一个完整的等式列表:

```mathematica
Table[Block[{e = Inactivate[n m]}, e == Activate[e]], {n, 0, 3}, {m,0, 3}] // Grid // TraditionalForm

0*0==0    0*1==0    0*2==0    0*3==0
1*0==0    1*1==1    1*2==2    1*3==3
2*0==0    2*1==2    2*2==4    2*3==6
3*0==0    3*1==3    3*2==6    3*3==9
````

显示代数等式:

```mathematica
Block[{e = Inactivate[x + 1 + 5 x]}, e == Activate[e]] // TraditionalForm
x+1+5*x==6 x+1

Block[{e = Inactivate[x x x^3]}, e == Activate[e]]
x*x*x^3==x^5
```

+ 常见的三角函数值:

```mathematica
Table[Inactive[Sin][k \[Pi]/6] == Sin[k \[Pi]/6], {k, 0, 3}] //
  Column // TraditionalForm

sin(0)==0
sin(\[Pi]/6)==1/2
sin(\[Pi]/3)==Sqrt[3]/2
sin(\[Pi]/2)==1
```

#### 函数等式

`Sin` 是参数的奇函数;

```mathematica
Block[{e = Inactivate[Sin[-x], Sin]}, e == Activate[e]] // TraditionalForm
sin(-x)==-sin(x)
```

`Cos` 是其参数的偶数函数:

```mathematica
Block[{e = Inactivate[Cos[-x], Cos]}, e == Activate[e]] // TraditionalForm

cos(-x)==cos(x)
```

+ 带 `虚值参数` 的 `双曲函数`(Hyperbolic) 等同于三角函数:

```mathematica
Block[{e = Inactivate[Sinh[I x], Sinh]}, e == Activate[e]] // TraditionalForm
sinh(I x)==I sin(x)

Block[{e = Inactivate[Cosh[I x], Cosh]}, e == Activate[e]] // TraditionalForm
cosh(I x)==cos(x)
```

`BesselJ[1,x]` 是 `x` 的 `奇函数`:

```mathematica
Block[{e = Inactivate[BesselJ[1, -x], BesselJ]}, TraditionalForm[e] == Activate[e]]

BesselJ(1,-x)==-BesselJ[1,x]
```

`BesselJ[2,x]` 是 `x` 的 `偶函数`:

```mathematica
Block[{e = Inactivate[BesselJ[2, -x], BesselJ]}, TraditionalForm[e] == Activate[e]]

BesselJ(2,-x)==BesselJ[2,x]
```

#### 微积分等式

显示等式, 包括莱布尼茨的对积分微分规则:

```mathematica
Inactivate[D[Integrate[f[x], {x, a[x], b[x]}], x], D | Integrate] == D[Integrate[f[x], {x, a[x], b[x]}], x]
```

乘法规则:

```mathematica
Inactivate[D[f[x] g[x], x], D] == D[f[x] g[x], x]
```

链式法则(Chain rule):

```mathematica
Inactivate[D[f[g[x]], x], D] == D[f[g[x]], x]
```

不定积分:

```mathematica
Inactivate[Integrate[x^n, x], Integrate] == Integrate[x^n, x]

Inactivate[Integrate[1/Sqrt[1 - x^2], x], Integrate] == Integrate[1/Sqrt[1 - x^2], x]
```

+ 不定求和与乘积:

```mathematica
Inactivate[Sum[1/n^2, {n, 1, Infinity}], Sum] == Sum[1/n^2, {n, 1, Infinity}] // TraditionalForm
Inactive[Sum][1/n^2, {n, 1, \[Infinity]}] == \[Pi]^2/6

Inactivate[Product[1 - 1/n^4, {n, 2, Infinity}], Product] == Product[1 - 1/n^4, {n, 2, Infinity}] // TraditionalForm
Inactive[Product][(1 - 1/n^4), {n, 2, \[Infinity]}] == sinh(\[Pi])/( 4 \[Pi])
```

+ 有限和无限的连分数:

```mathematica
Inactive[ContinuedFractionK][1, 2, {n, 1, m}] == ContinuedFractionK[1, 2, {n, 1, m}] // TraditionalForm

Inactive[ContinuedFractionK][1, 2, {n, 1, m}] == 2/((2 Sqrt[2] ((-3 - 2 Sqrt[2])^m + 1))/((-3 - 2 Sqrt[2])^m - 1) + 2)

Inactive[ContinuedFractionK][n, {n, 1, Infinity}] == ContinuedFractionK[n, {n, 1, Infinity}] // TraditionalForm

Inactive[ContinuedFractionK][n, {n, 1, DirectedInfinity[1]}] == BesselI[1,2]/BesselI[0,2]
```

+ 将 `DifferenceDelta` 应用于 `非活动求和`:

```mathematica
Timing[DifferenceDelta[Inactive[Sum][(1 + x)^150, x], x]]
{0., (1 + x)^150}
```

这明显比 显式计算 版本快:

```mathematica
Timing[DifferenceDelta[Sum[(1 + x)^150, x], x]]
{1.700411, (1 + x)^150}
```

分部求和的公式(summation by parts):

```mathematica
sumparts = Inactivate[
    Sum[f[k] g[k], k] == g[k] Sum[f[k], k] - Sum[DifferenceDelta[g[k], k] DiscreteShift[Sum[f[k], k], k], k],
    Sum | DifferenceDelta | DiscreteShift]
```

在特殊情况下验证该公式:

```mathematica
f[k_] := k
g[k_] := PolyGamma[k]

Activate[sumparts]
Out[4]= True
```

计算求和:

```mathematica
Inactive[Sum][k PolyGamma[k], k] == Sum[k PolyGamma[k], k]
```

+ 交换求和与积分的顺序:

```mathematica
(id = Inactivate[
    Sum[Integrate[t^(n - 1) E^(-t)/(2 n + 1)!!, {t, 0, Infinity}], {n, 1, Infinity}] == Integrate[
        Sum[E^(-t) t^(n - 1)/(2 n + 1)!!, {n, 1, Infinity}], {t, 0, Infinity}], Sum | Integrate]) // TraditionalForm
```

计算等式的两边:

```mathematica
{Activate[id[[1]]], Activate[id[[2]]]} // Together

{(4 - \[Pi])/2, (4 - \[Pi])/2}
```

用 `相应的求和` 获得相同的结果:

```mathematica
Sum[Gamma[n]/(2*n + 1)!!, {n, 1, Infinity}]

(4 - \[Pi])/2
```

+ `拉普拉斯` 算子的乘积规则:

```mathematica
Inactivate[Laplacian[f[x, y] g[x, y], {x, y}] == f[x, y] Laplacian[g[x, y], {x, y}] +
    g[x, y] Laplacian[f[x, y], {x, y}] + 2 Grad[f[x, y], {x, y}] . Grad[g[x, y], {x, y}], Laplacian | Grad] // TraditionalForm

Activate[%] // Simplify
True
````

+ 三向量 `u`, `v`, 和 `w` 的向量等式:

```mathematica
u = {ux, uy, uz};
v = {vx, vy, vz};
w = {wx, wy, wz};
```

叉积的 `反对称性`:

```mathematica
Inactivate[Cross[v, v], Cross] == Cross[v, v]
{vx,vy,vz}\[Cross]{vx,vy,vz}=={0,0,0}

% // Activate
True
```

交叉积的正交性(Orthogonality of the cross product):

```mathematica
Inactive[Dot][u, Inactive[Cross][u, v]] == Inactive[Dot][v, Inactive[Cross][u, v]] == 0

% // Activate // Simplify
```

三重标量积:

```mathematica
Inactive[Dot][u, Inactive[Cross][v, w]] == Inactive[Det][{u, v, w}]

% // Activate // Simplify
```

#### 导数恒等式

在 `积分` 或 `求和` 符号下进行 `微分` 的基本交换(commutation)技巧:

```mathematica
Inactivate[Integrate[D[f[x, a], a], x] == ((D[Integrate[f[x, a], x], a]))]

Inactivate[Sum[D[f[x, a], a], x] == ((D[Sum[f[x, a], x], a]))]
```

通过在 `a==1` 处对 `E^(a x)/[DifferentialD]x` 进行微分, 推导出 `E^x` 的封闭形式:

```mathematica
D[Inactive[Integrate][E^(a x), x], a]

lhs = % /. a -> 1
Inactive[Integrate][E^x x, x]
```

现在积分 `E^(a x)/[DifferentialD]x`, 然后在 `a==1` 处对 `a` 进行微分:

```mathematica
Integrate[E^(a x), x]
E^(a x)/a

D[%, a]
-(E^(a x)/a^2) + (E^(a x) x)/a

rhs = % /. a -> 1 // FullSimplify
E^x (-1 + x)
```

最终结果:

```mathematica
lhs == rhs
Inactive[Integrate][E^x x, x] == E^x (-1 + x)
```

验证结果:

```mathematica
Activate[%]
True
```

+ 通过在 `零` 对 `x` 微分 `Integrate[x^a/E^x, {x, 1, Infinity}]`, 推导出`Integrate[(Log*x)/E^x, {x, 1, Infinity}]` 的封闭形式:

```mathematica
D[Inactive[Integrate][E^(-x) x^a, {x, 1, \[Infinity]}], a]

lhs = % /. a -> 0
```

首先对 `Integrate[E^(-x) x^a, {x, 1, \[Infinity]}]` 积分, 然后在 `零` 对 `a` 进行微分:

```mathematica
Integrate[E^(-x) x^a, {x, 1, \[Infinity]}]
Gamma[1 + a, 1]

D[%, a]
MeijerG[{{}, {1, 1}}, {{0, 0, 1 + a}, {}}, 1]

rhs = % /. a -> 0 // FullSimplify
-ExpIntegralEi[-1]
```

最终结果:

```mathematica
lhs == rhs

Inactive[Integrate][E^-x Log[x], {x, 1, \[Infinity]}] == -ExpIntegralEi[-1]
```

验证结果:

```mathematica
Activate[%]
True
```

+ 通过在 `a==-1` 处对 `Sum[E^(a k), k]` 进行微分, 推导出 `Sum[E^(- k), k]`的封闭形式:

```mathematica
D[Inactive[Sum][E^(a k), k], a]

lhs = % /. a -> -1
```

计算 `Sum[E^(a k), k]`, 然后 微分:

```mathematica
Sum[E^(a k), k]
E^(a k)/(-1 + E^a)

D[%, a]
-(E^(a + a k)/(-1 + E^a)^2) + (E^(a k) k)/(-1 + E^a)

rhs = % /. a -> -1 // Simplify
-((E^(1 - k) (1 + (-1 + E) k))/(-1 + E)^2)
```

最终结果:

```mathematica
lhs == rhs

Inactive[Sum][E^-k k, k] == -((E^(1 - k) (1 + (-1 + E) k))/(-1 + E)^2)
```

验证结果:

```mathematica
Activate[%] // Simplify
True
```

+ 通过在 `a==-1` 对 `a` 微分 `Sum[Exp[a k^2], {k, -\[Infinity], \[Infinity]}]`, 推导出 `Sum[Exp[a k^2], {k, -\[Infinity], \[Infinity]}]` 的封闭形式:

```mathematica
D[Inactive[Sum][Exp[a k^2], {k, -\[Infinity], \[Infinity]}], a]

lhs = % /. a -> -1
```

计算 `Sum[Exp[a k^2], {k, -\[Infinity], \[Infinity]}]`, 然后 微分:

```mathematica
Sum[Exp[a k^2], {k, -\[Infinity], \[Infinity]}]
EllipticTheta[3, 0, E^a]

D[%, a]
E^a*Derivative[0, 0, 1][EllipticTheta][3, 0, E^a]

rhs = % /. a -> -1
```

最终结果:

```mathematica
lhs == rhs
```

推广到 `Sum[Exp[- k^2] k^(2n) , {k, -\[Infinity], \[Infinity]}]`

```mathematica
Table[D[Inactive[Sum][Exp[a k^2] , {k, -\[Infinity], \[Infinity]}], {a, n}] ==
    D[Sum[Exp[a k^2] , {k, -\[Infinity], \[Infinity]}], {a, n}] /.a -> -1, {n, 1, 3}] // Column
```

#### 求解微分方程

三维 `拉普拉斯方程` 的 `非活动` 积分形式的解:

```mathematica
V[x_, y_, z_] := Inactive[Integrate][f[z + I x Cos[u] + I y Sin[u], u], {u, -\[Pi], \[Pi]}]
```

通过指定函数 `f` 获得特定的解:

```mathematica
f[a_, b_] := 2 a^5 + b^4

V[x, y, z]
Out[3]= Inactive[Integrate][(u^4 + 2 (z + I x Cos[u] + I y Sin[u])^5), {u, -\[Pi], \[Pi]}]

sol = Activate[%] // Simplify
(2 \[Pi]^5)/5 + 1/2 \[Pi] z (15 (x^2 + y^2)^2 - 40 (x^2 + y^2) z^2 + 8 z^4)
```

可视化解:

```mathematica
Row[Table[
    Plot3D[sol /. {z -> j}, {x, -3, 3}, {y, -3, 3}, Ticks -> {Automatic, Automatic, None}], {j, -2, 2}]]
```

验证解:

```mathematica
Laplacian[sol, {x, y, z}] // Simplify
0
```

+ `麦克斯韦方程`, 以自然 洛伦兹--海维赛德单位(natural Lorentz--Heaviside units):

```mathematica
Inactivate[{
    gaussE = Div[\[ScriptCapitalE][x, y, z, t], {x, y, z}] == \[Rho][x, y, z, t], gaussB = Div[\[ScriptCapitalB][x, y, z, t], {x, y, z}] == 0,
    faraday = Curl[\[ScriptCapitalE][x, y, z, t], {x, y, z}] == -D[\[ScriptCapitalB][x, y, z, t], t],
    ampere = Curl[\[ScriptCapitalB][x, y, z, t], {x, y, z}] == j[x, y, z, t] + D[\[ScriptCapitalE][x, y, z, t], t]},
    Div | Curl | D];
    % // TableForm // TraditionalForm
```

在真空中取安培定律的 curl(j==0, Rho==0):

```mathematica
Inactive[Curl][#, {x, y, z}] & /@ ampere /. j -> (0 &)
```

交换微分次序:

```mathematica
% /. Inactive[Curl][Inactive[D][v_, t_], x_] :> Inactive[D][Inactive[Curl][v, x], t]
```

代入法拉第定律:

```mathematica
% /. Solve[faraday, Inactive[Curl][\[ScriptCapitalE][x, y, z, t], {x, y, z}]][[1]]
```

激活该方程, 得出磁场的波动方程:

```mathematica
wave = Activate[%]
```

验证方程的平面波解:

```mathematica
wave /. \[ScriptCapitalB] -> (A Exp[{kx, ky, kz} . {#1, #2, #3} - I Sqrt[kx^2 + ky^2 + kz^2] #4] &) // Simplify
True
```

#### 推导最小二乘法, Least-Squares Solution

定义一组给定数据的 `垂直偏差` 的平方之和:

```mathematica
squareDeviations = Inactivate[Sum[(y[[x]] - (a x + b))^2, {x, 1, n}], Sum | Part]
```

建立最小二乘法方程:

```mathematica
eqna = D[squareDeviations, a] == 0
eqnb = D[squareDeviations, b] == 0
```

生成一些数据:

```mathematica
n = 200; y = Table[3 x + 20 + RandomReal[{-15, 15}], {x, 1, n}];
ListPlot[y]
```

解决此数据的最小二乘法问题:

```mathematica
a x + b /. Solve[Activate[{eqna, eqnb}], {a, b}][[1]]
20.5208 + 2.99139 x
```

#### 代码转换

联合 `特有变量` 和 `Block`, 代替使用 `Module`:

```mathematica
Blockhead[icode_] := icode //. Inactive[Module][vars_, body_] :>(
    (*将局部变量替换成 新名变量 *)
    Inactive[Block][vars, body] /. (rule = Thread[# -> Unique[#]]) &[
        (* 提取出局部变量 旧名列表 *)
        Replace[vars, (Inactive[Set][var_, val_] | var_) :> var, {1}]])

code = Inactivate[f[s_] := Module[
    {x, y = Module[{x = 5 s}, x + 7]},
    x = y ^2; Sin[x]]]
```

应用该函数将 `Module` 局部变量替换为唯一变量:

```mathematica
bcode = Blockhead[code]
```

`Activate` 代码和转换后的代码, 为 `f` 和 `fb` 做出定义:

```mathematica
Activate[code];
Activate[bcode /. f -> fb];
```

比较随机测试值的数值:

```mathematica
testvalues = RandomReal[1, 5];
{f[#], fb[#]} & /@ testvalues

Out[7]= {{0.87208, 0.87208}, {-0.998674, -0.998674}, {0.517433, 0.517433}, {0.891984, 0.891984}, {0.247783, 0.247783}}
```

### 性质和关系

可以使用 `Inactivate` 来创建 `Inactive` 表达:

```mathematica
Inactivate[f[x], f]
Inactive[f][x]
```

`Inactive` 的表达式可以使用 `Activate` 来计算:

```mathematica
f[x_] := x^2
expr = Inactive[f][3]
Out[2]= Inactive[f][3]

Activate[expr]
Out[3]= 9
```

+ `Inactive` 创建符号的 `闲置形式`, 并允许表达式的 `某部分` 闲置:

```mathematica
isin = Inactive[Sin][ArcTan[1]]
Out[1]= Inactive[Sin][\[Pi]/4]

Activate[isin]
Out[2]= 1/Sqrt[2]
```

`Hold` 保持表达式的 `不计算形式`, 所有部分都不活动:

```mathematica
esin = Hold[Sin[ArcTan[1]]]
Out[3]= Hold[Sin[ArcTan[1]]]

ReleaseHold[%]
Out[4]= 1/Sqrt[2]
```

+ 将 `闲置表达式` 与相应的 `FullForm` 进行比较:

```mathematica
Inactivate[Cos[x  Sin[y]]]
Cos[x*Sin[y]]

FullForm[Cos[x  Sin[y]]]
Cos[Times[x,Sin[y]]]
```

### 巧妙范例

创建多元求和的图库:

```mathematica
multisums = Inactivate[{
    Sum[1/(2*n)^m, {m, 2, Infinity}, {n, 1, Infinity}],
    Sum[1/(4*n - 1)^(2*m), {m, 1, Infinity}, {n, 1, Infinity}],
    Sum[Zeta[i + j]/2^(i + j), {i, 1, Infinity}, {j, 1, Infinity}],
    Sum[1/(i + j)!, {i, 1, Infinity}, {j, 1, Infinity}],
    Sum[1/Max[i, j]!, {i, 1, Infinity}, {j, 1, Infinity}],
    Sum[1/Max[i, j]^3, {i, 1, Infinity}, {j, 1, Infinity}],
    Sum[1/(i*j + j*k)^s, {i, 1, Infinity}, {j, 1, Infinity}, {k, 1, Infinity}],
    Sum[Zeta[m + 2*n]/4^(m/2 + n), {m, 1, Infinity}, {n, 1, Infinity}],
    Sum[1/(i + 2*j + k)^4, {i, 0, Infinity}, {j, 0, Infinity}, {k, 1, Infinity}]},
    Sum];

FormulaGallery[forms_List] := Module[{vals = ParallelMap[Activate, forms]},
    Text[Grid[Table[{forms[[i]], "=", vals[[i]]}, {i, Length[forms]}], Dividers -> {{True, False, False, True}, All},
    Alignment -> {{Right, Center, Left}, Baseline},
    Background -> LightYellow, Spacings -> {{4, {}, 4}, 1}]]];

FormulaGallery[multisums] // TraditionalForm
```

## Activate

```mathematica
Activate[expr] 将 `expr` 中所有 `Inactive[f]` 的实例替换为 `f`
Activate[expr,patt] 只替换 `f` 与模式 `patt` 匹配的 `Inactive[f]` 的实例
```

### 细节和选项

在选项设置 `Heads->False` 的情况下, `Activate` 不会进入表达式的 `heads` 并激活其子部.

### 例子

Activate `inactive` 的表达式:

```mathematica
Activate[Inactive[Length][{a, b, c}]]
Out[1]= 3
```

`激活` `inactive` 表达式的不同部分:

```mathematica
expr = Inactivate[2 + 2 + 3^2]
2+2+3^2

Activate[expr, Plus]
4+3^2

Activate[expr, Power]
2+2+9

Activate[expr]
13
```

### 范围

定义 `闲置` 表达式:

```mathematica
expr = Inactive[Sin][\[Pi]/2]
Inactive[Sin][\[Pi]/2]
```

使用 `Activate` 计算该表达式:

```mathematica
Activate[expr]
1
```

使用 `Inactivate` 创建 `inactive` 表达式:

```mathematica
expr = Inactivate[Sin[Pi/2], Sin]
Out[1]= Inactive[Sin][\[Pi]/2]
```

计算表达式:

```mathematica
Activate[expr]
1
```

只闲置符号 `g`:

```mathematica
Inactivate[f[g[h[x]], y, g[z]], g]
f[Inactive[g][h[x]], y, Inactive[g][z]]
```

激活 `g`:

```mathematica
Activate[%]
f[g[h[x]], y, g[z]]
```

激活 `g` 和 `h`:

```mathematica
Inactivate[f[g[h[x]], y, g[z]], g | h]
f[Inactive[g][Inactive[h][x]], y, Inactive[g][z]]
```

激活 `h`:

```mathematica
Activate[%, h]
f[Inactive[g][h[x]], y, Inactive[g][z]]
```

激活闲置表达式:

```mathematica
expr=Cos[\[Pi]]+\[Integral]x\[DifferentialD]x;

Activate[expr]
-1 + x^2/2
```

激活 `Integrate` 之外的表达式:

```mathematica
Activate[expr, Except[Integrate]]
-1 + Inactive[Integrate][x, x]
```

防止 `数值函数` 被激活:

```mathematica
Activate[expr, Except[_?(MemberQ[Attributes[#], NumericFunction] &)]]
Cos[\[Pi]]+x^2/2
```

对 `拉普拉斯变换` 进行形式上的微分:

```mathematica
Inactive[LaplaceTransform][a t^2, t, s]
LaplaceTransform[a t^2,t,s]

D[%, s]
LaplaceTransform[-a t^3,t,s]

Activate[%]
-((6 a)/s^4)

D[LaplaceTransform[a t^2, t, s], s]
-((6 a)/s^4)
```

同理, 对 `t` 和 `a` 进行微分:

```mathematica
D[Inactive[LaplaceTransform][a t^2, t, s], t]
0

D[Inactive[LaplaceTransform][a t^2, t, s], a]
Inactive[LaplaceTransform][t^2, t, s]
```

+ `Inactive` 特殊函数表达式:

```mathematica
expr = Inactivate[Hypergeometric2F1[3, 1, 2, x], Hypergeometric2F1]
Inactive[Hypergeometric2F1][3, 1, 2, x]
```

自动简化后的表达式:

```mathematica
Activate[expr]
(2 - x)/(2 (1 - x)^2)

Hypergeometric2F1[3, 1, 2, x]
(2 - x)/(2 (1 - x)^2)
```

### 选项

`inactive` Derivative 表达式:

```mathematica
inactive = Inactivate[Derivative[1][Cos][x]]
Inactive[Derivative][1][Cos][x]
```

激活表达式:

```mathematica
Activate[inactive]
-Sin[x]
```

使用选项设置 `Heads->False` 来避免激活 `Derivative`:

```mathematica
Activate[inactive, Heads -> False]
Inactive[Derivative][1][Cos][x]
```

### 应用

+ 定义有两个闲置项的三角函数表达式:

```mathematica
expr = Inactivate[Sin[\[Pi]/3] + Cos[\[Pi]/3] + Tan[\[Pi]/3], Sin | Cos]
Sqrt[3] + Inactive[Cos][\[Pi]/3] + Inactive[Sin][\[Pi]/3]
```

激活表达式的不同部分:

```mathematica
Activate[expr, Sin]
(3 Sqrt[3])/2 + Inactive[Cos][\[Pi]/3]

Activate[expr, Cos]
1/2 + Sqrt[3] + Inactive[Sin][\[Pi]/3]

Activate[expr]
1/2 + (3 Sqrt[3])/2
```

定义 `D[Integrate[(t + x)^2, {t, 0, x}], x]`, 让导数和积分都 inactive:

```mathematica
inactive = Inactivate[D[Integrate[(t + x)^2, {t, 0, x}], x], D | Integrate]
```

对积分进行微分, 而不对积分进行计算:

```mathematica
Activate[inactive, D]
4 x^2 + Inactive[Integrate][2 (t + x), {t, 0, x}]
```

激活积分, 计算最终结果:

```mathematica
di = Activate[%]
7 x^2
```

在不进行微分的情况下进行 积分:

```mathematica
Activate[inactive, Integrate]
Inactive[D][(7 x^3)/3, x]
```

激活微分, 计算出最终结果:

```mathematica
id = Activate[%]
7 x^2
```

结果在数学上是相同的:

```mathematica
Simplify[di - id]
0
```

+ 三维拉普拉斯方程的非活动积分形式的解:

```mathematica
V[x_, y_, z_] = Inactivate[Integrate[f[z + I x Cos[u] + I y Sin[u], u], {u, -Pi, Pi}], Integrate]
Inactive[Integrate][f[z + I x Cos[u] + I y Sin[u], u], {u, -\[Pi], \[Pi]}]
```

通过指定函数 `f` 获得特定的解:

```mathematica
f[a_, b_] := 3 a^5 + 7 b^4
V[x, y, z]
Inactive[Integrate][(7 u^4 + 3 (z + I x Cos[u] + I y Sin[u])^5), {u, -\[Pi], \[Pi]}]

sol = Activate[%] // Simplify
(14 \[Pi]^5)/5 + 3/4 \[Pi] z (15 (x^2 + y^2)^2 - 40 (x^2 + y^2) z^2 + 8 z^4)
```

可视化解:

```mathematica
Row[Table[Plot3D[sol /. {z -> j}, {x, -3, 3}, {y, -3, 3}, Ticks -> {Automatic, Automatic, None}], {j, -2, 2}]]
```

验证解:

```mathematica
Laplacian[sol, {x, y, z}] // Simplify
0
```

分部求和公式:

```mathematica
sumparts = Inactivate[
    Sum[f[k] g[k], k] == g[k] Sum[f[k], k] - Sum[DifferenceDelta[g[k], k] DiscreteShift[Sum[f[k], k], k], k],
    Sum | DifferenceDelta | DiscreteShift]
```

在特殊情况下验证该公式:

```mathematica
f[k_] := k
g[k_] := HarmonicNumber[k]

Activate[sumparts]
True
```

计算求和:

```mathematica
Inactive[Sum][k HarmonicNumber[k], k] == Sum[k HarmonicNumber[k], k]
```

探索矢量恒等式:

```mathematica
divcurl = Inactivate[
    Div[Curl[{f[x, y, z], g[x, y, z], h[x, y, z]}, {x, y, z}], {x, y, z}], Div | Curl]
```

激活 `Curl` 并不是很有趣:

```mathematica
Activate[divcurl, Curl]
```

激活 `Div` 证明关系 $\nabla\cdot (\nabla \times v)=0$:

```mathematica
Activate[divcurl, Div]
0
```

### 关系和性质

`Inactive` 表达式可以使用 `Activate` 来计算:

```mathematica
f[x_] := x^2
expr = Inactive[f][3]
Inactive[f][3]

Activate[expr]
9
```

`Activate` 是 `Inactivate`的逆:

```mathematica
Inactivate[f[x], f]
Inactive[f][x]

Activate[%]
f[x]
```

`Activate` 替换表达式中所有 `inactive` 符号的实例:

```mathematica
Inactivate[f[x] + g[x] + h[x], f | g]
h[x] + Inactive[f][x] + Inactive[g][x]

Activate[%]
f[x] + g[x] + h[x]
```

`Activate` 计算`inactive`的表达式, 并允许表达式的部分`inactive`:

```mathematica
isin = Inactivate[Sin[ArcTan[1]]]
Inactive[Sin][Inactive[ArcTan][1]]

Activate[isin, Sin]
Sin[Inactive[ArcTan][1]]

Activate[%]
1/Sqrt[2]
```

`ReleaseHold` 计算保持为 `unevaluated` 的表达式, 所有部分都被计算:

```mathematica
esin = Hold[Sin[ArcTan[1]]]
Hold[Sin[ArcTan[1]]]

ReleaseHold[%]
1/Sqrt[2]
```

### 巧妙范例

创建不定求和的画册:

```mathematica
infiniteproducts = Inactivate[{
    Product[((k + 1)^3*(k + 5))/k^2, {k, 1, n}],
    Product[k!, {k, 0, n}],
    Product[1 + 1/k^2, {k, 1, Infinity}],
    Product[1 - (4/3)*Sin[x/3^k]^2, {k, Infinity}],
    Product[1 + 1/Prime[k]^s, {k, Infinity}],
    Product[((k + 3)/(k + 1))^k, {k, 1, n}],
    Product[Sin[3*k + 5]/Cos[3*k + 1], {k, 1, n}]},
    Product];

FormulaGallery[forms_List] := Module[{vals = ParallelMap[Activate, forms]},
    Text[Grid[Table[{forms[[i]], "=", vals[[i]]}, {i, Length[forms]}],
    Dividers -> {{True, False, False, True}, All},
    Alignment -> {{Right, Center, Left}, Baseline},
    Background -> LightYellow, Spacings -> {{4, {}, 4}, 1}]]];

FormulaGallery[infiniteproducts] // TraditionalForm
```

