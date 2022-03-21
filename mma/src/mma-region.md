# Region

## Element(属于)

```mathematica
Element[x,dom]; 或 `x\[Element] dom` 断言 `x` 是域 `dom` 的元素.

Element[x,reg]; 或 `x\[Element] reg` 断言 `x` 是区域 `reg` 中的元素.

Element[x1, x2, ..., dom]; 断言所有 `x_i` 都是 `dom` 的元素.

Element[patt,dom]; 断言任何匹配模式 `patt` 的表达式, 是 `dom` 中的元素.
```

### 详细

+ $x \in dom$ 可以被输入为 `x esc el esc dom` 或 `x \[Element] dom`.
+ `Element` 可用于, 在 `Simplify` 和相关函数中, 设置 `assumptions`.
+ `dom` 可以是 `数域`(`domain`), 或 $R^n$ 中的 `区域`(`region`).
+ 可能的域 `dom`是:
    + `Algebraics`;     代数数字
    + `Booleans`;       `True` or `False`
    + `Complexes`;      复数
    + `Integers`;       整数
    + `Primes`;     质数
    + `Rationals`;      有理数
    + `Reals`;      实数

+ 可能的 regions  `reg` 是由 `RegionQ` 定义的.
+ 如果可能的话, 当 `x` 是数字时, `x [Element] dom` 会立即计算.
+ 对于 domain `dom`, `{x1,x2, ...} \[Element] dom` 等同于 `(x1 | x2 | ...) \[Element] dom`.
+ 对于 region `reg`, `{x1, x2, ...} \[Element] reg` 断言, 坐标为 `x1, x2, ...` 的点属于 `reg`.
+ 如果不能立即确定真假, `{x1, x2, ...} \[Element] dom` 将被转换成 `(x1 | x2 | ...) \[Element] dom`.

### 基础例子

+ 检验 `\[Pi]` 是否属于 `reals`:

```mathematica
In[1]:= Pi \[Element] Reals

Out[1]= True
```

+ 测试 点 `{1/2,1/3}` 是否属于 `单位盘`(unit disk):

```mathematica
{1/2, 1/3} \[Element] Disk[]
Out[1]= True
```

+ 表示`表达式`的所属域(domain membership):

```mathematica
Element[x + y, Reals]
Out[1]= x + y \[Element] Reals
```

+ 断言点 `{x,y,z}` 属于单位球:

```mathematica
Element[{x, y, z}, Ball[]]
Out[1]= {x, y, z} \[Element] Ball[{0, 0, 0}]
```

+ 使用 `element` 断言来对区域积分:

```mathematica
Integrate[1, {x, y, z} \[Element] Ball[]]
Out[2]= (4 \[Pi])/3
```

+ 或者在 `区域上` 进行优化(optimize):

```mathematica
MinValue[x + y, {x, y, z} \[Element] Ball[]]
Out[3]= -Sqrt[2]
```

+ 使用 `esc elem esc` 输入:

```mathematica
x \[Element] Reals
Out[1]= x \[Element] Reals
```

## Simplex

```mathematica
Simplex[{p1, ..., pk}]; 表示由点 p_i 所张成的 单纯形
```

### 细节

`单纯形` 也被称为 `点`, `线段`, `三角形`, `四面体`, `pentachoron`, `hexateron` 等.
单形表示给定点的所有 凸组合(convex combinations)

    { l1 p1 + ... + l_k p_k | l_i>=0 And l1+... + l_k==1}.

该区域是 $k-1$维的,
当 $p_i \in \mathbb R^n$ 且仿射独立(affinely independent), 且 $n\ge k-1$时,

+ 对于整数 `n`, `Simplex[n]` 等同于

    Simplex[{{0,...,0}, {1,0,...,0}, ..., {0, ..., 0, 1}}],

即$\mathbb R^n$中的单位标准单形.

+ Simplex 可以作为 `几何区域`(region) 和 图形基元(graphics primitive)使用.
+ 在图形中, 点 `p_i` 可以是 `Scaled` 或者 `Dynamic` 表达式.
+ 图形渲染受到 `FaceForm`, `EdgeForm`, `Opacity` 和 `color` 等指令(directive)的影响.

### 基本例子

+ `3D` 的单形:

```mathematica
Graphics3D[Simplex[{{0, 0, 1}, {1, 0, 0}, {1, 0, 1}, {1, 1, 1}}]]
```

+ `2D` 的单形:

```mathematica
Graphics[Simplex[{{0, 0}, {1, 1}, {2, 0}}]]
```

+ 不同风格的单形:

```mathematica
\[ScriptCapitalR] = Simplex[3];

{Graphics3D[{Pink, \[ScriptCapitalR]}],
Graphics3D[{EdgeForm[Thick], \[ScriptCapitalR]}],
Graphics3D[{Opacity[0.25], Blue, \[ScriptCapitalR]}],
Graphics3D[{EdgeForm[Directive[Thick, Dotted]], FaceForm[None], \[ScriptCapitalR]}]}
```

+ 体积和中心点:

```mathematica
\[ScriptCapitalR] = Simplex[{{1, 0, 0}, {1, 0, 1}, {1, 1, 1}, {0, 0, 1}}];
Volume[\[ScriptCapitalR]]
Out[2]= 1/6

RegionCentroid[\[ScriptCapitalR]]
Out[3]= {3/4, 1/4, 3/4}
```

### 应用

定义维数为 `n` 的库恩 单形(Kuhn simplex):

```mathematica
KuhnSimplex[n_] := Simplex@Table[
If[j < i, 1, 0]
, {i, 1, n + 1}
, {j, n}]
```

二维库恩单形:

```mathematica
KuhnSimplex[2]
Out[2]= Simplex[{{0,0},{1,0},{1,1}}]

Graphics[%]
```

三维库恩单形:

```mathematica
KuhnSimplex[3]
Out[4]= Simplex[{{0,0,0},{1,0,0},{1,1,0},{1,1,1}}]

Graphics3D[%]
```

`n`维中的测度是 `1/n!`:

```mathematica
Table[RegionMeasure[KuhnSimplex[n]], {n, 5}]
Out[6]= {1,1/2,1/6,1/24,1/120}
```

`n`维中的中心点是 `{n, n-1, ..., 1}/(n+1)`:

```mathematica
Table[RegionCentroid[KuhnSimplex[n]], {n, 5}]
Out[7]= {{1/2},{2/3,1/3},{3/4,1/2,1/4},{4/5,3/5,2/5,1/5},{5/6,2/3,1/2,1/3,1/6}}
```
