# mma 数值相关

tutorial/Numbers

## 数值精度

tutorial/Numbers#21155

正如在 `精确和近似的结果` 中所讨论的, Wolfram 语言可以处理 `任何位数` 的近似实数.
一般来说, 一个近似实数的`精度`(precision) 是指: 其中被视为对计算有显著影响(significant)的十进制数字的的`有效位数`.
`确度`(accuracy)是指出现在`小数点`右边的数字的`有效位数`.
请注意, 为了实现能对数字完全一致地处理, `精度`和`确度`的值往往是`整数位数`.

实数的`精度`和`确度`:

+ `Precision[x]`;   `x` 中有效十进制数字的`总位数`
+ `Accuracy[x]`;    `x` 中小数点右边的有效数字的`位数`

这将产生一个具有30位`精度`的数字:

```mathematica
x = N[Pi^10, 30]
Out[45]= 93648.0474760830209737166901849
```

## FunctionInterpolation

```mathematica
FunctionInterpolation[expr,{x, x_min, x_max}];
用 `x` 从 `x_min` 到 `x_max` 计算 expr, 并构造 `InterpolatingFunction` 对象,
代表与结果对应的近似函数.

FunctionInterpolation[expr,{x, x_min, x_max}, {y, y_min, y_max}, ...];
构建 `多参数` 的 `插值函数对象`.
```

### 细节和选项

你可以使用 `FunctionInterpolation`, 从包含几个 `FunctionInterpolation` 对象的表达式中, 生成单个 `InterpolatingFunction` 对象.

### 例子

计算 `Exp[-Sin[x]^2]` 的 `InterpolatingFunction` 表示:

```mathematica
f = FunctionInterpolation[Exp[-Sin[x]^2], {x, 0, 6}]
```

将产生的函数像其他函数那样使用:

```mathematica
Plot[{f[x], f'[x]}, {x, 0, 6}]
```

### 范围

使用导数来提高近似的平滑度:

```mathematica
flist=Table[D[Sin[x y], {{x, y}, k}], {k, 0, 2}]
f = FunctionInterpolation[flist, {x, 0., 2. Pi}, {y, 0., 2. Pi}]

Plot3D[f[x, y], {x, 0, 2 Pi}, {y, 0, 2 Pi}]
```

## InterpolatingFunction

```mathematica
InterpolatingFunction[domain,table]; 代表一个近似的函数, 它的值是通过 `插值` 找到的.
```

+ `InterpolatingFunction` 的工作方式如同 `Function`.
+ `InterpolatingFunction[...][x]` 给出近似函数在特定参数 `x` 处的值.
+ 在标准输出格式中, `InterpolatingFunction` 对象中只有 `domain` 元素被明确打印. 其余元素用 `<>` 表示.
+ `domain` 指定了构建 `InterpolatingFunction` 的数据域.
+ 如果你提供 `domain` 外的参数, 会产生警告, 然后返回 `外推值`(extrapolated).
+ 可以构造, 接受任何数量 `实参数` 的 `InterpolatingFunction` 对象.
+ 你可以使用 `D` 和 `Derivative` 来获取 `InterpolatingFunction` 对象的 `导数`.
+ `NDSolve` 使用 `InterpolatingFunction` 对象返回其结果.

### 例子

+ 制作 `InterpolatingFunction` 对象, 它将穿过给定的点:

    ```mathematica
    points = {{0, 0}, {1, 1}, {2, 3}, {3, 4}, {4, 3}, {5, 0}};
    ```

    在标准输出格式中, 只显示出域:

    ```mathematica
    ifun = Interpolation[points]
    ```

    在 `domain` 中的一个点上计算该函数:

    ```mathematica
    ifun[2.5]
    Out[3]= 3.6875
    ```

+ 在其域上绘制函数图, 显示 `内插点`:

```mathematica
Plot[ifun[x], {x, 0, 5}, Epilog -> Point[points]]
```

+ 获取 `微分方程的解` 的近似插值函数对象:

```mathematica
ifun = First[u /. NDSolve[{u''[t] + u[t] == 0, u[0] == 0, u'[0] == 1}, u, {t, 0, \[Pi]}]]
```

+ 绘制函数及其导数图:

    ```mathematica
    Plot[{ifun[t], ifun'[t]}, {t, 0, \[Pi]}]
    ```

    给出解的 `不定积分`:

    ```mathematica
    Integrate[ifun[t], t]
    Plot[%, {t, 0, \[Pi]}]
    ```

### 范围

用精确数据制作`插值函数`:

```mathematica
points = {{0, 0}, {1, 1}, {2, 3}, {3, 4}, {4, 3}, {5, 0}};
ifun = Interpolation[points]
```

+ 使用精确算术计算该值:

```mathematica
ifun[5/2]
Out[2]= 59/16
```

使用 `机器精度数字`(machine-number)算术进行计算:

```mathematica
ifun[N[5/2]]
Out[3]= 3.6875
```

使用任意精度的算术进行计算:

```mathematica
ifun[N[5/2, 20]]
Out[4]= 3.687500000000000000
```

用所有数据的`数值`(numerical values), 制作新的 `InterpolatingFunction`:

```mathematica
nifun = N[ifun]
```

使用此 `InterpolatingFunction`, `values` 是用 `机器精度算术` 得到的:

```mathematica
nifun[5/2] (*尽管输入是任意精度值*)
Out[6]= 3.6875
```

+ 对 `InterpolatingFunction` 积分:

```mathematica
ifun = Interpolation[{{0, 0}, {1, 1}, {2, 3}, {3, 4}, {4, 3}, {5, 0}}];
Integrate[ifun[x], {x, 0, 5}]

Out[2]= 271/24
```

+ 制作新的 `InterpolatingFunction`, 它是不定积分:

```mathematica
indef[x_] = Integrate[ifun[x], x]

Plot[{ifun[x], indef[x]}, {x, 0, 5}]
```

+ `InterpolatingFunction` 的 `导数`, 是另一个 `InterpolatingFunction`:

```mathematica
ifun = Interpolation[{{0, 0}, {1, 1}, {2, 3}, {3, 4}, {4, 3}, {5, 0}}];
difun = ifun'

Plot[{ifun[x], difun[x]}, {x, 0, 5}]
```

+ 使用 `插值函数` 的 `偏导数`, 来检查 `PDE` 的 留数(residual):

```mathematica
ifun = NDSolveValue[{
    D[u[t, x], t] == D[u[t, x], x, x],
    u[0, x] == Exp[-100 x^2], u[t, 0] == 1, u[t, 1] == 0}, u, {t, 0, 1}, {x, 0, 1}];

Plot[Derivative[1, 0][ifun][1, x] - Derivative[0, 2][ifun][1, x], {x, 0, 1}]
```

+ 制作一个需要 `4参数` 的 `插值函数`:

```mathematica
ifun = ListInterpolation[RandomReal[1, {5, 5, 5, 5}], {{0, 1}, {0, 2}, {0, 3}, {0, 4}}]
```

+ 把它沿着 `第一个`和 `最后一个`维度积分:

```mathematica
integ = Integrate[ifun[x1, x2, x3, x4], {x1, 0, 1}, {x4, 0, 4}]

Plot3D[integ, {x2, 0, 2}, {x3, 0, 3}]
```

### 性质和关系

`InterpolatingFunction` 进行 `Piecewise` 多项式插值(polynomial):

```mathematica
points = {{0, 0}, {1, 1}, {2, 3}, {3, 4}, {4, 3}, {5, 0}};
ifun = Interpolation[points]

pf[x_] = Piecewise[{
    {InterpolatingPolynomial[Take[points, {1, 4}], x], x < 2},
    {InterpolatingPolynomial[Take[points, {2, 5}], x], 2 <= x < 3},
    {InterpolatingPolynomial[Take[points, {3, 6}], x], x >= 3}}]

(*比较差别*)
Plot[pf[x] - ifun[x], {x, 0, 5}, PlotRange -> All]
```

## FindMinimum(求极小值和其坐标)

### 范围

可以指定 `Or` 型约束条件:

```mathematica
NMinimize[{x + y, x^2 + y^2 <= 1 ||
(x + 2)^2 + (y + 2)^2 <= 1}, {x, y}] // Quiet

Out[1]= {-5.41421, {x -> -2.70711, y -> -2.70711}}
```

对于线性目标(linear objectives)和约束, 使用 `NMinimize`:

```mathematica
NMinimize[{x + y, 3 x + 2 y >= 7 && x + 2 y >= 6 && x >= 0 && y >= 0}, {x, y}]
Out[1]= {3.25, {x -> 0.5, y -> 2.75}}
```

可以施加 `整数` 约束:

```mathematica
NMinimize[{x + y, 3 x + 2 y >= 7 && x + 2 y >= 6 && x >= 0 && y >= 0 && {x, y} \[Element] Integers}, {x, y}]
Out[1]= {4., {x -> 2, y -> 2}}

NMinimize[{(x - 1/3)^2 + (y - 1/3)^2, x \[Element] Integers}, {x, y}]
Out[2]= {0.111111, {x -> 0, y -> 0.333333}}
```

+ 在 `区域内`(region)最小化:

```mathematica
t = RotationTransform[{{0, 0, 1}, {1, 1, 1}}];
\[ScriptCapitalR] = TransformedRegion[Ellipsoid[{0, 0, 0}, {1, 2, 3}], t];

res=NMinimize[z, {x, y, z} \[Element] \[ScriptCapitalR]]

Out[2]= {-2.16025, {x -> -1.40386, y -> -0.60208, z -> -2.16025}}
```

作图:

```mathematica
Graphics3D[{{Opacity[0.5], Green,
    GeometricTransformation[Ellipsoid[{0, 0, 0}, {1, 2, 3}], t]},
    {Red, PointSize[Large], Point[{x, y, z} /. Last[res]]}}]
```

找到两个区域之间的最小距离:

```mathematica
Subscript[\[ScriptCapitalR], 1] = Disk[];
Subscript[\[ScriptCapitalR], 2] = InfiniteLine[{{-2, 0}, {0, 2}}];

res=NMinimize[(x - u)^2 + (y - v)^2,
{{x, y} \[Element] Subscript[\[ScriptCapitalR], 1],
{u, v} \[Element] Subscript[\[ScriptCapitalR], 2]}]

Out[2]= {0.171573, {x -> -0.707107, y -> 0.707106, u -> -1., v -> 1.}}
```

作图:

```mathematica
Graphics[{{LightBlue, Subscript[\[ScriptCapitalR], 1]},
{Green, Subscript[\[ScriptCapitalR], 2]},
{Red, Point[{{x, y}, {u, v}} /. Last[res]]}}]
```

+ 找到最小的 `r`, 使三角形和椭圆仍保持相交(intersect,相切):

```mathematica
Subscript[\[ScriptCapitalR], 1] = Triangle[{{0, 0}, {1, 0}, {0, 1}}];
Subscript[\[ScriptCapitalR], 2] = Disk[{1, 1}, {2 r, r}];

res=NMinimize[{r, {x, y} \[Element] Subscript[\[ScriptCapitalR], 1] &&
{x, y} \[Element] Subscript[\[ScriptCapitalR], 2]}, {r, x, y}]

Out[2]= {0.447213, {r -> 0.447213, x -> 0.2, y -> 0.8}}
```

作图:

```mathematica
Graphics[{{LightBlue, Subscript[\[ScriptCapitalR], 1],
Subscript[\[ScriptCapitalR], 2]},
{Red, Point[{x, y}]}} /. Last[res]]
```

+ 求包含给定三点的最小半径的`圆盘`:

```mathematica
Subscript[\[ScriptCapitalR], 3] = Disk[{a, b}, r];

res=NMinimize[{r, ({0, 0} | {1, 0} | {0, 1}) \[Element]
Subscript[\[ScriptCapitalR], 3]}, {a, b, r}]

Out[2]= {0.707107, {a -> 0.5, b -> 0.5, r -> 0.707107}}
```

作图:

```mathematica
Graphics[{{LightBlue, Subscript[\[ScriptCapitalR], 3]} /. res[[2]],
{Red, Point[{{0, 0}, {1, 0}, {0, 1}}]}}]
```

+ 使用 `Circumsphere` 可以直接得到同样的结果:

```mathematica
Circumsphere[{{0, 0}, {1, 0}, {0, 1}}] // N
Out[4]= Sphere[{0.5, 0.5}, 0.707107]
```

+ 使用 `x/[Element] R` 来指定 `x` 是 `R^3` 中的矢量:

```mathematica
\[ScriptCapitalR] = Sphere[];
NMinimize[x . {1, 2, 3}, x \[Element] \[ScriptCapitalR]]

Out[2]= {-3.74166, {x -> {-0.267261, -0.534522, -0.801784}}}
```

+ 找到两个 `regions` 之间的最小距离:

```mathematica
Subscript[\[ScriptCapitalR], 1] = Triangle[{{1, 1}, {2, 1}, {1, 2}}];
Subscript[\[ScriptCapitalR], 2] = Disk[];

res=NMinimize[ EuclideanDistance[x, y],
    {x \[Element] Subscript[\[ScriptCapitalR], 1],
    y \[Element] Subscript[\[ScriptCapitalR], 2]}]

Out[2]= {0.414214, {x -> {1., 1.}, y -> {0.707107, 0.707107}}}
```

作图:

```mathematica
Graphics[{{LightBlue, Subscript[\[ScriptCapitalR], 1],
Subscript[\[ScriptCapitalR], 2]}, {Red, Point[{x, y}]}} /. res[[2]]]
```

### 性质和关系

`FindMinimum` 试图找到 `局部最小值`; `NMinimize` 试图找到 `全局最小值`.

```mathematica
FindMinimum[{-100/((x - 1)^2 + (y - 1)^2 + 1) - 200/((x + 1)^2 + (y + 2)^2 + 1), x^2 + y^2 > 3}, {{x, 2}, y}]

NMinimize[{-100/((x - 1)^2 + (y - 1)^2 + 1) - 200/((x + 1)^2 + (y + 2)^2 + 1), x^2 + y^2 > 3}, {x, y}] ``
```

```mathematica
ContourPlot[-100/(((x - 1))^2 + ((y - 1))^2 + 1) - 200/(((x + 1))^2 + ((y + 2))^2 + 1)
,{x, -3, 2}, {y, -3, 2}, RegionFunction -> (#1^2 + #2^2 > 3 &), Contours -> 10
, Epilog -> ({Red, PointSize[.02], Text["global minimum", {-.995, -2.092}]
,Point[{-.995, -1.992}], Text["local minimum", {0.5304, 1.2191}], Point[{1.2304, 1.2191}]}), ContourLabels -> True]
```

`Minimize` 可以找到 `全局最小值`, 并且可以在无限精度下工作:

```mathematica
Minimize[{-100/((x - 1)^2 + (y - 1)^2 + 1) - 200/((x + 1)^2 + (y + 2)^2 + 1), x^2 + y^2 > 3}, {x, y}]

N[%]
```

`FindMinimum` 同时给出 `最小值` 和 `最小值的位置`:

```mathematica
FindMinimum[{x - 2 y, x^2 + y^2 <= 1}, {x, y}]
```

`FindArgMin` 给出了最小值的位置:

```mathematica
FindArgMin[{x - 2 y, x^2 + y^2 <= 1}, {x, y}]
```

`FindMinValue` 给出了最小值的 `值`:

```mathematica
FindMinValue[{x - 2 y, x^2 + y^2 <= 1}, {x, y}]
```
