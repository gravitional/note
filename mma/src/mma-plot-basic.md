# Plot

## plot 术语

`Frame`; 指图的外框架.
`Axes`; 坐标轴, 位于 `Frame` 中, 一般情形下, 不与 `Frame` 重合.
`Ticks`; 刻度线, 位于 `Axes` 上

## 画图包

[SciDraw](https://scidraw.nd.edu/)
[mark-caprio/SciDraw](https://github.com/mark-caprio/SciDraw)
[SciDraw — Mathematica 科学绘图包 ](https://pencilq.com/35/)

`SciDraw` 是一个用Mathematica编制具有出版质量的科学图表的系统.

`SciDraw` 既提供了一个排版 figures 的框架, 又提供了生成 figures 内容的工具.
`SciDraw` 有助于生成涉及数学图, 数据图 和 diagrams.
(mathematical plots, data plots, and diagrams).

该软件包允许对文本和图形的风格进行广泛的手动微调.

其结构框架(structural framework)包括

+ 生成 多面板 和 插图 (子图), 并对所有属性进行精细控制.
+ 自定义的刻度线 tick marks.
+ 样式定义(style definitions), 用于统一控制多个图形的格式和外观.
+ 用 `文本标签`(text labels) 和坐标轴(axes) 等对 figures 进行注释的工具.

任何您可以在Mathematica中制作或导入的图形(plots, images), 都可以很容易地包含在SciDraw图形中.

除了这些结构元素外, SciDraw还提供了一个面向对象的绘图系统(object oriented drawing system),
使许多难以绘制的科学图表变得相对容易生成--
例如, 通过帮助您为图中的对象(如几何图形, 箭头和数学曲线)自动附加文本标签, 并帮助您将这些对象相互定位.

SciDraw还提供了与Mathematica内置功能互补的, 数据绘图和图例生成功能(data plotting and legend generation).

SciDraw是LevelScheme的继承者[参见Comput.Phys.Commun.171, 107 (2005)].

LevelScheme软件包的设计目的是允许简单而有效地准备高质量的 `能级方案`(level schemes),
或`能级图`(level energy diagrams), 用于核, 原子, 分子和强子物理学.

这些功能被保留了下来.  与LevelScheme一样, SciDraw自动处理了准备 `level schemes` 的许多繁琐环节,
比如定位`levels`之间的过渡箭头, 或将文本标签放在它们所标注的对象旁边.

它还包括创建核物理学中常见的几种类型的`level schemes`的专门功能.

## Plot 采样

默认特性

+ 在函数值变化较快的位置, 使用更多的采样点:
+ 自动选择绘图范围:
+ 排除非实数的函数范围:
+ 函数中存在断点时断开曲线:
+ `plot`有`HoldAll`属性, 一般是先代入具体的值, 再计算`f(x)`, 为了强迫先计算`f(x)`再代入具体的坐标, 可以使用`Evaluate[f(x)]`

其他特性还有

+ 使用 `Exclusions->None` 绘制连续的曲线:
+ 用 `PlotPoints` 和 `MaxRecursion` 控制自适应采样:
+ 用 `PlotRange` 来突出显示感兴趣的区域:
+ 可以用区域来指定自变量的取值范围:  `D = ImplicitRegion[x <= -1 \[Or] x >= 1, {x}];`
+ 用 `MeshRegion` 来指定自变量的取值范围:
`D =MeshRegion[{{-2}, {-1}, {-1/2}, {1/2}, {1}, {2}}, Line[{{1, 2}, {3, 4}, {5, 6}}]];`
+ 用 `ScalingFunctions` 来缩放坐标轴:

example

```mathematica
Plot[Tan[x^3 - x + 1] + 1/(x + 3 Exp[x]), {x, -2, 2},
Exclusions -> {Cos[x^3 - x + 1] == 0, x + 3 Exp[x] == 0}]
```

### PlotRange

```mathematica
PlotRange ; 是图形函数的选项, 用于指定在绘图中包括哪些坐标范围.
```

### 细节

+ `PlotRange` 可以用于二维和三维图形.
+ 可以使用以下设置:
    + `All`; 所有的点都包括在内
    + `Automatic`; 放弃外围(outlying)的点
    + `Full`; 包括 `原始数据` 的全部范围
    + `max`; 每个函数的显式上限(见下文)
    + `{min,max}`; y(2D), z(3D), 或 `数组值` 的明确范围
    + `{{x_min, x_max}, {y_min, y_max}}`; `x` 和 `y` 的明确范围
    + `{{x_min, x_max}, {y_min, y_max}, {z_min, z_max}}`; `x`, `y`和 `z` 的明确范围(三维).

+ 当没有为特定的坐标给出明确的限制时, 就会假定设置为 `Automatica`.
+ 在 `Automatica` 设置下, 会寻找坐标值的 `分布`, 任何在分布中足够远的点都会被放弃.
这些点通常是由于被绘制的函数的奇异性而产生的.
+ 任何明确的 `limit` 或 `{min,max} pair` 都可以用 `All` 或 `Automatic` 这样的指定来代替.
+ 像 `{min,Automatic}` 这样的设置为某个坐标提供了特定的 `最小值`, 而最大值将被自动确定.
+ 如果特定的 `最小值` 或 `最大值` 被指定为 `{Automatic, alpha}`, 这意味着该范围实际上应该被切断, 对于超过绘图中的 `alpha` 部分的点.
当绘制平滑曲线或曲面时, 点的测度基于 `投影长度` 或 `面积`.

+ 设置 `Full` 可以在 `Plot` 和相关函数中使用, 以指定作图范围应该由 `输入到绘图函数的范围` 决定.
+ `Plot[f,{x, x_min, x_max}, PlotRange->Full]` 指定应该使用 `{x_min, x_max}` 的整个范围, 即使在该范围的一部分, 并没有实际绘制出 `f`.

+ 通过设置 `PlotRange->s`, 相当于使用以下范围:
    + `Graphics`;    {{-s,s},{-s,s}}
    + `Graphics3D`;    {{-s,s},{-s,s},{-s,s}}
    + `Plot`;    {Full,{-s,s}}
    + `ListPlot` and `ListLinePlot`;     {Full,{0,s}}
    + `ParametricPlot` and `RegionPlot`     {{-s,s},{-s,s}}
    + `ContourPlot` and `ListContourPlot`    {Full,Full,{-s,s}}
    + `DensityPlot` and `ListDensityPlot`    {Full,Full,{-s,s}}
    + `ArrayPlot`;    {Full,Full,{0,s}}
    + `Spectrogram` and `Cepstrogram`;    {Full,{0,s},Full}
    + `Plot3D` and `ListPlot3D`    {Full,Full,{-s,s}}
    + `ListSurfacePlot3D`;  {{-s,s},{-s,s},{-s,s}}
    + `ParametricPlot3D` and `RegionPlot3D`;    {{-s,s},{-s,s},{-s,s}}
    + `ContourPlot3D` and `ListContourPlot3D`;    {Full,Full,Full,{-s,s}}

+ `AbsoluteOptions` 给出了`Automatic`设置对应的 `PlotRange` 规范的显式形式.
+ `plot` 中包含的最终绝对坐标范围由 `PlotRangePadding` 和 `PlotRange` 决定.

## Legended,图例

```mathematica
Legended[expr,leg]; 使用图例 leg 显示 expr.
Legended[expr,lbl]; 在 plotting 和 charting 函数中, 表明应该为 expr 创建图例, label 为 lbl.
```

### 细节和选项

+ `Legended[expr,leg]` 中的内容 `expr` 和 `leg` 可以是任何东西, 包括 graphics, tables 和 images.
+ 在`ListPlot` 和 `BarChart` 等函数中, `Legended` 可以作为 `数据元素` 和 `数据集`的符号封装(symbolic wrapper).
+ 与 `Legended[expr,lbl]` 相关的图例条目包括, `expr` 的 identifying prototype, 比如color swatch, 与标签 `lbl` 一起显示.
+ `Legended[expr,lbl]` 中的标签 `lbl` 可以是任何表达式, 包括 `graphics`.
+ `Legended[expr,Placed[..., pos]]` 可以用来在自定义位置创建图例.
+ `pos` 的可能形式是:
    + `Above`, `Below`, `Before`, `After` ; 在 `expr` 的 `bounding box` 外的位置.
    + `{{e_x, e_y}, {l_x, l_y}}`; 图例的比例位置 `{l_x, l_y}` 也就是锚点, 处于 `expr` 中的比例位置 `{e_x, e_y}`.
+ 图例 `leg` 将经常由 `BarLegend` 和 `LineLegend` 等函数创建.

`Legended` 是可以嵌套的

```mathematica
Legended[
 Legended[
  PieChart[{1, 2, 3, 4}],
  Placed[
   "aaaa",
   {{0.5, 0.5},
    {0, 0}}
   ]
  ],
 Placed[
  "aaaa",
  {{0.5, 0.51},
   {0, 0}}
  ]
 ]
```

https://scidraw.nd.edu/

### 范围: Labeling and Legending

+ 使用 `Legended` 为特定的数据集提供图例:

```mathematica
upper = RandomReal[{1.5, 2}, 50];
lower = RandomReal[0.5, 50];
ListPlot[{lower, Legended[Mean[{lower, upper}], "average"], upper}]
```

+ 使用 `Placed` 来改变图例的位置:

```mathematica
ListPlot[{lower, Legended[Mean[{lower, upper}], Placed["average", Below]], upper}]
```

+ 同时使用 `Legended` 数据封装(wrapper), 和 `PlotLegends` 选项:

```
ListPlot[{
  Legended[Sqrt[Range[40]], Placed["sqrt2", Right]],
  Legended[Log[Range[40]], Placed["log2", Left]]},
 PlotLegends -> {"sqrt"}
 ]
```

### 细节和选项

可以给出以下选项:

+ `Joined`;     `True`;  是否绘制 shapes
+ `LabelStyle`;    `Automatic`; 用于标签的样式
+ `LegendFunction`;     `Identity`;  图例整体的封装(wrapper)
+ `LegendLabel`;    `None`; 图例的整体标签
+ `LegendLayout`;       `Automatic`; 使用的图例布局(layout)
+ `LegendMargins`;   `None`;  图例整体和边界之间的空间
+ `LegendMarkers`;      `None`;  用于指示每个元素的标记(markers)
+ `LegendMarkerSize`;   `Automatic` 形状(shape)的大小

### 图例的大小,记号

### 高度自定义

`LegendLayout` 是 legends 的选项, 指定如何格式化 legend 内容.

+ `LegendLayout` 的可能设置是:
    + `Automatic`; 自动布局
    + `"Row"`; 从左到右布局
    + `"Column"`; 从下到上的布局
    + `"ReversedRow"`; 从右到左的布局
    + `"ReversedColumn"`; 从上到下的布局
    + `f`; 布局由函数 `f` 决定
+ 对于 `LineLegend`, `PointLegend` 和 `SwatchLegend`, 函数 `f` 接收到的参数是一个列表,
即样式和相关标签: `{{style1, label1},{style2, label2}, ...}`.
+ 对于 `BarLegend`, `f` 的参数是一个颜色函数 `cf`, 值的范围 `{min,max}`, 以及 contour值的列表: `contours`.

+ 图例要实现最一般的排版效果, 可以传入一个排版函数`f`, 例如:

    ```mathematica
    table[pairs_] := TableForm[pairs,
    TableHeadings -> {{"Group A", "Group B", "Group C"}, {"color", "mascot"}}, TableAlignments -> Center]

    SwatchLegend[63, {"lion", "whale", "rocket"}, LegendLayout -> table]
    ```

    所以要自定义图例之间的间隔, 可以指定排版函数`f`.

## Ticks,刻度线

`Ticks` 是 graphics 函数的选项, 用于指定 `axes`(坐标轴) 的 `tick marks`.
如果只想指定 `刻度`, 但不附加 `label`, 可以使用 `{{x1, ""}, {x2, ""}, ...}` 的规范, 也就是传入 `空字符串`. 例如:

```mathematica
Plot[Sin[x], {x, 0, 10}, Ticks -> {{{Pi, ""}, {2 Pi, 360 \[Degree]}, {3 Pi, 540 \[Degree]}}, {-1, 1}}]
```

也可以传入 `Null`, 或者使用 `f[e1, , e2]` 的语法.

### 细节

+ 下列设置可用于 `Ticks`:
    + `None`;不画刻度
    + `Automatic`; 自动放置刻度线
    + `{xticks,yticks,...}`; 为每个`axis` 分别指定 `刻度线` 选项

+ 在 `Automatic` 设置下, 刻度线通常被放置在, 十进制表示中,  坐标的 `数字位数` 最少的 点上.

+ 对于每个`轴`, 可以给出以下 `刻度线` 选项:
    + `None`; 不画刻度线
    + `Automatica`; 自动选择 `刻度线` 位置和 `标签`
    + `{x1,x2, ...}`; 在指定位置画出 `刻度线`
    + `{{x1, label1}, {x2, label2}, ...}`; 用指定的 `标签` 画出 `刻度线`
    + `{{x1, label1, len1}, ...}`; 用指定的 scaled 长度绘制 `刻度线`.
    + `{{x1, label1, {plen1, mlen1}}, ...}`;  在正负方向上分别指定,刻度线的长度
    + `{{x1, label1, len1, style1,}, ...}`; 具有指定 `样式` 的刻度线
    + `func`; 应用于`x_min, x_max`的函数, 以给出 `刻度线` 的规格.

+ 如果没有给出明确的 `标签`, 默认将 `刻度线位置` 的 `数值` 作为 `标签`.
+ 任何`表达式`, 都可以作为 `刻度线标签`.
+ `Tick` 的长度, 以横跨整个 `plot` 的距离的比例来给出.
+ `Tick` 样式可以包括任何 `图形指令`.
+ `Tick mark`  函数 `func[x_min, x_max]` 可以返回任何其他 `tick mark`选项.
+ `Ticks` 可以在二维和三维图形中使用.
+ `AbsoluteOptions` 给出 `Automatica` 对应的 `Ticks` 规范的显式.
+ `TicksStyle` 给出用于 `刻度线` 和 `刻度线标签` 的默认样式.

### 推广

指定一个 `grid` 函数, 应用于每个方向的 `x_min` 和 `x_max` 值:

```mathematica
ticks[min_, max_] := Table[If[EvenQ[i], {i, i, .06, Red}, {i, i, .02, Blue}], {i, Ceiling[min], Floor[max], 1}]

Graphics[Circle[{0, 0}, 4], Axes -> True, Ticks -> ticks]
```

### 应用

在每个 `整数` 处放 `带标签` 的大刻度, 中间放 `小刻度`:

```mathematica
ticks[min_, max_] := Join[
    Table[{i, Style[i, 12], {.04, 0}}, {i, Ceiling[min], Floor[max]}],
    Table[{j + .5, , {.02, 0}}, {j, Round[min], Round[max - 1], 1}]]

Plot3D[Sin[x y], {x, 0, 4}, {y, 0, 4}, Ticks -> ticks, Mesh -> None]
```

### 性质和关系

+ `TicksStyle` 影响 `ticks` 和 `tick labels`:

```mathematica
Plot[2 Sin[x], {x, 0, 10}, AxesLabel -> {x, y}, PlotLabel -> 2 Sin[x],TicksStyle -> Orange]
```

`LabelStyle` 提供了整体 `style`, 赋予所有 label-like 元素, 包括 tick labels:

```mathematica
Plot[2 Sin[x], {x, 0, 10}, PlotLabel -> 2 Sin[x], AxesLabel -> {x, y},
  LabelStyle -> Directive[Blue, FontFamily -> "Helvetica"]]
```

`TicksStyle` 可以和 `LabelStyle` 一起使用, 但优先级更高:

```mathematica
Plot[2 Sin[x], {x, 0, 10}, PlotLabel -> 2 Sin[x], AxesLabel -> {x, y},
  LabelStyle -> Directive[Blue, FontFamily -> "Helvetica"],
 TicksStyle -> Orange]
```

`AxesStyle` 影响 axes(坐标轴), axes labels, ticks 和 tick labels:

```mathematica
Plot[2 Sin[x], {x, 0, 10}, AxesLabel -> {x, y}, PlotLabel -> 2 Sin[x],
  TicksStyle -> Orange]
```

`TicksStyle` 也可以和 `AxesStyle` 一起使用, 但优先级更高:

```mathematica
Plot[2 Sin[x], {x, 0, 10}, AxesLabel -> {x, y}, PlotLabel -> 2 Sin[x],
  AxesStyle -> Directive[Gray, FontSize -> 15], TicksStyle -> Orange]
```

+ 可以使用单独定制的 `ticks`, 夹杂在其他 ticks 中间, 它自己样式的优先级更高:

```mathematica
Plot[Sin[x], {x, 0, 10},
 Ticks -> {{0, {Pi, Pi, 1, Directive[Blue, Thick]}, 2 Pi, 3 Pi}, {-1,
    1}}, AxesStyle -> Directive[Gray, Dashed],
 TicksStyle -> Directive[Orange, 12]]
```

`FrameTicks` 控制 `frame edges` 的 `ticks` 和 `tick labels`:

```mathematica
Graphics[Circle[], Frame -> True, FrameTicks -> All]
```

`GridLines` 在图片中加上网格:

```mathematica
Graphics[Circle[], GridLines -> Automatic, Axes -> True]
```

`FaceGrids` 在 3D 包装盒的各个面上添加网格:

```mathematica
Graphics3D[Cylinder[], FaceGrids -> All, Axes -> True]
```

### 可能的问题

位于坐标原点的 `tick label` 不显示:

```mathematica
Plot[Sin[x], {x, -4, 4}, Ticks -> {{-Pi, 0, Pi}, {-1, 0, 1}}]
```
