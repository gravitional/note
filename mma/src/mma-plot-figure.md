# mma 画图

## 图形结构

tutorial/TheStructureOfGraphics

基本思想是 Wolfram 语言用 `图形基元` 的集合表示所有图形.
`图形基元`包括代表图像基本元素的 `Point` (点), `Line` (线) 和 `Polygon` (多边形), 以及 `RGBColor` 和 `Thickness` 等指令.

`InputForm` 告诉 Wolfram 语言如何表示图形. 每个点被表示为一个 `Point` 图形基元的坐标形式.

在 Wolfram 语言中,  每个完整的图形块都用图形对象表示.
图形对象的种类很多, 分别对应于不同类型的图形. 每类图形对象都有确定的头部以表明它的类型.

+ `Graphics[list]` 生成二维图形
+ `Graphics3D[list]` 生成三维图形

Plot 和 ListPlot 等在 "图形和声音的结构" 中讨论的函数都是按照先建立 Wolfram 语言内部图形对象, 然后显示它们的顺序工作的.

### 重画和组合图形

tutorial/RedrawingAndCombiningPlots

`Wolfram` 语言的所有图形都是表达式, 其操控方式与其它表达式相同. 这些操控不要求使用 `Show`.
在 Wolfram 语言中, 用户可以自行建立图形对象产生其它类型的图像.
由于在 Wolfram 语言中的图形对象是符号表达式, 所以能用所有的 Wolfram 语言标准函数对其进行操作.

### 自定义图表和图形

howto/CustomizePlotsAndGraphics

修改图形的局部和全局方式

+ 图形指令(Directive) ; 例如: `RGBColor`, `Thickness`
+ 图形选项(Option) ; 例如: `PlotRange`, `Ticks`, `AspectRatio`, `ViewPoint`

给定一个 `图形基元` 列表后, Wolfram 语言提供了两种方式去修改最终的图形.
首先, 可以在图形基元列表中插入一些图形指令, 例如 `RGBColor`, 以修改随后列表中的图形基元.
用这种方式, 用户可以指定如何修改一个给定的图形基元列表.

通过插入图形指令, 可以指定图形基元的显示方式. 然而, 用户往往经常会希望通过全局修改来改变整个图形的显示.
使用图形选项可以达到这一目的. 通过增加图形选项 `Frame` 用户可以修改图形的整体外观.

`FullGraphics[g]` 将图形选项指定的对象转化为明确的图形基元列表

对 `Axes` 等图形选项, Wolfram 语言的前端会自动画出用户需要的坐标轴等对象.
这些对象由选项值表示, 而非被确定的图形基元列表表示.
然而, 用户会需要要找到代表这些对象的图形基元列表.
函数 `FullGraphics` 给出不使用任何选项的情况下, 生成图形的完整的图形基元列表.

```mathematica
Short[InputForm[FullGraphics[ListPlot[Table[EulerPhi[n], {n, 10}]]]],6]
```

## Plot

### Plot 采样

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

## 图形叠加

tutorial/TheStructureOfGraphicsAndSound
tutorial/GraphicsAndSound#9501

实现`图形叠加`的函数有

+ `Show `;  合并要显示的图形, 也可以改变选项
+ `Overlay`;  堆叠图形或任何其他表达方式
+ `Canvas`; (画布)代表一个可以与图形结合的自由画布.

+ `Inset`; 将一个图形或表达式插入到一个更大的图形中.
+ `GraphicsGroup`; 使图形作为一个组被选择.
+ `双击`; "向下钻取"(drill down) `inset` 或 `group`内的图形

+ `Epilog`, `Prolog`; 在 `graphics` 中包含额外的材料

## Overlay

```mathematica
Overlay[{expr1, expr2, ... }]; 显示为所有 expr_i 的叠加.
Overlay[{expr1, expr2, ...},{i,j,...}]; 显示为 expr_i, expr_j, ... 的叠加.
Overlay[{expr1, expr2, ...},{i,j,...}, s]; 允许在 expr_s 中进行选择和点击控制.
```

### 细节和选项

+ `expr_i` 可以是图形, 文本或任何其他表达式.
+ 在 `Overlay[{expr1, expr2, ...}]` 中, 后面的 `expr_i` 被 渲染在前者的上面.
+ `Overlay[exprs]` 等同于 `Overlay[exprs,All,None]`, 显示为所有 `exprs` 的叠加.
+ `Overlay[exprs]` 默认不允许在 `exprs` 中进行选择.
+ 可以给出以下选项:
    + `Alignment`; `{Automatic,Automatic}`; 如何对齐显示区域中的对象
    + `Background`; `None`; 使用的背景颜色
    + `BaselinePosition`; `Automatic`; 与周围文本基线对齐的位置.
    + `BaseStyle`; `{}`; 被显示对象的基本样式规范
    + `ContentPadding`; `True`; 是否紧缩内容周围的边距(margin)
    + `FrameMargins`; `Automatic`; 整体 frame 内, 要留出的边距
    + `ImageMargins`; `0`; 被显示对象的图像周围的边距
    + `ImageSize`;  `All`; 被显示对象的整体图像尺寸

+ 在默认选项设置 `ImageSize->All` 的情况下, `Overlay` 总是为要显示的最大的`expr_i`留出空间, 因此其整体尺寸不会改变.
+ 在选项设置 `ImageSize->Automatic` 时, `Overlay` 只为当前显示的`expr_i`留出空间.
+ `BaseStyle` 的设置被附加到, 当前样式表中 `"Overlay"` 样式所给出的默认样式上.

### 基本例子

+ Overlay 两个表达式:

    ```mathematica
    Overlay[{2, 4}]
    ```

+ Overlay 两个图形:

    ```mathematica
    Overlay[{Plot[Sin[x], {x, 0, 6}, PlotRange -> 2], Plot[Sin[x] + .1 Sin[10 x], {x, 0, 6}, PlotRange -> 2]}]
    ```

### 范围

+ Overlay 许多对象:

    ```mathematica
    Style[Overlay[Range[9]], 48]
    ```

+ Overlay 二维和三维的图形:

    ```mathematica
    Overlay[{Plot3D[Sin[x y], {x, 0, 3}, {y, 0, 3}], Plot[Sin[x], {x, 0, 6}]}]
    ```

+ Overlay 文本 和 graphics:

    ```mathematica
    Overlay[{StringTake[ExampleData[{"Text", "ToBeOrNotToBe"}], 400], Plot[Sin[E^x], {x, 0, 5}]}]
    ```

+ 使用非默认排序

    ```mathematica
    Overlay[{Button["short"], Button["\t\tlong"], Button["\tmedium"]}, {2,3, 1}]
   ```

+ 允许点击第二层:

    ```mathematica
    Overlay[{Slider2D[], Graphics[{Opacity[.2], Disk[]}]}, All, 1]
    ```

### 推广和延伸

+ 单个元素的`Overlay`在行为上类似于 `PaneSelector`:

    ```mathematica
    Grid[{{SetterBar[Dynamic[x], {1, 2, 3}],
       Overlay[{"aaa", "bbb", "ccc"}, Dynamic[{x}]],
       PaneSelector[{1 -> "aaa", 2 -> "bbb", 3 -> "ccc"}, Dynamic[x]]}}, Frame -> All]
   ```

### 选项

#### Alignment

排列对象:

```mathematica
Overlay[{Graphics[{Disk[]}], Slider2D[]}, All, 2, Alignment -> Center]
```

#### Background

设置背景:

```mathematica
Overlay[{Graphics3D[Sphere[]], Panel["a sphere"]}, Background -> LightOrange]
```

#### BaseStyle

设置 `Overlay` 的 base style:

```mathematica
Overlay[{1, 2, 3}, BaseStyle -> {FontSize -> 24}]
```

### 应用

+ 从已有的字符创建新字符:

    ```mathematica
    Overlay[{"\[DownRightTeeVector]", "\[DownRightVectorBar]"}]
    ```

+ 给输出加水印:

    ```mathematica
    Overlay[{Manipulate[x, {x, 0, 1}],
      Graphics[Text[Style["Confidential", Bold, Red, Opacity[.1], 40], {0, 0}, {0, 0}, {1, .3}], ImageSize -> {250, 100}]}]
    ```

+ 将不可见的`控件`叠加到图像上, 创建一个图像 map:

    ```mathematica
    calcButtonFactory[row_, col_] := With[{
        val =Switch[{row, col}, {1, 3}, ".", {1, _}, 0, _, (row - 2)*3 + col]},
        Tooltip[Button["", Print[val], Appearance -> None, ImageSize -> {70, 70}], val]];
    ```

    这些控件创造了一个逼真的, 功能齐全的数字键盘(photorealistic):

    ```mathematica
    Overlay[{Show[键盘图片.jpg], Grid[Table[
         calcButtonFactory[row, col], {row, 4, 1, -1}, {col, 1, 3}],
        Spacings -> {0, 0}, ItemSize -> All]}, All, 2]
    ```

### 可能的问题

前面的表达可能完全掩盖了(obscure)后面的表达式:

```mathematica
Overlay[{ExampleData[{"TestImage", "Clock"}], Plot[Cos[x], {x, 0, 6}, Background -> LightOrange]}, Alignment -> Center]
```

使用`透明度`或去除背景颜色可能会达到理想的外观:

```mathematica
Overlay[{ExampleData[{"TestImage", "Clock"}], Plot[Cos[x], {x, 0, 6}, Background -> Directive[{Opacity[0.1], LightOrange}]]}, Alignment -> Center]
```

### Neat 例子

+ 使用 `Overlay` 来制作一个渐变的过渡效果(fading transition effect):

    ```mathematica
    Manipulate[Overlay[{ExampleData[{"TestImage", "House"}], SetAlphaChannel[ExampleData[{"TestImage", "Clock"}], x]}], {x, 0, 1}]
    ```

+ 为三维物体创建一个装饰性框架, 同时不影响旋转:

    ```mathematica
    mask = Blur[Image[Graphics[Disk[{0, 0}, .7], PlotRange -> {-1, 1}, ImageSize -> {144, 144}]], 30]

    Overlay[{Graphics3D[{Cone[]}, ImageSize -> {144, 144}, Boxed -> False],
        SetAlphaChannel[
           Image[Graphics[{}, ImageSize -> {144, 144}, Background -> Brown]], mask]}, All, 1]
    ```

## Inset,插图

## Show

## Prolog,Epilog

## 图例,Legended

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

### LegendLayout

图例的大小, 记号, 图例之间的间隔

图例在一般情形下的排版, 可以通过传入一个排版函数, 例如:

```mathematica
table[pairs_] := TableForm[pairs,
TableHeadings -> {{"Group A", "Group B", "Group C"}, {"color", "mascot"}}, TableAlignments -> Center]

SwatchLegend[63, {"lion", "whale", "rocket"}, LegendLayout -> table]
```

图例之间的间隔, 可以通过排版函数自定义.
