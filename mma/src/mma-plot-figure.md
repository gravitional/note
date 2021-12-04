# mma 画图

图在英文中对应很多单词:

figure;  图形, 数字; 人的体形.
graphic; 图表的; 形象的; 绘画似的
graph; 多指用横纵坐标之间关系的曲线表示两个量之间的图表.

picture; 指广义的"图画", 现多用来指相片, 画像.
image; 形象, 概念; 镜像, 影像, 图像, 酷似的人(物), 翻版. 外形, 外表, 模样.

plot; 绘制; 标出, 故事情节; 小块地皮; 专指精确的草图或草案.
chart; 图表; 航海地图
map; 标有国家大小, 城市, 铁路, 河流, 山脉, 海洋等的地图.

outline; 事物要点或轮廓, 强调简化了的整体.
blueprint; 主要指绘制蓝图或指定纲领或计划.
drawing; 只用线条或色彩绘成的图画.
diagram; 侧重指用图形, 图表来说明.
illustration;  指插入书页之间, 帮助说明的任何插图或图解.

cartoon; 指幽默或讽刺性漫画.

painting; 指着色的画.

sketch; 草图, 主要特征的画.
portrait; 肖像, 一般只用于人

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

+ `Overlay` 两个表达式:

    ```mathematica
    Overlay[{2, 4}]
    ```

+ `Overlay` 两个图形:

    ```mathematica
    Overlay[{Plot[Sin[x], {x, 0, 6}, PlotRange -> 2], Plot[Sin[x] + .1 Sin[10 x], {x, 0, 6}, PlotRange -> 2]}]
    ```

### 范围

+ `Overlay` 许多对象:

    ```mathematica
    Style[Overlay[Range[9]], 48]
    ```

+ `Overlay` 二维和三维的图形:

    ```mathematica
    Overlay[{Plot3D[Sin[x y], {x, 0, 3}, {y, 0, 3}], Plot[Sin[x], {x, 0, 6}]}]
    ```

+ `Overlay` 文本 和 graphics:

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

```mathematica
Inset[obj]; 表示物体  obj 在  graphic 中的嵌入
Inset[obj,pos]; 指定 inset 应放置在 graphic 中的 pos 位置.
Inset[obj,pos,opos]; 对齐 inset, 使 对象的  opos 位置,  位于外层图形的 pos 位置
Inset[obj,pos,opos,size]; 指定 inset 的尺寸, 按照外层图形的坐标系统.
Inset[obj,pos,opos,size,dirs]; 指定 inset 的 axes 应以 dirs 为方向.
```

### 细节和选项

+ `Inset` 可以在 `Graphics` 和 `Graphics3D` 中使用.
+ 在交互式操作中, `Inset[obj,...]` 默认被选择为单个元素. 需要双击来选择里面的每个元素.
+ 对象 `obj` 可以是 graphic, cell 表达式, string 或任何其他表达式.
+ 在 `Graphics3D` 中嵌入的对象在三维图形旋转时显示为广告牌(billboard).
+ 位置可以通过以下方式指定:
    + `{x,y}`; plot 中的普通坐标
    + `Scaled[{x,y},...]`; 从0到1的比例坐标
    + `ImageScaled[{x,y}, ...]`; 在整个 Image 中从 `0` 到 `1` 的比例坐标.
    + `Offset[{dx,dy},...]` ; 绝对偏移, 单位是打印机点
    + `Center`, 等等;     `{Center,Center}`, 等等.
    + `{val_x, val_y, val_z}`; 3D graphic 中的座标

+ `x` 坐标可以是数字, `Automatic`, 或 `Left`, `Center`, `Right`, 或`Axis`.
+ `y` 坐标可以是数字, `Automatic`, 或 `Top`, `Center`, `Bottom`, `Baseline`, 或 `Axis`.
+ `Axis` 对应于 `plot` 中 `axis` 的位置, 或排版文本中的中心线.
+ `Center` 对应的是相对于整个图像的中心位置.
+ `Inset[obj]` 等同于 `Inset[obj,Center]`.
+ `opos` 的默认值是 `obj` 的 `AlignmentPoint` 选项, 默认是 `obj` 的边界框的中心.
+ 如果大小是 `Automatic`, 那么 `inset` 将以其自然大小给出:
    + graphic; 由 `ImageSize` 设置决定
    + `Typeset 表达式`; 不带 wrapping 的绝对尺寸.

+ 若尺寸为 `{w,Automatic}`, 则指定一个 Typeset 表达式 应该被 `宽度w` 包裹.
+ 如果给出的尺寸是 `{w,h}`, 那么任何长宽比不固定的对象 `obj` 将被水平或垂直拉伸, 以便正好适合 `w*h` 的矩形.
+ 如果 `obj` 是一个具有固定长宽比的物体, 那么如果在某个方向必须被拉伸以完全适合指定, 两边将保留相同的空白.
+ 选项 `Background` 指定了整个 `inset` 区域所使用的背景.
+ 默认情况下, `inset` 的排列方式是使其 `x` 和 `y` 方向与外层图形的 `x` 和 `y` 方向一致.
+ `Inset[..., dirs]` 可以用来指定不同的方向:
+ `dirs` 的可能选择是:
    + `Automatic` ; 默认方向
    + `{xx,xy}`; x方向沿着 `{xx,xy}`.
    + `{Automatic,{yx,yy}}`; y方向沿`{yx,yy}`.
    + `{{xx,xy},{yx,yy}}`; x, y 分别沿给定的方向.
    + `None`; 不重新调整 `Inset` 内容的方向

+ 一般来说, 只指定 `x` 方向或只指定 `y` 方向会导致 `inset` 的刚性转动.
+ 一般来说, 指定 `x` 和 `y` 方向会导致旋转和剪切(shear), 在这种情况下, 原来的 `inset` 矩形会变成由所给矢量方向定义的平行四边形.
+ 矢量 `{xx,xy}` 和 `{yx,yy}` 只有方向是重要的; `inset` 的比例由 `{w,h}` 决定.
+ 如果 `dirs` 指定为 `None`, `obj` 的 `x` 方向总是保持为水平.
+ 当 `dirs` 指定为 ` {Automatic,None}`时, `y` 方向被保持为垂直.
+ 如果 `Inset[Graphics3D[...],{x,y}]` 出现在 `2D图形` 中, 那么 `Graphics3D` 的`alignment point`的投影, 默认情况下是其 `2D bounding box` 的中心, 将被放置在2D图形的`{x,y}`位置.
+ `Inset[Graphics3D[...],{x,y,z},{ox,oy}]` 指定对象的 `2D投影` 中 `{ox,oy}` 的位置应该位于 `{x,y,z}` 的位置.
+ `2D graphic` 的方向总是与 `3D graphic` 的观察面平行.
+ 可以为 `Inset` 提供以下选项:
    + `Alignment`;  `Left`; `如何对齐嵌入的内容`
    + `Background`; `None`; `inset` 的整个区域要使用的背景.
    + `BaseStyle`; `{}`; `inset`的基本样式
    + `ContentSelectable`; `Automatic`;  是否允许内容被选中
    + `FormatType`;  `Automatic`;  文本的格式类型

+ `Background->Automatic` 使用 `inset` 外层 graphic 的背景, 作为 `inset`的背景.
+ `Scaled`, `ImageScaled`, and `Offset` 形式可以用来指定尺寸.

### 基本例子

```mathematica
Graphics[{LightGray, Disk[], Inset[Plot[Tan[x], {x, -3, 3}]]}]
```

在 graphic 中插入表达式:

```mathematica
Graphics[{Circle[], Inset[x^2 + y^2 == 1, {0, 0}]}]
```

## Show

```mathematica
Show[graphics,options]; 以特定的 options 显示  graphics
Show[g1,g2, ...]; 展示几幅图像的合并
```

### 细节和选项

+ `Show` 可以与 `Graphics` 和 `Graphics3D` 一起使用.
+ `Show` 可以接受任何能够用于 `Graphics` 的选项.
+ 在 `Show` 中明确指定的选项, 将覆盖在 `Graphics` 表达式中的选项.
+ `Show[g1, g2, ...]` 或 `Show[{g1,g2, ...}]` 将 `g_i` 中的图形基元连接(concatenate)起来, 有效地叠放(overlaying)图形.
+ `g_i` 中的非默认选项的列表被连接起来.
+ `Show` 应用 `DisplayFunction` 设置中定义的函数, 并返回结果. 对于普通的笔记本操作, 这个函数也即 `Identity`.

### 基本例子

将曲线的 plot 和 列表 的 plot 结合起来:

```mathematica
Show[Plot[x^2, {x, 0, 3.5}], ListPlot[{1, 4, 9}]]
```

将 密度图 和 等高线 plot 结合起来:

```mathematica
Show[DensityPlot[Sin[x] Sin[y], {x, -3, 3}, {y, -3, 3}], ContourPlot[Sin[x] Sin[y], {x, -3, 3}, {y, -3, 3},  ContourShading -> None]]
```

### 选项

在 `PopupWindow` 中显示 graphics, 当用户点击时:

```mathematica
Show[Graphics[{Pink, Rectangle[]}], DisplayFunction -> (PopupWindow[Button["Click here"], #] &)]
```

在新的笔记本中显示 graphics:

```mathematica
Show[Graphics3D[{Sphere[]}], DisplayFunction -> CreateDocument]
```

### 可能的问题

`Show` 使用 第一张 graphic 的选项:

```mathematica
Show[{PolarPlot[1 + 2 Sin[t/2], {t, 0, \[Pi]}, Ticks -> None, PlotStyle -> Red, PlotRange -> {Automatic, {0, 3}}],
    PolarPlot[1 + 2 Sin[t/2], {t, \[Pi], 2 \[Pi]}, Ticks -> None, PlotStyle -> Orange],
    PolarPlot[1 + 2 Sin[t/2], {t, -\[Pi], 0}, Ticks -> None, PlotStyle -> Blue],
    PolarPlot[1 + 2 Sin[t/2], {t, -2 \[Pi], -\[Pi]}, Ticks -> None, PlotStyle -> Green]}]
```

要显示整个 graphic,  使用 `PlotRange->All`:

```mathematica
Show[{PolarPlot[1 + 2 Sin[t/2], {t, 0, \[Pi]}, Ticks -> None, PlotStyle -> Red, PlotRange -> {Automatic, {0, 3}}],
  PolarPlot[1 + 2 Sin[t/2], {t, \[Pi], 2 \[Pi]}, Ticks -> None, PlotStyle -> Orange],
  PolarPlot[1 + 2 Sin[t/2], {t, -\[Pi], 0}, Ticks -> None, PlotStyle -> Blue],
  PolarPlot[1 + 2 Sin[t/2], {t, -2 \[Pi], -\[Pi]}, Ticks -> None, PlotStyle -> Green]}, PlotRange -> All]
```

## Prolog,Epilog

Prolog
is an option for graphics functions which gives a list of graphics primitives to be rendered before the main part of the graphics is rendered.

`Prolog` 是 graphics 函数类的一个选项, 它给出一个要渲染的图形基元(graphics primitives)的列表,
指定在渲染图形的主要部分之前绘制.

### 细节和选项

+ `Prolog` 指定的图形基元在 `axes`, `boxes`和 `frames` 之后渲染.
+ 在三维图形中, 二维图形基元可以由 `Prolog` 选项指定. 图形基元是在 `0, 1` 坐标系统中渲染的.
+ 由 `Prolog` 指定的指令(Directives)只影响 prolog, 而不影响图形的其他内容.

### 基本例子

在二次函数的 plot 后面画一个圆盘:

```mathematica
Plot[x^2, {x, -1.5, 1.5}, Prolog -> {Pink, Disk[{0, 1}, 1]}, PlotStyle -> Thick, AspectRatio -> Automatic]
```

### 范围

二维图形中的 `Prolog` 使用普通的坐标系:

```mathematica
Table[Graphics[{Pink, Opacity[.5], Disk[{0, 0}, 10]}, Axes -> True, Prolog -> Disk[p, 2]], {p, {{-7, -7}, {0, 0}, {7, 7}}}]
```

三维 graphics 的 `Prolog` 使用按比例的 `0,1` 坐标系:

```mathematica
Table[Graphics3D[{Opacity[.5], Sphere[{0, 0, 0}, 10]}, Prolog -> Disk[p, .1]], {p, {{.2, .2}, {.5, .5}, {.8, .8}}}]
```

### 性质和关系

在 `PlotRange` 的计算过程中, 不会包括 `Prolog` 中的对象:

```mathematica
Graphics[{Pink, Disk[]}, Prolog -> Circle[{0, 1}], Frame -> True]
```

## Epilog

### 细节

+ 是图形函数的一个选项, 它给出了一个图形基元的列表, 在图形的主要部分之后渲染.
+ 在三维图形中, 可以通过 `Epilog` 选项指定二维图形基元. 图形基元是在 `0,1` 坐标系中渲染的.

### 例子

在 plot 上画出样本点:

```mathematica
data = Table[{i, RandomReal[i]}, {i, 10}];
ListLinePlot[data, Epilog -> {PointSize[Medium], Point[data]}, InterpolationOrder -> 2]
```

在 3D graphic 的右下角放置文字:

```mathematica
SphericalPlot3D[\[Phi], {\[Theta], 0, Pi}, {\[Phi], 0, 3 Pi}, Epilog -> Inset[Framed[Style["Spiral", 20],
    Background -> LightYellow], {Right, Bottom}, {Right, Bottom}]]
```

### 性质和关系

在 2D中, axes 被画在图形的顶部:

```mathematica
Graphics[{Pink, Disk[]}, Axes -> True]
```

`Epilog` 中的对象被画在任何 `graphics` 的上面, 包括 `axes`:

```mathematica
Graphics[{Pink, Disk[]}, Axes -> True, Epilog -> {Blue, Disk[{0, 0}, .4]}]
```

在 plot 函数中, 默认情况下, `PlotRangeClipping` 也修剪(clips) graphics 的Epilog 部分:

```mathematica
Plot[2 Sin[x], {x, 0, 10}, Epilog -> Circle[], AspectRatio -> Automatic]
```

通过设置 `PlotRangeClipping` 为 `False`, 可以在 plot range 外面绘制 graphic:

```mathematica
Plot[2 Sin[x], {x, 0, 10}, Epilog -> Circle[], PlotRangeClipping -> False, AspectRatio -> Automatic]
```

### 整洁的例子

由 `RegionPlot` 生成的图像遮罩(Image mask):

```mathematica
g = RegionPlot[.45^10 < (x - .5)^10 + (y - .5)^10, {x, 0, 1}, {y, 0, 1}, ColorFunction -> "TemperatureMap"]
```

在 3D graphic 上应用遮罩, 就像一个 "画框"(picture frame):

```mathematica
Graphics3D[Cylinder[], Boxed -> False, SphericalRegion -> True, Epilog -> First[g]]
```

## 图例,Legended

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

`LegendLayout` 是 legends 的一个选项, 指定如何格式化 legend 内容.

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

图例在一般情形下的排版, 可以通过传入一个排版函数, 例如:

```mathematica
table[pairs_] := TableForm[pairs,
TableHeadings -> {{"Group A", "Group B", "Group C"}, {"color", "mascot"}}, TableAlignments -> Center]

SwatchLegend[63, {"lion", "whale", "rocket"}, LegendLayout -> table]
```

所以要自定义图例之间的间隔, 可以指定排版函数`f`.
