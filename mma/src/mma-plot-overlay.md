# 图形叠加

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
Overlay[{expr1, expr2, ... }]; 显示为所有 `expr_i` 的叠加.
Overlay[{expr1, expr2, ...},{i,j,...}]; 显示为 `expr_i`, `expr_j`, ... 的叠加.
Overlay[{expr1, expr2, ...},{i,j,...}, s]; 允许在 `expr_s` 中进行选择和点击控制.
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

+ Alignment; 排列对象:

```mathematica
Overlay[{Graphics[{Disk[]}], Slider2D[]}, All, 2, Alignment -> Center]
```

+ Background; 设置背景:

```mathematica
Overlay[{Graphics3D[Sphere[]], Panel["a sphere"]}, Background -> LightOrange]
```

+ BaseStyle; 设置 `Overlay` 的 base style:

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

+ 是 `图形函数` 的选项, 它给出了 `图形基元` 的 `列表`, 在图形的主要部分之后渲染.
+ 在三维图形中, 可以通过 `Epilog` 选项指定二维图形基元. `图形基元` 是在 `0,1` 坐标系中渲染的.

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

在 2D中, `axes` 被画在图形的顶部:

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
