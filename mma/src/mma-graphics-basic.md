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
