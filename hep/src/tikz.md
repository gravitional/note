# texdoc tikz

用来替换`pdf` 粘贴过程额外`h..i`的正则表达式

```bash
\bh([\. ]+?)i\b # vscode 里面的正则, 元字符 . 需要转义成 \.
<$1>
```

## introduction

欢迎来到`TikZ`和底层`pgf`系统的文档.
最初是小小的`LaTeX` style, 直接用`pdfLaTeX`创建我(Till Tantau)的博士论文中的图形, 
现在已经发展成一个完整的图形语言, 它的手册有一千多页.
`TikZ`提供的大量选项往往让初学者望而生畏;
但幸运的是, 这个文档附带了一些节奏缓慢的教程, 
这些教程几乎可以教会你所有关于`TikZ`的知识, 而不需要你去阅读其他的内容.

我希望从 "什么是TikZ?" 这个问题开始. 基本上, 它只是定义了一些`TEX`命令来绘制图形.
例如, 代码`\tikz \draw (0pt,0pt) --(20pt,6pt);` 产生线条. 代码`\tikz \fill[orange] (1ex,1ex) circle (1ex);`产生小橙子.
在某种意义上, 当你使用`TikZ`的时候, 你对你的图形进行了 "编程", 就像你在使用TEX时对你的文档进行了 "编程 "一样.
这也解释了`TikZ`的名字: `TikZ`是 `gnu's Not Unix` 传统的一个递归缩写, 即 `TikZ ist kein Zeichenprogramm`,
意思是 "TikZ不是一个绘图程序", 提醒读者不要抱有错误的期待.

有了`TikZ`, 你可以为你的图形获得 "TEX-approach to typesetting" 的所有优点.
快速创建简单的图形, 精确的定位, 使用宏, 通常是卓越的排版. 同时你也

你也继承了所有的缺点: 学习曲线陡峭, 没有 WYSIWYG, 小的改变需要长时间的重新编译. 而且代码并不真正 "显示 "事物的样子.

现在我们知道了`TikZ`是什么, 那么 `pgf` 呢?
如前所述, `TikZ`最初是作为一个实现TEX图形宏的项目, 它既可以用于`pdfLaTeX`, 也可以用于经典的(基于`PostScript`)的`LaTeX`.
换句话说, 我想为`TEX`实现一种 "portable graphics format"--因此被称为`pgf`.
这些早期的宏仍然存在, 它们构成了本手册中描述的系统的 `基本层`.

但现在文档作者的大部分互动是基于`TikZ`的, `TikZ`自己已经成为一门独立的语言.

### TikZ 的底层

`page 27`.

事实证明, 在`TikZ`下面有两个`layers`.

+ `System layer`: 这一层提供了对 `驱动` 中所发生的事情的完整抽象.

驱动程序是一个像`dvips`或`dvipdfm`这样的程序, 它接受一个 `.dvi` 文件作为输入, 并生成一个 `.ps` 或 `.pdf` 文件.
(`pdftex`程序也算作一个驱动程序, 尽管它不接受`.dvi`文件作为输入, 无所谓).

每个驱动程序都有自己的图形生成语法, 这让想以可移植方式创建图形的人感到头疼.

`pgf`的`系统层` 抽象化了这些差异.
例如, 系统命令`pgfsys@lineto{10pt}{10pt}`将当前路径扩展到当前`{pgfpicture}`的坐标点`(10pt, 10pt)`.

根据`dvips`, `dvipdfm`或`pdftex`这些处理文件的具体程序, 系统命令将被转换为不同的`\special`命令.
系统层尽可能地 "简约", 因为每一个额外的命令, 都会使将`pgf`在移植到新的驱动程序上时需要更多的工作.
作为一个用户, 你不会直接使用系统层.

+ `Basic layer` : 基本层提供了一组基本命令, 使你能够以比直接使用系统层更简单的方式产生复杂的图形.
比直接使用系统层要容易得多. 例如, 系统层没有提供创建`圆`的命令, 因为`圆`可以由更基本的`贝塞尔曲线`组成(嗯...差不多).
然而, 作为一个用户, 你会希望有一个简单的命令来创建`圆`(至少我是这样想). 而不是要写下半页的贝塞尔曲线的支撑坐标.
因此, 基本层提供了一个命令`\pgfpathcircle`, 为你生成必要的曲线坐标.

`基本层`包括一个`核心`, 核心由几个相互依赖的软件包组成, 这些软件包只能被整体加载(en bloc),
还有一些额外的模块, 模块扩展核心, 以提供更多特殊用途的命令, 如`node`管理或`plotting`接口.

例如, `beamer`包只使用`核心`, 而不使用, 例如`shapes`模块.

理论上, `TikZ`本身只是几个可能的 `frontends` 之一. 它提供了命令组和一些特殊语法, 使基本层的使用更容易.
直接使用基本层的一个问题是, 用该层编写的代码往往过于 "冗长". 例如, 为了画一个简单的三角形, 你可能需要多达五个命令.
一条是在三角形的第一个角上开始一个路径. 一条用于将路径延伸到第二个角, 一条用于前往第三个角, 一条用于关闭路径.
以及一条用于实际绘制三角形的(而不是填充它).
在TikZ的前端, 所有这些都可以归结为简单的类似 METAFONT 的命令.

```latex
\draw (0,0) -- (1,0) -- (1,1) -- cycle;
```

实际上, `TikZ`是`pgf`的唯一 `正式` 的前端.
它让你可以使用`pgf`的所有功能, 但的所有功能, 但它的目的是要使其易于使用. 语法是`METAFONT` 和 `PSTRICKS`的混合体, 也有一些我自己的想法.

除了`TikZ`之外, 还有其他的`前端`, 但它们更多的是作为 "技术研究", 而不是作为`TikZ`的重要替代品.
特别是`pgfpict2e`, 它使用`pgf`基本层, 重新实现了标准的`LaTeX {picture}`环境和命令, 例如`\line`或`\vector`.
这个层并非真正"必要", 因为`pict2e.sty`包在重新实现`{picture}`环境方面至少做得很好.
相反, 这个包背后的想法是为了简单地演示如何实现一个前端.

由于大多数用户只使用`TikZ`, 几乎没有人会直接使用`系统层`, 所以本手册在第一部分主要是关于`TikZ`的.
`基础层`和`系统层`将在最后解释.

## tikz 设计原则

`page 124`; `TikZ` 遵循以下基本设计原则:

1. 用于指定`points`的特殊语法.
2. 指定`path`的特殊语法.
3. Actions on paths.
4. 图形参数的`Key–value`语法.
5. `nodes`的特殊语法.
6. `trees`的特殊语法.
7. `graphs`的特殊语法.
8. 对图形的参数分组.
9. 坐标转换系统.

## scope 作用范围

`page131`; Using Scopes to Structure a Picture.
如果`scope`不生效的话, 可以尝试在导言区添加:

```latex
\usetikzlibrary {scopes}
```

命令 `\path` 用于创建一个路径 (`path`),
此命令可以带有图形选项 (graphic options), 这些选项只对本路径有效. 
使用简写形式的 scope 可以在路径内部插入一个 `scope`:

```latex
\tikz \draw (0,0) -- (1,1)
{[rounded corners, red] -- (2,0) -- (3,1)}
-- (3,0) -- (2,1);
```

上面例子中,选项 `rounded corners` 的作用范围受到花括号的限制,
并且颜色选项 `red` 没有起到作用, 这是因为 `\draw` 的默认颜色是 `draw=black`, 
颜色 `black` 把 `red` 覆盖了.
还要注意开启 scope 的符号组合`{[...]`要放在坐标点之后, `--`之前.

除了`\tikzpicture`环境, 可以使用简洁的`\tikz{path1;path2}`命令, 例如:

```latex
\tikz[baseline]{
  \draw (0,0)--(2,0);\draw (0.5,0) to [out=90,in=90,looseness=1.5] (1.5,0);
\draw (0.5,0) to [out=-90,in=-90,looseness=1.5] (1.5,0);
}
```

## 坐标计算

page 148

### 指定坐标点

`page 136`; Specifying Coordinates
`page 148`; TikZ Library calc, 可以计算坐标值.

坐标总是放在圆括号内, 
一般的语法是 `([<options>]<coordinate specification>)`. 有两种指定坐标的方法:

明确指定坐标系统和参数, 使用`xxx cs:`这种语法

```latex
\draw (canvas cs:x=0cm,y=2mm)
-- (canvas polar cs:radius=2cm,angle=30);
```

或者可以隐式地指定, `tikz` 会根据格式自动判断坐标系统. 
例如 `(0,0)` 对应笛卡尔坐标,`(30:2)` 对应极坐标 (其中 `30` 代表角度).

+ 基本使用:`(1cm,2pt)`
+ 极坐标:`(30:1cm)`
+ `PGF-xy` 坐标系统, 单位按照`cm` :  `(2,1)`
+ `PDF-xyz` 坐标系统:`(1,1,1)`
+ 也可以使用利用之前定义的形状作为锚点,如:`(first node.south)`
+ 连续相对坐标:`++(1cm,0pt)`,`(1,0), ++(1,0), ++(0,1)`给出`(1,0), (2,0),(2,1)`
+ 同源相对坐标:`+(1,0) +(1,0) +(0,1)` 给出 `(1,0), (2,0), (1,1)`.

对图像进行全局伸缩, 可以指定`xyz`单位矢量的长度, 也可以通过画布变换

`page 137`: Coordinate system xyz
`page 43`: Transformations

### 指定路径

路径是一些直线和曲线的组合.
部分使用`metapost`的语法, 例如一条三角形路径

```latex
(5pt,0pt) -- (0pt,0pt) -- (0pt,5pt) -- cycle
```

p168 The Let Operation

### 对路径的action

路径只是一系列直线和曲线的组合,但你尚未指定如何处理它.
可以绘制一条路径, 填充一条路径, 为其着色,
对其进行裁剪或进行这些操作的任意组合.

`draw`,`fill`,`shade`,`clip`

例如

```latex
\path[draw] (0,0) rectangle (2ex,1ex)
```

`\path[draw]`: `\draw`
`\path[fill]` :`\filldraw`
`\path[shade]`:`\shade` and `\shadedraw`
`\path[clip]`:
`\draw[clip]` or `\path[draw,clip]`: `\clip`

所有这些命令只能在`{tikzpicture}`环境中使用.  
`TikZ` 允许你用不同的颜色进行 填充 和 描边.

## 绘制曲线

### To 自由曲线

`page164`; The To Path Operation
`page838`; To Path Library
`page 841`; 75.4 Loops

+ 可以使用 `To`来绘制直线, 也可以用来绘制曲线.

```latex
\path ... to[<options>] <nodes> <coordinate or cycle> ...;
```

例如`(a) to [out=135,in=45] (b)`

各种选项

```latex
/tikz/out=<angle>
/tikz/in=<angle>
...
```

在给定出射和入射角度之后, 
`/tikz/looseness=<number>`选项中的`<number>`
调控`control points`与初始点以及与终点的距离.

还可以用 `/tikz/min distance=<distance>`, 
`/tikz/out min distance=<distance>` 控制最小距离, 避免计算无解.

+ 使用 `loop`选项来绘制圈图曲线, 例如

```latex
\begin{tikzpicture}
  \begin{feynman}
  \draw  (0,0) edge [anti charged scalar,loop, looseness=30] (h3);
  \end{feynman}
\end{tikzpicture}
```

+ `loop`选项只接受一个参数, 即初始点, 
终点位置和初始点相同, 然后把`looseness`设置为`8`, `min distance`设置为`5mm`.

+ 如果想精确控制圈图的形状, 可以手动添加控制点, 例如:

    ```latex
    \draw (a3) to [controls=+(45:1.5) and +(135:1.5)] (a3);
    ```

    上面使用`+(角度:距离)`的方式指定控制点的坐标, 
    `and`左右的坐标采用相对坐标的形式, 分别相对于路径的`起点`和`终点`.

+ 参考 page 33, 可以使用`to`的简称, 即`..`语法以 `曲线` 方式延伸路径:

    ```latex
    .. controls <控制点1> and <控制点2> .. <终点>
    ```

+ 你可以省略`and <控制点2>`, 这将导致使用`控制点1`两次.
+ 如果要使用`tikz-feynman`定义的线型, 使用下面的`edge`, 并使用`tikz-feynan`定义的全称, 例如:

```latex
\draw (a3) edge [controls=+(30:3) and +(150:3), /tikzfeynman/fermion] (a4);
```

### edge

17.12 Connecting Nodes: Using the Edge Operation

`edge(边)` 操作的作用类似于主路径绘制完成后添加的 `to` 操作, 
就像 `node` 是在主路径绘制完成后添加的.
这样, 每条 `边` 就可以有不同的外观.

和 `node` 操作一样, `edge`的操作会暂时中止当前路径的构建, 并构建一个新的路径`p`.
这个新的路径`p`将在主路径绘制完毕后被绘制. 请注意, `p`可以与主路径的选项完全不同.
还要注意的是, 如果主路径中有几个`edge`或`node`操作, 
每个操作都会创建自己的路径, 并且按照它们在主路径上出现的顺序来绘制.

```latex
\path ...  edge[<options>] <nodes> (<coordinate>) ... ;
```

`edge`操作的效果是, 在主路径之后, 下面的路径被添加到图片中:

```latex
\path[every edge,<options>] (\tikztostart) <path>;
```

这里, `<path>`是`to path`. 
注意, 与`to`操作所添加的路径不同, `(\tikztostart)`被添加到`<path>`之前,
这对`to`操作来说是不必要的, 因为这个坐标已经是主路径的一部分.

`\tikztostart`是`edge`操作之前的路径上的最后一个坐标, 
就像对`node`或`to`操作一样.
然而, 这条规则有一个例外: 如果`edge`操作的前面是`node`操作, 
那么这个刚刚声明的`node`就是起始坐标.
而不是像通常情况下那样, 是这个节点所处的坐标--一个微妙的区别. 
在这方面, `edge`与`node`和`to`都不同.

如果一行有多个`edge`操作, 那么所有这些操作的起始坐标都是一样的, 
但是目标坐标不同, 它们是主路径的一部分.
因此, 起始坐标就是第一个`edge`操作之前的坐标. 
这一点与`node`类似, `edge`操作也不会修改当前路径.
特别是, 它不改变最后访问的坐标, 见下面的例子:

```latex
\begin{tikzpicture}
\node (a) at (0:1) {$a$};
\node (b) at (90:1) {$b$}     edge    [->]              (a);
\node (c) at (180:1) {$c$}    edge    [->]             (a)
                                                      edge    [<-]             (b);
\node (d) at (270:1) {$d$}  edge    [->]              (a)
                                                     edge    [dotted]     (b)
                                                     edge    [<-]               (c);
\end{tikzpicture}
```

### arc 弧线

159 The Arc Operation

`\path ... arc[<options>] ...;`

从当前点开始画弧线, 可以用`x radius` and `y radius`指定半径, 
用`start angle`, `end angle`, and `delta angle`指定角度.

也有一个较简捷的句法来指定圆弧:
`arc(<start angle>:<end angle>:<radius>)`
或者
`arc(<start angle>:<end angle>:<x radius> and <y radius>)`
