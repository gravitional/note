# texdoc tikz

用来替换`pdf` 粘贴过程额外`h..i`的正则表达式

```bash
\bh([\. ]+?)i\b # vscode 里面的正则, 元字符 . 需要转义成 \.
<$1>
```

## introduction

欢迎来到`TikZ`和底层`pgf`系统的文档. 
最初是小小的`LaTeX` style, 直接用`pdfLaTeX`创建我(Till Tantau)的博士论文中的图形, 现在已经发展成一个完整的图形语言, 它的手册有一千多页. 
`TikZ`提供的大量选项往往让初学者望而生畏；但幸运的是, 这些文件中包含了一些节奏缓慢的
但幸运的是, 这个文档附带了一些节奏缓慢的教程, 这些教程几乎可以教会你所有关于`TikZ`的知识, 而不需要你去阅读其他的内容. 

我希望从 "什么是TikZ？"这个问题开始. 基本上, 它只是定义了一些`TEX`命令来绘制图形. 
例如, 代码`\tikz \draw (0pt,0pt) --(20pt,6pt);` 产生线条. 代码`\tikz \fill[orange] (1ex,1ex) circle (1ex);`产生小橙子.
在某种意义上, 当你使用`TikZ`的时候, 你对你的图形进行了 "编程", 就像你在使用TEX时对你的文档进行了 "编程 "一样. 
这也解释了`TikZ`的名字：`TikZ`是 `gnu's Not Unix` 传统的一个递归缩写, 即 `TikZ ist kein Zeichenprogramm`, 
意思是 "TikZ不是一个绘图程序", 提醒读者不要抱有错误的期待. 

有了`TikZ`, 你可以为你的图形获得 "TEX-approach to typesetting" 的所有优点. 
快速创建简单的图形, 精确的定位, 使用宏, 通常是卓越的排版. 同时你也

你也继承了所有的缺点：学习曲线陡峭, 没有 WYSIWYG, 小的改变需要长时间的重新编译. 而且代码并不真正 "显示 "事物的样子. 

现在我们知道了`TikZ`是什么, 那么 `pgf` 呢？
如前所述, `TikZ`最初是作为一个实现TEX图形宏的项目, 它既可以用于`pdfLaTeX`, 也可以用于经典的(基于`PostScript`)的`LaTeX`. 
换句话说, 我想为`TEX`实现一种 "portable graphics format"--因此被称为`pgf`. 
这些早期的宏仍然存在, 它们构成了本手册中描述的系统的 `基本层`. 

但现在文档作者的大部分互动是基于`TikZ`的, `TikZ`自己已经成为一门独立的语言. 

### TikZ 的底层

page 27.

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

## Tutorial Karl

### 路径命令

p32, `tikz`中, `path`就是一连串的坐标, 在路径的开头可以选择:

+ `\path` 什么也不做
+ `\draw` 画出路径
+ `\fill` 填充路径
+ `\filldraw` 等等

对路径的操作. 用分号表示一条路径的结束. 每一条路径会有初始点, 当前点等等特殊坐标. 
如`current subpath start`

路径构建命令和绘画命令相分离, 与路径构建的选项, 都在路径命令中指定；
与实际描绘相关的选项, 都在`\draw`,`\fill`等命令的选项中指定. 

曲线 33
`\draw (0,0) .. controls (1,1) and (2,1) .. (2,0);`

圆形
`(1,1) circle [radius=2pt]`
`ellipse [x radius=20pt, y radius=10pt]`

方形
`\draw (0,0) rectangle (0.5,0.5);`
`\draw[step=.5cm] (-1.4,-1.4) grid (1.4,1.4);`

自定义格式, \tikzset,p35

```latex
help lines/.style={color=blue!50,very thin} %在环境内部任意地方定义格式, 后面可以调用
\tikzset{help lines/.style=very thin} %在文档开头, 定义全局格式
\tikzset{Karl's grid/.style={help lines,color=blue!50}} %格式可以嵌套
\draw[step=.5cm,gray,very thin] (-1.4,-1.4) grid (1.4,1.4); % 使用 grid 绘制 参考格子
```

### 颜色线型等

绘制选项, p36

```latex
%颜色
color=<color>
draw=<color>
%%线型
ultra thin
very thin
thin
semithick
thick
very thick
ultra thick
%% 虚线
dashed
loosely dashed
densely dashed
dotted
loosely dotted
densely dotted
dash pattern
```

### 放大部分区域

剪切出图形的某一部分,clip

```latex
\draw[clip] (0.5,0.5) circle (.6cm);
\draw[step=.5cm,gray,very thin] (-1.4,-1.4) grid (1.4,1.4);
...
```

### 画抛物线

抛物线,parabola, p38

```latex
\tikz \draw (0,0) rectangle (1,1)(0,0) parabola (1,1);
\tikz \draw[x=1pt,y=1pt] (0,0) parabola bend (4,16) (6,12);
```

填充封闭区域, 使用`cycle`进行封闭 39

```latex
\begin{tikzpicture}[scale=3]
\clip (-0.1,-0.2) rectangle (1.1,0.75);
\draw[step=.5cm,gray,very thin] (-1.4,-1.4) grid (1.4,1.4);
\draw (-1.5,0) -- (1.5,0);
\draw (0,-1.5) -- (0,1.5);
\draw (0,0) circle [radius=1cm];
\filldraw[fill=green!20!white, draw=green!50!black] (0,0) -- (3mm,0mm)
arc [start angle=0, end angle=30, radius=3mm] -- cycle;
\end{tikzpicture}
```

### 渐变色

渐变色,shade,p39 

```latex
\shadedraw[left color=gray,right color=green, draw=green!50!black](0,0) -- (3mm,0mm)
```

### 相对坐标

定义命令, 相对坐标指定, p40

```latex
(30:1cm |- 0,0) % 垂直线与水平线的交点, |- 左边的坐标对应铅垂线, 右边的对应水平线. 
\def\rectanglepath{-- ++(1cm,0cm) -- ++(0cm,1cm) -- ++(-1cm,0cm) -- cycle} %% 连续的相对指定, 后一个坐标相对于前一个, 使用 \def 在任意地方定义一个命令替换. 
\def\rectanglepath{-- +(1cm,0cm) -- +(1cm,1cm) -- +(0cm,1cm) -- cycle} %% 基于相同root的相对坐标, 后面几个坐标相对于同一个最初坐标
```

### 路径交点

路径的交点, 使用`name path`在后面引用.

可以使用`(1,{tan(30)})`这种坐标形式, `tikz`的数学引擎可以处理`tan(30)`, 但是外面需要包围一层`{}`, 否则会与坐标的语法冲突. 一般情况中, 遇到含有`()`的坐标, 也需要用`{}`包裹起来. 

```latex
% 在导言区 \tikz 后面添加 \usetikzlibrary{intersections}
\path [name path=upward line] (1,0) -- (1,1); % 给第一条路径命名, \path 只计算路径, 不进行实际描绘.
\path [name path=sloped line] (0,0) -- (30:1.5cm);  % 第二条路径, 画得稍微长一点, 保证有交点
\draw [name intersections={of=upward line and sloped line, by=x}] [very thick,orange] (1,0) -- (x);
```

***
添加箭头, 通过`->`选项, 可以指定在路径末端加上箭头. 

```latex
\usetikzlibrary {arrows.meta}
\begin{tikzpicture}[>=Stealth]
\draw [->] (0,0) arc [start angle=180, end angle=30, radius=10pt];
\draw [<<-,very thick] (1,0) -- (1.5cm,10pt) -- (2cm,0pt) -- (2.5cm,10pt);
\end{tikzpicture}
```

### 作用域 scope

参考p42, scope.   类似于其他程序中的局部变量. 在导言区添加`\usetikzlibrary {scopes}`.

例如在`tikzpicture`环境中使用`tikz-feynman`包的`feynman`环境定义的`fermion` edge style.

```latex
\begin{tikzpicture}
{[every edge/.style={controls=+(27:3) and +(153:3), /tikzfeynman/fermion}]
\path (0,0) edge (0,1);
}  % 末尾无需分号
\end{tikzpicture}
```

`every edge/.style`是`tikzpicture`环境中的`handle`.

***
可以给选项设置作用范围, 如果想让整个环境都生效, 可以把选项传递给`\tikz`命令或者`{tikzpicture}`环境. 
如果希望定义一个局部环境, 可以使用`{scope}`环境. 例如:

```latex
\begin{tikzpicture}[ultra thick]
\draw (0,0) -- (0,1);
\begin{scope}[thin] % 在这里给出选项
\draw (1,0) -- (1,1);
\draw (2,0) -- (2,1);
\end{scope}
\draw (3,0) -- (3,1);
\end{tikzpicture}
```

`\clip`的生效范围也受`\scope`的控制, 只在`scope`范围内有效. 
类似于`\draw`的选项, 其实不是`\draw`的选项, 而是提供给`path`的选项, 可以在路径命令序列的任何地方提供. 大部分`graphic`选项是对整个路径生效的, 所以选项的位置不重要. 
例如, 下面三种用法是等价的. 如果同时给出`thick` and `thin`, 后一个覆盖前一个的效果. 

```latex
\begin{tikzpicture}
\draw[thin] (0,0) --(1,0);
\draw (0,1) [thin] --(1,1);
\draw (0,2) --(1,2) [thin];
\end{tikzpicture}
```

大部分图形选项应用于整个路径, 而所有的变形选项,`transformation` 只作用于跟在其后的路径片段. 

### 路径变形

page 43. 路径变形选项. 
图像的最后位置是由`TikZ`, `TeX`, `PDF`共同决定的. `tikz` 提供了一些选项可以在自己的坐标系统内变换图像的位置. 并且运行在路径中途修改变换方式. 例如:

```latex
\begin{tikzpicture}[even odd rule,rounded corners=2pt,x=10pt,y=10pt]
\filldraw[fill=yellow!80!black] (0,0)  rectangle (1,1) [xshift=5pt,yshift=5pt]  (0,0)  rectangle (1,1) [rotate=30]   (-1,-1) rectangle (2,2);
\end{tikzpicture}
```

这类选项有：

`x=<value>`, `y=<value>`, `z=<value>`. 例如：

```latex
\draw[x=2cm,color=red] (0,0.1) -- +(1,0);
\draw[x={(2cm,0.5cm)},color=red] (0,0) -- (1,0); %含有逗号的坐标需要放在括号里 escape
```

+ `xshift=<dimension>`,`yshift=<dimension>`:用来平移
+ `shift={<coordinate>}`: 平移到指定的点, 如`shift={(1,0)}, shift={+(1,0)},`
必须加上`{}`, 以避免`TeX`把坐标解析成两个选项. 例如:

```latex
\draw[shift={(1,1)},blue] (0,0) -- (1,1) -- (1,0);
\draw[shift={(30:1cm)},red] (0,0) -- (1,1) -- (1,0);
```

+ `rotate=<degree>`: 旋转特定角度. 以及`rotate around`:绕指定的点旋转. 
+ `scale=<factor>`: 放大或者缩小指定的倍数. 以及`xscale`, `yscale`. `xscale=-1`表示翻转. 
+ `xslant=<factor>`, `yslant=<factor>`：倾斜
+ `cm`: 指定任意的变换矩阵. 

详细可以参考 page 373: 25 Transformations

### 循环

`latex`本身有循环的命令, `pstricks`具有`\multido`命令. `tikz`也引入了自己的循环命令`\foreach`, 它定义在`\pgffor`中, `\tikz`会自动`\include`这个命令. 
语法是`\foreach \x in {1,2,3} {$x =\x $,}`. 循环区域用列表指定,  要循环的指令也放在一个`{}`中. 如果不用`{}`包裹, 就把下一个`;`之前的命令当作循环指令. 例如下面的语句绘制一个坐标系：

```latex
\begin{tikzpicture}[scale=3]
\draw[step=.5cm,gray,very thin] (-1.4,-1.4) grid (1.4,1.4);
\draw[->] (-1.5,0) -- (1.5,0);
\draw[->] (0,-1.5) -- (0,1.5);
\foreach \x in {-1cm,-0.5cm,1cm}
\draw (\x,-1pt) -- (\x,1pt);
\foreach \y in {-1cm,-0.5cm,0.5cm,1cm}
\draw (-1pt,\y) -- (1pt,\y);
\end{tikzpicture}
```

也可以结合平移使用:

```latex
\foreach \x in {-1,-0.5,1} \draw[xshift=\x cm] (0pt,-1pt) -- (0pt,1pt);
```

`\foreach`也可以使用`c`式的范围指定：`{a,...,b}`, 必须使用无量纲的实数.  `{1,3,...,11}`则可以指定步长. 两种语法可以混合, 例如:

```latex
\tikz \foreach \x in {1,3,...,11} \draw (\x,0) circle (0.4cm);
```

循环可以嵌套：

```latex
\begin{tikzpicture}
\foreach \x in {1,2,...,5,7,8,...,12}
\foreach \y in {1,...,5}
{
  \draw (\x,\y) +(-.5,-.5) rectangle ++(.5,.5);
  \draw (\x,\y) node{\x,\y};
}
\end{tikzpicture}
```

为了方便, 还有一种`key/value`型的循环语法：`foreach \key/\value in {1/a,2/b,3/c}`, `key/value`用`/`分隔开. 在每一次循环中, `\key`和`\value`的值将一一对应. 
如果循环域中, 某一项只给出了`key`, 会默认`value`等于`key`.

### 添加文字

添加文字可以使用`\node`命令, 也可以用来添加任意形状. 通常的用法是`\node[选项]{文字}`. `\node`会放在当前位置, 也就是`\node`命令前面的那个坐标上. 
当所有路径`draw/fill/shade/clipped/whatever`完成之后, 才绘制`\node`, 所以`\node`的图层在最上面. 

+ 可以使用类似于`anchor=north`指定`\node`的哪一个锚点放在前面给定的坐标上. 也可以使用`below=1pt`直接指定`\node`的相对偏移. 
+ 如果担心图形上的其他元素干扰了`node`的辨识度, 可以给`node`加上`[fill=white]`选项, 绘制一个白色的背景. 
+ 如果把`\node`放在`--`后面, 默认会将`\node`的位置放在这条线段的中点. 可以使用`pos=`选项控制具体的位置, 也可以使用`near start`, `near end`等等指定大概位置. 
+ 还可以使用`[above, sloped]`选项, 使曲线上方的`\node`贴合曲线斜率. 例如：

```latex
\begin{tikzpicture}
\draw (0,0) .. controls (6,1) and (9,1) .. 
node[near start,sloped,above] {near start} node {midway} 
node[very near end,sloped,below] {very near end} (12,0);
\end{tikzpicture}
```

如果`\node`中的文本量比较大, 需要控制换行, 可以使用类似`text width=6cm`的选项控制`\node`宽度. 完整的例子为:

```latex
\begin{tikzpicture}
  [scale=3,line cap=round,
    % 定义一些对象的格式
    axes/.style=,
    important line/.style={very thick},
    information text/.style={rounded corners,fill=red!10,inner sep=1ex}]
  % 定义一些对象的颜色
  \colorlet{anglecolor}{green!50!black}
  \colorlet{sincolor}{red}
  \colorlet{tancolor}{orange!80!black}
  \colorlet{coscolor}{blue}
  % 开始画图
  \draw[help lines,step=0.5cm] (-1.4,-1.4) grid (1.4,1.4);
  \draw (0,0) circle [radius=1cm];
  \begin{scope}[axes]
    \draw[->] (-1.5,0) -- (1.5,0) node[right] {$x$} coordinate(x axis);
    \draw[->] (0,-1.5) -- (0,1.5) node[above] {$y$} coordinate(y axis);
    \foreach \x/\xtext in {-1, -.5/-\frac{1}{2}, 1}
    \draw[xshift=\x cm] (0pt,1pt) -- (0pt,-1pt) node[below,fill=white] {$\xtext$};
    \foreach \y/\ytext in {-1, -.5/-\frac{1}{2}, .5/\frac{1}{2}, 1}
    \draw[yshift=\y cm] (1pt,0pt) -- (-1pt,0pt) node[left,fill=white] {$\ytext$};
  \end{scope}
  \filldraw[fill=green!20,draw=anglecolor] (0,0) -- (3mm,0pt)
  arc [start angle=0, end angle=30, radius=3mm];
  \draw (15:2mm) node[anglecolor] {$\alpha$};
  \draw[important line,sincolor]
  (30:1cm) -- node[left=1pt,fill=white] {$\sin \alpha$} (30:1cm |- x axis);
  \draw[important line,coscolor]
  (30:1cm |- x axis) -- node[below=2pt,fill=white] {$\cos \alpha$} (0,0);
  \path [name path=upward line] (1,0) -- (1,1);
  \path [name path=sloped line] (0,0) -- (30:1.5cm);
  \draw [name intersections={of=upward line and sloped line, by=t}]
  [very thick,orange] (1,0) -- node [right=1pt,fill=white]
  {$\displaystyle \tan \alpha \color{black}=
      \frac{{\color{red}\sin \alpha}}{\color{blue}\cos \alpha}$} (t);
  \draw (0,0) -- (t);
  \draw[xshift=1.85cm]
  node[right,text width=6cm,information text]
  {
    The {\color{anglecolor} angle $\alpha$} is $30^\circ$ in the
    example ($\pi/6$ in radians). The {\color{sincolor}sine of
        $\alpha$}, which is the height of the red line, is
    \[
      {\color{sincolor} \sin \alpha} = 1/2.
    \]
    By the Theorem of Pythagoras ...
  };
\end{tikzpicture}
```

### pic: 图形复用

`pic`是`picture`的简称. 通过预先定义的图片名字, 可以在指定的地方复用图形. 例如:

```latex
\usetikzlibrary {angles,quotes}
\begin{tikzpicture}[scale=3]
\coordinate (A) at (1,0);
\coordinate (B) at (0,0);
\coordinate (C) at (30:1cm);
\draw (A) -- (B) -- (C)
pic [draw=green!50!black, fill=green!20, angle radius=9mm,"$\alpha$"] {angle = A--B--C};
\end{tikzpicture}
```

这里调用了`angles` and `quotes`库. 前者预定义了`angle`图形, 后者可以简化参数输入为`"标记"`, 而不需要输入`label text="标记"`. 
`{angle = A--B--C}`表示`angle`是`BA`和`BC`的夹角.  `\coordinate`用于声明一个坐标点, 可以在后文引用. 

## tikz 设计原则

page 124, `TikZ` 遵循以下基本设计原则：

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

131 Using Scopes to Structure a Picture. 如果`scope`不生效的话, 可以尝试在导言区添加

```latex
\usetikzlibrary {scopes}
```

命令 `\path` 用于创建一个路径 (`path`),此命令可以带有图形选项 (graphic options),这些选项只对本路径有效. 使用简写形式的 scope 可以在路径内部插入一个 `scope`:

```latex
\tikz \draw (0,0) -- (1,1)
{[rounded corners, red] -- (2,0) -- (3,1)}
-- (3,0) -- (2,1);
```

上面例子中,选项 `rounded corners` 的作用范围受到花括号的限制,并且颜色选项 red 没有起到作用,这是因为 `\draw` 的默认颜色是 `draw=black`,颜色 `black` 把 `red` 覆盖了. 
还要注意开启 scope 的符号组合`{[...]`要放在坐标点之后, `--`之前. 

除了`\tikzpicture`环境, 可以使用简洁的`\tikz{path1;path2}`命令, 例如：

```latex
\tikz[baseline]{
  \draw (0,0)--(2,0);\draw (0.5,0) to [out=90,in=90,looseness=1.5] (1.5,0);
\draw (0.5,0) to [out=-90,in=-90,looseness=1.5] (1.5,0);
}
```

## 坐标计算

page 148

### 指定坐标点

page 136, Specifying Coordinates
page 148,TikZ Library calc, 可以计算坐标值. 

坐标总是放在圆括号内, 一般的语法是 `([<options>]<coordinate specification>)`. 有两种指定坐标的方法：

明确指定坐标系统和参数, 使用`xxx cs:`这种语法

```latex
\draw (canvas cs:x=0cm,y=2mm)
-- (canvas polar cs:radius=2cm,angle=30);
```

或者可以隐式地指定, `tikz` 会根据格式自动判断坐标系统. 例如 `(0,0)` 对应笛卡尔坐标,`(30:2)` 对应极坐标 (其中 `30` 代表角度). 

+ 基本使用:`(1cm,2pt)`
+ 极坐标:`(30:1cm)`
+ `PGF-xy` 坐标系统, 单位按照`cm` :  `(2,1)`
+ `PDF-xyz` 坐标系统:`(1,1,1)`
+ 也可以使用利用之前定义的形状作为锚点,如:`(first node.south)`
+ 连续相对坐标:`++(1cm,0pt)`,`(1,0), ++(1,0), ++(0,1)`给出`(1,0), (2,0),(2,1)`
+ 同源相对坐标:`+(1,0) +(1,0) +(0,1)` 给出 `(1,0), (2,0), (1,1)`.

对图像进行全局伸缩, 可以指定`xyz`单位矢量的长度, 也可以通过画布变换

page 137: Coordinate system xyz
page 43: Transformations

### 指定路径

路径是一些直线和曲线的组合. 
部分使用`metapost`的语法,例如,一条三角形路径

```latex
(5pt,0pt) -- (0pt,0pt) -- (0pt,5pt) -- cycle
```

p168 The Let Operation

### 对路径的action

路径只是一系列直线和曲线的组合,但你尚未指定如何处理它. 
可以绘制一条路径,填充一条路径,为其着色,对其进行裁剪或进行这些操作的任意组合. 

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

所有这些命令只能在`{tikzpicture}`环境中使用.  `TikZ` 允许您使用不同的颜色进行填充和描边. 

## 自定义

### 自定义 style

```latex
\begin{tikzpicture}[abcde/.style={
  double distance=10pt,
  postaction={
  decorate,
  decoration={
      markings,
      mark=at position .5 with {\arrow[blue,line width=1mm]{>}},
    }
  }
  }]

  \begin{feynman}
  ...
  \end{feynman}

  \draw [help lines] grid (3,2);
  \draw[abcde]  (a1) -- (b1);

\end{tikzpicture}
```

### 指定线型

173 Graphic Parameters: Line Width, Line Cap, and Line Join

`/tikz/dash pattern=<dash pattern>`
`/tikz/dashed` ： 指定虚线模式的简写
Shorthand for setting a dashed dash pattern.

### 指定 node 节点的形状

p224, Nodes and Their Shapes

比如

```tikz
\begin{tikzpicture}
\draw (0,0) node[minimum size=2cm,draw] {square};
\draw (0,-2) node[minimum size=2cm,draw,circle] {circle};
\end{tikzpicture}
```

p785: 72 Shape Library, 形状库

p229: 17.2.3 Common Options: Separations, Margins, Padding and Border Rotation: 给出了 `node` 一些几何参数的选项

p730: 63 Pattern Library : `node` 可以使用 `pattern` 填充, 某种图形模式. 

### 添加任意装饰

p191 Arrows 箭头

p196 指定箭头大小, 形状

p212 Reference: Arrow Tips 与定义箭头形状参考

```tikz
\usetikzlibrary{arrows.meta}
```

p365 Decorated Paths 装饰路径

p646 Arbitrary Markings 添加任意装饰

Decoration markings

`marking`可以被认为是"小图片", 或更准确地说是放置的`some scope contents`, 放置在路径的某个位置"上". 
假设`marking`为简单的十字.  可以用以下代码产生：

```latex
\draw (-2pt,-2pt) -- (2pt,2pt);
\draw (2pt,-2pt) -- (-2pt,2pt);
```

如果我们将此代码用作路径上`2cm`处的`marking`, 则会发生以下情况：
`pgf`先确定沿路径`2cm`的位置.  然后将坐标系平移到此处并旋转它, 使`x`轴正向与路径相切.  然后创建一个保护用的`scope`, 在内部执行上述代码--最后路径上出现一个叉叉. 

`marking`允许在路径上放置一个或多个装饰. 除了后面讲的少数情况, `decoration`摧毁路径输入, 也就是说, 计算完成, 作完装饰之后, 路径就消失了. 一般需要`postaction`来添加装饰. 
`postaction` 表示完成路径绘制之后再进行操作. 

```latex
\begin{tikzpicture}[decoration={
        markings,% 启用 markings
        mark=% mark 的形状
        at position 2cm
        with
          {
            \draw (-2pt,-2pt) -- (2pt,2pt);
            \draw (2pt,-2pt) -- (-2pt,2pt);
          }
      }
  ]
  \draw [help lines] grid (3,2);
  \draw [postaction={decorate}] (0,0) -- (3,1) arc (0:180:1.5 and 1); % postaction 表示画完路径之后再装饰, 再摧毁路径. 
\end{tikzpicture}
```

### Pics:复用图形组件

p263, `pic`是`picture`的简称. 通过预先定义的图片名字, 可以在指定的地方复用图形. 例如先定义一个海鸥的形状：

```latex
\tikzset{
seagull/.pic={
% Code for a "seagull". Do you see it?...
\draw (-3mm,0) to [bend left] (0,0) to [bend left] (3mm,0);
}
}
```

使用`\tikzset`定义的是全局的, 整个文档都可以调用. 然后调用它：

```latex
\tikz \fill [fill=blue!20]
(1,1)
-- (2,2) pic {seagull}
-- (3,2) pic {seagull}
-- (3,1) pic [rotate=30] {seagull}
-- (2,1) pic [red] {seagull};
```

#### 定义新的Pic类型

如`pic`命令说明中所述, 要定义新的`pic`类型, 您需要

1. 定义一个路径前缀为`/tikz/pics`的`key`, 
2. 将`/tikz/pics/code`设置为`pic`的`code`. 
 
这可以使用`.style` handler 实现：

```latex
\tikzset{
  pics/seagull/.style ={
      % 当调用 seagull 的时候, 下面的代码会设置 seagull 的 code key:
      code = { %
          \draw (...) ... ;
        }
    }
}
```

一些简单的情况下, 可以直接使用`.pic` handler,

```latex
\tikzset{
    seagull/.pic = {
    \draw (...) ... ;
  }
}
```

此`handler`只能对带有`/tikz/`前缀的`key`一起使用, 因此通常应将其用作`TikZ`命令或`\tikzset`命令的选项.  
它使用`<key>`的路径, 并把其中的`/tikz/`替换为`/tikz/pics/`. 最终得到一个`style`, 能够执行`code = some code`. 
大多数情况下, `.pic`handler足以设置`keys`.  但是, 在某些情况下确实需要使用第一个版本:

+ 当您的图片类型需要设置`foreground`或`background`代码时. 
+ 如果给`key`提供了复杂的参数

例如：

```latex
\tikzset{
    pics/my circle/.style = {
    background code = { \fill circle [radius=#1]; }
  }
}
\tikz [fill=blue!30]
```

这里给`my circle`使用了参数. 

+ `<key>/.code n args={<m>}{<代码>}`:传入`m`个参数`{#1}{#2}{#3}...`, `m`为`0~9`, 不能多也不能少, 空格也可以作为参数. 

## 绘制曲线

### To 自由曲线

164 The To Path Operation
838 To Path Library
page 841 75.4 Loops

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

在给定出射和入射角度之后, `/tikz/looseness=<number>`选项中的`<number>`调控`control points`与初始点以及与终点的距离. 
还可以用 `/tikz/min distance=<distance>`, `/tikz/out min distance=<distance>` 控制最小距离, 避免计算无解. 

+ 使用 `loop`选项来绘制圈图曲线, 例如

```latex
\begin{tikzpicture}
  \begin{feynman}
  \draw  (0,0) edge [anti charged scalar,loop, looseness=30] (h3);
  \end{feynman}
\end{tikzpicture}
```

`loop`选项只接受一个参数, 即初始点, 终点位置和初始点相同, 然后把`looseness`设置为`8`, `min distance`设置为`5mm`.
如果想精确控制圈图的形状, 可以手动添加控制点, 例如：

```latex
\draw (a3) to [controls=+(45:1.5) and +(135:1.5)] (a3); 
```

上面使用`+(角度:距离)`的方式指定控制点的坐标, `and`左右的坐标采用相对坐标的形式, 分别相对于路径的`起点`和`终点`.

参考 page 33, 可以使用`to`的简称, 即`..`语法以 `曲线` 方式延伸路径:

```latex
.. controls <控制点1> and <控制点2> .. <终点>
```

你可以省略`and <控制点2>`, 这将导致使用`控制点1`两次.

如果要使用`tikz-feynan`定义的线型, 使用下面的`edge`, 并使用`tikz-feynan`定义的全称, 例如:

```latex
\draw (a3) edge [controls=+(30:3) and +(150:3), /tikzfeynman/fermion] (a4);
```

### edge

17.12 Connecting Nodes: Using the Edge Operation

`edge(边)`操作的作用类似于主路径绘制完成后添加的`to`操作, 就像`node`是在主路径绘制完成后添加的. 
这样, 每条边就可以有不同的外观. 

和`node`操作一样, `edge`的操作会暂时中止当前路径的构建, 并构建一个新的路径`p`. 
这个新的路径`p`将在主路径绘制完毕后被绘制. 请注意, `p`可以与主路径的选项完全不同.
还要注意的是, 如果主路径中有几个`edge`或`node`操作, 每个操作都会创建自己的路径, 并且按照它们在主路径上出现的顺序来绘制. 

```latex
\path … edge[<options>] <nodes> (<coordinate>) …;
```

`edge`操作的效果是, 在主路径之后, 下面的路径被添加到图片中:

```latex
\path[every edge,<options>] (\tikztostart) <path>;
```

这里, `<path>`是`to path`. 注意, 与`to`操作所添加的路径不同, `(\tikztostart)`被添加到`<path>`之前,
这对`to`操作来说是不必要的, 因为这个坐标已经是主路径的一部分.

`\tikztostart`是`edge`操作之前的路径上的最后一个坐标, 就像对`node`或`to`操作一样. 
然而, 这条规则有一个例外：如果`edge`操作的前面是`node`操作, 那么这个刚刚声明的`node`就是起始坐标.
而不是像通常情况下那样, 是这个节点所处的坐标--一个微妙的区别. 在这方面, `edge`与`node`和`to`都不同. 

如果一行有多个`edge`操作, 那么所有这些操作的起始坐标都是一样的, 但是目标坐标不同, 它们是主路径的一部分. 
因此, 起始坐标就是第一个`edge`操作之前的坐标. 这一点与`node`类似, `edge`操作也不会修改当前路径. 
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

从当前点开始画弧线, 可以用`x radius` and `y radius`指定半径, 用`start angle`, `end angle`, and `delta angle`指定角度.

也有一个较简捷的句法来指定圆弧:
`arc(<start angle>:<end angle>:<radius>)`
或者
`arc(<start angle>:<end angle>:<x radius> and <y radius>)`

## node 文字节点

### node 指定

`tikz-feynman`包中的顶点相当于`node`, `node`的特点是需要添加文字(可以为空白--`{}`), 也就是类似下面这种. 在`node`周围会留下空白, 其实是`node`占据的空间.

```latex
\node[above right =0.7 and 4.2 of a1] {text}
```

参考 p229, 17.2.2 Predefined Shapes.
如果不想画出`node`, 只是给坐标分配名称, 例如`(x)`, 并希望传播子可以直接连接`(x)`.  可以使用`coordinate`.
它的效果类似于使用了`(x.center)`, 不会把路径断开成几段. 可以完整的连接起来, 或者给包围的区域上色.

类似于

```latex
\coordinate[right =2.2  of a1] (a3); % 泡泡起点
```

这样后面不需要有`{text}`. 

p224 基本语法：

```latex
\path ... node <foreach statements>  [<options>] (<name>) at (<coordinate>) : <animation attribute>
={<options>} {<node contents>} ...;
```

各部分规范的顺序.  在`node`和`{<node contents>}`之间的所有内容都是可选的. 如果有`<foreach>`语句,它必须首先出现,紧接在`node`之后. 
除此之外,节点规范的所有其他元素( `<options>` ,`name`,`coordinate` 和 `animation attribute` )的顺序都是任意的,
实际上,这些元素中的任何一个都可能多次出现(尽管对于`name`和`coordinate`,这没有意义). 
例如

```latex
\vertex (a2) at (0,0){2};
```

***
`at` p158

```latex
\path ... circle[<options>] ...;
/tikz/at=<coordinate>
```

如果在`<options>`内部显式设置了此选项(或通过`every circle`样式间接设置), 则`<coordinate>`将用作圆的中心而不是当前点.  在一个封闭范围内给某个值设置`at`无效. 

page 785,72 Shape Library：可以指定`node`的形状, 有预定义的各种形状. 
page 563, Part V Libraries： 从这里开始是各种库, 有预定义的各种命令. 

### node位置的相对指定

page 240;

```latex
/tikz/below=<specification>
/tikz/left=<specification>
/tikz/right=<specification>
/tikz/below left=<specification>
/tikz/below right=<specification>
/tikz/above left=<specification>
```

`/tikz/above left=<specification>`类似`above`,但是`<shifting parti>`的指定更加复杂. 

1. 当`<shifting part>`形式为`<number or dimension> and <number or dimension>`的时候(注意中间有个`and`), 先向左移动, 再向右移动(通常这令人满意, 除非你使用了`x` and `y`选项, 修改了`xy`--坐标系的单位矢量. )
2. 当`<shifting part>`形式为`<number or dimension> `时, 也就是只给出一个参数, 向对角线方向(135度方向)移动$\frac{1}{2}\sqrt{2}cm$. 按照数学的说法, 就是按照$l_{2}-norm$理解, 相当于极坐标中的半径. 而`<number or dimension> and <number or dimension>`是按照$l_{1}-norm$理解. 

### node 引用外域node

page 260; 17.13 Referencing Nodes Outside the Current Picture

### 引用不同图片中的节点

可以在图片`A`中引用图片`B`中的`node`( 但不是很trivial) . 
这意味着你可以创建图片和一个`节点`, 稍后你可以从其他位置画一条线(`edge`)到这个`节点`. 

要引用不同图片中的节点, 请按以下步骤操作. 

1. 你需要在所有包含`目标节点`的图片上添加`remember picture`选项, 同时也要在所有包含`起始节点`的图片上添加`remember picture`选项. 
2. 您需要为路径或整个图片添加`overlay`选项, 这些图片包含对`域外节点`的引用. (这个选项可以关闭`bounding box`的计算) . 
3. 你需要使用一个支持`remember picture`的`驱动程序`, `pdfLaTeX`是支持的, 并且你需要运行`TeX`两次. 
关于幕后操作的更多细节, 见第107.3.2节. 让我们来看看这些选项的效果. 

```latex
/tikz/remember picture=<boolean> 无默认, 初始值为 false
```

这个选项告诉`TikZ`, 它应该尝试记住`当前图片`在页面上的位置. 这种尝试可能会失败, 这取决于使用的是哪种后端驱动程序. 
另外, 即使记忆成功, 这个位置可能也只在第二次运行`TeX`程序才能用. 如果可以记忆的话, 你可以考虑

```latex
\tikzset{every picture/.append style={remember picture}}
```

来使`TikZ`记住所有图片. 这将在`.aux`文件中为每张图片添加一行记录--通常不会很多. 

然后, 你就不必担心记忆图片的问题了. 

```latex
/tikz/overlay=<boolean> 默认为 true
```

这个选项主要是为了在引用其他图片中的节点时使用, 但你也可以在其他情况下使用它在其他情况下使用. 
这个选项的作用是, 在计算当前图片的`边界框`时, 不会考虑到当前`scope`范围内的所有对象. 

你需要在所有包含`域外节点`引用的路径( 或者至少是路径的所有部分) 指定这个选项. 
否则, `TikZ`会试图使当前的图片足够大, 以涵盖其他图片中的`节点`. 然而, 在`TeX`的第二次运行中, 这将创建一个更大的图片, 导致图片越来越大. 
除非你知道自己在做什么, 否则我建议在所有包含`域外引用`的图片上指定`overlay`选项. 

现在让我们看几个例子. 这些例子只有在用支持`remember picture`的驱动程序处理文档时才有效. 

在当前文本中, 我们放置两张图片, 包含名为`n1`和`n2`的节点, 

```latex
\tikz[remember picture] \node[circle,fill=red!50] (n1) {};
\tikz[remember picture] \node[fill=blue!50] (n2) {};
```

为了连接这些节点, 我们使用`overlay`和 `remember picture` 选项创建另一张图片. 

```latex
\begin{tikzpicture}[remember picture,overlay]
\draw[->,very thick] (n1) -- (n2);
\end{tikzpicture}
```

注意, 最后一张图片似乎是空的. 实际上它的大小为零, 并且包含在它边界之外的箭头. 
最后一个例子, 我们将另一张图片中的`节点`连接到前两个`节点`. 
在这里, 我们只给连线提供了`overlay`选项, 我们不希望把这条线算作图片的一部分. 

```latex
\begin{tikzpicture}[remember picture]
  \node (c) [circle,draw] {Big circle};
  \draw[overlay,->,very thick,red,opacity=.5] (c) to [bend left]  (n1)  (n1) -| (n2);
\end{tikzpicture}
```

### 引用当前页节点--绝对定位

有一个特殊的节点叫做`current page`, 可以用来访问当前页. 
它是一个长方形的节点, 其`south west`锚点是页面的`左下角`, `north east`锚点是页面的`右上角`. 

这个节点在内部是以特殊的方式处理的, 你可以引用它, 就像它已经被`记忆过`一样. 
因此, 通过给图片增加`remember picture`和`overlay`选项, 你可以在页面上`绝对定位`节点. 

第一个例子将文本放在当前页面的左下角. 

```latex
\begin{tikzpicture}[remember picture]
  \node[xshift=1cm,yshift=1cm] at (current page.south west)
              [text width=7cm, fill=red!20,rounded corners,above right]
              {
                This is an absolutely positioned text in the lower left corner. No shipout-hackery is used.
              }
\end{tikzpicture}
```

下一个例子在页面中间添加圆圈.

```latex
\begin{tikzpicture}[remember picture]
  \draw [line width=1mm,opacity=.25]
  (current page.center) circle (3cm);
\end{tikzpicture}
```

最后一个例子在页面上方叠加文字( 取决于例子的位置, 也可能出现在下一页).

```latex
\begin{tikzpicture}[remember picture,overlay]
\node [rotate=60,scale=10,text opacity=0.2] at (current page.center) {Example};
\end{tikzpicture}
```

### Late 代码和Late 选项

给节点提供的所有选项只影响本节点自己. 虽然这在大多数情况下是件好事, 但你有时可能想让选项在 `以后`产生影响. 
反过来说, 你有时可能会注意到一些选项只应该在`后期`被添加到节点上. 于此,可以使用下面这个版本的`node`路径命令:

```latex
\path … node also[<late options>](<name>) …;
```

请注意, `<name>`是强制性的, 不能在这里给出节点文本. 另外, 选项和节点标签的顺序必须如上. 

节点`<name>`必须已经存在. `<late options>`是在局域`scope`中执行的. 这些选项中的大多数不会有任何影响, 因为你不能改变节点的外观. 
也就是说, 你不能用`late`选项把一个红色节点变成绿色节点. 
然而, 在`<late options>`选项里面给出`append after command`和`prefix after command`选项 (直接或间接地)确实能达到预期的效果. 
给定的路径被执行, `\tikzlastnode` 被设置为 determined node.

所有这些的净效果是, 例如,  你可以提供`label`选项, 为已被创建的节点添加标签:

```latex
\begin{tikzpicture}
\node [draw,circle] (a) {Hello};
\node also [label=above:world] (a);
\end{tikzpicture}
```

正如 Section 14 所解释的, 你可以使用选项`append after command`和`prefix after command`在节点后添加路径. 下面的宏可能很有用. 

+ `\tikzlastnode`; 展开为路径上的最后一个节点. 你也可以用下面的`选项`来代替`node also`语法. 

```latex
/tikz/late options=<options> (无默认值)
```

这个选项可以在`路径`上给出( 但不能作为`节点`命令的参数) , 其效果与`node alos`命令相同. 
在`<options>`中, 你应该使用`name`选项来指定你希望添加`late option`的节点

```latex
\begin{tikzpicture}
\node [draw,circle] (a) {Hello};
\path [late options={name=a, label=above:world}];
\end{tikzpicture}
```

## 颜色

### 透明度

page 353: 23 Transparency

常用透明度选项：

```latex
/tikz/draw opacity=<value>
/tikz/fill opacity=<value>
```

`<value>`除了数字取值$[0,1]$之外, 还可以下列预设：

```latex
/tikz/transparent
/tikz/ultra nearly transparent
/tikz/very nearly transparent
/tikz/nearly transparent
/tikz/semitransparent
/tikz/nearly opaque
/tikz/very nearly opaque
/tikz/ultra nearly opaque
/tikz/opaque
```

## key 管理

### key 简述

p975. `key` 就是`tikz`中的各种关键字, 它们是通过包`pgfkeys`管理的. `\usepackage{pgfkeys}`. 

`key`的例子：`/tikz/coordinate system/x`, 或者只用写`/x`. 这种写法类似于`Unix`上的文件路径. 可以使用绝对路径, 也可以使用相对路径. `tikz`的`key`名称经常包含空格. 
调用`key`使用`\pgfkeys`命令. `pgfkeys`接受`key=value`形式的参数. 例如:

```latex
\pgfkeys{/my key=hallo, /your keys=something\strange, others=something else}
```

可以在`key`中存储一些`code`, 通常用`handler`来管理`key`的内容. 例如:

```latex
\pgfkeys{/my key/.code=The value is '#1'.}
\pgfkeys{/my key=hi!}
```

有许多`handler`可以用来定义`key`. 例如, 我们可以定义一个具有多个参数的`key`:

```latex
\pgfkeys{/my key/.code 2 args=The values are '#1' and '#2'.}
\pgfkeys{/my key={a1}{a2}}
```

常用`handler`有:

+ `.default`:提供默认值. 
+ `/.value required`:必须指定参数.
+ `/.value forbidden`:不能指定参数. 

`tikz`的所有`key`都以`/tikz`开头. 然而不必每次都显式加上这个前缀, 可以使用`/.cd`来声明默认目录. 

```latex
\pgfkeys{/tikz/.cd,line width=1cm, linecap=round}
```

当处理一个`key`时, 除了直接执行某些代码, 也可以递归调用另一些`keys`, 这种`key`被称为`styles`. `styles`本质上就是`key`的列表. 例如:

```latex
\pgfkeys{/a/.code=(a:#1)}
\pgfkeys{/b/.code=(b:#1)}
\pgfkeys{/my style/.style={/a=foo,/b=bar,/a=#1}}
\pgfkeys{/my style=wow}
```

`.styles`也可以带有参数`#1`, 如同普通的`.code`.

### The Key Tree

p975, `key`的组织方式类似`Unix`. `/a/b.../x`中, 前面的`/a/b.../`是`路径`, 后面的`x`是`名称`.
`pgfkeys`包中有一些内部命令, 如`\pgfkeyssetvalue`, `\pgfkeyslet`等等. 但是通常用户不需要直接调用这些命令.

简要提一下`\def`, `\let`,`\edef`三个命令：[What is the difference between \let and \def](https://tex.stackexchange.com/questions/258/what-is-the-difference-between-let-and-def)
`\let`相当于直接赋值, 右边的式子计算之后赋给左边. `\def`命令相当于mma中的`SetDelay`即`:=`, 会在被调用时重新计算. 
`\edef`是`expand def`的缩写, 也就是赋值之前右边的式子会被展开. 

### Key Handlers

`handler`相当于`key`的方法, 或者构造函数之类的. 
p984, 常用的 `key handler`:

+ `<key>/.cd`: 设置默认路径为`key`.
+ `<key>/.is family`：当使用`key`时, 当前路径被设置为`key`,效果同`<key>/.cd`.
+ `<key>/.default=<值>` :设置`key`的默认值. 例如`\pgfkeys{/width/.default=1cm}`
+ `<key>/.value required`: 表明必须赋值. 
+ `<key>/.value forbidden`: 表明禁止赋值. 
+ `<key>/.code=<代码>`: 添加代码, 可以有一个参数`#1`
+ `<key>/.code 2 args=<代码>`:表示有两个参数`{#1}{#2}`, 如果第二个未给出, 将会是空字符. 如果传入`first`, 第一个参数将是`f`, 第二个将是`irst`, 所以需要用`{}`包裹起来. 
+ `<key>/.code n args={<m>}{<代码>}`:传入`m`个参数`{#1}{#2}{#3}...`, `m`为`0~9`, 不能多也不能少, 空格也可以作为参数. 
+ `<key>/.code args={<参数模式>}{<代码>}`:可以指定任意的参数模式, 比如`(#1/#2)`,调用对应的写成`(first/second)`, 空格将被去掉. 
+ `<key>/.add code={<前缀代码>}{<后缀代码>}`：将代码添加到已经存在的`key`.
+ `<key>/.prefix code=<前缀代码>`: 类似`.add code`, 但只添加前缀. 
+ `<key>/.append code=<后缀代码>`: 类似`.add code`, 但只添加后缀. 

其中 `.code 2 args`和`.code args`的区别见下面的例子：

```latex
\pgfkeys{/page size/.code 2 args={\paperheight=#2\paperwidth=#1}}
\pgfkeys{/page size={30cm}{20cm}}
% 同样的定义, 使用 .code args
\pgfkeys{/page size/.code args={#1 and #2}{\paperheight=#2\paperwidth=#1}}
\pgfkeys{/page size=30cm and 20cm}
```

### Defining Styles

`.style`是`key`的列表, 它的`handler`和`key`基本相同, 只需要作替换`.code`->`.style`
