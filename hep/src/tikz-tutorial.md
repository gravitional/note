# Tutorial Karl

## 路径命令

`p32`; `tikz`中, `path`就是一连串的坐标, 在路径的开头可以选择:

+ `\path` 什么也不做
+ `\draw` 画出路径
+ `\fill` 填充路径
+ `\filldraw` 等等

对路径的操作:
用 `分号` 表示一条路径的结束. 每一条路径会有 `初始点`, `当前点` 等等特殊坐标.
如`current subpath start`

路径构建命令和绘画命令相分离, 与路径构建的选项, 都在路径命令中指定;
与实际描绘相关的选项, 都在`\draw`,`\fill`等命令的选项中指定.

+ 曲线 33

```latex
\draw (0,0) .. controls (1,1) and (2,1) .. (2,0);
```

+ 圆形:

```latex
(1,1) circle [radius=2pt]
ellipse [x radius=20pt, y radius=10pt]
```

+ 方形:

```latex
\draw (0,0) rectangle (0.5,0.5);
\draw[step=.5cm] (-1.4,-1.4) grid (1.4,1.4);
```

## 自定义格式

`p35`; `\tikzset`

```latex
help lines/.style={color=blue!50,very thin} %在环境内部任意地方定义格式, 后面可以调用

\tikzset{help lines/.style=very thin} %在文档开头, 定义全局格式
\tikzset{Karl's grid/.style={help lines,color=blue!50}} %格式可以嵌套

\draw[step=.5cm,gray,very thin] (-1.4,-1.4) grid (1.4,1.4); % 使用 grid 绘制 参考格子
```

`\tikzset` 容纳 `<key=value>`列表, 其中不能有空行.

## 颜色线型等

`p36`; 绘制选项

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

## 放大部分区域

剪切出图形的某一部分,clip

```latex
\draw[clip] (0.5,0.5) circle (.6cm);
\draw[step=.5cm,gray,very thin] (-1.4,-1.4) grid (1.4,1.4);
...
```

## 画抛物线

`p38`; 抛物线,parabola

```latex
\tikz \draw (0,0) rectangle (1,1)(0,0) parabola (1,1);
\tikz \draw[x=1pt,y=1pt] (0,0) parabola bend (4,16) (6,12);
```

`p39`; 填充封闭区域, 使用`cycle`进行封闭

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

## 渐变色

`p39`; 渐变色,shade

```latex
\shadedraw[left color=gray,right color=green, draw=green!50!black](0,0) -- (3mm,0mm)
```

## 相对坐标

`p40`; 定义命令, 相对坐标指定, 

```latex
(30:1cm |- 0,0) % 垂直线与水平线的交点, |- 左边的坐标对应铅垂线, 右边的对应水平线.
\def\rectanglepath{-- ++(1cm,0cm) -- ++(0cm,1cm) -- ++(-1cm,0cm) -- cycle} %% 连续的相对指定, 后一个坐标相对于前一个, 使用 \def 在任意地方定义一个命令替换.
\def\rectanglepath{-- +(1cm,0cm) -- +(1cm,1cm) -- +(0cm,1cm) -- cycle} %% 基于相同root的相对坐标, 后面几个坐标相对于同一个最初坐标
```

## 路径交点

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

## 作用域 scope

`p42`; `scope`, 类似于其他程序中的局部变量.
在导言区添加`\usetikzlibrary {scopes}`.

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

## 路径变形

`page 43`; 路径变形选项.

图像的最后位置是由`TikZ`, `TeX`, `PDF`共同决定的. 
`tikz` 提供了一些选项可以在自己的坐标系统内变换图像的位置. 
并且运行在路径中途修改变换方式. 例如:

```latex
\begin{tikzpicture}[even odd rule,rounded corners=2pt,x=10pt,y=10pt]
\filldraw[fill=yellow!80!black] (0,0)  rectangle (1,1) [xshift=5pt,yshift=5pt]  (0,0)  rectangle (1,1) [rotate=30]   (-1,-1) rectangle (2,2);
\end{tikzpicture}
```

这类选项有:

`x=<value>`, `y=<value>`, `z=<value>`. 例如:

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
+ `xslant=<factor>`, `yslant=<factor>`: 倾斜
+ `cm`: 指定任意的变换矩阵.

详细可以参考 `page 373`: 25 Transformations

## for循环, 迭代

`latex`本身有循环的命令, `pstricks`具有`\multido`命令. 
`tikz`也引入了自己的循环命令`\foreach`, 它定义在`\pgffor`中, 
`\tikz`会自动`\include`这个命令.

语法是`\foreach \x in {1,2,3} {$x =\x $,}`. 
循环区域用列表指定,  要循环的指令也放在一个`{}`中. 
如果不用`{}`包裹, 就把下一个`;`之前的命令当作循环指令. 
例如下面的语句绘制一个坐标系:

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

也可以结合 `平移` 使用:

```latex
\foreach \x in {-1,-0.5,1} \draw[xshift=\x cm] (0pt,-1pt) -- (0pt,1pt);
```

`\foreach`也可以使用`c`式的范围指定: `{a,...,b}`, 必须使用无量纲的实数.  `{1,3,...,11}`则可以指定步长. 两种语法可以混合, 例如:

```latex
\tikz \foreach \x in {1,3,...,11} \draw (\x,0) circle (0.4cm);
```

`循环` 可以嵌套:

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

为了方便, 还有一种`key/value`型的循环语法: 

```latex
foreach \key/\value in {1/a,2/b,3/c}
```

其中 `key/value`用`/`分隔开. 
在每一次循环中, `\key`和`\value`的值将一一对应.
如果循环域中, 某一项只给出了`key`, 会默认`value`等于`key`.

## 添加文字

添加文字可以使用`\node`命令, 也可以用来添加任意形状. 
通常的用法是`\node[选项]{文字}`. 
`\node`会放在当前位置, 也就是`\node`命令前面的那个坐标上.
当所有路径`draw/fill/shade/clipped/whatever`完成之后, 
才绘制`\node`, 所以`\node`的图层在最上面.

+ 可以使用类似于`anchor=north`指定`\node`的哪一个锚点放在前面给定的坐标上. 
也可以使用`below=1pt`直接指定`\node`的相对偏移.

+ 如果担心图形上的其他元素干扰了`node`的辨识度, 
可以给`node`加上`[fill=white]`选项, 绘制一个白色的背景.

+ 如果把`\node`放在`--`后面, 默认会将`\node`的位置放在这条线段的中点. 
可以使用`pos=`选项控制具体的位置, 也可以使用`near start`, `near end`等等指定大概位置.

+ 还可以使用`[above, sloped]`选项, 使曲线上方的`\node`贴合曲线斜率. 例如:

```latex
\begin{tikzpicture}
\draw (0,0) .. controls (6,1) and (9,1) ..
node[near start,sloped,above] {near start} node {midway}
node[very near end,sloped,below] {very near end} (12,0);
\end{tikzpicture}
```

如果`\node`中的文本量比较大, 需要控制换行, 
可以使用类似`text width=6cm`的选项控制`\node`宽度. 完整的例子为:

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

## pic: 图形复用

`pic`是`picture`的简称. 
通过预先定义的图片名字, 可以在指定的地方复用图形. 例如:

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

这里调用了`angles` and `quotes`库. 
前者预定义了`angle`图形, 后者可以简化参数输入为`"标记"`, 而不需要输入`label text="标记"`.
`{angle = A--B--C}`表示`angle`是`BA`和`BC`的夹角.  
`\coordinate`用于声明一个坐标点, 可以在后文引用.
