# tikz

用来替换`pdf` 粘贴过程额外`h..i`的正则表达式

```bash
\bh([\. ]+?)i\b # vscode 里面的正则, 元字符 . 需要转义成 \.
<$1>
```

## tikz 示例教程

[TiKZ入门教程 ](https://www.latexstudio.net/archives/9774.html)

安装`texlive`

我们先从一个最简单的例子开始：画一条直线.  代码如下：

```latex
\documentclass[tikz,border=10pt]{standalone} % 生成自动裁剪过的pdf
\usepackage{tikz}
\begin{document}
    \begin{tikzpicture}
        \draw (0,0) -- (1,1);
    \end{tikzpicture}
\end{document}
```

这样,我们得到了一个`pdf`文件,我们用`pdf2svg`工具将其转换为`svg`格式的矢量图.
带`\`的都是LaTeX的宏命令,这段代码的核心就一句话 `\draw (0,0) -- (1,1)`;
这句话的意思就是从`(0,0)`到`(1,1)`画一条线段. 我们还可以画的稍微复杂一点：

```latex
\documentclass[tikz,convert=pdf2svg]{standalone}
\usepackage{tikz}
\begin{document}
    \begin{tikzpicture}
        \draw [color=blue!50,->](0,0) node[left]{$A$}-- node [color=red!70,pos=0.25,above,sloped]{Hello}(3,3) node[right]{$B$};
    \end{tikzpicture}
\end{document}
```

输出：可以看到,`\draw`后面的方括号中跟的是对线的一些设置,`color=blue!50`表示的是用`50%`的蓝色,因为LaTeX中,`%`用作了注释,所以这里用`!`替代,

`->`表示的是线形是一个箭头,我们注意到,在起点坐标,`-–`,终点坐标后面,我们分别加入了一个`node`元素,起点后面的`node`表示的是加入一个标示,它在坐标点`(0,0)`的左边,`--`后面的`node`采用`70%`的红色,位置在线段的上方`0.25`的位置,随线段倾斜,花括号中是`node`的文字,为`Hello`,终点坐标同理.

`node`经常用于加入一些标注,这一点我们在后面将会看到.

### 一些复杂的形状

在TikZ中,除了画线段之外,还支持各种复杂的形状,下面一个例子给出了一些常见的形状：

```latex
\documentclass[tikz,convert=pdf2svg]{standalone}
\usepackage{tikz}
\begin{document}
    \begin{tikzpicture}
        \draw (0,0) circle (10pt);     %画一个圆心在原点,半径为10pt的圆；
        \draw (0,0) .. controls (1,1) and (2,1) .. (2,0);       %画一个起点为(0,0),终点为(2,0),控制点为(1,1),(2,1)的贝塞尔曲线；
        \draw (0,0) ellipse (20pt and 10pt);       %画一个中心在原点,长轴, 短轴分别为20pt和10pt的椭圆；
        \draw (0,0) rectangle (0.5,0.5);       %画一个从(0,0)到(0.5,0.5)的矩形
        \filldraw[fill=green!20!white, draw=green!50!black](0,0) -- (3mm,0mm) arc (0:30:3mm) -- cycle;
        %画一个扇形,并填充,扇形的边色和填充色的透明度不同.
    \end{tikzpicture}
\end{document}
```

### 属性预定义

在刚才的例子中我们看到,随着我们对样式需求的多样化,属性越来越长,而且多个实体之间往往具有相同的属性,这样一来,我们希望能预定义一个属性集合,到时候直接赋给相应的实体,`TikZ`本身就是个宏,因此它为我们提供了强大的属性定义功能,来看这段代码：

```latex
\documentclass[tikz,convert=pdf2svg]{standalone}
\usepackage{tikz}
\begin{document}
    \begin{tikzpicture}
    [
    L1Node/.style={circle,   draw=blue!50, fill=blue!20, very thick, minimum size=10mm},
    L2Node/.style={rectangle,draw=green!50,fill=green!20,very thick, minimum size=10mm}
    ]
       \node[L1Node] (n1) at (0, 0){$\int x dx$};
       \node[L2Node] (n2) at (2, 0){$n!$};
    \end{tikzpicture}
\end{document}
```

输出：在这段代码中,我们在最开始定义了两个名为`L1Node`和`L2Node`的属性,在生成`node`结点的时候直接填到属性的位置即可.

### 循环

TikZ相比于Viso的一个优势就在于其循环功能,Viso里面要循环画十个圆就得复制十次,还要调节各自的位置,如果遇到需要调整位置又得重来,TikZ这种命令行的方式能直接画出来,如果需要调整位置,更改参数即可,我们在上一个例子上生成十个结点：

```latex
\documentclass[tikz,convert=pdf2svg]{standalone}
\usepackage{tikz}
\begin{document}
    \begin{tikzpicture}
    [
    L1Node/.style={circle,   draw=blue!50, fill=blue!20, very thick, minimum size=10mm},
    L2Node/.style={rectangle,draw=green!50,fill=green!20,very thick, minimum size=10mm}
    ]
       \foreach \x in {1,...,5}
        \node[L1Node] (w1_\x) at (2*\x, 0){$\int_\Omega x_\x$};
    \end{tikzpicture}
\end{document}
```

### node树

node结点不但可以用于添加标识,还可以来绘制树形图,下面看一个例子

```latex
\documentclass[tikz,convert=pdf2svg]{standalone}
\usepackage{tikz}
\begin{document}
    \begin{tikzpicture}
        \node {root}
            child {node {a1}}
            child {node {a2}
                child {node {b1}}
                child {node {b2}}}
            child {node {a3}};
    \end{tikzpicture}
\end{document}
```

稍微加点样式：

```latex
\documentclass[tikz,convert=pdf2svg]{standalone}
\usepackage{tikz}
\begin{document}
    \begin{tikzpicture}
    [every node/.style={fill=blue!30,draw=blue!70,rounded corners},
     edge from parent/.style={blue,thick,draw}]
        \node {root}
            child {node {a1}}
            child {node {a2}
                child {node {b1}}
                child {node {b2}}}
            child {node {a3}};
    \end{tikzpicture}
\end{document}
```

### 绘制函数图像

TikZ提供了强大的函数绘制功能,下面的代码展示了如何绘制函数,当然,绘制数据图表并非TikZ擅长的事情,也并非其设计初衷,TikZ着眼于定性的图表,定量数据的演示还是用其他工具绘制较好.

```latex
\documentclass[tikz]{standalone}
\usepackage{tikz}
\begin{document}
    \begin{tikzpicture}[domain=0:4]
  \draw[very thin,color=gray] (-0.1,-1.1) grid (3.9,3.9);
  \draw[->] (-0.2,0) -- (4.2,0) node[right] {$x$};
  \draw[->] (0,-1.2) -- (0,4.2) node[above] {$f(x)$};
  \draw[color=red]    plot (\x,\x)             node[right] {$f(x) =x$};
  % \x r 表示弧度
  \draw[color=blue]   plot (\x,{sin(\x r)})    node[right] {$f(x) = \sin x$};
  \draw[color=orange] plot (\x,{0.05*exp(\x)}) node[right] {$f(x) = \frac{1}{20} \mathrm e^x$};
\end{tikzpicture}
\end{document}
```

TikZ提供了图的支持,通过类似于`dot`语言的方式来生成图关系

```latex
\documentclass[tikz]{standalone}
\usepackage{tikz}
\usetikzlibrary{graphs}
\begin{document}
\begin{tikzpicture}
    \graph {
        "$x_1$" -> "$x_2$"[red] -> "$x_3,x_4$";
        "$x_1$" ->[bend left] "$x_3,x_4$";
    };
\end{tikzpicture}
\begin{tikzpicture}
    \graph  {
     a -> {
        b -> c,
        d -> e
     } -> f
    };
\end{tikzpicture}
\end{document}
```

[各种更样的示例](http://www.texample.net/tikz/examples/)

```latex
\coordinate [label=left:{$a$}](a) at (0,0);
\draw (a) circle (0.5);
\node[inner color=white, outer color=orange,inner sep=0.5cm] (b) at (5,2){$b$};
\draw (a)--(b);
\draw (a) .. controls (1,3) and (5,5) .. (b);
\draw (a) -| (b);
\draw (a)|- (b);
```

## tikz 图形基线对齐

[LaTeX中tikz画的图放在公式环境中如何和公式对齐](https://www.zhihu.com/question/381314763/answer/1096658439)

```latex
\documentclass{ctexart}
\usepackage{adjustbox}
\usepackage{tikz}

\makeatletter
\newcommand\valignWithTikz[1]{%
  text \tikzcircle{#1} text, $a^2 + \tikzcircle{#1} + b^2$ \par
}
\newcommand\tikzcircle[1]{%
  \tikz[#1] \draw (0, 0) circle (.5);
}

\newcommand\sep{\par\hspace*{10em}}
\makeatother

\begin{document}
\subsection*{默认效果}
绘图底部和文字基线对齐 \sep
  \valignWithTikz{}

\subsection*{使用 \texttt{baseline} 选项}
\verb|\tikz[baseline=(current bounding box.center)]| \sep
  \valignWithTikz{baseline=(current bounding box.center)}
调整纵向偏移,\verb|\tikz[baseline={([yshift=-.5ex]current bounding box.center)}]| \sep
  \valignWithTikz{baseline={([yshift=-.5ex]current bounding box.center)}}

\subsection*{使用 \texttt{\char`\\adjustbox} 命令的 \texttt{valign} 选项}
\renewcommand{\tikzcircle}[1]{%
  \adjustbox{valign=#1}{\tikz \draw (0, 0) circle (.5);}%
}
\verb|\adjustbox{valign=M}{\tikz ...}| \sep
  \valignWithTikz{M}
\verb|\adjustbox{valign=m}{\tikz ...}| \sep
  \valignWithTikz{m}
\end{document}
```

### 例子

```latex
\begin{equation}
\begin{aligned}
-i M^2 (p^2)=
\adjustbox{valign=M}{
\feynmandiagram [layered layout, horizontal=b to c] {
a -- [photon, momentum=\(p\)] b
-- [fermion, half left, momentum=\(k\)] c
-- [fermion, half left, momentum=\(k-p\)] b,
c -- [photon, momentum=\(p\)] d,

}};
\end{aligned}
\end{equation}
```
