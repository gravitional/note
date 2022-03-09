# 浮动体和图形

## 浮动体

[liam.page](https://liam.page/2014/09/08/latex-introduction/)

由两个 graphics packages:

`graphics` : The `standard` graphics package.
`graphicx` :The `extended` or `enhanced`  graphics package

这两个包的区别在于可选参数给出的形式不同. 参数名称和必选参数是相同的.

插图和表格通常需要占据大块空间,所以在文字处理软件中我们经常需要调整他们的位置.
`figure` 和 `table` 环境可以自动完成这样的任务; 这种自动调整位置的环境称作浮动体(`float`). 我们以 `figure` 为例.

```latex
\begin{figure}[htbp]
\centering
\includegraphics{a.jpg}
\caption{有图有真相}
\label{fig:myphoto}
\end{figure}
```

`htbp` 选项用来指定插图的理想位置,这几个字母分别代表 `here`, `top`, `bottom`, `float page`,也就是就这里, 页顶, 页尾, 浮动页(专门放浮动体的单独页面或分栏). `\centering` 用来使插图居中; `\caption` 命令设置插图标题,`LaTeX` 会自动给浮动体的标题加上编号. 注意 `\label` 应该放在标题命令之后.

如果你想了解 `LaTeX` 的浮动体策略算法细节,你可以参考我博客中关于[浮动体的系列文章](https://liam.page/series/#LaTeX-%E4%B8%AD%E7%9A%84%E6%B5%AE%E5%8A%A8%E4%BD%93)
如果你困惑于"为什么图表会乱跑"或者"怎样让图表不乱跑",请看[我的回答](https://www.zhihu.com/question/25082703/answer/30038248).

### 表格

```latex
\begin{table}[htbp]
\centering
\begin{tabular}{|l|l|l|}

\end{tabular}
\caption{input anything you need}
\end{table}
```

### float 包

[float – Improved interface for floating objects](https://www.ctan.org/pkg/float)

把浮动体放到确定的位置:
改进了用于定义浮动对象(如图形和表格)的接口.引入了`boxed float`, `ruled float` and the `plaintop float`. 您可以定义自己的`floats`并改善旧`floats`的行为.
该软件包还提供了`H` float修饰符选项,用来替换过时的`here`包.您可以使用`\floatplacement{figure}{H}`将其设置为默认. 例如:

```latex
\begin{figure}[H]
\centering
\includegraphics{a.jpg}
\end{figure}
```

### 子页面宽度resizebox

[一行代码解决LaTex表格过宽或过窄问题](https://blog.csdn.net/Rained_99/article/details/79389189)

若表格过宽, 则使用

```latex
\begin{table}[htbp]
\center
\caption{ Example}
\resizebox{\textwidth}{12mm}{ %12mm 是高度, 调整到适合自己的大小为止
\begin{tabular}{lll}
\...
\end{tabular}
}%注意这里还有一个半括号
\end{table}
```

若表格过窄, 则使用

```bash
\begin{table}[htbp]
\center
\caption{ Example}
\setlength{\tabcolsep}{7mm}{%7可随机设置,调整到适合自己的大小为止
\begin{tabular}{lll}
...
\end{tabular}
}%注意这里还有一个半括号
\end{table}
```

如果想在`LyX`中使用, 参考[extended features ofLYX/LATEX](https://johnrhudson.me.uk/computing/Tips_tricks_and_extended_features_of_LyX_LaTeX.pdf)
在`Document->Settings->Modules`中添加`GraphicBoxes`模块, 然后就可以在菜单栏使用`Insert->Custom Insets->Resizebox`.
第一个参数是宽度, 第二个是高度, 为了避免`LyX`把`\textwidth`中的`\`解析成`\textbackslash`, 可以按下`Ctrl+L`, 在源码环境中输入.

***
同类型还有`\rotatebox`, `\rotatebox[旋转原点]{旋转角度}{框内容}`.

其中原点指定为`c`, `l`, `r`, `b`, `t`或者使用它们的组合, 逆时针的旋转角度按度数表示.
从`LYX 2.2`开始,  一旦你安装了`GraphicBoxes`模块, 选择`Insert.CustomInsets.Rotatebox`,
在你想要旋转框出现的地方选择`Insert.Origin`来添加一个`Origin`选项.  在`Origin`盒子中输入`=c`, 在`Angle`盒子中输入`30`, 在`Angle`盒子后面输入文字.

### resizebox

[22.3.4 \resizebox](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#index-_005ctabcolsep)

+ 语法:

```latex
\resizebox{horizontal length}{vertical length}{material}
\resizebox*{horizontal length}{vertical length}{material}
```

+ 给定一个大小(例如`3`厘米),请转换`material`使其达到该大小.
如果水平长度或垂直长度是一个感叹号`!` 就进行等比缩放.

+ 此示例使图形的宽度为半英寸,并按相同的比例垂直缩放图形,以防止图形变形.

```latex
\resizebox{0.5in}{!}{\includegraphics{lion}}
```

未加星标形式 `\resizebox` 取垂直长度为`box`的高度,而带星标形式 `\resizebox*` 取其`height+depth`.
例如,使用 `\resizebox*{!}{0.25in}{\parbox{1in}{使此框同时具有高度和深度. }}`
使文本的高度+深度达到四分之一英寸.

您可以使用 `\depth`,`\height`,`\totalheight`和 `\width`来引用框的原始大小.
因此,使用 `\resizebox{2in}{\height}{Two inch}`将文本设置为两英寸宽,但保留原始高度.

+ [8.23 tabular](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#index-_005ctabcolsep)

`\tabcolsep`;

长度是列之间间隔的一半.  默认值为`6pt`.  用 `\setlength`更改它.

### 文本对齐

[LaTeX 对齐问题](https://blog.csdn.net/lvchaoshun/article/details/50518271)
[latex23 doc](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#index-_005ccentering)

对齐的语法是 `{\centering 文字}` 或者

```latex
\begin{group}
  \centering ...
\end{group}
```

它使材料在其范围内(`scope`)居中.  它最常在诸如图形之类的环境中或在`parbox`中使用.
常用来使插图居中:

```latex
\begin{figure}
  \centering
  \includegraphics[width=0.6\textwidth]{ctan_lion.png}
  \caption{CTAN Lion}  \label{fig:CTANLion}
\end{figure}
```

`\centering `的作用范围到`\end{figure}`为止. 与`center`环境不同,`\centering`命令不会在文本上方和下方添加垂直空间.
这就是上面示例中的优势--没有多余的空间.

***
单行文本对齐

+ `\leftline{左对齐}`
+ `\centerline{居中}`
+ `\rightline{右对齐}`

***
多行文本或段落对齐

左对齐

```latex
\begin{flushleft}
...
\end{flushleft}
```

居中

```latex
\begin{center}
...
\end{center}
```

右对齐

```latex
\begin{flushright}
...
\end{flushright}
```

### 公式对齐

默认情况下公式是居中对齐的,但若希望改成左对齐可以

```latex
\documentclass[a4paper,fleqn]{article}
```

这对整篇文章都有效.

对某一行公式进行左对齐

```latex
\begin{flalign}
  your equation (1)
\end{flalign}
```

***
对某一个公式左对齐

```latex
some text here\\
yourequationhere
\\
and more text here.
```

对某几行公式

```latex
\begin{flalign}
\begin{split}
your equation (1)
your equation (2)
\end{split}&
\end{flalign}
```

[amsdoc](http://mirrors.ustc.edu.cn/CTAN/macros/latex/required/amsmath/amsldoc.pdf)

ams 数学环境包括:

```latex
equation     equation*    align          align*
gather          gather*         alignat      alignat*
multline      multline*     flalign        flalign*
split
```

`split`环境是一种特殊的从属形式,仅在其他方程环境内部使用.  但是它不能在`multline`中使用.
`split`仅支持一个对齐(`＆`)列;  如果需要更多,应使用`aligned`或`alignedat`.
`split`结构的宽度是full line width

```latex
\begin{equation}\label{xx}
\begin{split}a& =b+c-d\\
& \quad +e-f\\
& =g+h\\
& =i
\end{split}
\end{equation}
```

### 其它对齐方法

`左对齐`, `居中对齐`, `右对齐` 的环境分别为`flushleft`, `center`和`flushright`.
也可以使用命令`\raggedright`, `\centering`和`\raggedleft`使以后的文本按指定方式对齐.

加载`amsmath`宏包后,使用选项`fleqn`(就是声明加载宏包时使用`\usepackage[fleqn]{amsmath}`)
可以使本该居中对齐的行间公式改为左对齐.

### parbox

[20.3 \parbox](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#g_t_005cparbox)

概要,其中之一:

```latex
\parbox{width} {contents}
\parbox[position] {width} {contents}
\parbox[position] [height] {width} {contents}
\parbox[position] [height] [inner-pos] {width} {contents}
```

产生一个宽度为`width`的文本框.
使用此命令可以使一小段文本框变成单个段落.该命令是`fragile`的(请参阅`\protect`).

```latex
\begin{picture}(0,0)
  ...
  \put(1,2){\parbox{1.75in}{\raggedright Because the graph is a line on
                         this semilog paper, the relationship is
                         exponential.}}
\end{picture}
```

内容被以 `文本模式` 处理
(请参见[`Modes`](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#Modes)),
因此`LaTeX` 会中断换行以形成段落. 但是它不会包含多个段落; 为此,请使用`minipage`环境(请参见`minipage`).

`\parbox`的选项(除了内容)与`minipage`的选项相同.
为方便起见,此处提供了选项的摘要,但完整说明请参见[minipage](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#minipage).

有两个必需的参数. `width`是刚性长度(请参见`Lengths`).
它设置 `LaTeX` 将内容排版到其中的框的宽度.
`contents`是放置在该框中的文本. 它不应包含任何`paragraph-making`组件.

有三个可选参数,`position`, `height`, and `inner-pos`.
`position`给出`parbox`相对于周围材料的垂直对齐.
可能的值是`c`或`m`以使`parbox`的垂直中心与相邻线的中心对齐(这是默认值),
或`t`可以使`parbox`的顶行与周围材料的基线匹配,或者`b`匹配底线.

可选参数`height`覆盖框的自然高度.

可选参数`inner-pos`控制内容在 `parbox` 中的位置. 它的默认值是`position`的值.
其可能的值为: `t` 将内容放置在框的顶部, `c` 将其放置在框的垂直中心, `b` 将其放置在框的底部,
`s` 将其垂直拉伸(为此,文本必须包含垂直可拉伸的空间).

## 源代码展示环境

[LaTex:插入代码的listings包和lstlisting环境](https://blog.csdn.net/quantumpo/article/details/26854289)
[TheListingsPackage](https://mirrors.aliyun.com/CTAN/macros/latex/contrib/listings/listings.pdf)

```latex
% LaTex中插入高亮显示的代码需以下设定
 % 注意,代码中不能含有中文,否则为无法编译.
\usepackage[utf8]{inputenc}
%\usepackage[T1]{fontenc}
% The package allows the user to select font encodings,
% and for each encoding provides an interface to 'font-encoding-specific' commands for each font
\usepackage{listings}
% 在LaTex中添加代码高亮
\usepackage{color}
%定义各种颜色
\definecolor{codegreen}{rgb}{0,0.6,0}
\definecolor{codegray}{rgb}{0.5,0.5,0.5}
\definecolor{codepurple}{rgb}{0.58,0,0.82}
\definecolor{backcolour}{rgb}{0.95,0.95,0.92}
%\lstdefinestyle{〈style name〉}{〈key=value list〉}
%stores the key=value list
\lstdefinestyle{mystyle}{
    backgroundcolor=\color{backcolour},
    commentstyle=\color{codegreen},
    keywordstyle=\color{magenta},
    numberstyle=\tiny\color{codegray},
    stringstyle=\color{codepurple},
    basicstyle=\footnotesize,
    breakatwhitespace=false,
    breaklines=true,
    captionpos=b,
    keepspaces=true,
    numbers=left,
    numbersep=5pt,
    showspaces=false,
    showstringspaces=false,
    showtabs=false,
    tabsize=2
}

例子

\begin{oframed}
\begin{lstlisting}[language=C++,style=codestyle1]
#include <iostream>
using namespace std;
int main(){
    cout << "Hello world!" << endl;
}
\end{lstlisting}
\end{oframed}
```

如果遇到因为中文字符报错的问题,可以尝试添加`framed`环境.

```latex
\begin{framed}
 \begin{lstlisting}  % 或者 \lstinputlisting{...}
 \end{lstlisting}
 \end{framed}
```

[package framed](https://www.ctan.org/pkg/framed)

该软件包创建了三个环境:

+ `framed`,用普通方框围绕该区域,通常的 frame box (`\fbox`),`edge`在`margin`(页边)
+ `oframed`,在分页处,方框的顶部和底部是开放的
+ `shaded`,阴影区域,`\colorbox`
+ `leftbar`,在左侧放置一条线. 环境允许在开始时有一个中断(`\FrameCommand`允许创建标题附加到该环境);
+ `framed/shaded`环境中也允许有`breaks`.

还有一个命令`\MakeFramed`可以创建自己的框架式环境.

创建可以跨页的`边框`,`阴影`或其他高亮区域. 定义的环境有
`framed` 通常的 frame box (`\fbox`),`edge`在`margin`(页边)
`oframed` 在分页符处带有开放的 顶/底框
`shaded` 背景阴影(\colorbox),阴影边界渗入页边
`shaded*` 背景阴影,阴影边界在页边
`snugshade` 阴影紧密贴合文本(特别是列表中的阴影)
`snugshade*` like snugshade with shading edge at margin
`leftbar` 左边缘的粗垂直线
`titled-frame` 带有标题栏的框

实际上,`shaded`环境只是将`\FrameCommand`重新定义为`\colorbox{shadecolor}`. (所以你需要定义颜色`shadecolor`:`\definecolor{shadecolor}...`).

常用颜色指定

```latex
\definecolor{shadecolor}{rgb}{0.9412,1,1} %靛青色
\definecolor{shadecolor}{rgb}{0.9,0.9,0.9} %灰色
 \colorbox[rgb]{.87, .9, .83} %  淡青色
\definecolor{notes}{rgb}{.75, .3, .3}% 橙色
\definecolor{shadecolor}{rgb}{0.96,0.96,0.93} % 土黄色
```

## 画费曼图

画费曼图有许多包, 现在了解到的有:

[ GkAntonius:feynman ](https://github.com/GkAntonius/feynman):Sharp-looking Feynman diagrams in python
[ JP-Ellis /tikz-feynman ](https://github.com/JP-Ellis/tikz-feynman):Feynman Diagrams with TikZ
[Asymptote: The Vector Graphics Language](https://asymptote.sourceforge.io/): 其中有个叫做`feynman`的模组

## 页面设置

一页的大小怎么设置,能容纳多少行,一行能有多宽,页眉, 页尾怎么设置,凡此种种,都叫整体页面的设置.
首先,这一切,都需要我们使用宏包geometry. 因此,首先,我们需要在导言区中写上`\usepackage{geometry}`.
[geometry](https://www.ctan.org/pkg/geometry)

该软件包提供了一个轻松灵活的用户界面来自定义页面布局,实现了自动居中和自动平衡机制,因此用户只需给出最少的页面布局描述即可.
例如,如果您想将每个边距设置为`2cm`,而没有标题空间,则只需要`\usepackage[margin=2cm,nohead]{geometry}`.

### 页面大小设置

我们常用的页面大小,就是`A4`纸.要实现这个,可以在导言区中写上`\geometry{a4paper}`
如果我们不是用约定的纸张大小,而是想以数值形式指定纸张大小,比如说长`22厘米`,宽`10厘米`,那么,我们可以在导言区中写

```latex
\geometry{paperheight=22cm, paperwidth=10cm}
```

***
版心位置, 大小设置

除了设置页面的大小以外,我们还可以设置版心的位置和大小. 页面文字一般会留出一些边距, 除去这些页边距以外,包括整个文字部分的长方形叫做版心.
每行的第一个字顶着版心的最左边开始,一直写到版心的最右边开始断行.

版心的位置可以通过设置其`左边距`, `右边距`, `上边距`, `下边距`来设置.
其对应的参数名分别为 `left`, `right`, `top`, `bottom`. 比如说,我们要设置左边距为`2cm`,可以在导言区中写

```latex
\geometry{left=2cm}
```

此外,我们如果需要将版心居中,竖直居中的参数名为`vcentering,` 水平居中的参数名为`hcentering`, 水平, 竖直均居中的参数名为`centering`.
例如让版心水平居中,可以在导言区中写

```latex
\geometry{hcentering}
```

此外,还可以设置版心的大小.版心的长为`textheight`, 宽为`textwidth`. 我们如果要设置版心长为`20cm`,可以在导言区中写

```latex
\geometry{textheight=20cm}
```

`geometry`宏包具有上述参数的图像描述
