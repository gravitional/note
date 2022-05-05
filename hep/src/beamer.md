# beamer

参考:
[使用 Beamer 制作学术讲稿 ](https://www.latexstudio.net/archives/2825.html)
[beamer class](https://mirrors.ustc.edu.cn/CTAN/macros/latex/contrib/beamer/doc/beameruserguide.pdf)

## 如何阅读用户指南

page 11 ; 1.4 How to Read this User's Guide

你应该从第一部分开始. 如果你还没有安装该软件包, 请先阅读第2部分. 如果你 如果你是`beamer`的新手, 你应该接着阅读第`3`节的教程.

当你坐下来创建你的第一个真正的`presentation`时, 请阅读第`4`节, 其中讨论了一个可能的工作流程的技术细节.
如果你仍然是创建演示文稿的新手, 你可能会发现第`5`节很有帮助, 其中给出了许多准则, 其中给出了什么该做和什么不该做.
最后, 你应该浏览一下第`6`节, 在那里你可以找到用于创建`talks`的现成的模板, 甚至可能是你打算使用的语言.

本用户指南的第二部分详细介绍了`beamer`中定义的所有命令, 但它也涉及到与创建演讲有关的其他技术问题(如如何包括图形或动画) .

第三部分解释了如何使用`主题`, 或通过为演示文稿的特定元素指定`颜色`或`字体`来轻松改变演示文稿的外观(例如, `枚举`中的`数字`所用的字体) .

第四部分是关于讲义(handouts)和演讲稿(lecture), 即所谓的 "支持材料".
你将经常 你经常需要制作一些支持材料, 在演讲中或演讲后送给你的听众, 这部分将解释如何使用演示文稿的源文件来做这件事.

最后一部分包含 "Howtos", 解释如何使用`beamer`来完成特定任务.

这个用户指南包含了所有 `公共` 命令, 环境和概念的描述, 这些都是由 b`eamer-class`定义的. 下面的例子显示了事物是如何被列出的.
一般来说, 红色文本是`defined`. 绿色文本是`optional`, 蓝色文本表示特殊模式的考虑.

```latex
\somebeamercommand[<optional arguments>]{<first argument>}{<second argument>}
```

这里你会找到`\somebeamercommand`命令的作用. 绿色参数是可选的. 例子中的这个命令需要两个参数. 例如

```latex
\somebeamercommand[opt]{my arg}{xxx}
```

```latex
\begin{somebeamerenvironment}[<optional arguments>]{<first argument>}
<environment contents>
\end{somebeamerenvironment}
```

这里你会找到`somebeamerenvironment`环境的效果. 和命令一样, 绿色的是可选的. 例如:

```latex
\begin{somebeamerenvironment}{Argument}
Some text.
\end{somebeamerenvironment}
```

***

```latex
Beamer-Template/-Color/-Font some beamer element
```

在这里你会发现对名称为`some beamer element`的`template`, `color`或`font` 的解释.
一个`beamer element`是一个概念, 在第16节有更详细的解释. 粗略地说, `element`是演示文档的一部分, 可能以某种特殊的方式进行排版.
`元素`的例子有: `帧标题`, `作者名称`, 或`脚注符号`. 对于大多数元素来说, 都有一个`template`,`beamer-color` 和 `beamer-font`, 请看第16节.

对于元素`some beamer element`, 将列出是否有`template`, `beamer-color`, 和`beamer-font`存在.
通常情况下, 这三种元素都存在, 并且在元素需要被排版时一起使用. 也就是说, 当模板被插入时, `beamer-color`和`-font`首先被安装.
然而, 有时模板没有`颜色`或`字体`与之相关联(像`父模板`) . 此外还存在没有底层`模板`的`beamer-color`和`-fonts`.

使用和改变模板将在第16.3节解释. 扼要的说, 要改变一个模板, 你可以使用

```latex
\setbeamertemplate{some beamer element}{your definition for this template}
```

不幸的是, 为一些模板想出好的定义并不是很 trivial 的事情. 幸运的是, 模板通常有`预定义`的选项, 这些选项是这样表示的:

+ `[square]` ; 使一个小正方形被用来渲染模板.
+ `[circle]{<radius>}` ; 使用给定半径的圆来呈现模板

你可以这样安装这样一个预定义的选项;

```latex
\setbeamertemplate{some beamer element}[square]
% 现在使用的是正方形
\Setbeamertemplate{some beamer element}[circle]{3pt}
现在使用一个圆形
```

`beamer-colors`在第17节有解释. 扼要的说,  要把颜色的前景改为红色, 使用

```latex
\setbeamercolor{some beamer element}{fg=red}
```

要把背景改成黑色, 就用

```latex
\setbeamercolor{some beamer element}{bg=black}
```

你也可以使用`fg=red,bg=black`来同时改变它们. 背景并不总是被 "尊重 "的.
因为要正确显示彩色背景是很困难的, `templates`必须做出额外的努力. 而前景颜色通常是自动使用的.

`beamer-font`在第18节有解释. 扼要的说, 要把字体的大小改为大字体, 使用

```latex
\setbeamerfont{some beamer element}{size=\large}
```

除了大小之外, 你还可以使用诸如`series=\bfseries`来设置系列, `shape=\itshape`来改变形状, `family=\sffamily`来改变字族.
而且你可以把它们结合起来使用. 带星号的命令可以首先`重置`字体, 再应用命令.

+ `PRESENTATION`-- 如同本段旁边, 有时你会发现在段落旁边有蓝色的`presentation`这个词.
这意味着该段只适用于你 `正常使用LaTeX或 pdfLaTeX排版你的演讲`.
+ `ARTICLE`-- 与此相反, 旁边有`article`的段落描述的是`article`模式的一些特殊行为. 这种特殊模式用于从演示文稿中创建讲义(两者可以共存于一个文件中) .

## 5 创造一个演示的参考

### 组织一个frame

+ 使用块环境, 例如 `block`, `theorem`, `proof`, `example` 等.
+ 优先使用`enumerations` and `itemize` 而不是纯文本环境.
+ 在定义几件事时使用`description`.
+ 请勿使用超过两个级别的`"subitemizing."`. `beamer`支持三个级别, 但您不应使用三层. 通常, 您甚至都不应该使用第二个. 请改用优质的图形.
+ 不要创建无尽的逐项`itemize`或`enumerate`列表.
+ 不要逐段显示列表.
+ 强调是创建结构的重要组成部分. 使用`\alert`突出显示重要的内容. 
适用对象可以是一个单词或整个句子. 但是, 不要过度使用突出显示, 因为这会抵消效果. 如 `\alert{prime number}`

+ 使用列, 如下:

```latex
\begin{columns}[t]
\column
{.22\textwidth}
%\pause
\column{.65\textwidth}
\end{columns}
 ```

+ 切勿使用脚注(footnotes.). 他们不必要地打乱了阅读流程. 
脚注中所说的如果重要, 应放在普通文本中; 或不重要, 应将其省略(尤其是演示文稿中).
+ 使用`quote`或`quotation`排版引文.

```latex
\begin{quotation/quote}<<action specification>>
<environment contents>
\end{quotation/quote}
```

+ 除较长的书目外, 请勿使用选项`allowframebreaks`.
+ 请勿使用较长的参考书目.

## 创建覆盖 9

page 80

### 递增指定

+ `+-` 会被替换成一个计数器(latex counter)`beamerpauses`的值, 它在每张`frame`上重置为`1`, 每个`overlay specification` 使它增加`1`.
这样可以方便的实现递增 `uncover` 效果.
+ 后面的`-`表示从此帧起, 例如`<1->`表示从第一帧开始.
+ `alert` 表示强调
+ `|` 用来分隔不同的指定.

```latex
\begin{itemize}
\item<+-| alert@+> Apple
\item<+-| alert@+> Peach
\item<+-| alert@+> Plum
\item<+-| alert@+> Orange
\end{itemize}
```

例如第一个 `<+-| alert@+>` 被替换成 `<1-| alert@1>`
由于`itemize`支持设置默认 `overlay` specification, 所以也可以这么写

```latex
\begin{itemize}[<+-| alert@+>]
\item Apple
\item Peach
\item Plum
\item Orange
\end{itemize}
```

任何一个`+`号的出现都可以在圆括号中跟上一个偏移量. 这个偏移量将被添加到`beamerpauses` 的值.
因此, 如果`beamerpauses`是`2`, 那么`<+(1)->`扩展为`<3->`, `<+(-1)-+>`扩展为`<1-2>`. 比如说

```latex
\begin{itemize}[<+(1)->]
\item Apple
\item Peach
\item Plum
\item Orange
\end{itemize}
```

## Structuring a Presentation: The Local Structure

### 高亮

`\structure<<overlay specification>>{<text>}`

给定的文本被标记为结构的一部分, 也就是说, 它应该可以帮助观众看到演示文稿的结构.
如果存在`<overlay specification>`, 则该命令仅对指定的幻灯片有效.

```latex
\begin{structureenv}<<overlay specification>>
<environment contents>
\end{structureenv}
```

`\structure` 命令的环境版本.

## 颜色

### 默认和特殊颜色主题

默认颜色主题中的主要颜色如下:

+ `normal text` is black on white.
+ `alerted text` is red.
+ `example text` is a dark green (green with 50% black).
+ `structure` is set to a light version of MidnightBlue (more precisely, 20% red, 20% green, and 70% blue)

`example`,`exampleblock`是环境

## beamer 加参考文献

[在 Beamer 中使用参考文献](https://guyueshui.github.io/post/use-reference-in-beamer/)

主要是要把参考文献生成部分的命令放在`frame`里面, 其他的和平常的用法一样.
指定参考文献库, 指定参考文献风格.

```latex
\usepackage{cite}
% Removes icon in bibliography
\setbeamertemplate{bibliography item}[text]
...
\begin{document}
...
%%% end of your presentation slides
\begin{frame}[allowframebreaks]{References}
    %\bibliographystyle{plain}
    \bibliographystyle{amsalpha}
    %\bibliography{mybeamer} also works
    \bibliography{./mybeamer.bib}
\end{frame}
\end{document}
```

`bibtex` 标准 style `plain`,`unsrt`,`alpha`,`abbrv`

用 `find` 查找到的本机的安装的支持`nat`的`bst`:

+ /bst/shipunov/rusnat.bst
+ /bst/bib-fr/abbrvnat-fr.bst
+ /bst/bib-fr/plainnat-fr.bst
+ /bst/bib-fr/unsrtnat-fr.bst
+ /bst/ksfh_nat/ksfh_nat.bst
+ /bst/natbib/unsrtnat.bst
+ /bst/natbib/plainnat.bst
+ /bst/natbib/abbrvnat.bst
+ /bst/persian-bib/plainnat-fa.bst
+ /bst/nature/naturemag.bst
+ /bst/sort-by-letters/plainnat-letters.bst
+ /bst/sort-by-letters/frplainnat-letters.bst
+ /bst/phfnote/naturemagdoi.bst
+ /bst/beebe/humannat.bst
+ /bst/swebib/sweplnat.bst
+ /bst/upmethodology/upmplainnat.bst
+ /bst/dinat/dinat.bst
+ /bst/din1505/natdin.bst

## beamer中文

[数学字体](https://mirrors.bfsu.edu.cn/CTAN/info/Free_Math_Font_Survey/en/survey.html)
[xeCJK中文字体包](https://www.ctan.org/pkg/xecjk)
[如何使用 LaTeX/XeLaTeX 编辑中文? ](https://zhuanlan.zhihu.com/p/27739925)
[全面总结如何在 LaTeX 中使用中文 (2020 最新版)](https://jdhao.github.io/2018/03/29/latex-chinese.zh/)

这里介绍 `LaTeX` 编辑中文的两种方式. 注意, 虽说是使用`LaTeX`,实际使用的是 `XeLaTeX` 引擎. 具体方法如下:

### 使用xeCJK宏包

***
[How to get Beamer Math to look like Article Math](https://tex.stackexchange.com/questions/34265/how-to-get-beamer-math-to-look-like-article-math)
如果你仅仅需要在文档中使用有限的一些中文字符, 你可以使用 `xeCJK` 宏包, 然后使用 `xelatex` 命令编译源文件.

***
[Beamer的中文自动换行问题](http://softlab.sdut.edu.cn/blog/subaochen/2018/11/)

在`LyX`中, 标准的`Beamer`无法实现中文自动换行, 观察其`tex`源文件可以发现, 其导入的`package`为: `\usepackage{fontspec}`
而在导言区使用`\usepackage{xeCJK}`
就可以支持中文自动换行了.

***
`beamer` 有个选项, 可以更改数学字体的显示方式, `\usefonttheme[onlymath]{serif}`可以使数学字体风格为`serif`

一个简单可运行的例子如下:

```latex
%!TEX program=xelatex
% 该文件使用 xelatex 命令可以编译通过
\documentclass[12pt, a4paper]{article}
\usepackage{fontspec}
\usepackage[slantfont, boldfont]{xeCJK}

% % 设置英文字体
\setmainfont{Latin Modern Roman}
\setsansfont{Latin Modern Sans}
\setmonofont{Latin Modern Roman}
% 设置中文字体
\setCJKmainfont[Mapping=tex-text]{Noto Serif CJK SC}
\setCJKsansfont[Scale=0.7,Mapping=tex-text]{Noto Sans CJK SC}
\setCJKmonofont[Scale=0.7]{Noto Sans Mono CJK SC}
\XeTeXlinebreaklocale "zh"  % 中文断行设置
\XeTeXlinebreakskip = 0pt plus 1pt
%设置数学字体
\usepackage{unicode-math}
\setmathfont{latinmodern-math.otf}
%开源数学字体见 http://www.gust.org.pl/projects/e-foundry/lm-math
\title{测试}
\author{东}
\date{2016年6月6日}
\begin{document}
\maketitle
\begin{center}
满纸荒唐言\\
一把辛酸泪\\
都云作者痴\\
谁解其中味\\
\end{center}
\begin{verse}
\texttt{Stray birds of summer come to my window to sing and fly away}. \\
\textsf{And yellow leaves of autumn, which have no songs}, \\
\textrm{flutter and fall there with a sign}.\\
\hfill \emph{RabindranathTagore}
\end{verse}
\begin{verse}
\texttt{夏天的飞鸟}, \textsf{飞到我的窗前唱歌}, \textrm{又飞去了}. \\
秋天的黄叶, 它们没有什么可唱, 只叹息一声, 飞落在那里. \\
\hfill \emph{罗宾德拉纳特·泰戈尔}
\end{verse}
\end{document}
```

对于中文来说, `\setCJKmainfont{}` 命令用来设置正文使用的中文字体, 同时也是 `\textrm{}` 命令使用的字体.
`\setCJKmonofont{}` 用来设置 `\texttt{}` 命令中的中文使用的字体. `\setCJKsansfont{}` 用来设置 `\textsf{}` 命令中的中文使用的字体.

那么问题来了, 如何找到可用的中文字体呢? 如果你已经安装了 `TeX Live`, 那么很容易找到中文字体. 在系统的命令行, 使用下面的命令:

```bash
fc-list :lang=zh
```

此种方式试用于有特定文档类型的情况, 如 `beamer` .

***
lyx 的设置

```latex
\batchmode
\makeatletter
\def\input@path{{/home/tom/note/chpt/}}
\makeatother
\documentclass[utf8,dvipsnames,svgnames,x11names,hyperref]{beamer}
\usepackage{amstext}
\usepackage{amssymb}
\usepackage{fontspec}
\setmainfont[Mapping=tex-text]{Noto Sans CJK SC}
\setsansfont[Scale=0.7,Mapping=tex-text]{Source Han Sans SC}
\setmonofont[Scale=0.7]{Noto Sans Mono CJK SC}
\usepackage{mathrsfs}

```

还可以同时用`Scale=0.7`调节大小

### 使用 ctexbeamer 文档类型:

```latex
%!TEX program=xelatex
\documentclass{ctexart}
\begin{document}
    你好!
\end{document}
```

此种方式比较简便, 也更适合中文排版要求, 建议选用.

## lyx 使用中文

首先必须调用 `xelatex` 进行编译, 找到设置`Document Settings`--`Fonts`--勾选`Use non-Tex fonts`,即可选择系统自带的字体, 即可显示中文,
建议选择默认字体, 避免字体不全问题.

`LaTeX font encoding: None fontenc`

对应 `latex`设置为

```latex
\setmainfont[Mapping=tex-text]{Times New Roman}
\setsansfont[Mapping=tex-text]{Noto Sans CJK SC}
\setmonofont{Noto Sans Mono CJK SC}
```

另外, `Document Settings`--`Language`中可设置语言, 以及`xeTeX,utf-8`编码.

可以在`Insert`菜单栏中插入`beamer`特有的格式.

## 添加当前位置

texdoc beamer : 101

```latex
\frame{\tableofcontents[currentsection]}
```

### 背景图片

page 76; 8.2.7 The Background

```latex
\setbeamertemplate{background}{\includegraphics[height=\paperheight]{b.jpg}}
```

DarkCyan

[beamer封面的设计制作 ](https://www.latexstudio.net/archives/966.html)

通常我们beamer的封面部分比较少做修改, 下面这个把具体修改的部分都写了出来. 对于我们定制beamer有所帮助, 推荐推荐.

```latex
\documentclass{beamer}
\usepackage{tikz}
\usepackage{pdfrender}

\usetikzlibrary{shapes,arrows}
\setbeamerfont{author}{size=\Huge}
\setbeamerfont{institute}{size=\normalsize\itshape}
\setbeamerfont{title}{size=\fontsize{30}{36}\bfseries}
\setbeamerfont{subtitle}{size=\Large\normalfont\slshape}

\setbeamertemplate{title page}{%
\begin{tikzpicture}[remember picture,overlay]
\fill[orange] ([yshift=15pt]current page.west) rectangle (current page.south east);
\node[anchor=east] at ([yshift=-50pt]current page.north east) (author) {\parbox[t]{.6\paperwidth}{\raggedleft%
\usebeamerfont{author}\textcolor{orange}{%
\textpdfrender{ TextRenderingMode=FillStroke, FillColor=orange, LineWidth=.1ex,}{\insertauthor}}}};
\node[anchor=north east] at ([yshift=-70pt]current page.north east) (institute)
{\parbox[t]{.78\paperwidth}{\raggedleft%
\usebeamerfont{institute}\textcolor{gray}{\insertinstitute}}};
\node[anchor=south west] at ([yshift=20pt]current page.west) (logo)
{\parbox[t]{.19\paperwidth}{\raggedleft%
\usebeamercolor[fg]{titlegraphic}\inserttitlegraphic}};
\node[anchor=east] at ([yshift=-10pt,xshift=-20pt]current page.east) (title) {\parbox[t]{\textwidth}{\raggedleft%
\usebeamerfont{author}\textcolor{white}{%
\textpdfrender{TextRenderingMode=FillStroke, FillColor=white, LineWidth=.1ex,}{\inserttitle}}}};
\node[anchor=east] at ([yshift=-60pt,xshift=-20pt]current page.east) (subtitle)
{\parbox[t]{.6\paperwidth}{\raggedleft\usebeamerfont{subtitle}\textcolor{black}{\insertsubtitle}}};
\end{tikzpicture}
}

\author{John Doe}
\institute{Harvard College Professor and\\ Johnstone Family Professor in Psychology, Harvard University}
\title{Say What?}
\subtitle{Linguistics as a Window for Understanding the Brain}
\titlegraphic{\includegraphics[width=2cm]{ctanlion}}

\begin{document}
\begin{frame}
\maketitle
\end{frame}

\end{document}
```

## 在确定的位置摆放图片

`latex`中在确定的位置放置对象, 诀窍是使用图片环境, 然后给对象提供坐标. 可以使用`LaTeX2e`自带的`picture`环境, 也可以使用专门的`tikz`包提供的`tikzpicture`环境. 参考下面的链接:

[How to position images in Beamer absolutely](https://bryanwweber.com/writing/personal/2014/09/02/how-to-position-images-in-beamer-absolutely/)
[Precise positioning in LaTeX beamer](https://blogs.helsinki.fi/smsiltan/2012/10/12/precise-positioning-in-latex-beamer/)
[lshort Page89](https://mirrors.sjtug.sjtu.edu.cn/ctan/info/lshort/english/lshort.pdf)

### picture 环境

基于对LaTeX `picture` 环境的巧妙使用, 将每张幻灯片变成一幅大图, 在其中可以使用坐标来放置公式, 文本, 图像或视频.

举个例子. 使用的图像文件在[blogs.helsinki.fi](https://blogs.helsinki.fi/smsiltan/?p=107).
![img](https://blogs.helsinki.fi/smsiltan/files/2012/10/sincos21-1024x768.png)

```latex
\documentclass[graphics]{beamer}
\begin{document}
\begin{frame}{Drawing the unit disc is a good way to introduce sine and cosine functions}
\begin{picture}(320,250)
\put(-80,20){\includegraphics[height=8cm]{sincos2.png}}
\put(180,180){\begin{minipage}[t]{0.4\linewidth}
{Choose a point on the unit circle. Connect it to the origin with a line of length one, and denote the angle between that line and the horizontal coordinate axis by $\theta$.}
\end{minipage}}
\end{picture}
\end{frame}
\end{document}
```

这是生成的幻灯片:

![img](https://blogs.helsinki.fi/smsiltan/files/2012/10/slide1-1024x768.png)

现在, 我们创建两张连续的幻灯片, 并包含一些新结构.

```latex
\documentclass[graphics]{beamer}
\begin{document}
\begin{frame}{Drawing the unit disc is a good way to introduce sine and cosine functions}
\begin{picture}(320,250)
\put(-80,20){\includegraphics[height=8cm]{sincos2.png}}
\put(180,180){\begin{minipage}[t]{0.4\linewidth}
{Choose a point on the unit circle. Connect it to the origin with a line of length one, and denote the angle between that line and the horizontal coordinate axis by $\theta$.}
\end{minipage}}
\end{picture}
\end{frame}

\begin{frame}{Now sine and cosine of angle $\theta$ can be found as the $x$ and $y$ coordinates of the chosen point at the unit circle}
\begin{picture}(320,250)
\put(-80,20){\includegraphics[height=8cm]{sincos3.png}}
\put(180,180){\begin{minipage}[t]{0.4\linewidth}
{Try drawing a similar figure with larger values of $\theta$. What happens to sine and cosine when you complete a full circle? Can you see from the figure which one of the functions $\sin$ and $\cos$ is odd and which one is even?}
\end{minipage}}
\end{picture}
\end{frame}
\end{document}
```

产生的两张幻灯片如下所示:

![img](https://blogs.helsinki.fi/smsiltan/files/2012/10/slide11-1024x768.png)
![img](https://blogs.helsinki.fi/smsiltan/files/2012/10/slide2-1024x768.png)

请注意, 圆的位置没有移动. 切换幻灯片时, 彩色部分将覆盖在上一张图像上.

上述方法仍然存在一个问题. 如果某一页标题较长, 超过下一页, 则图片环境的位置将发生改变, 失去连续幻灯片切换时的平稳覆盖效果. 解决方法是在标题较短的那一张添加额外的`ghost`行:

```latex
\begin{frame}{Too short title\\ \phantom{m}}
```

***
参考`lshort`, `picture`环境的语法如下:

```latex
\begin{picture}(width,height)...\end{picture}
%% 或者是
\begin{picture}(width,height)(x0;y0)...\end{picture}
```

`x,y,x0,y0`的单位是`\unitlength`, 默认是`1pt`. 可以随时使用命令重置, 比如`\setlength{\unitlength}{1.2cm}`, 但要在`picture`环境之外.
前一组坐标`(width,height)`指定矩形的大小, 后一组`(x0;y0)`指定矩阵左下角, 即锚点的位置.

大部分画图指定的形式为

```latex
\put(x;y){object}
%% 或者
\multiput(x;y)(∆x;∆y){n}{object}
```

一个简单的例子:

```latex
\setlength{\unitlength}{5cm}
\begin{picture}(1,1)
\put(0,0){\line(0,1){1}}
\put(0,0){\line(1,0){1}}
\put(0,0){\line(1,1){1}}
\put(0,0){\line(1,2){.5}}
\end{picture}
```

### tikz 环境

如果比较复杂的情况, 使用`tikz`会更简单, 且有更多的功能.

```latex
\tikz[remember picture, overlay] \node[anchor=center] at (current page.center) {\includegraphics{foo}};
```

编译两次得到输出, 图片恰好放置在幻灯片的中心.  可以更改锚点以移动图片, 并且可以使用`calc`库进行进一步的调整.

```latex
% 导言区
\usepackage{tikz}
\usetikzlibrary{calc}
% 主文档
\tikz[remember picture, overlay] \node[anchor=center] at ($(current page.center)-(1,0)$) {\includegraphics{foo}};
```

图像将被放置在中心左侧`1`厘米处.

***
参考[beamer中任意摆放图片的方法](http://softlab.sdut.edu.cn/blog/subaochen/2017/05/beamer%e4%b8%ad%e4%bb%bb%e6%84%8f%e6%91%86%e6%94%be%e5%9b%be%e7%89%87%e7%9a%84%e6%96%b9%e6%b3%95/). 也可以使用下面的语法:

```latex
\begin{tikzpicture}[remember picture,overlay]
    \node<1->[xshift=-3cm,yshift=-1cm] at (current page.center) {\includegraphics[height=3cm]{fig}};
    \node<2->[xshift=0cm,yshift=0cm] at (current page.center) {\includegraphics[height=3cm]{fig}};
    \node<3->[xshift=3cm,yshift=1cm] at (current page.center) {\includegraphics[height=3cm]{fig}};
\end{tikzpicture}
```

`texdoc tikz` 文档 p.238 有关于位置库的说明.  常用的位置有:

```latex
center, north west, west, north, base, north east, east
```
