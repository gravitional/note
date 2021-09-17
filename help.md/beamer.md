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
+ 请勿使用超过两个级别的`“subitemizing.”`. `beamer`支持三个级别, 但您不应使用三层. 通常, 您甚至都不应该使用第二个. 请改用优质的图形. 
+ 不要创建无尽的逐项`itemize`或`enumerate`列表. 
+ 不要逐段显示列表. 
+ 强调是创建结构的重要组成部分. 使用`\alert`突出显示重要的内容. 适用对象可以是一个单词或整个句子. 但是, 不要过度使用突出显示, 因为这会抵消效果. 如 `\alert{prime number}`
+ 使用列, 如下: 

```latex
\begin{columns}[t]      
\column
{.22\textwidth}
%\pause
\column{.65\textwidth}
\end{columns}
 ```

+ 切勿使用脚注(footnotes.). 他们不必要地打乱了阅读流程. 脚注中所说的如果重要, 应放在普通文本中; 或不重要, 应将其省略(尤其是演示文稿中). 
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

任何一个`+`号的出现都可以在圆括号中跟上一个偏移量。这个偏移量将被添加到`beamerpauses` 的值。
因此，如果`beamerpauses`是`2`，那么`<+(1)->`扩展为`<3->`，`<+(-1)-+>`扩展为`<1-2>`. 比如说

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
[如何使用 LaTeX/XeLaTeX 编辑中文？](https://zhuanlan.zhihu.com/p/27739925)
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

那么问题来了, 如何找到可用的中文字体呢？如果你已经安装了 `TeX Live`, 那么很容易找到中文字体. 在系统的命令行, 使用下面的命令: 

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
    你好！
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

基于对LaTeX`picture`环境的巧妙使用, 将每张幻灯片变成一幅大图, 在其中可以使用坐标来放置公式, 文本, 图像或视频. 

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

## Beamer学习笔记

整理自: [Beamer演示学习笔记](https://bbs.pku.edu.cn/attach/cb/40/cb401e254626b3f9/beamerlog-1112.pdf)

中文文档

如果要使用中文，可以用 `ctex` 宏包，例如：

```latex
\documentclass{beamer}
\usepacakge[UTF8]{ctex}
\begin{document}
\begin{frame}
你好 Beamer！
\end{frame}
\end{document}
```

对于中文文档，建议用 `UTF8` 编码，然后用 `xelatex` 程序编译。另外，可以在载入 ctex 宏包时加上 `noindent` 选项以取消段落的缩进。

在每张幻灯片中, 可以添加`标题`和`副标题`, 例如: 

```latex
\begin{frame}{幻灯片标题}{我是一个副标题}
Hello Beamer!
\end{frame}
% 或者也可以分开来写, 如下: 
\begin{frame}
\frametitle{幻灯片标题}
\framesubtitle{我是一个副标题}
Hello Beamer!
\end{frame}
```

在 `Beamer` 的每张幻灯片中, 正文内容(不包括幻灯片标题)默认都是`竖直居中`的. 
也许你就喜欢正文竖直居上, 只要在文档类中加上`t` 选项就可以了, 如下: 

```latex
\documentclass[t]{beamer}
% 如果你只需要让某张幻灯片的正文内容竖直居上, 居中或者居下, 可以在 frame 环境中分别加上 t, c 或者 b 选项. 
\begin{frame}[b]
Hello Beamer from the Bottom!
\end{frame}
```

### 标题页面

在幻灯片中用 `\titlepage` 命令可以生成标题页, 一般这是第一张幻灯片. 例如: 

```latex
\title{Beamer演示学习笔记}
\author{zoho@bbs.ctex.org}
\date{2011年12月6日}
\begin{frame}[plain]
\titlepage
\end{frame}
```

其中的 `plain` 选项表示不显示顶栏侧栏底栏等外部元素. 

在 `Beamer` 文档中, 可以用 `\part`, `\section`, `\subsection` 等结构命令, 但是不能用 `\chapter`. 
注意这些结构命令必须放置在各个 `frame` 环境之间, 放在里面会有负面效果. 

类似于标题页面, 我们可以在幻灯片中用 `\tableofcontents` 命令生成目录页. 例如: 

```latex
\begin{frame}
\tableofcontents[hideallsubsections]
\end{frame}
```

其中 `hideallsubsections` 选项表示不显示小节标题.

在 `Beamer` 中可以如常使用列表环境, 例如: 

```latex
\begin{enumerate}
\item 我是第一项
\item 我是第二项
\item 我是第三项
\end{enumerate}
% 无序列表环境:
\begin{itemize}
\item 红色 -- red
\item 绿色 -- green
\item 蓝色 -- blue
\end{itemize}
% 描述列表环境, 例如: 
\begin{description}
\item[红色] 热情, 活泼, 温暖, 幸福
\item[绿色] 新鲜, 平静, 安逸, 柔和
\item[蓝色] 深远, 永恒, 沉静, 寒冷
\end{description}
```

### 区块环境

Beamer 里面定义了一个区块环境, 可以用于显示重要的内容. 
例如下面的代码

```latex
\begin{block}{重要内容}
2012年12月21日是世界末日. 
\end{block}
%提醒环境:
\begin{alertblock}{重要提醒}
2012年12月21日是世界末日. 
\end{alertblock}
% 再来看看例子环境:
\begin{exampleblock}{重要例子}
2012年12月21日是世界末日. 
\end{exampleblock}
```

`Beamer` 中也定义了各种定理环境, 而且默认是用区块环境的样式来显示的:

```latex
\begin{theorem}
微积分基本公式: $\int_a^b f(x)\mathrm{d}x=F(b)-F(a)$. 
\end{theorem}
```

各种可用的定理类环境有这些: `theorem`, `corollary`, `definition`, `definitions`, `fact`, `example` 和 `examples`. 

在 Beamer 中定理名默认是英文显示的, 如果要改为中文显示, 可以在文档开头用类似下面的代码: 

```latex
\documentclass[notheorems]{beamer}
\usepackage[UTF8,noindent]{ctex}
\newtheorem{theorem}{定理}
\newtheorem{example}[theorem]{例子}
\newtheorem*{theorem*}{定理}
\newtheorem*{example*}{例子}
```

其中的 `notheorems` 选项表示不使用默认的定理类环境. `Beamer` 中也定义了证明环境:

```latex
\begin{proof}
令 $g(x)=e^x-x-1$. 则当 $x>1$ 时,  有 $g'(x)=e^x-1>0$, 
因此 $g(x)>g(1)=0$. 即有 $x>1$ 时 $e^x>1+x$. 
\end{proof}
% 类似于定理类环境, 证明环境默认用英文的Proof. 下面的代码将它改为中文的`证明'
\renewcommand{\proofname}{证明}
```

### 整体主题

`Beamer` 的整体主题包含了`结构`, `颜色`, `字体`各方面的设置. 我们可以用命令 `\usebeamertheme{主题名}` 来选择整体主题. 
其中主题名有如下这些选择: 

+ 无导航栏 ; `default, boxes, Bergen, Pittsburgh 和 Rochester. 
+ 带顶栏 Antibes, Darmstadt, Frankfurt, JuanLesPins, Montpellier 和 Singapore. 
+ 带底栏 Boadilla 和 Madrid. 
+ 带顶栏底栏 AnnArbor, Berlin, CambridgeUS, Copenhagen, Dresden, Ilmenau, Luebeck, Malmoe, Szeged 和 Warsaw. 
+ 带侧栏 Berkeley, Goettingen, Hannover, Marburg 和 PaloAlto. 

#### 细分主题

`Beamer` 的每个演示主题实际上都是由`外部主题`, `内部主题`, `颜色主题`和`字体主题`这四种细分主题组合而成的. 
如果要对演示主题作更加细致地选择, 可以按照下面这四种细分主题自由组合: 

1. 外部主题, 用 `\usebeameroutertheme` 命令; 
2. 内部主题, 用 `\usebeamerinnertheme` 命令; 
3. 颜色主题, 用 `\usebeamercolortheme` 命令; 
4. 字体主题, 用 `\usebeamerfonttheme` 命令. 

你可以通过这四种细分主题的选择得到一个新的整体主题. 

外部主题设定演示文稿是否有`顶栏`, `底栏`和`侧栏`, 以及它们的结构, 可以用 `\useoutertheme{主题名}` 来选择, 其中主题名有如下这些选择: 

```latex
default, infolines, miniframes
sidebar, smoothbars, split
shadow, tree, smoothtree
```

内部主题设定演示文稿正文内容(例如`标题`, `列表`, `定理`等)的样式, 可以用 `\useinnertheme{主题名}` 来选择, 其中主题名有如下这些选择: 

+ `default `
+ `circles`
+ `rectangles`
+ `rounded`

`颜色主题`设定演示文稿的各部分各结构各元素的配色, 可以用`\usecolortheme{主题名}` 来选择, 其中主题名有这些选择: 

+ 基本颜色 ; `default`, `sidebartab`, `structure`
+ 完整颜色 ; `albatross(信天翁)`, `beaver`(海狸), `beetle`(甲壳虫), `crane`(鹤), `dove`(鸽子), `fly`(苍蝇), `seagull`(海鸥), `wolverine`(狼獾)
+ 内部颜色 ; `lily(百合)`, `orchid(兰花)`, `rose(玫瑰)` 
+ 外部颜色 ; `dolphin(海豚)`, `seahorse(海马)`, `whale(鲸鱼)`

字体主题设定演示文稿的字体, 可以用 `\usefonttheme{主题名}` 命令来选择, 其中主题名有如下这些选择: 

+ `default`
+ `serif`
+ `structurebold`
+ `structureitalicserif`
+ `structuresmallcapsserif`

#### 主题画廊

`Beamer` 的整体主题太多了, 一个个尝试太费时. 你可以在下面这些网址直观地比较这些主题(同时也包含了不同的颜色主题的搭配): 

+ [beamer_gallery](http://deic.uab.es/~iblanes/beamer_gallery/)
+ [beamer-theme-matrix](http://www.hartwork.org/beamer-theme-matrix/)

`Beamer` 自带的各种主题的配色很多都不怎么好看, 不过配色可以自己定制, 虽然麻烦了点, 至少也是可以满足要求的.

### 自定义格式

`Beamer` 各部分的内容都可以自己定制和修改, 和`主题`的划分类似, 可以从如下这三个方面来定制自己的主题: 

1. 定制模板, 用 `\setbeamertemplate`
2. 定制颜色, 用 `\setbeamercolor`
3. 定制字体, 用 `\setbeamerfont`

+ 定制背景色, 下面的代码修改了演示文稿的渐变背景颜色: 

```latex
\definecolor{bottomcolor}{rgb}{0.32,0.3,0.38}
\definecolor{middlecolor}{rgb}{0.08,0.08,0.16}
\setbeamertemplate{background canvas}[vertical shading]
[bottom=bottomcolor, middle=middlecolor, top=black]
```

其中 `\definecolor` 命令设定了两种颜色, 而最后一行设定背景颜色在底部, 中部和顶部这三种颜色中渐变. 

+ 定制标题页, 下面的代码修改了文档标题的字体和颜色: 

```latex
\setbeamerfont{title}{size=\LARGE}
\setbeamercolor{title}{fg=yellow,bg=gray}
```

其中 `fg` 和 `bg` 分别表示文字颜色和背景颜色, 缺省就使用默认颜色. 

+ 定制目录页, 下面的代码修改了目录页中节标题的模板和颜色: 

```latex
\setbeamertemplate{section in toc}[sections numbered]
\setbeamercolor{section in toc}{fg=yellow!80!gray}
```

其中第一行设定显示节`标题的编号`, 第二行将`节标题`的颜色设为`yellow!80!gray`(表示 `80%` 黄色和 `20%` 灰色的混合). 

+ 定制幻灯片标题, 下面的代码修改了每个幻灯片的标题样式: 

```latex
\setbeamertemplate{frametitle}{
\noindent\insertframetitle\par
\noindent\insertframesubtitle\par}
\setbeamerfont{frametitle}{size=\large}
\setbeamercolor{frametitle}{fg=yellow!70!gray}
```

其中第一行的设定使得幻灯片标题和正文对齐, 看起来会更整齐点

+ 定制正文字体, 下面的代码修改了正文字体的样式: 
 
```latex
\setbeamercolor{normal text}{fg=white,bg=black}
```

其中黑底白字是看起来比较明显的一种颜色搭配.

+ 定制无序列表

无序列表项的样式可以用下面的代码来设定: 

```latex
\setbeamertemplate{itemize items}[样式名]
```

其中样式名一共有如下四种选择(`default` 和 `triangle` 一样): 

+ `default`
+ `triangle`
+ `circle`
+ `square`
+ `ball`

你可以从上面几种样式中任选一种. 

+ 有序列表项的样式可以用下面的代码来设定: 

```latex
\setbeamertemplate{enumerate items}[样式名]
```

其中样式名一共有如下四种选择: 

+ `default`
+ `circle`
+ `square`
+ `ball`

+ 定制区块环境, 下面的代码修改了区块环境的样式: 

```latex
\setbeamertemplate{blocks}[rounded][shadow=true]
\setbeamercolor{block title}{fg=yellow,bg=gray!50!black}
\setbeamercolor{block body}{bg=gray}
```

其中最后一行设定区块环境用圆角带阴影的矩形来表示. 

+ 定制底栏, 下面的代码修改了导航栏和底栏的样式: 

```latex
\setbeamertemplate{navigation symbols}{}
\setbeamertemplate{footline}[frame number]
```

其中第一行设定不显示导航栏, 而第二行设定底栏只显示页码. 

+ 字号大小. `Beamer` 演示中全部可以使用的字号如下: 
+ `8pt`, `9pt`, `10pt`, `11pt`, `12pt`, `14pt`, `17pt`, `20pt`, 默认为 `11pt`. 建议在较大的场合演示时使用大号的字体, 例如: 
 
```latex
\documentclass[14pt]{beamer}
```

`Beamer` 中的设置的页面大小比较小, 仅为 `128` 毫米乘以 `96` 毫米. 
在全屏放映时 `PDF` 浏览器会自动放大字体, 因此, 同样的大小看起来要比 `article` 的情形大很多. 

+ 抄录环境

在 `Beamer` 演示中使用 `\verb` 抄录命令或者 `verbatim` 抄录环境时, 必须在该 `frame` 中加上 `fragile` 选项: 

```latex
\begin{frame}[fragile]{抄录环境}
这是一段抄录代码: \verb!\frame{hello beamer}!. 
\end{frame}
```

这个选项将导致 `Beamer` 将该 `frame` 环境的全部内容先写在一个名为 `filename.vrb` 的临时文件里再处理. 

### 在beamer 中添加计时器和logo

[在 Beamer 中添加计时器和 Logo ](https://amito.me/2019/Adding-Timer-and-Logo-in-Beamer/)

#### 添加 timer 计时器

`LaTeX` 中 `tdclock` 包可以提供定时功能. 

1. 在导言区加入 `tdclock` 包. 可以设置一次提醒, 二次提醒时间, 更新时间间隔等等. 

    ```latex
    \usepackage[timeinterval=2.0, timeduration=2.0, timedeath=0,
                                fillcolorwarningsecond=white!60!yellow,timewarningfirst=900,timewarningsecond=1080]{tdclock}
    ```

1. `\initclock` 初始化. 在适当的位置, 如 `titlepage` 中加入这一命令. 

    ```latex
    \begin{frame}
    \titlepage
    \initclock
    \end{frame}
    ```

1. 在要显示的地方加入 `\crono` 命令, 比如在 `footer` 中. 如果使用的三段式脚注结构, 可以在脚注中显示时间: 

    ```latex
    \begin{frame}
    \titlepage
    \initclock
    \end{frame}
    \date{\today \crono}
    ```

`tdclock` 与 `xelatex` 一起使用会有些小问题, 会显示不完全,  [这里提供](https://tex.stackexchange.com/questions/219415/time-clock-at-the-footline-of-a-beamer-slide-is-not-adjusted-in-the-middle-if-co/407730#407730)了一种解决方法. 

#### 在 frame 中加入 logo

+ 在标题中加入 `logo`: 在 `\begin{document}` 之前重新定义 `frametitle`, 这会在 `frametitle` 最右端显示一个 `logo`:

    ```latex
    \setbeamertemplate{frametitle}{
        \begin{beamercolorbox}[wd=\paperwidth]{frametitle}
        \strut\hspace{0.5em}\insertframetitle\strut \hfill\raisebox{-2mm}{\includegraphics[width=1cm]{$logo$}}
        \end{beamercolorbox}
    }
    ```

+ 只为 `titlepage` 页面, 定义新的 `logo`, 而不是在 `title` 中添加 `logo`. 

    ```latex
    {\setbeamertemplate{logo}{}
    \titlegraphic{\includegraphics[height=1.8cm]{图片的路径}\hspace{1em}
    \includegraphics[height=1.8cm]{图片的路径}}
    \begin{frame}
    \titlepage
    \initclock
    \end{frame}
    }
    ```

+ 在页面右下角加入 `logo`: 有时我们会想在其它地方加上 `logo`,  比如页面右下角,  这时一般直接使用 `\logo` 命令即可. 

    ```latex
    \logo{\includegraphics[height=1cm]{logo的路径}
    ```

    如果想要调整位置, 可以使用 `pgf` 命令,  但坐标位置需要仔细调整. 

    ```latex
    \logo{\pgfputat{\pgfxy(-9,9)}{\pgfbox[center,base]{\includegraphics[width=1.5cm]{$logo$}}}}
    ```
