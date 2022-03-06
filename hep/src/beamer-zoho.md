# Beamer学习笔记

整理自: [Beamer演示学习笔记](https://bbs.pku.edu.cn/attach/cb/40/cb401e254626b3f9/beamerlog-1112.pdf)

## 中文文档

如果要使用中文, 可以用 `ctex` 宏包, 例如:

```latex
\documentclass{beamer}
\usepacakge[UTF8]{ctex}
\begin{document}
\begin{frame}
你好 Beamer!
\end{frame}
\end{document}
```

对于中文文档, 建议用 `UTF8` 编码, 然后用 `xelatex` 程序编译. 
另外, 可以在载入 ctex 宏包时加上 `noindent` 选项以取消段落的缩进.

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

## 标题页面

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

## 区块环境

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

## 整体主题

`Beamer` 的整体主题包含了`结构`, `颜色`, `字体`各方面的设置. 我们可以用命令 `\usebeamertheme{主题名}` 来选择整体主题.
其中主题名有如下这些选择:

+ 无导航栏 ; `default, boxes, Bergen, Pittsburgh 和 Rochester.
+ 带顶栏 Antibes, Darmstadt, Frankfurt, JuanLesPins, Montpellier 和 Singapore.
+ 带底栏 Boadilla 和 Madrid.
+ 带顶栏底栏 AnnArbor, Berlin, CambridgeUS, Copenhagen, Dresden, Ilmenau, Luebeck, Malmoe, Szeged 和 Warsaw.
+ 带侧栏 Berkeley, Goettingen, Hannover, Marburg 和 PaloAlto.

### 细分主题

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

### 主题画廊

`Beamer` 的整体主题太多了, 一个个尝试太费时. 你可以在下面这些网址直观地比较这些主题(同时也包含了不同的颜色主题的搭配):

+ [beamer_gallery](http://deic.uab.es/~iblanes/beamer_gallery/)
+ [beamer-theme-matrix](http://www.hartwork.org/beamer-theme-matrix/)

`Beamer` 自带的各种主题的配色很多都不怎么好看, 不过配色可以自己定制, 虽然麻烦了点, 至少也是可以满足要求的.

## 自定义格式

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

## 在beamer 中添加计时器和logo

[在 Beamer 中添加计时器和 Logo ](https://amito.me/2019/Adding-Timer-and-Logo-in-Beamer/)

### 添加 timer 计时器

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

### 在 frame 中加入 logo

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
