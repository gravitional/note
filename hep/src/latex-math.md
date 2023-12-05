# latex 数学相关

## 数学符号

[16.2 Math symbols](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#Math-symbols)

LaTeX几乎提供了 任何人 使用的 任何 数学或技术符号.
例如, 如果你在你的源文件中包括 `$pi$`, 你将得到符号 $\pi$ .
见 ["综合LaTeX符号列表"包 ](https://ctan.org/pkg/comprehensive).

这里有常用符号的列表, 它决不是详尽的.
每个符号都有简短的描述, 它的符号类别, 在括号里给出, 类别决定了它周围的间距.
除非另有说明, 这些符号的命令只能在数学模式下使用.
要重新定义一个命令, 使其可以在任何当前 mode 下使用, 请参见 `\ensuremath`.

+ `\Prime`
`Prime`, 或时间表达式中的分(minute), (ordinary).
通常作为上标使用. 例如`$f^\prime$`;
`$f^\prime$` 和 `$f'$` 产生相同的结果,
第二种方法的优点是 `$f'''$`能产生所需的符号, 即与 `$f^{prime\prime\prime}$` 的结果相同, 但使用的打字量比较少.
你只能在数学模式下使用 `\prime`, 在文本模式下使用右单引号 `'` 会产生不同的字符(撇号, apostrophe).

## 方程排版

+ 子方程

    ```latex
    \begin{subequations}
    ```

创建 子方程 环境

+ 二元运算符
    + `+` 号后面 加 `{}` , 变成二元运算符,强制排版,用在多行公式换行中
    + `=` 号也是同理

+ 数学模式中的空格

    ```latex
    /,  /:  /;  /quad   /qquad
    ```

+ Texstudio 的 占位符号Placeholders
使用占位符: 如果完成的命令具有需要填写的选项,则将`占位符`放在此位置,
并可以通过使用`Ctrl + Right`/`Ctrl + Left`跳转到它们

## align环境如何对齐

多&情况下flalign和align环境是如何对齐的:
[对齐@CSDN](https://blog.csdn.net/yanxiangtianji/article/details/54767265)

根据 `&`(假设`n`个)一行被分为`n+1`列. 从左向右将列两个分为一组,第一组紧靠页左侧,最后一组紧靠页左侧,其余组均匀散布在整个行中. 当公式比较短时,中间可能会有几段空白.
需要注意的是:
每一组内部也是有对齐结构的!它们在所在位置上向中间对齐的,即第一列向右对齐,第二列向左对齐.
所谓紧靠页左/右是在进行了组内对齐调整之后,最长的一块紧靠上去. 也就是说对于长度不一两行,较短的那一行是靠不上去的.
如果总共有奇数个列,及最后一组只有一个列,则它右对齐到页右侧,即所有行的最后一列的右侧都靠在页右侧.

## 数学符号

[RaySir](https://www.zhihu.com/people/a739643d07dc71b56c03cec1e1942358)

连字符(Hyphens), 连接号(En-dashes), 破折号(Em-dashes), 减号(Minus signs)

连字符为`-`, 连接号为`--`, 破折号为`---`, 减号为`$-$`.

u+2014*2

+ `hyphen`,用于连接复合词,比如 pesudo-vector,TeX 里面用`-`
+ `en dash`,大致相当于中文的连接号,可连接人名, 时间, 地点等,如 Newton–Leibniz formula, 10–20,TeX 里面用`--`
+ `em dash`,大致相当于中文的破折号,TeX 里面用`---` (即三个 hyphen)

```bash
\DeclareMathOperator{\tr}{Tr}
\DeclareMathOperator{\re}{Re}
\DeclareMathOperator{\im}{Im}
\newcommand*{\dif}{\mathop{}\!\mathrm{d}}
```

## 无序号公式

```latex
\begin{equation*}\begin{aligned}
%%\label{eq.6.1.2}
%%%%%+++++++++++++++++++++++---------------------

%%%%%+++++++++++++++++++++++
\end{aligned}\end{equation*}
```

## 有序号公式

```latex
\begin{equation}\begin{aligned}
%%\label{eq.6.1.2}
%%%%%+++++++++++++++++++++++---------------------

%%%%%+++++++++++++++++++++++
\end{aligned}\end{equation}
```

## 分段函数 cases

```latex
\begin{equation}
%%\label{eq.6.1.2}
%%%%%+++++++++++++++++++++++---------------------
y=
%%%%%+++++++++++++++++++++++
\begin{cases}
-x,\quad x\leq 0 \\
%%%%%+++++++++++++++++++++++
x,\quad x>0
\end{cases}
\end{equation}
```

```latex
f(n) = \begin{cases} n/2 &\mbox{if } n \equiv 0 \\
(3n +1)/2 & \mbox{if } n \equiv 1 \end{cases} \pmod{2}.
```

## 矩阵模板

ref amsdoc_4.1_矩阵

```latex
\begin{equation}\begin{aligned}
%%\label{eq.6.1.2}
%%%%%+++++++++++++++++++++++---------------------
\begin{pmatrix}

\end{pmatrix}
%%%%%+++++++++++++++++++++++
\end{aligned}\end{equation}
```

除了 LaTeX 的基本 array 环境外, amsmath 软件包还为矩阵提供了一些环境.
`pmatrix`,`bmatrix`,`Bmatrix`,`vmatrix`和`Vmatrix`
分别具有`()`,`[]`,`{}`,`|`,`||`分隔符.
为了命名一致性, 还有一个`matrix`环境, 没有分隔符.

对于`array`环境,这并不是完全多余的. `matrix`环境比`array`环境的水平间距更经济.
另外, 与 `array` 环境不同, 您不必为任何`matrix`环境提供`column specifications`;
默认情况下,您最多可以有`10`个居中的列. (如果需要以一列或其他特殊格式左对齐或右对齐,则必须诉诸`array`.)

为了产生适用于文本的小矩阵,需要有一个`smallmatrix`环境,它比普通矩阵更适合于单个文本行.
必须提供定界符; 没有`p`,`b`,`B`,`v`,`V`版本的`smallmatrix`.
上面的例子可以这些生成

```latex
\bigl( \begin{smallmatrix}
a&b\\ c&d
\end{smallmatrix} \bigr)
```

## 分隔符

```latex
%%%%%+++++++++++++++++++++++---------------------
```

## 常用颜色声明

```latex
{\color{main}text}
```

main, second, third

***
xcolor 使用

需求:`xcolor`默认颜色只有`19`种,使用时可以在`option`中加入另外3张颜色表来极大扩充颜色库.

宏包:`\usepackage{xcolor}`
选项:`dvipsnames, svgnames, x11names`
使用:`\usepackage[dvipsnames, svgnames, x11names]{xcolor}`
注意:`xcolor`宏包一般要放在最前面!否则那3张颜色表容易加不进来.

使用:

```latex
{\color{red}{红色}}是19个基本颜色中的一个,下面秀几个高级货:
这里是{\color{NavyBlue}{海军蓝}},这个是{\color{Peach}{桃子色}}
这个是{\color{SpringGreen}{春天绿}},最后一个{\color{SeaGreen3}{海绿3}}
```

[LaTeX:xcolor颜色介绍](https://www.jianshu.com/p/5aee7c366369)

## 颜色包的使用

```latex
\usepackage{color,xcolor}
% predefined color---black, blue, brown, cyan, darkgray, gray, green, lightgray, lime, magenta, olive, orange, pink, purple, red, teal, violet, white, yellow.
\definecolor{light-gray}{gray}{0.95}    % 1.灰度
\definecolor{orange}{rgb}{1,0.5,0}      % 2.rgb
\definecolor{orange}{RGB}{255,127,0}    % 3.RGB
\definecolor{orange}{HTML}{FF7F00}      % 4.HTML
\definecolor{orange}{cmyk}{0,0.5,1,0}   % 5.cmyk
```

## 简单枚举

```latex
$(\lambda)=(\lambda_1,\lambda_2,\cdots \lambda_m)$
```

## 杨图 diagrams 模板

[ytableau – Many-featured Young tableaux and Young diagrams](https://www.ctan.org/pkg/ytableau)

该软件包提供了一些绘制 Young tableaux 和 Young diagrams 的功能, 扩展了 `young` 和 `youngtab` 软件包,但提供了更多功能.
倾斜和彩色表格很容易, and pgfkeys-enabled options are provided both at package load and configurably.

```latex
\begin{equation}\begin{aligned}
%%\label{eq.6.1.2}
%%%%%+++++++++++++++++++++++---------------------
\ydiagram{4}&\quad&\ydiagram{3,1}&\quad&\ydiagram{2,2}\\
[4]&\quad&[3,1]&\quad&[2,2]
%%%%%+++++++++++++++++++++++
\end{aligned}\end{equation}
```

## 公式编号

数学公式的环境中,除了 `split` 环境,每个方程环境都有带`*`和不带`*`号的版本, 不带星号的版本将会自动编号,在一行结束之前,使用`\notag`可以抑制编号.
为了避免弄乱编号,`\notag`应该只在`display`类型的环境内使用. 可以使用`\tag{<lable>}`使用自定义编号,tag 可以引用 a different tagged display,
通过使用`\tag{\ref{<label>}<modifier>}`,其中`<modifier>`是可选的.如果你使用了`hyperref`,可以使用`\ref*`,避免创建包含内置链接的reference.

还有一个`\tag*`命令,可以原义输出文本,不加括号.`\tag` and `\tag*`也可以在`amsmath`包的无编号环境中使用.

## 杨表 tableaux  模板

[ytableau – Many-featured Young tableaux and Young diagrams](https://www.ctan.org/pkg/ytableau)

```latex
\ytableausetup{mathmode, boxsize=1.5em}
\begin{equation}\begin{aligned}
%%\label{eq.6.1.2}
%%%%%+++++++++++++++++++++++---------------------
\begin{ytableau}
a & d & f \\
b & e & g \\
c
\end{ytableau}%%&\quad&
%%%%%+++++++++++++++++++++++
\end{aligned}\end{equation}
```

最后一行不需要 `\\`

```latex
\ytableausetup{mathmode, boxsize=2.5em}
\begin{ytableau}
n & n+1 & n+2 \\
n-1 & n  \\
n-2 \\
n-3 %% last line needn't the \\
\end{ytableau}
```

## 标题中使用数学模式

```latex
\section{The classes \texorpdfstring{$\mathcal{L}(\gamma)$}{Lg}}
```

```latex
 \texorpdfstring{math objec}{Lg}
```

## Math accents

reference: [Math accents](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#Math-accents)

`LaTeX` provides a variety of commands for producing accented letters in math.
These are different from `accents` in `normal text` (see `Accents`).

| command      | description                  |
| ------------ | ---------------------------- |
| `\acute`     | Math acute accent            |
| `\bar`       | Math bar-over accent         |
| `\breve`     | Math breve accent            |
| `\check`     | Math hacek (check) accent    |
| `\ddot`      | Math dieresis accent         |
| `\dot`       | Math dot accent              |
| `\grave`     | Math grave accent            |
| `\hat`       | Math hat (circumflex) accent |
| `\mathring`  | Math ring accent             |
| `\tilde`     | Math tilde accent            |
| `\vec`       | Math vector symbol           |
| `\widehat`   | Math wide hat accent         |
| `\widetilde` | Math wide tilde accent       |

## 简单的规则

1. 空格:Latex 中空格不起作用.
1. 换行:用控制命令`\\\`,或`\newline`.
1. 分段:用控制命令`\par` 或空出一行.
1. 特殊控制字符: #,$, %, &, - ,{, }, ^, ~

## 换页

用控制命令`\newpage`或`\clearpage`

+ `\newpage`:  The `\newpage` 结束当前页.
+ `\clearpage`:The `\clearpage` 结束当前页面,并且强迫排版到目前为止`input`中的`图`和`表格`的浮动体(float).

## 连字符

连字符(`Hyphens`), 连接号(`En-dashes`), 破折号(`Em-dashes`)及减号(`Minus signs`)

+ 连字符 `-` 通常用来连接复合词,比如 `daughter-in-law`.
+ 连接号 `--` 通常用来表示范围,比如 `see pages 5--7`. 如果真的希望连续输入两个连字符,使用 `{-}{-}`.
+ 破折号 `---` 是一个正规的标点符号,用来表示转折或者承上启下. 要注意的是,破折号与其前后的单词之间不应该存在空格,例如 `A specter is haunting Europe---the specter of Communism.`
+ 排版中的减号应该比连字符要长,因此用来表示减号或者负号时,请严格使用数学模式 $-5$ 而不要使用文字模式 `-5`

## 计数器 Counters

所有的` LaTeX`自动编号都有一个`counter`与之关联.
`counter`的名称通常与与数字关联的`environment`或`command`的名称相同,只是`counter`的名称没有反斜杠` \`.
因此,与` \chapter`命令相关联的是` chapter`counter,该counter跟踪` chapter`编号.

以下是在` LaTeX`的标准文档类中用于控制编号的`counter`列表.

|               |              |            |         |
| ------------- | ------------ | ---------- | ------- |
| part          | paragraph    | figure     | enumi   |
| chapter       | subparagraph | table      | enumii  |
| section       | page         | footnote   | enumiii |
| subsection    | equation     | mpfootnote | enumiv  |
| subsubsection |              |            |         |

The `mpfootnote` counter is used by the `\footnote` command inside of a `minipage` (see minipage).
The counters `enumi` through `enumiv` are used in the `enumerate` environment, for up to four levels of nesting (see `enumerate`).

Counters can have any **integer** value but they are typically **positive**.

New counters are created with `\newcounter`.

+ `\alph` `\Alph` `\arabic` `\roman` `\Roman` `\fnsymbol`: Print value of a counter.
+ `\usecounter`: Use a specified counter in a list environment.
+ `\value`: Use the value of a counter in an expression.
+ `\setcounter`: Set the value of a counter.
+ `\addtocounter`: Add a quantity to a counter.
+ `\refstepcounter`: Add to a counter.
+ `\stepcounter`: Add to a counter, resetting subsidiary counters.
+ `\day` & `\month` & `\year`: Numeric date values.

## 列表环境

## enumerate

Synopsis:

```latex
\begin{enumerate}
  \item[optional label of first item] text of first item
  \item[optional label of second item] text of second item
  ...
\end{enumerate}
```

产生编号项目清单的环境.
标签编号的格式取决于此环境的嵌套级别. 见下文.
默认的顶级编号是``1.``,``2.``等.每个枚举列表环境必须至少包含一个项目;
缺少将导致` LaTeX`错误`Something's wrong--perhaps a missing \item`..

此示例给出了1908年奥运会马拉松比赛的前两名. 作为顶级列表,标签将显示为` 1.`和` 2.`.

```latex
\begin{enumerate}
 \item Johnny Hayes (USA)
 \item Charles Hefferon (RSA)
\end{enumerate}
```

用`\ item`命令开始列表项(请参阅`\ item`).
如果您给`\item`提供可选参数,通过在其后加上方括号,例如`\item[Interstitial label]`,
则下一项将继续中断的序列(请参见`\item`).

也就是说,您将获得诸如` 1`,`Interstitial label`,` 2`之类的标签. `\item`之后是可选文本,其中可能包含多个段落.

`Enumerations`可以嵌套在其他`enumerate`环境中,也可以嵌套在任何`paragraph-making`环境中,
例如` itemize`(请参阅` itemize`),深度最多为四个级别.
下面给出了` LaTeX`对每个嵌套层提供的默认格式,其中` 1`是顶层,即最外层.

+ arabic number followed by a period: `1.`, `2.`, ...
+ lowercase letter inside parentheses: `(a)`, `(b)` ...
+ lowercase roman numeral followed by a period: `i.`, `ii.`, ...
+ uppercase letter followed by a period: `A.`, `B.`, ...

The enumerate environment uses the counters `\enumi` through `\enumiv` (see `Counters`).

对于其他主要的LaTeX标签列表环境,请参阅`description`和`itemize`.
有关列表`layout parameters`(包括默认值)的信息,以及有关自定义列表布局的信息,请参见`list`.  软件包`enumitem`可用于自定义列表.

要更改标签的格式,对命令`\labelenumi`使用`\renewcommand`(请参阅`\newcommand`和`\renewcommand`).
例如,下面使第一级列表以大写字母标记,并以`boldface`显示,并且没有尾随句点.

```latex
\renewcommand{\labelenumi}{\textbf{\Alph{enumi}}}
\begin{enumerate}
  \item Shows as boldface A
  \item Shows as boldface B
\end{enumerate}
```

For a list of counter-labeling commands see `\alph \Alph \arabic \roman \Roman \fnsymbol`.

## enumitem

## itemize

Synopsis:

```latex
\begin{itemize}
  \item[optional label of first item] text of first item
  \item[optional label of second item] text of second item
\end{itemize}
```

产生一个无序的列表,有时称为项目符号列表.
环境必须至少有一个` \item`;  没有导致` LaTeX`错误`Something's wrong--perhaps a missing \item`.

这给出了两个项目列表.

```latex
\begin{itemize}
 \item Pencil and watercolor sketch by Cassandra
 \item Rice portrait
\end{itemize}
```

作为顶层列表,每个标签将以`bullet``$\textbullet$`的形式出现.标签的格式取决于嵌套级别.见下文.

用`\item`命令开始列表项(请参阅`\item`).
如果您给`\item`一个可选参数,通过在其后加上方括号(如`\item[Optional label]`),
则默认情况下它将以粗体显示并向右对齐,so it could extend into the left margin.
对于向左对齐的标签,请参见`description`环境. `\item`之后是可选文本,其中可能包含多个段落.

逐项列出的列表可以相互嵌套,最多可嵌套四个层次.
它们也可以嵌套在其他段落创建环境中,例如`enumerate`(请参阅​​`enumerate`).
`itemize`环境使用命令`\labelitemi`到`\labelitemiv`来生成默认标签
(这在命令名称的末尾使用小写罗马数字的约定来表示嵌套级别).下面是每个级别的默认标记.

+ $\textbullet$ (bullet, from \textbullet)
+ $\textdash$ (bold en-dash, from `\normalfont\bfseries\textendash`)
+ $\textasteriskcentered$ (asterisk, from `\textasteriskcentered`)
+ $\textperiodcentered$ (centered dot, rendered here as a period, from `\textperiodcentered`)

用`\renewcommand`更改标签. 例如,这使得第一级使用`diamonds`.

```latex
\renewcommand{\labelitemi}{$\diamond$}
```

`enclosing`环境的左边距与`itemize`列表的左边距之间的距离由参数`\leftmargini`到`\leftmarginvi`确定.
(请注意在命令名称的末尾使用小写罗马数字的约定,以表示嵌套级别.)
默认值为:`1`级`2.5em`(两列模式下`2em`),`2`级`2.2em`,`3`级`1.87em`和`4`级`1.7em`,较小的值表示嵌套深度更深的级.

对于其他带有LaTeX标签的主要列表环境,请参见`description`和`enumerate`.
有关列表布局参数(包括默认值)的信息,以及有关自定义列表布局的信息,请参见`list`. 软件包`enumitem`对于自定义列表很有用.

本示例极大地减少了最外面的`itemized`列表的边距空间.

```latex
\setlength{\leftmargini}{1.25em} % default 2.5em
```

特别是对于带有短`items`的列表,可能需要消除`items`之间的空间.
这是一个定义`itemize*`环境的示例,该环境在`items`之间或单个`item`内的段落之间没有多余的间距
(`\parskip`不是列表特有的,请参阅`\parindent`和`\parskip`):

```latex
\newenvironment{itemize*}%
  {\begin{itemize}%
    \setlength{\itemsep}{0pt}%
    \setlength{\parsep}{0pt}}%
    \setlength{\parskip}{0pt}}%
  {\end{itemize}}
```

## 超链接

使用宏包 `hyperref` 来制作

## email链接

```latex
\href{mailto:michaelbibby@gmail.com}{给我电邮}}
```

## URL链接

链接有颜色,显示为`OpenBSD官方网站`,链接到`http://www.openbsd.org`

```latex
\href{http://www.openbsd.org}{OpenBSD官方网站}
```

只显示`URL`

```latex
\url{http://www.openbsd.org}
```

显示URL,但是不做链接和跳转:

```latex
\nolinkurl{http://www.openbsd.org}
```

[LaTeX技巧159:如何在文中使用链接](https://www.latexstudio.net/archives/7741.html)

## 字体 font 数学符号

## 查看所有的字体

[Win10环境下LaTeX中文字体的设置](https://zhuanlan.zhihu.com/p/260989874)

```bash
fc-list -f "%{family}\n" :lang=zh
```

所有用逗号 , 隔开的短语,如 `Source Han Serif SC`,`思源宋体`,`Source Han Serif SC Medium`,`思源宋体 Medium`,都可以作为后面调用的代号,
其中后两者代表`Medium`这个粗细,而前两者实际上代表的是这个字体的默认粗细,即`Regular`,想要调用非默认粗细的话需要注意代号.

***
出现下面这种报错

```latex
! Internal error: bad native font flag in `map_char_to_glyph'
```

可能是数学字体设置有问题.

相关的包

```latex
\usepackage{unicode-math}
```

## latex 注音符号

http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#Accents

23.5 Accents

## latex 字体设置

[ctex宏包的文档](https://mirrors.hit.edu.cn/CTAN/language/chinese/ctex/ctex.pdf)
[LaTeX 特殊符号与数学字体](https://blog.csdn.net/lanchunhui/article/details/54633576)
[latex 字体设置](https://www.jianshu.com/p/68da21a1501a)

字体是由一些正交的属性决定的,通常讨论的属性为

+ 字体族(font family)
+ 字体形状(font shape)
+ 字体系列(font series)

关于字号这个属性我们一般单独作为一个字体属性进行设置.
本文不对具体的字体属性进行说明,仅通过实例来说明如何在我们的文档中分别设置中, 西文字体.

***
预定义的字体族有3种:

字体族 带参数命令 声明命令

+ 默认的罗马字体族(roman family),`\textrm{}` , `\rmfamily`
+ 无衬线字体族(sans serif family), `textsf{}`,`sffamily`
+ 打印机字体族(typewriter family),`texttt{}`,`ttfamily`

***
预定义的字体形状
字体族 带参数命令 声明命令

+ 默认的直立(upright shape,也称roman shape),`\textup`, `\upshape`
+ 意大利(italic shape),`\textit{}`,`\itshape`
+ 倾斜(slanted shape),`\textsl{}`,`\slshape`
+ 小型大写(small capitals shape),`\textsc{}`,`\scshape`

我们通常所说的"倾斜"往往是指意大利形状,因此我们在设置倾斜字体的时候往往都是指定意大利形状字体.

***
>字体形状这些概念源于英文,对于中文来说字体并没有这么复杂.
>中文中并没有倾斜字体,我们在 word 中看到的倾斜字体实际是通过对字符进行水平错切得到的伪斜体(对应的还有伪粗体是对字符多次略微错位输出得到的).
>一般情况下我们使用楷体来对应英文中的倾斜字体,用黑体来替代英文中的加粗字体.
>这一点后面的示例会进行说明.如果我们想使用伪斜体请查阅相关文档.

***
预定义的字体系列
字体族 带参数命令 声明命令

+ 默认的中等(medium),`\textmd{}`,`\mdseries`
+ 加宽加粗(bold extended),`\textbf{}`,`\bfseries`

## 编写样式表

在我们编写自己的包或者类文件时,一般我们都需要设置三个字体族使用什么样字体.
对于其他两个字体形状和字体系列,我们往往只关心罗马字体族的"倾斜"(实际为意大利)和加粗两个属性.
下面是一个常用的设置示例:

```latex
\RequirePackage{fontspec}

\setmainfont{Times New Roman} %正文罗马字体族
\setsansfont{Myriad Pro} %无衬线字体族
\setmonofont{Courier Std} %打印机字体族

\setCJKmainfont[BoldFont={方正小标宋_GBK}, ItalicFont={方正楷体_GBK}, BoldItalicFont={方正仿宋_GBK}]{方正书宋_GBK}
\setCJKsansfont{方正黑体_GBK}
\setCJKmonofont{方正中等线_GBK}
```

这里假设我们使用的文档模板为`ctexart`,这样我们可以直接使用下面三个 `CJK` 相关的命令.
第一个命令是用来加载包的,如果在我们的文档中使用的话对应的命令为`\usepackage{fontspec}`.

接下来的三行是设置英文状态下三种字体族使用的字体.
分别设置了正文罗马字体族, 无衬线字体族和打印机字体族.
英文字体一般都是成套的,当我们设置好这三种字体后,`fontspec` 宏包会自动的寻找对应的变体,无需我们关心.

最后的三行是设置中文状态下的三种字体族使用的字体.
中文各个字体之间一般都是独立的(只有少数字体由不同重量的成套字体),
因此这里我们给主要字体指定了其在加粗, 倾斜以及加粗倾斜时使用的字体.
由于正文字体及其加粗, 倾斜变体一般足以覆盖我们90%以上的文档,所以这里不再给另外两个字体族设置变体字体了.

## 更多字体

接下来介绍如何在 LaTeX 中引入更多的系统字体.

加载更多英文字体使用的命令为 `fontspec` 宏包的 `\newfontfamily<命令>[(可选项)]{<字体名>}`.
`xeCJK` 宏包,`ctex`宏包或文档类, 包括我们这里的 `ctexart `文档. 会自动调用.
对应的命令为 `\setCJKfamilyfont{<中文字体族>}[<可选项>]{字体名}`.下面是一个使用示例:

```latex
\setCJKfamilyfont{hwhp}{华文琥珀}
\newcommand{\hwhp}{\CJKfamily{hwhp}}
\newfontfamily\tempus{Tempus Sans ITC}
```

这里使用 `\newcommand` 命令将中文字体选择的命令重定义成一个更简单的形式.下面是一个使用示例:

```latex
{\hwhp 这是一段华文琥珀文字, english not work}
{\tempus this is Technic font, 中文不起作用}
```

对于字体不起作用的字符,会自动使用前面设置的正文罗马字体.

## 常用字号

[latex设置表格字体大小](https://blog.csdn.net/zzmgood/article/details/36419493)

默认的字号大小可以在我们加载标准文档时,通过指定参数来进行设置.如:`\documentclass[12pt]{report}` .
正文默认字体的选项有`10pt`(默认),`11pt`,`12pt`三种.
如果使用的为 `ctexart`,`ctexrep`,`ctexbook` 则还额外提供了 `c5size` 和 `cs4size` 两个选项.
其中 `c5size` 为默认值,表示五号字(`10.5pt`),`cs4size` 表示小四号字(`12pt`).

如果我们想要局部的修改某些字体的大小可以使用如下命令:

+ `\tiny`
+ `\scriptsize`
+ `\footnotesize`
+ `\small`
+ `\normalsize`
+ `\large`
+ `\Large`
+ `\LARGE`
+ `\huge`
+ `\Huge`
这些命令对应字体的大小和默认字体有关,具体对应关系如下:

***
Latex下 字体大小命令 比较

| size            | 10pt (default) | 11pt   | 12pt   |
| --------------- | -------------- | ------ | ------ |
| `\tiny`         | `5pt`          | `6pt`  | `6pt`  |
| `\scriptsize`   | `7pt`          | `8pt`  | `8pt`  |
| `\footnotesize` | `8pt`          | `9pt`  | `10pt` |
| `\small`        | `9pt`          | `10pt` | `11pt` |
| `\normalsize`   | `10pt`         | `11pt` | `12pt` |
| `\large`        | `12pt`         | `12pt` | `14pt` |
| `\Large`        | `14pt`         | `14pt` | `17pt` |
| `\LARGE`        | `17pt`         | `17pt` | `20pt` |
| `\huge`         | `20pt`         | `20pt` | `25pt` |
| `\Huge`         | `25pt`         | `25pt` | `25pt` |

如修改表格字体大小

```bash
\begin{table}[h]
{\small %此处写字体大小控制命令
\begin{tabular}
...
\end{tabular}
}
\end{table}
```

当然 LaTeX 中还允许更加灵活的设置字号,参考:
[Changing the font size in LaTeX](https://texblog.org/2012/08/29/changing-the-font-size-in-latex/)

## 特殊符号

[Symbol \sslash with XeLaTeX](https://tex.stackexchange.com/questions/341827/symbol-sslash-with-xelatex-and-unicode-math)

+ $\ell$ 用于和大小的$l$和数字$1$相区分
+ $\Re$
+ $\nabla$ 微分算子
+ `\sslash`:下标中表示平行分量的双斜线`//`.
+ $\circ$ : 可以用来当作角度的符号. 虽然国际单位更建议使用`siunitx`包处理.

***
`latinmodern-math.otf`中没有`\sslash`字形, 但总是可以从其他数学字体中借用缺少的字形. 例如下面的例子:

```latex
\documentclass[a4paper,11pt]{article}
\usepackage{fontspec}
\usepackage{unicode-math} %不用这个也能工作
\setmainfont[Ligatures=TeX]{CMU Serif} %需要用它来获得小型粗体大写
\setsansfont{CMU Sans Serif}
\setmonofont{CMU Typewriter Text}
\setmathfont{latinmodern-math.otf}
\setmathfont[range=\sslash]{Asana Math} %借用其他字体中的字形.
%\setmathfont[range=\sslash]{STIX Two Math}  %也可以使用这个字体中的\sslash
\begin{document}
$a \sslash b$
\end{document}
```

另一方面, 这个符号将与简单的斜线有很大的不同, 所以用标准的斜线来重新定义它可能会更好.
不过, 你必须在`unicode-math`完成工作后再进行重新定义.

```latex
\documentclass[a4paper,11pt]{article}
\usepackage{fontspec}
\usepackage{unicode-math} %不用这个也能工作
\setmainfont[Ligatures=TeX]{CMU Serif} %需要用它来获得小型粗体大写
\setsansfont{CMU Sans Serif}
\setmonofont{CMU Typewriter Text}
%\setmathfont{latinmodern-math.otf}%现在不需要
\AtBeginDocument{%
  \renewcommand\sslash{\mathbin{/\mkern-5.5mu/}}%
}
\begin{document}
$a \sslash b$
\end{document}
```

***
[mathbin](https://www.tutorialspoint.com/tex_commands/mathbin.htm)
[mkern](https://www.tutorialspoint.com/tex_commands/mkern.htm)
[AtBeginDocument]([mkern](https://tex.stackexchange.com/questions/177397/why-does-it-matter-when-atbegindocument-is-run))

+ ` \AtBeginDocument`宏: 将它的内容附加到`\@begindocumenthook`后面, 后者紧随`\begin{document}`展开.
+ `\mathbin`:给出正确的间距, 让某个对象称为二元运算符.例如`a\mathbin{\text{op}} b`
+ `\mkern`: 给出水平间距.例如, `a\mkern18mu b`

## 数学字体

+ `mathbb`; blackboard bold,黑板粗体
+ `mathcal`; calligraphy(美术字),还有普通的`cal`
+ `mathrm`; math roman
+ `mathbf`; math 粗体字, 还有`boldsymbol`, 在`amsbsy`中, 可以打出小写字母的粗体.
+ `\mathbbm`: `\usepackage{bbm}`, 黑板粗体字母,`\mathbbm{1}`

花体`\mathcal`:`L,F,D,N`

+ $\mathcal{L}$ 常用来表示损失函数
+ $\mathcal{D}$ 表示样本集
+ $\mathcal{N}$ 常用来表示高斯分布;

[为阿拉伯数字和小写字母实现类似 \mathbb 的效果 ](https://liam.page/2017/01/08/arabic-numbers-or-lowercase-letters-in-blackboard-bold-doublestroke-font/)

需要用到 `bbm` 包:

```latex
\usepackage{bbm}
```

这个包的文档里有很多字体的示范 [mathalpha](https://www.ctan.org/pkg/mathalpha)

还有其他数学字体:

+ `\mathsf`
+ `\mathtt`
+ `\mathit`

以下分别是`4`种字体形式:

1-4 行分别是:`默认`,`\mathsf`, `\mathtt`, `\mathit`.

```latex
\[
    \begin{split}
        &{f(x,y) = 3(x+y)y / (2xy-7)}\\
        &\mathsf{f(x,y) = 3(x+y)y / (2xy-7)}\\
        &\mathtt{f(x,y) = 3(x+y)y / (2xy-7)}\\
        &\mathit{f(x,y) = 3(x+y)y / (2xy-7)}
    \end{split}
\]
```

## 数学粗体细节

[LaTeX数学模式粗体-刘海洋](https://www.zhihu.com/question/25290041/answer/30422583)

LaTeX 不会把数学模式中的黑体全搞成 poor man's bold,
但用户可能会在在不清楚的情况下, 使用专门生成 poor man's bold 的命令
下面只考虑相对传统的数学字体, 也就是不使用 `unicode-math` 的情形, 后者用法比较特别,
仅用于 XeTeX 和 LuaTeX 的引擎, 你可以自己读它的手册.

### 基本机制

[LATEX 2efont selection](https://www.latex-project.org/help/documentation/fntguide.pdf)

在 `LaTeX` 中, 选择字体通常都是通过 `NFSS` 机制完成的, 数学字体也不例外. (New Font Selection Scheme)
基本的切换为数学粗体的命令是 `\boldmath`, 它通常被定义为 `\mathversion{bold}`,
意思是`全局地`把当前的数学字体设置为粗体, 并且它应该在 `数学环境外面` 使用.
与之相对的恢复正常粗细是 `\unboldmath` 即 `\mathversion{normal}`. 因此我们有:

```latex
\documentclass{article}
\begin{document}
\boldmath $a^2 + b^2 = c^2$
\end{document}
```

出现多次复制的 poor man's bold 了吗? 并没有.
和普通的公式对比就能看出, 这里不仅字母和数字被加粗了, 加号和等号也都加粗了.
这当然是最好的情况 -- 数学符号使用的实际字体确实有加粗和不加粗两种格式.

但也有一些时候, 字体是不完整的, 如果你所用的数学字体包并没有为一个符号设计粗细两种格式, 你当然不可能通过简单的命令选择到这个符号.
比如说 `积分号`, 默认的 `CM` 数学字体就没有对应的粗体形式的符号, 你直接使用 `\boldmath $\int$` 得到的就是不加粗的积分号.

对于字体不完整的情况, 最好的办法就是换一个完整的字体.
比如说使用 `txfonts` 或者说修正了的 `newtxmath` 字体包, 就会得到正确的`加粗积分号`, 如下图:

![加粗积分号](https://pic3.zhimg.com/80/d9cba5ae8369fadedcd1a4e90087bfc0_720w.jpg?source=1940ef5c)

而如果不更换字体还一定要加粗, 就可能需要使用后面提及的 `伪粗体`(poor man's bold)技术了.

### 加粗个别符号的机制

更多的时候我们只希望加粗个别符号. 这当然还是一个字体选择的问题.
LaTeX 并不允许在数学公式内部直接设置 `\mathversion`, 即在公式内使用 `\boldmath` 是无效的.
为此, `amsmath` 宏包(更确切地说是子包 `amsbsy`)提供了 `\boldsymbol` 命令,
使用 `\boldsymbol{符号}` 的效果大约就是临时切换到文本模式,
设置 `\boldmath`, 然后里面嵌套一个小数学公式, 输出加粗符号.
除此以外宏包也用一些代码处理了数学间距和大小的变化情况.
最后用起来的效果就是只加粗了参数里面的符号. 如:

```latex
\documentclass{article}
\usepackage{amsmath}
\begin{document}
\[
a+b\times c \ne \boldsymbol{a+b \times c}
\]
\end{document}
```

那么如果没有粗体形式呢? `amsmath` 还提供了 `\pmb` 命令来提供伪粗体(poor man's bold)的符号.
伪粗体的表现形式就是连续输出三个的相同符号, 三个符号相互重叠而略有错位, 形式伪粗体的效果. 如图所示:

![伪粗体](https://pic1.zhimg.com/80/2cbc0ef3075f73b31b8bd8d042f5228e_720w.jpg?source=1940ef5c)

在有可用的粗体字体时(比如上图中的字母 `k`), 伪粗体是应该尽量避免的;
而对于缺少粗体形式的符号(如上图的积分号), 如果没有可用的字体, 那么也不失为一种备用选择.

### 推荐的宏包--bm

与 `amsbsy` 功能类似, 更为强大的, 是专门处理数学粗体的 `bm` 宏包.

`bm` 宏包提供的主要命令是 `\bm`, 它和 `amsmath` 的 `\boldsymbol` 一样, 把参数中的数学符号用粗体输出.
不同的是, 在符号有粗体形式的字体可用时, 它的功能与 `\boldsymbol` 大致相同, 会选择对应的粗体输出;
而在符号没有粗体形式的字体可用时, 它的功能又与 `\pmb` 一样, 会采用伪粗体输出.
除此而外, `bm` 宏包也在参数嵌套, 数学字母字体切换等方面相比 `amsbsy` 处理得更为细致.
例如下例中, 数学字母是正常粗体, 积分号是伪粗体:

```latex
\documentclass{article}
\usepackage{bm}
\begin{document}
\[
\int k\,\mathrm{d}x \ne
\bm{\int k\,\mathrm{d}x}
\]
\end{document}
```

![bm 包](https://pic2.zhimg.com/80/35c50d58b71343ae651b696191b43fa8_720w.jpg?source=1940ef5c)

### 选用高质量的字体

在高质量的排版输出中, 应该避免使用伪粗体. 而要避免伪粗体, 没有其他路子可走, 就是选用 `字重齐全` 的高质量数学字体.
TeX 系统自带的 `CM` 字体缺少部分粗体符号, 但可以使用

+ 符号更为齐全的 `newtxmath`(`Times` 风格, 配合 `newtxtext` 使用, 前身是 `txfonts`),
+ `stix`(`Times` 风格, `STIX` 系列字体以大而全著称),
+ `pxfonts`(`Palatino` 风格),
+ `mathdesign`(本身只包含符号, 有不同选项用来配合 `Utopia`, `Garamond`, `Charter` 字体),
+ `MnSymbol`(只包含符号),
+ `fdsymbol`(只包含符号),
+ `lucidabr`(`Lucida Bright` 商业字体)等字体包.

一些专业的商业字体, 如 `MathTime Pro` 的两个版本, 甚至有 `normal`, `bold`, `heavy` 三种粗细可以使用
(`bm` 宏包为最后一种粗细提供了 `\hm` 命令). 在排版时可以优先选择这类字体.
下面是 `newtxmath` 的效果:

```latex
\documentclass{article}
\usepackage{newtxtext,newtxmath}
\usepackage{bm}
\begin{document}
\[
\sum \int (k\oplus j) \,\mathrm{d}x \ne
\bm{\sum \int (k\oplus j) \,\mathrm{d}x}
\]
\end{document}
```

![newtxmath](https://pic1.zhimg.com/80/4f3a74e377c25479cc8b47fc86cf0d02_720w.jpg?source=1940ef5c)

当然, 即使不使用这类拥有全套粗体形式的数学字体(毕竟加粗的求和, 积分号并不常用),
也应该尽量选择支持较多符号的数学字体(考虑拉丁字母, 希腊字母, 常用运算符, AMS 运算符等).

### 反例: 黑体多次复制的糟糕效果

最后我们举一个符号较少, 质量较差的数学字体作为反例来完结这段讨论.
`PSNFSS` 下属的 `mathptmx` 字体包, 是一个在各种文档模板中十分常见的数学字体包.
事实上, `mathptmx` 包并没有自己对应的任何实际字体, 所有数学字母和符号分别来自
`Times Roman` 的正文字母, `rsfs` 包的花体字母, `Adobe Symbol` 字体中的数学符号, 剩下的数学符号由默认的 `CM` 补全.
更糟糕的是, `mathptmx` 根本没有任何粗体形式的符号.
在 `mathptmx` 包中, `\boldmath` 命令被直接重定义为发出一个缺字体的警告:

```latex
\def\boldmath{%
   \PackageWarning%
   {mathptmx}%
   {There are no bold math fonts}%
   \global\let\boldmath=\relax
}
```

因此, 如果你使用了 `mathptmx` 这样不包含任何粗体形式的低质量的字体包,
而又采用 `bm` 包的 `\bm` 命令来产生粗体, 那么很自然, 所有加粗的字符就都被作为伪粗体输出了.
仍沿用前面 `newtxmath` 字体包的例子:

```latex
\documentclass{article}
\usepackage{mathptmx}
\usepackage{bm}
\begin{document}
\[
\sum \int (k\oplus \pi) \,\mathrm{d}x \ne
\bm{\sum \int (k\oplus \pi) \,\mathrm{d}x}
\]
\end{document}
```

效果非常之吓人:

![mathptmx](https://pic1.zhimg.com/80/f7f005c59b04d5713c55e9cba71b1a46_720w.jpg?source=1940ef5c)

## 小写字母的数字花体

ref-2: [LaTeX小写花体字母](https://www.zhihu.com/question/26941177/answer/34623570)
ref-3: [查找任意符号的LaTeX指令](https://www.zhihu.com/question/26941177/answer/34626024)

拥有小写字母的数字花体字体当然也是存在的 -- 比如`MathTime Pro 2`的`\mathcal`就支持小写,
需要付钱.  把一些正文字体借用于数学公式中是可能
把一些正文字体借用于数学公式中是可能的. 不过不推荐.
如果是为了区分不同的变量,可以考虑用`black letter`风格的`\mathfrak`.

推荐两个查找任意符号的LaTeX指令的方法

+ 查阅 [The Comprehensive LaTeX Symbol List](http://mirrors.ustc.edu.cn/CTAN/info/symbols/comprehensive/symbols-a4.pdf) ,这份资料很好,囊括众多宏包中的符号,就是查起来比较麻烦
+ 使用 [Detexify](http://detexify.kirelabs.org/classify.html) ,用鼠标直接画出你想要的符号即可查出该符号的`LaTeX`指令,灰常给力.

## 数学符号

| `latex`            | appearance           | 描述                                                        |
| ------------------ | -------------------- | ----------------------------------------------------------- |
| `\oint`            | $\oint$              | 环路积分                                                    |
| `\approx`          | $\approx$            | Almost equal to (relation) 双线约等于                       |
| `\sim`             | $\sim$               | 相似 单线约为                                               |
| `\ldots`           | $\ldots$             | lying dots 省略号                                           |
| `\cdots`           | $\cdots$             | centerd dots 省略号                                         |
| `\infty`           | $\infty$             | infinity 无穷                                               |
| `\gg`              | $\gg$                | greater greater 远远大于                                    |
| `\ll`              | $\ll$                | less less 远远小于                                          |
| `\propto`          | $\propto$            | 正比于                                                      |
| `\in`              | $\in$                | 属于                                                        |
| `\notin`           | $\notin$             | 不属于                                                      |
| `\ast`             | $\ast$               | Asterisk operator, convolution, six-pointed (binary) 六角星 |
| `\cong`            | $\cong$              | Congruent (relation).  全等                                 |
| `\dagger`          | $\dagger$            | Dagger relation (binary). 厄米共轭                          |
| `\ast`             | $\ast$               | asterisk. 复共轭                                            |
| `\equiv`           | $\equiv$             | Equivalence (relation).  恒等于                             |
| `\subset`          | $\subset$            | Subset (occasionally, is implied by) (relation) 子集        |
| `\varphi`          | $\varphi$            | Variant on the lowercase Greek letter 变型希腊字母          |
| `\zeta`            | $\zeta$              | Lowercase Greek letter                                      |
| `\Zeta`            | $\Zeta$              | Lowercase Greek letter                                      |
| `\xi`              | $\xi$                | Lowercase Greek letter                                      |
| `\upsilon`         | $\upsilon$           | Lowercase Greek letter                                      |
| `\mathsection`     | $\mathsection$       | Section sign in math mode                                   |
| `\langle`          | $\langle$            | Section sign in math mode 尖括号                            |
| `\left| a \right|` | $\left\| a \right\|$ | absolute value 绝对值                                       |
| `\leftrightarrow`  | $\leftrightarrow$    | 双向箭头                                                    |
| `\widehat{}`       | $\widehat{M}$        | 宽帽子                                                      |
| `\sqrt[4]{8}`      | $\sqrt[4]{8}$        | 四次根号`8`                                                 |

## 自定义数学符号

\mathrm v.s. \text

[is-there-a-preference-of-when-to-use-text-and-mathrm](https://tex.stackexchange.com/questions/19502/is-there-a-preference-of-when-to-use-text-and-mathrm)

警告:以下讨论假定软件包`amsmath`已加载.通常, `\ mathrm`应用于`符号`,而 `\ text`应用于文本. :)

但是,最好对代表函数的罗马字母簇使用运算符:命令`\lcm`和`\gcd`已预定义;  对于`ord`,没有预定义的命令,但是把下列定义放入导言区就足够了

```latex
\DeclareMathOperator{\ord}{ord}
```

在这种情况下,`\text {divides}`和`\mathrm {divides}`可能给出相同的结果,
但是它们在概念上是不同的(根据所使用的数学字体,它们实际上可以以不同的方式打印).
例如,`\mathrm`的参数中的空格将被忽略.
此外,`\text`跟周围环境的字体有关:在定理的陈述中它将以斜体显示.

应特别注意诸如`m/s`之类的单位;
最好不要`手工`制作它们,而要使用`siunitx`之类的程序包,它可以处理所有的细节,同时又非常灵活.

### 求迹 Trace

```latex
\usepackage{amsmath}
\DeclareMathOperator{\Tr}{Tr}
```

### ess sup 本性上确界 max

[LaTeX系列笔记(3)-数学运算符 Math Operator](https://zhuanlan.zhihu.com/p/137969798)

使用 `\operatorname` 打包成的 `\DeclareMathOperator`(推荐)

```latex
\usepackage{amsmath}

\DeclareMathOperator{\arccot}{arccot}        %   不能把上下标放在符号的正上方/正下方
\DeclareMathOperator*{\Max}{max}             %   可以把上下标放在符号的正上方/正下方
```

比如可以调用

```latex
$\arccot^2$                     % 而不能 \arccot\limits^2
$\Max_u, \Max\limits_u$         % 都可以
```

那如果经常使用 `\Max\limits_u`, 一个方便的办法是

```latex
\usepackage{amsmath}

\DeclareMathOperator*{\MaxTemp}{max}
\newcommand*{\Max}[1]{\MaxTemp\limits_{#1}}

\begin{document}
$\Max{u}$
\end{document}
```

## 定义配对的数学符号

绝对值

`\vert` or `|`

单行竖线(普通).

类似:双线竖线`\Vert`.
如果是类似于定义一个集合时用到,请使用`\mid`,因为它是一个关系.

对于绝对值,您可以使用`mathtools`软件包,并在您的序言中放入.

```latex
\DeclarePairedDelimiter\abs{\lvert}{\rvert}
```

它会提供三个命令变体,提供正确的水平对齐的单行铅垂线:

在正文中,使用带星号的版本,`\abs*{\frac{22}{7}}`,竖线的高度会匹配参数的高度
而`\abs{\frac{22}{7}}`会保留默认高度.
`\abs[size command]{\frac{22}{7}}`会给出指定的高度,比如`\Bigg`

## 微分符号

[在LaTeX中使用微分算子的正确姿势](https://www.latexstudio.net/archives/10115.html)

\newcommand*{\dif}{\mathop{}\!\mathrm{d}}

```latex
\begin{displaymath}
\mathop{\sum \sum}_{i,j=1}^{N} a_i a_j
{\sum \sum}_{i,j=1}^{N} a_i a_j
\end{displaymath}
```

`\mathop` is considered to be a single variable sized math symbol for purposes of placing limits below (subscripts) and above (superscripts)  in display math style

数学符号的标准,首先是定义在 ISO 31-11 当中; 而后这个标准被 ISO 80000-2:2009 取代.
因此,此篇讨论的内容,都是基于 ISO 80000-2:2009 的.  在 ISO 80000-2:2009 中,微分算子被描述为

+ 直立的拉丁字母 `d`;
+ 一个右边没有间距的操作符.

对于直立的拉丁字母 `d`,我们可以使用 `\mathrm{d}` 达成效果.
而若要微分算子的左边有间距,而右边没有,这个问题就值得思考了.  最简单的办法,是将微分算子做如下实现

```latex
\newcommand*{\dif}{\,\mathrm{d}}
```

看起来,这样是没有问题的. 但是,在某些情况下,就会出现尴尬的问题. 比如

```latex
关于 $x$ 的微分 $\dif x$ 是指的思考的问题.
```

因为在 `\dif` 的定义中,`\mathrm{d}` 之前有不可省略的铅空 `\,`.
于是,这份代码中 `\dif x` 与前后正文之间的距离就不一致了. 为了解决这个问题,更有经验的人可能会选择这样定义

```bash
\newcommand*{\dif}{\mathop{\mathrm{d}}\!}
```

这份代码,试图利用 `\mathop`,只在必要的时候于左边插入空白,修复了上面的问题.
不过,这样一来也带来了一些副作用 -- 在 `\mathop` 的作用下,`\mathrm{d}` 的基线发生了改变,不再与正常的数学变量保持在同一个基线上. 这也是不好的.
最终解决问题,应该对微分算子有这样的定义

```latex
\newcommand*{\dif}{\mathop{}\!\mathrm{d}}
```

在这个定义中,拉丁字母 d 本身的特点得到了保留(比如基线是正常的).
此外,在 \mathrm{d} 的左边,插入了一个空白的 `\mathop{}`; 其左边的空白保留,而右边与 `\mathrm{d}` 之间的距离,则由 `\!` 抑制. 这样就达成了我们的目标.

数学公式环境中,本来就没有距离,所以`\mathrm{d}`什么都没有,就代表右侧没有距离,
左边的`\!`是用来把`\mathrm{d}`往左移动的,就是离左边稍微近一点,因为插入了一个空白的数学符号.

测试如下:

```latex
$\int\,\mathrm{d} x$\\
关于 $x$ 的微分 $\,\mathrm{d} x$ 是指的思考的问题. \\
$\int \mathop{\mathrm{d}}\! x $\\
关于 $x$ 的微分 $\mathop{\mathrm{d}}\! x$ 是指的思考的问题. \\
$\int \mathop{}\!\mathrm{d} x $\\
关于 $x$ 的微分 $\mathop{}\!\mathrm{d} x$ 是指的思考的问题. \\
$\int \mathop{}\mathrm{d} x $\\
关于 $x$ 的微分 $\mathop{}\mathrm{d} x$ 是指的思考的问题. \\
```

## 积分符号

amsmath; chap 7.4; page 22

`\iint`,` \iiint`, 和 `\iiiint` 给出了多个积分符号, 它们之间的间距得到了很好的调整, 在`text`和`display` style上都是如此.
`\idotsint` 是同一理念的延伸, 它提供了两个用圆点分隔的积分号.

## 上下划线和大括号

[LaTeX教学3.2.2 数学结构-上下划线和大括号](https://www.jianshu.com/p/0217f22ebb3e)

有的时候我们会需要在公式的上面或者下面打一条线, 这时候我们需要用到两个命令:

`\overline`和`\underline`

这是两个带一个必要参数的命令 , 分别用来在公式上作上划线和下划线. 比如:

除了横线和箭头, 数学公式还可以使用`\overbrace`和`\underbrace`来带上花括号, 如

同时我们还可以用上下标在花括号上做标注, 如

```latex
\[
 ( \overbrace{a_{0},a_{1},\dots,a_{n}}^{n+1} )=
 ( \underbrace{0,0,\dots 0}_{n},1 )
\]
```

## 上下标,左下标

[数学运算符 Math Operator](https://zhuanlan.zhihu.com/p/137969798)
[如何公式实现左下标? ](https://wenda.latexstudio.net/q-2075.html)
[氢化脱苄苯甲醇-上下标](https://www.jianshu.com/p/229cbbac9446)

我们会发现, 在行内公式中, 所有的符号和上下标都以行内的大小为准, 尽量不去突出一行的高度, 而在行间公式中, 各种巨算符可以尽情舒展, 变成了平常我们见到的样子.
如果想在行内公式中也使用行间公式款的上下标的话, 可以在对应的命令前面加上`\displaystyle`, 即显示模式, 就可以变成我们想要的样子了, 但是这种方法会产生一些不良行距.
而另一种方法是比较推荐使用的: `\limits`, 顾名思义, 这个命令可以把上下标变成我们常见的"极限"形式的上下标, 也就是显示在算符的上下, 而不是右边. 而与它对应的命令是`\nolimits`, 也就是把上下标显示在算符的右侧而不是上下. 下面通过一个例子来比较这几种的显示效果

```latex
\documentclass{article}
\usepackage[UTF8]{ctex}
\begin{document}
\noindent 无穷级数$ \sum_{n=1}^{\infty}a_{i} $收敛\\
无穷级数$ \displaystyle\sum_{n=1}^{\infty}a_{i} $收敛\\
无穷级数$ \sum\limits_{n=1}^{\infty}a_{i} $收敛\\
无穷级数$ \displaystyle\sum\nolimits_{n=1}^{\infty}a_{i} $收敛
\end{document}
```

```latex
$\limsup\limits_{x\rightarrow0}$ vs $\lim\sup\limits_{x\rightarrow0}$
```

So you will want to use `\limsup`.

### 多行上下标,substack

amsmath; chap 7.1, page21;

`\substack` 命令可以用来产生多行上下标, 例如:

```latex
\sum_{\substack{
0\le i\le m\\
0<j<n}} P(i,j)
```

稍微通用的形式是 `subarray` 环境,
它允许你指定每一行应该是`左对齐`而不是`居中`, 就像这里:

```latex
\sum_{\begin{subarray}{l}
i\in\Lambda\\ 0<j<n
\end{subarray}}
P(i,j)
```

### \sideset

amsmath; chap 7.2, page21;

还有命令叫 `/sideset`, 用于一个相当特殊的目的: 把`符号`放在 `\sum`或`\prod` 的 下标和 上标角上.
注意: 这个命令不是为了应用于 `sum 类符号` 以外的东西. 最典型的例子是你想把 `\prime` 放在 `\sum` 符号上的情况.
如果在 `\sum` 的上方或下方没有上下限(`limits`), 你可以直接使用 `\nolimits`,
在显示模式下类似于`\sum\nolimits" E_n`:
$$\sum\nolimits' E_n$$

然而, 如果你不仅想要`\prime`, 还想使用上下限, 那就不那么容易了--事实上, 如果没有 `\sideset`, 这将是非常困难的.
`有了sideset`, 你可以写成 `\sideset{}{'}\sum_{n<k,\;\text{$n$ odd}} nE_n`.
额外一对空括号的解释是, `\sideset` 有能力在大运算符的 `每个角` 放一个或多个符号;
要在`product`符号的每个角放`星号`, 你可以输入 `\sideset{_*^*}{_*^*}\prod`.

其他的例子例如:

```bash
%\usepackage{amsmath}
\[\sideset{_h}{_h}{\mathop{\langle\psi|Q_h|\psi\rangle}}\]
```

## 交换图,Commutative diagrams

`AMS-TEX` 中的 `交换图` 命令作为单独的软件包, `amscd`.
对于复杂的交换图, 作者需要求助于更全面的软件包, 如 `TikZ`(详细说是 `tikz-cd`)或 `XY-pic`,
但对于没有对角线箭头的简单图, `amscd` 命令可能更方便. 下面是一个例子:

```latex
\begin{CD}
S^{\mathcal{W}_\Lambda}\otimes T    @>j>>       T\\
@VVV            @VV{\End P}V\\
(S\otimes T)/I          @=          (Z\otimes T)/J
\end{CD}
```

在 `CD` 环境中, 命令 `@>>>`, `@<<<`, `@VVV`, 和 `@AAA` 分别给出了 分别给出 `右`, `左`, `下`和`上`的箭头.
对于 `水平箭头`, 第一二个`>`或`<`符号之间的内容材料将被排成上标, 而第二三个之间的材料将被排成`下标`.
同样, 垂直箭头的第一二个或第二三个 `As` 或 `Vs` 之间的内容, 将被排成左或右的 `sidescripts`.
命令 `@=` 和 `@|` 给出水平和垂直双线.
在需要的情况下, 可以使用 "空箭头" 命令 `@.`, 来代替可见的箭头, 填充数组.

## 可延展箭头

[amsmath](https://www.ctan.org/pkg/amsmath)
[Extending arrows with overset text](https://tex.stackexchange.com/questions/5225/extending-arrows-with-overset-text)

`amsmath`包中提供了可延展/伸缩的箭头, 见 chapt 4.9; page15; Extensible arrows.

`\xleftarrow` and `\xrightarrow` 产生可以自动适应上下文字长度的箭头.
这些命令接受一个可选参数作为下标, 一个必须参数作为上标, 可以为空.
类似有`\xLeftarrow`, `xRightarrow`.

可以提供可选参数, 将放在箭头下方:

```latex
\documentclass{article}
\usepackage{mathtools}
\begin{document}
\[
    \xrightleftharpoons[k_2]{\,k_1\,}
\]
\end{document}
```

+ 类似的支持可扩展箭头的软件包:

amsmath ( `\xleftarrow` 等, 新的命令也可以类似地创建)
extarrows
extpfeil
mathtools

## 各种等号, 定义符号

### 三角等号

```latex
\usepackage{amssymb}
$\triangleq$
```

### 长等号

[如何输入长等号, 且上下可添加文字](https://www.latexstudio.net/archives/8004.html)

有网友提出问题, 如何输入长等号,
而且可以依据等号上下的文字宽度来自动伸缩长度, 同样我们也会用到箭头的输入.

这里我们提供如下方案使用 `\usepackage{extarrows}`:

```latex
$$ A \xlongequal{\quad\quad}B $$
$$ A\xlongequal[sub-script]{super-script}B $$
```

输出为:

![arrow](https://pics.latexstudio.net/article/2019/1203/65cb85bf4cc5415.jpg)

另外还有

```latex
\xLongleftarrow:
\xLongrightarrow:
\xLongleftrightarrow:
\xLeftrightarrow:
\xlongleftrightarrow:
\xlongrightarrow:
\xleftrightarrow:
\xlongleftarrow:
(amsmath) \xleftarrow:
(amsmath) \xrightarrow:
\xlongleftarrow:
\xlongrightarrow:
(amsmath) \xleftarrow:
(amsmath) \xrightarrow:
```

样式如下:

![longarrow](https://pics.latexstudio.net/article/2019/1203/1674d27738f84c9.jpg)

参看: [extarrows](http://mirror.ctan.org/macros/latex/contrib/extarrows/extarrows-test.pdf)

另外, 还有 `\usepackage{chemarrow}` 提供的扩展:
如下:

![chemarrow](https://pics.latexstudio.net/article/2019/1203/5b09a23951f9e3c.jpg)

## 下划线,中划线

[LaTeX文字的加粗, 斜体, 下划线, 删除线等](https://www.jianshu.com/p/a1838fa53882)

+ 加粗 `\textbf{文字}`
+ 斜体 `\emph{文字}`
+ 下划线 `\underline{文字}`

+ 删除线; 调用包, `\usepackage{ulem}`, 然后:

    ```latex
    \sout{文字} %删除线
    \uwave{文字} %波浪线
    \xout{文字} %斜删除线
    \uuline{文字}  %双下划线
    ```

## 多重下标

[如何排版公式的多行下标](https://jingyan.baidu.com/article/59703552e0fae18fc1074043.html)

第一种方法:使用命令`\substack`,可以排版多重上标或下标,两行之间用`\\`分隔, 居中显示. 例如:

```latex
\begin{equation}
\sum_{\substack{k_0,k_1,\dots>0\\   k_0+k_1+\dots=n}}   F(k_i)
\end{equation}
```

第二种方法: 我们可以使用`subarray`环境来实现多行上下标, 且可以自己选择对齐方式.

```latex
\begin{gather}
  \sum_{\substack{0 \le i \le m\\
  0 < j < n}}P(i, j)\\
  \sum_{\begin{subarray}{l}
    i \in \Lambda \\
    0 \le i \le m \\
    0 < j < n
  \end{subarray}} P(i, j)
\end{gather}
```

### 行间公式间距, display style

[Latex中调整多行公式间距的方法](https://blog.csdn.net/dongle0224/article/details/88820170)
[Latex中调整多行公式间距的方法](https://blog.csdn.net/LeleHEU/article/details/118438407)

+ 公式过于紧凑, 使得排版不美观.
这个时候我们可以在第一列的最后加上 `\vspace{1ex}`
(大括号内的数字表示行间距大小, 可以自行设置)

```latex
\begin{array}{l}
 M = M.*Y{A^T}./MA{A^T}\vspace{1ex}, \\
 A = A.*{M^T}Y./{M^T}MA.
 \end{array}
```

+ 调整方法2. 公式换行的时候, 加上参数 `\\[1mm]`, 例如

```latex
\Gamma(z) = \int_0^\infty t^{z-1}e^{-t}dt \\[5mm]
\Gamma(z) = \int_0^\infty t^{z-1}e^{-t}dt
```

### LaTeX 调整行内公式的行距

[LaTeX 中如何调整行内公式的行距](https://www.zhihu.com/question/388815628)

试试设置更大的 `\lineskip` 和 `\lineskiplimit`.  它们的作用是:

+ 当相邻行的 `纵向间距` 小于 `\lineskiplimit` 时, 额外插入高度为 `\lineskip` 的间距.
详见书籍 TeX by Topic[1], chap 15.
+ 另见刘海洋的知乎回答
    + [对于固定格式的文档, latex相对于word来说还有什么优势? ](https://www.zhihu.com/question/58100147/answer/155635456)
    + [LaTeX 设置的行距与 Word 的行距如何对应?](https://www.zhihu.com/question/62327906/answer/197899935)

从一致性的角度, 应注意避免行间距不一致.
减小行内公式的高度(使用 m/2 代替 \frac{m}{2})和 复杂程度,
在合适时使用行间公式都是可行的方面. 可依据习惯和个人偏好选用.

```latex
\documentclass{article}
\usepackage{amsmath}

\begin{document}

\newcommand\test{\par
  \noindent
  \texttt{\textbackslash lineskip = \the\lineskip,
          \textbackslash lineskiplimit = \the\lineskiplimit}\par
  text text text text text text text text text \\
  text text text text $\frac1{\frac nm} + \dfrac1{\frac nm}$ text text text text\\
  text text text text text text text text text text \\
  text text text text text text text text text text \par\bigskip
}

\test

\setlength{\lineskip}{2.5pt}
\setlength{\lineskiplimit}{2.5pt}
\test
\end{document}
```

## 调整array环境间距

[tools/array](https://mirror-hk.koddos.net/CTAN/macros/latex/required/tools/array.pdf)

array 和 tabular 环境的新实现是一个更大项目的一部分,
我们试图在某些方面改进 LATEX 代码, 使 LATEX 更容易处理.
读者应该熟悉上述环境的一般结构.
更多信息可参见 [3] 和 [1].

表 1 介绍了 preamble 中可以使用的 附加选项,
以及那些现在含义略有不同的选项.

### `\extrarowheight`

此外, 我们还引入了一个名为 `\extrarowheight` 的新参数.
如果它的长度为正数, 那么该参数的值将加到表格每一行的正常高度上,
而深度将保持不变.
这对有水平线的表格很重要, 因为这些线通常会接触到大写字母.
例如, 我们在表 1 中使用了 `\setlength{\extrarowheight}{1pt}`.

在讨论实现之前, 我们将讨论几个使用新 preamble选项的例子.

+ 如果想在左侧冲洗列中使用特殊字体(例如 `\bfseries`),
可以使用 `>{\bfseries}l` 来实现.
您不必再将 `\bfseries` 添加到列中的每个条目了.
