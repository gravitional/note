# latex 语法

## input与include

[Latex导入文件/input和/include方式](https://blog.csdn.net/OOFFrankDura/article/details/89644373)

`\input`命令可以改为 `\include`,
区别在于,`input`可以放在导言区和正文区,包含的内容不另起一页;
而 `include` 只能放在正文区,包含的内容另起一页.

另外`CJK`中还有`CJKinput`和`CJKinclude`命令.

## newcommand, 新命令

[LaTeX2e unofficial reference manual](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html)
12.1 \newcommand & \renewcommand

语法为:

```latex
\newcommand{\cmd}{defn}
\newcommand{\cmd}[nargs]{defn}
\newcommand{\cmd}[nargs][optargdefault]{defn}
\newcommand*{\cmd}{defn}
\newcommand*{\cmd}[nargs]{defn}
\newcommand*{\cmd}[nargs][optargdefault]{defn}
```

重定义已有命令

```latex
\renewcommand{\cmd}[nargs]{defn}
\renewcommand{\cmd}[nargs]{defn}
\renewcommand{\cmd}[nargs][optargdefault]{defn}
\renewcommand*{\cmd}{defn}
\renewcommand*{\cmd}[nargs]{defn}
\renewcommand*{\cmd}[nargs][optargdefault]{defn}
```

+ 定义或重定义命令. 参见关于 `\DeclareRobustCommand` 的讨论, 位于 `Class  and package commands`中.
+ 两命令的 `*` 号形式, 要求参数不能包含 `多段文字`. (用 `plain TeX` 术语说,不能为`\long` ).

### 参数说明

+ `cmd`:必选,命令名称. 用`\`开头. 且不能以`\end`开头,对于`\newcommand`, 命令不能定义过.
+ 对于`\renewcommand`, 命令必须已经定义过.

+ `nargs`:可选,一个从`0`到`9`的整数.
指定命令接受的参数个数,包括可选参数. 忽略这个参数相当于设定为`0`,
意味着命令不接受参数. 如果重定义命令,新命令可以和旧命令的参数数目可以不一样.

+ `optargdefault`:可选. 如果这个参数存在, `\cmd`的第一个参数将是可选参数(可以是空字符串).
如果这个参数不存在,`\cmd`不使用可选参数.
也就是说,如果用`\cmd[optval]{...}`调用, `#1`将会被设置成`optval`;
如果用`\cmd{...}`调用,`#1`将会被设置成`optargdefault`. 两种情况下,必选参数都从`#2`开始.
忽略`[optargdefault]`与使用`[]`是不同的, 前一种情况, `#1`被设置为`optargdefault`; 后一种情况,`#1`被设置为空字符串.

+ `defn`: 需要; 每次遇到`\cmd`就用`defn`替换.
参数`#1`,`#2`被替换成你提供的值.
`Tex`会忽略跟在`\cmd`后面的空白. 如果你想要一个空白,使用`\cmd{}`或者使用显式的控制序列`'\cmd\ '`.

定义新命令的简单例子:

```latex
\newcommand{\RS}{Robin Smith}
```

文中的每个`\RS` 会被 `Robin Smith`替换.
重定义命令是类似的 `\renewcommand{\qedsymbol}{{\small QED}}`.
用`\newcommand`重定义命令,或者用`\renewcommand`定义新命令,都会报错.

Here the first command definition has no arguments, and the second has one required argument.

```latex
\newcommand{\student}{Ms~O'Leary}
\newcommand{\defref}[1]{Definition~\ref{#1}}
```

使用第一个命令时,建议用`\student{}`(以便于和后面有空格区分开).
第二个命令有一个变量,`\defref{def:basis}`将会展开成`Definition~\ref{def:basis}`,最终展开成类似于`Definition~3.14`.

类似地,两个必选参数:`\newcommand{\nbym}[2]{$#1 \times #2$}`,调用时使用`\nbym{2}{k}`.

可选参数的例子:`\newcommand{\salutation}[1][Sir or Madam]{Dear #1:}`
`\salutation`给出`Dear Sir or Madam:`,`\salutation[John]`给出`Dear John:`.
`\salutation[]`给出 `Dear :`

这个例子给出一个可选参数和两个必选参数:

```bash
\newcommand{\lawyers}[3][company]{#2, #3, and~#1}
I employ \lawyers[Howe]{Dewey}{Cheatem}.
```

输出是`I employ Dewey, Cheatem, and Howe`.
`\lawyers{Dewey}{Cheatem}`将给出`I employ Dewey, Cheatem, and company`

`defn` 周围的大括号并不会定义一个组,也就是说,它并不会限制指令的生效范围.
比如,使用`\newcommand{\shipname}[1]{\it #1}`,

```latex
The \shipname{Monitor} met the \shipname{Merrimac}.
```

单词 `met the`也会变成斜体`italics`. 解决方法是在定义中额外加上一对大括号:

```latex
\newcommand{\shipname}[1]{{\it #1}}
```

### \providecommand

[12.2 \providecommand](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#index-_005cprovidecommand)

简洁, 使用下列形式之一:

```latex
\providecommand{\cmd}{defn}
\providecommand{\cmd}[nargs]{defn}
\providecommand{\cmd}[nargs][optargdefault]{defn}
\providecommand*{\cmd}{defn}
\providecommand*{\cmd}[nargs]{defn}
\providecommand*{\cmd}[nargs][optargdefault]{defn}
```

定义 `命令`, 只要没有这个名字的 `命令` 存在.
如果没有这个名字的命令存在, 那么这个定义的效果与 `newcommand` 相同.
如果这个名字的命令已经存在, 那么这个定义不做任何事情.
在文件可能被多次加载时, 这特别有用, 比如 `style` 文件.
参见 `\newcommand` & `\renewcommand` , 了解参数的描述.

这个例子

```latex
\providecommand{\myaffiliation}{Saint Michael's College}
\providecommand{\myaffiliation}{Lyc\'ee Henri IV}
From \myaffiliation.
```

输出 `From Saint Michael's College`.
与 `newcommand` 不同, 重复使用 `\providecommand` 不会出现错误.

### \DeclareRobustCommand

```latex
\DeclareRobustCommand{cmd}[num][default]{definition}
\DeclareRobustCommand*{cmd}[num][default]{definition}
```

就像 `\newcommand` 和 `\newcommand* ` (见 \newcommand & \renewcommand ),
但是它们声明 `健壮的命令`(robust), 即使定义中的一些代码是脆弱的.
(关于健壮的和脆弱的命令的讨论, 请参见 `\protect`. )

使用这个命令来定义新的健壮的命令, 或重新定义现有的命令并使其 `健壮`.
与 `\newcommand` 不同的是, 如果宏 `cmd` 已经存在, 它们不会报错;
相反, 如果命令被重新定义, `日志信息` 会被放入 `transcript` 文件.

这样定义命令, 比使用 `\newcommand` 定义效率要低一些,
所以除非命令的数据很脆弱, 而且命令是在 `moving` 参数中使用的, 否则请使用 `\newcommand`.

`etoolbox` 软件包提供了命令 `\newrobustcmd`, `\newrobustcmd*`,
以及命令 `\renewrobustcmd`, `\renewrobustcmd*`,
和命令 `\providerobustcmd`, 和`\providerobustcmd*`.

这些命令类似于 `\newcommand`, `\newcommand*`, `\renewcommand`,
`\renewcommand*`, `\providecommand`, and `\providecommand*`,
定义了健壮的 命令, 但与 `\DeclareRobustCommand` 相比, 有两个优点.

它们使用低级别的 `e-TeX` 保护机制, 而不是高级别的 LaTeX `\protect` 机制, 所以它们不会产生上面提到的性能的轻微损失,
并且它们在 `\new...`, `\renew...`, 和 `\provide...` 之间的区分, 与标准命令相同,
所以当你重新定义已经存在的 `cmd` 时, 它们不会只做一条日志信息,
在这种情况下, 你需要使用 `\renew...` 或 `\provide...`, 否则就会出现错误.

## IfFileExists

```latex
\IfFileExists{filename}{true code}{false code}
\InputIfFileExists{filename}{true code}{false code}
```

如果 `LaTeX` 找到了文件的文件名, 就执行 `true code`, 否则就执行 `false code`.
在第一种情况下, 它执行 `true code`, 然后输入文件. 因此, 该命令

```latex
\IfFileExists{img.pdf}{%
    \includegraphics{img.pdf}}{\typeout{!! img.pdf not found}
```

将 `include img.pdf`, 如果它存在, 否则就发出警告.

这个命令在 `LaTeX` 使用的所有搜索路径中寻找文件, 而不仅仅是在当前目录中.
要想只在当前目录中寻找, 请执行类似于 `\IfFileExists{./filename}{true code}{false code}`的命令.
如果你请求不含 `.tex` 扩展名的文件名, 那么 `LaTeX` 将首先通过添加 `.tex` 来寻找该文件;
关于 `LaTeX` 如何处理文件扩展名的更多信息, 请参见 `\input`.

## \RequirePackage \usepackage 区别

[What's the difference between \RequirePackage and \usepackage?](https://tex.stackexchange.com/questions/19919/whats-the-difference-between-requirepackage-and-usepackage)

惯例是在包或者文档类中使用`\RequirePackage`,在文档中使用`\usepackage`

`\RequirePackage`可以用在`\documentclass ....`之前

you can write :

```latex
\RequirePackage{atbegshi}
\documentclass ....
```

and not

```latex
\usepackage{atbegshi}
\documentclass ...
```

## 保留字符 Reserved characters

[23.1 Reserved characters](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#index-_005c_007e)

LaTeX为特殊目的预留了以下字符.  例如,百分号％用于注释.  它们被称为保留字符或特殊字符.

`# $ % & { } _ ~ ^ \ `

除了最后三个,都可以用转义实现

如果希望保留的字符以其自身的形式打印在文本正文中,
则除该列表中的最后三个字符外,对于所有字符,只需在字符前面加上反斜杠`\`. 因此,键入`\$ 1.23`将在输出中产生`$ 1.23`.

最后三个要使用
`\~{}` : 本来是用来给后面跟的字符加上波浪线的
`\^{}`:同理,本是用来加上音调符号的
`\textbackslash{}`:这个不知道有啥用,就是加个`backslash`

若要使用`typewriter font `,使用`verb!! `语法

```latex
\begin{center}
  \# \$ \% \& \{ \} \_ \~{} \^{} \textbackslash \\
  \verb!# $ % & { } _ ~ ^ \!
\end{center}
```

## verb 宏, 原文

概要:

```latex
\verb char文字文本char
\verb* char文字文本char
```

使用打字机(`\tt`)字体对输入的文字文本进行原样排版,包括特殊字符和空格.
此示例显示了`\verb`的两种不同调用.

```latex
This is \verb!literally! the biggest pumpkin ever.
And this is the best squash, \verb+literally!+
```

第一个`\verb`的文字文本带有感叹号`! `.第二个取而代之的是使用加号`+`,因为感叹号是文字文本的一部分.

包围文字文本的单字符定界符`char`必须相同.
`\verb`或`\verb*`与`char`之间,`char`与文字文本之间,或文本与第二个`char`之间不能有空格
(上面的空格是为了区分不同部分).分隔符不能出现在后续文本中,文本中不能包含换行符.
`\verb*`形式的不同之处仅在于, 将空格以可见字符打印出来.

## verbatim 环境

概要:

```latex
\ begin {verbatim}
文字文本
\ end {verbatim}
```

创建一个段落,对内容原样输出.例如,在文字文本中,反斜杠`\`字符不会启动命令,它会产生一个打印的`\`,
并按字面意义使用回车符和空格.输出以类似等距打字机的字体(`\tt`)出现.文字文本的唯一限制是它不能包含字符串`\end{verbatim}`.
您不能在宏的参数(例如`\section`的参数)中使用逐字记录环境.(但是`cprotect`软件包可以帮助您解决此问题.)

`verbatim`的一种常见用法是排版计算机代码.有一些软件包可以改善`verbatim`.
例如,一种改进是允许逐字包含外部文件或这些文件的一部分,比如`listings`, and `minted`.
一个为`verbatim`环境提供更多选项的软件包是`fancyvrb`.
另一个是`verbatimbox`. 有关所有相关软件包的列表,请参见CTAN.

## \makeatletter, \makeatother

[What do \makeatletter and \makeatother do?](https://tex.stackexchange.com/questions/8351/what-do-makeatletter-and-makeatother-do)

`TeX` 中的所有字符都分配有`Category Code `类别代码, 简称`catcode`. 总共有`16`种`Catcode`,有些仅包含一个字符,例如`\`(通常为 `catcode 0`,`{`, `catcode 1`等).
正常字符为`catcode 11`, 此类别通常包含所有字母字符.  `@`符号的`catcode`为`12`,这意味着它不会被视为普通字母.
这样做的结果是,`@`通常不能在用户文档文件中用作多字符宏名称的一部分. (宏名称中也禁止使用所有其他 non-letter 字符:例如,`\foo123`和`\foo?!`不是有效的宏名称.)

但是,在`LaTeX`类和程序包文件中,`@`被视为普通字母(`catcode 11`),这使程序包编写者可以在宏名中包含`@`.
这样做的好处是, 此类宏名称会自动避免普通用户的更改:普通用户那里`@`不是普通字母,没法用作宏名称, 所以不会来覆盖或更改包内部的宏.

但是,有时在用户文档中必须访问程序包内部的宏. 因此,

+ `\makeatletter` 将`@`的`catcode`从`12`更改为`11`, 也就是改成普通字母, 可以用作命令,
+ `\makeatother `将`@`的`catcode`从`11`更改为`12`, 不能用作命令.

实际上,如果需要修改名称中包含`@`符号的程序包内部宏,则需要用以下命令将修改内容括起来:

```latex
\makeatletter % changes the catcode of @ to 11
<your changes here>
\makeatother % changes the catcode of @ back to 12
```

这些命令不应在`.sty`和`.cls`文件本身中使用,因为它们可能与`package`和`class`文件时加载时发生的`catcode`更改冲突.

## 引用,\quote,\quotation

[quotation, quote环境](https://blog.csdn.net/ProgramChangesWorld/article/details/51762789)

使用`quote`,`quotation`环境.

## 标注, callout

可以使用[callouts 包](https://www.ctan.org/pkg/callouts)

## pdf 书签

[在 LaTeX 中使用含有中文的 PDF 书签避免乱码的正确姿势 ](https://liam.page/2014/11/22/latex-pdf-cjk-bookmarks/)
[hyperref – Extensive support for hypertext in LaTeX](https://www.ctan.org/pkg/hyperref)

LaTeX 的 `hyperref` 宏包可以处理交叉引用命令,在 PDF 文件中产生超文本链接,或者是 PDF 书签,

最好的办法是将中文支持和版式处理都交给 `ctex` 宏包/文档类,只需要开启 `hyperref` 选项即可.

```latex
\documentclass[hyperref, UTF8]{ctexart}
\begin{document}
\section{中文书签不会乱码}
UTF-8 编码,Xe\LaTeX{}/pdf\LaTeX{}/\LaTeX{} - DVIPDFMx 编译.
\end{document}
```

## Token not allowed

`Hyperref - Token not allowed [duplicate]`

The following code:

```latex
\subsection{The classes $\mathcal{L}(\gamma)$}
```

产生错误:

```shell
Package hyperref Warning: Token not allowed in a PDF string (PDFDocEncoding):
(hyperref)      removing `math shift' on input line 1938.
```

`PDF`书签与目录是不同的.
书签不是由`TeX`排版的:它们只是字符串,因此不允许使用数学或一般的格式说明.
避免警告的最简单方法是使用`\texorpdfstring`:

```latex
\subsection{The classes \texorpdfstring{$\mathcal{L}(\gamma)$}{Lg}}
```

在第二个参数位置中你写下一个最佳的近似即可;  毕竟,书签只是参考文档的指南.

## 在文中使用链接

使用宏包 [hyperref](https://www.ctan.org/pkg/hyperref) 来制作

```latex
\usepackage[dvipdfm, %
pdfstartview=FitH, %
bookmarks=true,
CJKbookmarks=true, %
bookmarksnumbered=true, %
bookmarksopen=true, %
colorlinks=true, %注释掉此项则交叉引用为彩色边框 %
%(将colorlinks和pdfborder同时注释掉) %
pdfborder=001, %注释掉此项则交叉引用为彩色边框 %
citecolor=magenta, % magenta , cyan %
linkcolor=blue,
%linktocpage
%nativepdf=true %
linktocpage=true, %
]{hyperref}
```

+ `email` 链接

    ```latex
    \href{mailto:michaelbibby@gmail.com}{给我电邮}
    ```

+ `URL链接`

    链接有颜色,显示为`OpenBSD官方网站`,链接到`http://www.openbsd.org`

    ```latex
    \href{http://www.openbsd.org}{OpenBSD官方网站}
    ```

+ 只显示`URL`

```latex
\url{http://www.openbsd.org}
```

显示URL,但是不做链接和跳转:

```latex
\nolinkurl{http://www.openbsd.org}
```

[LaTeX技巧159:如何在文中使用链接](https://www.latexstudio.net/archives/7741.html)

## 国际单位制

`latex`中使用单位, 现在最好使用[siunitx](https://www.ctan.org/pkg/siunitx), 它是`LaTeX 3 `项目中的包.
`hepunits`会调用`physics`和`SIunits`, 这两个包会与`siunitx`冲突, 所以不要调用这些宏包.

`siunitx`的说明文档中有具体的用法例子:
`3.3Units` 章节列举了常用的命令, `3.6Unit abbreviations` 中有大量单位的缩写, 但是注意很多单位的定义只在`\unit{}`环境内才生效.
另外`siunitx`有第二版和第三版, 使用`texdoc siunitx`查看本地对应版本的文档, 两个版本的命令名称不同, 根据具体情况使用.

例如我本地安装的是第二版, 对应的命令为

```latex
\SI[mode=text]{1.23}{J.mol^{-1}.K^{-1}}
\ang{1;2;3} % 角度: 1度2分3秒
\si{\henry\tothe{5}}  %一般的指数可以用 \tothe 输入
\si{\raiseto{4.5}\radian} % 或者用 \raiseto
\si{\kilogram\of{metal}} %一般的限定符可以使用 \of
```

相应的第三版命令是

```latex
\qty[mode=text]{1.23}{J.mol^{-1}.K^{-1}}
\ang{1;2;3}
\unit{\henry\tothe{5}}
\unit{\raiseto{4.5}\radian}
\unit{\kilogram\of{metal}}
```

### 兼容性

一般来说, `siunitx` 应该可以和其他软件包一起使用而不受干扰.
当 `physics` 在 `siunitx` 之前加载时, 不定义命令 `\qty` : 用户需要使用第二版的命令 `\SI`.
当 `units` 在 `siunitx` 之前加载时, 不定义命令 `\unit`, 用户将需要使用第二版的命令 `\si`.

## 页眉与页脚

[Latex的页脚和页眉](https://zhuanlan.zhihu.com/p/114676221)
[fancyhdr – Extensive control of page headers](https://www.ctan.org/pkg/fancyhdr)
[LaTex页码格式,第几页共几页 ](https://www.latexstudio.net/archives/7680.html)
[LaTeX入门(七) -- 页面设置](https://zhuanlan.zhihu.com/p/56405574)

对于页眉页脚的设置,我们使用宏包`fancyhdr`. 所以,我们首先在导言区中写上

```latex
\usepackage{fancyhdr}
\pagestyle{fancy}
\fancyhf{}
```

这里注意一点,如果我们同时使用了`geometry`和`fancyhdr`宏包,那么一定要把`\usepackage{fancyhdr}`及相应的页眉, 页脚设置写在`\usepackage{geometry}`的前面,否则会出现奇怪的错误.
`fancyhdr`宏包的说明文档中也有各个页眉页脚位置的图示.[fancyhdr](https://www.ctan.org/pkg/fancyhdr)

`fancyhdr`将页面的页眉, 页脚各分为左, 中, 右三个部分,其对应的指令名为`\lhead{}`,` \chead{}`, `\rhead{}`, `\lfoot{}`, `\lhead{},` `\rhead{}`.
括号中填写的内容将在对应的地方出现. 比如想在页眉正中出现`学习指南`,我们只需在导言区加上`\chead{学习指南} `.
现在版本的`fancyhdr`已经不建议使用这种风格的命令了,而是使用类似`\fancyhead[L]{xx}`这样的命令代替.

+ 如果想使用页码,可以用`\thepage`来实现. 它存储当前页面的页码.比如想在页尾右侧写上当前页码,则在导言区中加上`\rfoot{\thepage} `.
+ 此外,顺便提一句,如果要在正文中使用`\maketitle`,那么那一页的页面格式会自动变回原来的页面格式.需要在`\maketitle`后加上一句`\thispagestyle{fancy}`.
+ 同时,页眉也被默认设置了含有页眉线.页眉线, 页脚线的指令名分别为`\headrulewidth`和`\footrulewidth`. 其粗细可分别用`\renewcommand`来设置,
例如想取消页眉线,就在导言区加上`\renewcommand\headrulewidth{0pt} `
+ 如果要在正文中使用`\maketitle`,那么那一页的页面格式会自动变回原来的页面格式.需要在`\maketitle`后加上一句`\thispagestyle{fancy}`.

下面的用法生成`第x页,共y页`的页码样式.

```latex
\usepackage{fancyhdr}
\pagestyle{fancy} %页脚的样式
\fancyfoot[C]{\kaishu 第\thepage 页共\pageref{unknown}页}
\label{unknown} %把这一行置于文末
```

文章的总页数可以直接由`lastpage`宏包的`\pageref{LastPage}`得到.所以也可以这样

```latex
\usepackage{fancyhdr}
\usepackage{lastpage}
\pagestyle{fancy} %页脚的样式
\fancyfoot[C]{\kaishu 第\thepage 页共\pageref{LastPage}页}
```

## 行距

[LaTeX系列笔记(5)-行距](https://zhuanlan.zhihu.com/p/138408387)

首先, 行距就是相邻两行文字之间的距离. 行距的调节一般使用倍数, 比如两倍行距.
而`单倍行距`又根据字体, 字号, 软件的不同而改变(不同软件中有不同的定义, 没有一个通用的值).

在 LaTeX 里面也有这些概念, 在你定义字号的时候, `单倍行距`也随之确定. 更改时, 我们更改的是`单倍行距`的倍数.
比如在 LaTeX 中 `10` 号字(无论字体), 对应的单倍行距是 `12` 磅. 修改行距倍数的方法有两个:

***
在导言区使用 `\linespread` 命令. 这个是 `LaTeX2e` 提供的. (推荐)

```latex
\linespread{2.0}
```

使用了这个命令以后, 从此之后的行距都变成`2.0`倍了.

如果只想对一小部分做更改, 可以用大括号把这一段括起来, 如

```latex
{
\linespread{2.0} \selectfont
% 两倍行距的文字
}
% 一倍行距的文字,
% 这样就不会对括号外的部分进行改变, 字体, 字号类命令也可以这么用.
```

出现在正文的 `\linespread`, 需使用 `\selectfont` 刷新行距信息后才生效. 因为 `\document` 包含 `\normalsize` 包含 `\@setfontsize` 包含 `\selectfont`,
相当于 `\begin{document}` 处总是会执行一个 `\selectfont`, 所以在导言区使用的 `\linespread` 看起来自动生效了. ---慕子

***
使用 `setspace` 宏包.

```latex
\usepackage{setspace}
\begin{document}
\begin{spacing}{2.0}
% 这里是两倍行距
\end{spacing}
% 这里是默认行距
\end{document}
```

被 `spacing` 环境框住的地方行距就会改变. `spacing` 还有一些奇怪的功能:
如果想让`行距 = 2倍字体高度`, 并且你的字号正好是 `10/11/12 pt` 之一的话, 就可以使用 `\doublespace` 命令(这个命令容易使人误以为是两倍行距).

上面两个办法的效果差不多, `\linespread` 背后的实现更复杂一些, 本质上是把`行距倍数`传递给 `\baselinestretch`, 所以实际上你也可以调用

```latex
\renewcommand{\baselinestretch}{2.0}  % 不要和 \linespread 混合使用
```

如果想直接改行距的大小, 也是可以做到的, 但是也不推荐. 因为只要你换一下字号, 行距的大小也会随之改变, 效果不持久. 改法是

```latex
\setlength{\baselineskip}{20pt}    % 改变行距. 不推荐, 因为字号一改就得重新定义.
```

这里的 `20 pt` 表示 `20 磅`.  `pt` 是 `LaTeX` 里的长度单位.

如果想直接改`单位行距`的大小, 也是可以做到的, 因为同样理由, 也不推荐.

```latex
\setlength{\normalbaselineskip}{20pt}    % 改变"单倍行距". 不推荐, 因为字号一改就得重新定义.
```

如果你想把行距搞得和 `Word` 很像, 需要改变每个字号下的`单倍行距`大小. 参考[LaTeX 设置的行距与 Word 的行距如何对应? ](https://www.zhihu.com/question/62327906/answer/197899935)

## 添加水印或者背景图片 eso-pic

添加 watermark

如果是 `beamer`, 可以直接使用`beamer`预留的宏, 类似于

```latex
\setbeamertemplate{background}{\includegraphics[height=\paperheight]{b2.png}} %设置背景图片
```

如果是文章类, 可以使用 [eso-pic](https://www.ctan.org/pkg/eso-pic).

+ `eso-pic`; 在每一页上添加图片命令( 或背景).
该软件包为`LaTeX`的`shipout routine`添加一个或多个用户命令, 可用于将`命令的输出`置于固定位置. `grid`选项可以用来寻找正确的位置.

具体的用法可以参考包文件中附带的例子, `eso-*.tex`. 例如 `eso-ex1.tex`

```latex
\documentclass[a4paper]{article}
\usepackage{eso-pic,calc}
\listfiles
\makeatletter
\AddToShipoutPicture{%
  \begingroup %下面的命令画一个 frame
    \setlength{\@tempdima}{15mm}%
    \setlength{\@tempdimb}{\paperwidth-2\@tempdima}%
    \setlength{\@tempdimc}{\paperheight-2\@tempdima}%
    \thicklines%
    \put(\LenToUnit{\@tempdima},\LenToUnit{\@tempdima}){%
      \framebox(\LenToUnit{\@tempdimb},\LenToUnit{\@tempdimc}){}}%
  \endgroup
}
\makeatother
\begin{document}
  \section*{第一页}
    此页以及接下来的页, 带有离页边 15~mm 的 frame.\newpage
  \section*{第二页}
      \AddToShipoutPicture*{\put(100,100){\circle{40}}}
      带*号命令的作用 : 只有这一页在左下角有圆圈
      \AddToShipoutPictureBG*{\AtTextCenter{\put(-110,300){\color{red}\circle{40} }}}
      末尾的 BG 表示插入到背景, AddToShipoutPictureFG 表示插入到前景,
      AtTextCenter 命令 表示插入锚点在页面中间.  \newpage
    \section*{Last page}
\end{document}
```

## 添加水印 tikz pgfpages

page 28; 1.3 Utility Packages
page 1013; 91 Page Management

也可以考虑`tikz`中的`pgfpages`包.
`pgfpages`软件包用于将几个页面组合成一个单一的页面. 它提供了用于将几个 `虚拟页` 组合成`物理页`的命令.
其原理是, 每当`TeX`准备好将一个页面`shipout`的时候, `pgfpages`就会中断这种输出, 而把要输出的页面存储在一个特殊的`盒子`里.
当以这种方式积累了足够多的 `虚拟页面`时, 它们就会被按比例缩小并排列在 `物理页面`上. 然后真正输出.

这种机制允许你直接在`LaTeX`内部创建"一页两面"的文档版本, 而不需要使用任何外部程序.
然而, `pgfpages`的作用远不止这些. 你可以用它来在页面上添加`标识`和`水印`, 在一页上最多打印`16`面, 为页面添加边框, 等等.

## Latex 单字命令

已经预定义的字母

\a (tabbing); tabbing
\b (bar-under accent); Accents
\c (cedilla accent); Accents
\d (dot-under accent); Accents

\H (Hungarian umlaut accent); Accents
\i (dotless i); Accents
\j (dotless j); Accents
\k \capitalogonek, Ogonek. Not available in the OT1 encoding.
\l (ł); Additional Latin letters
\L (Ł); Additional Latin letters
\o (ø); Additional Latin letters
\O (Ø); Additional Latin letters
\P,\textparagraph; ¶ Paragraph sign (pilcrow).
\r; \capitalring; o* Ring accent.

\t, \capitaltie, \newtie, \capitalnewtie
Tie-after accent (used for transliterating from Cyrillic, such as in the ALA-LC romanization). 
It expects that the argument has two characters. 
The \newtie form is centered in its box.

\u, \capitalbreve; ŏ Breve accent.
\v, \capitalcaron; ǒ Háček (check, caron) accent.

未占用的字母

\e
\f
\g
\h
\m
\n
\p
\q
\s
\w
\x
\y
\z
