# latex.md

## 帮助文档

查看latex 的帮助文档,可以直接用 `texdoc pkg`

`texdoc` - find & view documentation in TeX Live

SYNOPSIS

`texdoc [OPTION]... NAME...`
`texdoc ACTION`

DESCRIPTION

对于给定的名字(可以多个),试图寻找合适的TeX文档. 或者,执行某些动作并退出.

Actions:

`-h`, `--help` 打印帮助信息.
`-V`, `--version`打印版本号
`-f`, `--files` 打印使用的配置文件
`--just-view file` 展示文件,给出绝对路径(不搜索)

OPTIONS

+ `-w`, `--view` 使用查看模式,打开文档阅读器(默认)
+ `-m`, `--mixed` 使用混合模式(查看或者列表)
+ `-l`, `--list` 使用列表模式:列出搜索结果.
+ `-s`, `--showall` 展示所有模式,包括"坏"的结果
+ `-i`, `--interact` 使用交互菜单(默认)
+ `-I`, `--nointeract`使用plain列表,不需要交互
+ `-M`, `--machine` 机器可读的结果

## latex 编译

### 简单的规则

1. 空格:`Latex` 中空格不起作用.
1. 换行:用控制命令`\\`,或`\newline`.
1. 分段:用控制命令`\par` 或空出一行.
1. 换页:用控制命令`\newpage`或`\clearpage`
1. 特殊控制字符: `#`,`$`, `%`, `&`, `-` ,`{}`, `^`, `~`

### 报错示例-1

```latex
./chapter-6.tex:58: Undefined control sequence.
l.58             \partial_\mu - i \eofphi
                                           A_\mu(x)
Output written on temp/main.xdv (21 pages, 329924 bytes).
SyncTeX written on temp/main.synctex.gz.

Transcript written on temp/main.log.
```

### 简短版latexmk

```powershell
$mk_message=(latexmk -f -xelatex); Write-Output ("*" * 90);$mk_message | Where-Object {$_ -like "*tex:*"}
```

### 详细版latexmk

```powershell
if ($null -eq $args[0]) {
    # the default tex compiler, used to compile the '*.tex' files
    $tex_compiler = "-xelatex";
}
else {
    $tex_compiler = $args[0]
};

$mk_message = (latexmk -f "$tex_compiler");
#detect the line number of the error message
$line_start = ($mk_message | Where-Object { $_ -match '[ ./\w]+tex:\d+:[ \w]+' });
$line_end = ($mk_message | Where-Object { $_ -match '^Transcript[ \w]*' });
$length = ($line_start.count - 1)
## show the erroe message
Write-Output ("`n" * 2 + "the error message start" + "`n" * 2 );
for ($i = 0; $i -le $length; $i++) {
    Write-Output ("*" * 90);
    $mk_message[$mk_message.IndexOf($line_start[$i])..($mk_message.IndexOf($line_end[$i]))] | Select-Object -First 10
}
```

在命令行下运行的时候,选择不同的引擎,关键字为

`-pdf` : `-pdflatex`
`-xelatex`
`-lualatex`

### 报错示例-2

ref-2: [LaTeX 如何进行 debug](https://www.zhihu.com/question/28698141/answer/41774879)

我们故意构建一段错误的代码看看.

```latex
\documentclass{minimal}
\begin{document}
\usepackage{amsmath}
\end{document}
```

编译运行之后,会提示错误

```latex
./test.tex:3: LaTeX Error: Can be used only in preamble.

See the LaTeX manual or LaTeX Companion for explanation.
Type  H <return>  for immediate help.
 ...

l.3 \usepackage
               {amsmath}
No pages of output.
```

`LaTeX` 的错误提示分成四个部分,以这个报错为例.

以叹号开头的行说明出错原因,示例中提示:

`LaTeX Error: Can be used only in preamble`

中间段落是`LATEX`给出的提示建议.

以字母`l`开头的那一行给出出错的具体位置.
可以看到代码在 `\usepackage` 之后截断分为两行,这说明问题出在截断处.
这里是第三行的 `\usepackage` 出错了. 以问号开头的行,表示 `LaTeX` 正在等待用户输入. 这里可以输入 `x` 停止编译,直接按回车忽略该错误,甚至输入 `s` 直接忽略后续一切错误.

这里表示"第三行的 `\usepackage` 只能放在导言区,不能放在正文部分",
于是你只需要根据提示调整一下 `\usepackage` 的位置就好了.

实际使用中遇到的错误多种多样,一些错误的分析和修复可能不这么简单.
刘海洋 的《LaTeX 入门》中有名为「从错误中救赎」的章节,
专门讲解 `LaTeX` 的排错,对 `LaTeX` 的不同报错进行了详细地叙述.

### 清理latex 辅助文件powershell

```powershell
remove-item -Path ('.\*.aux','.\*.lof','.\*.log','.\*.lot','.\*.fls','.\*.out','.\*.toc','.\*.fmt','.\*.fot','.\*.cb','.\*.cb2','.\*.ptc','.\*.xdv','.\*.fdb_latexmk','.\*.synctex.gz','.\*.ps1','.\*.bib','.\*.bbl','.\*.blg')
```

```powershell
remove-item -Path ($tepath+'*.aux',$tepath+'*.lof',$tepath+'*.log',$tepath+'*.lot',$tepath+'*.fls',$tepath+'*.out',$tepath+'*.toc',$tepath+'*.fmt',$tepath+'*.fot',$tepath+'*.cb',$tepath+'*.cb2',$tepath+'*.ptc',$tepath+'*.xdv',$tepath+'*.fdb_latexmk',$tepath+'*.synctex.gz',$tepath+'*.ps1')
```

### latex 编译模式

[如何加速 LaTeX 编译](https://zhuanlan.zhihu.com/p/55043560)

不同的编译模式也有细微的影响.
经过测试,使用批处理模式(`batchmode`)速度要优于默认的模式(不加参数)和其他一些模式(比如 `nonstopmode` 和 `scrollmode`),这是因为批处理模式在编译和执行阶段是静默的,不输出任何信息,因此要快上一些.

### 清理辅助文件

删除本层目录下除了源文件的`latex`辅助文件,只保留 `*.tex`,`*.pdf`,`*.bib`

```
temp_a=$(find . -mindepth 1 -maxdepth 1 -type f   \( -not -name  "*.pdf" \)  \( -not -name  "*.tex" \) \( -not -name  "*.bib" \) -print0); if [[ ${temp_a} != '' ]]; then  echo -n ${temp_a} |  xargs --null rm; fi
```

***
`latexmk -c`

清理除了`dvi`,`postscript`和`pdf`以外的,所有由`latex`和`bibtex`或`biber`生成的可重新生成的文件.
这些文件是`log`文件,`aux`文件,`latexmk`创建的关于源文件信息的`database`文件以及在`@generated_exts`配置变量中指定的扩展名的文件的组合.

另外,将删除`$ clean_ext`和`@generation_exts`配置变量指定的文件.

此清理不是`regular make`.如果要先清理然后再进行`make`,请参阅`-gg`选项.

`.bbl`文件的处理:如果`$bibtex_use`设置为`0`或`1`,则`bbl`文件始终被视为不可重新生成.

如果`$bibtex_use`设置为`1.5`:则取决于是否存在`bib`文件.
如果存在`bib`文件,则`bbl`文件可重新生成,并在清理中被删除.
如果不存在`bib`文件,则`bbl`文件将被视为不可重新生成,因此不会被删除.

相反,如果`$bibtex_use`设置为`2`,则`bbl`文件始终被视为可重新生成的文件,并在清理时将其删除.

`latexmk -C`

与`-c`选项相同,只是增加了`dvi`,`postscript`和`pdf`文件以及`$clean_full_ext`配置变量中指定的文件.

### pdftex/xetex --help

```bash
Usage: xetex [OPTION]... [TEXNAME[.tex]] [COMMANDS]
   or: xetex [OPTION]... \FIRST-LINE
   or: xetex [OPTION]... &FMT ARGS
```

在`TEXNAME`上运行`XeTeX`,通常创建`TEXNAME.pdf`.
读取`TEXNAME`后,所有剩余的` COMMANDS`都将作为` XeTeX`input 处理.

如果`TEXNAME`的第一行是`%&FMT`,而`FMT`是现有的`.fmt`文件,将使用它.
否则使用`NAME.fmt`,其中`NAME`是程序调用名称,最常见的是`xetex`.

(**note:** `.FMT` : `Format File Tex`)

或者,如果第一个非选项参数以`反斜杠`开头,则将所有非选项参数解释为`XeTeX` input 行.

或者,如果第一个非选项参数以`&`开头,则将下一个单词作为要读取的`FMT`,覆盖其他所有设置.
其余所有参数均按上述方式处理.

如果未指定任何参数或选项,则提示输入.

| options | effect |
| ----- | ----- |
| `[-no]-file-line-error`  |   disable/enable file:line:error style messages |
| `-fmt=FMTNAME`           |   use FMTNAME instead of program name or a %& line |
| `-halt-on-error`         |   stop processing at the first error |
| ------------------------ | ------------------------ |
| `-interaction=STRING`    |   set interaction mode (STRING=batchmode/nonstopmode/scrollmode/errorstopmode) |
| ------------------------ | ------------------------ |
| `-output-comment=STRING` |   use STRING for XDV file comment instead of date |
| `-output-directory=DIR`  |   use existing DIR as the directory to write files in |
| `-output-driver=CMD`     |   use CMD as the XDV-to-PDF driver instead of xdvipdfmx |
| `-no-pdf`                |   generate XDV (extended DVI) output rather than PDF |
| ------------------------ | ------------------------ |
| `[-no]-parse-first-line` |   disable/enable parsing of first line of input file |
| ------------------------ | ------------------------ |
| `[-no]-shell-escape`     |   disable/enable \write18{SHELL COMMAND} |
| `-shell-restricted`      |   enable restricted \write18 |
| ------------------------ | ------------------------ |
| `-help`                  |   display this help and exit |
| `-version`               |   output version information and exit |

```bash
pdflatex -halt-on-error file.tex 1 > /dev/null
[[ $? -eq 1 ]] && echo "msg in case of erros" && exit
```

Email bug reports to <xetex@tug.org>.

### latex in  powershell

```pwsh
Invoke-Expression $("lualatex" + " " + "-halt-on-error " + "-output-directory=temp -shell-escape -interaction=nonstopmode " + "test.tikz.tex" ) > ./null
```

### 查看文档使用的所有文件

[All the files used by this document](https://texfaq.org/FAQ-filesused#:~:text=The%20simplest%20solution%20is%20the%20LaTeX%20%5B%26listfiles%26%5D%20command.,as%20a%20check-list%20in%20case%20that%20problems%20arise.)

当你和别人分享一个文件时, 最好是安排双方都有相同的辅助文件.
你的联系人显然需要相同的文件集( 例如, 如果你使用`url`包, 她也必须有`url`).
但是, 假设你的`shinynew`是稳定版本,但她的是不稳定的开发版; 在你们都意识到发生了什么之前, 这种情况可能会非常混乱.

最简单的解决方案是`LaTeX \listfiles` 命令. 它将在日志文件中列出所使用的`文件`和它们的`版本号`.
如果你把这个列表提取出来并与你的文件一起传送, 它可以作为一个检查列表, 以防出现问题.

请注意, `\listfiles`只记录由 `标准LaTeX`机制( `\documentclass`、`\usepackage`、`\include`、`\includegraphics`等)输入的东西.
`\input`命令, 经`LaTeX`修改后, 用`LaTeX`的语法表示为：

```latex
input\mymacros}
```

它记录`mymacros.tex`文件的细节, 但如果你用`TeX`的原始语法来使用` \input`:

```latex
\input mymacros
```

`mymacros.tex`不会被记录, 所以也不会被 `\listfiles` 列出--你绕过了记录其使用的机制.

[snapshot](https://ctan.org/pkg/snapshot)包帮助`LaTeX`文档的作者获取该文档的外部依赖列表, 其结果可以嵌入文档的顶部.
该包的预期用途是创建文档的存档副本, 但它也适用于文档交换的情况.

`bundledoc`系统使用`snapshot`来产生文档所需依赖的归档( 例如, `tar.gz`或`zip`); 它带有用于`TeX Live-Unix`和`MiKTeX`的配置文件. 当你发送文件的第一份副本时, 它显然很有用.

`mkjobtexmf`可以通过`TeX`的`-recorder`选项, 或者使用(Unix)`strace`命令来监视`TeX`的工作, 找到 `job`中使用的文件.
这样找到的文件被复制( 或链接)到一个目录, 然后可以保存起来用于传输或归档.

## 中文西文数学字体

[在 LaTeX 中使用中文](https://jdhao.github.io/2018/03/29/latex-chinese.zh/)
[LaTeX数学公式的默认字体是什么](https://www.zhihu.com/question/30058577/answer/46612848).
`LaTeX` 默认的文章类中的字体是 `Computer Modern Math`(`LaTeX`), `Latin Modern Math`(`XeTeX`). 字体文件的位置可以用`kpsewhich`查看. 在安装`TeXLive`的时候会自动安装.
如果没有安装的话,[GUST](http://www.gust.org.pl/projects/e-foundry/lm-math)可以下载`Latin Modern Math`字体,以及其他字体.

```bash
kpsewhich latinmodern-math.otf
/usr/share/texmf/fonts/opentype/public/lm-math/latinmodern-math.otf
```

`kpsewhich`的介绍可以查看[The TeX Live Guide—2021 ](https://www.tug.org/texlive/doc/texlive-en/texlive-en.html)

***
一般可以使用[fontspec](https://ctan.org/pkg/fontspec)控制西文字体和数学字体.用法大概如下:

```latex
% 设置英文字体
\setmainfont{Microsoft YaHei}
\setsansfont{Comic Sans MS}
\setmonofont{Courier New}
% 设置数学字体
\setmathrm{⟨font name⟩}[⟨font features⟩]
\setmathsf{⟨font name⟩}[⟨font features⟩]
\setmathtt{⟨font name⟩}[⟨font features⟩]
\setboldmathrm{⟨font name⟩}[⟨font features⟩]
```

但有一个问题,`\boldsymbol`是`AMS`系列包中的`amsbsy`定义的宏,可以产生粗体数学符号.
如果用`fontspec`设置数学字体为`latinmodern-math.otf`字体时,没有粗体效果,而是变成直立体,原因不明.可以通过使用`unicode-math`包解决,见下文.

对中文字体的选择可以通过[xeCJK](https://www.ctan.org/pkg/xecjk)完成:

```latex
\usepackage[slantfont, boldfont]{xeCJK}
% 设置中文字体
\setCJKmainfont[Mapping=tex-text]{Noto Sans CJK SC}
\setCJKsansfont[Scale=0.7,Mapping=tex-text]{Source Han Sans SC}
\setCJKmonofont[Scale=0.7]{Noto Sans Mono CJK SC}
% 中文断行设置
\XeTeXlinebreaklocale "zh"
\XeTeXlinebreakskip = 0pt plus 1p
```

***
其他指定数学字体的包有:[mathspec – Specify arbitrary fonts](https://ctan.org/pkg/mathspec),以及[unicode-math](https://ctan.org/pkg/unicode-math).`stackexchange`上有关于 change the math italic font in XeTeX/fontspec 的讨论,见 [change the math italic font](https://tex.stackexchange.com/questions/11058/how-do-i-change-the-math-italic-font-in-xetex-fontspec). 作者给出的示例代码为:

```latex
\documentclass{article}
\usepackage{unicode-math}
\setmainfont{Georgia}
\setmathfont{xits-math.otf}
\setmathfont[range=\mathit]{Georgia Italic}
\begin{document}
Hello $a+b=c$
\end{document}
```

载入`unicode-math`包,并使用`\boldsymbol`时会报错:[Error:Extended mathchar used as mathchar](https://tex.stackexchange.com/questions/431013/error-extended-mathchar-used-as-mathchar-when-using-bm),解决方案是不使用`\bm`,`\boldsymbol`命令,而使用`\symbf`,`\symcal`等命令.
见[Theunicode-mathpackage](https://mirrors.bfsu.edu.cn/CTAN/macros/unicodetex/latex/unicode-math/unicode-math.pdf).
`unicode-math`引入了一些新的命令,例如:

```latex
\symbb, \symbbit, \symcal, \symscr, \symfrak, \symsfup, \symsfit,
\symbfsf, \symbfup, \symbfit, \symbfcal, \symbfscr, \symbffrak, \symbfsfup, \symbfsfit
```

用来表示单个粗体数学符号,跟粗体普通文字是不同的,粗体普通文字使用`latex`中通常的`\mathbb, \mathbbit, \mathcal`等命令. 例子是:

```latex
\documentclass{article}
\usepackage{unicode-math}
\setmainfont{XITS}
\setmathfont{XITS Math}

\begin{document}
This is a simple math example with a variable $k$.
This works $\symbfit{k}$.
What I actually need is this: $\mathbfcal{X}$ and $\symbf{\Theta}$.
Compare with $\mathcal{X}$ and $\Theta$.
\end{document}
```

****
[fontenc –selecting font encodings](https://www.ctan.org/pkg/fontenc)
[Why should I use \usepackage[T1]{fontenc}?](https://tex.stackexchange.com/questions/664/why-should-i-use-usepackaget1fontenc)

`fontenc`指定字体编码(确定使用哪种字体),而不是输入编码.

`TeX`的默认字体编码(`OT1`)为`7`位,并使用具有`128`个字形的字体,因此不包括带重音符号的字符作为单个字形. 因此,通过在现有的`o`字形上添加一个重音来制作字母`ö`.
`T1`字体编码是一种8位编码,并使用具有`256`个字形的字体. 因此,`ö`是字体中的实际单个字形.
许多较早的字体也为它们设计了`T1`变体,并且许多较新的字体仅在`T1`中可用. 我认为`Computer Modern`字体最初是`OT1`,而`Latin Modern`是T1.

如果您不使用`\usepackage[T1]{fontenc}`,

+ 包含重音符号的单词不能自动连字,
+ 您无法从输出`(DVI/PS/PDF)`中正确复制和粘贴此类文字,
+ 诸如竖线符号,小于号和大于号的字符会在文本中产生意外的结果.

***
在`lyx`中,使用 `xelatex` 进行编译,可以设置`Document Settings`--`Fonts`--`LaTeX font encoding: None fontenc`
在同一个页面,如果勾选`Use non-Tex fonts`,即可选择系统自带的字体,即可显示中文.

另外,`Document Settings`--`Language`中可设置语言,以及`xeTeX,utf-8`编码.
可以在`Insert`菜单栏中插入`beamer`特有的格式.

## 语法

### input与include

[Latex导入文件/input和/include方式](https://blog.csdn.net/OOFFrankDura/article/details/89644373)

`\input`命令可以改为`include`,
区别在于,`input`可以放在导言区和正文区,包含的内容不另起一页;
而`include`只能放在正文区,包含的内容另起一页.

另外`CJK`中还有`CJKinput`和`CJKinclude`命令.

### texdoc

`man` page 指出这些命令行选项等价于使用the command forms as `\scrollmode`,官方文档是`TeXBook`,或者输入

```powershell
texdoc texbytopic
```

自由选择(see chapter 32).

### newcommand 新命令

[LaTeX2e unofficial reference manual (October 2018)](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html)
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

```latex
\renewcommand{\cmd}[nargs]{defn}
\renewcommand{\cmd}[nargs]{defn}
\renewcommand{\cmd}[nargs][optargdefault]{defn}
\renewcommand*{\cmd}{defn}
\renewcommand*{\cmd}[nargs]{defn}
\renewcommand*{\cmd}[nargs][optargdefault]{defn}
```

+ 定义或重定义一个命令. 
参见关于 `\DeclareRobustCommand` 的讨论, 位于 `Class  and package commands`中.
+ 这两个命令的`*`号形式要求参数不能包含多段文字. (用 `plain TeX` 术语说,不能为`\long` ).

#### 参数说明

+ `cmd`:必选,命令名称. 用`\`开头. 且不能以`\end`开头,对于`\newcommand`,命令不能定义过.
对于`\renewcommand`,命令必须已经定义过.
+ `nargs`:可选,一个从`0`到`9`的整数. 指定命令接受的参数个数,包括可选参数. 忽略这个参数相当于设定为`0`,
意味着命令不接受参数. 如果重定义命令,新命令可以和旧命令的参数数目可以不一样.
+ `optargdefault`:可选. 如果这个参数存在,`\cmd`的第一个参数将是可选参数(可以是空字符串). 如果这个参数不存在,`\cmd`不使用可选参数. 也就是说,如果用`\cmd[optval]{...}`调用,`#1`将会被设置成`optval`; 如果用`\cmd{...}`调用,`#1`将会被设置成`optargdefault`. 两种情况下,必选参数都从`#2`开始.
忽略`[optargdefault]`与使用`[]`是不同的,前一种情况, `#1`被设置为`optargdefault`; 后一种情况,`#1`被设置为空字符串.
+ `defn`: 需要; 每次遇到`\cmd`就用`defn`替换. 参数`#1`,`#2`被替换成你提供的值. `Tex`会忽略跟在`\cmd`后面的空白. 如果你想要一个空白,使用`\cmd{}`或者使用显式的控制序列`'\cmd\ '`.
一个简单的定义新命令的例子:`\newcommand{\RS}{Robin Smith}`,文中的每个`\RS`会被`Robin Smith`替换.
重定义命令是类似的`\renewcommand{\qedsymbol}{{\small QED}}`.
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

### \RequirePackage \usepackage 区别

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

### 保留字符 Reserved characters

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

### 原文 verbatim

#### verb macro

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

第一个`\verb`的文字文本带有感叹号`！`.第二个取而代之的是使用加号`+`,因为感叹号是文字文本的一部分.

包围文字文本的单字符定界符`char`必须相同.
`\verb`或`\verb*`与`char`之间,`char`与文字文本之间,或文本与第二个`char`之间不能有空格
(上面的空格是为了区分不同部分).分隔符不能出现在后续文本中,文本中不能包含换行符.
`\verb*`形式的不同之处仅在于, 将空格以可见字符打印出来.

#### verbatim 环境

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

### \makeatletter, \makeatother

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

### 引用名言

[quotation, quote环境](https://blog.csdn.net/ProgramChangesWorld/article/details/51762789)

使用`quote`,`quotation`环境.

## 浮动体 图形

[liam.page](https://liam.page/2014/09/08/latex-introduction/)

由两个 graphics packages:

`graphics` : The `standard` graphics package.
`graphicx` :The `extended` or `enhanced`  graphics package

这两个包的区别在于可选参数给出的形式不同. 参数名称和必选参数是相同的.

插图和表格通常需要占据大块空间,所以在文字处理软件中我们经常需要调整他们的位置. `figure` 和 `table` 环境可以自动完成这样的任务; 这种自动调整位置的环境称作浮动体(`float`). 我们以 `figure` 为例.

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
该软件包还提供了`H` float修饰符选项,用来替换过时的`here`包.您可以使用`\floatplacement{figure}{H}`将其设置为默认. 例如：

```latex
\begin{figure}[H]
\centering
\includegraphics{a.jpg}
\end{figure}
```

### 子页面宽度resizebox

[一行代码解决LaTex表格过宽或过窄问题](https://blog.csdn.net/Rained_99/article/details/79389189#commentBox)

若表格过宽,则使用

```bash
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

若表格过窄,则使用

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

22.3.4 \resizebox

[22.3.4 \resizebox](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#index-_005ctabcolsep)

Synopses:

```latex
\resizebox{horizontal length}{vertical length}{material}
\resizebox*{horizontal length}{vertical length}{material}
```

给定一个大小(例如`3`厘米),请转换`material`使其达到该大小.  如果水平长度或垂直长度是一个感叹号`!` 就进行等比缩放.

此示例使图形的宽度为半英寸,并按相同的比例垂直缩放图形,以防止图形变形.

```bash
\ resizebox {0.5in} {!} {\ includegraphics {lion}}
```

未加星标形式 `\resizebox` 取垂直长度为`box`的高度,而带星标形式 `\resizebox*` 取其`height+depth`.
例如,使用 `\resizebox*{!}{0.25in}{\parbox{1in}{使此框同时具有高度和深度. }}`
使文本的高度+深度达到四分之一英寸.

您可以使用 `\depth`,`\height`,`\totalheight`和 `\width`来引用框的原始大小.
因此,使用 `\resizebox{2in}{\height}{Two inch}`将文本设置为两英寸宽,但保留原始高度.

***
8.23 tabular

[8.23 tabular](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#index-_005ctabcolsep)

`\tabcolsep`

长度是列之间间隔的一半.  默认值为`6pt`.  用 `\setlength`更改它.

### LaTeX对齐

[LaTeX 对齐问题](https://blog.csdn.net/lvchaoshun/article/details/50518271)
[latex23 doc](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#index-_005ccentering)

对齐的语法是 `{\centering 文字}`或者

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

***
LaTeX公式对齐

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

左对齐, 居中对齐, 右对齐的环境分别为`flushleft`, `center`和`flushright`.
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

产生一个宽度为`width`的文本框. 使用此命令可以使一小段文本框变成单个段落.该命令是`fragile`的(请参阅`\protect`).

```latex
\begin{picture}(0,0)
  ...
  \put(1,2){\parbox{1.75in}{\raggedright Because the graph is a line on
                         this semilog paper, the relationship is
                         exponential.}}
\end{picture}
```

内容被以文本模式处理
(请参见[`Modes`](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#Modes)),
因此`LaTeX`会中断换行以形成段落.但是它不会包含多个段落; 为此,请使用`minipage`环境(请参见`minipage`).

`\parbox`的选项(除了内容)与`minipage`的选项相同.为方便起见,此处提供了选项的摘要,但完整说明请参见[minipage](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#minipage).

有两个必需的参数.`width`是刚性长度(请参见`Lengths`).
它设置LaTeX将内容排版到其中的框的宽度.`contents`是放置在该框中的文本.它不应包含任何`paragraph-making`组件.

有三个可选参数,`position`, `height`, and `inner-pos`.
`position`给出`parbox`相对于周围材料的垂直对齐.
可能的值是`c`或`m`以使`parbox`的垂直中心与相邻线的中心对齐(这是默认值),
或`t`可以使`parbox`的顶行与周围材料的基线匹配,或者`b`匹配底线.

可选参数`height`覆盖框的自然高度.

可选参数`inner-pos`控制内容在`parbox`中的位置.它的默认值是`position`的值.
其可能的值为:`t`将内容放置在框的顶部,`c`将其放置在框的垂直中心,`b`将其放置在框的底部,
`s`将其垂直拉伸(为此,文本必须包含垂直可拉伸的空间).

### 代码环境

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

### 标注 callout

可以使用[callouts](https://www.ctan.org/pkg/callouts)包

## 方程

***
子方程

```latex
\begin{subequations}
```

创建 子方程 环境

***
二元运算符

`+` 号后面 加 `{}` , 变成二元运算符,强制排版,用在多行公式换行中

`=` 号也是同理

***
spacing in math mode

`/,`  `/:` `/;` `/quad` `/qquad`

***
占位符号Placeholders

使用占位符:如果完成的命令具有需要填写的选项,
则将`占位符`放在此位置,并可以通过使用`Ctrl + Right`/`Ctrl + Left`跳转到它们

### shell-escape

What does --shell-escape do?

[tex.stackexchange.com](https://tex.stackexchange.com/questions/88740/what-does-shell-escape-do)

有时候,能够从`tex`文件内部运行外部命令很有用:
例如,它可以使某些排版外部化,或使用诸如`bibtex`之类的外部工具. 可通过`\write18` tex primitive 达成.

问题在于它几乎允许所有事情.`tex`文件本来就是可移植的,
并且在编译第三方文件时不应该担心任何安全问题. 因此,默认情况下,此 primitive 处于禁用状态.

如果用户需要使用它,则需要明确告诉编译器,
他信任带有`shell`交互的文件的作者,而这正是可选的`--shell-escape`参数的目的.

### align环境如何对齐

多&情况下flalign和align环境是如何对齐的:
[对齐@CSDN](https://blog.csdn.net/yanxiangtianji/article/details/54767265)

根据 `&`(假设`n`个)一行被分为`n+1`列. 从左向右将列两个分为一组,第一组紧靠页左侧,最后一组紧靠页左侧,其余组均匀散布在整个行中. 当公式比较短时,中间可能会有几段空白.
需要注意的是:
每一组内部也是有对齐结构的!它们在所在位置上向中间对齐的,即第一列向右对齐,第二列向左对齐.
所谓紧靠页左/右是在进行了组内对齐调整之后,最长的一块紧靠上去. 也就是说对于长度不一两行,较短的那一行是靠不上去的.
如果总共有奇数个列,及最后一组只有一个列,则它右对齐到页右侧,即所有行的最后一列的右侧都靠在页右侧.

### 反向搜索设置 SumatraPDF

```code
"C:\Users\Thomas\AppData\Local\Programs\Microsoft VS Code\Code.exe"  "C:\Users\Thomas\AppData\Local\Programs\Microsoft VS Code\resources\app\out\cli.js" -r -g "%f:%l"
```

[使用VSCode编写LaTeX](https://blog.csdn.net/fenzang/article/details/99805315)

### BibTeX生成参考文献列表

[LaTeX技巧829:使用BibTeX生成参考文献列表](https://www.latexstudio.net/archives/5594)

***
bst 和 bib 格式简介

`BibTeX` 涉及到两种特有的辅助的文件格式: `bst` 和 `bib` .

`bst` 是 (B)ibliography (ST)yle 的缩写. 顾名思义,和 `sty` 文件是 `style` 的缩写一样,`bst` 文件控制着参考文献列表的格式.
在这里说的"格式",主要指参考文献列表中的编号, 排序规则, 对人名的处理(是否缩写), 月份的处理(是否缩写), 期刊名称的缩写等.

`bib` 是 `BibTeX` 定义的"参考文献数据库".
通常,我们会按照 `BibTeX` 规定的格式,向 bib 文件写入多条文献信息.
在实际使用时,我们就可以根据 bib 文件中定义的文献标记(label),
从数据库中调取文献信息,继而排版成参考文献列表.
值得注意的是,bib 是一个数据库,其中的内容并不一定等于 LaTeX 排版参考文献列表时的内容. 也就是说,如果 bib 数据库中有 10 条文献信息,并不一定说 LaTeX 排版出来的 PDF 文件中,参考文献列表里也一定有 10 条.
实际排版出来的参考文献列表中有多少条文献,实际是哪几条,具体由文中使用的 `\cite` 命令(以及 `\nocite` 命令)指定. 如果没有使用 `\cite` 命令调取文献信息,那么即使在 `bib` 文件中定义了文献信息,也不会展现在参考文献列表中.
很多人对此误解甚深,于是经常有人问道"为什么我在 bib 文件里写的文献,不出现在参考文献中"之类的问题.

***
BibTeX 的工作流程

介绍中提到,BibTeX 是一个参考文献格式化工具.
这个定义,给 BibTeX 的用处做了良好的界定:BibTeX 不是用来排版参考文献的,更不是个排版工具,它只是根据需要,按照( `bst` 文件规定的)某种格式,将( `bib` 文件中包含的)参考文献信息,格式化 为 LaTeX 能够使用的列表信息.

清楚了 `BibTeX` 需要做的事情(用软件工程的话说,就是清楚了 `BibTeX` 的 `API` ),我们就可以理清 `BibTeX` 的工作流程.

***
知道需要哪些参考文献信息

既然 `BibTeX` 会根据需要 格式化数据,那么首先要解决的问题就是:`BibTeX` 如何了解此处的"需求".  对 `BibTeX` 稍有了解的读者可能知道,运行 `BibTeX` 的命令行命令是:

`bibtex foo.aux` # 其中后缀名 `.aux` 可以省略

实际上,BibTeX 正是通过读取 `aux` 文件中的 `\citation{}` 标记,来确定用户需要哪些参考文献的.  举个例子,假设用户用 LaTeX 编译了以下代码:

```latex
\documentclass{article}
\begin{document}
bar\cite{baz}
\end{document}
```

如果该文件名为 `foo.tex`,那么就会生成 `foo.aux`. 其内容大约是:

```latex
\relax
\citation{baz}
```

在这里,`\relax` 表示休息一会儿,什么也不做; `\citation` 则是由 `tex` 文件中的 `\cite` 命令写入 `aux` 文件的标记.
它说明了:用户需要标记为 `baz` 的参考文献信息.
当 BibTeX 读入 `aux` 文件的时候,它就会记录下所有 `\citation` 命令中的内容(即文献标记——`label`),这样就知道了用户需要哪些参考文献信息.

***
了解文献列表格式以及读取文献数据库

当 BibTeX 清楚了用户需要哪些文献信息,接下来自然应该搞清楚用户想要什么样的格式.
而知道了格式之后,就可以从数据库中抽取所需的文献信息,按照格式准备数据.
为了讲清楚这个步骤,我们对上述 LaTeX 代码做些许的修改.

```latex
\documentclass{article}
\begin{document}
\bibliographystyle{unsrt}
bar\cite{baz}
\bibliography{foobar}
\end{document}
```

同样,我们将它保存为 foo.tex,经由 LaTeX 编译之后得到一个 `foo.aux` 文件,其内容如下:

```latex
\relax
\bibstyle{unsrt}
\citation{baz}
\bibdata{foobar}
```

简单的对比,不难发现:

`foo.tex` 中新增的 `\bibliographystyle{unsrt}` 与 `aux` 文件中的 `\bibstyle{unsrt}` 相对应.
`foo.tex` 中新增的 `\bibliography{foobar}` 与 `aux` 文件中的 `\bibdata{foobar}` 相对应.

根据命令的名字,我们很容易猜测各个命令的作用.

tex 文件中的 `\bibliographystyle` 指定了用户期待的参考文献列表格式文件,并将其写入 `aux` 文件备用,通过 `\bibstyle` 标记.
与此同时,`\bibliography` 命令则用 `\bibdata` 在 `aux` 文件中记录了参考文献数据库的名字(不含扩展名).
`在这里,unsrt` 是 `unsort` 的缩写,它对应着 `unsrt.bst` 文件,是大多数 TeX发行版自带的标准格式文件之一;
`foobar` 则对应着 `foobar.bib` 文件,该文件是用户自己编写或生成的参考文献数据库.

***
实际操作看看

我们假设上述 `foobar.bib` 文件有如下内容:

```bib
@BOOK{
    baz,
    title = {Dummy Book},
    publisher = {Egypt},
    year = {321},
    author = {The King}
}
```

我们在命令行执行以下操作:

```bash
latex foo.tex   # .tex 可以省略
bibtex foo.aux  # .aux 可以省略
```

我们会发现, `BibTeX` 生成了两个文件:`foo.bbl` 和 `foo.blg`.
其中 `foo.bbl` 的内容如下:

```latex
\begin{thebibliography}{1}

\bibitem{baz}
The King.
\newblock {\em Dummy Book}.
\newblock Egypt, 321.

\end{thebibliography}
```

显然,这就是一个标准的 `LaTeX` 环境. 对 `LaTeX` 参考文献排版稍有了解的读者可能知道 `thebibliography` 环境正是 `LaTeX` 中手工编排参考文献时使用的环境.
因此,`foo.bbl` 就是 `BibTeX` 格式化输出的结果,`LaTeX` 只需要将该文件的内容读入,就能在相应的位置输出格式化之后的参考文献列表了.
接下来,我们看看 `foo.blg` 的内容. `blg` 实际是 `BibTeX Log` 的缩写,亦即这是一个日志文件.

```bibtex
This is BibTeX, Version 0.99d (TeX Live 2015)
Capacity: max_strings=35307, hash_size=35307, hash_prime=30011
The top-level auxiliary file: foo.aux
The style file: unsrt.bst
Database file #1: foobar.bib
You've used 1 entry,
...
```

我们看到,BibTeX 打出的日志文件中,记录了读入 `aux/bst/bib` 文件的情况. 特别地,记录了所需的参考文献条目(entry)的数量(此处为 1).
日志中值得注意的地方是在提到 bib 文件时,使用了 `#1` 的标记. 既然存在 `#1`,那么合理推测也可以存在`#2`.
也就是说,BibTeX 可能支持两个或更多的 `bib` 数据库共同工作. 具体如何实现,请读者自己阅读相关资料(手册或 Google 检索)后实验.
紧接着,我们再执行一次 `LaTeX` :

```bash
latex foo.tex
```

首先,来看看 `aux` 文件会发生什么变化:

```latex
\relax
\bibstyle{unsrt}
\citation{baz}
\bibdata{foobar}
\bibcite{baz}{1}
```

相比上一次的 `foo.aux`,在读入 BibTeX 之后,LaTeX 向 `aux` 文件写入了更多的信息.
这里 `\bibcite{baz}{1}` 将 `baz` 这一参考文献标记(label)与参考文献编号(数字 `1`)绑定起来了.
接下来,我们看看 dvi 文件的内容:`foo.pdf`不难发现,由于读入了 `foo.bbl` 文件,参考文献列表已经正确展现出来了.
然而,正文中依然有一个问号.  实际上,LaTeX 需要 `aux` 文件中的 `\bibcite` 命令,将参考文献标记与参考文献编号关联起来,从而在 tex文件中的 `\cite` 命令位置填上正确的参考文献编号.
我们注意到,在我们第二次执行 `LaTeX` 命令编译之前,`foo.aux` 文件中是没有这些信息的,直到编译完成,这些信息才被正确写入. 因此,第二次执行 `LaTeX` 命令时,`LaTeX` 还不能填入正确的文献编号,于是就写入了一个问号作为占位符.
 解决这个问题的办法也很简单——此时 `aux` 文件中已经有了需要的信息,再编译一遍就好了.

```latex
latex foo.tex
```

如果没有意外,此时的 `foo.dvi` 文件应该看起来一切正常了.

***
小结

`BibTeX` 是一个参考文献格式化工具,它会根据需要,按照(bst 文件规定的)某种式,将(bib 文件中包含的)参考文献信息,格式化 为 LaTeX 能够使用的列表信息.

+ 正确使用 BibTeX 处理参考文献,需要先用 `(Xe/PDF)LaTeX` 编译 `tex` 文件,生成 `aux` 辅助文件.
+ 执行 `BibTeX` 将读入 `aux` 文件,搞清楚用户需要哪些文献.
+ 紧接着,BibTeX 根据 `aux` 文件中的内容,找到正确的 `bst` 和 `bib` 文件,并将参考文献信息格式化为 `LaTeX` 的 `thebibliography` 环境,作为 `bbl` 文件输出.
+ 第二次执行 `(Xe/PDF)LaTeX` 将会读入新生成的 `bbl` 文件,同时更新 `aux` 文件. 此时,参考文献列表将会正常展示,但是正文中的引用标记显示为问号.
+ 第三次执行 `(Xe/PDF)LaTeX` 将会读入 `bbl 文件和更新过后的 `aux` 文件. 此时,参考文献相关内容都正常显示.

因此,总的来说,想要正确使用 `BibTeX` 协同 `LaTeX` 处理参考文献,需要编译四次:

```bash
(xe/pdf)latex foo.tex   # 表示使用 latex, pdflatex 或 xelatex 编译,下同
bibtex foo.aux
(xe/pdf)latex foo.tex
(xe/pdf)latex foo.tex
```

### bibtex 常见问题

***
我希望将一条文献展示在参考文献列表中,但不想在正文中用 `\cite` 命令引用,怎么办?

首先,确保这条文献已经写入了 `bib` 文件.
其次,可以在 `\bibliography` 命令之前,用 `\nocite{label}`提示 `BibTeX` 调取这条文献.
我有很多条文献,都存在这样的情况. 每条文献逐一 `\nocite` 太繁琐了,有没有懒人适用的办法?
有的. `\nocite{*}`.

***
每次都要编译四次,我感觉懒癌又要发作了,有没有办法治疗?

有的. 可以尝试 `LaTeXmk`, `TeXify` 之类的自动化工具.

***
我对默认提供的 `bst` 文件的格式效果不满意,哪里能找到更多的 `bst` ?

现代 TeX 发行版都提供了多种 `bst` 可供选择,每个 `bst` 文件的格式, 适用范围, 使用条件都不一样,需要仔细甄别.
具体可以去安装目录下搜索试试.

***
有没有遵循国家标准的 `bst`?

有的

***
我找到的 bst,效果都不满意,怎么办?

你可以在命令行执行 `latex makebst`,制作一个符合自己要求的 `bst` 文件.
你需要回答大约 100 个关于参考文献列表效果的问题.

***
`bib` 文件怎么生成?

你可以手写,或者用 `JabRef` 之类的文献工具生成. 具体请自行 Google 检索,篇幅所限就不展开了.

***
我听说还有一个名为 `biblatex` 的工具,能介绍一下吗?

`biblatex` 与 `BibTeX` 是不同的工具,超出了本文的范围.

### 其他的参考文献包

#### cite

[cite – Improved citation handling in LaTeX](https://www.ctan.org/pkg/cite)

`cite`支持压缩,排序的数字引用列表,还处理各种标点符号和其他表示形式的问题,包括对断点的全面管理.
该软件包与`hyperref`和`backref`兼容.

支持给出多种`cite`格式:
`[?,Einstein,4–6,6–9]`
`[5a–5c] or [T1–T4])`
`information;12`

`cite` and `natbib` 不能同时使用.

#### natbib

[natbib – Flexible bibliography support ](https://www.ctan.org/tex-archive/macros/latex/contrib/natbib/)

natbib软件包是LATEX的扩展,允许作者年份(author–year)的引用形式,也支持数字引用.
可以方便的切换.

首先导入包,`\usepackage[sectionbib,square]{natbib}`
natbib 提供了三种新的格式,

+ plainnat.bst
+ abbrvnat.bst
+ unsrtnat.bst

通过在正文中调用以下命令使用这些格式:

`bibliographystyle{plainnat}`

natbib 特别定义了 `citet` (cite textual) and `citep`(cite parenthetical).
以及 `\citet*` and `\citep*` 可以打印出作者全名.
这些命令都可以接受一到两个参数,在引用前后输出额外文字.
可以同时引用多个参考文献.

```latex
\citet{jon90,jam91} --> Jones et al.  (1990); James et al.  (1991)
\citep{jon90,jam91} --> (Jones et al., 1990; James et al.  1991)
\citep{jon90,jon91} --> (Jones et al., 1990, 1991)
\citep{jon90a,jon90b} --> (Jones et al., 1990a,b)
```

These examples are for author–year citation mode.
In numerical mode,the results are different.

```latex
\citet{jon90} --> Jones et al.  [21]
\citet[chap.~2]{jon90} --> Jones et al.  [21, chap. 2]
\citep{jon90} --> [21]
\citep[chap.~2]{jon90} --> [21, chap. 2]
\citep[see][]{jon90} --> [see 21]
\citep[see][chap.~2]{jon90} --> [see 21, chap. 2]
\citep{jon90a,jon90b} --> [21, 32]
```

***
调用`\usepackage[options]{natbib}`的选项

+ `round` (默认)圆括号;
+ `square` 用于方括号;
+ `curly` 花括号;
+ `angle` 用于尖括号;
+ `semicolon` (默认)使用分号分隔多个引用;
+ `colon` 与`semicolon`相同,这是一个较早的术语错误;
+ `comma` 使用逗号作为分隔符;
+ `authoryear` (默认)作者年份( author–year)引文;
+ `numbers` 数字引用;
+ `super` 用于上标数字引用,类似`Nature`中的.
+ `sort` 将多个引文按其在参考文献列表中出现的顺序排序;
+ `sort&compress` 类似`sort`,但如果可能的话,还会压缩多个数字引用(如`3-6,15`);
+ `compress` 压缩而不排序,因此压缩仅在给定的引用按照数字升序时生效;
+ `longnamesfirst` 使任何参考文献的第一个引用都等同于已加星标的变体(完整作者列表),而随后的引用均是普通引用(缩写列表);
+ `sectionbib` 重新定义`\ thebibliography`来引用`\ section *`而不是`\ chapter *`;仅对带有`\ chapter`命令的类有效;  与`chapterbib`软件包一起使用;
+ `nonamebreak` 将所有作者的名字放在同一行中,导致`hbox`过多,但有助于解决一些`hyperref`问题;
+ `merge` 允许在`citation key`前面加上`*`前缀,并将此类引文的引用与先前引文的引用合并;
+ `elide` 合并参考文献后,去掉重复的共同要素,例如作者或年份;
+ `mcite`识别(并忽略)合并语法

#### lyx中使用 bib tex

菜单栏`Insert/List_Toc/Bibtex`添加 `bib`库文件,即可使用.

## 定理类环境 of elegant-note

### definition 定义

```latex
\begin{definition}{name}{}%%\ref{def:label}
%%some comment
\begin{equation}\begin{aligned}
%%\label{eq.6.1.2}

\end{aligned}\end{equation}
\end{definition}
```

### theorem 定理

```latex
\begin{theorem}{name}{}%%\ref{thm:label}
%%comment
\begin{equation}\begin{aligned}
%%\label{eq.6.1.2}

\end{aligned}\end{equation}
\end{theorem}
```

### lemma 引理

```latex
\begin{lemma}{name}{}%%\ref{lem:label}
%%some comment
\begin{enumerate}
%%%%%+++++++++++++++++++++++---------------------
\item aaa
%%%%%+++++++++++++++++++++++---------------------
\item bbb
\end{enumerate}
\end{lemma}
```

### corollary  推论

```latex
\begin{corollary}{name}{}%%\ref{cor:label}
%%some comment
\begin{enumerate}
%%%%%+++++++++++++++++++++++---------------------
\item aaa
%%%%%+++++++++++++++++++++++---------------------
\item bbb
\end{enumerate}
\end{corollary}
```

### proposition 命题

```latex
\begin{proposition}{name}{}%%\ref{pro:label}
%%some comment
\begin{equation}\begin{aligned}
%%\label{eq.6.1.2}

\end{aligned}\end{equation}
\end{proposition}
```

## 其他环境 of elegant-note

这几个都是同一类环境,区别在于

1. 示例环境(`example`), 练习(`exercise`)与例题(`problem`)章节自动编号
2. 注意(note),练习(exercise)环境有提醒引导符;
3. 结论(conclusion)等环境都是普通段落环境,引导词加粗.

## example 例子

```latex
\begin{example} %%some comment

\begin{equation}\begin{aligned}
%%\label{eq.6.1.2}
%%%%%+++++++++++++++++++++++---------------------

%%%%%+++++++++++++++++++++++
\end{aligned}\end{equation}
\end{example}
```

## note 附注

```latex
\begin{note} %%some comment

\begin{enumerate}
%%%%%+++++++++++++++++++++++---------------------
\item aaa
%%%%%+++++++++++++++++++++++---------------------
\item bbb
\end{enumerate}
\end{note}
```

## conclusion 结论

```latex
\begin{conclusion} %%comment
\begin{enumerate}
%%%%%+++++++++++++++++++++++---------------------
\item aaa
%%%%%+++++++++++++++++++++++---------------------
\item bbb
\end{enumerate}
\end{conclusion}
```

## 数学math

### 无序号公式

```latex
\begin{equation*}\begin{aligned}
%%\label{eq.6.1.2}
%%%%%+++++++++++++++++++++++---------------------

%%%%%+++++++++++++++++++++++
\end{aligned}\end{equation*}
```

### 有序号公式

```latex
\begin{equation}\begin{aligned}
%%\label{eq.6.1.2}
%%%%%+++++++++++++++++++++++---------------------

%%%%%+++++++++++++++++++++++
\end{aligned}\end{equation}
```

### 分段函数 cases

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

### 矩阵模板

```latex
\begin{equation}\begin{aligned}
%%\label{eq.6.1.2}
%%%%%+++++++++++++++++++++++---------------------
\begin{pmatrix}

\end{pmatrix}
%%%%%+++++++++++++++++++++++
\end{aligned}\end{equation}
```

参考 amsdoc_4.1_矩阵

除了LATEX的基本阵列环境外,amsmath软件包还为矩阵提供了一些环境.
`pmatrix`,`bmatrix`,`Bmatrix`,`vmatrix`和`Vmatrix`
分别具有`()`,`[]`,`{}`,`|`,`||`分隔符.
为了命名一致性,有一个`matrix`环境,没有定界符.

对于`array`环境,这并不是完全多余的.`matrix`环境都`array`环境的水平间距更经济.
另外,与阵列环境不同,您不必为任何`matrix`环境提供`column specifications`;
默认情况下,您最多可以有`10`个居中的列. (如果需要以一列或其他特殊格式左对齐或右对齐,则必须诉诸`array`.)

为了产生适用于文本的小矩阵,需要有一个`smallmatrix`环境,它比普通矩阵更适合于单个文本行.
必须提供定界符; 没有`p`,`b`,`B`,`v`,`V`版本的`smallmatrix`.
上面的例子可以这些生成

```latex
\bigl( \begin{smallmatrix}
a&b\\ c&d
\end{smallmatrix} \bigr)
```

### 分隔符

```latex
%%%%%+++++++++++++++++++++++---------------------
```

### 常用颜色声明

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

### 颜色包的使用

```latex
\usepackage{color,xcolor}
% predefined color---black, blue, brown, cyan, darkgray, gray, green, lightgray, lime, magenta, olive, orange, pink, purple, red, teal, violet, white, yellow.
\definecolor{light-gray}{gray}{0.95}    % 1.灰度
\definecolor{orange}{rgb}{1,0.5,0}      % 2.rgb
\definecolor{orange}{RGB}{255,127,0}    % 3.RGB
\definecolor{orange}{HTML}{FF7F00}      % 4.HTML
\definecolor{orange}{cmyk}{0,0.5,1,0}   % 5.cmyk
```

### 简单枚举

```latex
$(\lambda)=(\lambda_1,\lambda_2,\cdots \lambda_m)$
```

### 杨图 diagrams 模板

[ytableau – Many-featured Young tableaux and Young diagrams](https://www.ctan.org/pkg/ytableau)

该软件包提供了一些绘制 Young tableaux 和 Young diagrams 的功能,扩展了 `young` 和 `youngtab` 软件包,但提供了更多功能.
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

### 公式编号

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

如果要使用`左侧下标`,使用 `amsmath` 提供的`\sideset`.例如

```bash
%\usepackage{amsmath}
\[\sideset{_h}{_h}{\mathop{\langle\psi|Q_h|\psi\rangle}}\]
```

## Math accents

reference: [Math accents](http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#Math-accents)

`LaTeX` provides a variety of commands for producing accented letters in math.
These are different from `accents` in `normal text` (see `Accents`).

| command | description |
| ----- | ----- |
| `\acute` |      Math acute accent |
| `\bar` |        Math bar-over accent |
| `\breve` |      Math breve accent |
| `\check` |      Math hacek (check) accent |
| `\ddot` |       Math dieresis accent |
| `\dot` |        Math dot accent |
| `\grave` |      Math grave accent |
| `\hat` |        Math hat (circumflex) accent |
| `\mathring` |   Math ring accent |
| `\tilde` |      Math tilde accent |
| `\vec` |        Math vector symbol |
| `\widehat` |    Math wide hat accent |
| `\widetilde` |  Math wide tilde accent |

## 简单的规则

1. 空格:Latex 中空格不起作用.
1. 换行:用控制命令`\\\`,或`\newline`.
1. 分段:用控制命令`\par` 或空出一行.
1. 特殊控制字符: #,$, %, &, - ,{, }, ^, ~

### 换页

用控制命令`\newpage`或`\clearpage`

+ `\newpage`:  The `\newpage` 结束当前页.
+ `\clearpage`:The `\clearpage` 结束当前页面,并且强迫排版到目前为止`input`中的`图`和`表格`的浮动体(float).

### 连字符

连字符(`Hyphens`), 连接号(`En-dashes`), 破折号(`Em-dashes`)及减号(`Minus signs`)

+ 连字符 `-` 通常用来连接复合词,比如 `daughter-in-law`.
+ 连接号 `--` 通常用来表示范围,比如 `see pages 5--7`. 如果真的希望连续输入两个连字符,使用 `{-}{-}`.
+ 破折号 `---` 是一个正规的标点符号,用来表示转折或者承上启下. 要注意的是,破折号与其前后的单词之间不应该存在空格,例如 `A specter is haunting Europe---the specter of Communism.`
+ 排版中的减号应该比连字符要长,因此用来表示减号或者负号时,请严格使用数学模式 $-5$ 而不要使用文字模式 `-5`

### 计数器 Counters

所有的` LaTeX`自动编号都有一个`counter`与之关联.
`counter`的名称通常与与数字关联的`environment`或`command`的名称相同,只是`counter`的名称没有反斜杠` \`.
因此,与` \chapter`命令相关联的是` chapter`counter,该counter跟踪` chapter`编号.

以下是在` LaTeX`的标准文档类中用于控制编号的`counter`列表.

| | | | |
| ----- | ----- | ----- | ----- |
| part             | paragraph        | figure           | enumi |
| chapter          | subparagraph     | table            | enumii |
| section          | page             | footnote         | enumiii |
| subsection       | equation         | mpfootnote       | enumiv |
| subsubsection |  |  |  |

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

### enumerate

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

### enumitem

### itemize

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

### email链接

```latex
\href{mailto:michaelbibby@gmail.com}{给我电邮}}
```

### URL链接

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

### 查看所有的字体

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

### latex 注音符号

http://tug.ctan.org/tex-archive/info/latex2e-help-texinfo/latex2e.html#Accents

23.5 Accents

### latex 字体设置

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

### 编写样式表

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

### 更多字体

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

### 常用字号

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

|size| 10pt (default)| 11pt   |12pt |
|---|---|---|---|
| `\tiny` | `5pt`  | `6pt` | `6pt` |
| `\scriptsize` | `7pt`  | `8pt` | `8pt` |
| `\footnotesize` | `8pt`  | `9pt` | `10pt` |
| `\small` | `9pt`  | `10pt` | `11pt` |
| `\normalsize` | `10pt`  | `11pt` | `12pt` |
| `\large` | `12pt`  | `12pt` | `14pt` |
| `\Large` | `14pt`  | `14pt` | `17pt` |
| `\LARGE` | `17pt`  | `17pt` | `20pt` |
| `\huge` | `20pt`  | `20pt` | `25pt` |
| `\Huge` | `25pt`  | `25pt` | `25pt` |

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

### 特殊符号

[Symbol \sslash with XeLaTeX](https://tex.stackexchange.com/questions/341827/symbol-sslash-with-xelatex-and-unicode-math)

+ $\ell$ 用于和大小的$l$和数字$1$相区分
+ $\Re$
+ $\nabla$ 微分算子
+ `\sslash`:下标中表示平行分量的双斜线`//`.
+ $\circ$ : 可以用来当作角度的符号. 虽然国际单位更建议使用`siunitx`包处理.

***
`latinmodern-math.otf`中没有`\sslash`字形, 但总是可以从其他数学字体中借用缺少的字形. 例如下面的例子：

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

另一方面, 这个符号将与简单的斜线有很大的不同, 所以用标准的斜线来重新定义它可能会更好. 不过, 你必须在`unicode-math`完成工作后再进行重新定义.

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

+ ` \AtBeginDocument`宏：将它的内容附加到`\@begindocumenthook`后面, 后者紧随`\begin{document}`展开.
+ `\mathbin`:给出正确的间距, 让某个对象称为二元运算符.例如`a\mathbin{\text{op}} b`
+ `\mkern`: 给出水平间距.例如, `a\mkern18mu b`

### 数学字体

+ `mathbb`:blackboard bold,黑板粗体
+ `mathcal`:calligraphy(美术字),还有普通的`cal`
+ `mathrm`:math roman
+ `mathbf`:math 粗体字,还有一个`boldsymbol`,在`amsbsy`中,可以打出小写字母的粗体.
+ `\mathbbm`: `\usepackage{bbm}`,黑板粗体字母,`\mathbbm{1}`

花体`\mathcal`:`L,F,D,N`

+ $\mathcal{L}$ 常用来表示损失函数
+ $\mathcal{D}$ 表示样本集
+ $\mathcal{N}$ 常用来表示高斯分布;

[为阿拉伯数字和小写字母实现类似 \mathbb 的效果 ](https://liam.page/2017/01/08/arabic-numbers-or-lowercase-letters-in-blackboard-bold-doublestroke-font/)

需要用到bbm 字体

```latex
\usepackage{bbm}
```

这个包的文档里有很多字体的示范
[mathalpha](https://www.ctan.org/pkg/mathalpha)

### 其他数学字体

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

### 小写字母的数字花体

ref-2: [LaTeX小写花体字母](https://www.zhihu.com/question/26941177/answer/34623570)
ref-3: [查找任意符号的LaTeX指令](https://www.zhihu.com/question/26941177/answer/34626024)

拥有小写字母的数字花体字体当然也是存在的——比如`MathTime Pro 2`的`\mathcal`就支持小写,
需要付钱.  把一些正文字体借用于数学公式中是可能
把一些正文字体借用于数学公式中是可能的. 不过不推荐.
如果是为了区分不同的变量,可以考虑用`black letter`风格的`\mathfrak`.

推荐两个查找任意符号的LaTeX指令的方法

+ 查阅 [The Comprehensive LaTeX Symbol List](http://mirrors.ustc.edu.cn/CTAN/info/symbols/comprehensive/symbols-a4.pdf) ,这份资料很好,囊括众多宏包中的符号,就是查起来比较麻烦
+ 使用 [Detexify](http://detexify.kirelabs.org/classify.html) ,用鼠标直接画出你想要的符号即可查出该符号的`LaTeX`指令,灰常给力.

### 数学符号

| `latex` | appearance | 描述 |
| ----- | ----- |----- |
| `\oint` | $\oint$ | 环路积分 |
| `\approx` | $\approx$ | Almost equal to (relation) 双线约等于 |
| `\sim` | $\sim$ | 相似 单线约为 |
| `\ldots` | $\ldots$ | lying dots 省略号 |
| `\cdots` | $\cdots$ | centerd dots 省略号 |
| `\infty` | $\infty$ | infinity 无穷 |
| `\gg` | $\gg$ |greater greater 远远大于 |
| `\ll` | $\ll$ | less less 远远小于 |
| `\propto` | $\propto$ | 正比于 |
| `\in` | $\in$ | 属于 |
| `\notin` | $\notin$ | 不属于|
| `\ast` | $\ast$ | Asterisk operator, convolution, six-pointed (binary) 六角星|
| `\cong` | $\cong$ | Congruent (relation).  全等|
| `\dagger` | $\dagger$ | Dagger relation (binary). 厄米共轭  |
| `\ast` | $\ast$ | asterisk. 复共轭  |
| `\equiv` | $\equiv$ | Equivalence (relation).  恒等于 |
| `\subset` | $\subset$ | Subset (occasionally, is implied by) (relation) 子集|
| `\varphi` | $\varphi$ | Variant on the lowercase Greek letter 变型希腊字母 |
| `\zeta` | $\zeta$ | Lowercase Greek letter  |
| `\Zeta` | $\Zeta$ | Lowercase Greek letter  |
| `\xi` | $\xi$ | Lowercase Greek letter  |
| `\upsilon` | $\upsilon$ | Lowercase Greek letter  |
| `\mathsection` | $\mathsection$ | Section sign in math mode  |
| `\langle` | $\langle$ | Section sign in math mode 尖括号 |
| `\left| a \right|` | $\left\| a \right\|$ | absolute value 绝对值 |
| `\leftrightarrow` | $\leftrightarrow$ | 双向箭头 |
| `\widehat{}` | $\widehat{M}$ | 宽帽子 |
|`\sqrt[4]{8}`|$\sqrt[4]{8}$ | 四次根号`8` |

### 自定义数学符号

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

#### 求迹 Trace

```latex
\usepackage{amsmath}
\DeclareMathOperator{\Tr}{Tr}
```

### 定义配对的数学符号

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

### 微分符号

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
不过,这样一来也带来了一些副作用——在 `\mathop` 的作用下,`\mathrm{d}` 的基线发生了改变,不再与正常的数学变量保持在同一个基线上. 这也是不好的.
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

### 上下划线和大括号

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

### 可延展箭头

[amsmath](https://www.ctan.org/pkg/amsmath)

`amsmath`包中提供了可延展/伸缩的箭头 macro, 详见 page15: 4.9    Extensible arrows.

`\xleftarrow` and `\xrightarrow` 产生可以自动适应上下文字长度的箭头.这些命令接受一个可选参数作为下标, 一个必须参数作为上标, 可以为空.
类似有`\xLeftarrow`, `xRightarrow`.

### 下划线,中划线

[LaTeX文字的加粗, 斜体, 下划线, 删除线等](https://www.jianshu.com/p/a1838fa53882)

+ 加粗 `\textbf{文字}`
+ 斜体 `\emph{文字}`
+ 下划线 `\underline{文字}`
+ 删除线
删除线需要调用package:
`\usepackage{ulem}`
而后是:
+ `\sout{文字}` %删除线
+ `\uwave{文字}` %波浪线
+ `\xout{文字}` %斜删除线
+ `\uuline{文字}`  %双下划线

### 多重下标

[如何排版公式的多行下标](https://jingyan.baidu.com/article/59703552e0fae18fc1074043.html)

第一种方法:使用命令`\substack`,可以排版多重上标或下标,两行之间用`\\`分隔,居中显示. 例如:

```latex
\begin{equation}
\sum_{\substack{k_0,k_1,\dots>0\\   k_0+k_1+\dots=n}}   F(k_i)
\end{equation}
```

第二种方法

我们可以使用`subarray`环境来实现多行上下标,且可以自己选择对齐方式.

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

### pdf 书签

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

### Token not allowed

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

### 数学符号

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

### 在文中使用链接

使用宏包 `hyperref` 来制作

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

***
email链接

```latex
\href{mailto:michaelbibby@gmail.com}{给我电邮}
```

***
URL链接

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

### 国际单位制

`latex`中使用单位, 现在最好使用[siunitx](https://www.ctan.org/pkg/siunitx), 它是`LaTeX 3 `项目中的包.
`hepunits`会调用`physics`和`SIunits`, 这两个包会与`siunitx`冲突, 所以不要调用这些宏包.

`siunitx`的说明文档中有具体的用法例子：
`3.3Units` 章节列举了常用的命令, `3.6Unit abbreviations` 中有大量单位的缩写, 但是注意很多单位的定义只在`\unit{}`环境内才生效.
另外`siunitx`有第二版和第三版, 使用`texdoc siunitx`查看本地对应版本的文档, 两个版本的命令名称不同, 根据具体情况使用.

例如我本地安装的是第二版, 对应的命令为

```latex
\SI[mode=text]{1.23}{J.mol^{-1}.K^{-1}}
\ang{1;2;3} % 角度：1度2分3秒
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

### 页眉与页脚

[Latex的页脚和页眉](https://zhuanlan.zhihu.com/p/114676221)
[fancyhdr – Extensive control of page headers](https://www.ctan.org/pkg/fancyhdr)
[LaTex页码格式,第几页共几页 ](https://www.latexstudio.net/archives/7680.html)
[LaTeX入门(七)——页面设置](https://zhuanlan.zhihu.com/p/56405574)

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

### 行距

[LaTeX系列笔记(5)-行距](https://zhuanlan.zhihu.com/p/138408387)

首先, 行距就是相邻两行文字之间的距离. 行距的调节一般使用倍数, 比如两倍行距.
而`单倍行距`又根据字体, 字号, 软件的不同而改变(不同软件中有不同的定义, 没有一个通用的值).

在 LaTeX 里面也有这些概念, 在你定义字号的时候, `单倍行距`也随之确定. 更改时, 我们更改的是`单倍行距`的倍数.
比如在 LaTeX 中 `10` 号字(无论字体), 对应的单倍行距是 `12` 磅. 修改行距倍数的方法有两个：

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

被 `spacing` 环境框住的地方行距就会改变. `spacing` 还有一些奇怪的功能：
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
\setlength{\normalbaselineskip}{20pt}    % 改变“单倍行距”. 不推荐, 因为字号一改就得重新定义.
```

如果你想把行距搞得和 `Word` 很像, 需要改变每个字号下的`单倍行距`大小. 参考[LaTeX 设置的行距与 Word 的行距如何对应？](https://www.zhihu.com/question/62327906/answer/197899935)

### 添加水印或者背景图片 eso-pic

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

### 添加水印 tikz pgfpages

page 28; 1.3 Utility Packages
page 1013; 91 Page Management

也可以考虑`tikz`中的`pgfpages`包.
`pgfpages`软件包用于将几个页面组合成一个单一的页面. 它提供了用于将几个 `虚拟页` 组合成`物理页`的命令.
其原理是, 每当`TeX`准备好将一个页面`shipout`的时候, `pgfpages`就会中断这种输出, 而把要输出的页面存储在一个特殊的`盒子`里.
当以这种方式积累了足够多的 `虚拟页面`时, 它们就会被按比例缩小并排列在 `物理页面`上. 然后真正输出.

这种机制允许你直接在`LaTeX`内部创建"一页两面"的文档版本, 而不需要使用任何外部程序.
然而, `pgfpages`的作用远不止这些. 你可以用它来在页面上添加`标识`和`水印`, 在一页上最多打印`16`面, 为页面添加边框, 等等.
