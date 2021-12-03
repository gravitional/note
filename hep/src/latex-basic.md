# latex.md

## basic

### 简单的规则

1. 空格:`Latex` 中空格不起作用.
1. 换行:用控制命令`\\`,或`\newline`.
1. 分段:用控制命令`\par` 或空出一行.
1. 换页:用控制命令`\newpage`或`\clearpage`
1. 特殊控制字符: `#`,`$`, `%`, `&`, `-` ,`{}`, `^`, `~`

### 帮助文档

查看 LaTeX 的帮助文档,可以直接用 `texdoc pkg`. 官方文档是`TeXBook`, 输入

    texdoc texbytopic

`texdoc` - find & view documentation in TeX Live

+ 语法:

    ```bash
    texdoc [OPTION]... NAME...
    texdoc ACTION
    ```

+ 描述

    对于给定的名字(可以多个),试图寻找合适的TeX文档. 或者,执行某些动作并退出.

+ 动作:
    + `-h`, `--help` 打印帮助信息.
    + `-V`, `--version`打印版本号
    + `-f`, `--files` 打印使用的配置文件
    + `--just-view file` 展示文件,给出绝对路径(不搜索)

+ 选项:
    + `-w`, `--view` 使用查看模式,打开文档阅读器(默认)
    + `-m`, `--mixed` 使用混合模式(查看或者列表)
    + `-l`, `--list` 使用列表模式:列出搜索结果.
    + `-s`, `--showall` 展示所有模式,包括"坏"的结果
    + `-i`, `--interact` 使用交互菜单(默认)
    + `-I`, `--nointeract`使用plain列表,不需要交互
    + `-M`, `--machine` 机器可读的结果

### 转义到命令行

What does --shell-escape do?

[tex.stackexchange.com](https://tex.stackexchange.com/questions/88740/what-does-shell-escape-do)

有时候,能够从`tex`文件内部运行外部命令很有用:
例如,它可以使某些排版外部化,或使用诸如`bibtex`之类的外部工具. 可通过`\write18` tex primitive 达成.

问题在于它几乎允许所有事情.`tex`文件本来就是可移植的,
并且在编译第三方文件时不应该担心任何安全问题. 因此,默认情况下,此 primitive 处于禁用状态.

如果用户需要使用它,则需要明确告诉编译器,
他信任带有`shell`交互的文件的作者,而这正是可选的`--shell-escape`参数的目的.

### 反向搜索设置 SumatraPDF

```code
"C:\Users\Thomas\AppData\Local\Programs\Microsoft VS Code\Code.exe"  "C:\Users\Thomas\AppData\Local\Programs\Microsoft VS Code\resources\app\out\cli.js" -r -g "%f:%l"
```

[使用VSCode编写LaTeX](https://blog.csdn.net/fenzang/article/details/99805315)

### 报错示例1

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

### 报错示例2

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
刘海洋 的<LaTeX 入门>中有名为 `从错误中救赎` 的章节,
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

| options                  | effect                                                                       |
| ------------------------ | ---------------------------------------------------------------------------- |
| `[-no]-file-line-error`  | disable/enable file:line:error style messages                                |
| `-fmt=FMTNAME`           | use FMTNAME instead of program name or a %& line                             |
| `-halt-on-error`         | stop processing at the first error                                           |
| ------------------------ | ------------------------                                                     |
| `-interaction=STRING`    | set interaction mode (STRING=batchmode/nonstopmode/scrollmode/errorstopmode) |
| ------------------------ | ------------------------                                                     |
| `-output-comment=STRING` | use STRING for XDV file comment instead of date                              |
| `-output-directory=DIR`  | use existing DIR as the directory to write files in                          |
| `-output-driver=CMD`     | use CMD as the XDV-to-PDF driver instead of xdvipdfmx                        |
| `-no-pdf`                | generate XDV (extended DVI) output rather than PDF                           |
| ------------------------ | ------------------------                                                     |
| `[-no]-parse-first-line` | disable/enable parsing of first line of input file                           |
| ------------------------ | ------------------------                                                     |
| `[-no]-shell-escape`     | disable/enable \write18{SHELL COMMAND}                                       |
| `-shell-restricted`      | enable restricted \write18                                                   |
| ------------------------ | ------------------------                                                     |
| `-help`                  | display this help and exit                                                   |
| `-version`               | output version information and exit                                          |

```bash
pdflatex -halt-on-error file.tex 1 > /dev/null
[[ $? -eq 1 ]] && echo "msg in case of erros" && exit
```

Email bug reports to <xetex@tug.org>.

### powershell中的latex

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

请注意, `\listfiles`只记录由 `标准LaTeX`机制( `\documentclass`, `\usepackage`, `\include`, `\includegraphics`等)输入的东西.
`\input`命令, 经`LaTeX`修改后, 用`LaTeX`的语法表示为:

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

[GUST]: http://www.gust.org.pl/projects/e-foundry/lm-math
[fontspec]: https://ctan.org/pkg/fontspec
[xeCJK]: https://www.ctan.org/pkg/xecjk

[在 LaTeX 中使用中文](https://jdhao.github.io/2018/03/29/latex-chinese.zh/)
[LaTeX数学公式的默认字体是什么](https://www.zhihu.com/question/30058577/answer/46612848).

`LaTeX` 默认的文章类中的字体是 `Computer Modern Math`(`LaTeX`), `Latin Modern Math`(`XeTeX`). 
字体文件的位置可以用`kpsewhich`查看. 在安装`TeXLive`的时候会自动安装.
如果没有安装的话,[GUST][] 可以下载`Latin Modern Math`字体,以及其他字体.

```bash
kpsewhich latinmodern-math.otf
/usr/share/texmf/fonts/opentype/public/lm-math/latinmodern-math.otf
```

`kpsewhich`的介绍可以查看[The TeX Live Guide—2021 ](https://www.tug.org/texlive/doc/texlive-en/texlive-en.html)
一般可以使用[fontspec][] 包控制西文字体和数学字体.用法大概如下:

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

对中文字体的选择可以通过 [xeCJK][] 完成:

```latex
%\usepackage{xeCJK}% 可以用来实现中文断行
\usepackage{ctex} % 中文排版通用框架, 汉字, 标点, 字体字号, 标题, 版式, 数字日期转换
% 设置中文字体
\setCJKmainfont[Mapping=tex-text]{Noto Sans CJK SC}
\setCJKsansfont[Scale=0.7,Mapping=tex-text]{Source Han Sans SC}
\setCJKmonofont[Scale=0.7]{Noto Sans Mono CJK SC}
% 中文断行设置
\XeTeXlinebreaklocale "zh"
\XeTeXlinebreakskip = 0pt plus 1p
```

### unicode 数学字体

[mathspec]: https://ctan.org/pkg/mathspec
[unicode-math]: https://ctan.org/pkg/unicode-math
[change the math italic font in XeTeX/fontspec]: https://tex.stackexchange.com/questions/11058/how-do-i-change-the-math-italic-font-in-xetex-fontspec
[Error:Extended mathchar used as mathchar]: https://tex.stackexchange.com/questions/431013/error-extended-mathchar-used-as-mathchar-when-using-bm
[Theunicode-mathpackage]: https://mirrors.bfsu.edu.cn/CTAN/macros/unicodetex/latex/unicode-math/unicode-math.pdf

其他指定数学字体的包有: [mathspec][], 以及 [unicode-math][].
`stackexchange`上有关于 [change the math italic font in XeTeX/fontspec][] 的讨论. 作者给出的示例代码为:

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

载入`unicode-math`包,并使用`\boldsymbol`时会报错: [Error:Extended mathchar used as mathchar][],
解决方案是不使用`\bm`,`\boldsymbol`命令,而使用`\symbf`,`\symcal`等命令, 见[Theunicode-mathpackage][].
`unicode-math`引入了一些新的命令,例如:

```latex
\symbb, \symbbit, \symcal, \symscr, \symfrak, \symsfup, \symsfit,
\symbfsf, \symbfup, \symbfit, \symbfcal, \symbfscr, \symbffrak, \symbfsfup, \symbfsfit
```

用来表示单个粗体数学符号, 跟粗体普通文字是不同的,
粗体普通文字使用`latex`中通常的`\mathbb, \mathbbit, \mathcal`等命令. 例子是:

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

## BibTeX生成参考文献列表

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
当 BibTeX 读入 `aux` 文件的时候,它就会记录下所有 `\citation` 命令中的内容(即文献标记 -- `label`),这样就知道了用户需要哪些参考文献信息.

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
 解决这个问题的办法也很简单 -- 此时 `aux` 文件中已经有了需要的信息,再编译一遍就好了.

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

### cite,其他的参考文献包

[cite – Improved citation handling in LaTeX](https://www.ctan.org/pkg/cite)

`cite`支持压缩,排序的数字引用列表,还处理各种标点符号和其他表示形式的问题,包括对断点的全面管理.
该软件包与`hyperref`和`backref`兼容.

支持给出多种`cite`格式:
`[?,Einstein,4–6,6–9]`
`[5a–5c] or [T1–T4])`
`information;12`

`cite` and `natbib` 不能同时使用.

### natbib

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

### lyx中使用 bib tex

菜单栏`Insert/List_Toc/Bibtex`添加 `bib`库文件,即可使用.

## latex 语法

### input与include

[Latex导入文件/input和/include方式](https://blog.csdn.net/OOFFrankDura/article/details/89644373)

`\input`命令可以改为`include`,
区别在于,`input`可以放在导言区和正文区,包含的内容不另起一页;
而`include`只能放在正文区,包含的内容另起一页.

另外`CJK`中还有`CJKinput`和`CJKinclude`命令.

### newcommand 新命令

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

### 参数说明

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

### verb 宏, 原文

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

### verbatim 环境

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

### 标注 callout

可以使用[callouts](https://www.ctan.org/pkg/callouts)包

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

### 页眉与页脚

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

### 行距

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
