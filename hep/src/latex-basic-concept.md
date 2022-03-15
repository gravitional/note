# latex 基本概念

## 简单的规则

1. 空格:`Latex` 中空格不起作用.
1. 换行:用控制命令`\\`,或`\newline`.
1. 分段:用控制命令`\par` 或空出一行.
1. 换页:用控制命令`\newpage`或`\clearpage`
1. 特殊控制字符: `#`,`$`, `%`, `&`, `-` ,`{}`, `^`, `~`

## 帮助文档

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

## 转义到命令行

What does --shell-escape do?

[tex.stackexchange.com](https://tex.stackexchange.com/questions/88740/what-does-shell-escape-do)

有时候,能够从`tex`文件内部运行外部命令很有用:
例如,它可以使某些排版外部化,或使用诸如`bibtex`之类的外部工具. 可通过`\write18` tex primitive 达成.

问题在于它几乎允许所有事情.`tex`文件本来就是可移植的,
并且在编译第三方文件时不应该担心任何安全问题. 因此,默认情况下,此 primitive 处于禁用状态.

如果用户需要使用它,则需要明确告诉编译器,
他信任带有`shell`交互的文件的作者,而这正是可选的`--shell-escape`参数的目的.

## 反向搜索设置 SumatraPDF

```code
"C:\Users\Thomas\AppData\Local\Programs\Microsoft VS Code\Code.exe"  "C:\Users\Thomas\AppData\Local\Programs\Microsoft VS Code\resources\app\out\cli.js" -r -g "%f:%l"
```

[使用VSCode编写LaTeX](https://blog.csdn.net/fenzang/article/details/99805315)

## 报错示例1

```latex
./chapter-6.tex:58: Undefined control sequence.
l.58             \partial_\mu - i \eofphi
                                           A_\mu(x)
Output written on temp/main.xdv (21 pages, 329924 bytes).
SyncTeX written on temp/main.synctex.gz.

Transcript written on temp/main.log.
```

## 简短版latexmk

```powershell
$mk_message=(latexmk -f -xelatex); Write-Output ("*" * 90);$mk_message | Where-Object {$_ -like "*tex:*"}
```

## 详细版latexmk

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

## 报错示例2

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

## 清理latex 辅助文件powershell

```powershell
remove-item -Path ('.\*.aux','.\*.lof','.\*.log','.\*.lot','.\*.fls','.\*.out','.\*.toc','.\*.fmt','.\*.fot','.\*.cb','.\*.cb2','.\*.ptc','.\*.xdv','.\*.fdb_latexmk','.\*.synctex.gz','.\*.ps1','.\*.bib','.\*.bbl','.\*.blg')
```

```powershell
remove-item -Path ($tepath+'*.aux',$tepath+'*.lof',$tepath+'*.log',$tepath+'*.lot',$tepath+'*.fls',$tepath+'*.out',$tepath+'*.toc',$tepath+'*.fmt',$tepath+'*.fot',$tepath+'*.cb',$tepath+'*.cb2',$tepath+'*.ptc',$tepath+'*.xdv',$tepath+'*.fdb_latexmk',$tepath+'*.synctex.gz',$tepath+'*.ps1')
```

## latex 编译模式

[如何加速 LaTeX 编译](https://zhuanlan.zhihu.com/p/55043560)

不同的编译模式也有细微的影响.
经过测试,使用批处理模式(`batchmode`)速度要优于默认的模式(不加参数)和其他一些模式(比如 `nonstopmode` 和 `scrollmode`),这是因为批处理模式在编译和执行阶段是静默的,不输出任何信息,因此要快上一些.

## 清理辅助文件

删除本层目录下除了源文件的`latex`辅助文件,只保留 `*.tex`,`*.pdf`,`*.bib`

```bash
temp_a=$(find . -mindepth 1 -maxdepth 1 -type f \( -not -name "*.pdf" \) \( -not -name "*.tex" \) \( -not -name "*.bib" \) -print0)
if [[ ${temp_a} != '' ]]; then
    echo -n ${temp_a} | xargs --null rm
fi
```

或者使用 `latexmk` 命令行工具:

```bash
latexmk -c
```

+ 清理除了`dvi`, `postscript` 和`pdf` 以外的, 所有由`latex`和`bibtex`或`biber`生成的可重新生成的文件.
这些文件是`log`文件,`aux`文件,`latexmk`创建的关于源文件信息的`database`文件以及在`@generated_exts`配置变量中指定的扩展名的文件的组合.
+ 另外,将删除`$ clean_ext`和`@generation_exts`配置变量指定的文件.
+ 此清理不是`regular make`. 如果要先清理然后再进行`make`,请参阅`-gg`选项.

+ `.bbl`文件的处理:
    + 如果`$bibtex_use`设置为`0`或`1`,则`bbl`文件始终被视为不可重新生成.
    + 如果`$bibtex_use`设置为`1.5`:则取决于是否存在`bib`文件.
    + 如果存在`bib`文件,则`bbl`文件可重新生成,并在清理中被删除.
    + 如果不存在`bib`文件,则`bbl`文件将被视为不可重新生成,因此不会被删除.
    + 相反,如果`$bibtex_use`设置为`2`,则`bbl`文件始终被视为可重新生成的文件,并在清理时将其删除.

+ `latexmk -C`;
与`-c`选项相同,只是增加了`dvi`,`postscript`和`pdf`文件以及`$clean_full_ext`配置变量中指定的文件.

## pdftex/xetex --help

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

## Latex in PowerShell

```pwsh
Invoke-Expression $("lualatex" + " " + "-halt-on-error " + "-output-directory=temp -shell-escape -interaction=nonstopmode " + "test.tikz.tex" ) > ./null
```

## 查看文档使用的所有文件

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

它记录 `mymacros.tex` 文件的细节, 但如果你用`TeX`的原始语法来使用` \input`:

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
\usepackage{fontspec}
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

但有一个问题, `\boldsymbol` 是 `AMS` 系列包中的 `amsbsy` 定义的宏,可以产生粗体数学符号.
如果用 `fontspec` 设置数学字体为 `latinmodern-math.otf` 字体时,没有粗体效果,而是变成直立体,原因不明.可以通过使用`unicode-math`包解决,见下文.

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

## fontenc

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

### lyx 字体选择

在`lyx`中,使用 `xelatex` 进行编译,可以设置`Document Settings`--`Fonts`--`LaTeX font encoding: None fontenc`
在同一个页面,如果勾选`Use non-Tex fonts`,即可选择系统自带的字体,即可显示中文.

另外,`Document Settings`--`Language`中可设置语言,以及`xeTeX,utf-8`编码.
可以在`Insert`菜单栏中插入`beamer`特有的格式.
