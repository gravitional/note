# latex 基本概念

## 简单的规则

[LaTeX: 打出波浪线~](https://www.jianshu.com/p/b891a63948a6)

1. 空格:`Latex` 中空格不起作用.
2. 换行:用控制命令`\\`,或`\newline`.
3. 分段:用控制命令`\par` 或空出一行.
4. 换页:用控制命令`\newpage`或`\clearpage`
5. 特殊控制字符: `#`,`$`, `%`, `&`, `-` ,`{}`, `^`, `~`

方式1: 一般文字环境下, `\textasciitilde`
方式2: 公式环境下, `$\sim$`
区别: 用公式环境下打出, 会大一些!

## 帮助文档

查看 LaTeX 的帮助文档,可以直接用 `texdoc pkg`. 官方文档是`TeXBook`, 输入

```bash
texdoc texbytopic
```

`texdoc` - find & view documentation in TeX Live

### 语法

```bash
texdoc [OPTION]... NAME...
texdoc ACTION
```

+ 描述; 对于给定的名字(可以多个),试图寻找合适的TeX文档. 或者,执行某些动作并退出.

动作:

+ `-h`, `--help` 打印帮助信息.
+ `-V`, `--version`打印版本号
+ `-f`, `--files` 打印使用的配置文件
+ `--just-view file` 展示文件,给出绝对路径(不搜索)

选项:

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

## PowerShell

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

## 预设的 texmf 树概述

[2.3 Overview of the predeﬁned texmf trees](https://www.tug.org/texlive/doc/texlive-en/texlive-en.html)

本节列出了 预设的 变量,
这些变量指定了系统使用的 `texmf树` 和它们的目的, 以及 `TeX Live` 的默认布局.

`tlmgr conf` 命令将显示这些变量的值, 这样你就可以很容易地找出它们与对应的具体目录.

所有的 `目录树`, 包括个人的, 都应该遵循 `TeX目录结构`,
[TDS](https://tug.org/tds), 及其所有的子目录, 否则可能找不到文件.

第3.4.6节(第41页)对此有更详细的描述.
这里的顺序与搜索树的顺序相反, 也就是说, 列表中 较晚的树 `覆盖` 较早的树.

+ `TEXMFDIST`;

这棵树几乎保存了原发行版中所有的文件--`配置文件`, `脚本`, `软件包`, `字体` 等
(主要的例外是每个平台的可执行文件, 它们被保存在一个同级目录 `bin/` 中).

+ `TEXMFSYSVAR`;

`texconfig-sys`, `updmap-sys` 和 `fmtutil-sys` 以及 `tlmgr` 使用的(site-wide)树,
用来存储(缓存)运行时数据, 如 `格式文件` 和生成的 `map 文件`.

+ `TEXMFSYSCONFIG`;
`texconfig-sys`, `updmap-sys` 和 `fmtutil-sys` 等工具所使用的(site-wide)树, 用来存储修改后的配置数据.

+ `TEXMFLOCAL`;

管理员可以用来在全系统安装额外的或更新的 `宏`, `字体` 等的树.

+ `TEXMFHOME`;

用户可以用来安装额外的或更新的 `宏`, `字体` 等的树.
这个变量展开时将动态调整, 为每个用户找到他们自己的单独目录.

+ `TEXMFVAR`;

`texconfig`, `updmap-user` 和 `fmtutil-user` 使用的(个人)树,
用于存储(缓存)运行时数据, 如格式文件和生成的地图文件.

+ `TEXMFCONFIG`

`texconfig`, `updmap-sys` 和 `fmtutil-sys` 等工具所使用的(个人)树,
用于存储修改后的配置数据.

+ `TEXMFCACHE`

`ConTeXt MkIV` 和 `LuaLaTeX` 用来存储(缓存)运行时数据的树;
默认为 `TEXMFSYSVAR`, 或者(如果它不可写), `TEXMFVAR`.

### 默认的布局

```yaml
全系统根目录
可以跨越多个 TeX Live 版本( Unix 上默认为 /usr/local/texlive).

2020: 以前的版本.
2021: 当前的版本.
        bin:
            i386-linux:
                GNU/Linux二进制文件(32位)
            ...
            universal-darwin:
                Mac OS X二进制文件
            x86_64-linux:
                GNU/Linux二进制文件(64位)
            win32:
                Windows二进制文件
        texmf-dist:
            TEXMFDIST 和 TEXMFMAIN
        texmf-var:
            texmfsysvar, texmfcache
        texmf-config:
            TEXMFSYSCONFIG
texmf-local:
    TEXMFLOCAL, 打算从一个版本保留到另一个版本.

用户的主目录:
    (`$HOME` 或 `%USERPROFILE%`)
    .texlive2020:
        为以前的版本私人生成和配置的数据.
    .texlive2021:
        当前版本的私人生成和配置数据.
        texmf-var:
            TEXMFVAR, TEXMFCACHE
        texmf-config:
            TEXMFCONFIG
    texmf:
        TEXMFHOME 个人宏, 等等.
```

## 章节重编号

[latex排版时, 如何使每个part中的section分别重新编号?](https://www.zhihu.com/question/531891867)

在 `\section{xx}` 前一行加入下述语句:

```latex
\setcounter{section}{0} % 对 section 重编号
\setcounter{chapter}{0} %类似地, 对 chapter 重编号
```

## 如何去掉章节与公式编号关联

[如何去掉章节与公式编号关联](https://www.latexstudio.net/archives/2111.html)

熟悉TeX的用户自然会找到book基础类里面的重置编号的命令, `\@addtoreset {equation}{chapter}`,
对于初级用户各种方式修改都比较繁琐, 推荐大家用一个 `chngcntr` 包解决这一问题.

```latex
\usepackage{chngcntr}
\counterwithout{equation}{chapter} % 解除关联
\counterwithin{equation}{chapter}  % 设置新的关联
```

简单的一个命令即可解除编号关联了.
