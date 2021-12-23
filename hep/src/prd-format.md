# aps 格式

如果安装了`TeXLive`发行版, 不管`Linux`,`Windows`,`Mac`, 直接在命令行运行`texdoc apsguide4`查看`aps`期刊的`Author Guide`,
或者运行`texdoc -l revtex`进行更详细的选择.

## 三种格式

在`\documentclass[aps,prd,superscriptaddress]{revtex4-2}`中可以使用三个预设的整体格式选项.

+ `reprint`: 接近出版效果
+ `preprint`: 预印本格式, 加大行距, 便于修改.
+ `twocolumn`: 双栏样式.

+ `REVTEX 4.2` 设计成自动切换单双栏,只需在设置中更改选项.
+ `preprint` 做三件事: 增加字号到`12 pt`, 增加行距,改变格式到单栏.
+ 手稿提交到 APS 时,应该使用 letter size paper.

## FRONT MATTER 前置段落

front matter (title, authors,affiliations, abstract, etc)

### 标题

添加标题使用 `\title`宏. 使用双反斜线 `\\` 在标题中换行.

### 作者,机构,合作组

Authors, affiliations, and collaborations

`REVTEX 4.2` 会自动处理好作者和机构的成组问题.  如果使用 `superscript`格式给`affiliations`编号, Please follow these guidelines:

+ 对于每个作者名,使用`\author`宏,`REVTEX`会自动加上所有逗号和`and`
+ 如果 family name 有多个名字,或者不是 last name, 使用 `\surname` 详细制定作者的 family name,
+ 用 `\email`宏指定电子邮件地址,不要用 `\thanks`, 参数只能出现电子邮件地址本身.
+ 用 `\homepage` 指定主页,同理不要用  `\thanks`, 参数只能出现 `URL`.
+ 用 `\altaffiliation` 指定机构,同理不要用  `\thanks`
+ 上面放不下的,用 `\thanks`.
+ 对于每一个机构,用一个单独的`\affiliation`.
+ `Superscripts`连接作者和对应的机构,将会通过 `superscriptaddress` 选项自动完成,不需要手动指定.
+ 合作组请用 `\collaboration` 指定,而不是`\author`

例如:

```latex
\title{小猪佩奇的做法}

\author{王钢}
\email{wangg@ggg.com}
\affiliation{宽菜店}
\affiliation{比利比利}

\author{华农兄弟}
\email{huanong@ggg.com}

\affiliation{竹鼠快乐屋}
\affiliation{红烧肉中暑了}

\begin{abstract}
    太抽象了...
\end{abstract}
\maketitle
```

#### 摘要- Abstract

摘要需要通过 `abstract` 环境指定, `abstract` 必须出现在 `\maketitle` 命令之前. `revtex4.2 `现在支持结构性摘要,用法如下:

```latex
\begin{abstract}
    \begin{description}
        \item[Background] This part would describe the context needed to understand what the paper is about.
        \item[Purpose] This part would state the purpose of the present paper.
        \item[Method] This part describe the methods used in the paper.
        \item[Results] This part would summarize the results.
        \item[Conclusions] This part would state the conclusions of the paper.
    \end{description}
\end{abstract}
```

### 参考文献和脚标

建议使用 `BibTEX` 准备参考文献.

如果用 `BibTEX` 的话, `APS Author Guide for REVTEX 4.2`建议你把`.bib`编译生成的 `.bbl`  文件直接`included`到 `.tex`主文件中.参见`IV.REFERENCES AND FOOTNOTES`.
在`revtex 4.2`中,`BibTEX`风格被修改成:`杂志-文章名`
不管用不用`BibTEX`,应该满足以下条件:

1. 使用 `\cite` and `\bibitem` 宏创建和引用 参考文献,
2. `revtex4.2`提供一种新的语法,组合多个引用到一个条目中,或者在参考文献前或后放入额外文字.
脚注用 `\footnote` 宏指定, `revtex4.2`把脚注放在 `bibliography` 中.需要运行 `BibTEX` 让 `footnotes` 出现
3. 不要用自定义的 `\footnotemark` 或者 `\footnotetex`
4. 参考文献风格应该满足 `Physical Review Style Guide`,若使用 `BibTEX` 则能自动保证.
5. `eprint`标识符应该使用`\eprint`宏引入,如果有`eprint`域,`BibTEX` 会自动处理好.

关于`REVTEX4.2`的`APS BibTEX`样式的新功能, 请参见`REVTEX 4.2 Author's Guide`.
包括支持引用数据集,使用`DOI`代替页码的期刊. 和使用`year`和`issue`而不是`volume`来唯一地标识文章的期刊.

### bibtex 用法

`bibtex` 一般附在 Latex 发行版如`TeXLive`中,用于辅助制作参考文献.

+ 使用`bibtex`, 需要你先准备一个`.bib`文件, 它是参考文献条目的集合.
+ 还有一个控制参考文献格式的文件---`.bst` (bibtex style file). 它控制如何把`.bib`转换成 `latex` 标准的 `\bibitem` 格式,可以修改成适应不同的杂志.

在文章 `class` 中设置合适的选项,会自动从 `revtex4.2` 中选择合适的 `.bst` 文件, 也就是类似下面的`aps,prd`

```latex
\documentclass[aps,prd,superscriptaddress]{revtex4-2}
```

`revtex4.2`包含`5`种基本的格式:

+ `apsrev4-2.bst` (APS journals using a numeric citation style, i.e., all but RMP),
+ `apsrmp4-2.bst` (author/year style citations for RMP),
+ `aipauth4-2.bst` (AIP journal using an author/year citation style),
+ `aipnum4-2.bst` (AIP journals using a numeric citation style)
+ `aapmrev4-2.bst` for AAPM journals.

通过使用`latex2e` 标准的 `\bibliographystyle` 宏指定另外的 `.bst` 可以覆盖这个选择.
但是命令需要出现在导言区,  在 `\begin{document}` 行之前,这跟标准的 `latex2e`语法不同.(不知道现在是否更改)

bibtex 的 `.bib` 文件中将包含如下格式的条目:

```bib
@Book{GSW,
    author=''M. Greene, J. Schwarz,
    E. Witten'',
    title=''Superstring theory:
    Introduction'',
    publisher=''Cambridge University
    Press'',
    address=''London'',
    year=''1985''
}
```

参考文献条目可以有各种各样的格式, 一般直接从文献搜索引擎中导出即可.

+ articles,
+ technical reports,
+ e-prints,
+ theses,
+ books,
+ proceedings,
+ articles that appear in books or proceedings

***
`RevTeX4.2` 提供的格式还允许 `URL` 和 `e-print` 标识符. 除了`author` 域,还有`collaboration` 域.

在草稿中创建参考文献时,请使用 `\bibliography{bib文件,不需要后缀名}` 宏. `<bib files>` 是一串用逗号分隔开的参考文献库列表, 即`.bib`文件列表.
`\bibliography`宏应该被放在参考文献要出现的地方, 一般是正文的后面.

假设`.tex`源文件叫做`abc`, 编译的时候不用加上后缀名`.tex`.

+ 运行第一遍`pdflatex abc`时, `\cite` 宏引用的 `key` 会被写入 `abc.aux` 中.
+ 接着运行 `bibtex abc`,就会产生 `latex` 需要的 `\bibitem` 条目,保存在 `abc.bbl` 文件中.
+ 接下来运行两次`pdflatex abc`,`pdflatex` 会重复调用`abc.bbl` 文件,直到参考文献都顺利显示.

总结就是需要编译四次, `xelatex`同理:

```bash
pdflatex abc; bibtex abc; pdflatex abc; pdflatex abc;
```

使用`revtex style`文件产生的 `\bibitem` 看起来很复杂.
这是因为 style 增加了 `\bibinfo`, `\bibnamefont`, `\eprint`, and `\url` 等宏, 用于指定额外的格式化和标签.

`\bibinfo` 宏基本上啥也不干,只是用来标记从`bib` 文件得到的信息.
`\eprint `和`\url` 宏用来创建合适的 `hyperlinks`,并适应最终的格式如`PDF`. (需要使用`hyperref`包)

更多使用帮助,请参考:
`Sections 4.3.1 and C.11.3 of the LATEX User's Guide & Reference Manual[2], Section 13.2 of [4]`
[the online BibTEX manual btxdoc.tex](http://www.ctan.org/tex-archive/biblio/bibtex/distribs/doc/).

***
在`ubuntu`上安装的`TeXLive`, 附带的参考文献格式---`xxx.bst`

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

### arXiv.org 支持

`revtex4.2` 支持引用 `arXiv.org` 的 `e-prints`. 比如以下 `.bib`条目:

```bib
@Unpublished{Ginsparg:1988ui,
    author = "Ginsparg, Paul H.",
    title = "{Applied Conformal Field Theory}",
    year = "1988",
    eprint = "hep-th/9108028",
    archivePrefix = "arXiv",
    SLACcitation = "%%CITATION=HEP-TH/9108028;%%"
}
```

将会包含`arXiv.org e-print identifier as arXiv:hep-th/9108028`, 并加上超链接 (需要使用`hyperref`包)
`arXiv` 的新版标识符带有`primary classifications`, 也会产生相应的输出. 例如:

```bib
@Unpublished{Ginsparg:2014,
    author = "Ginsparg, Paul",
    title = "{Kenneth G. Wilson: Renormalized After-Dinner Anecdotes}",
    year = "2014",
    eprint = "1407.1855",
    archivePrefix = "arXiv",
    primaryClass = "physics.hist-ph",
}
```

将产生 `arXiv:1407.1855 [physics.hist-ph]`, 并带有超链接.

### 不显示 arXiv标识符, noeprint option

在`REVTEX 4.2` 样式文件中, `noeprint` 选项的行为被改变,
现在它只省略杂志参考文献(`journal`)中的 arXiv 标识符,
而保留电子版(`e-print`) 参考文献中的 arXiv 标识符.
使用时可添加到文档类的选项列表中, 例如:

```latex
\documentclass[aps,prd,superscriptaddress,noeprint]{revtex4-2}
```

### Citing data sets with a DOI

`BibTEX` styles in `REVTEX 4.2` 增加了对 data sets 的支持, 使用了新的 BibTEX type `@dataset`:

```bib
@dataset{haigh:2016,
    author = "Haigh, J. A. and Lambert, N. J. and
    Sharma, S. and Blanter, Y. and
    Bauer, G. E. W. and Ramsay, A. J.",
    year = "2018",
    title = "{Data from Figures in""Selection rules
    for cavity-enhanced Brillouin light scattering
    from magnetostatic modes" [Data set]}",
    doi = "10.5281/zenodo.1284434",
    note = "{Zenodo}"
}
```

将产生格式化的参考文献.

    "J. A. Haigh, N. J. Lambert, S. Sharma, Y. Blanter, G. E. W. Bauer, and A. J. Ramsay, Data from Figures in "Selection rules for cavity-enhanced     Brillouin light scattering from mag-netostatic modes" [Data set], 10.5281/zenodo.1284434(2018), Zenodo."

这主要用于分配有 `DOI` 的数据集.

### Journal references with only DOIs

一些杂志转而使用`volume`和`DOI`来标识文章, 不再分配页码或文章标识符.

对于`Phys. Rev. journals`使用的 `apsrev.bst` BibTEX 样式文件.
如果`bib`文件中的条目的`pages`字段缺失, 但存在`DOI`, `DOI`将会被明确显示并链接在格式化的`reference`中.

### Journals that use the year and issue for unique citations

年份-期号

用于`Phys. Rev. journals`的`apsrev.bst`样式, 现在支持四种使用`year`代替`volume`的期刊, 这些期刊需要一个明确的`issue`来确定地引用一篇论文.

+ J. High Energy Phys.
+ J. Cosmol. Astropart. Phys.
+ J. Instrum.
+ J. Stat. Mech.: Theory Exp.

`BibTEX`条目必须精确的匹配上述缩写之一, 或者使用相应的 macro:  `jhep`, `jcap`,`jinst`, or `jstat`,  来触发相应的格式化,比如

```bib
@Article{Cotogno2017,
    author="Cotogno, Sabrina and van Daal, Tom
    and Mulders, Piet J.",
    title="Positivity bounds on gluon {TMDs}
    for hadrons of spin $\le$ 1",,
    journal=jhep,
    year="2017",
    month="Nov",
    day="28",
    volume="2017",
    number="11",
    pages="185",
    doi="10.1007/JHEP11(2017)185",
    url="https://doi.org/10.1007/JHEP11(2017)185"
}
```

将会被格式化为:

    "S. Cotogno, T. van Daal, and P. J. Mulders, Positivity bounds on gluon TMDs for hadrons of spin ≤ 1, J. High Energy Phys. 2017 (11), 185."

### Multiple references in a single bibliography entry

若使用 `BibTEX`, `REVTEX 4.2`允许在单个`bibliography`条目中包含多个 `reference`. 这可以通过使用在 `\cite` 命令中使用带星号(`*`) 的参数实现.
并需要一个兼容的 `natbib` 版本和 `REVTEX 4.2` 自带的 `bst` 文件.

为了把多个 `ref` 包括进一个 `\bibitem`,在 `\cite`命令中的第二,三个`citation keys`前面加上星号,例如:

    \cite{bethe, *feynman, *bohr}

会组合 `\bibitems` with keys `bethe`, `feynman`, and `bohr`, 使它们成为参考文献列表中的单个条目,用 `semicolons` 分隔.

### Prepending and/or appending text to a citation

`cite`命令参数的扩展语法, 也可以用来指定 `citation` 前置的或后置的文本,例如:

使用以下命令:

```latex
\cite{*[{A similar expression was derived in }] [{ in the context of carbon nanotube p-n junctions. The only difference is that no integration over ky is present there.}] andreev2007}
```

将创建:

    [19] A similar expression was derived in A. V. Andreev, Phys. Rev.Lett. 99, 247204 (2007) in the context of carbon nanotube p-n junctions. The only difference is that no integration over ky is present there.

请注意其中用来封闭文字的`{}`, 以及外面的`[]`, 还有 brackets 后面的括号.

### natbib 简介

`natbib` 宏包重定义了 LATEX 命令 `\cite` ,可以采用作者年份格式或者数字格式引用文献,
适用于 `plain` 等标准的参考文献格式, 也与 `harvard` `,apalike` , `chicago` `,astron` `,authordate` 以及 `natbib` 等兼容.

与上述宏包相比, `natbib`  宏包不仅支持众多的作者年份格式,也支持标准的数字格式引用.

事实上,它还可以在作者年份的文献格式下产生数字格式引用,而且很容易在两种引用模式间切换.为此,它也提供了替代标准 LATEX 文献格式的专用格式(`.bst`).
`natbib` 宏包可以定义引用格式,如括号以及不同引用条目间标点的类型等.
甚至可以关联文献格式名以自动激活不同引用格式,也可以通过当前的配置文件 `natbib.cfg` 为 `.bst` 文件定义引用格式.

natbib 宏包与 `babel` , `index` ,`citeref` , `showkeys` , `chapterbib` , `hyperref` , `koma` 等宏包以及 `amsbook` , `amsart` 等文档类兼容,
也能实现 `cite` 宏包的排 序与压缩功能,还能实现 Thorsten Ohl 写的 `mcite` 宏包的多个引用的合并功能.
然而, `natbib` 宏包本身与 `cite` 或 `mcite` 宏包不兼容.

应该注意的是实现文献列表中增加引用页码功能的 `citeref` 宏包必须在`natbib` 宏包之后调用.
(调用 `hyperref` 宏包时设定 `pagebackref` 选项也有此功能,而且提供了超链接.)

此外,`natbib` 宏包为大多常见的参考文献格式提供了统一而灵活的接口.

## 正文部分

BODY OF THE PAPER

### 分节和交叉引用

用 `\section`, `\subsection`, `\subsubsection` 来给文章分节, 交叉引用必须用 `\label` and `\ref`命令, 不要手动引用.
不要用`\part`, `\chapter`, and `\subparagraph`

### 附录

附录应该通过 `\appendix` 指定,其后的 `\section`命令创建附录.
如果只有一个附录,也可以用 `\appendix*`

### 致谢

Acknowledgments

致谢应该用`acknowledgments`环境.

### 计数器 Counters

不要自创计数器,也不要更改标准的计数器.
实在想用,可以用 `\tag`命令(需要 `amsmath` class option),`\tag` 可能会跟`hyperref` package 冲突.

### 字体

不要用老掉牙的 `\rm`, `\it`等等. 用 `LATEX2e` 中引入的新命令. 字体和数学字体的控制命令如下:

粗体希腊字母或者其他粗体数学符号,可以用 `LATEX2e` 中的 `bm.sty` 实现, 通过 `\usepackage{bm}`载入.
这个包引入了 `\bm`宏, 一些粗体字母可能需要使用`amsfonts`class option

不要用 `\newfont` 创建新字体.不要自己选择font `family`,`shape`, and `series`, 而应该使用上面列出的 `LATEX2e`标准字体选择 macrc.
最后,不要用`\symbol`宏.

|               |                                                                                                         |
| ------------- | ------------------------------------------------------------------------------------------------------- |
| `\textit`     | Italics. Replaces `\it`                                                                                 |
| `\textbf`     | Bold face. Replaces `\bf`                                                                               |
| `\textrm`     | Roman. Replaces `\rm`                                                                                   |
| `\textsl`     | Slanted. Replaces `\sl`                                                                                 |
| `\textsc`     | Small caps. Replaces `\sc`                                                                              |
| `\textsf`     | Sans serif. Replaces `\sf`                                                                              |
| `\texttt`     | Typewriter. Replaces `\tt`                                                                              |
| `\textmd`     | Medium series                                                                                           |
| `\textnormal` | Normal                                                                                                  |
| `\textup`     | Upright                                                                                                 |
| `\mathbf`     | Bold face                                                                                               |
| `\mathcal`    | Replaces `\cal`                                                                                         |
| `\mathit`     | Italics                                                                                                 |
| `\mathnormal` | Replaces `\mit`                                                                                         |
| `\mathsf`     | Sans serif                                                                                              |
| `\mathtt`     | Typewriter                                                                                              |
| `\mathfrak`   | Fraktur: Requires amsfonts or amssymb class option                                                      |
| `\mathbb`     | Bold blackboard: Requires amsfonts or amssymb class option                                              |
| `\bm`         | Bold Greek and other math symbols: Requires `\usepackage{bm}` and may require the amsfonts class option |

### 环境

标准的列表环境都允许, `itemize`, `enumerate`, and `description`
`\item` macro 有没有参数都行.
自定义的 列表环境(通过用 `\labelstyle`, `\labelitemi`, `\labelenumi`, `\itemsep`, etc.)也行,不过可能会被忽略.

Generalized lists (`\begin{list}`) and trivial lists (`\begin{trivlist}`) are not allowed

### 其他环境

一般定义新环境是不行的,但是`\newtheorem` 是可以的.

The tabbing environment and the 宏 `\=`, `\>`, `\'`, and `\'` are allowed but may be ignored in production.

Conversion programs used in production should recognize the escapes `\a=`, `\a'`, and `\a'` for using the corresponding accents within a tabbing environment though.

The `verbatim` environment is allowed.

### 盒子

Boxes

大多数盒子命令是不行的.包括:

These include `\raisebox`, `\parbox`, `\minipage`, `\rulebox`, `\framebox`, `\mbox`, `\fbox`, `\savebox`, `\newsavebox`, `\sbox`, `\usebox`, and the environment `\begin{lrbox}`.

Rules produced with `\rule` are not allowed.

#### Margin Notes

不要用 `\marginpar`,也不要修改 `\marginparwidth`, `\marginparsep`, and `\marginparpush`.

## 图片 Figure

### 插入图片

`LATEX2e` 中两个主要的用来插入图片的包 `graphics` and `graphicx`. 两者都提供了一个叫做`\includegraphics`的宏.
它们的区别主要在于如何将参数传递给`includegraphics`, 控制图形的位置, 例如缩放和旋转, 建议使用`graphicx`.

图片应该被放置在 `figure` 环境中,以便于加上 `caption` 和放置到合适的位置.

如果需要在文中引用图片,应该使用 `\label`, 比如把 `\label` 加入到 `\caption` 的参数里. 跨页面的图形应该用`\figure*`环境.

不要直接使用 `picture` 环境.
但是当然可以`include`一个使用`picture`环境制作的封装的`Encapsulated PostScript`图.

### 图片放置

图片挨着第一次引用的位置放置即可. 没有必要在末尾再次附上所有图形.

`REVTEX 4.2` 有一个选项 `floatfix` class option, 可以对`stuck` floats 进行紧急排列,否则的话它们一般会被推迟到末尾.
(and can lead to the fatal "Too many unprocessed floats" message)

## 表格

使用 LATEX2e 标准表格排版环境即可,包括`longtable`包.

Hint:

+ 使用`longtable`包以使得表格跨页.
+ 使用`\squeezetable`宏减小表格字体.

The proper markup is

```latex
\begingroup
\squeezetable
\begin{table}
...
\end{table}
\endgroup
```

尝试浮动体的放置选项`H`来允许浮动体跨页, 虽然长表格更推荐使用`longtable`.

```latex
\begin{table}[H]
\begin{ruledtabular}
\begin{tabular}
...
\end{tabular}
\end{ruledtabular}
\end{table}
```

### Doubled rules and table formatting

`REVTEX 4.2` 提供了`ruledtabular`环境,它会在表格四周自动放置`scotch rules`(双线).
并将`tabular`环境格式化成`table`浮动题的全宽度, 并改善列间间距.
(And formats all enclosed tabular environments to the full width of the tables  and improves intercolumn spacing.)

你可以尽量多用它.例如:

```latex
\begin{table}[H]
\begin{ruledtabular}
\begin{tabular}
...
\end{tabular}
\end{ruledtabular}
\end{table}
```

### 宽表格

当使用双栏排版时,表格可以单栏或者跨栏. 使用 `table` or `longtable`环境的带`*` 号版本,来实现跨栏.

特别宽的表格可以用`landscape`朝向 (旋转 90 度 ).
或者可以用`turnpage`环境实现. 这将使旋转过的表格在单独一页上呈现.
一些`dvi`查看程序可能无法正常显示,但是`dvips`and `pdflatex`可以正常工作.

也可以使用`\resizebox`缩放表格.

```latex
\begin{table}[H]
    \centering
    \resizebox{\textwidth}{!}{  %这个命令可以缩放宽度, 第二个参数!表示等比缩放.
\begin{tabular}{c|c|c|c|c|c|c|c|c|c}
...
    \end{tabular}
       }
```

### 表格放置

表格无需单独在文末列出. 也可以使用 class option `floatfix`避免表格堆积.

### 表格沿小数点对齐

应该使用标准的LATEX2e宏包`dcolumn`来完成这个任务

### 表格附注

表格中的脚注一般用 `\footnote`宏即可.但是, 如果需要对同一脚注有多次`reference`, 作者可以使用`\footnotetext` and `\footnotemark`.

会在表格下方产生注解(labeled by lower-case roman letters),而不是在 `reference` 或者页面底部.

## 自定义宏

为了减少敲键盘次数,作者也可以自定义宏 . 但是不要调用上下文依赖的命令比如 `\if `.

`LATEX2e` 提供了三种声明新命令的宏: `\providecommand`, `\newcommand`, and `\renewcommand`, 以及带星号的版本(`*`versions)
不要用 `TEX` 的底层命令,如 `\def`, `\edef`, and `\gdef`.

## 总结

尽量少用底层命令,越简单越好.以避免杂志编辑过程中无法转换成 `XML`.
使用 `REVTEX 4.2` or `LATEX2e` 中合适的宏.

## others

### PACS codes

`PACS`代码过时了. `showpacs`选项没有任何作用, 但它的存在是为了在`REVTEX 4.2`下仍然可以处理旧的文件.

### 关键词

`\keywords` macro用来显示关键词, 例如

```latex
\keywords{nuclear form; yrast level}
```

会显示在 `abstract` 下面. `keywords` 是否显示,可以通过 `\documentclass`一行中的选项控制. 通过`showkeys` and `noshowkeys`.

### 多行公式

表格的对齐,是按奇偶列对齐的, 比如135左对齐, 246 右对齐.
