# BibTeX生成参考文献列表

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

## bibtex 常见问题

+ 我希望将一条文献展示在参考文献列表中,但不想在正文中用 `\cite` 命令引用,怎么办?

首先,确保这条文献已经写入了 `bib` 文件.
其次,可以在 `\bibliography` 命令之前,用 `\nocite{label}`提示 `BibTeX` 调取这条文献.
我有很多条文献,都存在这样的情况. 每条文献逐一 `\nocite` 太繁琐了,有没有懒人适用的办法?
有的. `\nocite{*}`.

+ 每次都要编译四次,我感觉懒癌又要发作了,有没有办法治疗?

有的. 可以尝试 `LaTeXmk`, `TeXify` 之类的自动化工具.

+ 我对默认提供的 `bst` 文件的格式效果不满意,哪里能找到更多的 `bst` ?

现代 TeX 发行版都提供了多种 `bst` 可供选择,每个 `bst` 文件的格式, 适用范围, 使用条件都不一样,需要仔细甄别.
具体可以去安装目录下搜索试试.

+ 有没有遵循国家标准的 `bst`?

有的

+ 我找到的 bst,效果都不满意,怎么办?

你可以在命令行执行 `latex makebst`,制作一个符合自己要求的 `bst` 文件.
你需要回答大约 100 个关于参考文献列表效果的问题.

+ `bib` 文件怎么生成?

你可以手写,或者用 `JabRef` 之类的文献工具生成. 具体请自行 Google 检索,篇幅所限就不展开了.

+ 我听说还有一个名为 `biblatex` 的工具,能介绍一下吗?

`BibLaTeX` 与 `BibTeX` 是不同的工具,超出了本文的范围.

### 只添加不引用

+ [tamethebeast](https://www.ctan.org/tex-archive/info/bibtex/tamethebeast/)
`texdoc latex-notes-zh-cn `: 包太雷的 latex 笔记

如何在 `参考文献列表` 中添加 `条目` 而不在文件中引用它们?

这是由 `\nocite` 命令实现的. 它的工作原理与 `\cite` 完全一样, 但在文件中不写任何内容.
它只是在 `.aux` 文件中包含了 `\citation` 命令.
这个命令的一个变体是 ``nocite{*}`: 它相当于一次把整个书目写进去.
这些参考文献会按照它们在 `.bib` 文件中出现的顺序包括在内, 除了那些在前面被引用过的文献.
请注意,  `\cite{*}` 也是正确的, 但我不确定它是否有任何意义......

### 引用讲义等其他文件类型

[How to cite a lecture note](https://tex.stackexchange.com/questions/183472/how-to-cite-a-lecture-note)

你想要的输出取决于你所使用的 `bibliographystyle{}`(`.bst`文件).
尽管如此, 你总是可以尝试用 `misc` 表示条目. 例如:

```bibtex
@misc{Cunha13,
  author        = {Jo{\~a}o Cunha},
  title         = {Lecture notes in Computer Assisted Diagnosis},
  month         = {February},
  year          = {2013},
  publisher={Faculdade de Engenharia da Universidade do Porto}
}
```

### 参考文献类型

[参考文献条目](https://www.jianshu.com/p/20461a21722d)

参考 BibTeX reference, 可选的条目类型和它们的字段为:

![bibitem必备与可选项列表](https://upload-images.jianshu.io/upload_images/1957089-6bddb79c07fb6497.PNG?imageMogr2/auto-orient/strip|imageView2/2/w/806/format/webp)

## cite,其他的参考文献包

[cite – Improved citation handling in LaTeX](https://www.ctan.org/pkg/cite)

`cite`支持压缩,排序的数字引用列表,还处理各种标点符号和其他表示形式的问题,包括对断点的全面管理.
该软件包与`hyperref`和`backref`兼容.

支持给出多种`cite`格式:
`[?,Einstein,4–6,6–9]`
`[5a–5c] or [T1–T4])`
`information;12`

`cite` and `natbib` 不能同时使用.

## natbib

[natbib – Flexible bibliography support ](https://www.ctan.org/tex-archive/macros/latex/contrib/natbib/)

`natbib` 软件包是 `LaTeX` 的扩展,允许作者年份(author–year)的引用形式,也支持数字引用. 可以方便的切换.

首先导入包,`\usepackage[sectionbib,square]{natbib}`
`natbib` 提供了三种新的格式,

+ plainnat.bst
+ abbrvnat.bst
+ unsrtnat.bst

通过在正文中调用以下命令使用这些格式:

`bibliographystyle{plainnat}`

`natbib` 特别定义了 `citet` (cite textual) and `citep`(cite parenthetical).
以及 `\citet*` and `\citep*` 可以打印出作者全名.
这些命令都可以接受一到两个参数,在引用前后输出额外文字.
可以同时引用多个参考文献.

```latex
\citet{jon90,jam91} --> Jones et al.  (1990); James et al.  (1991)
\citep{jon90,jam91} --> (Jones et al., 1990; James et al.  1991)
\citep{jon90,jon91} --> (Jones et al., 1990, 1991)
\citep{jon90a,jon90b} --> (Jones et al., 1990a,b)
```

这些例子是针对 `author–year` 引文模式的.
在 `数字模` 式下, 结果是不同的.

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
+ `sectionbib` 重新定义`\thebibliography`来引用`\ section*`而不是`\ chapter*`;仅对带有`\ chapter`命令的类有效;  与`chapterbib`软件包一起使用;
+ `nonamebreak` 将所有作者的名字放在同一行中,导致`hbox`过多,但有助于解决一些`hyperref`问题;
+ `merge` 允许在`citation key`前面加上`*`前缀,并将此类引文的引用与先前引文的引用合并;
+ `elide` 合并参考文献后,去掉重复的共同要素,例如作者或年份;
+ `mcite`识别(并忽略)合并语法

## lyx中使用 bib tex

菜单栏`Insert/List_Toc/Bibtex`添加 `bib`库文件,即可使用.
