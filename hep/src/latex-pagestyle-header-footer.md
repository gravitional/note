# latex 页面格式 页眉页脚

## ctex 章节标题样式

[ctex doc](https://ftp.kddilabs.jp/CTAN/language/chinese/ctex/ctex.pdf)

在ctex文档类中, 调整 章节标题样式, 例如章节标题前后的 空白,
见ctex帮助第七章. 使用下面的格式.

```latex
ctexset {
    part/pagestyle = empty,
    chapter = {
    format = \raggedright,
    pagestyle = empty,
 },
section = {
    name = {第,节},
    number = \chinese{section},
}}

\begin{document}
```

调整章节前的空白 和 页眉格式

```latex
\usepackage{fancyhdr} % 页眉和页脚的相关定义
\ctexset{
    chapter/beforeskip = -10pt %调整chapter前的空白
}
\pagestyle{fancy} %显示页眉,有下划线
```

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

这里注意一点,如果我们同时使用了`geometry`和`fancyhdr`宏包,
那么一定要把`\usepackage{fancyhdr}`及相应的
`页眉`, `页脚` 设置写在`\usepackage{geometry}`的前面, 否则会出现奇怪的错误.

`fancyhdr`宏包的说明文档中也有各个页眉页脚位置的图示.
[fancyhdr](https://www.ctan.org/pkg/fancyhdr)

`fancyhdr`将页面的页眉, 页脚各分为左, 中, 右三个部分,
其对应的指令名为`\lhead{}`,` \chead{}`, `\rhead{}`, `\lfoot{}`, `\lhead{},` `\rhead{}`.
括号中填写的内容将在对应的地方出现.
比如想在页眉正中出现`学习指南`,我们只需在导言区加上`\chead{学习指南} `.
现在版本的`fancyhdr`已经不建议使用这种风格的命令了,
而是使用类似`\fancyhead[L]{xx}`这样的命令代替.

+ 如果想使用页码,可以用`\thepage`来实现. 它存储当前页面的页码.
比如想在页尾右侧写上当前页码,则在导言区中加上`\rfoot{\thepage} `.

+ 此外,顺便提一句,如果要在正文中使用 `\maketitle`,
那么那一页的页面格式会自动变回原来的页面格式.
需要在`\maketitle`后加上一句`\thispagestyle{fancy}`.

+ 同时,页眉 也被默认设置了含有 `页眉线`.
页眉线, 页脚线的指令名分别为`\headrulewidth`和`\footrulewidth`. 其粗细可分别用`\renewcommand`来设置,
例如想取消页眉线,就在导言区加上`\renewcommand\headrulewidth{0pt} `

下面的用法生成 `第x页,共y页` 的页码样式.

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
