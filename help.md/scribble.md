# scribble

[scribble](https://docs.racket-lang.org/scribble/getting-started.html)

## 开始使用

无论你想用`Scribble`做什么, 最好先从生成一些简单的`HTML` 或 `PDF`文档开始.
本章向你介绍基础知识, 并在 `下一步` 中提出具体的使用建议.

## 第一个例子

创建一个文件 `mouse.scrbl`, 内容如下.

```scribble
#lang scribble/base

@title{论老鼠吃饼干的习惯}.

如果你给一只老鼠一块饼干, 它就会要求一杯牛奶.
```

第一行的`#lang scribble/base`表示该文件实现了一个`Scribble`文档.
文件以 `文本模式` 开始, `@`字符`转义`到`操作符`(operators), 如 `title`,
`大括号`为操作符提供`参数`, 参数仍然处于`文本模式`.
剩下的就是文档内容了.

现在运行`scribble`命令行程序, 指定一个模式, 输出到你想要的文档类型.

+ 运行

```bash
scribble mouse.scrbl
```

来生成 `HTML`, 即 `mouse.html`. 你可能会注意到, `he's` 中的撇号变成了弯曲的撇.

+ 运行

```bash
scribble --htmls mouse.scrbl
```

来生成 `HTML`, 即 `mouse/index.html`. `sub-sections`(子段落, 我们接下来添加)将作为单独的`HTML`文件出现在 `mouse`目录中.

+ 运行

```bash
scribble --pdf mouse.scrbl
```

来生成`PDF`格式的 `mouse.pdf`. 只有在你安装了`pdflatex` 后才能工作. 如果你想看中间产生的 `Latex`, 可以试试

```bash
scribble --latex mouse.scrbl
```

来生成 `mouse.tex`.

参见 `运行scribble` 以了解更多关于`scribble`命令行工具的信息.

## 多个部分

在 `mouse.scrbl` 中添加更多文字, 使其看起来像这样.

```scribble
#lang scribble/base

@title{论老鼠吃饼干的习惯}.

如果你给一只老鼠一块饼干, 它就会要求一杯牛奶.

@section{牛奶的后果}.

那 ``吱吱'' 声就是老鼠在要牛奶. 让我们假设你在大杯子里给他一些.

他是一只小老鼠. 这个杯子太大了--- 真滴大. 所以, 他可能会向你要一根吸管. 你不妨给他吧.

@section{不是最后一根稻草}.

现在, 要处理牛奶小胡子, 给他一张餐巾纸就够了. 但这还没有结束......哦, 不.
```

现在, 在论文的第一段之后, 我们有两个`子节`, 每个`子节`都是通过调用`section`来生成子节声明而创建.
首个`子节`有两段. 第二节, 由第二个`section`调用创建, 只有一段.

再次运行`第一个例子`中的`scribble`命令. 你可能会注意到输出中的弯曲 `双引号`, 以及`---`变成了破折号(em dash).

## 拆分文档源文件

当文档越来越大时, 最好将各部分分割成独立的源文件.
`include-section`操作将一个由 `.scrbl` 文件定义的文档并入一个更大的文档.

要把示例文件分割成多个文件, 把 `mouse.scrbl` 改为只有

```scribble
#lang scribble/base

@title{论小鼠吃饼干的习惯}.

如果你给一只老鼠一块饼干, 它就会要求一杯牛奶.

@include-section["milk.scrbl"]
@include-section["straw.scrbl"]
```

在 `mouse.scrbl` 的同一目录下创建 `milk.scrbl` 和 `straw.scrbl`.
在 `milk.scrbl` 中, 写入

```scribble
#lang scribble/base

@title{牛奶的后果}.

那个 ``吱吱'' 声是老鼠在要牛奶......
```

并在 `straw.scrbl` 中, 写入

```scribble
#lang scribble/base

@title{不是最后一根稻草}.

现在, 为了处理牛奶小胡子, ......
```

注意, 新文件都以`#lang`开头, 就像原先的文件一样, 原先文件中的`章节`在新文件中被写成`标题`.
`milk.scrbl` 和 `straw.scrbl` 都是有自己的`标题`的文件, 它们可以用 `scribble` 单独渲染.
同时, 如果在 `mouse.scrbl` 上运行 `scribble`, 将会把小文件合并成单个与之前相同的文件.

## 文档样式

`Scribble` 目前只支持一种形式的`HTML`输出. 你可以为生成的页面替换 `scribble.css`文件, 仅此而已. (我们期望在将来增加更多的样式).

对于基于 `Latex` 的PDF输出, `Scribble`包括对多种页面布局配置的支持.
到目前为止, `mouse.scrbl` 例子使用的是默认的`Latex`样式.
如果你打算把论文提交给一个关于编程语言的研讨会, 那么--嗯, 你可能需要一个不同的主题.
你可以通过将第一行改为

```scribble
#lang scribble/acmart
```

如果你想编写`Racket`库的文档, 可以试着把第一行改成

```scribble
#lang scribble/manual
```

这将产生具有单独`标题页`的输出, 该页上的`初始内容`(旨在作为文档的简要介绍), 以及`顶级章节`, 每个章节在新的一页上开始.
如果你把文件分成了多个文件, `主文件`的第一行决定了输出格式.

使用 `scribble/acmart` 或 `scribble/manual` 不会改变文档的 `HTML` 渲染--除了 `scribble/manual` 增加了一个版本号,
但它会改变文档主体中可用的`绑定集`. 例如, 使用 `scribble/acmart`, 介绍性文本可以被标记为`摘要`(abstract).

```scribble
#lang scribble/acmart

@title{论小鼠吃饼干的习惯}.

@abstract{如果你给老鼠一块饼干, 它就会要求一杯牛奶. }

@section{牛奶的后果}
....
```

当渲染为`HTML`时, `摘要`显示为一个嵌入的段落.
如果你试图用 `scribble/base` 或 `scribble/manual` 语言使用`abstract`, 那么你会得到一个`error`, 因为 `abstract` 没有定义.

当文件分解为多个文件实现时, 改变`主文件`的语言可以设置所有部分的风格, 但它不会将绑定引入其他部分文件.
例如, 如果你把 `mouse.scrbl` 的语言改为 `scribble/acmart`, 那么`abstract`在 `mouse.scrbl` 中变得可用,
但在 `milk.scrbl` 或 `straw.scrbl` 中则不可. 换句话说, 运算符名称是文法范围的(lexically scoped).

## 更多的功能

`scribble/base`语言提供了一系列的基本操作(`scribble/acmart`和`scribble/manual`都是`scribble/base`的超集).
许多操作可以用来改变文本的样式.

```scribble
他是一个@smaller{小老鼠}. 这个杯子太@larger{大} --- @bold{真@larger{滴@larger{大}}}.
所以, 他@italic{可能}向你要一根吸管.
```

正如你所期望的那样, 对诸如 `smaller`, `larger` 和 `bold` 等函数的调用可以嵌套在其他调用中.
它们也可以嵌套在对`标题`或`章节`的调用中.

```scribble
@section{@italic{不要} 最后的吸管}.
```

### 居中

`centered`操作可以使文本流居中.

```scribble
如果一只老鼠吃光了你的饼干, 那就挂个牌子, 上面写上
@centered{
    @bold{想吃饼干}.
    @italic{更想吃巧克力片! }.
    }
看看有没有人给你弄点.
```

### 边注,Margin Notes

`margin-note`操作的使用方法类似, 但渲染的文本会被移到页边.

### 列表

`itemlist`操作创建了一连串的`bulleted`文本, `item`操作将文本分组, 使其出现在一个项目中.
`itemlist` 操作与我们之前看到的其他操作不同, 因为它只接受`item`产生的值, 而不是任意的文本.
这个区别反映在 `itemlist` 的参数使用 [...] 而不是 {...}.

```scribble
@centered{@bold{Notice to Mice}}.

@itemlist[@item{我们为你准备了饼干. }
                      @item{如果你想吃饼干.你必须自己带吸管. ]
```

### 表格

`tabular`函数接收`列表的列表`, 以组织成`二维`的表格.
默认情况下, `列`与`列`之间不加间隔, 所以提供一个`#:sep`参数作为列的分隔符. 例如.

```scribble
@tabular[#:sep @hspace[1] .
                    (list (list @bold{动物} @bold{食物})
                            (list "老鼠" "饼干")
                            (list "moose" "muffin"))]
```

## 参数的文本模式与Racket模式

当 `[...]` 围绕着一个`操作`的`参数`时, 参数表达式处于`Racket`模式而不是`文本模式`.
即使在`Racket`模式下, `@`也可以用来应用操作;
一旦通过`scribble/base`这样的语言启用`@`语法(而不是`racket`), 它在`Racket`模式和`文本模式下`的行为是一样的.

对`itemlist`的参数使用`Racket`模式的一个好处是, 我们可以向`itemlist`传递带`关键字`的`可选参数`.
特别是, 如果你想要带有`数字`而不是`子弹`的列表, 可以使用`#:style`关键字向`itemlist`提供 `'ordered` 样式.

```scribble
@itemlist[#:style 'ordered
          @item{吃饼干.}
          @item{喝牛奶.}
          @item{擦擦嘴.}
          @item{...}]
```

`操作`并不关心它与`[...]`或`{...}`中的哪个一起使用. 粗略的说, `{...}` 形成的参数是一个`字符串`.
(不过只是粗略的. `换行符`或在`{...}`中使用`@`会使情况复杂化, 我们很快会回到这个问题上). 所以:

```scribble
@italic{Yummy!}
```

相当于

```scribble
@italic["Yummy!"]
```

相当于`Racket`的表达式

```scribble
(italic "Yummy!")
```

这些等价关系解释了为什么`Scribble`函数是用`Racket` 记号列记录在案的.
如果你是以`HTML`格式阅读的, 你可以点击上面的`italic`来访问其文档.
可能你还不能完全理解这些文档, 但在本章结束时就会理解了.

如果你想在`文本模式`下提供`参数`, 但你也想提供其他可选参数, 怎么办?
你可以同时使用 `[...]` 和 `{...}` 进行操作, 只要`[...]`在前, 并且在结束的`]`和开始的`{`之间没有字符隔开.

例如, 调用 `italic`与使用`elem`的 `'italic` 样式是一样的.

```scribble
@elem[#:style 'italic]{Yummy! }
```

你也可以同时省略 `[...]` 和 `{...}`. 在这种情况下, `@`后面的`Racket`表达式被直接使用, 而不是作为`操作`应用. 比如说

```scribble
1加2是 @(number->string (+ 1 2)).
```

渲染为

```scribble
1加2是3.
```

需要调用 `number->string`, 因为`naked`数字(不是字符串)作为文档内容是无效的.

## @ 语法基础知识

`Scribble`提供的`@`符号只是写 `Racket` 表达式的另一种方式.
`Scribble`文档可以使用正常的R`acket`符号来构建, 完全不使用`@`, 但这对大多数用途来说是不方便的.
`@`符号使处理文本内容更容易.

无论是在`文本模式`还是`Racket`模式下, 文档中的`@`都提供了一个转义到`Racket`模式的机会. `@`的基本语法是

```scribble
@ <cmd> [ <datum>* ] { <text-body> }
```

其中`@`后面的三个部分都是`可选的`, 但至少有一个必须存在. 在`@`和`<cmd>`之间不允许有空格.

+ `@` 和 `<cmd>`, `[`, 或 `{`
+ `<cmd>` 和 `[` 或 `{` ; 或
+ `]`和`{`.

`<cmd>` 或 `<datum>` 是正常的`Racket`符号, 而 `<text-body>` 处于文本模式.
`<cmd>` 显然不能以 `[` 或`{`开始, 即使在 `Racket`形式可以以这些字符开始.

只有 `@<cmd>` 时, 扩展为`Racket`代码

```lisp
<cmd>
```

当使用 `[ ]` 或 `{ }` 时, 扩展为

```lisp
(<cmd> <datum>*  <parsed-body>*)
```

其中 `<parsed-body>*`是 `<text-body>` 的解析结果.  `<parsed-body>*` 部分的通常是一系列`Racket`字符串.

在实践中, `<cmd>` 通常是 `Racket` 标识符, 它被绑定到一个`过程`或`syntactic 形式`.

+ 如果`程序`或`form`期望接收文本, 进一步排版, 那么`{...}` 就提供文本.
+ 如果`form`期望有其他数据, 通常会用`[...]`来包围`Racket`参数.
+ 即使一个操作的`参数`是`字符串`, 如果该字符串不用作`内容文本`(而是用作例如`超链接标签`), 那么该字符串通常通过 `[...]`而不是`{...}`提供.

有时, `[...]`和 `{...}` 都会被使用, 前者围绕着`Racket`参数, 后者是要排版的文本.
最后, 如果`form`是一个纯粹的`Racket-level`的`form`, 没有排版的效果, 比如`require`导入更多的`operations`, 那么通常只使用`@`.

例如, 文本模式的流

```scribble
@(require scriblib/figure)

@section[#:tag "poetry"]{Of Mice and Cookies}.
See @secref["milk"].

@section[#:tag "milk"]{@italic{重要的}. 牛奶供应}
@figure["straw" @elem{A straw}]{@image["straw.png"]}.
```

相当于`Racket`模式的序列

```lisp
(require scriblib/figure) "\n"
"\n"
(section #:tag "poetry" "Of Mice and Cookies") "\n"
"See " (secref "milk" ) "." "\n"
"\n"
(section #:tag "milk" (italic "Important" ) " Milk Supplies" ) "\n"
(figure "straw" (elem "A straw") (image "straw.png")) "\n"
```

除了展示不同的`操作`是如何使用不同的`参数`约定外,
上面的例子还说明了在Racket `文本模式`流的形式中是如何保留空白的--包括换行符, 保留为单独的字符串.
注意第二节的内容是如何得到两个参数的, 因为`source stream`中 `section` 的参数内容包括使用`操作符`(`italic`)和 附加的文本.
当像`section`或`italic`这样的操作接受要排版的内容时, 它通常接受任意数量的参数, 这些参数共同构成内容.

除了用作命令, `@`后面还可以跟上`;`来开始`注释`. 如果`;`后面的字符是`{`, 那么注释会一直运行到匹配的`}`, 否则注释会一直运行到`行末`.

```scribble
@;{ <注释> }
@; <行-注释>
```

关于`@`的语法的更多信息, 见`@ 语法`.
完整的语法包括更多的细节, 比如用于文本模式的参数可以用`|{...}|` 括起来, 这样可以禁用`@`的解析.

## 解码序列

在以`#lang scribble/base`开头的文档中, 顶层(top level)是一个`文本模式`的流, 就像`@ form`的 `text-body`.
正如上一节所说明的, 这样的`顶层序列` 是 `Racket模式`的字符串和`operation`应用(排版操作)的混合.
有一个隐含的操作, 即`decode`, 它包裹(wrap)了整个文档, 消耗这种`字符串`和其他`值`的混合物, 并将它们变成`文档描述`.

`decode`操作实现了`流解码`(flow decoding), 它接收一个`文档流`, 并将其分割成若干`sections`和`paragraphs`.
`空行`为段落划界, `title`和`section`等操作的结果产生 `"这里是标题"` 或 `"新的章节从这里开始"` 的声明, 这些声明被`decode`所识别.

一个不同但相关的`内容解码`(cotent decoding)发生在`段落`或`章节标题`内.
`内容解码`负责将`---`转换为`破折号`, 或将 `"` 和 `'` 转换为合适的弯曲引号.

`文档流`的解码过程最终由文档首行的`#lang`行决定.
`scribble/base`, `scribble/manual`和 `scribble/acmart` 语言都使用相同的`decode`操作.
然而, `scribble/text`语言更像一个`纯文本`生成器和`预处理器`, 它不执行任何此类解码规则.
(关于`scribble/text`的更多信息, 见`Scribble作为预处理器`).

>更确切地说, 像`scribble/base`这样的语言, 只有从文档流中取出所有`definitions`和`imports`后才会应用`decode`.

当使用`流解码器`时, 在它把输入流分成`段落`后, 它对`段落`内的字符串应用`内容解码`.
然而, 当`内容`被`操作`包裹时, `内容解码`并不自动应用.
`操作`负责在它认为合适的时候调用一个`内容`或`流解码器`.
大多数操作都会调用`解码器`; 例如, `italic`, `bold`, `smaller`等都会对其参数进行解码.
同样地, `title` 和 `section` 对`标题`或`章节名称`的内容进行解码.
然而, `literal`和`verbatim`操作符不对给定的字符串进行解码. 例如.

```scribble
@verbatim{---}
```

被译为

    ---

不要把`解码`和`@`符号的扩展混淆起来. 源形式

```scribble
@verbatim{@(number->string (+ 1 2))}
```

显示为

    3

因为该源码等同于

```scribble
(verbatim (number->string (+ 1 2)))
```

其中`(number->string (+ 1 2))`被计算以产生`verbatim`的参数.\
`|{...}|`风格的括号经常与`verbatim`一起使用, 因为`|{...}|` 禁用参数的`@`记号. 例如.

```scribble
@verbatim|{@(number->string (+ 1 2))}|
```

显示为

```scribble
@(number->string (+ 1 2))
```

## 图片

任何可转换为`图片`的`value`都可以直接在`Scribble`文档中使用.
例如, 来自`pict`和`2htdp/image`库的函数可以生成图片. 举例来说.

```scribble
@(require pict)

这块饼干失去了它的巧克力片.
@(colorize (filled-ellipse 40 40) "beige").
```

## 接下来的步骤

如果你的近期目标是记录`Racket`库或编写`识字`(literate)程序, 跳转到[文档入门](Getting Started with Documentation),
然后再回到[@语法][]和其他章节中.

如果你对制作与`Racket`无关的文档更感兴趣, 继续看[@语法][],
然后是[高层Scribble API](https://docs.racket-lang.org/scribble/generic-prose.html).
当你需要更多的能力时, 再转到[底层Scribble API](https://docs.racket-lang.org/scribble/internals.html).

如果你对文本生成和预处理感兴趣, 可以继续学习[@语法][],
然后转到[Scribble as Preprocessor](https://docs.racket-lang.org/scribble-pp/index.html).

[@语法]: https://docs.racket-lang.org/scribble/reader.html

## 运行scribble

[Running scribble](https://docs.racket-lang.org/scribble/running.html)

`scribble`命令行工具(也可以用 `raco scribble`)运行一个`Scribble`文档并将其渲染成特定的格式.
用下列标志(`flags`)之一选择格式, 其中输出名称`fn`默认为`源文件`名称, 不带文件后缀的部分.

+ `--html` ; 单个`HTML`页面 `fn.html`, 加上`CSS`源和需要的图像文件; 如果没有指定格式, 这个模式是默认的.
+ `--htmls` ; 在 `fn` 目录下的多个`HTML`页面(和相关文件), 以 `fn/index.html` 开始浏览.
+ `--html -tree <n>` ; --目录树中的`HTML`页面, 最深为`n`层; 树深度为`0`相当于使用 `--html`, 树的深度为`1`相当于使用 `--htmls`
+ `--latex` ; `LaTeX` 源代码 `fn.tex`, 加上任何运行 `latex` 或 `pdflatex` 所需的额外文件(如非标准`class`文件).
+ `--pdf` ; 通过 `pdflatex` 生成的PDF `fn.pdf`.
+ `--xelatex` ; 通过 `xelatex` 生成的PDF `fn.pdf`.
+ `--dvipdf` ; --通过 `latex`, `dvips` 和 `pstopdf` 生成的PDF `fn.pdf`.
+ `--latex-section <n>` ; `LaTeX` 源文件 `fn.tex` 加上额外的 `.tex` 文件, 包含在`enclosing 文件`的序言中,
其中`enclosing 文件`必须使用`UTF-8`输入编码和`T1`字体编码; 若`n` 为 `1`, 使渲染的文件成为`章节`, `2` 表示一个小节, 等等.
+ `--text` ; 单个文件 `fn.txt` 中的纯文本, 非 `ASCII` 内容编码为 `UTF-8`
+ `--markdown` ; Markdown文本, 在单个文件 `fn.md` 中, 非 `ASCII` 编码的内容为 `UTF-8`.

使用 `--dest-name` 来指定`默认名称`以外的 `fn`, 但只在单一源文件的情况下有效.
使用 `--dest` 标志来指定一个目标目录(对于任何数量的`源文件`).
使用`--dest-base` 为生成或复制到目的地的, 文件添加名称前缀, 前缀可以包含`目录`路径, 非目录的结尾元素被用作文件名称的`前缀`, 对于附属支持文件.
使用 `--keep-at-dest-base` 来避免用覆盖目的地已有的文件, 对于附属支持文件. (但是当已有文件的内容, 与将要写入的内容相匹配时, 就会使用现有文件).

在所有`标志`之后, 可以提供一个或多个`文件源`, 其中每个`源`声明了一个`模块`.
该模块应该有一个`doc`子模块, 将`doc`作为`part`导出, 或者它应该直接将`doc`作为`part`导出.
(首先尝试`子模块`, 如果`子模块`可以单独加载, 则不直接`加载`或`计算`主模块).
使用 `--doc-binding` 来访问替代的导出名称, 而不是默认的 `doc`.

当多个文档同时被渲染时, 文档中的`交叉引用`信息对其他文档是可见的.
请参阅 Handling Cross-Reference, 以了解关于: 跨文档的引用, 单独构建的信息.
