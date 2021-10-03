# racket

[Racket, the Programming Language](https://racket-lang.org/)
[走进 racket(lisp) 的世界](https://zhuanlan.zhihu.com/p/19906432)

安装; 见官方页面`download`. `Debian`系也可以使用`sudo apt install racket`直接安装.

+ `DSL` ; `domain-specific languages`

与`C#`或`UML`这样的通用语言不同, 领域专用语言(`DSL`)被设计用来表达特定问题空间或`domain`的语句.

知名的`DS`L包括`正则表达式`和`SQL`.
每种`DSL`在描述对文本字符串或数据库的操作时都比通用语言好得多, 但在描述其自身范围之外的想法时却差得多.
个别行业也有自己的`DSL`. 例如, 在电信行业, `呼叫描述语言`被广泛用于指定电话呼叫中的状态序列,
而在航空旅游行业, 标准的`DSL`被用于描述航班预订.

你的业务和你的项目也会处理一些特殊的概念集, 可以用`DSL`来描述. 例如, 你可以为这些应用定义一个`DSL`.

+ 网站的导航路径.
+ 电子元件的接线图.
+ 一个机场的传送带和行李处理设备的网络.

当你设计`DSL`时, 你为领域中的每个重要概念定义一个`domain class`, 如网页, 灯, 或机场登记台.
然后定义`domain relationships`, 如超链接, 电线或传送带, 将这些概念联系在一起.

`DSL`的用户创建`models`. 模型是`DSL`的实例.
例如, 它们描述一个特定的网站, 或者一个特定设备的线路, 或者一个特定机场的行李处理系统.

你的用户可以用图或者窗口的形式查看`模型`. `模型`也可以用`XML`的形式表示, 这就是它们的存储方式.
当你定义`DSL`时, 你定义了每个领域类和关系的实例如何在用户的屏幕上显示.
典型的`DSL`显示为由箭头连接的图标或矩形的集合.

## DrRacket

[DrRacket IDE 的使用教程](https://docs.racket-lang.org/drracket/interface-essentials.html)
[创建可执行程序](https://docs.racket-lang.org/drracket/create-exe.html)

`Racket`安装时会附带`DrRacket`编辑器, 可以方便地用来调试`Racket`程序. 它是一个`REPL`(Read, Evaluate, Print, Loop)程序.

顶部的编辑面板称为`定义窗口`, 用来定义程序中的组件. 底部面板称为`交互窗口`, 用于交互式地运行`Racket` 表达式.
`交互窗口`中的`Language`指出了`定义`和`交互`窗口中可以使用哪些原语. 在例子中, 语言是由程序源的`#lang行`决定的.

点击 `运行` 按钮可以运行`定义窗口`中的程序, 使程序的定义在`交互窗口`中可用.
再次点击 `运行` 按钮可以重置`交互窗口`并重新运行`定义窗口`.

签名框(右上角的蓝色四分圆)提供了对文档中摘要信息的访问.
`DrRacket`窗口底部的状态行提供了关于当前行和编辑光标的位置, 是否可以修改当前文件以及`DrRacket`当前是否正在计算任何表达式等信息.
当 `DrRacket` 正在 `回收` 内部资源(如内存)时, 回收图标会闪烁.

+ `DrRacket` 会自动进行缩进, 以及自动改写括号匹配前面的内容, 无论你输入的是`()`还是`[]`. 如果想禁止自动更改, 可以更改设置, 或者在输入时按着`Ctrl`.
+ 它会高亮光标前匹配的括号之内的内容.
+ 按下`Tab`会重新缩进行, 光标可以在任何地方, 同理也可以缩进一整段文字.

菜单栏中可以查看快捷键:

+ 运行; `Ctrl+R`
+ 补全; `C+/`
+ 缩进文件; `C+I`
+ 缩进本行 ; `tab`

## tutorial

教程地址: [Quick: An Introduction to Racket with Pictures](https://docs.racket-lang.org/quick/)

安装完成之后, 也可以使用`F1`打开本地附带的教程文件.

本教程通过使用`Racket`的一个图片绘制库, 对`Racket`编程语言进行了简要介绍.
即使你不打算使用`Racket`进行艺术创作, 图片库也支持有趣的, 有启发性的例子. 毕竟, 一张图片胜过五百个 `hello world`.

按照同样的思路, 我们假设你将使用`DrRacket`运行这些例子. 使用 `DrRacket`是了解语言和系统感觉的最快方式, 即使你最终使用`Emacs`, `vi`或其他编辑器运行`Racket`.

### 图片教程

要绘制图片, 我们必须首先加载一些图片函数. 这些函数是创建幻灯片演示的库的一部分. 将以下内容复制到定义区, 也就是`DrRacket`的顶部文本区.

```lisp
#lang slideshow
```

然后点击 `运行` 按钮. 你会看到文本光标移到底部文本区, 也就是`交互区`.
如果你以前使用过`DrRacket`, 你可能需要点击左下角的`Language|Choose Language...`, 来侦测源代码中声明的语言.

现在可以在`>`后面输入表达式进行计算. 例如

```lisp
5
"art gallery"
(circle 10)
```

函数调用采用 `(函数 参数)`的形式. 也就是`S-表达式`. symbol-expression. 再举个例子, 复合两个图形:

```lisp
(hc-append (circle 10) (rectangle 10 20))
```

### define,let

用下面的语法进行`定义`:

```lisp
#lang slideshow
(define c (circle 10))
(define r (rectangle 10 20))
```

然后可以直接使用`r c`在交互区调用定义. 函数定义类似, 括号中是函数名称和参数.

```lisp
(define (square n)
  ; A semi-colon starts a line comment.
  ; The expression below is the function body.
  (filled-rectangle n n))
```

`define`也可以创建局部变量, 即局部绑定.

```lisp
(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))
(four (circle 10))
```

更一般地,  `Racketeers`使用`let`或`let*`形式进行局部绑定,
`let`的优点是它可以在任何表达式的位置使用, 而且, 它可以一次绑定许多标识符, 而不要求为每个标识符单独定义,

```lisp
(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))
```

`let`同时绑定多个标识符, 所以这些绑定不能相互引用, 与此相反, `let*`允许后来的绑定引用之前的绑定.

```lisp
(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))
(checkerboard (square 10))
```

### 函数即变量

除了将`circle`作为一个函数来调用, 尝试将`circle`作为一个表达式来计算.

```lisp
> circle
#<procedure:circle>
```

也就是说, 标识符`circle`被绑定到一个函数(又称 "过程", procedure)上, 就像名称`c`被绑定到一个圆上,
与圆形图片不同的是, 没有简单的方法可以完全打印出函数, 所以`DrRacket`只是打印出`#<procedure:circle>`.

这个例子表明, `函数`是`values`, 就像`数字`和`图片`一样(即使它们的打印方式不那么好), 既然函数是值, 你可以定义一个函数, 它接受函数作为参数,

```lisp
(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))
  (series circle)
  (series square)
```

当调用一个接受函数参数的函数时, 往往在其他地方不需要这个作为参数的函数,
如果要通过`define`来写下这个函数, 那就很麻烦了. 另一种方法是使用`lambda`, 它可以创建一个匿名函数,

```lisp
> (series (lambda (size) (checkerboard (square size))))
```

`lambda` 后面的括号内的名称是函数的参数, 参数名称后面的表达式是函数体, 使用 `lambda` 一词而不是 `function` 或 `procedure` 是 `Racket` 历史和文化的一部分,
函数的`define`定义形式, 实际上是简单使用`lambda`作为值的快捷方式. 例如, `series`的定义可以写成

```lisp
(define series
    (lambda (mk)
        (hc-append 4 (mk 5) (mk 10) (mk 20))))
```

大多数`Racketeers`喜欢使用`define`的速记函数形式, 而不是扩展为`lambda`,

### Lexical Scope

`Racket`是一种词法范围的语言, 这意味着只要一个标识符被用作表达式, 表达式所处文本环境中决定了标识符的绑定,
这个规则适用于`lambda`函数体中的标识符, 也适用于其他地方,

在下面的`rgb-series`函数中, `mk`在每个`lambda`形式中指向的都是`rgb-series`的参数, 因为那是文本范围内的绑定,

```lisp
(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))
>(rgb-series circle)
>(rgb-series square)
```

下面是另一个例子, `rgb-maker` 接受一个函数并返回一个新的函数, 这个函数会记住并使用原来的函数,

```lisp
(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))

(series (rgb-maker circle))
(series (rgb-maker square))
```

请注意, 与使用`rgb-maker`相比, 使用`rgb-maker`得到的合成函数创建了不同的排列,

### 列表

`Racket`继承了`Lisp`语言的大部分风格, `Lisp`的名字最初代表 `LISt Processor`, 而列表仍然是`Racket`的重要组成部分,

列表函数接受任意数量的参数并返回一个包含给定值的列表,

```lisp
> (list "red" "green" "blue")
> (list (circle 10) (square 10))
```

正如你所看到的, 列表打印的时候前面会加上单引号. 由于小括号既用于表达式, 如`(circle 10)`, 也用于打印结果, 如`("红""绿""蓝")`,
可能造成混淆, 所以加上引号便于区别. 为了帮助强调这一区别, 在文档和`DrRacket`中, 结果中的括号是用蓝色打印的, 与表达式括号不同,

如果你有一个列表, 你可能想对每个元素做一些事情, `map`函数接收一个列表和一个函数, 然后它把函数应用于列表中每个元素; 它返回一个新的列表, 以组合函数的结果,

```lisp
(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

(rainbow (square 5))
```

另一个适用于列表的函数是`apply`,
和`map`一样, 它接受一个函数和一个列表, 但是给`apply`的函数应该一次接受所有的参数, 而不是单独使用每个参数.
`apply`函数对于接受任意数量参数的函数特别有用, 例如`vc-append`,

```lisp
(apply vc-append (rainbow (square 5)))
```

注意, `(vc-append (rainbow (square 5)))` 是不行的, 因为`vc-append`不想要列表作为参数;
它想要一个图片作为参数, 而且它愿意接受任何数量的图片, `appl`y函数在需要许多参数的函数, 和作为单个值的参数的列表之间架起了一座桥梁.

### 模块

由于你的程序在`定义窗口`中的开头是

```lisp
#lang slideshow
```

你在`定义窗口`中的所有代码都在一个`模块`中.
此外, 该`模块`初始化时从`slideshow`指定的模块中导入所有内容, 该模块包括了`图片制作函数`以及更常用的函数, 如`list`和`map`.

要导入其他库, 请使用`require`形式, 例如, 库`pict/flash`提供了一个`filled-flash`的功能.

```lisp
(require pict/flash)
> (filled-flash 40 30)
```

模块以不同的方式命名和发布:

+ 有些模块被打包在1发行版中, 或者以其他方式安装到一个层次的`collections`中,
例如, 模块名称`pict/flash`意味着位于 `pict` 集合中的文件 `flash.rkt` 中实现的模块. 当一个模块名称不包括`斜线`时, 那么它指的是`main.rkt`文件,
+ 一些模块的集合是以`packages`的形式发布的,
包可以用`DrRacket`的文件菜单中的`Install Package...`菜单项来安装, 也可以用`raco pkg`命令行工具来安装, 例如, 安装 `avl` 包就可以使用avl模块,
+ 包可以在[https://pkgs.racket-lang.org/](https://pkgs.racket-lang.org/)注册, 也可以直接从`Git仓库`, `网站`, `文件`或`目录`中安装,
关于包的更多信息, 请参见[Racket中的包管理](https://docs.racket-lang.org/pkg/index.html),

+ 有些模块是相对于其他模块存在的, 不一定属于任何特定的`collection`或`package`, 例如, 在`DrRacke`t中, 如果你把到目前为止的定义保存在 `quick.rkt` 文件中(`Ctrl+s`), 并添加一行

```lisp
(provide rainbow square)
```

然后你可以在`DrRacket`中打开一个新的标签或窗口, 在与 `quick.rkt` 相同的目录中输入新程序 `use.rkt`,

```lisp
#lang racket
(require "quick.rkt")
(rainbow (square 5))
```

当你运行 `use.rkt` 时, 输出是一个彩虹样式的方块列表.
请注意, `use.rkt` 初始导入了 `racket`, `racket` 本身不提供任何造图函数, 但提供了`require`和函数调用的语法,

Racketeers (~~敲诈者~~) 通常把新程序和库写成模块, 通过相对路径和基于集合的路径相互导入,
当以这种方式开发的程序或库对其他人有用时, 它可以被注册为一个包, 特别是如果该实现被托管在`Git`仓库中,

### Macros

这里有另一个库可以尝试.

```lisp
(require slideshow/code)
> (code (circle 10))
```

结果不是一个圆圈, 而是代码的图片, 如果把它作为一个表达式, 就会产生一个圆圈.
换句话说, `code`不是一个函数, 而是一个用于创建图片的新句法形式, `syntactic form` ;
`(circle 10)`的位置上不是一个表达式, 而是由`code`的句法形式操纵的,

这有助于解释我们在上一节中说`racket`提供`require`和函数调用语法的意思, 库并不局限于输出值, 如`函数`; 它们也可以定义新的语法形式,
从这个意义上说, `Racket`根本就不是一种语言; 它更像是一个关于如何构造语言的想法, 这样你就可以扩展它或者创造全新的语言,

引入新的语法形式的一种方法是通过`define-syntax `与`syntax-rules`,

```lisp
(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))
>(pict+code (circle 10))
```

这种定义是一个`macro`--宏, `(pict+code expr)`部分是宏要使用的`pattern`;
程序中该`模式`的`实例`, 被对应模板中的实例所取代, 也就是会换成`(hc-append 10 expr (code expr))`,
在这里, `(pict+code (circle 10))`会匹配模式, `(circle 10)`匹配`expr`, 所以它被替换为`(hc-append 10 (circle 10) (code (circle 10)))`,

当然, 这种句法扩展是双向的: 发明一种新的语言可以使你更容易说出你想要的东西, 但对其他人来说却更难理解,
碰巧的是, `Racket`的开发者们在不断地发表包括`Racket`代码的讲座和论文, 对于从事这些产品的人来说, 了解`code`是值得的.

事实上, 你可能想看看[这个文件的来源](https://docs.racket-lang.org/quick/quick.scrbl),
你会看到它是以`#lang`开头的, 但除此之外看起来并不像`Racket`; 尽管如此, 我们还是通过将其源码作为`Racket`程序运行来构建这份文档,
我们必须使用比`syntax-rules` 更多的东西来扩展`Racket`的语法, 以便编写文档, 但`Racket`的语法扩展可以让你走得更远,

### 面向对象

`对象系统`是另一个值得`Racket`用户学习和使用的复杂语言扩展的例子,
对象有时比`函数`更好, 即使你有`lambda`, 而且对象对图形用户界面的效果特别好,
`Racket`的图形用户界面和图形系统的`API`是用`对象`和`类`来表达的,

`类`系统本身是由`racket/class`库实现的, 而`racket/gui/base`库提供了 `GUI` 和`绘图类`, 按照惯例, 这些类的名字以`%`结尾,

```lisp
(require racket/class
         racket/gui/base)
(define f (new frame% [label "My Art"]
                      [width 300]
                      [height 300]
                      [alignment '(center center)]))
> (send f show #t)
```

`new`形式创建了`类`的`实例`, 其中的初始化参数如`label`和`width`是通过名称提供的,
`send`形式调用对象的方法, 如`show`, 参数在方法名后面; 本例中的参数`#t`是布尔常数 `true`.

用`slideshow`生成的图片`封装`了一个函数, 该函数使用图形工具箱的绘图命令将图片渲染到一个绘图环境中, 例如一个框架中的画布, `anvas` in a `frame`.
`slideshow`中的`make-pict-drawer`函数暴露了图片的绘制函数, 我们可以在`canvas-painting`回调(`callback`)中使用`make-pict-drawer`来将图片绘制到`canvas`中,

```lisp
(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent f]
                 [style '(border)]
                 [paint-callback (lambda (self dc)
                                   (drawer dc 0 0))])))

> (add-drawing (pict+code (circle 10)))
> (add-drawing (colorize (filled-flash 50 30) "yellow"))
```

每个`canvas`都会拉伸, 以填充`frame`的相等部分, 因为框架默认就是这样管理它的`children`.

### Where to Go From Here

对`Racket`的介绍有意避开了许多介绍和区分于`Lisp, Scheme`的传统方式:
`前缀算术符号`, `symbols`, `quoting`和`quasiquoting lists`, `eval`, `first-class continuations`,
以及所有语法实际上只是一个伪装的`lambda`的想法,
虽然这些都是`Racket`的一部分, 但它们并不是`Racket`日常编程的主要成分,

相反, `Racket`程序员通常使用`functions`, `records`, 对象, 异常, 正则表达式, `模块`和`线程`进行编程,
也就是说, Racket不是一种 `极简主义` 的语言--这也是`Scheme`经常给人的印象, 而是一种拥有大量`库`和`工具`的丰富语言,

如果你是编程新手, 或者你有耐心看完一本教科书, 我们推荐你阅读[How to Design Programs](http://htdp.org/),
如果你已经读过了, 或者你想看看这本书会带你到哪里去, 那么请看[Continue: Web Applications in Racket](https://docs.racket-lang.org/continue/index.html).

对于有经验的程序员来说, 要想从`系统`的角度, 而不是图片的角度来继续游览`Racket`, 你的下一站是[More: Systems Programming with Racket](https://docs.racket-lang.org/more/index.html)

如果想开始深入了解完整的`Racket`语言和工具, 请继续阅读[The Racket Guide](https://docs.racket-lang.org/guide/index.html).

### racket guide

要将程序打包成可执行文件, 你有几个选择.

+ 在`DrRacket`中, 你可以选择`Racket|Create Executable...`菜单项.
+ 在命令行中, 运行`raco exe <src-filename>`, 其中`<src-filename>` 是程序的名字. 参见`raco exe`. 更多参考[创建独立的可执行文件](https://docs.racket-lang.org/raco/exe.html).
+ 在 `Unix` 或 `Mac OS` 中, 你可以通过在文件的最开头插入以下一行, 将程序文件变成一个可执行脚本.

关于脚本文件的更多信息, 请参见[脚本](https://docs.racket-lang.org/guide/scripts.html).

 ```lisp
 #! /usr/bin/env racket
 ```

同时, 在命令行中使用 `chmod +x filename`, 将文件的权限改为可执行.
只要`racket`在用户的可执行文件搜索路径中, 该脚本就能工作. 另外, 如果在`#!`之后使用`racket`的完整路径(和路径之间有一个空格), 用户的可执行文件搜索路径并不重要.

`racket`可以模仿传统的Lisp环境, 但我们强烈建议不要使用`Load`或者在模块外编写程序.
在模块外编写定义会导致糟糕的错误信息, 糟糕的性能, 以及尴尬的脚本来组合和运行程序.
这些问题并不是`racket`所特有的; 它们是传统`top-level`环境的基本限制,
在历史上, `Scheme`和`Lisp`的实现都是用特别的命令行标志, 编译器指令和构建工具来对抗这些限制.

`模块`系统的设计是为了避免这些问题. 所以脚本请从`#lang`开始, 从长远来看, 你会对`Racket`更加满意.

## 简单值

`Racket`值包括`数字`, `布尔值`, `字符串`和`字节字符串`. 在`DrRacket`和文档示例中(当你阅读彩色文档时), 值表达式显示为`绿色`.

数字以常规方式书写, 包括分数和.

```lisp
1       3.14
1/2     6.02e+23
1+2i    9999999999999999999999
```

`Boole`值即`#t`--真, 以及 `#f`--假. 然而在条件判断中, 所有非`#f` 值都被当成真的.

## 简单定义和表达式

程序模块的形式为

```lisp
#lang <langname>  <topform>*
```

其中 `topform` 可以是 `定义` 或 `表达式`. `REPL` 也会对 `topform` 进行计算.

在语法规范中, 灰色背景的文本, 如`#lang`, 代表字面文本.
在这种字面意义和非字面意义文本, 如`<id>`之间必须有空格, 但在`(`, `)`,`[`, 或`]`之前或之后不需要空格.
注释以`;`开始, 一直到行末, 被当成空白处理.

按照惯例, 语法中的`*`表示对前一个元素的零次或多次重复, `+`表示对前一个元素的一次或多次重复, `{}`将序列作为单个元素, 以备重复.

### 定义

定义的形式是

```lisp
( define  <id>  <expr> )
```

将 `id` 与 `expr` 的结果绑定, 而

```lisp
( define ( <id>  <id>*  ) <expr> )
```

将第一个`<id>`绑定到`函数`(也叫`过程`)上, 该函数接受由其余`<id>*`命名的参数.
在函数的情况下, `<expr>` 是函数的主体. 当函数被调用时, 它返回最后一个`<expr>`的结果.

在`引擎盖`下, `函数定义`实际上和`非函数定义`是一样的, 而且`函数名称`不一定要在函数调用中使用.
`函数`只是另一种值, 尽管其打印形式必然不如数字或字符串的打印形式完整.

`函数`定义可以为函数主体包括多个表达式.
在这种情况下, 当函数被调用时, 只有最后一个表达式的值被返回.
其他的表达式只是为了某些副作用而被计算, 例如`打印`.

```lisp
(define (bake flavor)
  (printf "preheating oven...\n")
  (string-append flavor " pie"))

> (bake "apple")
```

`Racket`程序员喜欢避免产生副作用, 所以一个定义通常只有一个表达式在其主体中.
不过, 理解定义主体中允许有多个表达式是很重要的, 因为它解释了为什么下面的`nobake`函数没有在其结果中包含参数.

```lisp
(define (nobake flavor)
  string-append flavor "jello")

> (nobake "green")
```

函数体中的`string-append flavor "jello"`被当成独立的元素, 最后只返回`"jello"`.

#### 关于缩进

换行和缩进对于解析`Racket`程序来说并不重要, 但大多数`Racket`程序员都使用一套标准的惯例来使代码更易读.
例如, 定义的主体通常在定义的第一行下缩进. 标识符紧跟在开放小括号之后, 不占多余的空间, 闭合小括号不另起一行.

当你在程序或`REPL`表达式中键入`Enter`时, `DrRacket`会自动按照标准样式缩进.
例如, 如果你在输入`(define (greet name)`后按下回车键, 那么`DrRacket`会自动为下一行插入两个空格.
如果你改变了一个`代码区域`, 你可以在`DrRacket`选中它并点击`Tab`, `DrRacket`将重新缩进代码(不插入任何换行符).

`Emacs`等编辑器在Racket或Scheme模式中, 也支持类似的缩进.

重新缩进不仅可以使代码更容易阅读, 还可以给你额外的反馈, 使你的括号与你的意图相匹配.
例如, 如果你在一个函数的最后一个参数后漏掉了一个闭合小括号, 自动缩进就会在第一个参数下开始下一行, 而不是在`define`关键字下.

```lisp
    (define (halfbake flavor
                      (string-append flavor " creme brulee"))
```

在这种情况下, 缩进有助于突出这个错误.
在其他情况下, 缩进可能是正常的, 而开放的小括号没有匹配的封闭小括号, `racket`和`DrRacket`都使用源头的缩进来提示可能缺少小括号的地方.

#### 标识符

标识符可以绑定到一段程序上. `Racket` 的标识符语法是非常自由的. 除了特殊字符

    ( ) [ ] { } " , ' ` ; # | \

除了构成`数字常数`的字符序列外, 几乎任何非空白字符的序列都可以作为`id`.
例如, `substring` 就是一个标识符. 另外, `string-append`和`a+b`是标识符, 而不是`算术表达式`. 这里还有几个例子.

```lisp
+
integer?
pass/fail
Hfuhruhurr&Uumellmahaye
john-jacob-jingleheimer-schmidt
a-b-c+1-2-3
```

### 函数 Calls,程序Applications

我们已经看到了许多`函数调用`(calls), 在更传统的术语中,它们被称为`应用过程`(procedure applications).
`函数调用`的语法是:

```lisp
( <id>  <expr>* )
```

其中`<expr>` 的数量决定了, 提供给名为`<id>`的函数的参数数量.
`racket`语言预先定义了许多函数`标识符`, 比如`substring`和`string-append`.更多的例子在下面.

在整个文档的`Racket`代码示例中, `预定义`的名称的被超链接到`参考手册`中. 因此,你可以点击`标识符`来获得关于其使用的全部细节.

```lisp
> (string-append "rope" "twine" "yarn")  ; append strings
"ropetwineyarn"
> (substring "corduroys" 0 4)            ; extract a substring
"cord"
> (string-prefix? "shoelace" "shoe")     ; recognize string prefix/suffix
#t
> (string-suffix? "shoelace" "shoe")
#f
> (string? "Ceci n'est pas une string.") ; recognize strings
#t
> (string? 1)
#f
> (sqrt 16)                              ; find a square root
4
> (sqrt -16)
0+4i
> (+ 1 2)                                ; add numbers
3
> (- 2 1)                                ; subtract numbers
1
> (< 2 1)                                ; compare numbers
#f
> (>= 2 1)
#t
> (number? "c'est une number")           ; recognize numbers
#f
> (number? 1)
#t
> (equal? 6 "half dozen")                ; compare anything
#f
> (equal? 6 6)
#t
> (equal? "half dozen" "half dozen")
#t
```

### 条件式;if,or和cond

下一个最简单的表达式是`if`条件.

```lisp
( if <expr> <expr> <expr> )
```

第一个`<expr>`总是被计算.
如果它产生一个非`#f`的值,那么第二个`<expr>`会被计算,作为整个`if`表达式的结果;  否则第三个`<expr>`会被计算为结果.
例子.

```lisp
> (if (> 2 3)
        "2 is bigger than 3"
        "2 is smaller than 3")
"2 is smaller than 3"

(define (reply s)
  (if (string-prefix? s "hello ")
        "hi!"
        "huh?"))
> (reply "hello racket")
"hi!"
> (reply "λx:(μα.α→α).xx")
"huh?"
```

`复杂`的条件式可以通过嵌套`if`表达式来形成.
例如,在前面的回复例子中,输入必须是一个字符串,因为`string-prefix?`在给出非字符串时将会出错.
你可以通过添加另一个`if`来移除这个限制, 首先检查输入是否是字符串.

```lisp
(define (reply-non-string s)
  (if (string? s)
      (if (string-prefix? s "hello ")
          "hi!"
          "huh?")
      "huh?"))
```

与其重复 `"huh?"` 的情况, 这个函数最好写成

```lisp
(define (reply-non-string s)
  (if (if (string? s)
            (string-prefix? s "hello ")
            #f)
      "hi!"
      "huh?"))
```

但是这些嵌套的`if`很难阅读. `Racket`通过`and`和`or`的形式提供了更易读的快捷方式.

```lisp
( and <expr>* )
( or <expr>* )
```

`and`形式是`短路`的: 当一个表达式产生`#f`时, 它就会停止并返回`#f`, 否则就会继续下去. `or` 形式在遇到一个`真`结果时也同样会短路.
例子.

```lisp
(define (reply-non-string s)
  (if (and (string? s) (string-prefix? s "hello "))
      "hi!"
      "huh?"))
> (reply-non-string "hello racket")
"hi!"
> (reply-non-string 17)
"huh?"
```

请注意,在上述语法中, `and` 和`or`形式可以与任何数量的表达式一起使用.
例子.

```lisp
(define (reply-only-enthusiastic s)
  (if (and (string? s)
                  (string-prefix? s "hello ")
                  (string-suffix? s "!"))
          "hi!"
          "huh?"))
 > (reply-only-enthusiastic "hello racket!")
"hi!"
> (reply-only-enthusiastic "hello racket")
"huh?"
```

另一种常见的嵌套`if`模式涉及一连串的测试,每个测试都有自己的结果.

```lisp
(define (reply-more s)
  (if (string-prefix? s "hello ")
      "hi!"
      (if (string-prefix? s "goodbye ")
          "bye!"
          (if (string-suffix? s "?")
              "I don't know"
              "huh?"))))
```

测试序列的简写是 `cond` 形式.

```lisp
( cond {[ <expr> <expr>* ]}* )
```

`cond`形式由一系列, `方括号`内子句(clause)组成.
在每个`子句`中,第一个`<expr>`是`测试表达式`. 如果它产生了`真值`,那么`子句`中剩下的`<expr>`就会被计算,
`clause`中的最后一个`expr`作为整个`cond`的值返回; 其余的`clause`被忽略.

如果测试`<expr>`产生`#f`, 那么子句中剩余的`<expr>`将被忽略,并继续计算下一个子句.
最后一个子句可以使用`else`作为`#t`的同义词.

使用 `cond`, `reply-more` 函数可以更清楚地写成如下.

```lisp
#lang racket
(define (reply-more s)
  (cond
    [(string-prefix? s "hello ")
     "hi!"]
    [(string-prefix? s "goodbye ")
     "bye!"]
    [(string-suffix? s "?")
     "I don't know"]
    [else "huh?"]))

> (reply-more "hello racket")
"hi!"
> (reply-more "goodbye cruel world")
"bye!"
> (reply-more "what is your favorite color?")
"I don't know"
> (reply-more "mine is lime green")
"huh?"
```

为`cond` clauses 使用`方括号`是一种惯例.
在`Racket`中, 小括号和方括号实际上是可以互换的,只要`(`与`)`匹配, `[`与`]`匹配即可.
在一些关键的地方使用`方括号`可以使`Racket`的代码更具有可读性.

### 函数调用, Again

在我们先前的`函数调用`语法中,我们过度简化了. `函数调用`的实际语法允许提供任意表达式作为`函数`, 而不仅仅是`<id>`.

```lisp
( <expr> <expr>* )
```

第一个`<expr>`通常是一个`<id>`, 例如`string-append`或`+`, 但它也可以是任何可以计算为`函数`的东西. 例如,它可以是一个`条件表达式`.

```lisp
(define (double v)
  ((if (string? v) string-append +) v v))
> (double "mnah")
"mnahmnah"
> (double 5)
10
```

从语法上讲, 函数调用中的第一个表达式甚至可以是一个`数字`--但这将导致一个错误,因为数字不是`函数`.

```lisp
> (1 2 3 4)
application: not a procedure;
  expected a procedure that can be applied to arguments
    given: 1
```

当你不小心省略了`函数名`,或者在表达式周围使用了额外的括号时,你最经常得到的是像这样的`expected a procedure`的错误.

### 使用匿名函数,lambda

如果你必须为所有的数字命名,那么用`Racket`编程就会很乏味. 这样你不能用`(+1 2)`, 而是得写

```lisp
> (define a 1)
> (define b 2)
> (+ a b)
3
```

事实证明,必须为所有的函数命名也是很乏味的.
例如,你可能有函数`twice`,它接受一个`函数`和一个`参数`.
如果你已经有了另一个函数的名字,比如`sqrt`, 那么使用`twice`就很方便.

```lisp
(define (twice f v)
  (f (f v)))
> (twice sqrt 16)
2
```

如果你想调用尚未定义的函数,你可以定义它,然后把它传给 `twice`.

```lisp
(define (louder s)
  (string-append s "!"))
> (twice louder "hello")
"hello!!"
```

但如果只是在`twice`这儿用了下`louder`, 那么没必要写个完整的定义.
在`Racket`中,你可以使用`lambda`表达式来直接产生一个函数.
`lambda`形式后面是函数`参数`的`标识符`, 然后是函数的`主体`表达式.

```lisp
( lambda ( <id>* ) <expr>+ )
```

计算`lambda`形式本身会产生一个`函数`.

```lisp
> (lambda (s) (string-append s "!"))
#<procedure>
```

使用`lambda`,上面对 `twice` 的调用可以改写为

```lisp
> (twice (lambda (s) (string-append s "!"))
                  "hello")
"hello!!"
> (twice (lambda (s) (string-append s "?!"))
                  "hello")
"hello?!?!"
```

`lambda`的另一个用途是作为函数返回的结果.

```lisp
(define (make-add-suffix s2)
  (lambda (s) (string-append s s2)))
> (twice (make-add-suffix "!") "hello")
"hello!!"
> (twice (make-add-suffix "?!") "hello")
"hello?!?!"
> (twice (make-add-suffix "...") "hello")
"hello......"
```

`Racket`是一种`文法定域`(lexically scoped)的语言,
这意味着`make-add-suffix`返回的`匿名函数`中的`s2`, 总是指向`匿名函数`被创建时指向的参数.
换句话说, `lambda` 生成的函数 `记住` 了正确的`s2`. 类似于`mma` 中的 `Module`.

```lisp
> (define louder (make-add-suffix "!"))
> (define less-sure (make-add-suffix "?"))
> (twice less-sure "really")
"really??"
> (twice louder "really")
"really!!"
```

到目前为止,我们将形式为`(define <id> <expr>)`的定义称为 `非函数定义`.
这种描述有误导性, 因为`expr`可以是`lambda`形式, 在这种情况下,该定义等同于使用 `函数` 定义形式. 例如,下面两个`louder`的定义是等价的.

```lisp
(define (louder s)
  (string-append s "!"))
 (define louder
  (lambda (s)
    (string-append s "!")))
> louder
#<procedure:louder>
```

注意,在第二种情况下, `louder` 的表达式是一个用 `lambda` 写的 `匿名`函数.
但是如果可能的话,编译器还是会推断出一个名字,以使打印和错误报告更有信息含量.

### 局部绑定,define,let和let*

现在是时候收回我们`Racket`语法中的另一个简化了. 在`函数`的主体中, `定义`可以出现在主体表达式之前.

```lisp
( define ( <id> <id>* ) <define>* <expr>+ )
( lambda ( <id>* ) <define>* <expr>+ )
```

位于`函数体`开始的定义是函数体的`局部定义`.
例子.

```lisp
(define (converse s)
  (define (starts? s2) ; 对 converse 是定域的, 是否用 s2 开始
    (define spaced-s2 (string-append s2 " ")) ;  对 starts? 是定域的, 给 s2 追加空格.
    (string-prefix? s spaced-s2)) ; s 是否为前缀
  (cond
   [(starts? "hello") "hi!"]
   [(starts? "goodbye") "bye!"]
   [else "huh?"]))
> (converse "hello world")
"hi!"
> (converse "hellonearth")
"huh?"
> (converse "goodbye friends")
"bye!"
> (converse "urp")
"huh?"
> starts? ; 在 converse 之外, 所以无法使用...
starts?: undefined;
 cannot reference an identifier before its definition
  in module: top-level
```

另一种创建`局部绑定`的方法是 `let` 形式. `let` 的优点是它可以在任何表达式的位置使用.
而且, `let` 一次可以绑定许多标识符, 而不要求为每个`标识符`单独定义.

```lisp
( let ( {[ <id>  <expr> ]}* )  <expr>+ )
```

每个绑定子句有一个`<id>`和一个` <expr>`, 用`方括号`包围, `子句`后面的表达式`<expr>+`是`let`的主体.
在每个子句中, `<id>` 被绑定到 `<expr>` 的结果上, 以便在主体中使用.

```lisp
> (let ([x (random 4)]
             [o (random 4)])
        (cond
            [(> x o) "X wins"]
            [(> o x) "O wins"]
            [else "cat's game"]))
"O wins"
```

`let`形式的绑定只在`let`的主体中可用, 所以绑定子句之间不能相互`引用`.
与此相反, `let*`形式允许后面的子句使用前面的`绑定`.

```lisp
> (let* ([x (random 4)]
               [o (random 4)]
               [diff (number->string (abs (- x o)))])
          (cond
              [(> x o) (string-append "X wins by " diff)]
              [(> o x) (string-append "O wins by " diff)]
              [else "cat's game"]))
"X wins by 2"
```

## 列表,循环和递归

`Racket`是`Lisp`语言的一种方言,它的名字最初是 `LISt Processor`. 内置的`列表`数据类型仍然是该语言的一个突出特点.

`list`函数接收任意数量的值并返回一个包含这些值的列表.

```lisp
> (list "red" "green" "blue")
'("red" "green" "blue")
> (list 1 2 3 4 5)
'(1 2 3 4 5)
```

列表通常带`'`打印, 但列表的打印形式取决于其内容. 更多信息请参见`对儿和列表`.

正如你所看到的,`列表`的结果在 `REPL` 中打印为: 一个引号`'`接着是`小括号`包裹着列表元素的形式.
这里有个混淆的机会, 因为`小括号`既用于表达式, 如 `(list "red" "green" "blue")` ;  也用于打印结果, 如 `'("red" "green" "blue")`.

除了`引号`外,在文档和`DrRacket`中, 结果的小括号以`蓝色`打印, 而表达式的小括号是`棕色`的.
许多预定义函数对列表进行操作.下面是几个例子.

```lisp
> (length (list "hop" "skip" "jump"))        ; 计算元素数
3
> (list-ref (list "hop" "skip" "jump") 0)    ; 按位置提取
"hop"
> (list-ref (list "hop" "skip" "jump") 1)
"skip"
> (append (list "hop" "skip") (list "jump")) ; 组合列表
'("hop" "skip" "jump")
> (reverse (list "hop" "skip" "jump"))       ; 倒转顺序
'("jump" "skip" "hop")
> (member "fall" (list "hop" "skip" "jump")) ; 检查元素是否存在
#f
```

### 预定义的列表循环

除了像`append`这样的简单操作外, `Racket` 还包括对列表中的元素进行`迭代`的函数.
这些迭代函数的作用类似于 `Java`,`Racket` 和其他语言中的 `for`.
`Racket` 迭代的`主体`被打包成`函数`, 应用于每个元素, 所以`lambda`形式在与`迭代`函数的结合中变得特别方便.

不同的`list-iteration`函数以不同的方式组合迭代结果. `map` 函数使用每个元素的结果来创建一个新的列表.

```lisp
> (map sqrt (list 1 4 9 16))
'(1 2 3 4)
> (map (lambda (i)
         (string-append i "!"))
       (list "peanuts" "popcorn" "crackerjack"))
'("peanuts!" "popcorn!" "crackerjack!")
```

`andmap`和`ormap`函数通过`and`或`or`来组合结果.

```lisp
> (andmap string? (list "a" "b" "c"))
#t
> (andmap string? (list "a" "b" 6))
#f
> (ormap number? (list "a" "b" 6))
#t
```

`map`,`andmap` 和 `ormap` 函数都可以处理多个列表,而不仅仅是一个列表.
这些列表必须都有相同的长度, 并且给定的函数必须从每个列表接受一个参数.

```lisp
> (map (lambda (s n) (substring s 0 n))
                (list "peanuts" "popcorn" "crackerjack")
                (list 6 3 7))
'("peanut" "pop" "cracker")
```

`filter`函数保留主体结果为`真`的元素,并丢弃其为`#f`的元素.

```lisp
> (filter string? (list "a" "b" 6))
'("a" "b")
> (filter positive? (list 1 -2 6 7 0))
'(1 6 7)
```

`foldl`函数推广了一些迭代函数. 它使用`per-element`函数来处理每个元素, 并将其与 `当前` 值相结合,
所以`per-element`函数需要额外的第一个参数. 另外,在列表前必须提供一个开始的 `当前` 值.

```lisp
> (foldl (lambda (elem v)
           (+ v (* elem elem)))
         0
         '(1 2 3))
14
```

尽管`foldl`具有通用性,但它并不像其他函数那样流行.
原因之一是 `map`,`ormap`,`andmap` 和 `filter` 涵盖了最常见的列表循环.

`Racket`提供了一个通用的`list comprehension` 形式`for/list`, 它通过对序列迭代(itering through sequence)来建立一个列表.
`列表解析`和相关的迭代形式在 `Iterations and Comprehensions`   中有描述.

### 从零开始的列表迭代

尽管`map`和其他迭代函数是预定义的, 但它们在任何有趣的意义上都不是原始的.
你可以使用少量的列表`原语`(primitives)来编写等价的迭代.

由于`Racket`列表是一个链表, 在一个非空列表上的两个核心操作是

+ `first`: 获取列表中的第一项;以及
+ `rest`: 获取列表的其余部分.

例子.

```lisp
> (first (list 1 2 3))
1
> (rest (list 1 2 3))
'(2 3)
```

要为`链表`创建新的`节点`--即添加到列表的前面--使用 `cons` 函数, 它是 `construct` 的缩写.
要从`空列表`开始,使用`empty` 常量.

```lisp
> empty
'()
> (cons "head" empty)
'("head")
> (cons "dead" (cons "head" empty))
'("dead" "head")
```

要处理`列表`, 你需要能够区分`空列表`和`非空列表`,
因为`first`和`rest`只对非空列表工作. 函数 `empty?` 检测`空列表`, 而 `cons? `检测`非空列表`.

```lisp
> (empty? empty)
#t
> (empty? (cons "head" empty))
#f
> (cons? empty)
#f
> (cons? (cons "head" empty))
#t
```

有了这些片段,你可以写出你自己版本的`length`函数, `map`函数等.
例子.

```lisp
(define (my-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (my-length (rest lst)))]))
> (my-length empty)
0
> (my-length (list "a" "b" "c"))
3

(define (my-map f lst)
    (cond
        [(empty? lst) empty]
        [else (cons (f (first lst))
                   (my-map f (rest lst)))]))
> (my-map string-upcase (list "ready" "set" "go"))
'("READY" "SET" "GO")
```

如果上述定义的推导对你来说是神秘的,可以考虑阅读[如何设计程序](https://htdp.org/).
如果你只是对使用`递归调用`而不是`循环结构`感到怀疑,那么请继续阅读.

### 尾部递归

对于长度为`n`的列表, `my-length` 和 `my-map` 函数都在`O(n)` 空间内运行.
通过想象`(my-length (list "a" "b" "c"))`必须如何计算,就很容易看出这一点.

```lisp
(my-length (list "a" "b" "c"))
= (+ 1 (my-length (list "b" "c")))
= (+ 1 (+ 1 (my-length (list "c"))))
= (+ 1 (+ 1 (+ 1 (my-length (list)))))
= (+ 1 (+ 1 (+ 1 0)))
= (+ 1 (+ 1 1))
= (+ 1 2)
= 3
```

对于一个有`n`个元素的列表,求值会堆积`n`个`(+1 ...)`加法, 最后在列表耗尽时将它们加起来.

你可以通过`沿途加法`来避免堆积加法. 为了以这种方式积累长度,我们需要一个同时接收列表和到目前为止看到的列表长度的函数;
下面的代码使用了一个本地函数`iter`,在参数`len`中积累长度.

```lisp
(define (my-length lst)
  ; local function iter:
    (define (iter lst len)
         (cond
             [(empty? lst) len]
             [else (iter (rest lst) (+ len 1))]))
  ; body of my-length calls iter:
  (iter lst 0))
```

现在计算看起来像这样.

```lisp
(my-length (list "a" "b" "c"))
= (iter (list "a" "b" "c") 0)
= (iter (list "b" "c") 1)
= (iter (list "c") 2)
= (iter (list) 3)
3
```

修订后的`my-length`在恒长空间中运行, 正如上面的计算步骤所提示的那样.

也就是说,当一个函数调用的结果,比如`(iter (list "b" "c") 1)`, 正好是另一个函数调用的结果,比如 `(iter (list "c") 2)` ,
那么第一个函数原地等待第二个函数的结果, 因为那会毫无理由地占用空间.

这种计算行为有时被称为`尾部调用优化`(tail-call optimization), 但是在 `Racket` 中它不仅仅是一种 `优化`; 它是对代码运行方式的一种保证.
更确切地说, 相对于表达式`A`, 处于`尾部`的表达式`B`不会占用额外的计算空间.

在`my-map`的例子中, `O(n)`空间复杂度是合理的, 因为它必须生成一个大小为`O(n)`的结果. 
尽管如此,你可以通过累积结果列表来减少常数因子. 
唯一的问题是,累积的列表将是`反向`的,所以你必须在最后把它倒过来.

> 试图像这样减少一个常数因子通常是不值得的,如下文所述.

```lisp
(define (my-map f lst)
    (define (iter lst backward-result)
        (cond
            [(empty? lst) (reverse backward-result)]
            [else (iter (rest lst)
                                 (cons (f (first lst))
                                              backward-result))]))
      (iter lst empty))
```

事实证明,如果你写

```lisp
(define (my-map f lst)
    (for/list ([i lst])
        (f i)))
```

那么函数中的`for/list`形式被扩展为与`iter`局部定义基本相同的代码, 然后被使用. 区别只是语法更方便.

### 递归与迭代

`my-length`和`my-map`的例子表明, `迭代`(iteration)只是`递归`(recursion)的一种特殊情况.

在许多语言中,重要的是要尽可能多地将计算纳入`迭代`形式.
否则,性能会很差,而且适度的大输入会导致`堆栈溢出`(stack overflow).

同样,在`Racket`中, 当计算很容易在`常数空间`中进行时,有时必须确保使用`尾部递归`以避免`O(n)`空间的消耗.

同时, 递归在`Racket`中不会导致特别糟糕的性能,也不存在`堆栈溢出`的问题;
如果一个计算涉及太多的上下文,你可能会耗尽内存,但耗尽内存通常需要比其他语言中触发`堆栈溢出`更深的递归数量级.
这些考虑,再加上`尾部递归`程序自动运行, 等价于循环的事实, 导致`Racket`程序员拥抱递归形式而不是避免它们.

例如, 假设你想从`列表`中删除连续的重复内容.
虽然这样的函数可以写成`循环`,在每次迭代中记住前一个元素,但`Racket`程序员更有可能直接写成下面这样.

```lisp
(define (remove-dups l)
  (cond
      [(empty? l) empty]
       [(empty? (rest l)) l]
       [else
          (let ([i (first l)])
              (if (equal? i (first (rest l)))
                    (remove-dups (rest l))
                    (cons i (remove-dups (rest l)))))]))
> (remove-dups (list "a" "b" "b" "b" "c" "c"))
'("a" "b" "c")
```

一般来说, 对于一个长度为`n`的输入列表,这个函数会消耗`O(n)`的空间,但这很好,因为它产生的结果是`O(n)`.
如果输入的列表恰好大部分是连续重复的,那么产生的列表可以比`O(n)`小得多--而且`remove-dups`也会使用比`O(n)`小得多的空间.

原因是,当该函数丢弃重复数据时,它直接返回 `remove-dups` 调用的结果,因此`尾部调用`的 "优化" 开始发挥作用.

```lisp
(remove-dups (list "a" "b" "b" "b" "b" "b"))
= (cons "a" (remove-dups (list "b" "b" "b" "b" "b")))
= (cons "a" (remove-dups (list "b" "b" "b" "b")))
= (cons "a" (remove-dups (list "b" "b" "b")))
= (cons "a" (remove-dups (list "b" "b")))
= (cons "a" (remove-dups (list "b")))
= (cons "a" (list "b"))
= (list "a" "b")
```

### 对儿,列表和Racket语法

`cons`函数实际上接受任何`成对值`, 第二个参数可以不是`列表`.
当第二个参数不是`empty`, 并且本身不是由`cons`产生的, 其结果会以特殊的方式打印出来.
用`cons`连接的两个值被打印在小括号`()`之间, 但中间有一个`点`(即一个由`空白`包围的`句号`).

```lisp
> (cons 1 2)
'(1 . 2)
> (cons "banana" "split")
'("banana" . "split")
```

因此, 由 `cons` 产生的值并不总是列表.一般来说, `cons`的结果是一个`pair`.
`cons?`函数更传统的名称是`pair?`, 从现在开始我们将使用这个传统名称.

对于非列表`对`来说, `rest`这个名字也没有什么意义; `first`和`rest`更传统的名字分别是`car`和`cdr`.
当然, 传统的名字也没啥含义. 只要记住 `a` 在 `d` 之前, 而`cdr`的发音是 `could-er`. 例子:

```lisp
> (car (cons 1 2))
1
> (cdr (cons 1 2))
2
> (pair? empty)
#f
> (pair? (cons 1 2))
#t
> (pair? (list 1 2 3))
#t
```

`Racket`的`pair`数据类型, 及其与列表的关系基本上是历史原因, 还有用于打印的`点`符号, 和滑稽的名字`car`和`cdr`.
然而, `pair`已经深深地融入了`Racket`的文化, 规范和实现中, 所以它们在语言中仍然存在.

如果犯错误, 你很有可能得到非列表`pair`, 比如不小心把`cons`的参数颠倒了:

```lisp
> (cons (list 2 3) 1)
'((2 3) . 1)
> (cons 1 (list 2 3))
'(1 2 3)
```

有时, 会刻意使用`非列表对`. 例如, `make-hash`函数接收`pair`的列表, 其中每个`pair`的`car`是一个`key`, `cdr`是一个任意值.

唯一会让`Racket`新手更加困惑的是 `pair`的打印约定, 如果其中第二个元素是`pair`, 但不是`list`时:

```lisp
> (cons 0 (cons 1 2))
'(0 1 . 2)
```

一般来说, 打印`pair`的规则如下: 使用`dot`符号, 除非`点`的后面紧跟着`开放的小括号`. 在这种情况下, 删除`点`, `开放括号`和匹配的`封闭括号`.
因此, `'(0 . (1 . 2))` 变成 `'(0 1 . 2)`, 而 `'(1 . (2 . (3 . ())))`成为 `'(1 2 3)`.

### 使用quote引用Pairs和Symbols

`列表`在打印时前面有一个引号`'`, 但如果`列表`的某个元素本身就是`列表`, 那么`内部`的列表就不会伴随引号`'`.

```lisp
> (list (list 1) (list 2 3) (list 4))
'((1) (2 3) (4))
```

特别是对于`嵌套`的列表, `quote`形式可以让你把`列表`写成`表达式`, 基本上与`列表`的打印方式相同.

```lisp
> (quote ("red" "green" "blue"))
'("red" "green" "blue")
> (quote ((1) (2 3) (4)))
'((1) (2 3) (4))
> (quote ())
'()
```

`quote`形式也适用于`点`符号, 无论`quoted`形式是否遵循`点`-`括号`抵消规则:

```lisp
> (quote (1 . 2))
'(1 . 2)
> (quote (0 . (1 . 2)))
'(0 1 . 2)
```

当然, 任何种类的列表都可以被嵌套.

```lisp
> (list (list 1 2 3) 5 (list "a" "b" "c"))
'((1 2 3) 5 ("a" "b" "c"))
> (quote ((1 2 3) 5 ("a" "b" "c")))
'((1 2 3) 5 ("a" "b" "c"))
```

如果你用`quote`包裹`标识符`, 那么你得到的输出看起来像`标识符`, 但有一个`'`前缀.

```lisp
> (quote jane-doe)
'jane-doe
```

打印出来像带引号的`标识符`的`值`是一个`符号`(`symbol`). 就像`括号内的输出`不应该与`表达式`相混淆一样, 打印出来的`符号`也不应该与`标识符`相混淆.
特别是, 符号`(引号map)`与`map`标识符, 或与`map`绑定的预定义函数没有任何关系, 只是`符号`和`标识符`碰巧是由相同的字母组成的.

事实上, `符号`的内在价值无非是它的`字符内容`. 在这个意义上, `符号`和`字符串`几乎是一回事, 主要区别在于它们的打印方式.
函数`symbol->string`和`string->symbol`可以在它们之间进行转换. 举例说明:

```lisp
> map
#<procedure:map>
> (quote map)
'map
> (symbol? (quote map))
#t
> (symbol? map)
#f
> (procedure? map)
#t
> (string->symbol "map")
'map
> (symbol->string (quote map))
"map"
```

与列表的`quote`自动应用于`嵌套列表`的方式类似, `quote`应用到`标识符`组成的`括号序列`上时, 会自动应用于每个`标识符`, 以创建`符号`的列表.

```lisp
> (car (quote (road map)))
'road
> (symbol? (car (quote (road map))))
#t
```

当`符号`位于用`'`打印的`列表`内时, `符号`前面的`'`将被省略, 因为`'`已经在起到效果.

```lisp
> (quote (road map))
'(road map)
```

`quote`形式对`数字`或`字符串`等字面表达式没有影响.

```lisp
> (quote 42)
42
> (quote "on the record")
"on the record"
```

### 使用quote的缩写'

正如你可能猜到的那样, 你可以通过在某个`形式`前面加上`'`来简略地使用`quote`.

```lisp
> '(1 2 3)
'(1 2 3)
> 'road
'road
> '((1 2 3) road ("a" "b" "c"))
'((1 2 3) road ("a" "b" "c"))
```

在文档中, 表达式内的 `'` 与它后面的`形式`一起被打印成绿色, 因为这个组合是一个常量表达式.
在`DrRacket`中, 只有`'`被染成绿色. `DrRacket`的做法更准确, 因为`quote`的含义可以根据表达式的上下文而变化.
但在文档中, 我们通常认为`标准绑定` are in scope, 因此我们将`引用形式`涂成绿色, 以增加清晰度.

`'`以相当直白的方式扩展为一个引用形式. 你可以看到, 如果你在一个有`'`的形式前面再加一个`'`.

```lisp
> (car ''road)
'quote
> (car '(quote road))
'quote
```

`'`的缩写在`输出`和`输入`中都有效. 在打印输出时, `REPL`的 printer 将符号`'quote` 识别为双元素列表的首个元素, 在这种情况下, 它使用`'`来打印输出.

```lisp
> (quote (quote road))
''road
> '(quote road)
''road
> ''road
''road
```

### Lists和Racket语法

现在你知道了关于`paris`和`lists`的真相, 也看到了`quote`, 你已经准备好了理解, 我们简化`Racket`真实语法的主要方式.

`Racket`的语法不是直接用`字符流`(character streams)来定义的. 相反, 语法是由两层决定的:

+ `reader` layer, 它将字符的序列转化为`lists`, `symbols`和其他`常量`; 以及
+ `expander` layer, 它处理`列表`, `符号`和其他`常量`, 将它们解析为一个`表达式`. 相当于`Mathematica`的`InputForm`??

`printing`和`reading`的规则是一起的.
例如, `列表`是用`小括号`打印的, 读取一对`小括号`会产生`列表`.
同样地, 非列表`pair`用`点`符号打印, 输入中若含有的`点`, 就反过来用`点`符号的规则解析成`pair`.

表达式的`读取层`的一个结果是, 你可以在非`quoted`形式的表达式中, 使用`点`符号.

```lisp
> (+ 1 . (2))
3
```

可以这样做是因为`(+ 1 . (2))` 只是`(+ 1 2)`的另一种写法. 在实践中使用`点`符号来写应用表达式不是好主意; 这只是`Racket`语法定义方式的一个结果.

通常情况下, 读者只允许在括号内的序列中使用`.`, 并且需要在序列的最后一个元素之前使用.
然而, 一对`.`也可以出现在括号序列的单个元素周围两边, 只要该元素不是`第一个`或`最后一个`.
这样的一对`.`将触发了`reader`转换, 将`.`之间的元素移到列表的前面. 这种转换实现了一种通用的中缀(`infix`)记号.

```lisp
> (1 . < . 2)
#t
> '(1 . < . 2)
'(< 1 2)
```

这种`双点符号`是非传统的, 它与`非列表对`的`点`符号基本上没有关系. `Racket`程序员很少使用`infix`约定--主要用于不对称的二进制运算符, 如`<`和`is-a?`.

## 内置的数据类型

[The Reader](https://docs.racket-lang.org/reference/reader.html#%28part._parse-vector%29)

上一章介绍了`Racket`的一些内置数据类型: `数字`, `布尔值`, `字符串`, `列表`和`过程`.
本节将对简单形式的内置数据类型进行更全面的介绍.

```lisp
数字: 5,    0.99,    1/2,   1+2i
字符串: "Apple",    "\u03BB"

符号: 'a,    '|one, two|,   #ci'A ,   (string->symbol "one, two")    

布尔值:  #t,    #f,    "字符串" 相当于 #t
字符 :  #\A,    #\u03BB,    #\space,    #\newline.
字节串 : #"Apple",    #"\0\0"


关键字: '#:apple,   (string->keyword "apple"),   
pair, list: '(1 . 2),   '((1 . 2) . 3),   '(0 1 2),   (cons 0 (cons 1 (cons 2 null)))


矢量: #("a" "b" "c"),   #4(baldwin bruce),    '#("a" "b" "c")
```

### 布尔值 Booleans

`Racket`有两个特别的`常量`来表示`布尔值`. `#t`表示真, `#f`表示假. 大写的`#T`和`#F`被解析为相同的值, 但小写的形式更受欢迎.
`boolean?` procedure 可以识别这两个布尔值常量. 在`if`, `cond`, `and`, `or`等表达式的测试结果中, 除了`#f`以外的任何值都算作真.
例子:

> (= 2 (+ 1 1))

```lisp
#t
> (boolean? #t)
#t
> (boolean? #f)
#t
> (boolean? "no")
#f
> (if "no" 1 0)
1
```

### 数字 Numbers

`Racket`数字可以是`精确`的, 也可以是`不精确`(inexact)的.

+ 精确的数字可以是:
  + 任意大或小的`整数`, 如`5`, `9999999999999999`, 或`-17`.
  + 有理数, 两个任意大或小的`整数`精确比值, 如`1/2`, `999999999999/2`, 或`-3/4`;
  + 具有精确`实部`和`虚部`的`复数`(其中虚部不为零), 如`1+2i`或`1/2+3/4i`.

+ 不精确的数是可以是:
  + 数字的`IEEE`浮点表示, 例如`2.0`或`3.14e+87`, 其中`IEEE`的无穷数和非数被写成`+inf.0`, `-inf.0`和`+nan.0`(或`-nan.0`);
  + `复数`, 其实部和虚部都是`IEEE`的浮点表示, 如`2.0+3.0i`或`-inf.0+nan.0i`; 作为一种特殊情况, 不精确复数可以有精确的`零实部`和不精确的`虚部`.

`非精确数`打印时带有小数点或指数符号, 而精确数则打印为`整数`和`分数`.
同样的约定适用于读取`数字常量`, 但`#e`或`#i`可以作为数字的前缀, 强制其解析为`精确`或`非精确`数字. 前缀`#b`, `#o`和`#x`指定将数字解释为`二进制`, `八进制`和`十六进制`.
例子:

```lisp
> 0.5
0.5
> #e0.5
1/2
> #x03BB
955
```

涉及`不精确`数字的计算会产生不精确的结果, 所以不精确性会污染数字.
不过要注意的是, `Racket`没有提供 `不精确的布尔`, 所以对`不精确`数字进行比较时,仍然产生`精确`的结果.
procedures `exact->inexact` 和 `inexact->exact` 在这两种类型的数字之间进行转换.
举例说明:

```lisp
> (/ 1 2)
1/2
> (/ 1 2.0)
0.5
> (if (= 3.0 2.999) 1 2)
2
> (inexact->exact 0.1)
3602879701896397/36028797018963968
```

当精确的结果需要表示成非有理数的实数时, 例如`sqrt`, `log`和`sin`等 procedure 也会产生`非精确`的结果. `Racket`只能表示有理数, 以及有理数组成的`复数`.
例子:

```lisp
> (sin 0)   ; rational...
0
> (sin 1/2) ; not rational...
0.479425538604203
```

就`性能`而言, 小整数的计算通常最快, 其中 `小` 是指`数字`比机器`字大小`的`有符号数字`少一个比特.
用非常大的`精确整数`或`非整数`的`精确数字`进行计算, 可能比用`非精确数字`进行计算要花费更多资源.

```lisp
(define (sigma f a b)
  (if (= a b)
      0
      (+ (f a) (sigma f (+ a 1) b))))
> (time (round (sigma (lambda (x) (/ 1 x)) 1 2000)))
cpu time: 29 real time: 15 gc time: 6 ; 花费更多时间
> (time (round (sigma (lambda (x) (/ 1.0 x)) 1 2000)))
cpu time: 0 real time: 0 gc time: 0
```

`整数`, `有理数`, `实数`(总是`有理数`)和`复数`等数字分类(categories)是以通常的方式定义的,
可以由 `integer?`, `rational?`, `real?` 和 `complex?` 等 procedures 识别, 除了通常的`number?` .
少数数学程序只接受`实数`, 但大多数实现了对`复数`的标准扩展. 例子:

```lisp
> (integer? 5)
> (complex? 5)
> (integer? 5.0)
> (integer? 1+2i)
> (complex? 1+2i)
> (complex? 1.0+2.0i)
> (abs -5)
> (abs -5+2i)
abs: contract violation
  expected: real?
  given: -5+2i
> (sin -5+2i)
3.6076607742131563+1.0288031496599335i
```

`=` 过程(procedure) 比较数字是否相等.
如果同时给定了`非精确数`和`精确数`进行比较, 那么在比较之前, 基本上它会将`非精确数`转换为`精确数`.
`eqv?` (因此也就是`equal?`)过程, 与此相反, 它在比较数字时既考虑`exactness`又考虑数值的`equality`. 例子:

```lisp
> (= 1 1.0)
#t
> (eqv? 1 1.0)
#f
```

小心涉及`不精确数字`的比较, 由于其性质, 可能会有令人惊讶的行为.
即使是表面上简单的`不精确数`, 也可能不是你认为的那样; 例如, 虽然一个以`2`为基数的`IEEE浮点数`可以精确表示`1/2`, 但它只能近似表示`1/10`.

例子.

```lisp
> (= 1/2 0.5)
#t
> (= 1/10 0.1)
#f
> (inexact->exact 0.1)
3602879701896397/36028797018963968
```

### 字符 Characters

`Racket`字符对应于`Unicode`标量值.
粗略的说, `标量值`是一个无符号的整数, 可以表示成`21`位`bit`, 它对应自然语言的`字符`, 或字符的某个`部分`.
从技术上讲, `标量值`是比`Unicode`标准中的`字符` 更简单的概念, 但它作为近似说法, 在许多方面都很方便.
例如, 任何带`accent`的罗马字母都可以表示为`标量值`, 任何常见的`中文字符`也是如此.

尽管每个`Racket`字符对应一个整数, 但`字符`数据类型与`数字`是分开的.
`char->integer`和`integer->char`程序可以在`scalar-value`的数字和对应的字符之间进行转换.

可打印的字符通常打印为`#\`,  后面是代表的字符.
不可打印的字符通常以`#u`的形式打印, 后面是标量值的十六进制数字.
有几个字符按特殊方式打印的; 例如, `空格`和`换行`字符分别打印为`#\space`和`#\newline`.
例子.

```lisp
> (integer->char 65)
#\A
> (char->integer #\A)
65
> #\λ
#\λ
> #\u03BB
#\λ
> (integer->char 17)
#\u0011
> (char->integer #\space)
32
```

相较于打印字符结果的`字符常数`语法, `display` procedure 直接将`字符`写入当前的`输出端口`(见输入和输出).
例子.

```lisp
> #\A
#\A
> (display #\A)
A
```

`Racket`提供了一些关于字符的`分类`和`转换`程序.
但要注意的是, 一些`Unicode`字符的转换只有在字符串中, 才会像人类所期望的那样进行(例如, upcasing `ß` 或 downcasing `Σ`). 例子.

```lisp
> (char-alphabetic? #\A)
> (char-numeric? #\0)
> (char-whitespace? #\newline)
> (char-downcase #\A)
> (char-upcase #\ß)
```

procedure `char=?` 比较两个或多个字符, `char-ci=?` 比较字符时忽略大小写.
`eqv?` 和 `equal?` 过程在字符上的行为, 与 `char=?` 相同; 当你想更具体地声明被比较的值是`字符`时, 使用`char=?`
例子.

```lisp
> (char=? #\a #\A)
#f
> (char-ci=? #\a #\A)
#t
> (eqv? #\a #\A)
#f
```

### 字符串 Unicode

`string`是一个固定长度的`字符`(characters)数组.
它使用`双引号`进行打印, 其中`string`中的双引号`"`, 和反斜线`\`字符用反斜线转义.

还支持其他常见的`string`转义,包括换行的`\n`, 回车的`\r`, 八进制转义(`\`后面跟三个八进制数字) 以及十六进制转义(使用`\u`,最多四个数字).
在打印`string`时,`string`中的不可打印字符通常用`\u`表示.

`display` procedure 直接将字符串中的`字符`写入当前的`输出端口`(见`Input and Output`), 这跟打印`字符串常量`的语法不同, 后者定义一个`字符串`.
例子.

```lisp
> "Apple"
"Apple"
> "\u03BB"
"λ"
> (display "Apple")
Apple
> (display "a \"quoted\" thing")
a "quoted" thing
> (display "two\nlines")
two
lines
> (display "\u03BB")
λ
```

`string`可以是`可变`的,也可以是`不可变`的;直接写成表达式的`string`是`不可变`的, 但大多数其他`string`是可变的.

`make-string` 过程, 在给出`长度`和可选的`填充字符`的情况下, 创建一个可变的`string`.
`string-ref` 过程 从`string`中访问字符(使用基于`0`的索引);
`string-set!` 过程改变可变`string`中的字符.

例子.

```lisp
> (string-ref "Apple" 0)
#\A
> (define s (make-string 5 #\.))
> s
"....."
> (string-set! s 2 #\λ)
> s
"..λ.."
```

`字符串`排序和`大小写`操作通常是独立于`locale`的; 也就是说,它们对所有用户都是一样的.
我们提供了一些依赖`locale`的操作, 使字符串的大小写折叠, 和排序方式可以取决于终端用户的地区设置.

例如,如果你要对字符串进行排序, 需要排序结果在不同机器和用户间保持一致,就使用 `string<?` 或 `string-ci<?`,
如果要依据终端用户给出特定的结果,就使用 `string-locale<?` 或 `string-locale-ci<?`
例子.

```lisp
> (string<? "apple" "Banana")
#f
> (string-ci<? "apple" "Banana")
#t
> (string-upcase "Straße")
"STRASSE"
> (parameterize ([current-locale "C"])
(string-locale-upcase "Straße"))
"STRAßE"
```

对于处理纯`ASCII`, 处理`raw bytes`, 或将Unicode字符串`编码`/`解码`为`字节`(bytes), 使用`byte strings`(字节字符串).

### 字节和字节字符串

`byte`是`0`到`255` 之间的精确`整数`,包括两端. `byte?` 谓词(predicate)可以识别代表`字节`的数字.
例子.

```lisp
> (byte? 0)
#t
> (byte? 256)
#f
```

`byte string`类似于`字符串`--见字符串(Unicode)--但其内容是一串`字节`而不是字符.
`byte string`可用于处理纯`ASCII`码而非`Unicode`文本的应用程序中.

`byte string`的打印形式特别适合这种用途, 因为`byte string`的打印形式就像`byte string`的`ASCII`码, 但前缀为`#`.
`byte string`中不可打印的`ASCII`字符或`非ASCII字节`会用`八进制`符号来书写.
例子.

```lisp
> #"Apple"
#"Apple"
> (bytes-ref #"Apple" 0)
65
> (make-bytes 3 65)
#"AAA"
> (define b (make-bytes 2 0))
> b
#"\0\0"
> (bytes-set! b 0 1)
> (bytes-set! b 1 255)
> b
#"\1\377"
```

`bytes串`的`display`形式将其`raw bytes`写到当前的输出端口(见输入和输出).
从技术上讲,普通的(即character)`字符串`的`display`, 就是将该字符串的`UTF-8`编码打印到当前的输出端口, 因为输出最终是以`字节`为单位定义的;
但是, `display`一个`byte 字符串`, 输出的是没有`编码`的原始字节.
按照同样的思路,当本文档显示输出时, 从技术上讲, 它显示的是输出的`UTF-8`解码形式.
例子.

```lisp
> (display #"Apple")
Apple
> (display "\316\273")  ; same as "Î»"
Î»
> (display #"\316\273") ; UTF-8 encoding of λ
λ
```

为了明确地在字符串和字节字符串之间进行转换, `Racket` 直接支持三种编码方式.
`UTF-8`,`Latin-1`,以及`当前地区`的编码. `字节`与`字节`之间的转换(尤其是与`UTF-8`之间的转换)的通用实现, 填补了支持任意字符串编码的空白.
例子.

```lisp
> (bytes->string/utf-8 #"\316\273")
"λ"
> (bytes->string/latin-1 #"\316\273")
"Î»"
> (parameterize ([current-locale "C"])  ; C locale supports ASCII,
    (bytes->string/locale #"\316\273")) ; only, so...
bytes->string/locale: byte string is not a valid encoding for the current locale  byte string: #"\316\273"
> (let ([cvt (bytes-open-converter "cp1253" ; Greek code page
                                   "UTF-8")]
            [dest (make-bytes 2)])
    (bytes-convert cvt #"\353" 0 1 dest)
    (bytes-close-converter cvt)
    (bytes->string/utf-8 dest))
"λ"
```

### 符号 Symbols

`符号`(symbol)是一个`原子值`(atomic),其打印方式类似于, 前面带有`'`的标识符. 
以`'`开始,接着一个标识符的表达式, 产生一个`符号`值.
例子:

```lisp
> 'a
'a
> (symbol? 'a)
#t
```

对于任何字符序列, 内部(interned)都对应唯一的`符号`;
调用`string->symbol` 过程, 或者读取一个语法(syntactic)标识符,都会产生一个`内部符号`(interned symbol).
由于内部符号可以方便地用`eq?` (以及`eqv?`或`equal?`)进行比较,它们可以方便地作为`标签`和`枚举`来使用.

`符号`是区分大小写的. 通过使用`#ci`前缀或其他方式, 可以控制 `reader` 简并字符序列的大小写, 以得到同一个符号,但`reader` 默认保留大小写.
例子.

```lisp
> (eq? 'a 'a)
#t
> (eq? 'a (string->symbol "a"))
#t
> (eq? 'a 'b)
#f
> (eq? 'a 'A)
#f
> #ci'A
'a
```

任何`字符串`(即任何`字符`序列) 都可以提供给`string->symbol`以获得相应的符号.
输入给 `reader` 时, 任何字符都可以直接出现在`标识符`中, 除了`空白`和以下特殊字符.

    ( ) [ ] { } " , ' ` ; # | \

实际上, 只有符号开头不允许出现`#`, 并且后面不允许跟有`%`; 否则, `#` 也是允许的. 另外, `.` 本身不能当作符号.

`空白`或`特殊字符`可以通过用`|`或`\` quoting , 来包含在`标识符`中.
这些 quoting 机制用于打印`特殊字符`, 或者避免与`数字`模样的标识符混淆.
例子.

```lisp
> (string->symbol "one, two")
'|one, two|
> (string->symbol "6")
'|6|
```

`write` 函数 打印出不带`'`前缀的`symbol`. `symbol`的显示形式与相应的字符串相同.
例子.

```lisp
> (write 'Apple)
Apple
> (display 'Apple)
Apple
> (write '|6|)
|6|
> (display '|6|)
6
```

`gensym`和 `string->uninterned-symbol` procedure 生成新的`uninterned`符号,
这些符号不等于(根据`eq?`)任何先前的 `interned` 或 `uninterned` 符号.
`unterned` 符号作为新的`标签`是很有用的, 它不会与任何其他值混淆.
例子.

```lisp
> (define s (gensym))
> s ; s 不是内部符号,
'g42
> (eq? s 'g42) ; 它根内部符号 'g45 也不相等
#f
> (eq? 'a (string->uninterned-symbol "a"))
#f
```

### 关键字,Keywords

`关键字`值与`符号`(symbol)相似(见`符号`), 但它的打印形式是以`#:`为前缀.
例子.

```lisp
> (string->keyword "apple")
'#:apple
> '#:apple
'#:apple
> (eq? '#:apple (string->keyword "apple"))
#t
```

更确切地说, `关键字`类似于`标识符`; 类似于`标识符`被`引用`就会产生`符号`, `关键字`被引用也可以产生`值`.
尽管在引用前后我们都它为 `关键字`, 但是有时我们用`关键字值`来特指`quote-keyword`表达式或`string->keyword`产生的结果.

没`quote`的`关键字`不是`表达式`, 就像没`quote`的`标识符`不产生`符号`值.
例子.

```lisp
> not-a-symbol-expression
not-a-symbol-expression: undefined;
 cannot reference an identifier before its definition
  in module: top-level
> #:not-a-keyword-expression // 没有加引号, 不构成 quote-keyword 表达式
eval:2:0: #%datum: keyword misused as an expression
  at: #:not-a-keyword-expression
```

尽管它们有相似之处,`关键字`的使用方式与`标识符`或`符号`不同.

`关键字`(不加引号)被用来作为`参数列表`和某些句法形式(syntactic forms)中的特殊`标记`.
对于`运行时标志`(flag)和枚举(enumerations), 使用`符号`而不是`关键字`. 
下面的例子说明了`关键字`和`符号`的不同作用.
例子.

```lisp
> (define dir (find-system-path 'temp-dir)) ; 不是 '#:temp-dir
> (with-output-to-file (build-path dir "stuff.txt")
    (lambda () (printf "example\n"))
    ; 可选参数 #:mode 可以是 'text 或者 'binary
    #:mode 'text
    ; 可选参数 #:exists 可以是 'replace, 'truncate, ...
    #:exists 'replace)
```

### 对儿和列表,Pairs and Lists

`对`连接两个任意`值`. `cons`程序构建`对`, `car`和`cdr`程序分别提取`对`中的`第一`和`第二`元素. `pair?`谓词可以识别`对`.

有些`对`的打印方法是: 在两个对元素的打印形式周围包上小括号`()`, 在元素的开头加上`'`, 在元素之间加上`.`:
例子.

```lisp
> (cons 1 2)
'(1 . 2)
> (cons (cons 1 2) 3)
'((1 . 2) . 3)
> (car (cons 1 2))
1
> (cdr (cons 1 2))
2
> (pair? (cons 1 2))
#t
```

`列表`是`对`的组合,  作为后者的`链表`.  更确切地说, 一个列表要么是空列表`null`,
要么是一个`对`, 首元素是列表元素, 次元素是`列表`. 
`list?` 谓词可以识别`列表`. `null?` 谓词识别`空列表`.

列表通常打印为`'`, 后面是一对包裹着列表元素的小括号`()`.
例子.

```lisp
> null
'()
> (cons 0 (cons 1 (cons 2 null)))
'(0 1 2)
> (list? null)
#t
> (list? (cons 1 (cons 2 null)))
#t
> (list? (cons 1 2))
#f
```

当`列表`或`对`中的`元素`不能被写成`quoted value`时, 就用`list`或`cons`来显式表示.
例如,用`srcloc`构造的值不能用引号表示, 就用`srcloc`打印它.

```lisp
> (srcloc "file.rkt" 1 0 1 (+ 4 4))
(srcloc "file.rkt" 1 0 1 8)
> (list 'here (srcloc "file.rkt" 1 0 1 8) 'there)
(list 'here (srcloc "file.rkt" 1 0 1 8) 'there)
> (cons 1 (srcloc "file.rkt" 1 0 1 8))
(cons 1 (srcloc "file.rkt" 1 0 1 8))
> (cons 1 (cons 2 (srcloc "file.rkt" 1 0 1 8)))
(list* 1 2 (srcloc "file.rkt" 1 0 1 8))
```

如最后一个例子所示, `list*`用来缩写一系列不能用`list`表示的`cons`.

`write`和`display`函数 打印没有前导`'`,`cons`,`list`或`list*`的, `对`或`列表`.
对于`对`或`列表`, `write` 和`display`没有区别, 但是它们作用于列表的元素时有区别.
例子.

```lisp
> (write (cons 1 2))
(1 . 2)
> (display (cons 1 2))
(1 . 2)
> (write null)
()
> (display null)
()
> (write (list 1 2 "3"))
(1 2 "3")
> (display (list 1 2 "3"))
(1 2 3)
```

在`列表`的预定义程序中, 最重要的是那些在列表的元素中进行`迭代`的程序.

```lisp
> (map (lambda (i) (/ 1 i))
       '(1 2 3))
'(1 1/2 1/3)
> (andmap (lambda (i) (i . < . 3))
         '(1 2 3))
#f
> (ormap (lambda (i) (i . < . 3))
         '(1 2 3))
#t
> (filter (lambda (i) (i . < . 3))
          '(1 2 3))
'(1 2)
> (foldl (lambda (v i) (+ v i))
         10
         '(1 2 3))
16
> (for-each (lambda (i) (display i))
            '(1 2 3))
123
> (member "Keys"
          '("Florida" "Keys" "U.S.A."))
'("Keys" "U.S.A.")
> (assoc 'where
         '((when "3:30") (where "Florida") (who "Mickey")))
'(where "Florida")
```

`对`是不可变的(与`Lisp`的传统相反), 而 `pair?` 和 `list?` 只识别不可变的`对`和`列表`.
`mcons` 过程创建可变的`对`,它与 `set-mcar!` 和 `set-mcdr!` 以及 `mcar` 和 `mcdr` 一起工作.
可变对使用 `mcons` 打印,而 `write` 和 `display` 使用 `{` 和 `}` 打印`可变对`.
例子.

```lisp
> (define p (mcons 1 2))
> p
(mcons 1 2)
> (pair? p)
#f
> (mpair? p)
#t
> (set-mcar! p 0)
> p
(mcons 0 2)
> (write p)
{0 . 2}
```

### 矢量,Vectors

`矢量`是`固定长度`的任意值的数组. 与列表不同,矢量支持对其元素的 constant-time `访问`和`更新`.
`矢量`的打印方式类似于`列表`--括号包围的元素序列--但矢量的前缀为`'#`. 如果其中某个元素不能用`quote`表示, 则使用 `vector`.
对于作为`表达式`的`矢量`,可以提供可选的`长度`.
另外,作为表达式的矢量隐含着对其内容的`quote`, 这意味着矢量常量中的标识符和括号形式, 分别代表`symbols`和`lists`.
例子.

```lisp
> #("a" "b" "c")
'#("a" "b" "c")
> #(name (that tune))
'#(name (that tune))
> #4(baldwin bruce)
'#(baldwin bruce bruce bruce)
> (vector-ref #("a" "b" "c") 1)
"b"
> (vector-ref #(name (that tune)) 1)
'(that tune)
```

像`字符串`一样,矢量是`可变`或`不可变`的,直接写成表达式的矢量是`不可变`的.

`矢量`可以通过`vector->list`和`list->vector`与`列表`互相转换; 当与列表的预定义程序结合使用时候, 这种转换特别有用.
当分配额外的列表似乎太昂贵时,可以考虑使用`for/fold`这样的循环形式,它既可以识别`矢量`,也可以识别`列表`.

例子.

```lisp
> (list->vector (map string-titlecase
                     (vector->list #("three" "blind" "mice"))))
'#("Three" "Blind" "Mice")
```

### 哈希表,Hash Tables

`哈希表`实现了从键到值的映射,其中键和值都可以是任意的`Racket`值,对表的`访问`和`更新`通常是 constant-time.
根据哈希表建立函数 <-> 键值比较 

+ `make-hash`, `equal?`
+ `make-hasheqv`, `eqv?`
+ `make-hasheq`, `eq?`

例子.

```lisp
> (define ht (make-hash))
> (hash-set! ht "apple" '(red round))
> (hash-set! ht "banana" '(yellow long))
> (hash-ref ht "apple")
'(red round)
> (hash-ref ht "coconut")
hash-ref: no value found for key
  key: "coconut"
> (hash-ref ht "coconut" "not there")
"not there"
```

`hash`, `hashqv` 和 `hashq` 函数从初始的`键`和`值`集合中创建`不可变`的`哈希表`, 其中每个`值`在`键`之后作为参数提供.
不可变哈希表可以用`hash-set`来扩展, 它可以在`constant time`内产生新的不可变的哈希表.
例子.

```lisp
> (define ht (hash "apple" 'red "banana" 'yellow))
> (hash-ref ht "apple")
'red
> (define ht2 (hash-set ht "coconut" 'brown))
> (hash-ref ht "coconut")
hash-ref: no value found for key
  key: "coconut"
> (hash-ref ht2 "coconut")
'brown
```

不可变哈希表可以通过使用`#hash`(对基于`equal?`的表), `#hasheqv`(对基于`eqv?`的表)
或者`#hasheq`(对基于`eq?`的表)写成`表达式`.
在`#hash`, `#hasheq` 或 `#hasheqv` 之后必须紧跟包在括号`()`内的序列, 其中每个元素是带点的`键值对`.
`#hash`等形式隐含地`quote`了它们的键和值.
例子.

```lisp
> #hash(("apple" . red)
        ("banana" . yellow))
'#hash(("apple" . red) ("banana" . yellow))
> (hash 1 (srcloc "file.rkt" 1 0 1 (+ 4 4)))
(hash 1 (srcloc "file.rkt" 1 0 1 8))
```

`可变`和`不可变`的哈希表都按照不可变哈希表的样式打印,
如果所有的`键`和`值`都可以用`quote`表达, 则使用带引号的`#hash`,`#hasheqv` 或`#hasheq` 形式,
否则使用 `hash`,`hasheq` 或 `hasheqv`.
例子.

```lisp
> #hash(("apple" . red)
        ("banana" . yellow))
'#hash(("apple" . red) ("banana" . yellow))
> (hash 1 (srcloc "file.rkt" 1 0 1 (+ 4 4)))
(hash 1 (srcloc "file.rkt" 1 0 1 8))
```

可变的哈希表可以选择`弱保留`它的键(reatin its keys weakly),  只有当`key`在别的地方保留时, 这个`映射`才被保留.
例子.

```lisp
> (define ht (make-weak-hasheq))
> (hash-set! ht (gensym) "can you see me?")
> (collect-garbage)
> (hash-count ht)
0
```

请注意,即使是`弱`的哈希表,只要相应的`键`可以访问,它的值也会`强保留`(tetains its values strongly).
这就产生了`catch-22`的依赖性, 当一个`值`套娃引用它的`键`时, 这样的`映射`就会被永久地保留.
为了打破这个循环, 将`键`映射到一个`ephemeron`, 后者将`值`与它的`键`配对(除了哈希表本身的隐式配对之外).
例子.

```lisp
> (define ht (make-weak-hasheq))
> (let ([g (gensym)])
    (hash-set! ht g (list g)))
> (collect-garbage)
> (hash-count ht)
1
> (define ht (make-weak-hasheq))
> (let ([g (gensym)])
    (hash-set! ht g (make-ephemeron g (list g)))) ; 将 g 和它的值打包成 ephemero, 再作为 g 的映射
> (collect-garbage)
> (hash-count ht)
0
```

### 箱子, Boxes

`箱子`就像只有一个元素的`向量`. 它的打印形式为 quoted `#&`, 后面是被框的值的打印形式.
`#&` 形式也可以作为表达式使用, 但由于产生的`框`是`常数`,它实际上没有什么用.
例子.

```lisp
> (define b (box "apple"))
> b
'#&"apple"
> (unbox b)
"apple"
> (set-box! b '(banana boat))
> b
'#&(banana boat)
```

### 空和未定义,Void and Undefined

一些`过程`或`表达式`不需要结果值. 例如,调用显示过程只是为了`输出屏幕`的副作用.
在这种情况下, 结果值通常是一个特殊的`常数`, 打印为`#<void>`. 
当表达式的结果只是`#<void>`时, `REPL` 不会打印任何东西.

`void`过程接受任意数量的参数,并返回`#<void>`.
(也就是说, 标识符 `void` 被绑定到一个返回`#<void>`的过程, 而不是直接绑定到`#<void>`).
例子.

```lisp
> (void)
> (void 1 2 3)
> (list (void))
'(#<void>)
```

`undefined`常数, 打印为`#<undefined>`, 有时会被用作`引用`的结果, 当其值还不可用.
在以前的`Racket`版本中(6.1版之前), 过早地引用局部绑定会产生`#<undefined>`; 现在过早的引用会引发`异常`.

>在某些情况下,`undefined`结果仍然可以由`shared`形式产生.

```lisp
(define (fails)
  (define x x)
  x)
> (fails)
x: undefined;
 cannot use before initialization
```
