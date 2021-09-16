# racket

[Racket, the Programming Language](https://racket-lang.org/)
[走进 racket(lisp) 的世界](https://zhuanlan.zhihu.com/p/19906432)

安装; 见官方页面`download`. `Debian`系也可以使用`sudo apt install racket`直接安装.

+ `DSL` ; `domain-specific languages`

与`C#`或`UML`这样的通用语言不同, 领域专用语言（`DSL`）被设计用来表达特定问题空间或`domain`的语句.

知名的`DS`L包括`正则表达式`和`SQL`. 
每种`DSL`在描述对文本字符串或数据库的操作时都比通用语言好得多, 但在描述其自身范围之外的想法时却差得多. 
个别行业也有自己的`DSL`. 例如, 在电信行业, `呼叫描述语言`被广泛用于指定电话呼叫中的状态序列, 
而在航空旅游行业, 标准的`DSL`被用于描述航班预订. 

你的业务和你的项目也会处理一些特殊的概念集, 可以用`DSL`来描述. 例如, 你可以为这些应用定义一个`DSL`. 

+ 网站的导航路径. 
+ 电子元件的接线图. 
+ 一个机场的传送带和行李处理设备的网络. 

当你设计`DSL`时, 你为领域中的每个重要概念定义一个`domain class`, 如网页、灯、或机场登记台. 
然后定义`domain relationships`, 如超链接、电线或传送带, 将这些概念联系在一起. 

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

```racket
#lang slideshow
```

然后点击 `运行` 按钮. 你会看到文本光标移到底部文本区, 也就是`交互区`.  
如果你以前使用过`DrRacket`, 你可能需要点击左下角的`Language|Choose Language...`, 来侦测源代码中声明的语言.

现在可以在`>`后面输入表达式进行计算. 例如

```racket
5
"art gallery"
(circle 10)
```

函数调用采用 `(函数 参数)`的形式. 也就是`S-表达式`. symbol-expression. 再举个例子, 复合两个图形:

```racket
(hc-append (circle 10) (rectangle 10 20))
```

### define,let

用下面的语法进行`定义`:

```racket
#lang slideshow
(define c (circle 10))
(define r (rectangle 10 20))
```

然后可以直接使用`r c`在交互区调用定义. 函数定义类似, 括号中是函数名称和参数.

```racket
(define (square n)
  ; A semi-colon starts a line comment.
  ; The expression below is the function body.
  (filled-rectangle n n))
```

`define`也可以创建局部变量, 即局部绑定.

```racket
(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))
(four (circle 10))
```

更一般地,  `Racketeers`使用`let`或`let*`形式进行局部绑定, 
`let`的优点是它可以在任何表达式的位置使用, 而且, 它可以一次绑定许多标识符, 而不要求为每个标识符单独定义, 

```racket
(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))
```

`let`同时绑定多个标识符, 所以这些绑定不能相互引用, 与此相反, `let*`允许后来的绑定引用之前的绑定.

```racket
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

```racket
> circle
#<procedure:circle>
```

也就是说, 标识符`circle`被绑定到一个函数(又称 "过程", procedure)上, 就像名称`c`被绑定到一个圆上, 
与圆形图片不同的是, 没有简单的方法可以完全打印出函数, 所以`DrRacket`只是打印出`#<procedure:circle>`.

这个例子表明, `函数`是`values`, 就像`数字`和`图片`一样(即使它们的打印方式不那么好), 既然函数是值, 你可以定义一个函数, 它接受函数作为参数, 

```racket
(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))
  (series circle)
  (series square)
```

当调用一个接受函数参数的函数时, 往往在其他地方不需要这个作为参数的函数, 
如果要通过`define`来写下这个函数, 那就很麻烦了. 另一种方法是使用`lambda`, 它可以创建一个匿名函数, 

```racket
> (series (lambda (size) (checkerboard (square size))))
```

`lambda` 后面的括号内的名称是函数的参数, 参数名称后面的表达式是函数体, 使用 `lambda` 一词而不是 `function` 或 `procedure` 是 `Racket` 历史和文化的一部分, 
函数的`define`定义形式, 实际上是简单使用`lambda`作为值的快捷方式. 例如, `series`的定义可以写成

```racket
(define series
    (lambda (mk)
        (hc-append 4 (mk 5) (mk 10) (mk 20))))
```

大多数`Racketeers`喜欢使用`define`的速记函数形式, 而不是扩展为`lambda`, 

### Lexical Scope

`Racket`是一种词法范围的语言, 这意味着只要一个标识符被用作表达式, 表达式所处文本环境中决定了标识符的绑定, 
这个规则适用于`lambda`函数体中的标识符, 也适用于其他地方, 

在下面的`rgb-series`函数中, `mk`在每个`lambda`形式中指向的都是`rgb-series`的参数, 因为那是文本范围内的绑定, 

```racket
(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))
>(rgb-series circle)
>(rgb-series square)
```

下面是另一个例子, `rgb-maker` 接受一个函数并返回一个新的函数, 这个函数会记住并使用原来的函数, 

```racket
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

```racket
> (list "red" "green" "blue")
> (list (circle 10) (square 10))
```

正如你所看到的, 列表打印的时候前面会加上单引号. 由于小括号既用于表达式, 如`(circle 10)`, 也用于打印结果, 如`("红""绿""蓝")`, 
可能造成混淆, 所以加上引号便于区别. 为了帮助强调这一区别, 在文档和`DrRacket`中, 结果中的括号是用蓝色打印的, 与表达式括号不同, 

如果你有一个列表, 你可能想对每个元素做一些事情, `map`函数接收一个列表和一个函数, 然后它把函数应用于列表中每个元素；它返回一个新的列表, 以组合函数的结果, 

```racket
(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

(rainbow (square 5))
```

另一个适用于列表的函数是`apply`, 
和`map`一样, 它接受一个函数和一个列表, 但是给`apply`的函数应该一次接受所有的参数, 而不是单独使用每个参数.
`apply`函数对于接受任意数量参数的函数特别有用, 例如`vc-append`, 

```racket
(apply vc-append (rainbow (square 5)))
```

注意, `(vc-append (rainbow (square 5)))` 是不行的, 因为`vc-append`不想要列表作为参数；
它想要一个图片作为参数, 而且它愿意接受任何数量的图片, `appl`y函数在需要许多参数的函数, 和作为单个值的参数的列表之间架起了一座桥梁.

### 模块

由于你的程序在`定义窗口`中的开头是

```racket
#lang slideshow
```

你在`定义窗口`中的所有代码都在一个`模块`中. 
此外, 该`模块`初始化时从`slideshow`指定的模块中导入所有内容, 该模块包括了`图片制作函数`以及更常用的函数, 如`list`和`map`.

要导入其他库, 请使用`require`形式, 例如, 库`pict/flash`提供了一个`filled-flash`的功能.

```racket
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

```racket
(provide rainbow square)
```

然后你可以在`DrRacket`中打开一个新的标签或窗口, 在与 `quick.rkt` 相同的目录中输入新程序 `use.rkt`, 

```racket
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

```racket
(require slideshow/code)
> (code (circle 10))
```

结果不是一个圆圈, 而是代码的图片, 如果把它作为一个表达式, 就会产生一个圆圈.
换句话说, `code`不是一个函数, 而是一个用于创建图片的新句法形式, `syntactic form` ; 
`(circle 10)`的位置上不是一个表达式, 而是由`code`的句法形式操纵的, 

这有助于解释我们在上一节中说`racket`提供`require`和函数调用语法的意思, 库并不局限于输出值, 如`函数`；它们也可以定义新的语法形式, 
从这个意义上说, `Racket`根本就不是一种语言；它更像是一个关于如何构造语言的想法, 这样你就可以扩展它或者创造全新的语言, 

引入新的语法形式的一种方法是通过`define-syntax `与`syntax-rules`, 

```racket
(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))
>(pict+code (circle 10))
```

这种定义是一个`macro`--宏, `(pict+code expr)`部分是宏要使用的`pattern`；
程序中该`模式`的`实例`, 被对应模板中的实例所取代, 也就是会换成`(hc-append 10 expr (code expr))`, 
在这里, `(pict+code (circle 10))`会匹配模式, `(circle 10)`匹配`expr`, 所以它被替换为`(hc-append 10 (circle 10) (code (circle 10)))`, 

当然, 这种句法扩展是双向的：发明一种新的语言可以使你更容易说出你想要的东西, 但对其他人来说却更难理解, 
碰巧的是, `Racket`的开发者们在不断地发表包括`Racket`代码的讲座和论文, 对于从事这些产品的人来说, 了解`code`是值得的.

事实上, 你可能想看看[这个文件的来源](https://docs.racket-lang.org/quick/quick.scrbl), 
你会看到它是以`#lang`开头的, 但除此之外看起来并不像`Racket`；尽管如此, 我们还是通过将其源码作为`Racket`程序运行来构建这份文档, 
我们必须使用比`syntax-rules` 更多的东西来扩展`Racket`的语法, 以便编写文档, 但`Racket`的语法扩展可以让你走得更远, 

### 面向对象

`对象系统`是另一个值得`Racket`用户学习和使用的复杂语言扩展的例子, 
对象有时比`函数`更好, 即使你有`lambda`, 而且对象对图形用户界面的效果特别好, 
`Racket`的图形用户界面和图形系统的`API`是用`对象`和`类`来表达的, 

`类`系统本身是由`racket/class`库实现的, 而`racket/gui/base`库提供了 `GUI` 和`绘图类`, 按照惯例, 这些类的名字以`%`结尾, 

```racket
(require racket/class
         racket/gui/base)
(define f (new frame% [label "My Art"]
                      [width 300]
                      [height 300]
                      [alignment '(center center)]))
> (send f show #t)
```

`new`形式创建了`类`的`实例`, 其中的初始化参数如`label`和`width`是通过名称提供的, 
`send`形式调用对象的方法, 如`show`, 参数在方法名后面；本例中的参数`#t`是布尔常数 `true`.

用`slideshow`生成的图片`封装`了一个函数, 该函数使用图形工具箱的绘图命令将图片渲染到一个绘图环境中, 例如一个框架中的画布, `anvas` in a `frame`.
`slideshow`中的`make-pict-drawer`函数暴露了图片的绘制函数, 我们可以在`canvas-painting`回调(`callback`)中使用`make-pict-drawer`来将图片绘制到`canvas`中, 

```racket
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

对`Racket`的介绍有意避开了许多介绍和区分于`Lisp, Scheme`的传统方式：
`前缀算术符号`, `symbols`, `quoting`和`quasiquoting lists`, `eval`, `first-class continuations`, 
以及所有语法实际上只是一个伪装的`lambda`的想法, 
虽然这些都是`Racket`的一部分, 但它们并不是`Racket`日常编程的主要成分, 

相反, `Racket`程序员通常使用`functions`, `records`, 对象, 异常, 正则表达式, `模块`和`线程`进行编程, 
也就是说, Racket不是一种 `极简主义` 的语言--这也是`Scheme`经常给人的印象, 而是一种拥有大量`库`和`工具`的丰富语言, 

如果你是编程新手, 或者你有耐心看完一本教科书, 我们推荐你阅读[How to Design Programs](http://htdp.org/), 
如果你已经读过了, 或者你想看看这本书会带你到哪里去, 那么请看[Continue: Web Applications in Racket](https://docs.racket-lang.org/continue/index.html).

对于有经验的程序员来说, 要想从`系统`的角度, 而不是图片的角度来继续游览`Racket`, 你的下一站是[More: Systems Programming with Racket](https://docs.racket-lang.org/more/index.html)

如果想开始深入了解完整的`Racket`语言和工具, 请继续阅读[The Racket Guide](https://docs.racket-lang.org/guide/index.html).

## racket guide

要将程序打包成可执行文件, 你有几个选择. 

+ 在`DrRacket`中, 你可以选择`Racket|Create Executable...`菜单项. 
+ 在命令行中, 运行`raco exe <src-filename>`, 其中`<src-filename>` 是程序的名字. 参见`raco exe`. 更多参考[创建独立的可执行文件](https://docs.racket-lang.org/raco/exe.html).
+ 在 `Unix` 或 `Mac OS` 中, 你可以通过在文件的最开头插入以下一行, 将程序文件变成一个可执行脚本.

关于脚本文件的更多信息, 请参见[脚本](https://docs.racket-lang.org/guide/scripts.html). 

 ```racket
 #! /usr/bin/env racket
 ```

同时, 在命令行中使用 `chmod +x filename`, 将文件的权限改为可执行. 
只要`racket`在用户的可执行文件搜索路径中, 该脚本就能工作. 另外, 如果在`#!`之后使用`racket`的完整路径(和路径之间有一个空格), 用户的可执行文件搜索路径并不重要.

`racket`可以模仿传统的Lisp环境, 但我们强烈建议不要使用`Load`或者在模块外编写程序.
在模块外编写定义会导致糟糕的错误信息, 糟糕的性能, 以及尴尬的脚本来组合和运行程序. 
这些问题并不是`racket`所特有的；它们是传统`top-level`环境的基本限制, 
在历史上, `Scheme`和`Lisp`的实现都是用特别的命令行标志, 编译器指令和构建工具来对抗这些限制. 

`模块`系统的设计是为了避免这些问题. 所以脚本请从`#lang`开始, 从长远来看, 你会对`Racket`更加满意. 

### Simple Values

`Racket`值包括`数字`, `布尔值`, `字符串`和`字节字符串`. 在`DrRacket`和文档示例中(当你阅读彩色文档时), 值表达式显示为`绿色`. 

数字以常规方式书写, 包括分数和虚数. 

```racket
1       3.14
1/2     6.02e+23
1+2i    9999999999999999999999
```

`Boole`值即`#t`--真, 以及 `#f`--假. 然而在条件判断中, 所有非`#f` 值都被当成真的.

### 简单定义和表达式

一个程序模块为

```racket
#lang ‹langname› ‹topform›*
```

其中 `topform` 是一个 `定义` 或 `表达式`. `REPL` 也会对 `topform` 进行计算. 

在语法规范中, 灰色背景的文本, 如`#lang`, 代表字面文本. 
在这种字面意义和非字面意义文本, 如`<id>`之间必须有空格, 但在`(`, `)`,`[`, 或`]`之前或之后不需要空格. 
注释以`;`开始, 一直到行末, 被当成空白处理.

按照惯例, 语法中的`*`表示对前一个元素的零次或多次重复, `+`表示对前一个元素的一次或多次重复, `{}`将序列作为单个元素, 以备重复.

一个定义的形式是

```racket
( define <id>  <expr> )
```

将 `id` 与 `expr` 的结果绑定, 而

```racket
( define ( <id>  <id>*  ) <expr> )
```

将第一个`<id>`绑定到一个函数(也叫过程)上, 该函数接受由其余`<id>*`命名的参数. 
在函数的情况下, `<expr>` 是函数的主体. 当函数被调用时, 它返回最后一个`<expr>`的结果. 

在引擎盖下, 一个函数定义实际上和一个非函数定义是一样的, 而且一个函数名称不一定要在函数调用中使用. 
一个函数只是另一种值, 尽管其打印形式必然不如数字或字符串的打印形式完整. 

一个函数定义可以为函数主体包括多个表达式. 
在这种情况下, 当函数被调用时, 只有最后一个表达式的值被返回. 
其他的表达式只是为了某些副作用而被评估, 例如`打印`. 

```racket
(define (bake flavor)
  (printf "preheating oven...\n")
  (string-append flavor " pie"))

> (bake "apple")
```

`Racket`程序员喜欢避免产生副作用, 所以一个定义通常只有一个表达式在其主体中. 
不过, 理解定义主体中允许有多个表达式是很重要的, 因为它解释了为什么下面的`nobake`函数没有在其结果中包含参数. 

```racket
(define (nobake flavor)
  string-append flavor "jello")
 
> (nobake "green")
```

函数体中的`string-append flavor "jello"`被当成独立的元素, 最后只返回`"jello"`.

### 关于缩进

换行和缩进对于解析`Racket`程序来说并不重要, 但大多数`Racket`程序员都使用一套标准的惯例来使代码更易读. 
例如, 定义的主体通常在定义的第一行下缩进. 标识符紧跟在开放小括号之后, 不占多余的空间, 闭合小括号不另起一行. 

当你在程序或`REPL`表达式中键入`Enter`时, `DrRacket`会自动按照标准样式缩进. 
例如, 如果你在输入`(define (greet name)`后按下回车键, 那么`DrRacket`会自动为下一行插入两个空格. 
如果你改变了一个`代码区域`, 你可以在`DrRacket`选中它并点击`Tab`, `DrRacket`将重新缩进代码(不插入任何换行符). 

`Emacs`等编辑器在Racket或Scheme模式中, 也支持类似的缩进. 

重新缩进不仅可以使代码更容易阅读, 还可以给你额外的反馈, 使你的括号与你的意图相匹配. 
例如, 如果你在一个函数的最后一个参数后漏掉了一个闭合小括号, 自动缩进就会在第一个参数下开始下一行, 而不是在`define`关键字下. 

```racket
    (define (halfbake flavor
                      (string-append flavor " creme brulee"))
```

在这种情况下, 缩进有助于突出这个错误. 
在其他情况下, 缩进可能是正常的, 而开放的小括号没有匹配的封闭小括号, `racket`和`DrRacket`都使用源头的缩进来提示可能缺少小括号的地方. 

### 标识符

`Racket` 的标识符语法是非常自由的. 除了特殊字符

```racket
( ) [ ] { } " , ' ` ; # | \
```

除了构成数字常数的字符序列外, 几乎任何非空白字符的序列都可以作为`id`. 
例如, `substring` 就是一个标识符. 另外, `string-append`和`a+b`是标识符, 而不是算术表达式. 这里还有几个例子. 

```racket
+
integer?
pass/fail
Hfuhruhurr&Uumellmahaye
john-jacob-jingleheimer-schmidt
a-b-c+1-2-3
```