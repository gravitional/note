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
