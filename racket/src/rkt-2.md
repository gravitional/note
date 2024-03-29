# racket Essentials

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

`racket`可以模仿传统的`Lisp`环境, 但我们强烈建议不要使用`Load`或者在模块外编写程序.
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

## 对儿,列表和Racket语法

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
