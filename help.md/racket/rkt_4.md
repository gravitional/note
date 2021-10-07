# 表达式和定义

## 记号

`本章`(以及其余部分)使用的`notation`与`Racket Essentials`章节中基于`字符`的语法略有不同.

语法形式 `something` 的用法是这样显示的.

(something [*id ...+*] *an-expr* ...)

本规范中的斜体`元变量`(meta-variables), 如`id`和`an-expr`, 使用Racket `标识符`的语法,
所以 `an-expr` 是个`元变量`. 命名规则隐含地定义了许多`元变量`的含义.

+ 以`id`结尾的`元变量`代表`标识符`. 实例为:  `x` 或 `my-favorite-martian`.
+ 以`keyword`结尾的`元标识符`代表`关键词`(keyword). 实例为: `#:tag`.
+ 以 `expr` 结尾的元标识符代表任何`sub-form`. 它将被作为`表达式` 解析.
+ 以`body`结尾的元标识符代表任何`子形式`; 它将被作为`局部定义`或者`表达式` 解析.
The last `body`必须是`表达式`; 另见[Internal Definitions](https://docs.racket-lang.org/guide/define.html#%28part._intdefs%29).

语法中的`方括号`表示括起来的`形式序列`, 通常使用`方括号`(按照惯例).
也就是说,方括号并不意味着 `语法形式` 的可选部分(optional).
`...`  表示前接形式的`零次`或`多次`重复, 而 `...+` 表示`前接` datum 的`一次`或`多次`重复.

除此之外, 非斜体字母的`标识符`就代表它本身.

基于上述语法, 那么下面是 `something` 的几个符合要求的用法.

```lisp
(something [x])
(something [x] (+ 1 2))
(something [x my-favorite-martian x] (+ 1 2) #f)
```

一些`syntactic 形式`的规范使用了没有隐含定义的`元变量`, 之前没给出定义.
这样的元变量的含义会在 `main form`之后给出, 使用类似 `BNF` 的格式来替代.

    (something-else [thing ...+] an-expr ...)

    thing   =   thing-id
                    |   thing-keyword

上面的例子说, 在 `something-else` 形式中, `thing` 是`标识符`或`关键词`.

### BNF 范式

[BNF范式](https://www.zhihu.com/question/27051306)
[Ryan FZY](https://www.zhihu.com/question/27051306/answer/35904732)
[不是Zoe](https://www.zhihu.com/question/27051306/answer/579820547)
[Evie.D](https://www.zhihu.com/question/27051306/answer/138631357)

`BNF`是描述编程语言的文法.自然语言存在不同程度的二义性. 这种模糊, 不确定的方式无法精确定义一门程序设计语言.
必须设计一种准确无误地描述程序设计语言的语法结构. 这种严谨,简洁,易读的形式规则描述的语言结构模型称为`文法`.

最著名的`文法`描述形式是由`Backus`定义`Algol60`语言时提出的`Backus-Naur`范式(Backus-Naur Form, BNF)及其扩展形式EBNF.
`BNF`能以一种简洁,灵活的方式描述语言的语法.具体内容可参考针对编译原理的书.

+ `BNF`范式是一种用递归的思想来表述计算机语言符号集的定义规范.
+ 法则:
  + `::=`表示定义;
  + `" "`双引号里的内容表示`字符`;
  + `<>`尖括号里的内容表示`必选`内容;
  + `|` 竖线两边的是`可选`内容,相当于`or`;

基本结构为: `<non-terminal> ::= <replacement>`

`non-terminal`意为非终止符, 就是说我们还没有定义完的东西,还可以继续由右边的`replacement`,也就是`代替物`(alternatives)来进一步解释, 定义.

举个例子: 在中文语法里, 句子一般由`主语`, `谓语`和`宾语`组成,
`主语`可以是`名词`或者`代词`, `谓语`一般是`动词`, `宾语`可以是`形容词`, `名词`或者`代词`.
那么`主语`, `谓语`和`宾语`就是`非终止符`, 因为还可以继续由`名词`, `代词`, `动词`, `形容词`等替代.

    例1. <句子> ::= <主语><谓语><宾语>
    例2. <主语> ::= <名词>|<代词>
    例3. <谓语>::=<动词>
    例4. <宾语>::=<形容词>|<名词>|<代词>
    例5. <代词>::=<我>例6. <动词>::=<吃>
    例7. <动词>::=<喜欢>
    例8. <名词>::=<车>
    例9. <名词>::=<肉>

如上,在 `::=` 左边的就是`non-terminal`非终止符, 右边的就是`replacement`,可以是一系列的非终止符,
如`例1`中的`replacement`便是后面`例234`左边的非终止符,也可以是终止符, 如`例56789`的右边,找不到别的符号来进一步代替.
因此, `终止符` 永远不会出现在左边. 一旦我们看到了`终止符`, 这个`描述过程`就结束了.

## 标识符和绑定

`表达式`的`上下文`决定了, 表达式中出现的`标识符`的含义. 特别是,用语言`racket`启动一个模块, 如

```lisp
#lang racket
```

意味着在这个模块中, `标识符`以本指南中描述的含义开始: `cons`指的是创建`对儿`的函数,
`car`指的是提取`pari`中第一个元素的`函数`,以此类推.

>`Symbols`介绍了`标识符`的语法.

像`define`, `lambda` 和 `let`这样的形式将`meaning`与一个或多个`标识符`联系起来;
也就是说,它们`bind`了`标识符`. 程序中`绑定`所适用的部分是绑定的`范围`.
对于给定的`表达式`, 生效的`绑定集合`是表达式的`环境`(environment).
例如,在

```lisp
#lang racket
(define f
  (lambda (x)
    (let ([y 5])
      (+ x y))))
(f 10)
```

`define` 是`f`的一个绑定, `lambda`有对`x`的绑定, `let`有对`y`的绑定.
`f` 的绑定范围是整个模块; `x`的绑定范围是`(let ([y 5]) (+ x y))`; 而`y`的绑定范围只是`(+ x y)`.
`(+ x y)`的环境包括`y`,`x`和`f`的绑定, 以及`racket`中的一切.

`模块级`的定义只能绑定那些, 还没有`定义`, 或还没有`require`到模块的`标识符`.
然而, `local define`或其他`绑定`形式, 可以为已经有`绑定`的标识符提供一个新的`local 绑定`;
这样的绑定会`shadow`现有的绑定.

例子.

```lisp
(define f
  (lambda (append)
    (define cons (append "ugly" "confusing"))
    (let ([append 'this-was])
      (list append cons))))
> (f list)
'(this-was ("ugly" "confusing"))
```

同样, `模块级`的定义可以`shadow`掉模块所属语言中定义的绑定.
例如, 若`racket`模块中存在`(define cons 1)`,则 `shadow` 掉 `racket` 提供的 `cons`.
有意地`shadow`语言的`绑定` 很难是个好主意--特别是对于像 `cons` 这样广泛使用的绑定,
但是`shadow`可以让程序员解脱, 不必小心避开语言所提供的每个晦涩的`绑定`.

甚至像`define`和`lambda`这样的标识符也从绑`定`中获得它们`meaning`,
尽管它们有`transformer`绑定(这意味着它们表示`syntactic 形式`), 而不是`valuee`绑定.
由于`define`有`transformer`绑定, 标识符`define`本身不能被用来获取一个`值`.
然而,`define`的正常绑定可以被`shadow`.
例子.

```lisp
> define
eval:1:0: define: bad syntax
  in: define
> (let ([define 5]) define)
5
```

同样,以这种方式`shadow`标准绑定很难是个好主意, 但这种可能性是`Racket`灵活性的固有部分(inherent part).

### 函数调用, Function Calls(Procedure Applications)

一个形式为

    (proc-expr arg-expr ...)

的表达式是一个`函数调用`--也被称为`过程应用`.
`proc-expr`不能是一个被绑定为`语法 transformer`的`标识符`(例如不能是`if`或`define`).

### 计算顺序和元数,Order and Arity

`函数调用`的计算方法是, 首先计算`proc-expr`, 然后按顺序(从左到右)计算所有`arg-exprs`.
然后, 如果`proc-expr`产生的函数接受的`参数`和提供的`arg-exprs`一样多, 则调用该函数. 否则,就抛出(raise)`异常`.
例子.

```lisp
> (cons 1 null)
'(1)
> (+ 1 2 3)
6
> (cons 1 2 3)
cons: arity mismatch;
 the expected number of arguments does not match the given
number
  expected: 2
  given: 3
> (1 2 3)
application: not a procedure;
 expected a procedure that can be applied to arguments
  given: 1
```

一些函数,如`cons`,接受固定数量的参数. 一些函数,如`+`或`list`, 接受任何数量的参数.
有些函数接受一定范围的`参数`数目, 例如`substring`接受两个或三个参数.
函数的`arity`就是它所接受的`参数`数量.

### 关键字参数,Keyword Arguments

一些函数除了接受`by-position`参数外,还接受`关键字参数`.
在这种情况下, `arg`可以是`arg-keyword arg-expr`序列, 而不仅仅是一个`arg-expr`.

    (proc-expr arg ...)
    arg =   arg-expr
            |   arg-keyword arg-expr

比如说

```lisp
(go "super.rkt" #:mode 'fast)
```

调用与`go`绑定的函数, `"super.rkt"`作为位置参数, `'fast`(symbol 值)作为与 `#:mode` 关键字相关的参数.
`关键字`与它后面的`表达式`是隐含配对(paired)的.

由于`关键字`本身不是表达式, 所以

```lisp
(go "super.rkt" #:mode #:fast)
```

是`syntax 错误`的. `#:mode` 关键字后面必须跟着表达式, 表达式才能产生`参数值`, 而`#:fast`不是`表达式`.

虽然`关键字参数`出现的顺序决定了`arg-exprs` 被计算的顺序,
但是计算之后, `函数`接收到的`关键字参数`, 与它们在`参数列表`中出现的位置无关.
上面对`go`的调用可以等价地写成

```lisp
(go #:mode 'fast "super.rkt")
```

### Apply 函数

类似`MMA`中的`Apply`.

`函数调用`的语法支持`任何数量`的参数,但一个具体的调用总是指定固定数量的参数.
因此, 接收`列表`作为参数的函数, 不能直接对`列表`中的所有项目, 作用像`+`这样的函数.
因为`+` 作用在数字序列上, 现在参数被`list`包裹着.

```lisp
(define (avg lst) ; 不能工作
  (/ (+ lst) (length lst)))

> (avg '(1 2 3))
+: contract violation
  expected: number?
  given: '(1 2 3)
(define (avg lst) ; 不能处理所有情况
  (/ (+ (list-ref lst 0) (list-ref lst 1) (list-ref lst 2))
     (length lst)))

> (avg '(1 2 3))
2
> (avg '(1 2))
list-ref: index too large for list
    index: 2
    in: '(1 2)
```

`apply` 函数提供了绕过这一限制的方法.
它接受一个`函数`和一个`list`参数, 并将该`函数`应用于`列表`中的`值`.

```lisp
(define (avg lst)
  (/ (apply + lst) (length lst)))
> (avg '(1 2 3))
2
> (avg '(1 2))
3/2
> (avg '(1 2 3 4))
5/2
```

为了方便起见, `apply` 函数 可以在`函数`和`列表`之间接收额外的参数.
额外的参数被有效地`cons`到`参数列表`中.

```lisp
(define (anti-sum lst)
  (apply - 0 lst))
> (anti-sum '(1 2 3))
-6
```

`apply`函数也接受`关键字参数`,并将它们传递给被调用的函数.

```lisp
(apply go #:mode 'fast '("super.rkt"))
(apply go '("super.rkt") #:mode 'fast)
```

包含在`apply`的列表参数中的`关键字`, 不作为被调函数`go`的`关键字参数`;
相反,这个列表中的所有参数都被当作`位置参数`(by-position 参数).
要向`函数`传递`关键字参数列表`, 请使用`keyword-apply` 函数, 它接受一个要`作用`的函数和`三个列表`.

前两个列表是平行的,
第一个列表包含`关键词`(按`keyword<?`排序),
第二个列表包含对应每个`关键词`的`参数`.
第三个列表包含按位置排列的`函数参数`, 类似 `apply`.

```lisp
(keyword-apply go
               '(#:mode)
               '(fast)
               '("super.rkt"))
```

## 函数(Procedures):lambda

`lambda`表达式可以创建一个函数.在最简单的情况下, `lambda`表达式的形式是

    (lambda (arg-id ...)
      body ...+)

有`n`个`arg-id`的`lambda`形式接受`n`个参数.

```lisp
> ((lambda (x) x)
   1)
1
> ((lambda (x y) (+ x y))
   1 2)
3
> ((lambda (x y) (+ x y))
   1)
#<procedure>: arity mismatch;
 the expected number of arguments does not match the given
number
  expected: 2
  given: 1
```

### 声明 Rest 参数

`lambda`表达式也可以是以下形式

    (lambda rest-id
        body ...+)

也就是说, `lambda`表达式可以有一个没有被括号包围的`rest-id`.
由此产生的函数接受`任意数量`的参数, 参数被放入一个列表中, 列表与`rest-id`绑定.
例子.

```lisp
> ((lambda x x)
   1 2 3)
'(1 2 3)
> ((lambda x x))
'()
> ((lambda x (car x))
   1 2 3)
1
```

带有`rest-id`的函数, 经常使用`apply`来调用另一个接受`任何数量`参数的函数.
例子.

```lisp
(define max-mag
  (lambda nums
    (apply max (map magnitude nums))))
> (max 1 -2 0)
1
> (max-mag 1 -2 0)
2
```

`lambda` 形式也支持`必要`参数与 `rest-id` 相结合.

    (lambda (arg-id ...+ . rest-id)
        body ...+)

这种形式的意思是, 函数需要和`arg-ids`一样多的参数, 同时也接受任何数量的额外参数.
例子.

```lisp
(define max-mag
  (lambda (num . nums)
    (apply max (map magnitude (cons num nums)))))
> (max-mag 1 -2 0)
2
> (max-mag)
max-mag: arity mismatch;
 the expected number of arguments does not match the given
number
  expected: at least 1
  given: 0
```

`rest-id`变量有时被称为`rest argument`, 因为它接受函数参数的 `剩余部分`.

### 声明可选参数, Optional Arguments

`lambda` 形式中的参数(除`rest`参数外), 可以用`标识符`加上`默认值`的形式来指定, 而不仅仅只有`标识符`.

    (lambda gen-formals
        body ...+)

    gen-formals     =     (arg ...)
                                    |     rest-id
                                    |     (arg ...+ . rest-id)

                        arg     =     arg-id
                                    |     [arg-id default-expr]

形式为`[arg-id default-expr]`的参数是可选的(optional). 当`application`时没有提供该`参数`时, `default-expr` 产生`默认值`.
`default-expr` 可以引用任何前面的 `arg-id`, 后面的每一个 `arg-id` 也必须提供`默认值`.
和其他编程语言的规定相同, `默认参数`序列必须出现在`位置参数`序列的后面, 这样先消耗位置参数的`slot`.
否则无法消除参数解析的歧义.

例子.

```lisp
(define greet
  (lambda (given [surname "Smith"])
    (string-append "Hello, " given " " surname)))
> (greet "John")
"Hello, John Smith"
> (greet "John" "Doe")
"Hello, John Doe"
(define greet
  (lambda (given [surname (if (equal? given "John")
                              "Doe"
                              "Smith")])
    (string-append "Hello, " given " " surname)))
> (greet "John")
"Hello, John Doe"
> (greet "Adam")
"Hello, Adam Smith"
```

### 声明关键字参数

`lambda`形式可以声明某个`参数`是通过`关键字`传递的,而不是`位置`.
`关键词参数`可以与`位置参数`混合使用, 并且可以为任何一种参数提供`默认值`表达式.

    (lambda gen-formals
        body ...+)

    gen-formals   =   (arg ...)
                                |   rest-id
                                |   (arg ...+ . rest-id)

                        arg   =   arg-id
                                |   [arg-id default-expr]
                                |   arg-keyword arg-id
                                |   arg-keyword [arg-id default-expr]

用 `arg-keyword arg-id` 声明的参数, 在`应用`时候, 会匹配到使用相同`arg-keyword`的`arg-id`.
`关键字-标识符`对在`参数列表`中的位置, 对于`应用`时的参数匹配并不重要,
因为匹配依靠`关键字`, 而不是`位置`.

```lisp
(define greet
  (lambda (given #:last surname)
    (string-append "Hello, " given " " surname)))
> (greet "John" #:last "Smith")
"Hello, John Smith"
> (greet #:last "Doe" "John")
"Hello, John Doe"
```

`arg-keyword [arg-id default-expr]` 参数指定了一个基于`关键字`的参数, 并且有`默认值`.
例子.

```lisp
(define greet
  (lambda (#:hi [hi "Hello"] given #:last [surname "Smith"])
    (string-append hi ", " given " " surname)))
> (greet "John")
"Hello, John Smith"
> (greet "Karl" #:last "Marx")
"Hello, Karl Marx"
> (greet "John" #:hi "Howdy")
"Howdy, John Smith"
> (greet "Karl" #:last "Marx" #:hi "Guten Tag")
"Guten Tag, Karl Marx"
```

`lambda`形式并不直接支持创建一个接受 `rest keywords`的函数.
要构造一个接受所有`关键字参数`的函数,请使用`make-keyword-procedure`.

提供给`make-keyword-procedure`的函数在前两个(位置)参数中, 接收两个平行列表, 也就是`关键字--参数`对,
然后将来自`应用`的所有`位置`参数, 作为剩余的位置参数传入.
相当于会帮助你整理获得的所有参数.
例子.

```lisp
(define (trace-wrap f) ; 跟踪参数传递
    (make-keyword-procedure
         (lambda (kws kw-args . rest) ; 分别对应 keywords, keywords-参数, rest 参数
           (printf "Called with ~s ~s ~s\n" kws kw-args rest) ; 格式化字符串
           (keyword-apply f kws kw-args rest)))) ; 将函数应用到整理好的参数上.

> ((trace-wrap greet) "John" #:hi "Howdy")
Called with (#:hi) ("Howdy") ("John")
"Howdy, John Smith"
```

### 元数目敏感的函数:case-lambda

`case-lambda`形式创建一个`函数`,它可以根据提供的`参数数量`选择完全不同的行为.
`case-lambda` 表达式的形式是

    (case-lambda
        [formals body ...+]
            ...)

        formals   =   (arg-id ...)
                            |   rest-id
                            |   (arg-id ...+ . rest-id)

其中每个`[formals body ...+]`都类似于`(lambda formals body ...+)`.
应用由 `case-lambda` 产生的函数, 就像对于首个`参数数目`匹配的情形应用`lambda`.
例子.

```lisp
(define greet
  (case-lambda
    [(name) (string-append "Hello, " name)]
    [(given surname) (string-append "Hello, " given " " surname)]))
> (greet "John")
"Hello, John"
> (greet "John" "Smith")
"Hello, John Smith"
> (greet)
greet: arity mismatch;
 the expected number of arguments does not match the given
number
  given: 0
```

`case-lambda` 函数不能直接支持`可选参数`或`关键字参数`.

### 定义: define

基本`定义`的形式是

    (define id expr)

在这种情况下, `id` 被绑定到 `expr` 的结果.
例子.

```lisp
(define salutation (list-ref '("Hi" "Hello") (random 2)))
> salutation
"Hi"
```

### 函数快捷:shorthand

`define`形式也支持`函数定义`的速记.

    (define (id arg ...) body ...+)

这是对以下内容的`简写`

    (define id (lambda (arg ...) body ...+))

例子.

```lisp
(define (greet name)
  (string-append salutation ", " name))
> (greet "John")
"Hi, John"
(define (greet first [surname "Smith"] #:hi [hi salutation])
  (string-append hi ", " first " " surname))
> (greet "John")
"Hi, John Smith"
> (greet "John" #:hi "Hey")
"Hey, John Smith"
> (greet "John" "Doe")
"Hi, John Doe"
```

`define`的函数速记也支持`rest 参数`(即在末尾的`id`, 将额外参数收集到一个列表中).

    (define (id arg ... ... rest-id) body ...+)

这是`速记`,

    (define id (lambda (arg ... ... rest-id) body ...+))

例子.

```lisp
(define (avg . l)
  (/ (apply + l) (length l)))
> (avg 1 2 3)
2
```

### curried 函数速记

考虑下面这个`make-add-suffix`函数, 它接收一个`字符串`, 并返回另一个接收`字符串`的`函数`.

```lisp
(define make-add-suffix
  (lambda (s2)
    (lambda (s) (string-append s s2))))
```

虽然这并不常见, 但`make-add-suffix` 的结果可以直接调用, 就像这样.

```lisp
> ((make-add-suffix "!") "hello")
"hello!"
```

从某种意义上说, `make-add-suffix` 是接受两个`参数`的`函数`, 但它一次接受一个`参数`.
若函数可以接收`部分参数`并返回消耗更多参数的`函数`, 就称它为 `curried` 函数.

使用`define`的函数速记形式, `make-add-suffix` 可以等价写成

```lisp
(define (make-add-suffix s2)
  (lambda (s) (string-append s s2)))
```

这种速记法反映了`函数调用`的形状`(make-add-suffix "!")`.
`define`形式进一步支持, 定义`curried 函数`的速记, 反映`嵌套`的函数调用.

```lisp
(define ((make-add-suffix s2) s)
  (string-append s s2))

> ((make-add-suffix "!") "hello")
"hello!"
(define louder (make-add-suffix "!"))
(define less-sure (make-add-suffix "?"))
> (less-sure "really")
"really?"
> (louder "really")
"really!"
```

`define`的函数速记的完整语法如下.

    (define (head args) body ...+)
        head    =   id
                        |   (head args)
        args      =    arg ...
                        |   arg ... . rest-id

这个`shorthand`的展开, 对于定义中的每个`head`提供一个嵌套的`lambda`形式,
其中最里面的`head`对应于最外面的`lambda`.

### 多重值和 define-values

`Racket`表达式通常产生单个结果, 但有些表达式可以产生多个结果.
例如, `quotient`和`remainder`各自产生单个值,
但是`quotient/remainder`一次产生两个值, 分别相同.

```lisp
> (quotient 13 3)
4
> (remainder 13 3)
1
> (quotient/remainder 13 3)
4
1
```

如上所示, `REPL`将每个结果值打印在它自己的行中.

多值函数可以用 `values`函数来实现, 它接收任意数量的`值`并将其作为结果返回.

```lisp
> (values 1 2 3)
1
2
3
(define (split-name name)
  (let ([parts (regexp-split " " name)])
    (if (= (length parts) 2)
        (values (list-ref parts 0) (list-ref parts 1))
        (error "not a <first> <last> name"))))
> (split-name "Adam Smith")
"Adam"
"Smith"
```

`define-values`形式将多个标识符一次绑定到由单个`表达式`产生的`多个结果`上.

    (define-values (id ...) expr)

由`expr`产生的结果的数量必须与`id`的数量相匹配.
例子.

```lisp
(define-values (given surname) (split-name "Adam Smith"))
> given
"Adam"
> surname
"Smith"
```

`define`形式(不作为函数速记的时候), 等同于带有单个`id`的`define-values`形式.

### 内部定义

当一个`syntactic 形式`的语法指定了`body`, 那么相应的形式可以是`定义`或`表达式`.
作为`body`的`定义`是一个`内部定义`.

在一个`body`序列中, `表达式`和`内部定义`可以混合使用, 只要最后一个`body`是`表达式`(会被返回).
例如, `lambda` 的语法是

    (lambda gen-formals
      body ...+)

所以下面这些是该语法的有效实例.

```lisp
(lambda (f)                ; no definitions
  (printf "running\n")
  (f 0))

(lambda (f)                ; one definition
  (define (log-it what)
    (printf "~a\n" what))
  (log-it "running")
  (f 0)
  (log-it "done"))

(lambda (f n)              ; two definitions
  (define (call n)
    (if (zero? n)
        (log-it "done")
        (begin
          (log-it "running")
          (f n)
          (call (- n 1)))))
  (define (log-it what)
    (printf "~a\n" what))
  (call n))
```

在特定的`body`序列中的`内部定义`是`相互递归`(mutually recursive)的;
也就是说, 任何`定义`都可以引用任何其他`定义`--只要该`引用`不会在`定义`之前, 就被实际`计算`.
如果一个`定义`被过早`引用`, 就会发生错误.
例子.

```lisp
(define (weird)
  (define x x)
  x)
> (weird)
x: undefined;
 cannot use before initialization
```

只使用`define`的内部定义序列, 很容易被翻译成等价的`letrec`形式(在下一节介绍).
然而, 其他定义形式可以作为`body`出现, 包括`define-values`, `struct` (见程序员定义的数据类型)或`define-syntax`(见宏).

## 局域绑定,local binding

尽管可以使用内部`define`用于`局域绑定`, 但`Racket`提供了三种形式, 让程序员对绑定有更多控制: `let`, `let*` 和 `letrec`.

### 平行绑定: let

`Local Binding: let, let*, letrec, ...` in The Racket Reference也记录了 `let`.

`let`形式绑定了一组`标识`符, 每个`标识符`都是某个`表达式`的结果, 以便在`let`主体中使用.

    (let ([id expr] ...) body ...+)

这些标识符是 `并行` 绑定的.
也就是说, 任何`id`都不会被绑定在中括号内, 右侧的`expr` 中, 但是所有的`id`都可以在`body`中使用.
这些`id`必须是彼此不同的.
例子.

```lisp
> (let ([me "Bob"])
    me)
"Bob"
> (let ([me "Bob"]
        [myself "Robert"]
        [I "Bobby"])
    (list me myself I))
'("Bob" "Robert" "Bobby")
> (let ([me "Bob"]
        [me "Robert"])
    me)
eval:3:0: let: duplicate identifier
  at: me
  in: (let ((me "Bob") (me "Robert")) me)
```

`id`的`expr` 看不到它自己的`绑定`, 这对于必须引用`旧值`的`包装器`(wrappers)来说通常很有用.

```lisp
> (let ([+ (lambda (x y)
             (if (string? x)
                 (string-append x y)
                 (+ x y)))]) ; 使用原来的 + 的定义
    (list (+ 1 2)
          (+ "see" "saw")))

'(3 "seesaw")
```

偶尔, `let` 绑定的平行性质, 对于交换或重排一组`绑定`也是很方便的. 相当于其他语言中的传统艺能`a=c,a=b,b=c`.

```lisp
> (let ([me "Tarzan"]
        [you "Jane"])
    (let ([me you]
          [you me])
      (list me you)))

'("Jane" "Tarzan")
```

将 `let` 绑定描述为 `平行`, 并不意味着要进行`并发`计算(concurrent).
`绑定`被延迟, 所有表达式按照顺序被计算之后, 再进行绑定操作.

参考: [如何理解: 程序, 进程, 线程, 并发, 并行, 高并发? ](https://www.zhihu.com/question/307100151/answer/894486042)

### 顺序绑定: let*

`Local Binding: let, let*, letrec, ...` in The Racket Reference也记录了 `let*`.

`let*`的语法与`let`相同.

    (let* ([id expr] ...) body ...+)

不同的是, 每个`id`都可以在后面的 `expr` 中使用, 也可以在 `body` 中使用.
此外, 这些`id`不需要不相同, 最近的绑定是唯一可见的 (most recent binding is the visible one).
例子.

```lisp
> (let* ([x (list "Burroughs")]
         [y (cons "Rice" x)]
         [z (cons "Edgar" y)])
    (list x y z))
'(("Burroughs") ("Rice" "Burroughs") ("Edgar" "Rice" "Burroughs"))
> (let* ([name (list "Burroughs")]
         [name (cons "Rice" name)]
         [name (cons "Edgar" name)])
    name)
'("Edgar" "Rice" "Burroughs")
```

换句话说, `let*` 形式相当于嵌套的 `let` 形式, 每个都有单独的`绑定`.

```lisp
> (let ([name (list "Burroughs")]) ; 绑定 name(1)
      (let ([name (cons "Rice" name)]) ; 绑定 name(2)
          (let ([name (cons "Edgar" name)]) ; 绑定 name(3)
              name)))

'("Edgar" "Rice" "Burroughs")
```

### 递归绑定: letrec

`Local Binding: let, let*, letrec, ...` in The Racket Reference 也记录了 `letrec`.

`letrec` 的语法也与 `let` 相同.

    (letrec ([id expr] ...) body ...+)

`let` 只让它的绑定在 `bodys` 中可用, 而 `let*`让它的绑定对任何后来的绑定 `expr` 可用,
`letrec` 则让它的绑定对所有其他 `expr` --甚至前面的 `expr`--都可用. 换句话说, `letrec` 的绑定是`递归的`.

`letrec` 形式中的 `expr` 通常是`递归`和`相互递归`函数的, `lambda`形式.

```lisp
> (letrec ([swing ; 将 swing 绑定到匿名函数; swing,摇荡
            (lambda (t)
              (if (eq? (car t) 'tarzan) ; 判断首元素是否为 'tarzan ...
                  (cons 'vine
                        (cons 'tarzan (cddr t))) ; 是的话, 就改成 'vine 'tarzan ...
                  (cons (car t)
                        (swing (cdr t)))))]) ; 否则就判断余下的元素, cdr 取对儿的第二个元素, 对于列表, 相当于首元素之外的部分.
    (swing '(vine tarzan vine vine))) ; 藤 泰山 藤 藤
'(vine vine tarzan vine) ; 藤 藤 泰山 藤

> (letrec ([tarzan-near-top-of-tree? ; 局部函数定义: 泰山在树顶吗? 这个函数用来查找文件树前几层的文件.
                      (lambda (name path depth) ; 它是匿名函数, 名字, 路径, 深度
                          (or (equal? name "tarzan") ; 如果 name 是泰山, 返回 true
                                  (and (directory-exists? path) ; 否则, 需要 path 存在
                                            (tarzan-in-directory? path depth))))] ; 且 泰山在目录中. 调用局部函数
                    [tarzan-in-directory? ; 局部函数定义: 泰山在目录? 的定义
                      (lambda (dir depth) ; 它是匿名函数, 目录,深度
                          (cond ; 分支判断
                              [(zero? depth) #f] ; 路径深度为0, 则返回 false, 泰山不在目录中.
                              [else
                                  (ormap ; 否则返回第一个不为 false 的值, 把下面的 lambda 函数作用到所有的文件上.
                                        (λ (elem) ; λ 是 lambda 的同义词,
                                            (tarzan-near-top-of-tree? (path-element->string elem);
                                                                                                (build-path dir elem)
                                                                                                (- depth 1))) ; 查找深度减少1
                                        (directory-list dir))]))]); 给出 dir 中的文件和目录
    (tarzan-near-top-of-tree? "tmp" ; 此次查找中, 泰山的名字
                                                        (find-system-path 'temp-dir) ; 给出起始路径
                                                        4)) ; 给出最大查找深度
directory-list: could not open directory
  path: /var/tmp/systemd-private-1eaff33eab174a96aebae0ebdcb
a6e2e-chronyd.service-b5fOZN
  system error: Permission denied; errno=13
```

虽然`letrec`形式的`exprs`通常是`lambda`表达式, 但它们可以是任何表达式.
表达式是`按顺序`计算的, 在获得每个`值`后, 它立即与相应的`id`关联.
如果某`id`在其值准备好之前被`引用`, 就会产生(raise)一个`错误`, 就像`内部定义`一样.

```lisp
> (letrec ([quicksand quicksand])
    quicksand)
quicksand: undefined;
 cannot use before initialization
```

### let 递归, named let

命名的`let`是一种`迭代`和`递归`形式.
它使用与`局部绑定`相同的语法关键字 `let`, 但 `let` 后面的`标识符`(而不是直连的开放小括号`(`)会触发不同的解析.

    (let proc-id ([arg-id init-expr] ...)
      body ...+)

命名的 `let` 形式等同于

    (letrec ([proc-id (lambda (arg-id ...) ; 递归定义匿名函数
                         body ...+)])
      (proc-id init-expr ...)) ; 使用 init-expr 的结果作为参数, 初始化调用 proc-id, 用户可以进行后续调用

也就是说, 命名的 `let` 绑定了一个仅在函数`body`中可见的函数`标识符`, 它隐含地用一些`初始表达式`的`值`来调用该`函数`.
例子.

```lisp
(define (duplicate pos lst); 函数 deplicate, 变量 pos lst
  (let dup ([i 0] ; 定义匿名函数 dup, 变量 i=0, lst=lst, 这里给出的是初始值.
                    [lst lst]) ;这里给出的是初始值.
   (cond
    [(= i pos) (cons (car lst) lst)] ; 如果 i=pos, 复制首元素
    [else (cons  (car lst) (dup (+ i 1) (cdr lst)))]))) ; 如果 i != pos, 递归判断后面的元素
> (duplicate 1 (list "apple" "cheese burger!" "banana"))
'("apple" "cheese burger!" "cheese burger!" "banana")
```

### 多重值: let-values, let*-values, letrec-values

`Local Binding: let, let*, letrec, ...` in The Racket Reference也记录了`多值绑定`形式.

与`define-values` 在定义中`绑定`多个结果的方式相同(见`Multiple Values and define-values`),
`let-values`, `let*-values`, and `letrec-values` 在局部绑定多个结果.

    (let-values ([(id ...) expr] ...)
      body ...+)

    (let*-values ([(id ...) expr] ...)
      body ...+)

    (letrec-values ([(id ...) expr] ...)
      body ...+)

每个`expr`必须产生与相应id一样多的值. `绑定规则`和没有`-values`的形式是相同的:
`let-values` 的 `id` 只在 `bodys` 中绑定,
`let*-values` 的 `id` 在后面子句的 `expr` 中绑定,
而`letrec-values`的 `id` 对所有 `expr` 都绑定.
例子.

```lisp
> (let-values ([(q r) (quotient/remainder 14 3)])
    (list q r))
'(4 2)
```

## 条件式,Conditionals

大多数用于`分支`的函数, 如`<`和`string?`, 产生`#t`或`#f`.
然而, `Racket` 的分支形式将 `#f` 以外的`任何值`都视为`真`.
我们说的`真值`是指`#f`以外的任何值.

这种对 `真值` 的约定与某些协议相吻合, 在这些协议中, `#f`可以作为`failure`或者表示一个`可选值`没有被提供.
(注意不要过度使用这个技巧, 记住`exception`通常是报告失败的更好机制). )

例如, `member`函数有双重作用;
它可以用来查找以某一特定项目开始的`尾部列表`, 也可以用来简单地检查`列表`中是否存在某一`项目`.

```lisp
> (member "Groucho" '("Harpo" "Zeppo"))
#f
> (member "Groucho" '("Harpo" "Groucho" "Zeppo"))
'("Groucho" "Zeppo")
> (if (member "Groucho" '("Harpo" "Zeppo"))
      'yep
      'nope)
'nope
> (if (member "Groucho" '("Harpo" "Groucho" "Zeppo"))
      'yep
      'nope)
'yep
```

### 简单分支: if

`条件: if, cond, and, and or` 在`<Racket参考>`中也记录了 `if`.

在`if`的形式中.

    (if test-expr then-expr else-expr)

`test-expr`总是被计算. 如果它产生了 `#f` 以外的`任何值`, 那么`then-expr`将被计算. 否则, `else-expr`被计算.

`if`形式必须有一个`then-expr`和一个`else-expr`; 后者不是`可选的`.
要根据`test-expr`执行(或跳过)副作用, 可以使用`when`或`unless`, 我们将在后面的 `Sequencing` 中介绍.

### 组合测试: and 和 or

条件: if, cond, and, and or 在`<Racket参考>`中也记录了and和or.

`Racket`的`and`和`or`是`syntactic 形式`, 而不是`函数`.
与`函数`不同, 如果前面的表达式决定了答案, 那么`and`和`or`形式可以跳过后面的表达式的计算.

    (and expr ...)

如果它的任何一个`expr`产生`#f`, 那么`and`形式就会产生`#f`. 否则, 它产生最后一个`expr`的值.
作为一种特殊情况, `(and)`产生`#t`.

    (or expr ...)

如果所有的表达式都产生`#f`, 那么`or`形式会产生`#f`. 否则, 它会产生其表达式中第一个非`#f`的值. 作为一种特殊情况, `(or)`产生`#f`.
例子.

```lisp
> (define (got-milk? lst)
    (and (not (null? lst)) ; 不能是空列表
         (or (eq? 'milk (car lst)) ; 对列表一个接一个判断
             (got-milk? (cdr lst))))) ; 如果需要, 对后面的元素递归
> (got-milk? '(apple banana))
#f
> (got-milk? '(apple milk banana))
#t
```

如果求值到达`and` 或 `or` 形式的最后一个`expr`, 那么 `expr` 的值直接决定了`and` 或 `or` 的结果.
因此, 最后一个`expr` 处于`尾部位置`, 这意味着上面的 `got-milk?` 函数在恒定空间中运行(constant space).

`Tail Recursion` 介绍了`尾部调用`和`尾部位置`.

### 链式测试: cond

`cond`形式将一系列的`测试`串联起来, 以选择一个`结果`表达式. 初步来说, `cond` 的语法如下.

条件: if, cond, and, and or 在Racket Reference中也记录了cond.

    (cond [test-expr body ...+] .
                  ...)

每个`test-expr`都是按顺序计算的.
如果产生`#f`, 相应的`bodys`就会被忽略, 然后继续计算下一个`test-expr`.
一旦某个测试表达式产生`真值`, 相关的`bodys`就会被计算, 以产生`cond`形式的结果, 并且`不再计算`其他测试表达式.

`cond`中的最后一个`test-expr可`以用`else`代替.
在计算方面, `else`是`#t`的同义词, 但是它阐明了最后一个`子句`是为了捕捉`所有`剩余的情况.
如果不使用`else`, 那么有可能没有测试表达式产生一个`真值`; 在这种情况下, `cond` 表达式的结果是`#<void>`.
示例.

```lisp
> (cond
   [(= 2 3) (error "wrong!")]
   [(= 2 2) 'ok])
'ok
> (cond
   [(= 2 3) (error "wrong!")])
> (cond
   [(= 2 3) (error "wrong!")]
   [else 'ok])
'ok
(define (got-milk? lst)
  (cond
    [(null? lst) #f]
    [(eq? 'milk (car lst)) #t]
    [else (got-milk? (cdr lst))]))

> (got-milk? '(apple banana))
#f
> (got-milk? '(apple milk banana))
#t
```

`cond` 的完整语法还包括两种子句.

    (cond-clause ...)

    cond-clause = [test-expr then-body ...+]
                                | [else then-body ...+].
                                | [test-expr => proc-expr].
                                | [test-expr] [test-expr

`=>` 变体捕获其`test-expr` 的`true`结果, 并将其传递给 `proc-expr` 的结果, 该`结果`必须是个`单参数的函数`.
例子.

```lisp
> (define (after-groucho lst)
    (cond
      [(member "Groucho" lst) => cdr]
      [else (error "not there")]))
> (after-groucho '("Harpo" "Groucho" "Zeppo"))
'("Zeppo")
> (after-groucho '("Harpo" "Zeppo"))
not there
```

只包括`test-expr` 的`子句`很少使用. 它捕获`test-expr`的`true`结果, 并简单地返回为整个`cond`表达式的结果.

## 流程化,Sequencing

`Racket`的程序员们更喜欢编写尽可能少的`副作用`的程序, 因为纯粹的`functional code`更容易测试和组成更大的程序.
然而, 与外部环境的`交互`需要`sequencing`, 比如在向显示器写东西, 打开`图形窗口`, 或者操作磁盘上的`文件`时.

### Before的效果: begin

序列: begin, begin0和begin-for-syntax在 `<Racket参考>` 中也记录了begin.

`begin`表达式对`表达式`进行排序.

    (begin expr ...+)

这些`表达式`按顺序被计算, 除了最后一个`表达式`, 其他的结果都被`忽略`.
最后一个`expr`的结果作为`begin` 表达式的结果, 相对于 `begin` 表达式, 它处于`尾部位置`.
例子.

```lisp
(define (print-triangle height)
  (if (zero? height)
      (void)
      (begin
        (display (make-string height #\*))
        (newline)
        (print-triangle (sub1 height)))))
> (print-triangle 4)
****
***
**
*
```

许多形式, 如 `lambda` 或 `cond` , 即使没有 `begin`, 也支持表达式的`sequence`.
这些位置有时被称为`含有隐含begin`.
举例来说.

```lisp
(define (print-triangle height)
  (cond
    [(positive? height)
     (display (make-string height #\*))
     (newline)
     (print-triangle (sub1 height))]))
> (print-triangle 4)
****
***
**
*
```

`begin` 形式在`顶层`, `模块层`或在`内部定义`之后作为`body`时, 是很特别的.
在这些位置, `begin` 的内容不形成一个表达式, 而是被`拼接`(spliced)到周围的上下文中.
例子.

```lisp
> (let ([curly 0])
    (begin
      (define moe (+ 1 curly))
      (define larry (+ 1 moe)))
    (list larry curly moe))
'(2 0 1)
```

这种拼接行为主要对宏有用, 我们在后面的`Macros`中讨论.

### After的效果:  begin0

Sequencing: begin, begin0, 和 The Racket Reference 中的 begin-for-syntax 也记录了 begin0.

`begin0`表达式的语法与`begin`表达式相同.

    (begin0 expr ...+)

不同的是, `begin0` 返回第一个 `expr` 的结果, 而不是最后一个 `expr` 的结果.
`begin0` 形式对于实现计算后发生的副作用很有用, 特别是在计算产生`未知数量`的结果的情况下.
例子.

```lisp
(define (log-times thunk)
  (printf "Start: ~s\n" (current-inexact-milliseconds))
  (begin0
    (thunk)
    (printf "End..: ~s\n" (current-inexact-milliseconds))))
> (log-times (lambda () (sleep 0.1) 0))
Start: 1626421344073.401
End..: 1626421344173.4265
0
> (log-times (lambda () (values 1 2)))
Start: 1626421344174.647
End..: 1626421344174.661
1
2
```

### Effects If...: when 和 unless

Guarded Evaluation: when and unless in The Racket Reference 也记录了 when 和 unless.

`when`形式结合了`if`风格的条件, 对 `then`子句进行`sequencing`, 没有 `else` 子句.

    (when test-expr then-body ...+)

如果 `test-expr` 产生一个`真值`, 那么所有的 `then-body` 都被计算.
最后一个 `then-body` 的结果是 `when` 形式的结果. 否则, 没有 `then-body` 被计算, 结果是 `#<void>`.

`unless` 形式也是类似的.

    (unless test-expr then-body ...+)

不同的是, `test-expr` 的结果是相反的: 只有当 `test-expr` 的结果是 `#f` 时, `then-body` 才被计算.
例子:

```lisp
(define (enumerate lst)
  (if (null? (cdr lst))
      (printf "~a.\n" (car lst))
      (begin
        (printf "~a, " (car lst))
        (when (null? (cdr (cdr lst)))
          (printf "and "))
        (enumerate (cdr lst)))))

> (enumerate '("Larry" "Curly" "Moe"))
Larry, Curly, and Moe.
(define (print-triangle height)
  (unless (zero? height)
    (display (make-string height #\*))
    (newline)
    (print-triangle (sub1 height))))

> (print-triangle 4)
****
***
**
*
```

## 赋值: set!

赋值: set! 和set! -value 在`<Racket参考>`中也记载了set! .

使用 `set!` 给变量赋值.

    (set! id expr)

`set!` 表达式会对 `expr` 进行求值, 并将 `id`(必须在包围的环境中绑定过)改为`结果值`.
`set!` 表达式本身的结果是 `#<void>`.
例子.

```lisp
(define greeted null)
(define (greet name)
  (set! greeted (cons name greeted))
  (string-append "Hello, " name))
> (greet "Athos")
"Hello, Athos"
> (greet "Porthos")
"Hello, Porthos"
> (greet "Aramis")
"Hello, Aramis"
> greeted
'("Aramis" "Porthos" "Athos")

(define (make-running-total)
  (let ([n 0])
    (lambda ()
      (set! n (+ n 1))
      n)))
(define win (make-running-total))
(define lose (make-running-total))
> (win)
1
> (win)
2
> (lose)
1
> (win)
3
```

### 使用赋值的准则

尽管有时使用 `set!` 是合适的, 但 `Racket` 风格一般不鼓励使用 `set!`. 下面的指南可以帮助解释什么时候使用 `set!` 是合适的.

如同在任何现代语言中, 不要使用 `对共享标识符的赋值` 代替 `向过程传递参数或从过程获得其结果`.

真垃圾的例子:

```lisp
(define name "unknown")
(define result "unknown")
(define (greet)
  (set! result (string-append "Hello, " name)))

> (set! name "John")
> (greet)
> result
"Hello, John"
```

Ok 的例子:

```lisp
(define (greet name)
(string-append "Hello, " name))

> (greet "John")
"Hello, John"
> (greet "Anna")
"Hello, Anna"
```

对局部变量的`一连串赋值`远不如`嵌套式绑定`.

Bad 例子:

```lisp
> (let ([tree 0])
    (set! tree (list tree 1 tree))
    (set! tree (list tree 2 tree))
    (set! tree (list tree 3 tree))
    tree)
'(((0 1 0) 2 (0 1 0)) 3 ((0 1 0) 2 (0 1 0)))
```

Ok 例子:

```lisp
> (let* ([tree 0]
         [tree (list tree 1 tree)]
         [tree (list tree 2 tree)]
         [tree (list tree 3 tree)])
    tree)
'(((0 1 0) 2 (0 1 0)) 3 ((0 1 0) 2 (0 1 0)))
```

使用`赋值`来累积`迭代`的结果是不好的风格. 通过一个`loop`参数进行`累积`是更好的.

不太好的例子:

```lisp
(define (sum lst)
  (let ([s 0])
    (for-each (lambda (i) (set! s (+ i s)))
              lst)
    s))
> (sum '(1 2 3))
6
```

Ok 例子:

```lisp
(define (sum lst)
  (let loop ([lst lst] [s 0]) ; let 的迭代形式, 这里给出初始值
    (if (null? lst) ; 如果为空,返回 0
        s
        (loop (cdr lst) (+ s (car lst)))))) ; 否则迭代
> (sum '(1 2 3))
6
```

更好的(使用现有函数)例子:

```lisp
(define (sum lst)
  (apply + lst))
> (sum '(1 2 3))
6
```

Good(一个一般的方法)例子:

```lisp
(define (sum lst)
  (for/fold ([s 0]) ; (for/fold ([累积-id 初始化-expr] ... 可以有-result处理 ) (for-子句 ...)
            ([i (in-list lst)])
    (+ s i)))
> (sum '(1 2 3))
6
```

若必须使用`有状态`的对象, 或这样更合适, 那么用 `set!` 来设置对象的状态是合理的.

Ok 例子(计数器):

```lisp
(define next-number!
  (let ([n 0])
    (lambda ()
      (set! n (add1 n))
      n)))
> (next-number!)
1
> (next-number!)
2
> (next-number!)
3
```

在其他条件相同的情况下, 不使用`赋值`或`变异`(mutation)的程序总是比使用`赋值`或`变异`的程序要好.
虽然要避免副作用, 但是如果产生的代码可读性明显提高, 或者实现了明显更好的算法, 就应该使用.

与直接使用`set!` 相比, 使用`可变值`(mutable values), 如`向量`和`哈希表`, 引起对程序风格的疑虑(suspicions)更少.
然而, 简单地用`vector-set!`代替程序中的`set!`显然不能改善程序的风格.

### 多重值: set! -values

Racket Reference>中的+赋值: set! 和set! -values也记录了set! -values.

`set!-values` 形式一次赋值给`多个变量`, 如果给定一个`表达式`, 产生适当数量的`值`.

    (set!-values (id ...) expr)

这种形式等同于使用 `let-values` 从 `expr`接收多个结果, 然后使用 `set!` 将结果单独分配给 `id`.
例子.

```lisp
(define game
  (let ([w 0]
        [l 0])
    (lambda (win?)
      (if win?
          (set! w (+ w 1))
          (set! l (+ l 1)))
      (begin0
        (values w l)
        ; swap sides...
        (set!-values (w l) (values l w))))))
> (game #t)
1
0
> (game #t)
1
1
> (game #f)
1
2
```

## 引用: quote和 '

Literals: quote和#%datum在The Racket Reference中也记录了quote.

`quote`形式产生常数.

    (quote datum)

在技术上, `datum`的语法, 可以指定为任何能被`read`函数解析为`单元素`的东西.
`quote` 形式的值与 `read` 在给定 `datum`时产生的`值`相同.

`datum`可以是`符号`, `布尔值`, `数字`, `(字符或字节)字符串`, `字符`, `关键字`, `空列表`,
包含更多此类值的`对(或列表)`, 包含更多此类值的`向量`, 包含更多此类值的`哈希表`, 或者包含其他此类值的`盒子`.
例子.

```lisp
> (quote apple)
'apple
> (quote #t)
#t
> (quote 42)
42
> (quote "hello")
"hello"
> (quote ())
'()
> (quote ((1 2 3) #("z" x) . the-end))
'((1 2 3) #("z" x) . the-end)
> (quote (1 2 . (3)))
'(1 2 3)
```

正如上面的最后一个例子所示, `datum`不一定要与`值`的`规范化打印形式`相匹配.
`datum`不能是以`#<`开头的打印形式, 所以它不能是 `#<void>`, `#<undefined>`, 或一个`过程`(procedure).

`引号`形式很少用于本身是`布尔值`, `数字`或`字符串`的数据, 因为这些值的打印形式已经可以作为`常量`使用.
`引号`形式更多的是用于`符号`和`列表`, `符号`和`列表`在不加`引号`时有其他的含义(`标识符`, `函数调用`等).

表达式

    'datum

是以下内容的缩写

    (quote datum)

Racket参考>中的+Reading Quotes提供了更多关于`'` 速记的信息.

而且这个`速记`几乎总是用来代替 `quote`. 这个速记法甚至适用于`datum`内部, 所以它可以产生包含`quote`的列表.
例子.

```lisp
> 'apple
'apple
> '"hello"
"hello"
> '(1 2 3)
'(1 2 3)
> (display '(you can 'me))
(you can (quote me))
```

## 准引用: quasiquote和'

Racket参考>中的+准引号: 准引号, 无引号和无引号拼接也记录了准引号.

`quasiquote` 的形式与 `quote` 类似.

    (quasiquote datum)

然而, 对于每一个出现在`datum`中的`(unquote expr)`, `expr` 会被计算出`值`, 以取代`unquote`子形式.
例子.

```lisp
> (quasiquote (1 2 (unquote (+ 1 2)) (unquote (- 5 1))))
'(1 2 3 4)
```

这种形式可以用来编写某些函数, 根据`模式`建立起`列表`.
例子.

```lisp
> (define (deep n)
    (cond
      [(zero? n) 0]
      [else
       (quasiquote ((unquote n) (unquote (deep (- n 1)))))]))
> (deep 8)
'(8 (7 (6 (5 (4 (3 (2 (1 0))))))))
```

甚至可以`廉价`地以编程的方式构造`表达式`. (当然, 十有八九, 你应该使用`macro`来做这件事
(第十次是当你在学习[PLAI](https://cs.brown.edu/~sk/Publications/Books/ProgLangs/)这样的教科书时).
例子.

```lisp
> (define (build-exp n)
    (add-lets n (make-sum n)))
> (define (add-lets n body)
    (cond
      [(zero? n) body]
      [else
       (quasiquote
        (let ([(unquote (n->var n)) (unquote n)])
          (unquote (add-lets (- n 1) body))))]))
> (define (make-sum n)
    (cond
      [(= n 1) (n->var 1)]
      [else
       (quasiquote (+ (unquote (n->var n))
                      (unquote (make-sum (- n 1)))))]))
> (define (n->var n) (string->symbol (format "x~a" n)))
> (build-exp 3)
'(let ((x3 3)) (let ((x2 2)) (let ((x1 1)) (+ x3 (+ x2 x1)))))
```

`unquote-splicing` 形式与 `unquote` 相似, 但是它的 `expr` 必须产生一个列表,
而 `unquote-splicing` 形式必须出现在产生`列表`或`向量`的`上下文`中.
顾名思义, 它产生的`列表`被拼接到它所在的`上下文`中.
例子.

```lisp
> (quasiquote (1 2 (unquote-splicing (list (+ 1 2) (- 5 1))) 5))
'(1 2 3 4 5)
```

使用`splicing`, 我们可以修改上面的示例`表达式`的结构, 使其只有一个`let`表达式和一个`+`表达式.
例子.

```lisp
> (define (build-exp n)
    (add-lets
     n
     (quasiquote (+ (unquote-splicing
                     (build-list
                      n
                      (λ (x) (n->var (+ x 1)))))))))
> (define (add-lets n body)
    (quasiquote
     (let (unquote
           (build-list
            n
            (λ (n)
              (quasiquote
               [(unquote (n->var (+ n 1))) (unquote (+ n 1))]))))
       (unquote body))))
> (define (n->var n) (string->symbol (format "x~a" n)))
> (build-exp 3)

'(let ((x1 1) (x2 2) (x3 3)) (+ x1 x2 x3))
```

如果 `quasiquote` 形式出现在一个 enclosing 的`quasiquote`形式内,
那么内部的`quasiquote`就有效地取消一层`unquote`和`unquote-splicing`形式,
这样就需要第二个`unquote`或`splicing`.
例子.

```lisp
> (quasiquote (1 2 (quasiquote (unquote (+ 1 2)))))
'(1 2 (quasiquote (unquote (+ 1 2))))
> (quasiquote (1 2 (quasiquote (unquote (unquote (+ 1 2))))))
'(1 2 (quasiquote (unquote 3)))
> (quasiquote (1 2 (quasiquote ((unquote (+ 1 2)) (unquote (unquote (- 5 1)))))))
'(1 2 (quasiquote ((unquote (+ 1 2)) (unquote 4))))
```

上面的求值实际上不会如图所示打印.  相反, 将使用`quasiquote`和`unquote`的简写形式.
`` ` ``, 即`反引号` 和`,` 即`逗号`. 同样的速记方式也可以在`表达式`中使用.
例子.

```lisp
> `(1 2 `(,(+ 1 2) ,,(- 5 1)))
'(1 2 `(,(+ 1 2) ,4))
```

`unquote-splicing` 的速记形式是`,@`:
例子.

```lisp
> `(1 2 ,@(list (+ 1 2) (- 5 1)))
'(1 2 3 4)
```

### 简单派发: case

`case` 形式跳转(dispatch)到子句, 通过将`表达式`的结果与子句的`值`相匹配.

    (case expr
      [(datum ...+) body ...+]
      ...)

每个 `datum` 将使用 `equal?` 与 `expr` 的结果进行比较, 然后对相应的 `bodys` 进行计算.
对于`N`个`datum`s, `case` 形式可以在 `O(log N)` 时间内调度到正确的子句.

每个`子句`可以提供多个`datums`, 如果有任何一个`datum`匹配, 就计算相应的`子句`.
例子.

```lisp
> (let ([v (random 6)])
    (printf "~a\n" v)
    (case v
      [(0) 'zero]
      [(1) 'one]
      [(2) 'two]
      [(3 4 5) 'many]))
3
'many
```

`case`形式的最后一个子句可以使用`else`, 就像`cond`一样.
例子.

```lisp
> (case (random 6)
    [(0) 'zero]
    [(1) 'one]
    [(2) 'two]
    [else 'many])
'zero
```

对于更一般的`模式匹配`(但没有`调度时间`(dispatch-time)的保证), 使用`match`, 它在`模式匹配`中介绍.

## 动态绑定: 参数化

The Racket Reference中的+Parameters也记录了参数化.

`parameterize` 形式在计算 `body` 表达式时, 将一个新`值`与`参数`(parameter)联系起来.

    (parameterize ([parameter-expr value-expr] ...)
      body ...+)

>术语 `参数` 有时用来指函数的参数, 但在这里, `Racket` 中的 `参数` 具有更具体的含义.
例如, `error-print-width` 参数控制, 在错误信息中打印多少个字符的`值`.

```lisp
> (parameterize ([error-print-width 5])
    (car (expt 10 1024)))
car: contract violation
  expected: pair?
  given: 10...
> (parameterize ([error-print-width 10])
    (car (expt 10 1024)))
car: contract violation
  expected: pair?
  given: 1000000...
```

更一般地说, `参数`实现了一种`动态绑定`(dynamic binding). 
`make-parameter`函数接受任何值, 并返回一个新的`参数`, 该`参数`被初始化为`给定值`.
将参数作为一个`函数`应用, 返回其`当前值`.

```lisp
> (define location (make-parameter "here"))
> (location)
"here"
```

在`参数化`形式中, 每个`parameter-expr`必须产生一个`参数`. 
在`body`s的计算过程中, 每个指定的参数都被赋予相应的`value-expr`的结果. 
当`控制`(control)离开`parameterize`形式时--无论是通过正常的`返回`(return), 
`异常`还是其他的`escape`-- `参数`都会被恢复到其先前的值.

```lisp
> (parameterize ([location "there"])
    (location))
"there"
> (location)
"here"
> (parameterize ([location "in a house"])
    (list (location)
          (parameterize ([location "with a mouse"])
            (location))
          (location)))
'("in a house" "with a mouse" "in a house")
> (parameterize ([location "in a box"])
    (car (location)))
car: contract violation
  expected: pair?
  given: "in a box"
> (location)
"here"
```

`parameterize` 形式不是像`let`那样的`绑定`形式; 上述`location`的每次使用都直接指向`原始定义`. 
`parameterize`形式在`parameterize` 的 body 被计算的整个过程中, 调整`parameter`的`值`, 
即使是在`parameterize`body 以外的文本中使用`parameter `.
这也是 dynamic scope 和 lexical scope 的主要区别.

```lisp
> (define (would-you-could-you?)
    (and (not (equal? (location) "here"))
         (not (equal? (location) "there"))))
> (would-you-could-you?)
#f
> (parameterize ([location "on a bus"])
    (would-you-could-you?))
#t
```

如果一个`parameter `的使用, 在`文本`意义上位于`parameterize`的 body  内,
但在`parameterize`形式产生`值`之前没有被`计算`, 那么这次`使用`就不会看到`parameterize`形式所安装的`值`.
也就是`parameterize`生效之后, 如果不及时使用, `parameter` 的值会被还原.

```lisp
> (let ([get (parameterize ([location "with a fox"])
               (lambda () (location)))]); 生成调用 location 的匿名函数
    (get)) ; 但是这里 location 的值已经被还原了 
"here"
```

可以通过把`parameter`作为带`值`的`函数`调用, 来强制性地调整`parameter `的当前`绑定`.
如果一个`parameterize`已经调整了`参数`的值, 那么直接应用这个`parameter ` procedure , 
只影响到与当前激活的`parameterize`相关的值.

```lisp
> (define (try-again! where)
    (location where)) ; 把参数当函数, 直接调用
> (location)
"here"
> (parameterize ([location "on a train"])
    (list (location)
          (begin (try-again! "in a boat")
                 (location)))) ; 直接调用, 强行改变参数的值, 只在这里起效
'("on a train" "in a boat")
> (location) ; 但是 location 的值之后会被恢复原状
"here"
```

使用`parameterize`通常比强制更新`参数`值要好, 这和用 `let` 绑定新的`变量`, 比用 `set!` 要好的原因差不多. (见`Assignment: set!`).

`变量`和 `set!` 似乎可以解决许多 `参数` 能解决的问题.
例如, `lokation` 可以被定义为一个`字符串`, 而 `set!` 可以用来调整它的值.

```lisp
> (define lokation "here")
> (define (would-ya-could-ya?)
    (and (not (equal? lokation "here"))
         (not (equal? lokation "there"))))
> (set! lokation "on a bus")
> (would-ya-could-ya?)
#t
```

然而, 与 `set!` 相比, `参数` 提供了几个关键的优势.

+ `parameterize`形式有助于在 control  因`异常`而 escapes  时, 自动重置`参数`的`值`. 
添加`异常处理`程序和其他形式来恢复 `set!` 是相对繁琐的.
+ `参数`与`尾部调用`配合得很好(见`Tail Recursion`). 
`parameterize`形式中的最后一个`body`相对于`parameterize`形式来说处于`尾部位置`.
+ `Parameters`可以配合`threads `正常工作(见`Threads`). 
`parameterize`形式只为当前线程的计算调整`parameter `值, 这就避免了与其他线程的`race conditions`. 
