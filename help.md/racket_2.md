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
