# racket_6

## 巨集,Macros

宏是一种语法形式(syntactic form),它有一个关联的`转化器`,可以将`原始形式`展开(expand)到`现有形式`中.
换句话说, `宏`是对`Racket`编译器的一种展开. 大多数`Racket/base`和`Racket`的语法形式实际上都是展开到一小部分核心结构的宏.

像许多语言一样, `Racket`提供了基于模式的宏, 使简单的转换容易实现, 使用起来也很可靠.
`Racket`还支持在`Racket`中实现的任意宏变换器, 或者在`Racket`的宏展开变体中实现.

本章提供了对`Racke`t宏的介绍, 但请看Fear of Macros,从另一个角度来介绍.

`Racket`包括对宏开发的额外支持.
`macro debugger`使有经验的程序员更容易调试他们的宏,也使新手更容易研究他们的行为,以及宏的行为.
还有`syntax/parse library`, 用于编写`宏`和`specifying`语法, 自动验证宏的使用并报告语法错误.

### 基于模式的宏

`基于模式的宏`将任何与模式相匹配的代码, 替换为使用原始语法中与模式的部分内容相匹配的展开.

#### define-syntax-rule

创建宏的最简单方法是使用`define-syntax-rule`.

```lisp
(define-syntax-rule pattern template)
```

作为可运行的例子,考虑一下`swap`宏, 它可以交换存储在两个变量中的值. 它可以用 `define-syntax-rule` 实现,如下所示.

> 这个宏是 `非Rackety` 的,因为它涉及到变量的副作用--但宏的意义在于让你添加一些其他语言设计者可能不允许的语法形式.

```lisp
(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))
```

`define-syntax-rule` 形式绑定了一个`macro`, 可以匹配单一`模式`.
`模式`必须以左小括号`(`开始, 后面是一个`标识符`,在本例中是`swap`.
在初始`标识符`之后,其他`标识符`是`macro pattern variables`,可以匹配宏使用时的任何内容.
因此,这个宏匹配任何`(swap form1 form2)`的形式.

> `宏模式变量`与匹配的`模式变量`类似.参见模式匹配.

`在define-syntax-rule`的模式之后是`模板`(template).
`模板`被用来代替匹配`模式`的形式, 只是模板中模式变量的每个实例, 都被替换成`宏`的`模式变量`匹配的`部分`.
例如,在

```lisp
(swap first last)
```

中,模式变量`x`匹配`first`, `y`匹配`last`, 所以展开为

```lisp
(let ([tmp first])
   (set! first last)
   (set! last tmp))
```

#### 词法定域,lexical scope

假设我们使用`swap`宏来交换名为`tmp`和`other`的变量.

```lisp
(let ([tmp 5] [other 6])
    (swap tmp other)
    (list tmp other))
```

上述表达式的结果应该是`(6 5)`.  理解这里`swap`使用的天真`展开`是

```lisp
(let ([tmp 5] [other 6])
    (let ([tmp tmp])
        (set! tmp other)
        (set! other tmp))
    (list tmp other))
```

其结果是`(5 6)`. 问题是,天真的展开方式混淆了`swap`上下文中的`tmp`和宏模板中的`tmp`.

`Racket`并没有为上述`swap`的使用产生天真的`展开`. 相反,它产生了

```lisp
(let ([tmp 5]
         [other 6])
    (let ([tmp_1 tmp])
        (set! tmp other)
        (set! other tmp_1))
    (list tmp other))
```

得到了正确的结果`(6 5)`. 同样地,在例子中

```lisp
(let ([set! 5]
         [other 6])
    (swap set! other)
    (list set! other))
```

的展开是

```lisp
(let ([set!_1 5] [other 6])
    (let ([tmp set!_1])
        (set! set!_1 other)
        (set! other tmp))
    (list set!_1 other))
```

这样,本地的`set!`绑定就不会干扰`宏模板`引入的赋值.

换句话说, `Racket`的`基于模式的宏`会自动保持`词法定域`, 所以宏实现者可以像对待`函数`和`函数调用`一样,对`宏`和宏使用中的`变量`引用进行推理.

#### 定义-语法和语法-规则

`define-syntax-rule`形式绑定了匹配`单一模式`的宏, 但`Racket`的宏系统支持匹配多个模式的`转换器`, 它们以同一`标识符`开头.
要编写这样的`宏`,程序员必须使用更通用的`define-syntax`形式和`syntax-rules` 转化器形式.

```lisp
(define-syntax id
    (syntax-rules (literal-id ...)
        [pattern template]
        ...))
```

`define-syntax-rule` 形式本身就是`宏`, 它展开为带有`synta-
rules`形式的`define-syntax`, 其中只包含一个`pattern`和`template`.

例如,假设我们想要一个`rotate`宏,它将`swap`推广为对`两个`或`三个`标识符起作用,因此

```lisp
(let ([red 1] [green 2] [blue 3])
  (rotate red green)      ; 对换, 2 1 3
  (rotate red green blue) ; 向左轮换, 1 3 2
  (list red green blue))
```

产生`(1 3 2)`.我们可以用语法规则来实现`rotate`.

```lisp
(define-syntax rotate
  (syntax-rules ()
    [(rotate a b) (swap a b)]
    [(rotate a b c) (begin
                     (swap a b)
                     (swap b c))]))
```

表达式 `(rotate red green)` 与匹配语法规则中的第一个`模式`,所展开为`(swap rd
 green)`.
表达式`(rotate red green blue)`与第二个`模式`相匹配, 所展开为`(begin s
wap red green) (swap green blue))`. 

#### 匹配序列

一个更好的`rotate`宏会允许任何数量的标识符,而不是只有两个或三个.
为了匹配使用任何数量的标识符的`rotate`,我们需要一个类似`Kleene star`的`模式`形式. 在`Racket`的宏模式中, `star`被写成`....`

为了用`...`实现`rotate`, 我们需要一个基本`case`来处理单个标识符,以及一个归纳`case`来处理多个标识符.

```lisp
(define-syntax rotate
  (syntax-rules ()
    [(rotate a) (void)]
    [(rotate a b c ...) (begin
                          (swap a b)
                          (rotate b c ...))]))
```

当像`c`这样紧跟`...`的模式变量出现在模式中, 那么它在模板中也必须紧跟`....`, 
此`模式变量`有效地匹配`零个`或`多个`形式的序列(sequence),它在`模板`中被同样的`序列`所取代.

到目前为止,两个版本的`rotate`都有点低效, 因为`对换`将持续移动第一个变量的值, 经过中间的所有变量, 直到到达最后一个.
更有效的`rotate`将直接把第一个值移到最后一个变量. 我们可以使用`...`模式来实现更有效的改进, 使用一个`辅助宏`.

```lisp
(define-syntax rotate
  (syntax-rules ()
    [(rotate a c ...)
     (shift-to (c ... a) (a c ...))]))
 
(define-syntax shift-to
  (syntax-rules ()
    [(shift-to (from0 from ...) (to0 to ...))
     (let ([tmp from0])
       (set! to from) ...
       (set! to0 tmp))]))
```

在`shift-to`宏中,模板中紧随在`(set! to from)`后面的`...`, 
使得`(set! to from)`表达式被重复多次, 以耗尽`to`和`from`序列中匹配的每个标识符. (`to`和`from`匹配到的数量必须相同,否则展开就会出错).

#### 标识符宏,Identifier Macros

鉴于我们的`宏`定义, `swap`或`rotate`标识符必须用在`开放括号`之后,否则会报告语法错误.

```lisp
> (+ swap 3)
eval:2:0: swap: bad syntax
  in: swap
```

`标识符宏`(identifier macro)是一个`模式匹配宏`,它在不含括号的情况下, 单独使用也可以发挥作用.
例如,我们可以将`val`定义为`标识符宏`, 它展开为`(get-va)
`,所以 `(+ val 3)`展开为`(+ (getv
al) 3)`.

```lisp
> (define-syntax val
    (lambda (stx)
      (syntax-case stx ()
        [val (identifier? (syntax val)) (syntax (get-val))])))
> (define-values (get-val put-val!)
    (let ([private-val 0])
      (values (lambda () private-val)
              (lambda (v) (set! private-val v)))))
> val
0
> (+ val 3)
3
```

`val`宏使用了`syntax-case`,它可以定义更强大的`宏`,这将在`Mixing Patterns and Expressions: syntax-case`部分进行解释.
现在我们只需知道,为了定义`宏`,在`lambda`中需使用`syntax-case`, 其`模板`必须用显式的`syntax`构造器(constructor)来包装.
最后,`syntax-case`可以在`模式`后面指定额外的`保护条件`(guard conditions).

我们的`val`宏使用了一个`identifier?` 条件来确保`val`不能和`括号`一起使用, 否则该`宏`引发一个语法错误.

```lisp
> (val)
eval:8:0: val: bad syntax
  in: (val)
```

#### set! 变换器,Transformers

使用上面的`val`宏,我们仍然必须调用`put-val!`来改变存储的值. 然而, 直接对`val`使用`set!`会更方便.
为了在`val`与`set!`一起使用时调用宏,我们用`make-set!-transformer`创建一个`assignment transformer`. 
我们还必须在`syntax-case`的`literal`列表中声明`set!` 是字面字符(literal).

```lisp
> (define-syntax val2
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [val2 (identifier? (syntax val2)) (syntax (get-val))]
         [(set! val2 e) (syntax (put-val! e))]))))
> val2
0
> (+ val2 3)
3
> (set! val2 10)
> val2
10
```

#### 生成宏的宏,Macro-Generating Macros

假设我们有许多像`val`和`val2`这样的标识符, 我们想把它们重定向到像`get-val`和`put-val`这样的`accessor`和`mutator`函数.
我们希望能够直接写:

```lisp
(define-get/put-id val get-val put-val!)
```

当然,我们可以把`define-get/put-id`实现为一个宏.

```lisp
> (define-syntax-rule (define-get/put-id id get put!)
    (define-syntax id
      (make-set!-transformer
       (lambda (stx)
         (syntax-case stx (set!)
           [id (identifier? (syntax id)) (syntax (get))]
           [(set! id e) (syntax (put! e))])))))
> (define-get/put-id val3 get-val put-val!)
> (set! val3 11)
> val3
11
```

`define-get/put-id`宏可以生成宏; 套娃.

###展开实例: 逐次调用函,
 Call-by-Reference Functions

我们可以使用`模式匹配宏`来为`Racket`添加一种`形式`: 定义`first-order call-by-reference functions`. 
当`call-by-reference functions`的主体`mutates`其`形式参数`时, 该`mutations`应用到在调用该函数时提供的`实际参数`.

例如,如果 `define-cbr` 与 `define` 相似, 只是它定义了`call-by-reference`, 那么

```lisp
(define-cbr (f a b)
  (swap a b))
 
(let ([x 1] [y 2])
  (f x y)
  (list x y))
```

产生`(2 1)`.

我们将通过让函数调用, 为参数提供`accessor`和`mutators`, 而不是直接提供参数值来实现 `call-by-reference` 函数.
特别是,对于上面的函数`f`, 我们将生成

```lisp
(define (do-f get-a get-b put-a! put-b!)
  (define-get/put-id a get-a put-a!)
  (define-get/put-id b get-b put-b!)
  (swap a b))
```

并将函数调用`(f x y)`重定向到

```lisp
(do-f (lambda () x)
      (lambda () y)
      (lambda (v) (set! x v))
      (lambda (v) (set! y v)))
```

那么显然,`define-cbr`是`生成宏`的宏, 它将`f`绑定到`宏`, 后者展开为`do-f`调用.
也就是说, `(define-cbr (f a b) (swap a b))` 需要生成定义

```lisp
(define-syntax f
  (syntax-rules ()
    [(id actual ...)
     (do-f (lambda () actual)
           ...
           (lambda (v)
             (set! actual v))
           ...)]))
```

同时, `define-cbr`需要使用`f`的主体来定义`do-f`, 这第二部分稍显复杂,
所以我们将其大部分推迟到`define-for-cbr`辅助模块中,这让我们写`define-cbr`足够容易.

```lisp
(define-syntax-rule (define-cbr (id arg ...) body)
  (begin
    (define-syntax id
      (syntax-rules ()
        [(id actual (... ...))
         (do-f (lambda () actual)
               (... ...)
               (lambda (v)
                 (set! actual v))
               (... ...))]))
    (define-for-cbr do-f (arg ...)
      () ; explained below...
      body)))
```

我们剩下的任务是定义`define-for-cbr`, 使它能够转换

```lisp
(define-for-cbr do-f (a b) () (swap a b))
```

到上面的函数定义`do-f`. 大部分的工作是为每个参数a和b生成一个define-get/put-id声明,并把它们放在主体之前.
通常情况下,对于模式和模板中的...来说,这是一项简单的任务,但这次有一个问题: 
我们需要生成get-a和put-a!以及get-b和put-b!这些名字,而模式语言没有提供基于现有标识符的合成标识符的方法.

事实证明,词法范围给了我们一个解决这个问题的方法.诀窍是对函数中的每个参数进行一次define-for-cbr的展开,
这就是为什么deine-for-cbr在参数列表后以一个明显无用的()开始.
我们需要跟踪到目前为止看到的所有参数,以及为每个参数生成的get和put名称,此外还有剩余的参数需要处理.
在我们处理完所有的标识符之后,那么我们就有了我们需要的所有名字.

下面是`define-for-cbr`的定义.

```lisp
(define-syntax define-for-cbr
  (syntax-rules ()
    [(define-for-cbr do-f (id0 id ...)
       (gens ...) body)
     (define-for-cbr do-f (id ...)
       (gens ... (id0 get put)) body)]
    [(define-for-cbr do-f ()
       ((id get put) ...) body)
     (define (do-f get ... put ...)
       (define-get/put-id id get put) ...
       body)]))
```

逐步的展开过程如下.

```lisp
(define-for-cbr do-f (a b)
  () (swap a b))
=> (define-for-cbr do-f (b)
     ([a get_1 put_1]) (swap a b))
=> (define-for-cbr do-f ()
     ([a get_1 put_1] [b get_2 put_2]) (swap a b))
=> (define (do-f get_1 get_2 put_1 put_2)
     (define-get/put-id a get_1 put_1)
     (define-get/put-id b get_2 put_2)
     (swap a b))
```

`get_1`, `get_2`, `put_1`, 和 `put_2`上的 `下标` 是 `macro expander` 插入的,以保持`lexical scope`,
因为由`define-for-cbr`的每次迭代产生的`get`不应该绑定由不同迭代产生的`get`. 换句话说,我们基本上是在欺骗`展开器`为我们生成新的名字,
但这个技术说明了, 带有自动词法范围的, 基于`模式`的宏的具有令人惊讶的力量.

最后一个表达式展开为

```lisp
(define (do-f get_1 get_2 put_1 put_2)
  (let ([tmp (get_1)])
    (put_1 (get_2))
    (put_2 tmp)))
```

它实现了`call-by-name`函数`f`.

综上所述, 我们只需用三个简短的, 基于模式的`宏`, 就可以在Racket中添加`call-by-reference`函数: 
`define-cbr`, `define-for-cbr`, 和 `define-get/put-id`.
