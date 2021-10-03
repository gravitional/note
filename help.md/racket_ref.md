# racket reference

## 计算模型,Evaluation Model

### 垃圾收集

+ 参见内存管理,了解与垃圾收集有关的功能.

在某时刻的程序状态,

```lisp
objects: (define <o1> (vector 10 20))
                (define <o2> (vector 0))
defined: (define x <o1>)
evaluate: (+ 1 x)
```

计算不能依赖于`<o2>`,因为它不是要计算的程序的一部分,而且它没有被程序可以访问的任何定义所引用.
这个对象被说成是`不可达的`. 因此,对象`<o2>`可以通过`垃圾收集`从程序状态中删除.

一些特殊的`复合数据`类型持有指向对象的`弱引用`.
垃圾收集器在确定哪些对象在剩余的计算中是可到达的时候,会特别处理这种`弱引用`.
如果对象只能通过`弱引用`到达,那么这个对象可以被回收, `弱引用`被一个不同的值(通常是`#f`)取代.

作为特例, `fixnum`总是被垃圾收集器认为是可到达的. 许多其他的值由于它们的实现和使用方式,总是可以达到的.
在`Latin-1`范围内的字符总是可达的, 因为`equal?` Latin-1字符总是`eq?`, 而且所有的`Latin-1`字符都被一个内部模块所引用.
同样,`null`,`#t`, `#f`, `eof` 和 `#<void>` 也总是可达的. 当`quote`表达式本身可达时, 由`quote`产生的值仍然可达.

### 程序应用和局部变量,Procedure Applications and Local Variables

给出

    f(x) = x + 10

代数学生会将`f(7)`简化如下.

    f(7) = 7 + 10 = 17

这个简化的关键步骤是将定义的函数`f`的主体, 用实际值`7`替换每个`x`.
`Racket` 过程应用(Procedure Applications)的工作方式与此基本相同.
过程是一个对象,所以计算`(f 7)`要从一个变量的查找开始.

```lisp
objects: (define <p1> (lambda (x) (+ x 10)))
defined: (define f <p1>)
evaluate: (f 7)

→objects:(define <p1> (lambda (x) (+ x 10)))
defined:(define f <p1>)
evaluate:(<p1> 7)
```

然而,与代数不同的是, 与`过程`参数变量相关的值, 可以在过程的主体中通过使用`set!`来改变, 如例子中的`(lambda (x) (begin (set! x 3) x))`.
由于与参数变量`x`相关的值应该是可以改变的, 所以我们不能在第一次应用过程时直接用这个值来代替`x`.

>我们不使用 `参数变量` (arameter variable) 这一术语, 来指代与函数一起声明的`参数变量名`(argument variable names).
>这样可以避免与`parameters`的混淆.

取而代之的是,在每次`application`中为变量创建新的`位置`(location).
参数`value`被放置在这个`位置`上, 而`程序主体`中的每个`变量实例`都被替换成新的`位置`.

```lisp
objects: (define <p1> (lambda (x) (+ x 10)))
defined: (define f <p1>)
evaluate: (<p1> 7)

→objects:(define <p1> (lambda (x) (+ x 10)))
defined:    (define f <p1>)
                    (define xloc 7)
evaluate:(+ xloc 10)

→objects: (define <p1> (lambda (x) (+ x 10)))
defined: (define f <p1>)
                    (define xloc 7)
evaluate: (+ 7 10)

→objects: (define <p1> (lambda (x) (+ x 10)))
    defined: (define f <p1>)
                    (define xloc 7)
    evaluate:17
```

`位置`和`顶层变量`(top-level variable)是一样的, 但是当`位置`被生成时,它(在概念上)使用了之前没有被使用过的名字, 并且不能再次`生成`或`直接访问`.

以这种方式生成`位置`意味着`set!`对局部变量(包括`参数变量`)的计算方式,
与对`顶层变量`的计算方式相同, 因为在`set!`形式被计算时, `局部变量`总是被替换成`位置`.

```lisp
        objects: (define <p1> (lambda (x) (begin (set! x 3) x)))
        defined: (define f <p1>)
        evaluate: (f 7)

→ objects: (define <p1> (lambda (x) (begin (set! x 3) x)))
    defined: (define f <p1>)
    evaluate: (<p1> 7)

→objects: (define <p1> (lambda (x) (begin (set! x 3) x)))
    defined: (define f <p1>)
                    (define xloc 7)
    evaluate: (begin (set! xloc 3) xloc)

→objects: (define <p1> (lambda (x) (begin (set! x 3) x)))
    defined:  (define f <p1>)
                        (define xloc 3)
    evaluate: (begin (void) xloc)

→objects: (define <p1> (lambda (x) (begin (set! x 3) x)))
    defined: (define f <p1>)
                        (define xloc 3)
    evaluate:  xloc

→objects: (define <p1> (lambda (x) (begin (set! x 3) x)))
    defined: (define f <p1>)
                        (define xloc 3)
    evaluate: 3
```

程序`作用`时, `位置生成`和 替换步骤要求`参数`是一个`值`.
因此,在`((lambda (x) (+ x 10)) (+ 1 2))`中, `(+ 1 2)` 子表达式必须能被化简到值`3`, 然后`3`可以被放置到`x`的位置.
换句话说, Racket 是`值调用` 语言. (call-by-value language)

对`局部变量`形式的计算,如 `(let ([x (+ 1 2)]) expr)`,与`procedure`调用是一样的.
在`(+1 2)`产生`值`后, 它被存储到新的`location`, `location`将取代`expr`中`x`的每个实例.

在这里, `location` 表示局域变量的意思, 并不是指通常的`位置`.

### 变量和位置,Variables and Locations

`变量`是`值`的`占位符`, 初始程序中的`表达式`指向`变量`.

`顶层变量`既是`变量`又是`位置`.  任何其他`变量`在运行时总是被`位置`取代;
因此, 表达式的计算只涉及`位置`. 单一的`局部变量`(即`非顶层`,`非模块级`的变量), 如`参数变量`, 在不同的`applications`中可以对应不同的`位置`.
例如,在程序

```lisp
(define y (+ (let ([x 5]) x) 6))
```

中, `y`和`x`都是变量. `y`变量是`顶层变量`, 而`x`是局部变量.
当这段代码被计算时,为`x`创建了存放数值`5`的`位置`, 同时也为`y`创建了存放数值`11`的位置.

在计算过程中, 用`位置`替换`变量`实现了`Racket`的`文法作用域`(lexical scoping).
例如,当参数变量`x`被位置`xloc`替换时, 整个`过程`的`主体`中的`x`都将被替换, 包括任何嵌套的 `lambda` 形式.
因此, 将来对该`变量`的`引用`总是访问同一个`位置`.

为了实现用`xloc`替换掉`x`, 所有的`变量绑定`都必须使用不同的名字, 从而保证不会替换掉实际上不同的变量`x`.
确保这种区别是`宏扩展器`(macro expander)的工作之一; 见 Syntax Model.

## The Reader

### Delimiters and Dispatch

[The Reader](https://docs.racket-lang.org/reference/reader.html#%28part._parse-vector%29)

除了`空格`和`BOM`字符外, 以下字符也是`分隔符`.

        ( ) [ ] { } " , ' ` ;

以任何其他字符开始的, 被定界的`序列`通常被解析为`符号`(symbol), `数字`或`extflonum`, 但有几个非定界字符起着特殊作用.

+ `#`作为划线序列中的初始字符具有特殊意义;其意义取决于后面的字符;见下文.
+ `|`开始一个子序列,该子序列将被逐字包含在划定的序列中(也就是说,它们从不被视为分界符,当启用大小写不敏感时,它们不会被折算);
该子序列由另一个`|`终止,并且初始和终止的`|`都不是该子序列的一部分.
+ `\` 在一个`|`对之外,导致下面的字符被逐字包括在一个限定的序列中.
更确切地说,在跳过`空白`和`#uFEFF` BOM字符后, `reader` 根据输入流中的下一个或多个字符进行分配,如下所示.
