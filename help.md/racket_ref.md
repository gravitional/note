# racket reference

## 计算模型,Evaluation Model

`Racket`计算可以被看作是对`表达式`的化简以获得`值`. 例如, 就像小学生化简

    1 + 1 = 2

`Racket` 计算化简

    (+ 1 1) → 2

箭头`→`取代了更传统的`=`, 以强调计算是朝着更简单的表达式的特定方向进行的.
特别地, `值`(value), 如数字`2`, 是无法通过计算继续化简的 `表达式`.

### 子表达式计算和`continuation`,Sub-expression Evaluation and Continuations

有些`简化`需要一个以上的步骤. 比如说.

    (- 4 (+ 1 1)) → (- 4 2) → 2

一个不是`值`的`表达式`总是可以被分割成两部分:
`redex`("可简化表达式"), 这是单步简化中可以改变的部分, 例如`(+ 1 1)`.
以及 `continuation`, 也就是包裹 `redex` 的计算环境.
在`(- 4 (+ 1 1))`中, redex 是`(+ 1 1)`, `continuation`是`(- 4 [])`,
其中`[]`代替`redex`的位置, 它要被简化. 也就是说, `continuation` 说的是在`redex`被化简到`值`之后如何 `继续` 计算.

在某些`表达式`被计算之前, 它们的一些或全部`子表达式`必须被计算.
例如, 在`application (- 4 (+ 1 1))` 中, 在 `子表达式(+ 1 1)`被化简之前, `-`的应用不能被化简.
因此, 每个`syntactic  形式`的规范都指定了如何计算其`子表达式`, 然后如何将结果结合起来以化简该形式.

`表达式`的 `dynamic extent` 是该表达式的计算步骤序列, 过程中表达式包含 `redex`.

### 尾部位置,Tail Position

设`expr2` 包围着 `expr1`, 当`expr1` 被化简成 `redex` 之后,  
如果`expr1`的`continuation`, 与`expr2` 的`continuation`相同,
那么就称作: 表达式`expr1` 处于 `expr2` 的尾部位置.
直白点说, 就是该做的化简都差不多了, `expr1`可以跳出这层结构.

例如, `(+1 1)` 表达式相对于 `(- 4 (+1 1))` 来说不处于`尾部位置`.
为了说明这点, 我们使用符号`C[... expr]`来表示, 在某个`continuation C` 中用 `expr` 代替 `[]` 得到的结果.

    C[(- 4 (+ 1 1)) ] → C[(- 4 2)]

在这种情况下, 化简 `(+1 1)`之后, 接下来的 `continuation` 是 `C[(- 4 [])]` , 
而外层的`continuation` 仅仅是 `C`,  内外层的 `continuation` 不相同, 所以说 `(+1 1)` 不在尾部位置. 

相反, 我们可以说 `(+ 1 1)` 处于`(if (zero? 0) (+ 1 1) 3) ` 的尾部位置, 因为对于任何`continuation C`.

    C[(if (zero? 0) (+ 1 1) 3)] → C[(if #t (+ 1 1) 3)] → C[(+ 1 1)]

内外层的`continuation` 相同, 上述规定的要求得到了满足.  
这个化简序列的步骤是由`if`的定义驱动的, 它们不依赖于`continuation C` 的具体形式.
事实上, `if`形式的 `then` 分支相对于`if`形式总是处于`尾部位置`.
由于`if` 和`#f`的类似化简规则, `if` 形式的 `else` 分支也在 `尾部位置`.

`尾部位置`规范提供了关于计算的`渐进空间消耗`(asymptotic space consumption )的保证.
一般来说, 尾部位置的指定伴随着每个`syntactic  形式`的描述, 比如 `if`.

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
确保这种区别是`宏展开器`(macro expander)的工作之一; 见 Syntax Model.

## 语法模型,Syntax Model

`Racket`程序的语法是由以下部分定义的;

+ `read` pass, 将`字符流`处理成`语法对象`; 以及
+ `expand` pass, 对语法对象进行处理, 产生完全解析的`语法对象`.

关于`read`通道的细节, 请参见 The Reader.  源代码通常是在 `read-syntax ` 模式下读取的, 它产生`syntax object`.

`expand`通道递归地处理`语法对象`, 产生完整的程序解析.
语法对象中的`绑定信息`驱动`展开`过程, 当`展开`过程遇到`绑定`形式时, 它用新的`绑定`信息展开子表达式的`语法对象`.

### 标识符, 绑定和作用域

`标识符`(identifier )是源代码中的`实体`(entity).
解析 (即展开) `Racket` 程序会发现, 一些`标识符`对应于`变量`(variables), 一些指的是`语法形式`(如 `lambda`, 它是函数的语法形式),
一些指的是用于宏扩展的`转化器`(transformers), 还有一些被`quoted`来产生 `symbols` 或 `syntax objects`.
当`标识符A`被解析为`变量`或`语法形式`, 而`标识符B`被解析为对`A`的`引用`时, `标识符A`就会绑定`标识符B`(即binding); 后者`被绑定`(bound).

例如, 作为`源码`的一个片段, 该文本

```lisp
(let ([x 5]) x)
```

包括两个`标识符`: `let` 和`x`(出现两次). 当这个源码按照`let` 通常的含义被解析时, 第一个`x`绑定了第二个`x`.

`绑定`(bindings )和`引用`(references )是通过`范围集`(scope sets)确定的.
`范围`(scope)对应于程序的`区域`, 这个区域要么是`源代码`的一部分, 要么是通过对`源代码`的阐述而合成的.
嵌套绑定的上下文(如嵌套的`函数`) 创造嵌套的`作用域`, 而`宏展开`创造了以更复杂方式重叠的`作用域`.
从概念上讲, 每个`作用域`都由唯一的`标记`(token)表示, 但这个标记是不能直接访问的.
相反, 每个`作用域`由一个值来表示, 这个值对于程序的表示是内部的.
each scope is represented by a `value` that is internal to the representation of a program

`form`是程序的`片段`, 如`标识符`或`函数调用`.
`form`被表示为`语法对象`, 每个`语法对象`都有一个相关的`作用域集`(即`scope set`).
在上面的例子中, `x` 的表示包括与`let`形式对应的`scope`.

当`form`解析为特定`标识符`的绑定时, 解析会更新一个`全局 Table`,
该`Table`将`标识符`的`符号`(symbol)和`作用域集`的组合映射到其含义上:
也就是`变量`, `语法形式`(syntactic form)或`转化器`.
当`引用`(reference)的`symbol `和`标识符`的`symbol`相同,
且`引用的 scope set`是`绑定的 scope set`的`超集`(superset)时, `标识符`就指向这个特定的`绑定`.

对于给定的`标识符`, 可能有多个`绑定`的`scope set` 是 `标识符` scope set 的子集;
在这种情况下,` 标识符`指向的绑定, 是具有最大`scope set`的绑定, 即后者是所有其他`scope set`的超集;
如果不存在这样的绑定, `引用`(此次调用)就是模糊的(如果它被解析为表达式, 会触发一个语法错误).
作用域大的`绑定`会`掩盖`(shadow)作用域小的绑定, 如果他们具有相同的`symbol `.

注意: 一般越内层的变量/绑定, 它的作用域越大, 跟源代码的直观形式是刚好相反的.
例如, 在

```lisp
(let ([x 5]) x)
```

中, `let`对应于通常的语法形式, `let`的解析为`x`的`绑定`引入了新的`作用域`.
由于第二个`x`作为`let`主体的一部分, 接收到了`let`的作用域, 第一个`x` binds 第二个`x`.
更复杂的情形为,

```lisp
(let ([x 5])
    (let ([x 6])
        x))
```

内部 `let` 为第二个`x` 创建了第二个作用域, 所以它的作用域集是第一个 `x` 作用域集的超集
--这意味着第二个 `x` 的绑定会掩盖第一个 `x` 的绑定, 从而第三个 `x` 指向的是, 第二个`x`创建的绑定.

`顶层绑定`是在顶层的`定义`确立的`绑定`; `模块绑定`是模块中的`定义`确立的`绑定`;
所有其他绑定是`本地绑定`(local bindings). 在模块内, 不允许引用`顶层绑定`. 没有任何 `绑定`的`标识符` is `unbound`.

在整个文档中, `标识符`的命名表明了它们被解析的方式.
像 `lambda这` 样的超链接`标识符`, 表示它是对`syntactic  形式`或`变量`的`引用`.
像`x`这样的普通标识符是一个`变量`, 或对某个非指定`顶层变量`的引用.

每个`绑定`都有阶段(phase level), 在这个阶段可以引用此`绑定`.
`相位`通常对应于一个`整数`(但特殊`label phase level`并不对应于整数).
`phase level 0`对应于`enclosing 模块`的`run time`(或顶层表达式的`run time`).
处于 `phase level 0`的绑定构成`base environment`.

`阶段1` 对应于`enclosing 模块`(或顶层表达式)被`展开`的时刻; `阶段1` 中的`绑定`构成`transformer environment`.
`阶段-1` 是另一个模块`B`的`run time`, `enclosing 模块`在`阶段1`(相对于 importing 模块)被导入模块`B`使用; (也就是父模块)
`阶段-1` 中的绑定构成`template environment`.
`label phase level`不对应任何`execution time`; 它被用来跟踪`bindings `(例如, 文档中出现的`标识符`), 但它不意味着执行时的`依赖`.

`标识符`在不同的`阶段` 可以具有不同的`绑定`.
更确切地说, 与一个`form`相关的`scope set`在不同的`阶段`可以是不同的;

`top-level`或`模块上下文`在每个`阶段`都意味着不同的`作用域`,
而在所有阶段, 来自`宏展开`或其他`语法形式`的`作用域`都被添加到`form`的`作用域集`.
每个`绑定`和`引用`的`上下文`, 决定了`phase level`, 后者又决定了前者的`scope set`.

在软件包`base`的6.3版本中进行了修改. 改变了本地绑定, 使其具有特定的`阶段`, 就像`顶级绑定`和`模块绑定`一样.

### 语法对象,Syntax Objects

`语法对象`将较简单的`Racket值`(如`symbol `或`pair`)与`lexical 信息`, ` source-location`信息, `syntax properties`和`tamper status`相结合.
`语法对象`的`lexical information`包括一组`范围集`, 每个`阶段`都有一个.
特别是, `标识符`被表示为包含`symbol`的语法对象, 它的`lexical 信息`可以与`全局绑定表`相结合, 以确定它在每个`阶段`的`binding`(如果有的话).

例如, `car `标识符可能有`lexical 信息`, 将其指定为`racket/base`语言中的`car`(即内置`car`).
同样地, `lambda` 标识符的`lexical 信息`可能表明它代表一个`procedure `形式.
其他`标识符`的`lexical 信息`可能表明它引用了`顶层变量`.

当`语法对象`代表一个比`标识符`或`简单常量`更复杂的`表达式`时, 它的内部组件可以被提取出来.
即使对于提取出的`标识符`, 关于`绑定`的详细信息也大多是间接可用的;
两个标识符可以进行比较, 以确定它们是否引用了相同的`绑定`(即`free-identifier=?`),
或者`标识符`是否具有相同的`scope set`, 以便如果`标识符A`处于`binding `位置,
`标识符B`处于`expression `位置, 则`A`应该`bind`标识符`B`(bound-identifier=?)

例如, 当程序,

```lisp
(let ([x 5]) (+ x 6))
```

被表示为`语法对象`, 那么就可以为两个`x`提取两个`语法对象`.
`free-identifier=?` 和 `bound-identifier=?` 谓词都将表明这两个`x`是相同的.
相反, `let` 标识符对任何一个`x`都不是` free-identifier=?`或`bound-identifier=?`.

`语法对象`中的`lexical 信息`是独立于`语法对象`的其他部分的, 它可以和任意的其他`Racket值`一起被复制到新的语法对象中.
因此, `语法对象`中的标识符`绑定信息`是由标识符的`符号名称`以及标识符的`lexical信息`共同决定的的;
同一个问题, 如果有相同的`lexical 信息`, 但`base value` 不同, 就会产生不同的答案.

例如, 将上面程序中`let`的 `lexical 信息` 结合到`'x`, 不会产生与任一`x`相同绑定的标识符(通过 `free-identifier=?` 判断),
因为它没有出现在`x`绑定的范围内.
相反, 将`6`的 `lexical 信息` 与`'x`结合起来, 会产生一个和两个`x`具有相同绑定的标识符( 通过`bound-identifier=?` ).

`Quote-syntax`形式, 连接了程序的`计算`(evaluation)和程序的`表示`(representation ).
具体来说, `(quote-syntax datum #:local)`产生一个`语法对象`, 它保留了 `datum` 的所有`lexical 信息`,
当`datum ` 作为 `quote-syntax` 形式的一部分被解析(parsed )时.

请注意, `(quote-syntax datum)`形式是类似的, 但它从`datum`的`scope sets`中删除了某些`scopes`;
更多信息见`quote-syntax`.

### 展开,Expansion (Parsing)

[Expansion (Parsing)](https://docs.racket-lang.org/reference/syntax-model.html#%28part._fully-expanded%29)

`Expansion `以递归方式处理特定`阶段`的`语法对象`, 从`阶段0`开始.
`语法对象`的`lexical 信息`的`Binding`推动了展开过程, 并导致引入新`绑定`到`子表达式`的`lexical 信息`中.
在某些情况下, `子表达式`被展开到比`enclosing  表达式`更深的`阶段`(具有更大的`阶段数`).

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

## 数据类型,Datatypes

### Equality

`Equality `是指两个 values  是否 `相同` 的概念. `Racket`默认支持几种不同的`equality `, 但在大多数情况下, `equal?`是首选. 

    (equal? v1 v2) → boolean?
        v1 : any/c
        v2 : any/c

当且仅当它们是`eqv?`时, 两个值是`equal?`的, 除非对特定的`数据类型`另有规定. 

对`equal?`有进一步说明的数据类型包括 `字符串`, `字节字符串`, 
`对`, `可变对`, `向量`, `盒子`, `哈希表` 和 `可检查结构`(inspectable structures). 

对于后六种情况, `equality`是递归定义的; 
如果`v1`和`v2`都包含引用循环, 那么当值的无限展开是`equal`的, 它们就是`equal`的. 
另见 gen:equal+hash 和 prop:impersonator-of . 
例子. 

```lisp
> (equal? 'yes 'yes)
#t
> (equal? 'yes 'no)
#f
> (equal? (* 6 7) 42)
#t
> (equal? (expt 2 100) (expt 2 100))
#t
> (equal? 2 2.0)
#f
> (let ([v (mcons 1 2)]) (equal? v v))
#t
> (equal? (mcons 1 2) (mcons 1 2))
#t
> (equal? (integer->char 955) (integer->char 955))
#t
> (equal? (make-string 3 #\z) (make-string 3 #\z))
#t
> (equal? #t #t)
#t
```

(eqv? v1 v2) → boolean?

  v1 : any/c
  v2 : any/c

当且仅当两个值是`eq?`时, 它们就是`eqv?`, 除非对特定`数据类型`另有规定. 

`数字`(number)和`字符`(character)数据类型是`eqv?`与`eq?`不同的数据类型. 

当两个数字具有相同的`精确性`, `精度`, 并且都是相等且`非零`, 
或者都是`+0.0`, 都是`+0.0f0`, 都是`-0.0`, 都是`-0.0f0`, 都是`+nan.0`, 或者都是`+nan.f`
在复数的情况下分别考虑`实部`和`虚部`, 从而返回它们的`eqv?`结果.

当两个字符的`char->integer`结果相等时, 它们`eqv?`.

一般来说, `eqv?`与`equal?`相同, 只是`eqv?`不能递归比较复合数据类型(如`lists `和`structs`)的内容, 
也不能由用户定义的数据类型来定制. 我们不鼓励使用`eqv?`, 而是使用`equal?`. 
例子. 

```lisp
> (eqv? 'yes 'yes)
#t
> (eqv? 'yes 'no)
#f
> (eqv? (* 6 7) 42)
#t
> (eqv? (expt 2 100) (expt 2 100))
#t
> (eqv? 2 2.0)
#f
> (let ([v (mcons 1 2)]) (eqv? v v))
#t
> (eqv? (mcons 1 2) (mcons 1 2))
#f
> (eqv? (integer->char 955) (integer->char 955))
#t
> (eqv? (make-string 3 #\z) (make-string 3 #\z))
#f
> (eqv? #t #t)
#t
```

    (eq? v1 v2) → boolean?
        v1 : any/c
        v2 : any/c

如果`v1`和`v2`指向同一个对象, 返回`#t`, 否则返回`#f`. 
数字作为一个特例, 对于`eq?`, 两个`=`的`fixnums`也是相同的. 
参见 Object Identity and Comparisons.
例子. 

```lisp
> (eq? 'yes 'yes)
#t
> (eq? 'yes 'no)
#f
> (eq? (* 6 7) 42)
#t
> (eq? (expt 2 100) (expt 2 100))
#f
> (eq? 2 2.0)
#f
> (let ([v (mcons 1 2)]) (eq? v v))
#t
> (eq? (mcons 1 2) (mcons 1 2))
#f
> (eq? (integer->char 955) (integer->char 955))
#t
> (eq? (make-string 3 #\z) (make-string 3 #\z))
#f
> (eq? #t #t)
#t
```

    (equal?/recur v1 v2 recur-proc) → boolean?
        v1 : any/c
        v2 : any/c
        recur-proc : (any/c any/c -> any/c)

与`equal?`相似,但使用`recur-proc`进行递归比较(这意味着不会自动处理引用循环).
来自`recur-proc`的非`#f`结果在被`equal?/recur`返回之前被转换为`#t`.
例子.

```lisp
> (equal?/recur 1 1 (lambda (a b) #f))
#t
> (equal?/recur '(1) '(1) (lambda (a b) #f))
#f
> (equal?/recur '#(1 1 1) '#(1 1.2 3/4)
                (lambda (a b) (<= (abs (- a b)) 0.25)))
#t
```
