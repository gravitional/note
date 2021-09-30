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

`define-syntax-rule`形式绑定了匹配`单一模式`的宏, 但Racket的宏系统支持匹配多个以同一标识符开始的模式的转换器.
要编写这样的宏,程序员必须使用更通用的定义-语法形式和语法-规则转化器形式.

        (define-syntax id
          (syntax-rules (literal-id ...)
            [模式模板]
            ...))

            define-syntax-rule形式本身就是一个宏,它可以扩展为带有语法规则形式的define-syntax,其中只包含一个模式和模板.

例如,假设我们想要一个rotate宏,它将swap泛化为对两个或三个标识符起作用,因此

    (让([红1] [绿2] [蓝3])
      (rotate red green) ; swaps
      (rotate red green blue) ; 向左旋转
      (list red green blue))

产生(1 3 2).我们可以用语法规则来实现rotate.

    (define-syntax rotate
      (语法规则()
        [(rotate a b) (swap a b)]
        [(旋转a b c)(开始
                         (交换a b)
                         (交换b c))])

表达式(旋转红绿)与语法规则中的第一个模式相匹配,所以它扩展为(互换红绿).表达式(旋转红绿蓝)与第二个模式相匹配,所以它扩展为(开始(交换红绿)(交换绿蓝)). 