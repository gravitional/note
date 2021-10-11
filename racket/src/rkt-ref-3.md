# Syntactic Forms

[Syntactic Forms](https://docs.racket-lang.org/reference/for.html)

## 迭代和解析: for, for/list, ...

### Iteration and Comprehension Forms

    (for (for-clause ...) body-or-break ... body)

            for-clause     =     [id seq-expr]
                                      |     [(id ...) seq-expr]
                                      |     #:when guard-expr
                                      |     #:unless guard-expr
                                      |     break-clause

        break-clause  =     #:break guard-expr
                                    |     #:final guard-expr

    body-or-break =     body
                                    |     break-clause

        seq-expr     :     sequence?

`迭代`(iteratively)地计算`body`s. `for-clauses` 引入`绑定`, 其范围包括`body`, 并决定`body`被计算的次数.
无论是 `for-clause` 还是 `body`s 中的 `break-clause` 都会停止进一步`迭代`.

在简单的情况下, 每个`for-clause`都有其前两种形式之一, 其中 `[id seq-expr]` 是 `[(id) seq-expr]` 的缩写.
在这种简单的情况下, `seq-expr` 从左到右被计算, 并且每个都必须产生`序列值`(见`序列`).

`for`形式通过从每个序列中抽取一个`元素`进行`迭代`; 如果任何一个`序列`是`空的`, 那么`迭代`停止, `#<void>`是 `for` 表达式的结果.
否则就为每个`id` 创建一个`location`来保存每个`元素`的`值`;
由 `seq-expr` 产生的`序列`, 必须为每个`迭代`返回与对应 `id`s 相同数量的值. 特别是对于 `[(id ...) seq-expr]` 这种形式.

然后, 这些 `id`s 被绑定在`body`中, `body`被计算, 其结果被忽略. `迭代`继续进行, 使用每个`序列`中的下一个`元素`, 对每个`id`使用新的`location`.

带有零个 `for-clause` 的 `for` 形式, 等同于单个 `for-clause`,
它将一个`未引用`(unreferenced)的`id`绑定到由`单个元素`组成的`序列`上.
所有的 `id`s 必须是不同的, 由 `bound-identifier=?` 判断.

如果任何 `for-clause` 的形式是 `#:when guard-expr`, 那么只有前面的子句(不包含#:when或#:unless)才能像上面那样确定`迭代`,
然后 `body` 相当于, 使用其余的 `for-clause` 被包装为

```lisp
    (when guard-expr
      (for (for-clause ...) body ...+))
```

形式为 `#:unless guard-expr` 的 `for-clause` 对应于相同的变换, 用 `unless` 来代替 `when`.

`#:break guard-expr` 子句与 `#:unless guard-expr` 子句相似, 但是当 `#:break` 避免了对 `body`s 的计算时, 它也有效地结束了 `for` 形式中的`所有序列`.

`#:final guard-expr`子句类似于`#:break guard-expr`, 但是它`不是立即结束`序列并跳过 `body`s,
而是允许从后面的每个`序列`中最多取出`一个`元素, 并最多对下面的`body`s进行`一次`计算.

在 `body`s 中, 除了停止迭代和阻止后来的`body`计算外, `#:break guard-expr` 或 `#:final guard-expr` 子句开始新的`内部定义`上下文(context).

在`list`和`stream`序列的情况下, `for` 形式本身并不保持每个元素的`可及性`(reachable).
如果由`seq-expr`产生的`列表`或`stream`在其他地方无法被引用, 并且如果 `for` body 不再引用某个列表元素的`id`, 那么该元素将被`垃圾回收`(gabage collection).
`make-do-sequence` 序列`构造函数`支持额外的`序列`, 这些`序列` 在这方面表现得像`list`和`stream`.

如果`seq-expr`是一个带`引号`的字面`列表`, `向量`, `精确的整数`, `字符串`, `字节字符串`, `不可变的哈希`(immutable hash),
或者展开到这样的`字面`, 那么可以认为它将被`序列转化器`(sequence transformer)处理, 比如 `in-list`;
除非 `seq-expr` 的 `'for:no-implicit-optimization` 语法属性为`真值`; 在大多数情况下这可以提高性能.

```lisp
> (for ([i '(1 2 3)]
        [j "abc"]
        #:when (odd? i)
        [k #(#t #f)])
    (display (list i j k)))
(1 a #t)(1 a #f)(3 c #t)(3 c #f)

> (for ([(i j) #hash(("a" . 1) ("b" . 20))])
    (display (list i j)))
(a 1)(b 20)

> (for ([i '(1 2 3)]
        [j "abc"]
        #:break (not (odd? i))
        [k #(#t #f)])
    (display (list i j k)))
(1 a #t)(1 a #f)

> (for ([i '(1 2 3)]
        [j "abc"]
        #:final (not (odd? i))
        [k #(#t #f)])
    (display (list i j k)))
(1 a #t)(1 a #f)(2 b #t)

> (for ([i '(1 2 3)]
        [j "abc"]
        [k #(#t #f)])
    #:break (not (or (odd? i) k))
    (display (list i j k)))
(1 a #t)

> (for ()
    (display "here"))
here

> (for ([i '()])
    (error "doesn't get here"))
    无输出
```

在软件包base的6.7.0.4版本中有所改变. 增加了对可选第二结果的支持.
在7.8.0.11版本中修改: 增加了对隐式优化的支持.

+ inlist

    (in-list lst) → stream?
    lst : list?

返回`序列`(也是一个`流`), 相当于直接将`lst`作为`序列`使用.

>关于将列表作为序列使用的信息, 请参见对和列表.

当`列表`直接出现在 `for`子句中时, `in-list` 作用可以为`列表迭代`提供更好的性能.
关于`迭代过程`中列表元素的`可及性`, 请参见 `for`.
例子.

```lisp
> (for/list ([x (in-list '(3 1 4))])
    `(,x ,(* x x)))
'((3 9) (1 1) (4 16))
```

在软件包base的6.7.0.4版本中有所改变. 改进了`for`中列表的元素`可达性`保证.

+ for/list

    (for/list (for-clause ...) body-or-break ... body)              syntax

像 `for` 一样进行迭代, 但 `bodys` 中的最后一个`表达式`必须产生`单个值`, `for/list`表达式的结果是按顺序排列的`值列表`. 
当由于 `#:when` 或 `#:except` 子句而跳过一个`body`的计算时, `结果列表`不包括相应的元素.
例子.

```lisp
> (for/list ([i '(1 2 3)]
             [j "abc"]
             #:when (odd? i)
             [k #(#t #f)])
    (list i j k))
'((1 #\a #t) (1 #\a #f) (3 #\c #t) (3 #\c #f))
> (for/list ([i '(1 2 3)]
             [j "abc"]
             #:break (not (odd? i))
             [k #(#t #f)])
    (list i j k))
'((1 #\a #t) (1 #\a #f))
> (for/list () 'any)
'(any)
> (for/list ([i '()])
    (error "doesn't get here"))
'()
```
