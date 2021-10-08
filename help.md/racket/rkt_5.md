# 用户自定义数据类型

新的`数据类型`通常是用 `struct` 形式创建的, 这也是本章的主题.
基于`class`的`对象`(object)系统 (我们在 Classes and Objects 中会提到) 为创建新的`数据类型`提供了另一种机制,
但即使是`类`和`对象`也是以`结构类型`(structure types)的方式实现.

## 简单的结构类型: struct

>定义结构类型: The Racket Reference中的struct也记录了struct.

初步来讲, `struct`的语法是

    (struct-id (field-id ...))

例子.

```lisp
(struct posn (x y))
```

`struct`形式`绑定`了 `struct-id`, 并自动绑定一些其他的`标识符`, 后者由 `struct-id` 和`field-id`s 组合成.

+ `struct-id` : 一个`构造函数`(constructor), 它接受与 `field-id`s 数量相同的参数, 并返回`结构类型`的实例.
例子.

    ```lisp
    > (posn 1 2)
    #<posn>
    ```

+ `struct-id?` : `谓词`(predicate)函数, 接收单个参数, 如果是`结构类型`的实例, 则返回`#t`, 否则返回 `#f`.
例子.

    ```lisp
    > (posn? 3)
    #f
    > (posn? (posn 1 2))
    #t
    ```

+ `struct-id-field-id` : `访问器`(accessor); 对于每个`field-id`, 可以从`结构类型`的实例中提取相应`字段`(field)的值.
示例.

    ```lisp
    > (posn-x (posn 1 2))
    1
    > (posn-y (posn 1 2))
    2
    ```

+ `struct:struct-id` ; `structure type descriptor`(结构类型描述符),
它是代表`结构类型`的`first-class value`(一等值, 与`#:super`一起使用, 后面将讨论`More Structure Type Options`).

`struct`形式对`结构类型`实例中的`field`所能出现的值的`kind`没有任何限制.
例如, `(posn "apple" #f)` 产生一个 `posn` 的实例, 尽管 `"apple"` 和 `#f` 不是 `posn` 实例的有效坐标.
强制执行对`字段值`的`约束`, 比如要求它们是`数字`, 通常是`契约`(contract)的工作, 在后面的`Contracts`中会讨论.

## 复制和更新

`struct-copy` 形式克隆一个`结构`, 并且可以选择更新`克隆`中的指定`字段`.
这个`过程`有时被称为`functional update`, 因为其结果是具有更新后`字段值`的`结构`, 但是`原始结构`没有被修改.

    (struct-copy struct-id struct-expr [field-id expr] ...)

出现在 `struct-copy` 之后的 `struct-id` 必须是由 `struct` 绑定的 `结构类型` 名称(即不能直接作为`表达式`使用的`名称`).
`struct-expr` 必须产生`结构类型`的`实例`.
结果是一个新的`结构类型`实例, 它与旧的`结构类型`一样, 只是每个 `field-id` 所指示的字段得到了相应 `expr` 的值.
例子.

```lisp
> (define p1 (posn 1 2))
> (define p2 (struct-copy posn p1 [x 3]))
> (list (posn-x p2) (posn-y p2))
'(3 2)
> (list (posn-x p1) (posn-y p1))
'(1 2)
```

## 结构子类型

`struct`的扩展形式可以用来定义`structure subtype`, 它是对现有`结构类型`的扩展.

    (struct-id super-id (field-id ...)

`super-id` 必须是由 `struct` 绑定的结构类型名称(即不能直接作为`表达式`使用的名称).
举例来说.

```lisp
(struct posn (x y))
(struct 3d-posn posn (z))
```

`结构子类型`继承其`超类型`(supertype)的`字段`,
并且在`超类型`字段的值之后,`子类型`的构造函数再接收`子类型`字段的值.
`结构子类型`的`实例`可以与`超类型`的`谓词`和`访问器`一起使用.
例子.

```lisp
> (define p (3d-posn 1 2 3))
> p
#<3d-posn>
> (posn? p)
#t
> (3d-posn-z p)
3
; a 3d-posn has an x field, but there is no 3d-posn-x selector:
> (3d-posn-x p)
3d-posn-x: undefined;
 cannot reference an identifier before its definition
  in module: top-level
; use the supertype's posn-x selector to access the x field:
> (posn-x p)
1
```

## 不透明与透明的结构类型, Opaque versus Transparent

对于`结构类型`的定义, 例如

    (struct posn (x y))

`结构类型`的`实例`在打印时不会显示关于`字段值`的任何信息. 也就是说, `结构类型`默认是`不透明的`(opaque).
如果`结构类型`的`访问器`和`突变器`对`模块`是私有的, 那么其他`模块`就不能依赖该类型`实例`的`表示`(representation).

要使某个`结构类型`透明(transparent), 请在`字段`名序列后使用 `#:transparent` 关键字.

```lisp
(struct posn (x y)
        #:transparent)
> (posn 1 2)
(posn 1 2)
```

透明`结构类型`的`实例`, 会像按照调用`构造函数`的形式打印出来, 因此它显示了该`结构`的`字段`的值.
`透明结构类型`还允许在其`实例`上使用`反射性`(reflective)操作, 如 `struct?` 和 `struct-info` (参见`Reflection and Dynamic Evaluation`).

`结构类型`默认是`不透明的`, 因为不透明的`结构实例`提供了更多的`封装`(encapsulation)保证.
也就是说, `库`可以使用不透明的`结构`来封装数据, 而`库`的`客户端`不能操作`结构`中的数据, 除非`库`允许这样做.

## 结构比较

一般的`equal?`会自动递归作用在`透明结构类型`的`字段`上.
但对于`不透明结构类型`, `equal?`默认仅仅比较`实例`相同(identity), 而不对`字段`进行递归比较:

```lisp
(struct glass (width height) #:transparent)
> (equal? (glass 1 2) (glass 1 2))
#t
(struct lead (width height))
> (define slab (lead 1 2))
> (equal? slab slab)
#t
> (equal? slab (lead 1 2))
#f
```

为了支持通过 `equal?` 进行`实例`比较, 同时不使`结构类型`透明, 你可以使用 `#:methods` 关键字, `gen:equal+hash`, 并实现三种方法.

```lisp
(struct lead (width height)
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur) ; 定义函数 equal-proc, 比较 a 和 b
     (and (equal?-recur (lead-width a) (lead-width b))
          (equal?-recur (lead-height a) (lead-height b))))
   (define (hash-proc a hash-recur) ; 计算 primary hash code of a
     (+ (hash-recur (lead-width a))
        (* 3 (hash-recur (lead-height a)))))
   (define (hash2-proc a hash2-recur) ; 计算 secondary hash code of a
     (+ (hash2-recur (lead-width a))
             (hash2-recur (lead-height a))))])
> (equal? (lead 1 2) (lead 1 2))
#t
```

列表中的第一个`函数`实现对两个`lead`的`equal?` 测试;
该函数的第三个`参数`被用来代替 `equal?`, 进行`递归`相等性测试, 因此可以正确处理数据循环.
另外两个`函数`是计算用于`hash table`的`主要`和`次要`哈希码.

```lisp
> (define h (make-hash))
> (hash-set! h (lead 1 2) 3)
> (hash-ref h (lead 1 2))
3
> (hash-ref h (lead 2 1))
hash-ref: no value found for key
  key: #<lead>
```

`gen:equal+hash` 提供的第一个函数不需要`递归地`比较`结构`的`字段`.
例如, 代表`集合`的`结构类型`, 可以通过检查`集合`的`成员`是否相同, 来实现`equality` 测试, 与内部表示中`元素`的`顺序`无关.
只要注意, 对任何两个被认为是`等价`的`结构类型`, `哈希函数`应该产生相同的`值`即可.

### 结构类型的生成性, Generativity

每次计算`struct`形式时, 它都会生成一个新`结构类型`, 不同于所有现存的`结构类型`, 即使其他`结构类型`有相同的`名称`和`字段`.

这种`generativity`对于执行`抽象`(abstraction), 和实现例如`解释器`(interpreters)这样的程序非常有用.
但是如果将`struct`形式放在被多次计算的位置上, 要小心:
举例来说.

```lisp
(define (add-bigger-fish lst)
  (struct fish (size) #:transparent) ; 每次调用 struct, 都会创建全新的结构类型
  (cond
   [(null? lst) (list (fish 1))]
   [else (cons (fish (* 2 (fish-size (car lst))))
               lst)]))
> (add-bigger-fish null)
(list (fish 1))
> (add-bigger-fish (add-bigger-fish null))
fish-size: contract violation
  expected: fish?
  given: (fish 1)
(struct fish (size) #:transparent)
(define (add-bigger-fish lst)
  (cond
   [(null? lst) (list (fish 1))]
   [else (cons (fish (* 2 (fish-size (car lst))))
               lst)]))
> (add-bigger-fish (add-bigger-fish null))
(list (fish 2) (fish 1))
```

## 预制结构类型, Prefab

尽管`透明结构类型`的`打印`可以显示其内容,
但`结构`的打印形式不能像`数字`, `字符串`, `符号`或`列表`那样, 在`表达式`中使用后能重获`结构`.

`prefab`(previously fabricated)`结构类型`是Racket `printer`和表达式`reader`已知的`内置类型`.
存在无限多这样的类型, 它们通过 `名称`, `字段数`, `超类型`和其他类似的细节来`索引`(indexed).
`预制结构`的打印形式类似于`矢量`, 但它的开头是`#s`, 而不仅仅是`#`, 而且`打印形式`的第一个元素是`预制结构类型`的`名称`.

下面的例子显示了含有一个`字段`的 `sprout 预制结构类型`的`实例`.
第一个`实例`的`字段值`是`bean`, 第二个`实例`的`字段值`是`alfalfa`.

```lisp
> '#s(sprout bean) ; 发芽的豆子
'#s(sprout bean)
> '#s(sprout alfalfa)
'#s(sprout alfalfa)
```

像`数字`和`字符串`一样, 预制结构是`slef-quoting`, 所以上面的`quote`是可选的.

```lisp
> #s(sprout bean)
'#s(sprout bean)
```

当你将 `#:prefab` 关键字与`struct`一起使用时, 不会生成新的`结构类型`, 而是获得基于现有`预制结构类型`的`绑定`.

```lisp
> (define lunch '#s(sprout bean))
> (struct sprout (kind) #:prefab)
> (sprout? lunch)
#t
> (sprout-kind lunch)
'bean
> (sprout 'garlic)
'#s(sprout garlic)
```

上面的字段名 `kind` 对于查找`预制结构类型`并不重要, 只有名称 `sprout` 和`字段数目`才重要.
也就是, 有三个`字段`的`预制结构类型sprout`与只有一个`字段`的`预制结构`是不同的.

```lisp
> (sprout? #s(sprout bean #f 17))
#f
> (struct sprout (kind yummy? count) #:prefab) ; redefine
> (sprout? #s(sprout bean #f 17))
#t
> (sprout? lunch)
#f
```

`预制结构类型`可以用另一个`预制结构类型`作为其`超类型`, 它可以有`可变字段`(mutable), 也可以有`自动字段`(auto field).
这些方面的任何变化, 都对应于不同的`预制结构类型`, 而`结构类型`名称的打印形式, 编码了所有相关的细节.

```lisp
> (struct building (rooms [location #:mutable]) #:prefab)
> (struct house building ([occupied #:auto]) #:prefab
        #:auto-value 'no)
> (house 5 'factory)
'#s((house (1 no) building 2 #(1)) 5 factory no) ; house 1个字段, building 2个字段.
```

每一个`预制结构类型`都是`透明的`--但甚至比`透明类型`更不抽象(less abstract),
因为不需要访问特定的`结构类型声明`或现有的`例子`, 就可以创建`实例`.
总的来说, `结构类型`的不同`选项`, 提供了从`更抽象`到`更方便`的可能性范围(spectrum).

+ `Opaque`(默认): 如果不能访问`结构类型声明`, 就不能`inspect`或`forge`实例.
正如下一节所讨论的, `constructor guards`和`properties`可以被`附加到结构`类型上, 以进一步`保护`或`专门化`(specialize)其`实例`的行为.
+ `transparent` ; 任何人都可以`检查`或`创建`实例, 而不需要访问`结构类型声明`, 这意味着`value printer`可以显示`实例`的`内容`.
然而, 所有`实例`的`创建`都要经过`constructor guards`, 因此`实例`的内容可以受到`控制`, 实例的`行为`也可以通过`属性`进行`专门化`.
由于`结构类型`是由其`定义`生成的, 所以不能简单地通过`结构类型`的`名称`来制造`实例`, 因此也不能由`表达式 reader`自动生成.
+ `Prefab`: 任何人都可以在任何时候`检查`或`创建`一个实例, 而不需要事先访问`结构类型声明`或`实例`.
因此, `表达式 reader`可以直接制造`实例`. 实例不能有`constructor guards`或`properties`.

由于`表达式 reader`可以生成`预制实例`, 所以当方便的`serialization`比`抽象化`更重要时, 它们就很有用.
然而, `Opaque`和`transparent`结构也可以被`序列化`, 如果用`Datatypes and Serialization`中描述的`serializable-struct`来定义它们的话.

## 更多结构类型选项

`struct`的完整语法支持许多`选项`, 包括在`结构类型`层面和单个`字段`层面.

    (struct struct-id maybe-super (field ...)
            struct-option ...)

    maybe-super     =
                                    |     super-id

                        field   =   field-id
                                    |   [field-id field-option ...]

`struct-option`总是以`关键词`开始:

### `#:mutable`

导致该`结构`的所有`字段`都是可变的, 并为每个`field-id`引入`mutator` -- `set-struct-id-field-id!`, 用于设置该`结构类型`实例中相应`字段`的值.
示例.

```lisp
> (struct dot (x y) #:mutable)
(define d (dot 1 2))
> (dot-x d)
1
> (set-dot-x! d 10)
> (dot-x d)
10
```

`#:mutable` 选项也可以作为`字段`选项使用, 在这种情况下, 它使一个单独的`字段`变得可变.
例子.

```lisp
> (struct person (name [age #:mutable]))
(define friend (person "Barney" 5))
> (set-person-age! friend 6)
> (set-person-name! friend "Mary")
set-person-name!: undefined;
 cannot reference an identifier before its definition
  in module: top-level
```

### `#:transparent`

控制对`结构实例`的`反射式访问`(reflective access), 正如前一节 `不透明与透明的结构类型`中所讨论的.

### `#:inspector inspector-expr`

推广了 `#:transparent`, 以支持对`反射操作`的更多`控制性访问`.

### `#:prefab`

访问`内置`的`结构类型`, 在上一节中讨论过, `Prefab Structure Types`.

### `#:auto-value auto-expr`

指定一个用于`结构类型`中所有`自动字段`的`值`, 其中`自动字段`由 `#:auto field` 选项表示.
对于`自动字段`, `构造函数`不接受 arguments. `自动字段`是隐式`可变的`(通过`反射操作`), 但是只有在同时指定 `#:mutable` 的情况下, 才会绑定`mutator 函数`.
例子.

```lisp
> (struct posn (x y [z #:auto])
               #:transparent
               #:auto-value 0)
> (posn 1 2)
(posn 1 2 0)
```

### `#:guard guard-expr`

指定一个`constructor guard`程序, 当`结构类型`的`实例`被创建时, 它将被调用.
guard 需要的`参数`, 与`结构类型`中的`非自动字段`一样多, 再加上被`实例化的`类型的`名称`
(如果是`子类型`被`实例化`, 在这种情况下, 最好使用`子类型`的名称报告错误).
`Guard` 应该返回与`给定值`相同`数量`的`值`, 减去`name`参数.
如果给定的`参数`之一是不可接受的,  `guard` 可以引发`异常`(exception), 或者它可以转换`arguments`.
示例.

```lisp
> (struct thing (name)
          #:transparent
          #:guard (lambda (name type-name)
                    (cond
                      [(string? name) name]
                      [(symbol? name) (symbol->string name)]
                      [else (error type-name
                                   "bad name: ~e"
                                   name)])))
> (thing "apple")
(thing "apple")
> (thing 'apple)
(thing "apple")
> (thing 1/2)
thing: bad name: 1/2
```

即使在创建`子类型实例时`, 也会调用该`guard`(防护).
在这种情况下, 只有父类`构造函数`应该接受的`字段`被提供给`guard`,
(但是`子类型`的`guard`同时得到`原始字段`和`子类型`添加的`字段`).
示例.

```lisp
> (struct person thing (age)
          #:transparent
          #:guard (lambda (name age type-name)
                    (if (negative? age)
                        (error type-name "bad age: ~e" age)
                        (values name age))))
> (person "John" 10)
(person "John" 10)
> (person "Mary" -1)
person: bad age: -1
> (person 10 10)
person: bad name: 10
```

### `#:methods interface-expr [body ...]`

关联`结构类型`的`方法`定义, 对应于`通用接口`(generic interface).
例如, 若 实现 `gen:dict 方法`, 则允许`结构类型`的`实例`作为`字典`(dictionary)使用.
实现 `gen:custom-write 方法`, 相当于自定义了`结构类型`的`实例`的`display`方式.
例子.

```lisp
> (struct cake (candles)
          #:methods gen:custom-write
          [(define (write-proc cake port mode)
             (define n (cake-candles cake))
             (show "   ~a   ~n" n #\. port)
             (show " .-~a-. ~n" n #\| port)
             (show " | ~a | ~n" n #\space port)
             (show "---~a---~n" n #\- port))
           (define (show fmt n ch port)
             (fprintf port fmt (make-string n ch)))])
> (display (cake 5))
   .....
 .-|||||-.
 |       |
-----------
```

### `#:property prop-expr val-expr`

将`属性`和`值`, 与`结构类型`相关联.
例如, 设置 `prop:procedure` 属性,允许将`结构实例`作为`函数`使用;
`属性`的`值`决定了, 将`结构`作为函数使用时, 如何实现`调用`.
示例.

```lisp
> (struct greeter (name)
          #:property prop:procedure
                     (lambda (self other) ; self 指向这个结构体本身.
                       (string-append
                        "Hi " other
                        ", I'm " (greeter-name self))))
(define joe-greet (greeter "Joe"))
> (greeter-name joe-greet)
"Joe"
> (joe-greet "Mary")
"Hi Mary, I'm Joe"
> (joe-greet "John")
"Hi John, I'm Joe"
```

### `#:super super-expr`

提供 `super-id` 的`替代方法`, 等价于在 `struct-id` 旁边给出 `super-id`.
`super-expr` 应该产生`结构类型描述符`值(structure type descriptor), 描述符在创建结构体时自动生成.
`super-expr` 不能是 `结构类型` 的`名称`(`名称`不是`表达式`).
` #:super` 的一个优点是: `结构类型描述符`是`值`, 因此可以传递给`程序`.
例子.

```lisp
(define (raven-constructor super-type) ; raven 送信乌鸦
  (struct raven ()
          #:super super-type
          #:transparent
          #:property prop:procedure (lambda (self) ; 指定结构类型的 调用方法
                                      'nevermore))
  raven)
> (let ([r ((raven-constructor struct:posn) 1 2)]) ; struct:posn 是结构类型描述符, 1 2 被传递给 super-类型.
    (list r (r)))
(list (raven 1 2) 'nevermore)
> (let ([r ((raven-constructor struct:thing) "apple")])
    (list r (r)))
(list (raven "apple") 'nevermore)
```
