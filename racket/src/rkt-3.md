# 内置的数据类型

[The Reader](https://docs.racket-lang.org/reference/reader.html#%28part._parse-vector%29)

上一章介绍了`Racket`的一些内置数据类型: `数字`, `布尔值`, `字符串`, `列表`和`过程`.
本节将对简单形式的内置数据类型进行更全面的介绍.

```lisp
5,    0.99,    1/2,   1+2i ; 数字

"Apple",     "\u03BB" ; 字符串(unicode)
#\A,    #\u03BB,    #\space,    #\newline ; 单字符
#"Apple",    #"\0\0" ; 字节串(byte)

'a,    (string->symbol "one, two"),    '|one, two|,    #ci'A  ; 符号(symbol)
'#:apple,   (string->keyword "apple"),   ; 关键字 与 其引用

'(1 . 2),   '((1 . 2) . 3),   '(0 1 2),   (cons 0 (cons 1 (cons 2 null))) ; 对儿, 列表
'#("a" "b" "c"),   #4(baldwin bruce),  ; 矢量
'#hash(("apple" . red) ("banana" . yellow)) ; 哈希表

#t,    #f,    一般值如 "字符串" 相当于 #t ; 布尔值

'#&"apple" , (define b (box "apple")), ; 箱子

void,   undefined,    #<void>,    #<undefined> ; 空和未定义
```

## 布尔值 Booleans

`Racket`有两个特别的`常量`来表示`布尔值`. `#t`表示真, `#f`表示假. 大写的`#T`和`#F`被解析为相同的值, 但小写的形式更受欢迎.
`boolean?` procedure 可以识别这两个布尔值常量. 在`if`, `cond`, `and`, `or`等表达式的测试结果中, 除了`#f`以外的任何值都算作真.
例子:

> (= 2 (+ 1 1))

```lisp
#t
> (boolean? #t)
#t
> (boolean? #f)
#t
> (boolean? "no")
#f
> (if "no" 1 0)
1
```

## 数字 Numbers

`Racket`数字可以是`精确`的, 也可以是`不精确`(inexact)的.

+ 精确的数字可以是:
  + 任意大或小的`整数`, 如`5`, `9999999999999999`, 或`-17`.
  + 有理数, 两个任意大或小的`整数`精确比值, 如`1/2`, `999999999999/2`, 或`-3/4`;
  + 具有精确`实部`和`虚部`的`复数`(其中虚部不为零), 如`1+2i`或`1/2+3/4i`.

+ 不精确的数是可以是:
  + 数字的`IEEE`浮点表示, 例如`2.0`或`3.14e+87`, 其中`IEEE`的无穷数和非数被写成`+inf.0`, `-inf.0`和`+nan.0`(或`-nan.0`);
  + `复数`, 其实部和虚部都是`IEEE`的浮点表示, 如`2.0+3.0i`或`-inf.0+nan.0i`; 作为一种特殊情况, 不精确复数可以有精确的`零实部`和不精确的`虚部`.

`非精确数`打印时带有小数点或指数符号, 而精确数则打印为`整数`和`分数`.
同样的约定适用于读取`数字常量`, 但`#e`或`#i`可以作为数字的前缀, 强制其解析为`精确`或`非精确`数字. 前缀`#b`, `#o`和`#x`指定将数字解释为`二进制`, `八进制`和`十六进制`.
例子:

```lisp
> 0.5
0.5
> #e0.5
1/2
> #x03BB
955
```

涉及`不精确`数字的计算会产生不精确的结果, 所以不精确性会污染数字.
不过要注意的是, `Racket`没有提供 `不精确的布尔`, 所以对`不精确`数字进行比较时,仍然产生`精确`的结果.
procedures `exact->inexact` 和 `inexact->exact` 在这两种类型的数字之间进行转换.
举例说明:

```lisp
> (/ 1 2)
1/2
> (/ 1 2.0)
0.5
> (if (= 3.0 2.999) 1 2)
2
> (inexact->exact 0.1)
3602879701896397/36028797018963968
```

当精确的结果需要表示成非有理数的实数时, 例如`sqrt`, `log`和`sin`等 procedure 也会产生`非精确`的结果. `Racket`只能表示有理数, 以及有理数组成的`复数`.
例子:

```lisp
> (sin 0)   ; rational...
0
> (sin 1/2) ; not rational...
0.479425538604203
```

就`性能`而言, 小整数的计算通常最快, 其中 `小` 是指`数字`比机器`字大小`的`有符号数字`少一个比特.
用非常大的`精确整数`或`非整数`的`精确数字`进行计算, 可能比用`非精确数字`进行计算要花费更多资源.

```lisp
(define (sigma f a b)
  (if (= a b)
      0
      (+ (f a) (sigma f (+ a 1) b))))
> (time (round (sigma (lambda (x) (/ 1 x)) 1 2000)))
cpu time: 29 real time: 15 gc time: 6 ; 花费更多时间
> (time (round (sigma (lambda (x) (/ 1.0 x)) 1 2000)))
cpu time: 0 real time: 0 gc time: 0
```

`整数`, `有理数`, `实数`(总是`有理数`)和`复数`等数字分类(categories)是以通常的方式定义的,
可以由 `integer?`, `rational?`, `real?` 和 `complex?` 等 procedures 识别, 除了通常的`number?` .
少数数学程序只接受`实数`, 但大多数实现了对`复数`的标准扩展. 例子:

```lisp
> (integer? 5)
> (complex? 5)
> (integer? 5.0)
> (integer? 1+2i)
> (complex? 1+2i)
> (complex? 1.0+2.0i)
> (abs -5)
> (abs -5+2i)
abs: contract violation
  expected: real?
  given: -5+2i
> (sin -5+2i)
3.6076607742131563+1.0288031496599335i
```

`=` 过程(`procedure`) 比较数字是否相等.
如果同时给定了`非精确数`和`精确数`进行比较, 那么在比较之前, 基本上它会将`非精确数`转换为`精确数`.
`eqv?` (因此也就是`equal?`)过程, 与此相反, 它在比较数字时既考虑`exactness`又考虑数值的`equality`. 例子:

```lisp
> (= 1 1.0)
#t
> (eqv? 1 1.0)
#f
```

小心涉及`不精确数字`的比较, 由于其性质, 可能会有令人惊讶的行为. 即使是表面上简单的`不精确数`, 也可能不是你认为的那样; 
例如, 虽然一个以`2`为基数的`IEEE浮点数`可以精确表示`1/2`, 但它只能近似表示`1/10`.

例子.

```lisp
> (= 1/2 0.5)
#t
> (= 1/10 0.1)
#f
> (inexact->exact 0.1)
3602879701896397/36028797018963968
```

## 字符 Characters

`Racket`字符对应于`Unicode`标量值.
粗略的说, `标量值`是一个无符号的整数, 可以表示成`21`位`bit`, 它对应自然语言的`字符`, 或字符的某个`部分`.
从技术上讲, `标量值`是比`Unicode`标准中的`字符` 更简单的概念, 但它作为近似说法, 在许多方面都很方便.
例如, 任何带`accent`的罗马字母都可以表示为`标量值`, 任何常见的`中文字符`也是如此.

尽管每个`Racket`字符对应一个整数, 但`字符`数据类型与`数字`是分开的.
`char->integer`和`integer->char`程序可以在`scalar-value`的数字和对应的字符之间进行转换.

可打印的字符通常打印为`#\`,  后面是代表的字符.
不可打印的字符通常以`#u`的形式打印, 后面是标量值的十六进制数字.
有几个字符按特殊方式打印的; 例如, `空格`和`换行`字符分别打印为`#\space`和`#\newline`.
例子.

```lisp
> (integer->char 65)
#\A
> (char->integer #\A)
65
> #\λ
#\λ
> #\u03BB
#\λ
> (integer->char 17)
#\u0011
> (char->integer #\space)
32
```

相较于打印字符结果的`字符常数`语法, `display` procedure 直接将`字符`写入当前的`输出端口`(见输入和输出).
例子.

```lisp
> #\A
#\A
> (display #\A)
A
```

`Racket`提供了一些关于字符的`分类`和`转换`程序.
但要注意的是, 一些`Unicode`字符的转换只有在字符串中, 才会像人类所期望的那样进行(例如, upcasing `ß` 或 downcasing `Σ`). 例子.

```lisp
> (char-alphabetic? #\A)
> (char-numeric? #\0)
> (char-whitespace? #\newline)
> (char-downcase #\A)
> (char-upcase #\ß)
```

procedure `char=?` 比较两个或多个字符, `char-ci=?` 比较字符时忽略大小写.
`eqv?` 和 `equal?` 过程在字符上的行为, 与 `char=?` 相同; 当你想更具体地声明被比较的值是`字符`时, 使用`char=?`
例子.

```lisp
> (char=? #\a #\A)
#f
> (char-ci=? #\a #\A)
#t
> (eqv? #\a #\A)
#f
```

## 字符串 Unicode

`string`是一个固定长度的`字符`(characters)数组.
它使用`双引号`进行打印, 其中`string`中的双引号`"`, 和反斜线`\`字符用反斜线转义.

还支持其他常见的`string`转义,包括换行的`\n`, 回车的`\r`, 八进制转义(`\`后面跟三个八进制数字) 以及十六进制转义(使用`\u`,最多四个数字).
在打印`string`时,`string`中的不可打印字符通常用`\u`表示.

`display` procedure 直接将字符串中的`字符`写入当前的`输出端口`(见`Input and Output`), 这跟打印`字符串常量`的语法不同, 后者定义一个`字符串`.
例子.

```lisp
> "Apple"
"Apple"
> "\u03BB"
"λ"
> (display "Apple")
Apple
> (display "a \"quoted\" thing")
a "quoted" thing
> (display "two\nlines")
two
lines
> (display "\u03BB")
λ
```

`string`可以是`可变`的,也可以是`不可变`的;直接写成表达式的`string`是`不可变`的, 但大多数其他`string`是可变的.

`make-string` 过程, 在给出`长度`和可选的`填充字符`的情况下, 创建一个可变的`string`.
`string-ref` 过程 从`string`中访问字符(使用基于`0`的索引);
`string-set!` 过程改变可变`string`中的字符.

例子.

```lisp
> (string-ref "Apple" 0)
#\A
> (define s (make-string 5 #\.))
> s
"....."
> (string-set! s 2 #\λ)
> s
"..λ.."
```

`字符串`排序和`大小写`操作通常是独立于`locale`的; 也就是说,它们对所有用户都是一样的.
我们提供了一些依赖`locale`的操作, 使字符串的大小写折叠, 和排序方式可以取决于终端用户的地区设置.

例如,如果你要对字符串进行排序, 需要排序结果在不同机器和用户间保持一致,就使用 `string<?` 或 `string-ci<?`,
如果要依据终端用户给出特定的结果,就使用 `string-locale<?` 或 `string-locale-ci<?`
例子.

```lisp
> (string<? "apple" "Banana")
#f
> (string-ci<? "apple" "Banana")
#t
> (string-upcase "Straße")
"STRASSE"
> (parameterize ([current-locale "C"])
(string-locale-upcase "Straße"))
"STRAßE"
```

对于处理纯`ASCII`, 处理`raw bytes`, 或将Unicode字符串`编码`/`解码`为`字节`(bytes), 使用`byte strings`(字节字符串).

## 字节和字节字符串

`byte`是`0`到`255` 之间的精确`整数`,包括两端. `byte?` 谓词(predicate)可以识别代表`字节`的数字.
例子.

```lisp
> (byte? 0)
#t
> (byte? 256)
#f
```

`byte string`类似于`字符串`--见字符串(Unicode)--但其内容是一串`字节`而不是字符.
`byte string`可用于处理纯`ASCII`码而非`Unicode`文本的应用程序中.

`byte string`的打印形式特别适合这种用途, 因为`byte string`的打印形式就像`byte string`的`ASCII`码, 但前缀为`#`.
`byte string`中不可打印的`ASCII`字符或`非ASCII字节`会用`八进制`符号来书写.
例子.

```lisp
> #"Apple"
#"Apple"
> (bytes-ref #"Apple" 0)
65
> (make-bytes 3 65)
#"AAA"
> (define b (make-bytes 2 0))
> b
#"\0\0"
> (bytes-set! b 0 1)
> (bytes-set! b 1 255)
> b
#"\1\377"
```

`bytes串`的`display`形式将其`raw bytes`写到当前的输出端口(见输入和输出).
从技术上讲,普通的(即character)`字符串`的`display`, 就是将该字符串的`UTF-8`编码打印到当前的输出端口, 因为输出最终是以`字节`为单位定义的;
但是, `display`一个`byte 字符串`, 输出的是没有`编码`的原始字节.
按照同样的思路,当本文档显示输出时, 从技术上讲, 它显示的是输出的`UTF-8`解码形式.
例子.

```lisp
> (display #"Apple")
Apple
> (display "\316\273")  ; same as "Î»"
Î»
> (display #"\316\273") ; UTF-8 encoding of λ
λ
```

为了明确地在字符串和字节字符串之间进行转换, `Racket` 直接支持三种编码方式.
`UTF-8`,`Latin-1`,以及`当前地区`的编码. `字节`与`字节`之间的转换(尤其是与`UTF-8`之间的转换)的通用实现, 填补了支持任意字符串编码的空白.
例子.

```lisp
> (bytes->string/utf-8 #"\316\273")
"λ"
> (bytes->string/latin-1 #"\316\273")
"Î»"
> (parameterize ([current-locale "C"])  ; C locale supports ASCII,
    (bytes->string/locale #"\316\273")) ; only, so...
bytes->string/locale: byte string is not a valid encoding for the current locale  byte string: #"\316\273"
> (let ([cvt (bytes-open-converter "cp1253" ; Greek code page
                                   "UTF-8")]
            [dest (make-bytes 2)])
    (bytes-convert cvt #"\353" 0 1 dest)
    (bytes-close-converter cvt)
    (bytes->string/utf-8 dest))
"λ"
```

## 符号 Symbols

`符号`(symbol)是一个`原子值`(atomic),其打印方式类似于, 前面带有`'`的标识符. 
以`'`开始,接着一个标识符的表达式, 产生一个`符号`值.
例子:

```lisp
> 'a
'a
> (symbol? 'a)
#t
```

对于任何字符序列, 内部(interned)都对应唯一的`符号`;
调用`string->symbol` 过程, 或者读取一个语法(syntactic)标识符,都会产生一个`内部符号`(interned symbol).
由于内部符号可以方便地用`eq?` (以及`eqv?`或`equal?`)进行比较,它们可以方便地作为`标签`和`枚举`来使用.

`符号`是区分大小写的. 通过使用`#ci`前缀或其他方式, 可以控制 `reader` 简并字符序列的大小写, 以得到同一个符号,但`reader` 默认保留大小写.
例子.

```lisp
> (eq? 'a 'a)
#t
> (eq? 'a (string->symbol "a"))
#t
> (eq? 'a 'b)
#f
> (eq? 'a 'A)
#f
> #ci'A
'a
```

任何`字符串`(即任何`字符`序列) 都可以提供给`string->symbol`以获得相应的符号.
输入给 `reader` 时, 任何字符都可以直接出现在`标识符`中, 除了`空白`和以下特殊字符.

    ( ) [ ] { } " , ' ` ; # | \

实际上, 只有符号开头不允许出现`#`, 并且后面不允许跟有`%`; 否则, `#` 也是允许的. 另外, `.` 本身不能当作符号.

`空白`或`特殊字符`可以通过用`|`或`\` quoting , 来包含在`标识符`中.
这些 quoting 机制用于打印`特殊字符`, 或者避免与`数字`模样的标识符混淆.
例子.

```lisp
> (string->symbol "one, two")
'|one, two|
> (string->symbol "6")
'|6|
```

`write` 函数 打印出不带`'`前缀的`symbol`. `symbol`的显示形式与相应的字符串相同.
例子.

```lisp
> (write 'Apple)
Apple
> (display 'Apple)
Apple
> (write '|6|)
|6|
> (display '|6|)
6
```

`gensym`和 `string->uninterned-symbol` procedure 生成新的`uninterned`符号,
这些符号不等于(根据`eq?`)任何先前的 `interned` 或 `uninterned` 符号.
`unterned` 符号作为新的`标签`是很有用的, 它不会与任何其他值混淆.
例子.

```lisp
> (define s (gensym))
> s ; s 不是内部符号,
'g42
> (eq? s 'g42) ; 它根内部符号 'g45 也不相等
#f
> (eq? 'a (string->uninterned-symbol "a"))
#f
```

## 关键字,Keywords

`关键字`值与`符号`(symbol)相似(见`符号`), 但它的打印形式是以`#:`为前缀.
例子.

```lisp
> (string->keyword "apple")
'#:apple
> '#:apple
'#:apple
> (eq? '#:apple (string->keyword "apple"))
#t
```

更确切地说, `关键字`类似于`标识符`; 
类似于`标识符`被`引用`就会产生`符号`, `关键字`被引用也可以产生`值`.
尽管在引用前后我们都它为 `关键字`, 但是有时我们用`关键字值`, 来特指`quote-keyword`表达式,或`string->keyword`产生的结果.

`unquoted keyword`不是`表达式`, 就像`unquoted identifier`不会产生`symbol`值:
例子.

```lisp
> not-a-symbol-expression ; 不会产生 symbol 值
not-a-symbol-expression: undefined;
 cannot reference an identifier before its definition
  in module: top-level
> #:not-a-keyword-expression ; 没有被 quote, 不构成表达式
eval:2:0: #%datum: keyword misused as an expression
  at: #:not-a-keyword-expression
```

尽管它们有相似之处,`关键字`的使用方式与`标识符`或`symbol`不同.

`关键字`(不加引号)被用来作为`参数列表`和某些句法形式(syntactic forms)中的特殊`标记`.
对于`运行时标志`(flag)和枚举(enumerations), 使用`symbol`而不是`关键字`. 
下面的例子说明了`关键字`和`symbol`的不同作用.
例子.

```lisp
> (define dir (find-system-path 'temp-dir)) ; 不是 '#:temp-dir
> (with-output-to-file (build-path dir "stuff.txt")
    (lambda () (printf "example\n"))
    ; 可选参数 #:mode 可以是 'text 或者 'binary
    #:mode 'text
    ; 可选参数 #:exists 可以是 'replace, 'truncate, ...
    #:exists 'replace)
```

## 对儿和列表,Pairs and Lists

`对`连接两个任意`值`. `cons`程序构建`对`, `car`和`cdr`程序分别提取`对`中的`第一`和`第二`元素. `pair?`谓词可以识别`对`.

有些`对`的打印方法是: 在两个对元素的打印形式周围包上小括号`()`, 在元素的开头加上`'`, 在元素之间加上`.`:
例子.

```lisp
> (cons 1 2)
'(1 . 2)
> (cons (cons 1 2) 3)
'((1 . 2) . 3)
> (car (cons 1 2))
1
> (cdr (cons 1 2))
2
> (pair? (cons 1 2))
#t
```

`列表`是`对`的组合,  作为后者的`链表`.  更确切地说, 一个列表要么是空列表`null`,
要么是一个`对`, 首元素是列表元素, 次元素是`列表`. 
`list?` 谓词可以识别`列表`. `null?` 谓词识别`空列表`.

列表通常打印为`'`, 后面是一对包裹着列表元素的小括号`()`.
例子.

```lisp
> null
'()
> (cons 0 (cons 1 (cons 2 null)))
'(0 1 2)
> (list? null)
#t
> (list? (cons 1 (cons 2 null)))
#t
> (list? (cons 1 2))
#f
```

当`列表`或`对`中的`元素`不能被写成`quoted value`时, 就用`list`或`cons`来显式表示.
例如,用`srcloc`构造的值不能用引号表示, 就用`srcloc`打印它.

```lisp
> (srcloc "file.rkt" 1 0 1 (+ 4 4))
(srcloc "file.rkt" 1 0 1 8)
> (list 'here (srcloc "file.rkt" 1 0 1 8) 'there)
(list 'here (srcloc "file.rkt" 1 0 1 8) 'there)
> (cons 1 (srcloc "file.rkt" 1 0 1 8))
(cons 1 (srcloc "file.rkt" 1 0 1 8))
> (cons 1 (cons 2 (srcloc "file.rkt" 1 0 1 8)))
(list* 1 2 (srcloc "file.rkt" 1 0 1 8))
```

如最后一个例子所示, `list*`用来缩写一系列不能用`list`表示的`cons`.

`write`和`display`函数 打印没有前导`'`,`cons`,`list`或`list*`的, `对`或`列表`.
对于`对`或`列表`, `write` 和`display`没有区别, 但是它们作用于列表的元素时有区别.
例子.

```lisp
> (write (cons 1 2))
(1 . 2)
> (display (cons 1 2))
(1 . 2)
> (write null)
()
> (display null)
()
> (write (list 1 2 "3"))
(1 2 "3")
> (display (list 1 2 "3"))
(1 2 3)
```

在`列表`的预定义程序中, 最重要的是那些在列表的元素中进行`迭代`的程序.

```lisp
> (map (lambda (i) (/ 1 i))
       '(1 2 3))
'(1 1/2 1/3)
> (andmap (lambda (i) (i . < . 3))
         '(1 2 3))
#f
> (ormap (lambda (i) (i . < . 3))
         '(1 2 3))
#t
> (filter (lambda (i) (i . < . 3))
          '(1 2 3))
'(1 2)
> (foldl (lambda (v i) (+ v i))
         10
         '(1 2 3))
16
> (for-each (lambda (i) (display i))
            '(1 2 3))
123
> (member "Keys"
          '("Florida" "Keys" "U.S.A."))
'("Keys" "U.S.A.")
> (assoc 'where
         '((when "3:30") (where "Florida") (who "Mickey")))
'(where "Florida")
```

`对`是不可变的(与`Lisp`的传统相反), 而 `pair?` 和 `list?` 只识别不可变的`对`和`列表`.
`mcons` 过程创建可变的`对`,它与 `set-mcar!` 和 `set-mcdr!` 以及 `mcar` 和 `mcdr` 一起工作.
可变对使用 `mcons` 打印,而 `write` 和 `display` 使用 `{` 和 `}` 打印`可变对`.
例子.

```lisp
> (define p (mcons 1 2))
> p
(mcons 1 2)
> (pair? p)
#f
> (mpair? p)
#t
> (set-mcar! p 0)
> p
(mcons 0 2)
> (write p)
{0 . 2}
```

## 矢量,Vectors

`矢量`是`固定长度`的任意值的数组. 与列表不同,矢量支持对其元素的 constant-time `访问`和`更新`.
`矢量`的打印方式类似于`列表`--括号包围的元素序列--但矢量的前缀为`'#`. 如果其中某个元素不能用`quote`表示, 则使用 `vector`.
对于作为`表达式`的`矢量`,可以提供可选的`长度`.
另外,作为表达式的矢量隐含着对其内容的`quote`, 这意味着矢量常量中的标识符和括号形式, 分别代表`symbols`和`lists`.
例子.

```lisp
> #("a" "b" "c")
'#("a" "b" "c")
> #(name (that tune))
'#(name (that tune))
> #4(baldwin bruce)
'#(baldwin bruce bruce bruce)
> (vector-ref #("a" "b" "c") 1)
"b"
> (vector-ref #(name (that tune)) 1)
'(that tune)
```

像`字符串`一样,矢量是`可变`或`不可变`的,直接写成表达式的矢量是`不可变`的.

`矢量`可以通过`vector->list`和`list->vector`与`列表`互相转换; 当与列表的预定义程序结合使用时候, 这种转换特别有用.
当分配额外的列表似乎太昂贵时,可以考虑使用`for/fold`这样的循环形式,它既可以识别`矢量`,也可以识别`列表`.

例子.

```lisp
> (list->vector (map string-titlecase
                     (vector->list #("three" "blind" "mice"))))
'#("Three" "Blind" "Mice")
```

## 哈希表,Hash Tables

`哈希表`实现了从键到值的映射,其中键和值都可以是任意的`Racket`值,对表的`访问`和`更新`通常是 constant-time.
根据哈希表建立函数 <-> 键值比较 

+ `make-hash`, `equal?`
+ `make-hasheqv`, `eqv?`
+ `make-hasheq`, `eq?`

例子.

```lisp
> (define ht (make-hash))
> (hash-set! ht "apple" '(red round))
> (hash-set! ht "banana" '(yellow long))
> (hash-ref ht "apple")
'(red round)
> (hash-ref ht "coconut")
hash-ref: no value found for key
  key: "coconut"
> (hash-ref ht "coconut" "not there")
"not there"
```

`hash`, `hashqv` 和 `hashq` 函数从初始的`键`和`值`集合中创建`不可变`的`哈希表`, 其中每个`值`在`键`之后作为参数提供.
不可变哈希表可以用`hash-set`来扩展, 它可以在`constant time`内产生新的不可变的哈希表.
例子.

```lisp
> (define ht (hash "apple" 'red "banana" 'yellow))
> (hash-ref ht "apple")
'red
> (define ht2 (hash-set ht "coconut" 'brown))
> (hash-ref ht "coconut")
hash-ref: no value found for key
  key: "coconut"
> (hash-ref ht2 "coconut")
'brown
```

不可变哈希表可以通过使用`#hash`(对基于`equal?`的表), `#hasheqv`(对基于`eqv?`的表)
或者`#hasheq`(对基于`eq?`的表)写成`表达式`.
在`#hash`, `#hasheq` 或 `#hasheqv` 之后必须紧跟包在括号`()`内的序列, 其中每个元素是带点的`键值对`.
`#hash`等形式隐含地`quote`了它们的键和值.
例子.

```lisp
> #hash(("apple" . red) ("banana" . yellow))
'#hash(("apple" . red) ("banana" . yellow))
> (hash 1 (srcloc "file.rkt" 1 0 1 (+ 4 4)))
(hash 1 (srcloc "file.rkt" 1 0 1 8))
```

`可变`和`不可变`的哈希表都按照不可变哈希表的样式打印,
如果所有的`键`和`值`都可以用`quote`表达, 则使用带引号的`#hash`,`#hasheqv` 或`#hasheq` 形式,
否则使用 `hash`,`hasheq` 或 `hasheqv`.
例子.

```lisp
> #hash(("apple" . red) ("banana" . yellow))
'#hash(("apple" . red) ("banana" . yellow))
> (hash 1 (srcloc "file.rkt" 1 0 1 (+ 4 4)))
(hash 1 (srcloc "file.rkt" 1 0 1 8))
```

可变的哈希表可以选择`弱保留`它的键(reatin its keys weakly),  只有当`key`在别的地方保留时, 这个`映射`才被保留.
例子.

```lisp
> (define ht (make-weak-hasheq))
> (hash-set! ht (gensym) "can you see me?")
> (collect-garbage)
> (hash-count ht)
0
```

请注意,即使是`弱`的哈希表,只要相应的`键`可以访问,它的值也会`强保留`(tetains its values strongly).
这就产生了`catch-22`的依赖性, 当一个`值`套娃引用它的`键`时, 这样的`映射`就会被永久地保留.
为了打破这个循环, 将`键`映射到一个`ephemeron`, 后者将`值`与它的`键`配对(除了哈希表本身的隐式配对之外).
例子.

```lisp
> (define ht (make-weak-hasheq))
> (let ([g (gensym)])
    (hash-set! ht g (list g)))
> (collect-garbage)
> (hash-count ht)
1
> (define ht (make-weak-hasheq))
> (let ([g (gensym)])
    (hash-set! ht g (make-ephemeron g (list g)))) ; 将 g 和它的值打包成 ephemero, 再作为 g 的映射
> (collect-garbage)
> (hash-count ht)
0
```

## 箱子, Boxes

`箱子`就像只有一个元素的`向量`. 它的打印形式为 quoted `#&`, 后面是被框的值的打印形式.
`#&` 形式也可以作为表达式使用, 但由于产生的`框`是`常数`,它实际上没有什么用.
例子.

```lisp
> (define b (box "apple"))
> b
'#&"apple"
> (unbox b)
"apple"
> (set-box! b '(banana boat))
> b
'#&(banana boat)
```

## 空和未定义,Void and Undefined

一些`过程`或`表达式`不需要结果值. 例如,调用显示过程只是为了`输出屏幕`的副作用.

在这种情况下, 结果值通常是一个特殊的`常数`, 打印为`#<void>`. 
当表达式的结果只是`#<void>`时, `REPL` 不会打印任何东西.

`void`过程接受任意数量的参数,并返回`#<void>`.
(也就是说, 标识符 `void` 被绑定到一个返回`#<void>`的过程, 而不是直接绑定到`#<void>`).
例子.

```lisp
> (void)
> (void 1 2 3)
> (list (void))
'(#<void>)
```

`undefined`常数, 打印为`#<undefined>`, 有时会被用作`引用`的结果, 当其值还不可用.
在以前的`Racket`版本中(6.1版之前), 过早地引用局部绑定会产生`#<undefined>`; 现在过早的引用会引发`异常`.

>在某些情况下,`undefined`结果仍然可以由`shared`形式产生.

```lisp
(define (fails) (define x x) x)
> (fails)
x: undefined;
 cannot use before initialization
```
