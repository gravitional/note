# 输入和输出

Racket`端口`(port)对应于`Unix`的`流`(stream)的概念(不要与`racket/stream`的`流`混淆).

Racket`端口`代表一个`数据源`或`数据汇`(source or sink), 比如`文件`, `终端`, `TCP连接`, 或`内存字符串`.
`端口`提供了`顺序访问`(suquential access)的功能, 数据可以每次一部分地`读取`或`写入`, 而不需要一次性吞吐所有`数据`.
更具体地说, `输入端口`(input port)代表一个`源`, 程序可以从中读取数据, `输出端口`(output port)代表一个`汇`, 程序可以向其写入数据.

## 端口的种类

各种各样的函数, 创造了各种类型的`端口`. 这里有几个例子.

+ `文件`: `open-output-file` 函数打开一个`文件`供写入, `open-input-file` 打开一个文件供读取.
例子.

```lisp
> (define out (open-output-file "data"))
> (display "hello" out)
> (close-output-port out)
> (define in (open-input-file "data"))
> (read-line in)
"hello"
> (close-input-port in)
```

如果`文件`已经存在, 那么 `open-output-file` 默认会引发一个`异常`.
提供像 `#:existence 'truncate` 或 `#:existence 'update` 这样的`选项`来`覆盖`或`更新`该文件.
例子.

```lisp
> (define out (open-output-file "data" #:exists 'truncate))
> (display "howdy" out)
> (close-output-port out)
```

使用 `close` 调用来匹配 `open` 调用是繁琐且容易出错的,
所以, 大多数`Racket`程序员选择使用 `call-with-input-file` 和 `call-with-output-file` 函数,
它们接收一个函数`f`用来调用, 从而执行所需的操作. `f`的唯一参数是`端口`, 在动作执行前后, 端口会自动`打开`和`关闭`.
    例子.

```lisp
> (call-with-output-file "data" ; 自动进行端口的打开和关闭
                          #:exists 'truncate
                          (lambda (out) ; 实现一个写入函数
                                (display "hello" out)))
> (call-with-input-file "data"
                        (lambda (in) ; 以行一行读取
                          (read-line in)))
"hello"
```

+ `字符串`. `open-output-string` 函数创建`端口`, 将数据累积到`字符串`中, 而 `get-output-string` 则提取累积的`字符串`.
`open-input-string` 函数创建`端口`, 用于从`字符串`中读取数据.
例子.

```lisp
> (define p (open-output-string))
> (display "hello" p)
> (get-output-string p)
"hello"
> (read-line ; 每次都取一行数据
        (open-input-string "goodbye\nfarewell")) ; 打开字符串读取端口
"goodbye"
```

+ `TCP` 连接. `tcp-connect` 函数为`TCP`通信的客户端同时创建了一个输入端口和一个输出端口. tcp-listen函数创建一个服务器, 它通过tcp-accept接受连接.
[TCP/IP协议详解](https://zhuanlan.zhihu.com/p/33889997)
例子.

```lisp
> (define server (tcp-listen 12345))
> (define-values (c-in c-out) (tcp-connect "localhost" 12345))
> (define-values (s-in s-out) (tcp-accept server))
> (display "hello\n" c-out)
> (close-output-port c-out)
> (read-line s-in)
"hello"
> (read-line s-in)
#<eof>
```

+ `进程管道`; `subprocess` 函数在`操作系统`级别运行一个`新进程`, 并返回对应于`子进程`的 `stdin`, `stdout` 和 `stderr` 的端口.
(前`三个参数`可以是某些种类的已有`端口`, 直接连接到`子进程`, 而不需要创建`新端口`).
例子.

```lisp
> (define-values (p stdout stdin stderr)
    (subprocess #f #f #f "/usr/bin/wc" "-w"))
> (display "a b c\n" stdin)
> (close-output-port stdin)
> (read-line stdout)
"3"
> (close-input-port stdout)
> (close-input-port stderr)
```

+ `内部管道`; `make-pipe` 函数返回两个作为`管道末端`的`端口`. 这种管道是`Racket`内部的, 与`操作系统`级别的, 用于不同进程之间的通信的`管道`无关.
例子.

```lisp
> (define-values (in out) (make-pipe))
> (display "garbage" out) ; 向 out 端口写入的数据,
> (close-output-port out)
> (read-line in) ; 可以从 in 端口读入
"garbage"
```

## 默认端口

对于大多数简单的`I/O`函数, `目标端口`是`可选`的参数, 默认是当前`输入端口`或当前`输出端口`.
此外, `错误信息`被写到当前`错误端口`, 这是一个`输出端口`. `current-input-port`, `current-output-port` 和 `current-error-port` 函数返回相应的当前端口.
例子.

```lisp
> (display "Hi")
Hi
> (display "Hi" (current-output-port)) ; the same
Hi
```

如果你在终端中启动 `racket` 程序, 那么当前的`输入`, `输出` 和 `错误端口` 都是与`终端`相连的.
更一般地说, 它们是与 `操作系统` 级别的 `stdin`, `stdout` 和 `stderr` 相连的.
在本指南的例子中, 用`紫色`显示写到 `stdout` 的输出, 用`红色斜体` 显示写到 `stderr` 的输出.
例子.

```lisp
(define (swing-hammer)
  (display "Ouch!" (current-error-port)))
> (swing-hammer)
Ouch!
```

`current-port`函数实际上是`参数`(parameters), 这意味着它们的`值`可以用`parameterize`来设置.
参见`动态绑定: 参数化`, 了解参数的介绍.
例子.

```lisp
> (let ([s (open-output-string)])
    (parameterize ([current-error-port s])
      (swing-hammer)
      (swing-hammer)
      (swing-hammer))
    (get-output-string s))
"Ouch!Ouch!Ouch!"
```

## 读取和写入Racket数据

正如在整个`内置数据类型`中所提到的, `Racket` 提供了三种方法来打印`内置值`的`实例`.

+ `print`, 以与打印 `REPL` 结果相同的方式打印一个值;
+ `write`, 打印一个`值`,  并且满足, 若读取`输出`, 能重新得到`输入值`;
+ `display`, 它倾向于将`值`简化为其`字符`或`字节`内容--至少对于那些主要是关于`字符`或`字节`的数据类型, 否则它就回到与`write`相同的输出.

下面是一些使用各自的例子.

```lisp
;;; print
> (print 1/2)
1/2
> (print #\x)
#\x
> (print "hello")
"hello"
> (print #"goodbye")
#"goodbye"
> (print '|pea pod|)
'|pea pod|
> (print '("i" pod))
'("i" pod)
> (print write)
#<procedure:write>
;;; write
> (write 1/2)
1/2
> (write #\x)
#\x
> (write "hello")
"hello"
> (write #"goodbye")
#"goodbye"
> (write '|pea pod|)
|pea pod|
> (write '("i" pod))
("i" pod)
> (write write)
#<procedure:write>
;;; display
> (display 1/2)
1/2
> (display #\x)
x
> (display "hello")
hello
> (display #"goodbye")
goodbye
> (display '|pea pod|)
pea pod
> (display '("i" pod))
(i pod)
> (display write)
#<procedure:write>
```

总的来说,

+ `print` 对应于 `Racket` 语法的`expression`(表达式)层, 把输入当作`表达式`.
+ `write` 对应于`reader`层; 把输入进行`reader`解析.
+ `display` 大致对应于`字符层`(character layer), `reader` 之后再取`字符内容`.

`printf` 函数支持`数据`和`文本`的简单`格式化`.
在提供给`printf`的格式字符串中, `~a dispaly` 对应参数, `~s write` 对应参数, 而 `~v print` 对应参数.
例子.

```lisp
(define (deliver who when what)
  (printf "Items ~a for shopper ~s: ~v" who when what))

> (deliver '("list") '("John") '("milk"))
Items (list) for shopper ("John"): '("milk")
```

在使用`write`, 而不是`dsiplay`或`print`之后, 许多形式的`数据`可以使用`read`读回来.
`print`输出的值同样也可以用`read`来解析, 但结果可能有额外的`quote`形式,
因为`print` 输出的`形式`是为了像`表达式`一样被阅读.
例子.

```lisp
> (define-values (in out) (make-pipe))
> (write "hello" out)
> (read in)
"hello"
> (write '("alphabet" soup) out)
> (read in)
'("alphabet" soup)
> (write #hash((a . "apple") (b . "banana")) out)
> (read in)
'#hash((a . "apple") (b . "banana"))
> (print '("alphabet" soup) out)
> (read in)
''("alphabet" soup)
> (display '("alphabet" soup) out)
> (read in)
'(alphabet soup)
```

## 数据类型和序列化,Datatypes and Serialization

`预制结构类型`(见 `Prefab Structure Types`) 自动支持`序列化`: 它们可以被写入`输出流`, 并且可以从`输入流`中读回`副本`.

```lisp
> (define-values (in out) (make-pipe))
> (write #s(sprout bean) out)
> (read in)
'#s(sprout bean)
```

由 `struct` 创建的其他`结构类型`比 `prefab` 结构类型提供了更多的`抽象`(abstraction),
通常使用 `#<....>` 符号, 对于`不透明`(opaque)的结构类型;
或使用 `#(....)` 矢量符号, 对于`透明`(transparent)的结构类型, 来`write`.
在这两种情况下, 结果都`不能`作为结构类型的`实例`被读回.

```lisp
> (struct posn (x y))
> (write (posn 1 2))
#<posn>
> (define-values (in out) (make-pipe))
> (write (posn 1 2) out)
> (read in)
pipe::1: read: bad syntax `#<`
> (struct posn (x y) #:transparent)
> (write (posn 1 2))
#(struct:posn 1 2)
> (define-values (in out) (make-pipe))
> (write (posn 1 2) out)
> (define v (read in))
> v
'#(struct:posn 1 2)
> (posn? v)
#f
> (vector? v)
#t
```

`serializable-struct` 形式定义了, 可以`serialize`为`值`的`结构类型`, 这个`值`可以通过`write`来打印, 通过`read`来恢复.
`serialize`的结果可以被`deserialize`, 以取回`原始结构类型`的实例.
`序列化`形式和`函数`是由 `racket/serialize` 库提供的.
例子.

```lisp
> (require racket/serialize)
> (serializable-struct posn (x y) #:transparent)
> (deserialize (serialize (posn 1 2)))
(posn 1 2)
> (write (serialize (posn 1 2)))
((3) 1 ((#f . deserialize-info:posn-v0)) 0 () () (0 1 2))
> (define-values (in out) (make-pipe))
> (write (serialize (posn 1 2)) out)
> (deserialize (read in))
(posn 1 2)
```

除了由 `struct` 绑定的名称之外, `serializable-struct `还绑定了一个带有`反序列化信息`的`标识符`,
并且它自动`provide`来自`模块上下文`的`反序列化标识符`.
当一个`值`被`反序列化`时, 这个`反序列化标识符` 可以被访问(reflectively).

## 字节, 字符和编码, Bytes, Characters, and Encodings

像 `read-line`, `read`, `display` 和 `write` 这样的函数都是以 `字符`(对应于 `Unicode` 标量值)为单位工作的.
从概念上讲, 它们是以 `read-char` 和 `write-char` 的方式实现的.

更简单(primitively)地说, `端口`(ports)读取和写入`字节`(bytes), 而不是`字符`(characters).
函数 `read-byte` 和 `write-byte` 读和写`原始字节`(raw byte).
其他函数, 例如 `read-byte-line`, 建立在`字节`操作之上, 而不是`字符`操作.

其实在概念上, `read-char` 和 `write-char` 函数是以 `read-byte` 和 `write-byte` 的方式实现的.
当一个字节的`值`小于`128`时, 它就对应于一个`ASCII`字符.
任何其他的`字节`都被视为 `UTF-8` 序列的一部分, 其中 `UTF-8` 是用`字节` 编码 `Unicode` 标量值的一种特殊的标准方式
(它有一个很好的特性, 即`ASCII`字符被编码为自己).
因此, 单个`read-char` 可以多次调用 `read-byte`, 而单个 `write-char` 可以产生多个`输出字节`.

`read-char` 和 `write-char` 操作总是使用 `UTF-8 编码`.
如果你有一个使用不同编码的`文本流`, 或者你想生成一个不同编码的文本流, 使用 `reencode-input-port` 或 `reencode-output-port`.
`reencode-input-port` 函数将`输入流`从你指定的`编码`转换为 `UTF-8` 流;
这样, `read-char` 就能看到 `UTF-8编码` , 即使原始编码使用的是不同的编码.
然而, 要注意的是, `read-byte` 也会看到重新编码的数据, 而不是原始的`字节流`.

## I/O模式, I/O 模式

举个例子, 假设你有两个文件, 与你的程序在同一目录下, `"oneline.txt"`和 `"manylines.txt"`.

    "oneline.txt"
    我只有一行, 但在这一行之后有一个空行.

    "manylines.txt"

    我是
    一条信息
    分成了几行.

如果你有一个相当小的文件, 你可以把`文件`作为一个`字符串`读入.
例子.

```lisp
> (define file-contents
    (port->string (open-input-file "oneline.txt") #:close? #t))
> (string-suffix? file-contents "after this one.")
#f
> (string-suffix? file-contents "after this one.\n")
#t
> (string-suffix? (string-trim file-contents) "after this one.")
#t
```

+ 我们使用来自 `racket/port的port->string` 来完成对一个字符串的读取: `#:close? #t` 关键字参数, 确保我们的文件在`读取`后被`关闭`.
+ 我们使用 `racket/string` 中的 `string-trim` 来删除文件`最开始`和`最后`的多余的`空白`. (很多格式化软件坚持认为文本文件以一个`空行`结束). )
+ 如果你的文件只有一行文字, 也请参见`read-line`.

如果你想处理文件的每一行, 那么你可以使用`in-lines`来处理.

```lisp
> (define (upcase-all in)
    (for ([l (in-lines in)])
      (display (string-upcase l))
      (newline)))
> (upcase-all (open-input-string
               (string-append
                "Hello, World!\n"
                "Can you hear me, now?")))
HELLO, WORLD!
CAN YOU HEAR ME, NOW?
```

你也可以在跨行`组合`计算. 因此, 如果你想知道有多少行包含 `"m"`, 你可以这样做.
例子.

```lisp
> (with-input-from-file "manylines.txt"
    (lambda ()
      (for/sum ([l (in-lines)] ; in-lines 的默认参数是默认输入端口
                 #:when (string-contains? l "m"))
        1)))
2
```

其中`for/sum` 累加 `for` 循环的结果.

```lisp
> (for/sum ([i '(1 2 3 4)]) i)
10
```

这里, 来自 `racket/port` 的 `with-input-from-file` 将默认的输入端口设置为 `子程序`(thunk) 中的文件 `"manylines.txt"`. 
它还会在计算完成后关闭该文件(以及其他一些情况下).

然而, 如果你想确定 `"hello"` 是否出现在`文件`中, 那么你可以搜索单独的行, 但更简单的是将一个`正则表达式`(见正则表达式)应用到`stream`中.

```lisp
> (define (has-hello? in)
    (regexp-match? #rx"hello" in))
> (has-hello? (open-input-string "hello"))
#t
> (has-hello? (open-input-string "goodbye"))
#f
```

如果你想把一个端口`复制`到另一个端口, 可以使用来自 `racket/port的copy-port`, 
当`大量数据`准备好时, 它可以有效地传输`大块数据`, 但如果只有`小块数据`可用, 也可以`立即传输`.

```lisp
> (define o (open-output-string))
> (copy-port (open-input-string "broom") o)
> (get-output-string o)
"broom"
```
