# 函数如何工作

### 函数

[ch03-03-how-functions-work.md](https://github.com/rust-lang/book/blob/main/src/ch03-03-how-functions-work.md)

函数在 `Rust` 代码中非常普遍.
你已经见过语言中最重要的函数之一: `main` 函数, 它是很多程序的 `入口点`.
你也见过 `fn` 关键字, 它用来声明 `新函数`.

`Rust` 代码中的函数和变量名使用 `snake case` 规范风格. 在 `snake case` 中, 所有字母都是 `小写` 并使用 `下划线` 分隔单词.
这是一个包含 `函数定义` 示例的程序:

```rust
fn main() {
    println!("Hello, world!");

    another_function();
}

fn another_function() {
    println!("Another function.");
}
```

`Rust` 中的函数定义以 `fn` 开始, 并在函数名后跟一对 `圆括号`.
`大括号` 告诉编译器哪里是 `函数体` 的开始和结尾.

可以使用函数名后跟圆括号 --- 即`f()` 来调用我们定义过的任意函数.
因为程序中已定义 `another_function` 函数, 所以可以在 `main` 函数中调用它.

>注意, 源码中 `another_function` 定义在 `main` 函数 之后; 也可以定义在之前.

`Rust` 不关心函数定义于何处, 只要 `定义了` 就行.

让我们新建一个叫做 `functions` 的二进制项目来进一步探索函数.
将上面的 `another_function` 例子写入 `src/main.rs` 中并运行. 你应该会看到如下输出:

```log
$ cargo run
   Compiling functions v0.1.0 (file:///projects/functions)
    Finished dev [unoptimized + debuginfo] target(s) in 0.28s
     Running `target/debug/functions`
Hello, world!
Another function.
```

`main` 函数中的代码会按顺序执行.
首先, 打印 `"Hello, world!"` 信息, 然后调用 `another_function` 函数并打印它的信息.

### 函数参数

函数也可以被定义为拥有 `参数`(parameters).
`参数`是 `特殊变量`, 是 `函数签名` 的一部分, `函数签名` 即对函数的名称,参数个数, 类型等的声明.

当函数拥有 `参数`(`形参`)时, 可以为这些 `参数` 提供具体的值(`实参`).
技术上讲, 这些具体值被称为 `参数`(arguments), 但是在日常交流中,
人们倾向于不区分使用 `parameter` 和 `argument`, 来表示函数定义中的`变量`, 或调用函数时传入的 `具体值`.

下面被重写的 `another_function` 版本展示了 `Rust` 中参数是什么样的:

```rust
fn main() {
    another_function(5);
}

fn another_function(x: i32) {
    println!("The value of x is: {}", x);
}
```

尝试运行程序, 将会输出如下内容:

```log
$ cargo run
   Compiling functions v0.1.0 (file:///projects/functions)
    Finished dev [unoptimized + debuginfo] target(s) in 1.21s
     Running `target/debug/functions`
The value of x is: 5
```

`another_function` 的声明中有一个命名为 `x` 的参数. `x` 的类型被指定为 `i32`.
当将 `5` 传给 `another_function` 时, `println!` 宏将 `5` 放入 `格式化字符串` 中 `大括号` 的位置.

在 `函数签名` 中, **必须** 声明每个参数的 `类型`.
这是 Rust 设计中一个经过慎重考虑的决定: 要求在函数定义中提供 `类型注解`,
意味着编译器不需要你在代码的其他地方, 注明类型来指出你的意图.

当一个函数有 `多个参数` 时, 使用 `逗号` 分隔, 像这样:

```rust
fn main() {
    print_labeled_measurement(5, 'h');
}

fn print_labeled_measurement(value: i32, unit_label: char) {
    println!("The measurement is: {}{}", value, unit_label);
}
```

这个例子创建了一个名为 `print_labeled_measurement` 的函数, 它有两个参数.
第一个参数名为 `value`,  类型是 `i32`. 第二个参数是 `unit_label` , 类型是 `char`.
然后, 该函数打印包含 `value` 和 `unit_label` 的文本.

尝试运行代码. 使用上面的例子替换当前 `functions` 项目的 `src/main.rs` 文件, 并用 `cargo run` 运行它:

```log
$ cargo run
   Compiling functions v0.1.0 (file:///projects/functions)
    Finished dev [unoptimized + debuginfo] target(s) in 0.31s
     Running `target/debug/functions`
The measurement is: 5h
```

因为我们使用 `5` 作为 `value` 的值, `h` 作为 `unit_label` 的值来调用函数, 所以程序输出包含这些值.

### 包含语句和表达式的函数体

`函数体` 由一系列的 `语句` 和一个可选的 `结尾表达式` 构成.
目前为止, 我们只介绍了没有 `结尾表达式` 的函数, 不过你已经见过作为 `语句` 一部分的 `表达式`.
因为 `Rust` 是一门**基于表达式**(expression-based) 的语言, 这是一个需要理解的(不同于其他语言)重要区别.
其他语言并没有这样的区别, 所以让我们看看 `语句` 与 `表达式` 有什么区别以及这些区别是如何影响 `函数体` 的.

实际上, 我们已经使用过 `语句` 和 `表达式`.
`语句`(Statements)是执行一些操作但 `不返回值` 的指令.
`表达式`(Expressions)计算并产生一个`值`. 让我们看一些例子:

使用 `let` 关键字创建变量,并绑定值的操作, 是一个`语句`.
在列表 3-1 中, `let y = 6;` 是一个`语句`.

```rust
fn main() {
    let y = 6;
}

```

列表 3-1: 包含一个语句的 `main` 函数定义

`函数定义`也是`语句`, 上面整个例子本身就是一个`语句`.

***语句不返回值**. 因此, 不能把 `let` 语句赋值给另一个变量, 比如下面的例子尝试做的, 会产生一个错误:

```rust
fn main() {
    let x = (let y = 6);
}
```

当运行这个程序时, 会得到如下错误:

```log
$ cargo run
   Compiling functions v0.1.0 (file:///projects/functions)
error[E0658]: `let` expressions in this position are experimental
 --> src/main.rs:2:14
  |
2 |     let x = (let y = 6);
  |              ^^^^^^^^^
  |
  = note: see issue #53667 <https://github.com/rust-lang/rust/issues/53667> for more information
  = help: you can write `matches!(<expr>, <pattern>)` instead of `let <pattern> = <expr>`
...
```

`let y = 6` 语句并不返回值, 所以没有可以绑定到 `x` 上的值.
这与其他语言不同, 例如 `C` 和 `Ruby`, 它们的赋值语句会返回所赋的值.
在这些语言中, 可以这么写 `x = y = 6`, 这样 `x` 和 `y` 的值都是 `6`; `Rust` 中不能这样写.

`表达式`会计算出一个`值`, 并且你将编写的大部分 `Rust` 代码是由`表达式`组成的.
考虑一个数学运算, 比如 `5 + 6`, 这是一个表达式并计算出值 `11`.
`表达式`可以是`语句`的一部分: 在示例 3-1 中, 语句 `let y = 6;` 中的 `6` 是一个表达式, 它计算出的值是 `6`.
`函数调用`是一个`表达式`, `宏调用`是一个`表达式`. 我们用来创建`新作用域`的大括号(代码块) --`{}`, 也是一个表达式, 例如:

```rust
fn main() {
    let x = 5;

    let y = {
        let x = 3;
        x + 1
    };

    println!("The value of y is: {}", y);
}
```

这个表达式:

```rust
{
    let x = 3;
    x + 1
}
```

是一个代码块, 它的值是 `4`. 这个值作为 `let` 语句的一部分被绑定到 `y` 上.
注意结尾没有`分号`的那一行 `x+1`, 与你见过的大部分代码行不同.
表达式的结尾没有`分号`. 如果在表达式的结尾加上`分号`, 它就变成了语句, 而语句不会返回值.
在接下来探索具有 `返回值` 的函数和 `表达式` 时要谨记这一点.

### 具有返回值的函数

函数可以向调用它的代码 `返回值`. 我们并不对返回值命名, 但要在`箭头`(`->`)后声明它的`类型`.
在 `Rust` 中, 函数的 `返回值` 等同于函数体 `最后` 一个表达式的值.
使用 `return` 关键字和指定值, 可从函数中`提前返回`; 但大部分函数隐式的返回最后的表达式.
这是一个有`返回值`的函数的例子:

```rust
fn five() -> i32 {
    5
}

fn main() {
    let x = five();

    println!("The value of x is: {}", x);
}
```

在 `five` 函数中没有`函数调用`, `宏`, 甚至没有 `let` 语句 -- 只有数字 `5`.
这在 `Rust` 中是一个完全有效的函数. 注意, 也指定了函数返回值的类型, 就是 `-> i32`.
尝试运行代码; 输出应该看起来像这样:

```log
$ cargo run
   Compiling functions v0.1.0 (file:///projects/functions)
    Finished dev [unoptimized + debuginfo] target(s) in 0.30s
     Running `target/debug/functions`
The value of x is: 5
```

`five` 函数的返回值是 `5`, 所以返回值类型是 `i32`. 让我们仔细检查一下这段代码.
有两个重要的部分: 首先, `let x = five();` 这一行表明我们使用函数的返回值, 初始化一个变量.
因为 `five` 函数返回 `5`, 这一行与如下代码相同:

```rust
let x = 5;
```

其次, `five` 函数没有参数并定义了返回值类型, 不过函数体只有单单一个 `5` 也没有分号, 因为这是一个表达式, 我们想要返回它的值.
让我们看看另一个例子:

```rust
fn main() {
    let x = plus_one(5);

    println!("The value of x is: {}", x);
}

fn plus_one(x: i32) -> i32 {
    x + 1
}
```

运行代码会打印出 `The value of x is: 6`. 但如果在包含 `x + 1` 的行尾加上一个分号, 把它从表达式变成语句, 我们将看到一个错误.

```rust
fn main() {
    let x = plus_one(5);

    println!("The value of x is: {}", x);
}

fn plus_one(x: i32) -> i32 {
    x + 1;
}
```

运行代码会产生一个错误, 如下:

```rust
$ cargo run
   Compiling functions v0.1.0 (file:///projects/functions)
error[E0308]: mismatched types
 --> src/main.rs:7:24
  |
7 | fn plus_one(x: i32) -> i32 {
  |    --------            ^^^ expected `i32`, found `()`
  |    |
...
```

主要的错误信息, `mismatched types`(类型不匹配), 揭示了代码的核心问题.
函数 `plus_one` 的定义说明它要返回一个 `i32` 类型的值, 不过语句没有 `返回值`, 这种情况下, 使用单位类型 `()` 来表示 `不返回值`.
因为不返回值与函数定义相矛盾, 从而出现一个错误.
在输出中, Rust 提供了一条信息, 可能有助于纠正这个错误: 它建议`删除分号`, 这会修复这个错误.
