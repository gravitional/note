# 猜猜看游戏教程

```rust
use std::io;

fn main() {
    println!("Guess the number!");

    println!("Please input your guess.");

    let mut guess = String::new();

    io::stdin().read_line(&mut guess)
        .expect("Failed to read line");

    println!("You guessed: {}", guess);
}
```

## 处理一次猜测

为了获取用户输入并打印结果作为输出, 我们需要将 `io`(输入/输出)库引入当前作用域. `io` 库来自于标准库(也被称为 `std`):

```rust
use std::io;
```

默认情况下, `Rust` 将 `prelude` 模块中少量的类型引入到每个程序的作用域中.
如果需要的类型不在 `prelude` 中, 你必须使用 `use` 语句显式地将其引入作用域.
`std::io` 库提供很多有用的功能, 包括接收用户输入的功能.

如第一章所提及, `main` 函数是程序的入口点:

```rust
fn main() {
```

`fn` 语法声明了一个新函数, `()` 表明没有参数, `{ `作为函数体的开始.

第一章也提及了 `println!` 是一个在屏幕上打印字符串的宏:

## 使用变量储存值

接下来, 创建一个储存用户输入的地方, 像这样:

```rust
let mut guess = String::new();
```

这一小行代码发生了很多事. 注意这是一个 `let` 语句, 用来创建 `变量(variable)`. 这里是另外一个例子:

```rust
let apples = 5;
```

这行代码新建了一个叫做 `apples` 的变量并把它绑定到值 `5` 上. 在 `Rust` 中, 变量默认是`不可变`的.
我们将会在第三章的 "变量与可变性" 部分详细讨论这个概念.
下面的例子展示了如何在变量名前使用 `mut` 来使一个变量可变:

```rust
let apples = 5; // immutable
let mut bananas = 5; // mutable
```

>注意: `//` 语法开始一个注释, 持续到行尾. `Rust` 忽略注释中的所有内容, 第三章将会详细介绍注释.

让我们回到猜猜看程序中.

现在我们知道了 `let mut guess` 会引入一个叫做 `guess` 的`可变变量`.
等号`(=)`的右边是 `guess` 所绑定的值, 它是 `String::new` 的结果, 这个函数会返回一个 `String` 的新`实例`.
`String` 是一个`标准库`提供的`字符串`类型, 它是 `UTF-8` 编码的可增长文本块.

`::new` 那一行的 `::` 语法表明 `new` 是 `String` 类型的一个 `关联函数(associated function)`.
`关联函数`是针对`类型`实现的, 在这个例子中是 `String`.

`new` 函数创建了一个新的空字符串, 你会发现很多类型上有 `new` 函数, 因为它是创建`类型实例`的惯用函数名.

总结一下, `let mut guess = String::new()`; 这一行创建了一个`可变变量`, 当前它绑定到一个新的 `String 空实例`上.

回忆一下, 我们在程序的第一行使用 `use std::io`; 从标准库中引入了`输入/输出`功能. 现在调用 `io` 库中的函数 `stdin`:

```rust
io::stdin().read_line(&mut guess)
    .expect("Failed to read line");
```

如果程序的开头没有 `use std::io` 这一行, 可以把函数调用写成 `std::io::stdin`.
`stdin` 函数返回一个 `std::io::Stdin` 的实例, 这代表终端`标准输入句柄`的类型.

代码的下一部分, `.read_line(&mut guess)`, 调用 `read_line` 方法从标准输入句柄获取`用户输入`.
我们还向 `read_line()` 传递了一个参数: `&mut guess`.

`read_line` 的工作是接收用户在`标准输入`中输入的任何内容, 并将其追加到一个`字符串`中(不覆盖其内容), 因此它将该`字符串`作为参数.
这个`字符串`参数需要是`可变`的, 以便该`方法`可以通过添加用户输入来改变`字符串`的内容.

`&` 表示这个`参数`是一个 `引用(reference)`, 它允许多处代码访问同一处数据, 而无需在内存中多次`拷贝`.
`引用`是一个复杂的特性, `Rust` 的一个主要优势就是安全而简单的操纵引用.  完成当前程序并不需要了解如此多细节.
现在, 我们只需知道它像`变量`一样, 默认是`不可变的`. 因此, 需要写成 `&mut guess` 来使其可变, 而不是 `&guess`. (第四章会更全面的解释引用. )

## 使用 Result 类型来处理潜在的错误

后一部分是这个方法:

```rust
.expect("Failed to read line");
```

当使用 `.method_name()` 语法调用方法时, 通过`换行加缩进`来把长行拆开是明智的. 我们完全可以这样写:

```rust
io::stdin().read_line(&mut guess).expect("Failed to read line");
```

不过, 过长的行难以阅读, 所以最好拆开来写, 两个`方法调用`占两行. 现在来看看这行代码干了什么.

之前提到了 `read_line` 将用户输入附加到传递给它的字符串中, 不过它也返回一个值 -- 在这个例子中是 `io::Result`.
`Rust` 标准库中有很多叫做 `Result` 的类型: 一个通用的 `Result` 以及在`子模块`中的特化版本, 比如 `io::Result`.

`Result` 类型是 `枚举(enumerations)`, 通常也写作 `enums`.
枚举类型持有`确定集合`的值, 这些`值`被称为枚举的 `成员(variants)`. 第六章将介绍枚举的更多细节.

`Result` 的成员是 `Ok` 和 `Err`, `Ok` 成员表示`操作成功`, 内部包含成功时产生的`值`.
`Err` 成员则意味着`操作失败`, 并且包含失败的`前因后果`.

这些 `Result` 类型的作用是编码`错误处理信息`. `Result` 类型的值, 像其他类型一样, 拥有定义于其上的`方法`. `io::Result` 的实例拥有 `expect` 方法.
 如果 `io::Result` 实例的值是 `Err`, `expect` 会导致程序崩溃, 并显示传递给 `expect` 的参数(在这里是`"Failed to read line"`).
如果 `read_line` 方法返回 `Err`, 则可能是来源于底层操作系统错误的结果.
如果 `io::Result` 实例的值是 `Ok`, `expect` 会获取 `Ok` 中的`值`并原样返回.
在本例中, 这个`值`是用户输入到`标准输入`中的`字节数`.

如果不调用 `expect`, 程序也能`编译`, 不过会出现一个`警告`:

    $ cargo build
    Compiling guessing_game v0.1.0 (file:///projects/guessing_game)
    warning: unused `Result` that must be used
    --> src/main.rs:10:5
    |
    10 |     io::stdin().read_line(&mut guess);
    |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |
    = note: `#[warn(unused_must_use)]` on by default
    = note: this `Result` may be an `Err` variant, which should be handled

    warning: 1 warning emitted

        Finished dev [unoptimized + debuginfo] target(s) in 0.59s

`Rust` 警告我们没有使用 `read_line` 的返回值 `Result`, 说明有一个可能的`错误`没有处理.

`消除警告`的正确做法是实际编写`错误处理代码`, 不过由于我们就是希望程序在出现问题时立即崩溃, 所以直接使用 `expect`. 第九章会学习如何从错误中恢复.

## 使用 `println!` 占位符打印值

除了位于结尾的`右花括号`, 目前为止就只有这一行代码值得讨论一下了:

```rust
println!("You guessed: {}", guess);
```

这行代码打印存储用户输入的`字符串`.
第一个参数是格式化`字符串`, 里面的 `{}` 是预留在特定位置的`占位符`.
使用 `{}` 也可以打印多个值: 第一对 `{}` 使用格式化`字符串`之后的第一个值, 第二对则使用第二个值, 依此类推.
调用一次 `println!` 打印多个值看起来像这样:

```rust
let x = 5;
let y = 10;

println!("x = {} and y = {}", x, y);
```

这行代码会打印出 `x = 5 and y = 10`.

## 测试第一部分代码

让我们来测试下猜猜看游戏的第一部分. 使用 `cargo run` 运行:

    $ cargo run
    Compiling guessing_game v0.1.0 (file:///projects/guessing_game)
        Finished dev [unoptimized + debuginfo] target(s) in 6.44s
        Running `target/debug/guessing_game`
    Guess the number!
    Please input your guess.
    6
    You guessed: 6

至此为止, 游戏的第一部分已经完成: 我们从键盘获取输入并打印了出来.

## 生成一个秘密数字

接下来, 需要生成一个秘密数字, 好让用户来猜.
秘密数字应该每次都不同, 这样重复玩才不会乏味; 范围应该在 `1` 到 `100` 之间, 这样才不会太困难.
`Rust` 标准库中尚未包含随机数功能. 然而, `Rust` 团队还是提供了一个 `rand crate`.

### 使用 crate 来增加更多功能

记住, `crate` 是一个 `Rust` 代码包.
我们正在构建的项目是一个 二进制 `crate`, 它生成一个`可执行文件`.
`rand crate` 是一个 `库 crate`, `库 crate` 可以包含任意能被其他程序使用的代码.

`Cargo` 对外部 `crate` 的运用是其真正的亮点所在.
在我们使用 `rand` 编写代码之前, 需要修改 `Cargo.toml` 文件, 引入一个 `rand` 依赖.
现在打开这个文件并将下面这一行添加到 `[dependencies]` 片段标题之下.
请确保按照我们这里的方式指定 `rand`, 否则本教程中的代码示例可能无法工作.

文件名: `Cargo.toml`

    [dependencies]
    rand = "0.8.3"

在 `Cargo.toml` 文件中, `标题`以及之后的内容属同一个`片段`, 直到遇到下一个`标题`才开始新的`片段`.
`[dependencies]` 片段告诉 `Cargo` 本项目依赖了哪些外部 `crate` 及其版本.
本例中, 我们使用语义化版本 `0.8.3` 来指定 `rand crate. Cargo` 理解语义化版本(Semantic Versioning)(有时也称为 `SemVer`), 这是一种定义版本号的标准.
`0.8.3` 事实上是 `^0.8.3` 的简写, 它表示 `任何与 0.8.3 版本公有 API 相兼容的版本`.

现在, 不修改任何代码, 构建项目, 如示例 2-2 所示:

    $ cargo build
        Updating crates.io index
      Downloaded rand v0.8.3
      ....
       Compiling guessing_game v0.1.0 (file:///projects/guessing_game)
        Finished dev [unoptimized + debuginfo] target(s) in 2.53s

示例 2-2: 将 `rand crate` 添加为依赖之后运行 `cargo build` 的输出

可能会出现不同的`版本号`(多亏了`语义化`版本, 它们与代码是兼容的! ), 不同的`行`(取决于操作系统), 同时`显示顺序`也可能会有所不同.

因为我们有了一个`外部依赖`, `Cargo` 从 `registry` 上获取所有`包`的最新`版本信息`, `registry`是一份来自 `Crates.io` 的`数据拷贝`.
[Crates.io](https://crates.io/) 是 `Rust` 生态环境中的开发者们向他人贡献 `Rust` 开源项目的地方.

在更新完 `registry` 后, `Cargo` 检查 `[dependencies]` 片段并下载缺失的 `crate` .
本例中, 虽然只声明了 `rand` 一个`依赖`, 然而 `Cargo` 还是额外获取了 `rand` 所需要的其他 `crates`, 因为 `rand` 依赖它们来正常工作.
下载完成后, `Rust` 编译`依赖`, 然后使用这些`依赖`编译项目.

如果不做任何修改, 立刻再次运行 `cargo build`, 则不会看到任何除了 `Finished` 行之外的输出.
`Cargo` 知道它已经下载并编译了依赖, 同时 `Cargo.toml` 文件也没有变动.
`Cargo` 还知道代码也没有任何修改, 所以它不会重新`编译`代码. 因为无事可做, 它简单的退出了.

如果打开 `src/main.rs` 文件, 做一些无关紧要的修改, 保存并再次`构建`, 则会出现两行输出:

    $ cargo build
    Compiling guessing_game v0.1.0 (file:///projects/guessing_game)
        Finished dev [unoptimized + debuginfo] target(s) in 2.53 secs

这一行表示 `Cargo` 只针对 `src/main.rs` 文件的微小修改而更新构建.
`依赖`没有变化, 所以 `Cargo` 知道它可以`复用`已经为此下载并`编译`的代码. 它只是重新构建了部分(项目)代码.

### Cargo.lock 文件确保构建是可重现的

`Cargo` 有一个机制来确保任何人在任何时候重新构建代码, 都会产生相同的结果:
`Cargo` 只会使用你指定的依赖版本, 除非你又手动指定了别的.
例如, 如果下周 `rand crate` 的 `0.8.4` 版本出来了, 它修复了一个重要的 `bug`, 同时也含有一个会破坏代码运行的缺陷, 这时会发生什么呢?

这个问题的答案是 `Cargo.lock` 文件. 它在第一次运行 `cargo build` 时创建, 并放在 `guessing_game` 目录.
当第一次构建项目时, `Cargo` 计算出所有符合要求的依赖版本并写入 `Cargo.lock` 文件.
当将来构建项目时, `Cargo` 会发现 `Cargo.lock` 已存在并使用其中指定的版本, 而不是再次计算所有的版本.
这使得你拥有了一个自动化的可重现的`构建`.
换句话说, 项目会持续使用 `0.8.3` 直到你显式升级, 多亏有了 `Cargo.lock` 文件.

### 更新 crate 到一个新版本

当你 确实 需要升级 `crate` 时, `Cargo` 提供了另一个命令, `update`,
它会忽略 `Cargo.lock` 文件, 并计算出所有符合 `Cargo.toml` 声明的最新版本.
如果成功了, `Cargo` 会把这些版本写入 `Cargo.lock` 文件.

不过, `Cargo` 默认只会寻找大于 `0.8.3` 而小于 `0.9.0` 的版本.
如果 `rand crate` 发布了两个新版本, `0.8.4` 和 `0.9.0`, 在运行 `cargo update` 时会出现如下内容:

    $ cargo update
        Updating crates.io index
        Updating rand v0.8.3 -> v0.8.4

这时, 你也会注意到的 `Cargo.lock` 文件中的变化无外乎现在使用的 `rand crate` 版本是 `0.8.4`

如果想要使用 `0.9.0` 版本的 `rand` 或是任何 `0.9.x` 系列的版本, 必须像这样更新 `Cargo.toml` 文件:

```conf
[dependencies]
rand = "0.9.0"
```

下一次运行 `cargo build` 时, `Cargo` 会从 `registry` 更新可用的 `crate`, 并根据你指定的新版本重新计算.

第十四章会讲到 `Cargo` 及其生态系统 的更多内容, 不过目前你只需要了解这么多.
通过 `Cargo` 复用`库文件`非常容易, 因此 `Rustacean` 能够编写出由很多`包`组装而成的, 更轻巧的项目.

## 生成一个随机数

你已经把 `rand crate` 添加到 `Cargo.toml` 了, 让我们开始使用 `rand`. 下一步是更新 `src/main.rs`, 如示例 2-3 所示.
文件名: src/main.rs

```rust
use std::io;
use rand::Rng;

fn main() {
    println!("Guess the number!");
    let secret_number = rand::thread_rng().gen_range(1..101);
    println!("The secret number is: {}", secret_number);
    println!("Please input your guess.");
    let mut guess = String::new();
    io::stdin()
        .read_line(&mut guess)
        .expect("Failed to read line");

    println!("You guessed: {}", guess);
}
```

示例 2-3: 添加生成随机数的代码

首先, 我们新增了一行 `use: use rand::Rng`. `Rng` 是一个 `trait`, 它定义了`随机数生成器`应实现的方法, 想使用这些方法的话, 此 `trait` 必须在作用域中. 第十章会详细介绍 `trait`.

接下来, 我们在中间还新增加了两行. `rand::thread_rng` 函数提供实际使用的随机数生成器: 
它位于当前执行线程的本地环境中, 并从操作系统获取 `seed`. 接下来, 调用随机数生成器的 `gen_range` 方法. 

这个方法由刚才引入到作用域的 `Rng trait` 定义. 
`gen_range` 方法接受一个范围表达式作为参数, 并在该范围内生成一个随机数. 
我们在这里使用的范围表达式采用 `start..end` 形式, 它包含下限但不包含上限, 所以需要指定 `1..101` 来请求一个 `1` 和 `100` 之间的数. 
另外, 我们也可以传递 `1..=100`, 这是等价的.

注意: 你不可能凭空就知道应该 `use` 哪个 `trait` 以及该从 `crate` 中调用哪个方法. 
`crate` 的使用说明位于其文档中. 
`Cargo` 有一个很棒的功能是: 运行 `cargo doc --open` 命令来构建所有本地依赖提供的文档, 并在浏览器中打开. 
例如, 假设你对 `rand crate` 中的其他功能感兴趣, 你可以运行 `cargo doc --open` 并点击左侧导航栏中的 `rand`.

新增加的第二行代码打印出了`秘密数字`. 这在开发程序时很有用, 因为可以测试它, 不过在最终版本中会删掉它. 
如果游戏一开始就打印出结果就没什么可玩的了!

尝试运行程序几次:

```bash
$ cargo run
...
Guess the number!
The secret number is: 7
Please input your guess.
4
You guessed: 4
$ cargo run
...
Guess the number!
The secret number is: 83
Please input your guess.
5
You guessed: 5
```

你应该能得到不同的随机数, 同时它们应该都是在 `1` 和 `100` 之间的. 干得漂亮!

## 比较猜测的数字和秘密数字

现在有了用户输入和一个`随机数`, 我们可以`比较`它们. 
这个步骤如示例 2-4 所示. 注意这段代码还不能通过`编译`, 我们稍后会解释.

文件名: src/main.rs

```rust
// [这些代码不能编译! ]
use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main() {

    // ---snip---

    println!("You guessed: {}", guess);

    match guess.cmp(&secret_number) {
        Ordering::Less => println!("Too small!"),
        Ordering::Greater => println!("Too big!"),
        Ordering::Equal => println!("You win!"),
    }
}
```

示例 2-4: 处理比较两个数字可能的返回值

新代码的第一行是另一个 `use`, 从标准库引入了一个叫做 `std::cmp::Ordering` 的类型. 
同 `Result` 一样,  `Ordering` 也是一个枚举, 不过它的成员是 `Less`, `Greater` 和 `Equal`. 
这是比较两个值时可能出现的三种结果.

接着, 底部的五行新代码使用了 `Ordering` 类型, `cmp` 方法用来比较两个值并可以在任何可比较的值上调用. 
它获取一个被比较值的引用: 这里是把 `guess` 与 `secret_number` 做比较.  
然后它会返回一个刚才通过 `use` 引入作用域的 `Ordering` 枚举的成员. 
使用一个 `match` 表达式, 根据对 `guess` 和 `secret_number` 调用 `cmp` 返回的 `Ordering` 成员来决定接下来做什么.

一个 `match` 表达式由 分支(`arms`) 构成. 一个分支包含一个 模式(`pattern`)和表达式开头的值与分支模式相匹配时应该执行的代码. 
`Rust` 获取提供给 `match` 的值并挨个检查每个分支的模式. 
`match` 结构和模式是 `Rust` 中强大的功能, 它体现了代码可能遇到的多种情形, 并帮助你确保没有遗漏处理. 
这些功能将分别在第六章和第十八章详细介绍.

让我们看看使用 `match` 表达式的例子. 假设用户猜了 `50`, 这时随机生成的秘密数字是 `38`. 
比较 `50` 与 `38` 时, 因为 `50` 比 `38` 要大, `cmp` 方法会返回 `Ordering::Greater`. 
`Ordering::Greater` 是 `match` 表达式得到的值. 
它检查第一个分支的模式, `Ordering::Less` 与 `Ordering::Greater`并不匹配, 所以它忽略了这个分支的代码并来到下一个分支. 
下一个分支的模式是 `Ordering::Greater`, `正确` 匹配! 
这个分支关联的代码被执行, 在屏幕打印出 `Too big!`. 
`match` 表达式就此终止, 因为该场景下没有检查最后一个分支的必要.

然而, 示例 2-4 的代码并不能编译, 可以尝试一下:

```bash
$ cargo build
   Compiling guessing_game v0.1.0 (file:///projects/guessing_game)
error[E0308]: mismatched types
  --> src/main.rs:23:21
   |
23 |     match guess.cmp(&secret_number) {
   |                     ^^^^^^^^^^^^^^ expected struct `std::string::String`, found integer
...
```

错误的核心表明这里有 `不匹配的类型`(mismatched types). `Rust` 有一个静态强类型系统, 同时也有`类型推断`. 
当我们写出 `let guess = String::new()` 时, `Rust` 推断出 `guess` 应该是 `String` 类型, 并不需要我们写出类型. 
另一方面, `secret_number`, 是数字类型. 有几个数字类型拥有 `1` 到 `100` 之间的值: `32` 位数字 `i32`; `32` 位无符号数字 `u32`; `64` 位数字 `i64` 等等. 
`Rust` 默认使用 `i32`, 所以它是 `secret_number` 的类型, 除非增加类型信息, 或任何能让 `Rust` 推断出不同数值类型的信息. 
这里错误的原因在于 `Rust` 不会比较`字符串`类型和`数字`类型.

所以我们必须把从输入中读取到的 `String` 转换为一个真正的`数字`类型, 才好与`秘密数字`进行比较. 
这可以通过在 `main` 函数体中增加另一行代码来实现:

```rust
// --snip--

    let mut guess = String::new();

    io::stdin().read_line(&mut guess)
        .expect("Failed to read line");

    let guess: u32 = guess.trim().parse()
        .expect("Please type a number!");

    println!("You guessed: {}", guess);

    match guess.cmp(&secret_number) {
        Ordering::Less => println!("Too small!"),
        Ordering::Greater => println!("Too big!"),
        Ordering::Equal => println!("You win!"),
    }
}
```

这行新代码是:

    let guess: u32 = guess.trim().parse().expect("Please type a number!");

这里创建了一个叫做 `guess` 的变量. 不过等等, 不是已经有了一个叫做 `guess` 的变量了吗? 
确实如此, 不过 Rust 允许用一个新值来 隐藏 (shadow) `guess` 之前的值. 这个功能常用在需要转换值类型之类的场景. 
它允许我们复用 `guess` 变量的名字, 而不是被迫创建两个不同变量, 诸如 `guess_str` 和 `guess2` 之类. (第三章会介绍 shadowing 的更多细节. )

我们将 `guess` 绑定到 `guess.trim().parse()` 表达式上. 
表达式中的 `guess` 是包含输入的原始 `String` 类型. `String` 实例的 `trim` 方法会去除字符串开头和结尾的空白字符. 
`u32` 只能由数字字符转换, 不过用户必须输入 `enter` 键才能让 `read_line` 返回, 然而用户按下 `enter` 键时, 会在字符串中增加一个`换行(`newline)符. 
例如, 用户输入 `5` 并按下 `enter`(在 Windows 上, 按下 enter 键会得到一个回车符和一个换行符, `\r\n`), `guess` 看起来像这样: `5\n` 或者 `5\r\n`. 
`\n` 代表 `换行`; `\r` 代表 `回车`. `trim` 方法会消除 `\n` 或者 `\r\n`, 只留下 `5`.

字符串的 `parse` 方法 将字符串解析成数字. 
因为这个方法可以解析多种数字类型, 因此需要告诉 `Rust` 具体的数字类型, 这里通过 `let guess: u32` 指定. 
`guess` 后面的`冒号`(`:`)告诉 `Rust` 我们指定了变量的类型. `Rust` 有一些内建的`数字类型`; 
`u32` 是一个`无符号`的 `32 位整型`. 对于不大的正整数来说, 它是不错的类型, 第三章还会讲到其他`数字类型`. 

另外, 程序中的 `u32` 注解以及与 `secret_number` 的比较, 意味着 `Rust` 会推断出 `secret_number` 也是 `u32` 类型. 
现在可以使用相同类型比较两个值了!

`parse` 调用很容易产生错误. 例如, 字符串中包含 `A👍%`, 就无法将其转换为一个数字. 因此, `parse` 方法返回一个 `Result` 类型. 
像之前 `使用 Result 类型来处理潜在的错误` 讨论的 `read_line` 方法那样, 再次按部就班的用 `expect` 方法处理即可.

如果 `parse` 不能从字符串生成一个数字, 返回一个 `Result` 的 `Err` 成员时, `expect` 会使游戏崩溃并打印附带的信息. 
如果 `parse` 成功地将字符串转换为一个数字, 它会返回 `Result` 的 `Ok` 成员, 然后 `expect` 会返回 `Ok` 值中的数字.

现在让我们运行程序!

```bash
$ cargo run
...
Guess the number!
The secret number is: 58
Please input your guess.
  76
You guessed: 76
Too big!
```

漂亮! 即便是在猜测之前添加了空格, 程序依然能判断出用户猜测了 `76`.
现在游戏已经大体上能玩了, 不过用户只能猜一次. 增加一个循环来改变它吧!

## 使用循环来允许多次猜测

`loop` 关键字创建了一个无限循环. 将其加入后, 用户可以反复猜测:

```rust
// --snip--

    println!("The secret number is: {}", secret_number);

    loop {
        println!("Please input your guess.");

        // --snip--

        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal => println!("You win!"),
        }
    }
}
```

如上所示, 我们将提示用户猜测之后的所有内容放入了循环. 
确保 `loop` 循环中的代码多缩进四个空格, 再次运行程序. 
注意这里有一个新问题, 因为程序忠实地执行了我们的要求: 永远地请求另一个猜测, 用户好像无法退出啊!

用户总能使用 `ctrl-c` 终止程序. 不过还有另一个方法跳出无限循环, 就是 `比较猜测与秘密数字` 部分提到的 `parse`:
如果用户输入的答案不是一个数字, 程序会崩溃. 用户可以利用这一点来退出, 如下所示:

```bash
$ cargo run
...
You guessed: 59
You win!
Please input your guess.
quit
thread 'main' panicked at 'Please type a number!: ParseIntError { kind: InvalidDigit }', src/main.rs:28:47
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
```

输入 `quit` 确实退出了程序, 同时其他任何非数字输入也一样. 
然而, 这并不理想, 我们想要当猜测正确的数字时游戏能自动退出.

## 猜测正确后退出

让我们增加一个 `break` 语句, 在用户猜对时退出游戏:

```rust
// --snip--

        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal => {
                println!("You win!");
                break;
            }
        }
    }
}
```

通过在 `You win!` 之后增加一行 `break`, 用户猜对了神秘数字后会退出循环. 
退出循环也意味着退出程序, 因为循环是 `main` 的最后一部分.

## 处理无效输入

为了进一步改善游戏性, 不要在用户输入非数字时崩溃, 需要忽略非数字, 让用户可以继续猜测. 
可以通过修改 `guess` 将 `String` 转化为 `u32` 那部分代码来实现, 如示例 2-5 所示:

```rust
// --snip--

io::stdin().read_line(&mut guess)
    .expect("Failed to read line");

let guess: u32 = match guess.trim().parse() {
    Ok(num) => num,
    Err(_) => continue,
};

println!("You guessed: {}", guess);

// --snip--
```

示例 2-5: 忽略非数字的猜测并重新请求数字而不是让程序崩溃

将 `expect` 调用换成 `match` 语句, 是从遇到错误就崩溃转换到真正处理错误的惯用方法. 
须知 `parse` 返回一个 `Result` 类型, 而 `Result` 是一个拥有 `Ok` 或 `Err` 成员的枚举.
 这里使用的 `match` 表达式, 和之前处理 `cmp` 方法返回 `Ordering` 时用的一样.

如果 `parse` 能够成功的将字符串转换为一个数字, 它会返回一个包含结果数字的 `Ok`. 
这个 `Ok` 值与 `match` 第一个分支的模式相匹配, 该分支对应的动作返回 `Ok` 值中的数字 `num`, 最后如愿变成新创建的 `guess` 变量.

如果 `parse` 不 能将字符串转换为一个数字, 它会返回一个包含更多错误信息的 `Err`. 
`Err` 值不能匹配第一个 `match` 分支的 `Ok(num)` 模式, 但是会匹配第二个分支的 `Err(_)` 模式: 
`_` 是一个通配符值, 本例中用来匹配所有 `Err` 值, 不管其中有何种信息. 
所以程序会执行第二个分支的动作, `continue` 意味着进入 `loop` 的下一次循环, 请求另一个猜测. 
这样程序就有效的忽略了 `parse` 可能遇到的所有错误!

现在万事俱备, 只需运行 `cargo run`:

```bash
$ cargo run
  ...
Please input your guess.
foo
Please input your guess.
61
You guessed: 61
You win!
```

太棒了! 再有最后一个小的修改, 就能完成猜猜看游戏了: 还记得程序依然会打印出秘密数字. 
在测试时还好, 但正式发布时会毁了游戏. 删掉打印秘密数字的 println!. 示例 2-6 为最终代码:

```rust
use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main() {
    println!("Guess the number!");

    let secret_number = rand::thread_rng().gen_range(1..101);

    loop {
        println!("Please input your guess.");

        let mut guess = String::new();

        io::stdin().read_line(&mut guess)
            .expect("Failed to read line");

        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => continue,
        };

        println!("You guessed: {}", guess);

        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal => {
                println!("You win!");
                break;
            }
        }
    }
}
```

示例 2-6: 猜猜看游戏的完整代码

## 总结

此时此刻, 你顺利完成了猜猜看游戏. 恭喜!

本项目通过动手实践, 向你介绍了 `Rust` 新概念: `let`, `match`, `方法`, `函数`, 使用外部 `crate` 等等, 
接下来的几章, 你会继续深入学习这些概念. 
第三章介绍大部分编程语言都有的概念, 比如变量, 数据类型和函数, 以及如何在 `Rust` 中使用它们. 
第四章探索所有权(`ownership`), 这是一个 `Rust` 同其他语言大不相同的功能. 
第五章讨论结构体和方法的语法, 而第六章侧重解释枚举. 
