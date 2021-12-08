# 输出

```rust
println!("Hello World");
```

嗯, 这很容易. 很好, 进入下一个主题.

## 使用println

你几乎可以用 `println!` 宏来打印所有你喜欢的东西.
这个宏有一些相当惊人的功能, 但也有一个特殊的语法.
它希望你写一个字符串作为第一个`参数`(parameter), 其中包含占位符(placeholders),
这些占位符将被后面的参数(parameter)值填入, 作为进一步的`参数`(arguments).

比如说:

```rust
let x = 42;
println!("My lucky number is {}.", x);
```

将打印

```log
My lucky number is 42.
```

上面的字符串中的`大括号`(`{}`)是这些占位符之一.
这是默认的占位符类型, 它试图以人类可读的方式打印给定的值. 对于数字和字符串来说, 这非常好用, 但不是所有的类型都能做到这一点.
这就是为什么还有一个 "调试表示"(debug representation), 你可以通过填充占位符的大括号来获得: `{:?}`.

比如说:

```rust
let xs = vec![1, 2, 3];
println!("The list is: {:?}", xs);
```

将打印

```rust
The list is: [1, 2, 3]
```

如果你想让你自己的`数据类型`在调试和记录时可以打印, 在大多数情况下, 你可以在它们的定义上方添加 `#[derive(Debug)]`.

>旁白: "用户友好" 的打印使用 [Display][] trait, 调试输出(人类可读但针对开发者)使用 [Debug][] trait.
>你可以在[std::fmt模块的文档]中找到更多关于你可以在 `println!` 中使用的语法的信息.

[Display]: https://doc.rust-lang.org/1.39.0/std/fmt/trait.Display.html
[Debug]: https://doc.rust-lang.org/1.39.0/std/fmt/trait.Debug.html
[std::fmt模块的文档]: https://doc.rust-lang.org/1.39.0/std/fmt/index.html

## 打印错误

打印错误应该通过 `stderr` 来完成, 以使用户和其他工具, 更容易将他们的`output`s 输送到文件或更多工具.

>顺便说一下. 在大多数操作系统中, 一个程序可以写到两个输出流, `stdout` 和 `stderr`.
>`stdout` 是程序的实际输出, 而 `stderr` 允许错误和其他信息与 `stdout` 分开.
>这样一来, 输出可以被存储到一个文件中, 或者被输送到另一个程序中, 而 `errors` 则显示给用户.

在 `Rust` 中, 这是通过 `println!` 和 `eprintln!` 实现的, 前者打印到 `stdout`, 后者打印到 `stderr`.

```rust
println!("这是信息").
eprintln!("这是一个错误! :(")).
```

>请注意. 打印 [转义代码][] (escape code)可能很危险, 会使用户的终端进入一个奇怪的状态. 在手动打印它们时一定要小心!
>
>理想情况下, 在处理原始转义代码时, 你应该使用像 [ansi_term][] 这样的工具箱, 以使你(和你的用户)的生活更轻松.

[转义代码]: https://en.wikipedia.org/wiki/ANSI_escape_code
[ansi_term]: https://docs.rs/ansi_term/0.12.1/ansi_term/

### 关于打印性能的说明

[BufWriter]: https://doc.rust-lang.org/1.39.0/std/io/struct.BufWriter.html

打印到终端的速度出乎意料地慢!
如果你在一个循环中调用 `println!` 这样的东西, 它很容易成为一个快速程序的瓶颈(bottleneck).
为了加快这个速度, 有两件事你可以做.

首先, 你可能想减少实际 "刷新"(flush) 到终端的写入次数.
`println!` 告诉系统每次都刷新到终端, 因为打印每个新行是很常见的.
如果你不需要, 你可以把你的 `stdout` handle 包在 [BufWriter][] 中, `BufWriter` 默认的缓冲区最大为8 kB.
当你想立即打印时, 你仍然可以在这个 `BufWriter` 上调用 `.flush()`.

```rust
use std::io::{self, Write};

let stdout = io::stdout(); // get the global stdout entity
let mut handle = io::BufWriter::new(stdout); // optional: wrap that handle in a buffer
writeln!(handle, "foo: {}", 42); // add `?` if you care about errors here
```

其次, 在 `stdout`(或 `stderr`)上获取锁, 并使用 `writeln!` 直接打印到它, 会有帮助
这可以防止系统一次又一次地锁定和解锁 `stdout`.

```rust
use std::io::{self, Write};

let stdout = io::stdout(); // 获取全局stdout entity
let mut handle = stdout.lock(); // 获得对它的锁
writeln!(handle, "foo: {}", 42); // 如果你关心这里的错误, 可以添加`?`
```

你也可以把这两种方法结合起来.

## 显示一个进度条

[indicatif]: https://crates.io/crates/indicatif

有些 `CLI` 应用程序运行不到一秒钟, 有些则需要几分钟或几小时.
如果你正在编写后一种类型的程序, 你可能想向用户显示正在发生的事情.
为此, 你应该尝试打印有用的状态更新, 最好是以一种容易被消费(consumed)的形式.

使用 [indicatif][] crate, 你可以在你的程序中添加进度条和小转盘. 下面是一个快速的例子.

```rust
use std::thread;
use std::time::{Duration, Instant};
use indicatif::{HumanDuration, MultiProgress, ProgressBar, ProgressStyle};

fn main() {
    let pb = indicatif::ProgressBar::new(20);
    for i in 0..20 {
        thread::sleep(Duration::from_millis(10)); // 模拟一些操作
        pb.println(format!("[+] finished #{}", i));
        pb.inc(1);
    }
    pb.finish_with_message("done");
}
```

更多信息请参见[文档](https://docs.rs/indicatif/0.16.2/indicatif/)和[示例](https://github.com/mitsuhiko/indicatif/tree/main/examples).

## 日志

[`syslog`]: https://en.wikipedia.org/wiki/Syslog
[env_logger]: https://crates.io/crates/env_logger

为了更容易理解我们程序中发生的事情, 我们可能想添加一些`日志`语句(log).
这在编写应用程序时通常很容易. 但在半年后再次运行这个程序时, 它将变得非常有用.
在某些方面, 日志与使用 `println` 是一样的, 只是你可以指定消息的重要性.
你通常可以使用的级别是 `error`, `warn`, `info`, `debug` 和 `trace`
(`error` 有最高的优先级, `trace`最低).

要给你的应用程序添加简单的日志, 你需要两样东西. [日志](https://crates.io/crates/log) 包
(它包含以日志级别命名的 macros)和一个`adapter`, 它实际上是把日志输出写到有用的地方.
拥有使用`log adapters`的能力是非常灵活的.
例如, 你可以用它们来写 `logs`, 不仅写到终端, 也写到 [`syslog`][], 或者写到中央日志服务器(central log server).

由于我们现在只关心写一个 `CLI` 应用程序, 一个容易使用的适配器是 [env_logger][].
它被称为 `env` 记录器, 因为你可以使用环境变量, 来指定你想记录你应用程序的哪些部分(以及你想在哪个级别记录它们).
它将在你的日志信息前加上一个`时间戳`和日志信息来源的模块.
由于库也可以使用`log`, 你也可以轻松地配置它们的日志输出.

下面是一个快速的例子:

```rust
use log::{info, warn};

fn main() {
    env_logger::init();
    info!("启动");
    warn!("哎呀, 什么都没实现!");
}
```

假设你把这个文件作为 `src/bin/output-log.rs`, 在 `Linux` 和 `macOS` 上, 你可以像这样运行它.

```bash
$ env RUST_LOG=info cargo run --bin grrs
    Finished dev [unoptimized + debuginfo] target(s) in 0.17s
     Running `target/debug/output-log`
[2018-11-30T20:25:52Z INFO  output_log] 启动
[2018-11-30T20:25:52Z WARN  output_log] 哎呀, 什么都没实现!
```

在 `Windows PowerShell` 中, 你可以这样运行它.

```powershell
$ $env:RUST_LOG="info"
$ cargo run --bin output-log
    Finished dev [unoptimized + debuginfo] target(s) in 0.17s
     Running `target/debug/output-log.exe`
[2018-11-30T20:25:52Z INFO  output_log] 启动
[2018-11-30T20:25:52Z WARN  output_log] 哎呀, 什么都没实现!
```

在 `Windows CMD` 中, 你可以像这样运行它.

```cmd
$ set RUST_LOG=info
$ cargo run --bin output-log
    Finished dev [unoptimized + debuginfo] target(s) in 0.17s
     Running `target/debug/output-log.exe`
[2018-11-30T20:25:52Z INFO  output_log] 启动
[2018-11-30T20:25:52Z WARN  output_log] 哎呀, 什么都没实现!
```

`RUST_LOG` 是环境变量的名称, 你可以用它来配置日志.
`env_logger` 还包含一个构建器(builder), 所以你可以通过编程来调整这些设置, 例如, 也可以默认显示`info`级别的消息.

有很多替代性的 logging adapters , 也有`log`的替代品或扩展.
如果你知道你的应用程序会有很多需要记录的内容, 一定要查看它们, 让你的用户的生活更轻松.

提示: 经验表明, 即使是温和有用的 `CLI` 程序最终也会被使用多年. (特别是如果它们是作为一个临时的解决方案. )
如果你的应用程序不工作, 有人(例如, 你, 在未来)需要找出原因, 能够通过 `--verbose` 来获得额外的日志输出, 就会导致几分钟和几个小时的调试的天壤之别.
[clap-verbosity-flag][] 箱包含一个快速方法, 可以为使用`structopt` 的项目添加 `--verbose`.

[clap-verbosity-flag]: https://crates.io/crates/clap-verbosity-flag
