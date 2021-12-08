# 解析命令行参数

我们的 `CLI` 工具的典型调用将看起来像这样.

```bash
$ grrs foobar test.txt
```

我们希望我们的程序能够查看 `test.txt`, 并打印出包含 `foobar` 的行. 但我们如何获得这两个值呢?

程序名称后面的文字通常被称为 `命令行参数`, 或 `命令行标志`(特别是当它们看起来像 `--this` 的时候).
在内部, 操作系统通常将它们表示为一个`字符串列表` --大致上说, 它们被空格隔开.

有很多方法可以考虑这些参数, 以及如何将它们`解析`(parsing) 成更容易操作的东西.
你还需要告诉你的程序的用户, 他们需要给出哪些参数, 以及他们应该以何种格式给出.

## 获取参数

标准库包含函数 [`std::env::args()`][], 它给你一个给定参数的迭代器.
第一个条目, 即索引`0` 将是你的程序被调用时的名称(例如 `grrs`), 后面的是用户后续提供的内容.

通过这种方式获取 `原始参数`是非常容易的(在文件 `src/main.rs` 中, 插入到 `fn main() {` 之后):

```rust
let pattern = std::env::args().nth(1).expect("没有给出模式");
let path = std::env::args().nth(2).expect("没有给出路径");
```

[`std::env::args()`]: https://doc.rust-lang.org/1.39.0/std/env/fn.args.html

## 作为数据类型的CLI参数

与其把它们看作是一堆文本, 不如把 `CLI` 参数看作是`自定义数据类型`, 代表你的程序的输入, 这样做往往会有好处.

看看 `grrs foobar test.txt`, 有两个参数, 首先是`模式`(要查找的字符串), 然后是`路径`(要查找的文件).

关于它们我们还能说什么呢? 嗯, 首先, 这两个参数都是必须的.
我们还没有谈到任何默认值, 所以我们希望我们的用户总是提供两个值.

此外, 我们还可以对它们的类型做一些说明.
我们希望 `pattern` 是`字符串`, 而第二个参数是文件的路径.

在 Rust 中, 围绕它们所处理的数据来构造程序是很常见的, 所以这种看待 `CLI` 参数的方式非常合适.
让我们从这个开始(在文件 `src/main.rs` 中, 在 `fn main() {` 之前).

```rust
struct Cli {
    pattern: String,
    path: std::path::PathBuf,
}
```

这就定义了一个新的结构, [ struct](https://doc.rust-lang.org/1.39.0/book/ch05-00-structs.html),
它有两个字段来存储数据: `pattern` 和 `path`.

>顺便说一下: [`PathBuf`][] 就像 [`String`][], 但用于记录文件系统路径, 可以跨平台工作.

现在, 我们仍然需要把我们的程序得到的实际参数变成这种形式.
一个选择是手动解析我们从操作系统得到的字符串列表, 并自己建立结构. 它看起来就像这样.

```rust
let pattern = std::env::args().nth(1).expect("没有给出模式");
let path = std::env::args().nth(2).expect("没有给出路径");
let args = Cli {
    pattern: pattern,
    path: std::path::PathBuf::from(path),
};
```

这样做可以, 但不是很方便. 你将如何处理支持 `--pattern="foo"` 或 `--pattern "foo"` 的要求?
你将如何实现 `--help`?

[`PathBuf`]: https://doc.rust-lang.org/1.39.0/std/path/struct.PathBuf.html
[`String`]: https://doc.rust-lang.org/1.39.0/std/string/struct.String.html

## 用StructOpt解析CLI参数

[structopt 库]: https://docs.rs/structopt/0.3.25/structopt/

更好的方法从许多可用的 lib 中挑一个.  最流行的解析命令行参数的库被称为 [clap](https://clap.rs/).
它拥有你所期望的所有功能, 包括对子命令的支持, shell补全和 很棒的帮助信息.

[structopt 库][] 建立在 `clap` 的基础上, 提供了 `derive` 宏, 为 `struct` 定义生成 `clap` 代码, 这太棒了.
我们所要做的就是 `annotate`  一个 `struct`, 它将生成代码, 将参数(arguments) 解析到 `字段`(fields)中.

首先, 我们在 `Cargo.toml` 文件的`[dependencies]`部分添加 `structopt = "0.3.13"` 来导入 `structopt`.

现在, 我们可以在代码中写上 `use structopt::StructOpt;`,并在我们的 `struct Cli` 上面添加 `#[derive(StructOpt)]`.
我们还可以顺便写一些文档注释(comment).
看起来像这样(在文件 `src/main.rs` 中, 在 `fn main() {` 之前).

```rust
use structopt::StructOpt;

/// 在文件中搜索一个 pattern, 并显示包含该模式的行.
#[derive(StructOpt)]
struct Cli {
    /// 要寻找的模式.
    pattern: String,
    /// 要读取的文件.
    #[structopt(parse(from_os_str))]
    path: std::path::PathBuf,
}
```

>注意: 你可以为 `字段` 添加很多自定义 `attributes`(属性).
>例如, 我们添加了一个`attributes`来告诉 `structopt` 如何解析 `PathBuf` 类型.
>比如说你想用这个字段作为 `-o` 或 `--output` 之后的参数, 你可以添加 `#[structopt(short = "o", long = "output")]`.
>更多信息请参见 [structopt 文档](https://docs.rs/structopt/0.3.25/structopt/).

在 `Cli` 结构的正下方, 我们的模板包含了它的 `main`函数. 当程序启动时, 它将调用这个函数. 第一行是:

```rust
fn main() {
    let args = Cli::from_args();
}
```

这将尝试将`参数`解析到我们的 `Cli` 结构体中. 但如果失败了呢?
这就是这种方法的魅力所在. `Clap` 知道哪些字段是需要的, 以及它们的预期格式是什么.
它可以自动生成一个漂亮的 `--help` 消息, 以及给出一些很好的`errors`,
如果你拼写错误, 例如 `--putput`,  它会建议你传递 `--output`.

>注意: `from_args` 方法是为了在你的`main`函数中使用.
>当它失败时, 它将打印出一个`错误`或`帮助`信息, 并立即退出程序. 不要在其他地方使用它!

## 这是它可能的模样

在没有任何参数的情况下运行它:

```bash
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 10.16s
     Running `target/debug/grrs`
error: The following required arguments were not provided:
    <pattern>
    <path>

USAGE:
    grrs <pattern> <path>

For more information try --help
```

我们可以在使用 `cargo run` 时直接传递参数, 把它们写在 `--` 后面.

```bash
$ cargo run -- some-pattern some-file
    Finished dev [unoptimized + debuginfo] target(s) in 0.11s
     Running `target/debug/grrs some-pattern some-file`
```

正如你所看到的, 没有任何输出. 这很好. 这意味着没有错误, 我们的程序结束了.

>给读者做个练习. 让这个程序输出它的参数!
