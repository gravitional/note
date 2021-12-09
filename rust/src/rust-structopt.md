# structopt

[structopt](https://lib.rs/crates/structopt)

通过定义一个 结构体, 解析命令行参数.

## 文档

在 [Docs.rs](https://docs.rs/structopt/latest/structopt/) 上找到它.
你也可以查看 [实例](https://github.com/TeXitoi/structopt/tree/master/examples)
和[更新日志](https://github.com/TeXitoi/structopt/blob/master/CHANGELOG.md).

## 例子

将 `structopt` 添加到你的 `Cargo.toml` 的依赖项中.

```toml
[dependencies]
structopt = "0.3"
```

然后, 在你的 `rust` 文件中

```rust
use std::path::PathBuf;
use structopt::StructOpt;

/// 一个基本的例子
#[derive(StructOpt, Debug)]
#[structopt(name = "basic")]
struct Opt {
    /// 一个 flag(标志), 如果在命令行中使用则为 true. 注意 doc注释将用于该 flag 的帮助信息.
    ///  默认情况下, 参数的名称将基于该字段的名称.
    /// 激活调试模式
    #[structopt(short, long)]
    debug: bool,

    /// `v/verbose`标志出现的次数
    /// Verbose 模式 (-v, -vv, -vvv等).
    #[structopt(short, long, parse(from_occurrences))]
    verbose: u8,

    /// 设置速度
    #[structopt(short, long, default_value = "42")]
    speed: f64,

    /// 输出文件
    #[structopt(short, long, parse(from_os_str))]
    output: PathBuf,

    // 长选项将被默认翻译成 kebab case, 即 `--nb-cars`.
    /// 汽车的数量
    #[structopt(short = "c", long)]
    nb_cars: Option<i32>,

    /// 需要考虑的管理级别
    #[structopt(short, long)]
    level: Vec<String>,

    /// 要处理的文件
    #[structopt(name = "FILE", parse(from_os_str))]
    files: Vec<PathBuf>,
}

fn main() {
    let opt = Opt::from_args();
    println!("{:#?}", opt);
}
```

使用这个例子, 运行下面的命令查看效果

```bash
$ cargo run

error: The following required arguments were not provided:
    --output <output>

USAGE:
    texmk --output <output> --speed <speed>

For more information try --help

$ cargo run -- --help

$ cargo run -- -o foo.txt

$ cargo run -- -o foo.txt -dvvvs 1337 -l alice -l bob --nb-cars 4 bar.txt baz.txt
```
