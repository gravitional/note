# 项目建立

如果你还没有, 在你的电脑上安装 `Rust`(应该只需要几分钟时间).
之后, 打开一个终端, 导航到你想存放程序代码的目录.

首先, 在你存放编程项目的目录中运行 `cargo new grrs`.
如果你看一下新创建的 `grrs` 目录, 你会发现一个典型的Rust项目的设置.

+ `Cargo.toml` 文件, 包含我们项目的元数据, 包括我们使用的依赖/外部库的列表.
+ `src/main.rs` 文件, 是我们(主)二进制文件的入口.

如果你能在 `grrs` 目录下执行 `cargo run`, 并得到一个 `"Hello World"`, 你就完成了所有的设置.

它可能是这样的

```bash
$ cargo new grrs
     Created binary (application) `grrs` package
$ cd grrs/
$ cargo run
   Compiling grrs v0.1.0 (/Users/pascal/code/grrs)
    Finished dev [unoptimized + debuginfo] target(s) in 0.70s
     Running `target/debug/grrs`
Hello, world!
``
