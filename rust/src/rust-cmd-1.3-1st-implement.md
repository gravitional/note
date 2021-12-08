# grrs的第一个实现

[`.expect`]: https://doc.rust-lang.org/1.39.0/std/result/enum.Result.html#method.expect

在上一章关于命令行参数的内容之后, 我们有了我们的输入数据, 我们可以开始编写我们的实际工具.
我们的 `main` 函数现在只包含这一行.

```rust
let args = Cli::from_args();
```

让我们从打开指定的文件开始:

```rust
let content = std::fs::read_to_string(&args.path)
    .expect("找不到文件");
```

>旁白: 看到这里的 [`.expect`] 方法了吗? 这是一个`quit`的快捷函数, 当`值`(这里指输入文件)无法被读取时, 它将使程序立即退出.
这不是很美观, 在下一章 "更好的错误报告" 中, 我们将研究如何改进这个问题.

现在, 让我们对文件的行进行迭代, 并打印每一个包含目标模式的行.

```rust
for line in content.lines() {
    if line.contains(&args.pattern) {
        println!("{}", line);
    }
}
```

试一试:  `cargo run -- main src/main.rs` 现在应该可以工作了!

>给读者做个练习: 这并不是最好的实现. 它将把整个文件读入内存--不管文件有多大.
>找到一种方法来优化它吧, 一个想法可能是使用 [BufReader][] 来代替 `read_to_string()`.

[BufReader]: https://doc.rust-lang.org/1.39.0/std/io/struct.BufReader.html
