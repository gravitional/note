# 将错误信息输出到标准错误而不是标准输出

目前为止, 我们将所有的输出都 `println!` 到了终端.
大部分终端都提供了两种输出: 标准输出(standard output, stdout)对应一般信息, 标准错误(standard error, stderr)则用于错误信息.
这种区别允许用户选择将程序的正常输出定向到一个文件, 同时将错误信息打印到屏幕上.

但是 println! 函数只能够打印到标准输出, 所以我们必须使用其他方法来打印到标准错误.

## 检查错误应该写入何处

首先, 让我们观察一下目前 minigrep 打印的所有内容是如何被写入标准输出的, 包括那些应该被写入标准错误的错误信息.
可以通过将标准输出流重定向到文件,  同时有意产生一个错误来做到这一点.
我们没有重定向`标准错误流`, 所以任何发送到标准错误的内容将会继续显示在屏幕上.

命令行程序被期望将`错误信息`发送到标准错误流, 这样即便选择将标准输出流重定向到文件中时, 仍然能看到错误信息.
目前我们的程序并不符合期望; 相反我们将看到它将错误信息输出保存到了文件中.

我们通过 `>` 和文件名 output.txt 来运行程序, 我们期望重定向标准输出流到该文件中.
在这里, 我们没有传递任何参数, 所以会产生一个错误:

```rust
$ cargo run > output.txt
```

`>` 语法告诉 shell 将标准输出的内容写入到 output.txt 文件中而不是屏幕上.
我们并没有看到期望的错误信息打印到屏幕上, 所以这意味着它一定被写入了文件中. 如下是 output.txt 所包含的:

```log
Problem parsing arguments: not enough arguments
```

是的, 错误信息被打印到了标准输出中. 而`错误信息`被打印到标准错误中将会有用得多, 如此, 只有成功运行所产生的输出才会写入文件.
我们接下来就修改.

## 将错误打印到标准错误

让我们使用 `示例 12-24` 的代码, 改变错误信息的打印通道.
得益于本章早些时候的重构, 所有打印错误信息的代码都位于单个 `main` 函数中.
标准库提供了 eprintln! 宏来打印到标准错误流, 所以在两处调用 println! 打印错误信息的位置, 修改为 eprintln!:

文件名: src/main.rs

```rust
fn main() {
    let args: Vec<String> = env::args().collect();

    let config = Config::new(&args).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        process::exit(1);
    });

    if let Err(e) = minigrep::run(config) {
        eprintln!("Application error: {}", e);

        process::exit(1);
    }
}
```

示例 12-24: 使用 eprintln! 将错误信息写入标准错误而不是标准输出

将 println! 改为 eprintln! 之后, 让我们再次尝试用同样的方式运行程序, 不使用任何参数并通过 `>` 重定向标准输出:

```log
$ cargo run > output.txt
Problem parsing arguments: not enough arguments
```

现在我们看到了屏幕上的错误信息, 同时 output.txt 里什么也没有, 这正是命令行程序所期望的行为.

让我们使用不造成错误的参数再次运行程序, 仍然将标准输出重定向到一个文件, 像这样:

```bash
$ cargo run to poem.txt > output.txt
```

我们并不会在终端看到任何输出, 同时 output.txt 将会包含其结果:

文件名: output.txt

```log
Are you nobody, too?
How dreary to be somebody!
```

这说明, 现在我们使用标准输出打印成功时的输出, 使用标准错误(standard error)打印错误时的输出, 这很合理.

## 总结

在这一章中, 我们回顾了目前为止的一些主要章节, 并涉及了如何在 Rust 环境中进行常规的 I/O 操作.
通过使用 `命令行参数`, `文件`, `环境变量` 和 打印错误的 `eprintln! 宏`, 现在你已经准备好编写命令行程序了.
通过结合前几章的知识, 你的代码将会是组织良好的,
并能有效的将数据存储到合适的数据结构中, 更好的处理错误, 并且还是经过良好测试的.

接下来, 让我们探索一些 Rust 中受函数式编程语言影响的功能: `闭包` 和 `迭代器`.
