# 更好的错误报告

我们都只能接受`错误`发生的事实. 而与其他许多语言相比, 在使用Rust时很难不注意和处理这个现实.
由于它没有`异常`(exceptions), 所有可能的错误状态通常都被编码在函数的返回类型(type)中.

## 结果

[`read_to_string`]: https://doc.rust-lang.org/1.39.0/std/fs/fn.read_to_string.html
[`Result`]: https://doc.rust-lang.org/1.39.0/std/result/index.html
[`std::io::Error`]: https://doc.rust-lang.org/1.39.0/std/io/type.Result.html

像 [`read_to_string`][] 这样的函数并不返回一个字符串.
相反, 它返回一个[`Result`][], 其中包含一个`字符串`, 或某种类型的错误(在这里是 [`std::io::Error`]).

你怎么知道它是哪一种呢? 因为 `Result` 是一个枚举, 你可以用 `match` 来检查它具体是哪个变量:

```rust
#![allow(unused)]
fn main() {
let result = std::fs::read_to_string("test.txt");
match result {
    Ok(content) => { println!("File content: {}", content); }
    Err(error) => { println!("Oh noes: {}", error); }
}
}
```

>另外: 不知道什么是枚举或者它们在Rust中如何工作? 请看[Rust书中的这一章](https://doc.rust-lang.org/1.39.0/book/ch06-00-enums.html)来了解一下.

## 解除包装

现在, 我们能够访问文件的内容, 但在`match`块之后, 我们不能真正对它做任何事情.
为此, 我们需要以某种方式来处理错误的情况. 挑战在于, 匹配块的所有`分支`(arms)都需要返回相同`类型`的东西.
但有一个巧妙的技巧可以解决这个问题.

```rust
#![allow(unused)]
fn main() {
let result = std::fs::read_to_string("test.txt");
let content = match result {
    Ok(content) => { content },
    Err(error) => { panic!("Can't deal with {}, just exit here", error); }
};
println!("file content: {}", content);
}
```

我们可以在`match`块之后, 使用`content`中的字符串. 如果结果是一个错误, 这个字符串就不会存在.
但由于程序在到达我们使用 `content` 的地方之前就会退出, 所以它没有问题.

这可能看起来很`激烈`(drastic), 但它非常方便.
如果你的程序需要读取该文件, 而如果该文件不存在, 就不能做任何事情, 退出是一个有效的策略.
在 `Results` 上甚至有一个快捷的方法, 叫做 `unwrap`.

```rust
fn main() {
let content = std::fs::read_to_string("test.txt").unwrap();
}
```

## 不需要惊慌

当然, 中止程序不是处理错误的唯一方法. 取代`panic!`, 我们也可以轻松地使用`return`.

```rust
let result = std::fs::read_to_string("test.txt");
let _content = match result {
    Ok(content) => { content },
    Err(error) => { return Err(error.into()); }
};
```

然而, 这改变了我们的函数需要的返回类型.
事实上, 一直以来, 我们的例子中隐藏着一些东西. 这个代码所处的函数签名.
而在这最后一个使用`return`的例子中, 它变得很重要. 下面是完整的代码:

```rust
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let result = std::fs::read_to_string("test.txt");
    let content = match result {
        Ok(content) => { content },
        Err(error) => { return Err(error.into()); }
    };
    println!("file content: {}", content);
    Ok(())
}
```

我们的返回类型是一个结果! 这就是为什么我们可以在第二个匹配臂中写 `return Err(error);`.
看到底部有一个 `Ok(())` 了吗? 这是该函数的默认返回值, 意味着 "结果没问题, 没有内容".

>旁白: 为什么不写成 `return Ok(());`? 它很容易就能做到--这也是完全有效的.
>在Rust中, 任何块的最后一个表达式都是它的返回值, 而且习惯上要省略不必要的`return`s.

## 问题标记

[`std::io::Error`]: https://doc.rust-lang.org/1.39.0/std/io/type.Result.html
[Error]: https://doc.rust-lang.org/1.39.0/std/error/trait.Error.html

就像调用 `.unwrap()` 是错误臂中带 `panic!` 的`match`的快捷方式一样,
对于`match`, 我们还有另一个快捷方式在错误臂中`return`: `?`

我没写错, 一个问号.  你可以将这个操作符附加到一个结果类型的值上, Rust会在内部将其扩展为与我们刚才写的匹配非常相似的东西.
试一试吧:

```rust
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = std::fs::read_to_string("test.txt")?;
    println!("file content: {}", content);
    Ok(())
}
```

非常简明!

>旁白: 这里还发生了一些不需要理解的事情, 在这个阶段. 例如, 我们主函数中的错误类型是`Box<dyn std::error::Error>`.
但是我们在上面看到, `read_to_string` 返回一个 [`std::io::Error`]. 这就可以了, 因为`?` 将展开为代码, 后者 converts 错误类型.

`Box<dyn std::error::Error>`也是一个有趣的 type. 它是一个盒子, 可以包含任何实现了标准[Error][] trait的 type.
这意味着基本上所有的错误都可以放在这个盒子里, 所以我们可以在所有返回 `Result`s 的常规函数上使用`?`

## 提供上下文

在`main`函数中使用 `?` 时, 你得到的错误是可以的, 但不是很好.
比如说: 当你运行 `std::fs::read_to_string("test.txt")?` 但文件 `test.txt` 并不存在, 你会得到这样的输出:

```log
Error: Os { code: 2, kind: NotFound, message: "No such file or directory" }
```

在你的代码没有 literally 包含文件名的情况下, 将很难分辨哪个文件是 `NotFound`. 有多种方法来处理这个问题.

例如, 我们可以创建我们自己的错误类型, 然后用它来建立一个自定义的错误信息:

```rust
#[derive(Debug)]
struct CustomError(String);

fn main() -> Result<(), CustomError> {
    let path = "test.txt";
    let content = std::fs::read_to_string(path)
        .map_err(|err| CustomError(format!("Error reading `{}`: {}", path, err)))?;
    println!("file content: {}", content);
    Ok(())
}
```

现在, 运行这个我们会得到我们的自定义错误信息.

```log
Error: CustomError("Error reading `test.txt`: No such file or directory (os error 2)")
```

不是很好看, 但我们可以在以后很容易地调整调试输出, 以适应我们的类型.

这种模式实际上是很常见的. 不过, 它有一个问题. 我们不存储原始错误, 只存储其字符串表示.
经常使用的 [anyhow库][] 对此有一个很好的解决方案.
类似于我们的 `CustomError` 类型, 它的 `Context` trait 可以用来添加一个描述.
此外, 它还保留了原始错误, 所以我们得到了一个错误信息的 "链"(chain), 指出错误的根本原因.

让我们首先通过在 `Cargo.toml` 文件的[dependencies]部分添加 `anyhow = "1.0"` 来导入 `anyhow` crate:

然后, 完整的例子将是这样的.

```rust
use anyhow::{Context, Result};

fn main() -> Result<()> { // 这里是 anyhow 定义的 Result
    let path = "test.txt";
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("could not read file `{}`", path))?;
    println!("file content: {}", content);
    Ok(())
}
```

这将打印一个错误.

```log
Error: could not read file `test.txt`

Caused by:
    No such file or directory (os error 2)
```

[anyhow库]: https://docs.rs/anyhow/1.0.48/anyhow/
