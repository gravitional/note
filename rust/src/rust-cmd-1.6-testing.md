# 测试

在几十年的软件开发过程中, 人们发现了一个真理: 未经测试的软件很少能正常工作.
(许多人甚至会说. "大多数经过测试的软件也不工作". 但我们在这里都是乐观主义者, 对吗? )
因此, 为了确保你的程序能做你所期望的事情, 对它进行测试是明智的.

做到这一点的简单方法是写一个README文件, 描述你的程序应该做什么.
当你觉得准备好发布新的版本时, 就去看看README文件, 确保其行为仍然符合预期.
你可以通过记录你的程序对错误的输入的反应, 使之成为更严格的测试.

有一个更花哨的想法. 在你写代码之前写下 `README`.

>旁白: 如果你没有听说过[测试驱动开发][](TDD), 请看一下它.

[测试驱动开发]: https://en.wikipedia.org/wiki/Test-driven_development

## 自动测试

现在, 这一切都很好, 但手动做这一切? 这可能需要大量的时间.
同时, 许多人已经开始喜欢告诉计算机为他们做事情. 让我们来谈谈如何将这些测试自动化.

`Rust` 有一个内置的测试框架, 所以让我们从编写第一个测试开始.

```rust
#[test]
fn check_answer_validity() {
    assert_eq!(answer(), 42);
}
```

你可以把这段代码放在几乎任何文件中, `cargo test` 会找到并运行它.
这里的关键是 `#[test]` 属性. 它允许构建系统发现这些函数, 并将其作为测试来运行, 以验证它们不会出现`panic`.

>给读者的练习. 使这个测试工作.
>你最终应该得到如下的输出.
>
>```log
>running 1 test
>test check_answer_validity ... ok
>
>test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
>```

现在我们已经看到了**如何编写测试**, 我们仍然需要弄清楚**要测试什么**.
正如你已经看到的, 为函数写断言是相当容易的.
但是一个CLI应用程序往往不止一个函数! 更糟糕的是, 它往往涉及到用户.
更糟糕的是, 它经常处理用户输入, 读取文件, 并写入输出.

## 使你的代码可测试

[std::io::Write]: https://doc.rust-lang.org/1.39.0/std/io/trait.Write.html

有两种互补的方法来测试功能. 测试你建立完整应用程序的小单元, 这些被称为 `单元测试`(unit tests).
还有一种是 `从外部` 测试最终的应用程序, 称为 `黑盒测试` 或 `集成测试`(integration tests).
让我们从第一个开始.

为了弄清楚我们应该测试什么, 让我们看看我们的程序特点是什么.
总的来说, `grrs` 应该打印出符合给定模式的行. 所以, 让我们为这一点写单元测试.
我们要确保逻辑中最重要的部分能够工作, 而且我们不依赖于它周围的任何设置(例如, 处理 CLI 参数):

回到我们对 [grrs的第一个实现](https://rust-cli.github.io/book/tutorial/impl-draft.html),
我们在主函数中加入了这段代码.

```rust
// ...
for line in content.lines() {
    if line.contains(&args.pattern) {
        println!("{}", line);
    }
}
```

遗憾的是, 这不是很容易测试. 首先, 它是在主函数中, 所以我们不能轻易调用它.
把这段代码移到一个函数中, 就可以轻松解决这个问题.

```rust
fn main() {
fn find_matches(content: &str, pattern: &str) {
    for line in content.lines() {
        if line.contains(pattern) {
            println!("{}", line);
        }
    }
}
}
```

现在我们可以在我们的测试中调用这个函数, 看看它的输出是什么.

```rust
#[test]
fn find_a_match() {
    find_matches("lorem ipsum\ndolor sit amet", "lorem");
    assert_eq!( // uhhhh
```

嗯......我们可以调用吗?
现在, `find_matches` 直接打印到 `stdout`, 即`终端`. 我们不能轻易地在测试中捕捉到输出!
这是一个在实现功能后,  编写测试时经常出现的问题.
我们写了一个函数, 它被牢牢地集成到它所使用的环境中.

>注意: 在编写小型 `CLI` 应用程序时, 这完全没有问题. 没有必要让所有的东西都是可测试的!
>然而, 重要的是要考虑, 你想让哪部分代码作单元测试.
>我们会看到,  虽然很容易将这个函数改为可测试的, 但并不总是能做到.

好吧, 我们怎样才能使其成为可测试的呢? 我们需要以某种方式捕获输出.
Rust的标准库有一些处理 `I/O`(输入/输出)的整洁的抽象, 我们将使用一个叫做 [std::io::Write][] 的抽象.
这个 `trait` 抽象了可写入的目标, 其中包括 `strings`, 也包括 `stdout`.

如果这是你第一次在 `Rust` 的背景下听到 `trait`, 那么你将会大饱眼福. `trait` 是 `Rust` 最强大的特性之一.
你可以把它们想象成 `Java` 中的 `interfaces` , 或者 `Haskell` 中的 `type classes`(无论你更熟悉什么).
它们允许你抽象出可以被不同 type 共享的行为(behavior).
使用 `trait` 的代码可以用非常通用和灵活的方式来表达想法. 但这意味着它也可能变得难以阅读.  不要让这一点吓倒(intimidate)你.
即使是使用Rust多年的人也不一定能立即理解一般代码(generic)的作用.
在这种情况下, 想一想具体的用途会有帮助.

例如, 在我们的例子中, 我们抽象出来的行为是 `write to it`.
实现(`impl`)它的 types 的例子包括: 终端的标准输出, 文件, 内存中的 `缓冲区`(buffer), 或 `TCP网络连接`.
(在 [std::io::Write][] 的文档中向下滚动, 可以看到一个 `implementors` 的列表).

有了这些知识, 让我们改变我们的函数以接受第三个参数.
它应该是任何实现了 `Write` 的 type.
这样, 我们就可以在测试中提供一个简单的字符串并对其进行断言.
下面是我们如何编写这个版本的 `find_matches`:

```rust
fn find_matches(content: &str, pattern: &str, mut writer: impl std::io::Write) {
    for line in content.lines() {
        if line.contains(pattern) {
            writeln!(writer, "{}", line);
        }
    }
}
```

新的参数是 `mut writer`, 也就是说, 一个我们称之为 `writer` 的可变事物.
它的类型是 `impl std::io::Write`, 你可以把它理解为 "任何实现 `Write` trait 的 type  的占位符".
还请注意我们是如何用 `writeln!(writer, ...)` 替换我们之前使用的` println!(...)` 的.
`println!` 的作用与 `writeln!` 相同, 但总是使用 `标准输出`.

现在我们可以测试输出了:

```rust
#[test]
fn find_a_match() {
    let mut result = Vec::new();
    find_matches("lorem ipsum\ndolor sit amet", "lorem", &mut result);
    assert_eq!(result, b"lorem ipsum\n");
}
```

现在要在我们的应用程序代码中使用这个,
我们必须改变 `main` 中对 `find_matches` 的调用, 加入 `&mut std::io::stdout()` 作为第三个参数.
下面是 `main` 函数的例子, 它建立在我们在前几章看到的基础上,
并使用了我们提取的 `find_matches` 函数.

```rust
fn main() -> Result<()> {
    let args = Cli::from_args();
    let content = std::fs::read_to_string(&args.path)
        .with_context(|| format!("could not read file `{}`", args.path.display()))?;

    find_matches(&content, &args.pattern, &mut std::io::stdout());

    Ok(())
}
```

>注意: 由于 `stdout` 期望的是`bytes`(而不是`字符串`), 我们使用 `std::io::Write` trait 而不是 `std::fmt::Write`.
>结果, 我们在测试中给了一个空的向量作为 "writer"(其类型将被推断为 `Vec<u8>`),
>在 `assert_eq!` 中我们使用了 `b "字符串"`.
>`b`前缀使其成为`byte string literal`(字节字符串字面), 所以其类型将是 `&[u8]` 而不是 `&str`).
>
>注意: 我们也可以让这个函数返回 `string`, 但这将改变它的行为.
>它不会直接写到终端, 而是将所有内容收集到`字符串`中, 并在最后一次性`转储`(dump)所有结果.
>
>给读者的练习: [writeln!][] 返回 [io::Result][], 因为写入可能会失败, 例如当缓冲区满了, 无法扩展. 给 `find_matches` 添加错误处理.

我们刚刚看到如何使这段代码易于测试. 我们已经

+ 确定了我们应用程序的核心部分之一.
+ 把它放到自己的函数中.
+ 并使其更加灵活.

尽管我们的目标是让它变得可测试, 但我们最终得到的结果实际上是一段非常 idiomatic 和可复用的 Rust 代码. 这真是太棒了!

[writeln!]: https://doc.rust-lang.org/1.39.0/std/macro.writeln.html
[io::Result]: https://doc.rust-lang.org/1.39.0/std/io/type.Result.html

## 将你的代码分割成库和二进制目标

我们可以在这里再做一件事. 到目前为止, 我们已经把我们写的所有东西都放到了 `src/main.rs` 文件中.
这意味着我们当前的项目只产生一个二进制文件.
但我们也可以把我们的代码作为一个`库`(library)来使用, 像这样.

+ 把 `find_matches` 函数放到一个新的 `src/lib.rs` 中.
+ 在 `fn` 前面加一个 `pub`(所以是 `pub fn find_matches`), 使其成为我们库的用户可以访问的东西.
+ 从 `src/main.rs` 中删除 `find_matches`.
+ 在 `fn main` 中, 在对 `find_matches` 的调用前加上 `grrs::`, 所以它现在是 `grrs::find_matches(...)`. 这意味着它使用了我们刚刚编写的库中的函数.

`Rust` 处理项目的方式是相当灵活的, 在早期考虑将那些功能放入 `crate` 的`library` 中是个好主意.
例如, 你可以考虑先为你的特定应用逻辑编写一个`库`, 然后像其他库一样在你的 `CLI` 中使用它.
或者, 如果你的项目有多个二进制文件, 你可以把共同的功能放到 crate 的`库`部分.

注意: 说到把所有东西都放到 `src/main.rs` 中. 如果我们继续这样做, 就会变得难以阅读.
[模块系统][] 可以帮助你结构化和组织你的代码.

[模块系统]: https://doc.rust-lang.org/1.39.0/book/ch07-00-managing-growing-projects-with-packages-crates-and-modules.html

## 通过运行CLI应用程序来测试它们

到目前为止, 我们已经不遗余力地测试了我们应用程序的 `business logic`(业务逻辑), 即 `find_matches` 函数.
这是非常有价值的, 是迈向经过良好测试的 code base 的第一步. (通常情况下, 这类测试被称为 "单元测试").

不过, 有很多代码我们并没有测试. 我们写的所有的东西都是为了处理外部世界的问题!
想象一下, 你写了主函数, 但不小心留下了一个硬编码的字符串, 而不是使用用户提供的路径的参数.
我们也应该为这个写测试! (这个层次的测试通常被称为 "集成测试", 或 "系统测试").

它的核心是, 我们仍然编写函数并用 `#[test]` 来注释它们. 只是我们在这些函数中要做别的事情.
例如, 我们要使用项目的主二进制, 并像普通程序一样运行它.
我们还将把这些测试放到一个新目录下的新文件中: `test/cli.rs`.

除此以外. 按照惯例, `cargo` 会在 `test/` 目录中寻找集成测试.
同样地, 它也会在 `benches/` 目录下寻找 `基准`(benchmarks), 在 `examples/` 目录下寻找例子.
这些约定也延伸到了你的主源代码: 库有一个 `src/lib.rs` 文件, 主二进制文件是 `src/main.rs`,
或者如果有多个二进制文件, `cargo` 希望它们在 `src/bin/<name>.rs` 中.
遵循这些约定, 将使你的代码库更容易被 Rust 的熟练者发现.

回顾一下, `grrs` 是在文件中搜索字符串的小工具. 我们之前已经测试过, 可以找到匹配.
让我们想一想, 我们还可以测试哪些功能.

下面是我想出的办法.

+ 当文件不存在时会发生什么?
+ 当没有匹配时, 输出是什么?
+ 当我们忘记一个(或两个)参数时, 我们的程序是否会以错误退出?

这些都是有效的测试案例.
此外, 我们还应该包括一个 "happy path" 的测试案例, 也就是说, 我们至少找到了一个匹配项, 并打印出来.

为了使这类测试更容易, 我们将使用 [assert_cmd][] 工具箱.
它有一堆整洁的助手, 使我们能够运行我们的主二进制文件并查看它的表现.
此外, 我们还将添加 [predicates][] crate, 它可以帮助我们编写 [assert_cmd] 可以测试的断言(并且有很好的错误信息).
我们不把这些依赖添加到主列表中, 而是添加到 `Cargo.toml` 的 `dev dependencies` 部分.
它们只在开发 crate 时需要, 在使用时不需要.

```toml
[dependencies]
structopt = "0.3.25"
```

这听起来像是很多的设置. 尽管如此--让我们直接进入并创建我们的 `test/cli.rs` 文件.

```rust
use assert_cmd::prelude::*; // 在命令上添加方法
use predicates::prelude::*; // 用来编写断言.
use std::process::Command; // 运行程序

#[test]
fn file_doesnt_exist() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("grrs")?;

    cmd.arg("foobar").arg("test/file/doesnt/exist");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("could not read file"));

    Ok(()) // 成功时返回
}
```

你可以用 `cargo test` 来运行这个测试, 即上面写的测试.
第一次可能要花点时间, 因为 `Command::cargo_bin("grrs")` 需要编译你的主二进制文件.

[assert_cmd]: https://docs.rs/assert_cmd/latest/assert_cmd/
[predicates]: https://docs.rs/predicates/latest/predicates/

## 生成测试文件

我们刚才看到的测试, 只是检查程序在输入文件不存在的情况下是否输出错误信息. 这是重要的测试, 但也许不是最重要的:
现在让我们来测试一下, 是否真的可以在文件中打印出找到的匹配信息! 我们需要一个文件.

我们需要有一个已知内容的文件, 这样就可以知道程序应该返回什么, 并在我们的代码中检查这个期望.
一个想法是在项目中添加带有自定义内容的文件, 并在我们的测试中使用它.
另一个是在我们的测试中创建临时文件. 在本教程中, 我们将使用后一种方法.
主要是因为它更灵活, 而且在其他情况下也能工作; 例如, 当你测试改变文件的程序时.

为了创建临时文件, 我们将使用 [assert_fs][] crate.
让我们把它添加到 `Cargo.toml` 的 `dev-dependencies` 中:

```toml
anyhow = "1.0"
```

这里有一个新的测试案例(你可以写在另一个案例下面),
首先创建临时文件( "命名 "的文件, 这样我们就可以得到它的路径), 用一些文本填充它,
然后运行我们的程序, 看看我们是否得到正确的输出. 当 `file` 超出作用域时(在函数的末尾), 实际的临时文件将被自动删除.

```rust
use assert_fs::prelude::*;

#[test]
fn find_content_in_file() -> Result<(), Box<dyn std::error::Error>> {
    let file = assert_fs::NamedTempFile::new("sample.txt")?;
    file.write_str("A test\nActual content\nMore content\nAnother test")?;

    let mut cmd = Command::cargo_bin("grrs")?;
    cmd.arg("test").arg(file.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("test\nAnother test"));

    Ok(())
}
```

给读者的练习: 增加集成测试, 对于传递 `空字符串` 作为 `pattern`. 根据需要调整程序.

[assert_fs]: https://docs.rs/assert_fs/latest/assert_fs/

## 要测试什么?

虽然写 `集成测试` 肯定会很有趣, 但也要花一些时间来写, 以及在你的程序行为发生变化时更新它们.
为了确保明智地使用时间, 你应该问自己 `测试哪些内容`.

一般来说, 为用户可以观察到的所有类型的行为编写集成测试是个好主意.
这意味着你不需要覆盖所有的边缘情况. 通常有不同类型的例子就足够了, 并依靠单元测试来覆盖边缘情况.

不要把你的测试集中在你不能主动控制的事情上也是明智的.
测试 `--help` 的确切布局是个坏主意, 因为它是程序为你生成的.
相反, 你可能只想检查某些元素是否存在.

根据你的程序的性质, 你也可以尝试添加更多的测试技术.
例如你拎出程序的一部分, 发现自己写了很多 `example casee` 作为单元测试, 同时试图想出所有的边缘案例, 你应该研究一下 [proptest][].
如果你有一个程序, 消耗任意文件并解析它们, 试着写一个 [fuzzer][] 来发现边缘案例的bug.

顺便说一下. 你可以在 [本书的资源库][] 中找到本章中使用的完整的, 可运行的源代码.

[proptest]: https://docs.rs/proptest/latest/proptest/
[fuzzer]: https://rust-fuzz.github.io/book/introduction.html
[本书的资源库]: https://github.com/rust-cli/book/tree/master/src/tutorial/testing
