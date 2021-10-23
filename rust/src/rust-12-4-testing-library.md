# 采用测试驱动开发完善库的功能

现在我们将逻辑提取到了 src/lib.rs, 并将所有的参数解析和错误处理留在了 src/main.rs 中, 为代码的核心功能编写测试将更加容易.
我们可以直接使用多种参数调用函数, 并检查返回值而无需从命令行运行二进制文件了.
如果你愿意的话, 请自行为 Config::new 和 run 函数的功能编写一些测试.

在这一部分, 我们将遵循 `测试驱动开发`(Test Driven Development, TDD)的模式来逐步增加 minigrep 的搜索逻辑.
这是一个软件开发技术, 它遵循如下步骤:

+ 编写一个将会失败的测试, 并运行它, 以确保它失败的原因是你所期望的.
+ 编写或修改足够的代码来使新的测试通过.
+ 重构刚刚增加或修改的代码, 并确保测试仍然能通过.
+ 从步骤 1 开始重复!

这只是众多编写软件的方法之一, 不过 TDD 有助于驱动代码的设计.
在编写 `能通过测试的代码`之前编写`测试`, 有助于在开发过程中保持高测试覆盖率.

我们将通过`测试驱动`实现如下功能: 在文件内容中搜索, 用户查询的字符串, 并返回匹配的行.
我们将在一个叫做 `search` 的函数中增加这些功能.

## 编写失败测试

去掉 src/lib.rs 和 src/main.rs 中用于检查程序行为的 println! 语句, 因为不再真正需要他们了.
接着我们会像 第十一章 那样增加一个 test 模块和一个测试函数.
测试函数指定了 search 函数期望拥有的行为:
它会获取一个需要查询的字符串和用来查询的文本, 并只会返回包含请求的文本行.
示例 12-15 展示了这个测试, 它还不能编译:

文件名: src/lib.rs

```rust
#[cfg(test)] // cfg 属性代表 configuration, 只包含在 test 配置中
mod tests {
    use super::*;

    #[test]
    fn one_result() {
        let query = "duct";
        let contents = "\
Rust:
safe, fast, productive.
Pick three.";

        assert_eq!(
            vec!["safe, fast, productive."],
            search(query, contents)
        );
    }
}
```

示例 12-15: 创建一个我们期望的 search 函数的失败测试

这里选择使用 "duct" 作为这个测试中需要搜索的字符串. 用来搜索的文本有三行, 其中只有一行包含 "duct".
我们断言 search 函数的返回值只包含期望的那一行.

我们还不能运行这个测试并看到它失败, 因为它甚至都还不能编译: search 函数还不存在呢!
我们将增加足够的代码来使其能够编译: 一个总是会返回空 vector 的 search 函数定义, 如示例 12-16 所示.
这个测试应该能够编译, 但空 vector 并不匹配, 包含一行 `"safe, fast, productive."` 的 `vector`, 测试会失败.

文件名: src/lib.rs

```rust
pub fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    vec![]
}
```

示例 12-16: 刚好足够使测试通过编译的 search 函数定义

注意需要在 search 的签名中定义一个显式生命周期 `'a` 并用于 contents 参数和返回值.
回忆一下 第十章 中讲到, 生命周期参数指定哪个参数的生命周期, 与返回值的生命周期相关联.
在这个例子中, 我们表明返回的 `vector` 中, 应该包含`引用` `contents`参数(而不是 `query`参数) 的 `字符串 slice`.

换句话说, 我们告诉 Rust 函数 search 返回的数据, 将与 search 函数中的 `contents 参数的数据`, 存活地一样久. 这是非常重要的!
为了使这个引用有效, 那么 `被 slice 引用` 的数据也需要保持有效;
如果编译器认为我们是在创建 `query`的, 而不是 contents 的字符串 slice, 那么安全检查将是不正确的.

尝试不用生命周期编译, 我们将得到如下错误:

```powershell
error[E0106]: missing lifetime specifier
 --> src/lib.rs:5:51
  |
5 | pub fn search(query: &str, contents: &str) -> Vec<&str> {
  |                                                   ^ expected lifetime
parameter
  |
  = help: this function's return type contains a borrowed value, but the
  signature does not say whether it is borrowed from `query` or `contents`
```

Rust 不可能知道我们需要的是哪一个参数, 所以需要告诉它.
因为参数 contents 包含了所有的文本而且我们希望返回匹配的那部分文本,
所以我们知道, 应该使用生命周期语法, 将 `contents` 参数与返回值相关联.

其他语言中并不需要你在函数签名中将参数与返回值相关联.
所以这么做可能让你感觉有些陌生, 随着时间的推移这将会变得越来越容易.
你可能想要将这个例子, 与第十章中 "生命周期与引用有效性" 部分做对比.

现在运行测试:

```bash
$ cargo test
   Compiling minigrep v0.1.0 (file:///projects/minigrep)
--warnings--
    Finished dev [unoptimized + debuginfo] target(s) in 0.43 secs
     Running target/debug/deps/minigrep-abcabcabc

running 1 test
test tests::one_result ... FAILED
...
error: test failed, to rerun pass '--lib'
```

好的, 测试失败了, 这正是我们所期望的. 修改代码来让测试通过吧!

## 编写使测试通过的代码

目前测试之所以会失败是因为, 我们总是返回一个空的 vector.
为了修复并实现 search, 我们的程序需要遵循如下步骤:

+ 遍历内容的每一行文本.
+ 查看这一行是否包含要搜索的字符串.
+ 如果有, 将这一行加入列表返回值中.
+ 如果没有, 什么也不做.
+ 返回匹配到的结果列表

让我们一步一步的来, 从遍历每行开始.

## 使用 lines 方法遍历每一行

Rust 有一个有助于一行一行遍历字符串的方法, 出于方便它被命名为 `lines`, 它如示例 12-17 这样工作.
注意这还不能编译:

文件名: src/lib.rs

```rust
pub fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    for line in contents.lines() {
        // do something with line
    }
}
```

示例 12-17: 遍历 contents 的每一行

lines 方法返回一个迭代器.
第十三章 会深入了解迭代器, 不过我们已经在 示例 3-5 中见过使用迭代器的方法了,
在那里使用了一个 for 循环和迭代器, 在集合的每一项上运行了一些代码.

## 用查询字符串搜索每一行

接下来将会增加另一个功能, 检查当前行是否包含要查询的字符串.
幸运的是, 为此`字符串`类型也有一个叫做 contains 的实用方法!
如示例 12-18 所示, 在 search 函数中加入 contains 方法调用. 注意这仍然不能编译:

文件名: src/lib.rs

```rust
pub fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    for line in contents.lines() {
        if line.contains(query) {
            // do something with line
        }
    }
}
```

示例 12-18: 增加检查文本行是否包含 query 中字符串的功能

## 存储匹配的行

我们还需要一个方法, 来存储包含被查询字符串的行.
为此可以在 for 循环之前创建一个可变的 vector, 并调用 push 方法在 vector 中存放一个 line.
在 for 循环之后, 返回这个 vector, 如示例 12-19 所示:

文件名: src/lib.rs

```rust
pub fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let mut results = Vec::new();

    for line in contents.lines() {
        if line.contains(query) {
            results.push(line);
        }
    }

    results
}
```

示例 12-19: 储存匹配的行以便可以返回他们

现在 search 函数应该返回只包含 query 的那些行, 而测试应该会通过. 让我们运行测试:

```rust
$ cargo test
--snip--
running 1 test
test tests::one_result ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

测试通过了, 它可以工作了!

现在正是可以考虑重构的时机, 在保证测试通过, 保持功能不变的前提下重构 search 函数.
search 函数中的代码并不坏, 不过并没有利用迭代器的一些实用功能.
第十三章将回到这个例子, 并深入探索迭代器, 看看如何改进代码.

## 在 run 函数中使用 search 函数

现在 search 函数可以工作, 并能通过测试, 我们需要在 run 函数中实际调用 search.
我们需要把 config.query, 以及 run 从文件中读取到的 `contents`, 传递给 search 函数.
接着 run 会打印出 search 返回的每一行:

文件名: src/lib.rs

```rust
pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(config.filename)?;

    for line in search(&config.query, &contents) {
        println!("{}", line);
    }

    Ok(())
}
```

这里仍然使用 for 循环获取 search 返回的每一行, 并打印出来.

现在整个程序应该可以工作了!
让我们试一试, 首先使用只会在艾米莉·狄金森的诗中返回一行的单词, "frog":

```log
$ cargo run frog poem.txt
   Compiling minigrep v0.1.0 (file:///projects/minigrep)
    Finished dev [unoptimized + debuginfo] target(s) in 0.38 secs
     Running `target/debug/minigrep frog poem.txt`
How public, like a frog
```

好的! 现在试试一个会匹配多行的单词, 比如 "body":

```log
$ cargo run body poem.txt
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/minigrep body poem.txt`
I'm nobody! Who are you?
Are you nobody, too?
How dreary to be somebody!
```

最后让我们确保, 搜索一个在诗中不存在的单词时, 不会得到任一行, 比如 "monomorphization":

```log
$ cargo run monomorphization poem.txt
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/minigrep monomorphization poem.txt`
```

非常好! 我们创建了一个属于自己的迷你版经典工具, 并学习了很多如何组织程序的知识.
我们还学习了一些文件输入输出, 生命周期, 测试和命令行解析的内容.

为了使这个项目更丰满, 我们将简要的展示, 如何处理环境变量和打印到标准错误, 这两者在编写命令行程序时都很有用.
