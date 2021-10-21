# 包和crate

在模块系统的第一部分, 我们将介绍`包`和 `crate`. `crate` 是一个`二进制文件`或者`库`(library).
crate: 板条箱.

`crate root` 是一个源文件(source file), Rust 编译器以它为起点, 建立起你的 `crate` 的`根模块`
(我们将在 `定义模块来控制作用域与私有性` 一节深入解读).

`包`(package) 一个或者多个 `crate` 组成, 提供一系列功能.
`包`中会包含一个 `Cargo.toml` 文件, 来阐述如何构建这些 `crate`.

`包`中可以含有的内容由几条规则来确立.
`包`中**最多**包含一个`库 crate`(library crate);
`包`中可以包含任意多个`二进制 crate`(binary crate);
包中至少包含一个 `crate`, 无论是`library`的还是`二进制`的.

让我们来看看, 创建包的时候会发生什么. 首先, 我们输入命令 `cargo new`:

```bash
$ cargo new my-project
     Created binary (application) `my-project` package
$ ls my-project
Cargo.toml
src
$ ls my-project/src
main.rs
```

当我们输入了这条命令, `Cargo` 会给我们的`包`创建一个 `Cargo.toml` 文件. 查看 `Cargo.toml` 的内容, 会发现其中没有提到 `src/main.rs`,
因为 `Cargo` 遵循一个约定: `src/main.rs` 就是`二进制 crate` 的 `root`, `root` 的名字与包相同.
同理, 如果包目录中包含 `src/lib.rs`, `Cargo` 就知道`包`中具有与其同名的`库 crate`, 且 `src/lib.rs` 是 `crate root`.

`Cargo` 将把 `crate root 文件` 传递给 `rustc`, 来实际构建`库`或者`二进制文件`.

在此, 我们有一个只包含 `src/main.rs` 的包, 意味着它只含有一个名为 `my-project` 的 `二进制 crate`.
如果一个包同时含有 `src/main.rs` 和 `src/lib.rs`, 则它有两个 `crate`: 一个`library`和一个`binary`, 且名字都与`包`相同.
一个`包`可以拥有多个`二进制 crate`, 只需通过把文件放在 `src/bin` 目录下:
每个 `src/bin` 下的文件都会被编译成单独的 `二进制 crate`.

一个 `crate` 会将相关功能分组到同一个作用域中, 使得该功能可以方便地在多个项目之间共享.
例如, 我们在 第二章 使用的 `rand` crate 提供了生成随机数的功能.
通过将 `rand` crate 加入到我们项目的`作用域`中, 我们就可以在自己的项目中使用该功能.
`rand` crate 提供的所有功能都可以通过该 `crate` 的名字: `rand` 进行访问.

将一个 `crate` 的`功能`保持在其自身的作用域中, 可以区分 `特定的功能`, 到底是定义在我们的 `crate` 中, 还是定义在 `rand` crate 中的, 这可以防止潜在的冲突.
例如, `rand` crate 提供了一个名为 `Rng` 的特性(trait). 我们也可以在自己的 `crate` 中定义一个名为 `Rng` 的 `struct`.
因为一个 `crate` 的功能, 命名空间限制在其自身的`作用域`, 当我们将 `rand` 添加为依赖时, 编译器不会混淆 `Rng` 这个名字的指向.
在我们的 `crate` 中, 它指向的是我们自己定义的 `struct Rng`. 我们可以通过 `rand::Rng` 这一方式来访问 `rand` crate 中的 `Rng` 特性(trait).

接下来让我们来说一说`模块`系统!
