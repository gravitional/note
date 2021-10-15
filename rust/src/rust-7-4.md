# 使用 use 关键字将名称引入作用域

到目前为止, 似乎我们编写的, 用于调用函数的`路径`都很冗长且重复, 并不方便.
例如, 示例 7-7 中, 无论我们选择 `add_to_waitlist` 函数的绝对路径还是相对路径,
每次我们想要调用 `add_to_waitlist` 时, 都必须指定 `front_of_house` 和 `hosting`.
幸运的是, 有一种方法可以简化这个过程. 我们可以使用 `use` 关键字将路径一次性引入作用域,
然后调用该路径中的项, 就如同它们是 `本地项` 一样.

在示例 7-11 中, 我们将 `crate::front_of_house::hosting` 模块引入了 `eat_at_restaurant` 函数的作用域,
而我们只需要指定 `hosting::add_to_waitlist` 即可在 `eat_at_restaurant` 中调用 `add_to_waitlist` 函数.

```rust
mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {}
    }
}

use crate::front_of_house::hosting;

pub fn eat_at_restaurant() {
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
}
```

示例 7-11: 使用 `use` 将模块引入作用域

在某个作用域中添加 `use 路径`, 类似于在文件系统中创建`软连接`(符号连接, symbolic link).
通过在 `crate 根` 增加 `use crate::front_of_house::hosting`, 现在 `hosting` 在作用域中就是有效的名称了, 如同 `hosting` 模块被定义在 `crate 根` 上一样.
通过 `use` 引入作用域的路径也会检查 `私有性`, 同其它路径一样.

你还可以使用 `use` 和 `相对路径` 来将一个项引入作用域. 示例 7-12 展示了如何指定相对路径来取得与示例 7-11 中一样的行为.

```rust
mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {}
    }
}

use front_of_house::hosting;

pub fn eat_at_restaurant() {
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
}
```

示例 7-12: 使用 `use` 和`相对路径`将模块引入作用域

## 创建惯用的 use 路径

在示例 7-11 中, 你可能会比较疑惑, 为什么我们是指定 `use crate::front_of_house::hosting`, 然后在 `eat_at_restaurant` 中调用 `hosting::add_to_waitlist` ,
而不是通过`use` 一直到 `add_to_waitlist` 函数的路径, 来得到相同的结果, 如示例 7-13 所示.

```rust
mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {}
    }
}

use crate::front_of_house::hosting::add_to_waitlist;

pub fn eat_at_restaurant() {
    add_to_waitlist();
    add_to_waitlist();
    add_to_waitlist();
}
```

示例 7-13: 使用 `use` 将 `add_to_waitlist` 函数引入作用域, 这并不符合习惯

虽然示例 7-11 和 7-13 都完成了相同的任务, 但示例 7-11 是使用 `use` 将函数引入作用域的习惯用法(idiomatic).
要想使用 `use` 将函数的父模块引入作用域, 我们必须在调用函数时指定父模块,
这样可以清晰地表明函数不是在本地定义的, 同时使完整路径的重复度最小化.
示例 7-13 中的代码不清楚 `add_to_waitlist` 是在哪里被定义的.

另一方面, 使用 `use` 引入结构体, 枚举和其他项时, 习惯是指定它们的完整路径.
示例 7-14 展示了将 标准库中的 `HashMap` 结构体引入 某`二进制 crate` 作用域的习惯用法.

```rust
use std::collections::HashMap;

fn main() {
    let mut map = HashMap::new();
    map.insert(1, 2);
}
```

示例 7-14: 将 `HashMap` 引入作用域的习惯用法

这种习惯用法背后没有什么硬性要求: 它只是一种惯例, 人们已经习惯了以这种方式阅读和编写 `Rust` 代码.

这个习惯用法有一个例外, 那就是我们想使用 `use` 语句将两个具有相同名称的项带入作用域, 因为 Rust 不允许这样做.
示例 7-15 展示了, 如何将两个具有相同名称但不同父模块的 `Result` 类型引入作用域, 以及如何引用它们.

```rust
use std::fmt;
use std::io;

fn function1() -> fmt::Result {
    // --snip--
}

fn function2() -> io::Result<()> {
    // --snip--
}
```

示例 7-15: 使用父模块将两个具有相同名称的类型引入同一作用域

如你所见, 使用父模块可以区分这两个 `Result` 类型. 如果我们是指定 `use std::fmt::Result` 和 `use std::io::Result`,
我们将在同一作用域拥有了两个 `Result` 类型, 当我们使用 `Result` 时, `Rust` 并不知道我们要用的是哪个.

## 使用 as 关键字提供新的名称

使用 `use` 将两个同名类型引入同一作用域的问题, 还有另一个解决办法:
在`类型`的路径后面, 我们使用 `as` 指定一个新的本地名称或者别名.
示例 7-16 展示了另一个编写示例 7-15 中代码的方法, 通过 `as` 重命名其中一个 `Result` 类型.

```rust
use std::fmt::Result;
use std::io::Result as IoResult;

fn function1() -> Result {
    // --snip--
}

fn function2() -> IoResult<()> {
    // --snip--
}
```

示例 7-16: 使用 `as` 关键字重命名引入作用域的类型

在第二个 `use` 语句中, 我们选择 `IoResult` 作为 `std::io::Result` 的新名称, 它与从 `std::fmt` 引入作用域的 `Result` 并不冲突.
示例 7-15 和示例 7-16 都是惯用的, 如何选择都取决于你!

## 使用 pub use 重导出名称

当使用 `use` 关键字将名称导入作用域时, 在`新作用域`中可用的名称是`私有的`.
为了能够让调用你的代码的第三方, 能够像在它自己的作用域内`引用`这些类型, 可以结合 `pub` 和 `use`.
这个技术被称为 `重导出`(re-exporting), 因为除了将项目引入我们的作用域, 还允许其他人将代码引入他们的作用域.

示例 7-17 展示了, 将示例 7-11 中使用 `use` 的根模块, 变为使用 `pub use`.

```rust
mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {}
    }
}

pub use crate::front_of_house::hosting;

pub fn eat_at_restaurant() {
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
}
```

示例 7-17: 通过 `pub use` 使名称可引入任何代码的作用域中

通过 `pub use`, 现在可以通过新路径 `hosting::add_to_waitlist` 来调用 `add_to_waitlist` 函数.
如果没有指定 `pub use`, `eat_at_restaurant` 函数可以在其作用域中调用 `hosting::add_to_waitlist`, 但外部代码则不允许使用这个新路径.

当你的代码的`内部结构`, 与调用你的代码的程序员的看待方式(domain)不同时, `重导出`会很有用.

例如, 在这个餐馆的比喻中, 经营餐馆的人会想到 `前台` 和 `后台`.
但顾客在光顾一家餐馆时, 可能不会以这些术语来考虑餐馆的各个部分.
使用 `pub use`, 我们可以使用`某种结构`编写代码, 而用 `不同的结构` 暴露代码.
这样做可以使我们的库井井有条, 同时方便开发这个库的程序员, 和调用这个库的程序员.

## 使用外部包

在第二章中我们编写了一个猜猜看游戏.
那个项目使用了一个外部包, `rand`, 来生成随机数. 为了在项目中使用 `rand`, 在 `Cargo.toml` 中加入了如下行:

```rust
[dependencies]
rand = "0.5.5"
```

在 `Cargo.toml` 中加入 `rand` 依赖告诉了 `Cargo` 要从 `crates.io` 下载 `rand` 和其依赖, 并使其可在项目代码中使用.

接着, 为了将 `rand` 定义引入项目包的作用域, 我们加入一行 `use` 起始的包名, 它以 `rand` 包名开头并列出了需要引入作用域的项.
回忆一下第二章的 `生成一个随机数` 部分, 我们曾将 `Rng trait `引入作用域并调用了 `rand::thread_rng` 函数:

```rust
use rand::Rng;

fn main() {
    let secret_number = rand::thread_rng().gen_range(1, 101);
}
```

`crates.io` 上有很多 `Rust` 社区成员发布的包, 将其引入你自己的项目都需要一道相同的步骤:
在 `Cargo.toml` 列出它们, 并通过 `use` 将其中定义的项引入项目包的作用域中.

注意`标准库`(std)对于你的包来说也是外部 `crate`.
因为`标准库`随 Rust 语言一同分发, 无需修改 `Cargo.toml` 来引入 `std`,
不过需要通过 `use` 将`标准库`中定义的`项`, 引入项目包的作用域中从而引用它们, 比如我们使用的 `HashMap`:

```rust
use std::collections::HashMap;
```

这是一个以`std` 开头的`绝对路径`, 它是`标准库 crate` 的名称.

## 嵌套路径来消除大量的 use 行

当需要引入很多定义于相同`包`或相同`模块`的项时, 为每一项单独列出一行会占用源码很大的空间.
例如猜猜看章节示例 2-4 中有两行 `use` 语句都从 `std` 引入项到作用域:

```rust
use std::cmp::Ordering;
use std::io;
// ---snip---
```

相反, 我们可以使用`嵌套路径`, 将相同的项在一行中引入作用域.
我们需要指定路径的`相同部分`, 接着是`两个冒号`, 接着将各自不同的路径部分放入大括号, 如示例 7-18 所示.

```rust
use std::{cmp::Ordering, io};
// ---snip---
```

示例 7-18: 指定嵌套的路径, 在一行中将多个带有相同前缀的项引入作用域

在较大的程序中, 使用`嵌套路径`从相同包或模块中引入很多项, 可以显著减少所需的独立 `use` 语句的数量!

我们可以在路径的任何层级使用嵌套路径, 这在组合具有相同共享子路径的 `use` 语句时非常有用.
例如, 示例 7-19 中展示了两个 `use` 语句: 一个将 `std::io` 引入作用域, 另一个将 `std::io::Write` 引入作用域:

```rust
use std::io;
use std::io::Write;
```

示例 7-19: 通过两行 `use` 语句引入两个路径, 其中一个是另一个的子路径

两个路径的相同部分是 `std::io`, 这正是第一个路径.
为了在一行 `use` 语句中引入这两个路径, 可以在嵌套路径中使用 `self`, 如示例 7-20 所示.

```rust
use std::io::{self, Write};
```

示例 7-20: 将示例 7-19 中部分重复的路径合并为一个 `use` 语句

这一行便将 `std::io` 和 `std::io::Write` 同时引入作用域.

## 通过 glob 运算符将所有的公有定义引入作用域

如果希望将一个路径下 **所有** 公有项引入作用域, 可以指定路径后跟 `*`, 即 `glob` 运算符:

```rust
use std::collections::*;
```

这个 `use` 语句将 `std::collections` 中定义的所有公有项引入当前作用域.
使用 `glob` 运算符时请多加小心! `Glob` 会使得我们难以推导作用域中有什么名称, 以及它们是在何处定义的.

`glob` 运算符经常用于测试模块 `tests` 中, 这时会将所有内容引入作用域;
我们将在第十一章 `如何编写测试` 部分讲解. `glob` 运算符有时也用于 `prelude` 模式;
查看 [标准库中的文档](https://doc.rust-lang.org/std/prelude/index.html#other-preludes) 了解这个模式的更多细节.
