# 宏

    ch19-06-macros.md
    commit 7ddc46460f09a5cd9bd2a620565bdc20b3315ea9

我们已经在本书中使用过像 `println!` 这样的宏了, 不过还没完全探索什么是`宏`以及它是如何工作的.
`宏`(`Macro`)指的是 Rust 中一系列的功能:
使用 `macro_rules!` 的 `声明`(Declarative)宏, 和三种 `过程`(Procedural)宏:

+ 自定义 `#[derive]` 宏; 当在结构体和枚举上指定 `derive` 属性时, 指定需要添加的代码.
+ `类属性宏(Attribute-like)`; 定义可用于任意项的`自定义属性`
+ `类函数宏`; 看起来像`函数`, 不过作用于传递给它的  `token` 参数.

我们会依次讨论每种宏. 不过首要的问题是, 为什么有了`函数`还需要`宏`?

## 宏和函数的区别

从根本上来说, 宏是编写一种特殊代码的方式, 它们会帮你编写更多其他代码, 即所谓的 `元编程`(metaprogramming).
在附录 C 中会探讨 `derive` 属性(attribute), 它会帮你生成各种 trait 的`实现`(implementation).
我们也在本书中使用过 `println!` 宏和 `vec!` 宏.

所有的这些宏以 `展开`(expand) 的方式, 来生成更多的代码,  相比你手工所能编写的.

`元编程`可以帮你减少需要编写和维护的代码的数量, 这恰好也是函数扮演的角色之一.
但宏有一些函数所没有的附加能力.

函数签名(signature)必须声明函数`参数`个数和类型.
相比之下, 宏能够接受不同数量的参数:
用一个参数调用 `println!("hello")`, 或用两个参数调用 `println!("hello {}", name)` .

并且, 宏可以在编译器解释代码之前先`展开`, 所以宏可以, 例如在给定`type`上实现某个 `trait`.
而`函数`则不行, 因为函数在运行时才被调用, 而 trait 需要在编译时实现.

相比于`函数`, 编写 `宏`  的缺点是: 宏的定义要比函数更复杂, 因为你正在编写生成 `Rust` 代码的 `Rust` 代码.
由于这样的间接性, 宏定义通常要比函数定义更难阅读, 理解以及维护.

宏和函数的另一个重要的区别是:
`宏`必须先声明(定义), 或者先引入作用域, 然后才能在文件中调用它.
而函数可以在任何地方定义和调用.

## 使用 macro_rules! 的声明宏用于通用元编程

Rust 中最常用的宏形式是 `声明宏`(declarative macros).
它们有时也被称为 "macros by example", "`macro_rules!` 宏" 或者就是 "macros".

`声明宏`的核心概念是, 允许我们编写一些类似 Rust `match` 表达式的代码.
正如在第六章讨论的那样, match 表达式是控制结构,
它接收一个表达式, 将表达式的`结果` 和模式进行匹配, 然后根据匹配到的模式执行相关代码.

宏也将`值`和关联某些代码的`模式`进行比较;
此时, `值`是传递给宏的 `Rust 源代码`(字面值), `模式` 将与`源代码`的结构进行比较,
当匹配时, 将使用与模式关联的代码, 替换接收到的`源代码`.
这一切都发生于编译时.

使用 `macro_rules!` 来定义宏. 让我们观察 `vec!` 宏的定义, 来探索如何使用 `macro_rules!` 结构.

第八章讲述了如何使用 `vec!` 宏来生成给定值的 `vector`.
例如, 下面的宏用三个整数创建 `vector`:

```rust
let v: Vec<u32> = vec![1, 2, 3];
```

也可以使用 `vec!` 宏来构造两个整数的 `vector` 或五个字符串 `slice` 的 `vector` .
但却无法使用函数实现相同的功能, 因为我们无法预知参数值的`数量`和`类型`.

在示例 19-28 中展示了 `vec!` 稍微简化的定义.

文件名: src/lib.rs

```rust
#[macro_export]
macro_rules! vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}
```

示例 19-28:  `vec!` 宏定义的简化版本

>注意: 标准库中实际定义的 `vec!`, 包括了预分配适量内存的代码.
此处为了让简化示例, 并没有包含这部分代码优化.

`#[macro_export]` 注解表明: 只要将定义该`宏`的`crate`导入作用域, 该`宏` 就是可用的.
如果没有这个`注解`, 此`宏`不能被引入作用域.

接着我们使用 `macro_rules!` 和`宏名称`开始`宏定义`, 我们所定义的宏  `不带`  感叹号.
宏的名称后跟着大括号, 表示宏的定义体, 在该例中宏名称是 `vec` .

`vec!` body中的结构和 `match` 表达式的结构类似.
此处有个带有模式 `( $( $x:expr ),* )` 的分支(arm), 后面跟着 `=>`, 以及和模式关联的`代码块`.
如果模式匹配成功, 则执行关联的代码块.
假设这是此宏中仅有的模式, 则只有这一种有效匹配, 其他任何模式都会引起错误.
更复杂的`宏`会有多个分支(arm).

`宏定义`中`有效模式语法`和在第十八章提及的模式语法是不同的, 因为宏模式所匹配的是 `Rust 代码结构`而不是`值`.
让我们回过头来检查下, 示例 19-28 中模式片段的含义. 对于全部的宏模式语法, 请查阅[The Rust Reference](https://doc.rust-lang.org/reference/macros.html).

首先, 一对`括号 ()`包裹(encompass)了整个模式.
接下来是美元符号(`$`), 后跟一对`括号`, 捕获了匹配括号内模式的`值`, 以用于替换后的代码(后向引用).
`$()` 内则是 `$x:expr` , 其匹配 Rust 的任意表达式, 并将该表达式记作 `$x`.

`$()` 之后的逗号说明, 一个可选的`逗号分隔符`,  可以出现在 `$()` 所匹配的代码之后.
紧随逗号之后的 `*` 说明, `*`号之前的模式可以被匹配零次或者多次.

当以 `vec![1, 2, 3];` 调用该宏时,模式 `$x` 会与表达式 `1`, `2` 和 `3` 进行三次匹配.

现在让我们来查看与此`arm` 关联的代码块中的`模式`:
在这里, `temp_vec.push()`位于 `$()*` 内,
对于每个能匹配到模式中 `$()` 的部分(`=>` 前面的模式), 将这段代码生成零次或更多次(在 `=>` 后面),
生成的具体次数取决于该模式被匹配的次数. `$x` 被替换成每次匹配到的表达式.

当以 `vec![1, 2, 3];` 调用该宏时, 通过替换所生成的代码如下:

```rust
let mut temp_vec = Vec::new();
temp_vec.push(1);
temp_vec.push(2);
temp_vec.push(3);
temp_vec
```

我们已经定义了一个宏, 它可以接收任意数量和类型的参数,
它生成的代码, 可以创建包含指定元素的 `vector`.

`macro_rules!` 中有一些奇怪的地方(strange edge cases).
在将来, 会有第二种采用 `macro` 关键字的`声明宏`, 其工作方式类似, 但修复了这些极端情况.
经过此更新, `macro_rules!` 实际上就过时(deprecated)了.
在此基础之上, 同时鉴于大多数 Rust 程序员 **使用** 宏而非 **编写** 宏的事实, 此处不再深入探讨 `macro_rules!`.
请查阅在线文档或其他资源, 如 ["The Little Book of Rust Macros"](https://veykril.github.io/tlborm/), 来深入了解如何写宏.

## 用于从属性生成代码的过程宏

第二种形式的宏被称为 `过程宏`(procedural macros), 因为它们更像函数(过程的一种type).
过程宏接收 Rust 代码作为输入, 在这些代码上进行操作, 然后产生另一些代码作为输出,
而非像声明式宏那样匹配对应模式, 然后以另一部分代码替换当前代码.

有三种类型的过程宏: `custom derive`, `attribute-like`, 和 `function-like`, 不过它们的工作方式都类似.

创建`过程宏`时, 其定义必须驻留在它们自己的crate 中, 并使用特殊的 crate type.
这么做出于复杂的技术原因, 将来我们希望能够消除这些限制.
使用`过程宏` 的方式 类似于示例 19-29 的代码所示,
其中 `some_attribute` 是个占位符, 表示使用了某个特定宏.(也就是可以在定义宏的时候使用别的宏, 套娃)

文件名: src/lib.rs

```rust
use proc_macro;

#[some_attribute]
pub fn some_name(input: TokenStream) -> TokenStream {
}
```

示例 19-29: 使用过程宏的例子

定义`过程宏`的函数, 以 `TokenStream` 作为输入并生成 `TokenStream` 作为输出.
`TokenStream` type 定义在 `proc_macro` crate 中, 后者包含在 `Rust` 中, `TokenStream` 表示令牌序列.
这是宏的核心: 宏所操作的`源代码`构成了输入的 `TokenStream`, 宏产生的代码是 输出的`TokenStream`.

这个定义宏的函数, 还附加了一个 attribute, 指出了它创建的`过程宏`的种类(kinds).
同一个 crate 中, 我们可以在拥有多种(kinds)的 `过程宏`.

让我们看看不同种类的`过程宏`.
我们将从`自定义的派生宏`(custom derive)开始, 然后解释其他形式的微小区别.

## custom derive macro

让我们创建名为 `hello_macro` 的 crate, 其包含名为 `HelloMacro` 的 `trait`, 和关联函数 `hello_macro`.
不同于让 `crate` 的用户为每个类型实现 `HelloMacro` trait,
我们将会提供一个`过程宏`,
以便用户可以使用 `#[derive(HelloMacro)]` 注解他们的 type, 来得到 `hello_macro` 函数的默认实现.
该默认实现会打印 `Hello, Macro! My name is TypeName!`, 其中 `TypeName` 是实现此 `trait` 的具体 type 名.
换言之, 我们将创建一个 `crate`, 使程序员能够写类似示例 19-30 中的代码.

文件名: src/main.rs

```rust
use hello_macro::HelloMacro;
use hello_macro_derive::HelloMacro;

#[derive(HelloMacro)]
struct Pancakes;

fn main() {
    Pancakes::hello_macro();
}
```

示例 19-30: `crate` 用户所写的能够使用过程式宏的代码

运行该代码将会打印 `Hello, Macro! My name is Pancakes!`
第一步是像下面这样新建库 `crate`:

```bash
$ cargo new hello_macro --lib
```

接下来, 我们定义 `HelloMacro` trait 以及它的关联函数:

文件名: src/lib.rs

```rust
pub trait HelloMacro {
    fn hello_macro();
}
```

现在建立了包含函数的 `trait`, `crate` 用户可以实现该 `trait` 以达到其期望的功能, 如下:

```rust
use hello_macro::HelloMacro;

struct Pancakes;

impl HelloMacro for Pancakes {
    fn hello_macro() {
        println!("Hello, Macro! My name is Pancakes!");
    }
}

fn main() {
    Pancakes::hello_macro();
}
```

然而, 他们需要为每个使用 `hello_macro` 的 type 实现`HelloMacro`.
我们希望能节约这些工作.

另外, 我们也无法为 `hello_macro` 函数提供一个默认实现, 能够给每个实现了该 trait 的 type 类型打印名称:
Rust 没有`反射`的能力, 所以它在运行时无法获取 type 名称.
我们需要一个在编译时生成代码的宏.

下一步是定义`过程宏`.
在编写本书时, `过程宏` 必须呆在它自己专有的 `crate` 内. 该限制最终可能被取消.
构造 `crate` 和 `macro crates`的惯例如下:
对于 `foo crate` 来说, `自定义的派生过程宏`的 crate 应该叫做 `foo_derive`.
我们在 `hello_macro` 项目内部, 新建名为 `hello_macro_derive` 的 `crate`:

```bash
$ cargo new hello_macro_derive --lib
```

由于两个 `crate` 紧密相关, 因此我们在 `hello_macro` crate 的目录下, 创建`过程式宏`的 crate.
如果我们改变在 `hello_macro` 中的 `trait` 定义  , 也必须同时改变在 `hello_macro_derive` 中实现的`过程式宏`.
这两个包需要分别发布, 编程人员如果使用这些包, 则需同时添加两个`依赖`, 并将其引入作用域.

我们也可以只引用 `hello_macro` 包, 而将 `hello_macro_derive` 作为一个依赖, 并重导出(re-export)`过程式宏`的代码.
但此处我们组织项目的方式, 使编程人员在无需 `derive` 功能时, 能够单独使用 `hello_macro`.

我们需要声明 `hello_macro_derive` crate 是`过程宏`(proc-macro)crate.
正如稍后将看到的那样, 我们还需要 `syn` 和 `quote` crate 中的功能, 所以需要将其添加到依赖中.
将下面的代码添加到 `hello_macro_derive` 的 `Cargo.toml` 文件中.

文件名: hello_macro_derive/Cargo.toml

```conf
[lib]
proc-macro = true

[dependencies]
syn = "1.0"
quote = "1.0"
```

为定义`过程宏`, 请将示例 19-31 中的代码放在 `hello_macro_derive` crate 的 `src/lib.rs` 文件里面.
注意这段代码在我们添加 `impl_hello_macro` 函数的定义之前是无法编译的.

文件名: hello_macro_derive/src/lib.rs

>在 Rust 1.31.0 时, `extern crate` 仍是必须的, 请查看
>https://github.com/rust-lang/rust/issues/54418
>https://github.com/rust-lang/rust/pull/54658
>https://github.com/rust-lang/rust/issues/55599

```rust
extern crate proc_macro;

use crate::proc_macro::TokenStream;
use quote::quote;
use syn;

#[proc_macro_derive(HelloMacro)] // 过程宏 API
pub fn hello_macro_derive(input: TokenStream) -> TokenStream {
    // 将 Rust 代码解析为语法树以便进行操作
    let ast = syn::parse(input).unwrap();

    // 构建 trait 实现
    impl_hello_macro(&ast)
}
```

示例 19-31: 大多数过程式宏处理 `Rust` 代码时所需的代码

注意, 我们把代码分成了 `hello_macro_derive` 函数和 `impl_hello_macro` 函数,
前者负责解析 `TokenStream`, 后者负责转换`语法树`(abstract syntax tree): 这使得编写`过程宏`更加方便.
几乎你能看到的或创建的每个`过程宏`, 外层函数(本例中为 `hello_macro_derive`)中的代码都是一样的.
在`内部函数`(本例中为 `impl_hello_macro`) body 中指定的代码, 将根据你的`过程宏`的目的而有所不同.

我们已经引入了三个新的 crate: `proc_macro`, `syn` 和 `quote`.
`proc_macro` crate是 `Rust` 自带的, 所以我们不需要在 `Cargo.toml` 的依赖项中添加它.
`proc_macro` crate 是编译器的API, 它允许我们读取和操作 Rust 源代码.

`syn` crate 将 Rust 代码从`字符串`解析成我们可以执行操作的`数据结构`.
`quote` crate 将 `syn` 数据结构转换回 `Rust` 代码.
这些 crates, 使得解析任何目标 Rust 源码变得更简单: 为Rust代码编写完整的解析器并不是一件简单的工作.

当用户在某个 type 上指定 `#[derive(HelloMacro)]` 时, `hello_macro_derive` 函数将会被调用.
因为在 `hello_macro_derive` 函数那里, 我们已经使用 `proc_macro_derive` 和特定名称 `HelloMacro` 进行了注解:
它和我们的 trait 名称匹配, 这是大多数`过程宏`遵循的习惯.

该函数首先将来自 `TokenStream` 的 `input` 转换为一个数据结构, 我们可以在解释和操作后者.
这正是 `syn` 的用武之地. `syn` 中的 `parse_derive_input` 函数获取一个 `TokenStream`,
并返回一个 `DeriveInput` 结构体, 表示解析完毕的 `Rust` 代码.
示例 19-32 展示了从字符串 `struct Pancakes;` 中解析出来的 `DeriveInput` 结构体的相关部分:

```rust
DeriveInput {
    // --snip--

    ident: Ident {
        ident: "Pancakes",
        span: #0 bytes(95..103)
    },
    data: Struct(
        DataStruct {
            struct_token: Struct,
            fields: Unit,
            semi_token: Some(
                Semi
            )
        }
    )
}
```

示例 19-32: 解析示例 19-30 中带有 macro attribute 的代码时, 得到的 `DeriveInput` 实例

该`结构体`的字段展示了, 我们解析完的 Rust 代码, 是一个`类单元结构体`, 其 `ident`( identifier, 表示名字)为 `Pancakes`.
该`结构体`里面有更多字段, 描述了其中各种类型的 Rust 代码,
查阅 [syn 中 DeriveInput 的文档](https://docs.rs/syn/0.14.4/syn/struct.DeriveInput.html) 以获取更多信息.

待会儿, 我们将定义 `impl_hello_macro` 函数, 它用于构建新的 Rust 代码.
但在此之前, 注意其输出也是 `TokenStream`.  它返回的 `TokenStream` 会被添加到 crate 用户所写的代码中,
因此, 当用户编译他们的 `crate` 时, 他们会获取到我们提供的额外功能.

你可能也注意到了, 当调用 `syn::parse` 函数失败时, 我们用 `unwrap` 来使 `hello_macro_derive` 函数 `panic`.
在错误时 `panic`, 对`过程宏`来说是必须的,
因为 `proc_macro_derive` 函数必须返回 `TokenStream` 而不是 `Result`,这样才符合过程宏的 `API`.
这里选择用 `unwrap` 来简化这个例子;
在生产代码中, 应通过 `panic!` 或 `expect` 提供更加明确的错误信息, 来提示错误的种类.

现在我们实现了,将被注解的 `Rust` 代码从 `TokenStream` 转换为 `DeriveInput` 实例,
下一步让我们在被注解的 type 上实现 `HelloMacro` trait, 如示例 19-33 所示.

文件名: hello_macro_derive/src/lib.rs

```rust
fn impl_hello_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl HelloMacro for #name {
            fn hello_macro() {
                println!("Hello, Macro! My name is {}", stringify!(#name));
            }
        }
    };
    gen.into()
}
```

示例 19-33: 使用解析过的 `Rust` 代码实现 `HelloMacro` trait

我们得到一个 `Ident` 结构体的实例, 被注解 type 的名字(identifier)包含在 `ast.ident` 中.
示例 19-32 中的结构体表明, 当 `impl_hello_macro` 函数运行于示例 19-30 中的代码上时,  `ident` 字段的值是 `"Pancakes"`.
因此, 示例 19-33 中 `name` 变量会包含 `Ident` 结构体的实例,
当打印时, 恰好是字符串 `"Pancakes"`, 也就是示例 19-30 中结构体的名称.

`quote!` 宏可以让我们编写想要返回的 `Rust` 代码.
`quote!` 宏执行的直接结果, 并不是编译器期望的输入, 所以还需要转换为 `TokenStream`.
为此需要调用 `into` 方法, 它会消费这个中间表示(intermediate representation, IR)并返回所需的 `TokenStream` 类型值.

这个宏也提供了一些非常酷的`模板`机制; 我们可以写 `#name` , 然后 `quote!` 会以用变量 `name` 中的`值`来替换它.
你甚至执行一些重复工作, 类似于常用宏的工作方式. 查阅 [quote crate 的文档](https://docs.rs/quote/1.0.10/quote/) 来获取详尽的介绍.

我们期望, 对于用户注解过的 type, 此`过程宏`能够生成 `HelloMacro` trait 的实现,  `type` 可以通过 `#name` 获取.
该 `trait` 的实现有一个函数 `hello_macro` , 其函数体包括了我们期望提供的功能:
打印 `Hello, Macro! My name is` 和被注解的 type 名.

此处所使用的 `stringify!` 为 Rust 内置宏.
它接收一个 Rust 表达式, 如 `1 + 2`,  然后在编译时将表达式转换为`字符串常量`, 如 `"1 + 2"`.
它与 `format!` 或 `println!` 是不同的, 后者`计算`表达式, 然后将结果转换为 `String`.
这种情况时可能的, 即所输入的 `#name` 是一个需要原样打印的`表达式`, 因此我们使用 `stringify!` .
在编译时, `stringify!` 也保留了一份内存分配, 通过将 `#name` 转换为字符串字面值.

此时, `cargo build` 应该能成功编译 `hello_macro` 和 `hello_macro_derive` .
我们将这些 `crate` 连接(hook up)到示例 19-30 的代码中, 来看看`过程宏`的运行情况吧!
在 `projects` 目录下用 `cargo new pancakes` 命令新建一个二进制项目.
需要将 `hello_macro` 和 `hello_macro_derive` 作为依赖, 添加到 `pancakes` 包的 `Cargo.toml` 文件中去.
如果你正将 `hello_macro` 和 `hello_macro_derive` 的版本发布到 crates.io 上, 它们只需用常规依赖的写法;
如果不是, 则可以像下面这样将其指定为 `path 依赖`:

```conf
[dependencies]
hello_macro = { path = "../hello_macro" }
hello_macro_derive = { path = "../hello_macro/hello_macro_derive" }
```

把示例 19-30 中的代码放在 `src/main.rs` , 然后执行 `cargo run`: 它应该打印 `Hello, Macro! My name is Pancakes!`.
其包含了该`过程宏`中 `HelloMacro` trait 的实现, 而无需 `pancakes` crate 实现它;
`#[derive(HelloMacro)]` 增加了该 trait 实现.

接下来, 让我们探索一下其他类型的`过程宏`与`自定义派生宏`有何区别.

## 属性宏

`属性宏`(attribute-like)与自定义派生宏相似, 但不同于为 `derive` attribute 生成代码, 它们允许你创建新的 `attributes`.
它们也更为灵活; `derive` 只能用于结构体和枚举; `attributes`还可以用于其它的项, 比如`函数`.
作为使用`属性宏`的例子, 可以创建一个名为 `route` 的属性, 用于注解 `web 应用程序框架`(web application framework)的函数:

```rust
#[route(GET, "/")]
fn index() {
```

`#[route]` 属性将由框架本身定义为`过程宏`. 其`宏定义`的函数签名看起来像这样:

```rust
#[proc_macro_attribute]
pub fn route(attr: TokenStream, item: TokenStream) -> TokenStream {
```

这里有两个 `TokenStream` type 的参数; 第一个用于 attribute contents 本身, 也就是 `GET, "/"` 部分.
第二个是 attribute 所标记的项: 在本例中, 是 `fn index() {}` 和剩下的函数体.

除此之外, 类属性宏与自定义派生宏工作方式一致: 创建 `proc-macro` type 的 crate, 并实现生成目标代码的函数!

## 函数宏

[使用 macro_rules! 的声明宏用于通用元编程]: https://kaisery.github.io/trpl-zh-cn/ch19-06-macros.html

`Function-like` 宏定义看起来像函数调用.
类似于 `macro_rules!`, 但它们比函数更灵活; 例如, 可以接受未知数量的参数.

然而 `macro_rules!` 宏只能使用之前 [使用 macro_rules! 的声明宏用于通用元编程][] 介绍的 `match-like` 的语法定义.
`Function-like` 宏获取 `TokenStream` 参数, 在其定义中, 使用 Rust 代码操纵 `TokenStream`, 就像另两种`过程宏`一样.
一个`类函数宏`的例子是, 以如下方式被调用的 `sql!` 宏:

```rust
let sql = sql!(SELECT * FROM posts WHERE id=1);
```

这个宏会解析其中的 `SQL` 语句, 并检查句法(syntactically)是否正确, 它可以实现比 `macro_rules!` 更复杂的处理.
`sql!` 宏的定义应该类似于:

```rust
#[proc_macro]
pub fn sql(input: TokenStream) -> TokenStream {
```

这类似于`自定义派生宏`的签名: 获取括号中的 `token`, 并返回想要生成的代码.

## 总结

好的! 现在我们学习了 Rust 中并不常用, 但在特定情况下你可能用得着的功能.
我们介绍了很多复杂的主题, 这样若你在错误信息提示, 或阅读他人代码时遇到他们,
至少可以说之前已经见过这些概念和语法了. 你可以使用本章作为寻找解决方案的参考.

接下来, 我们将再开始一个项目, 将本书所学的所有内容付诸实践!
