# 高级函数与闭包

    ch19-05-advanced-functions-and-closures.md
    commit 426f3e4ec17e539ae9905ba559411169d303a031

接下来我们将探索一些有关函数和闭包的高级功能: `函数指针`以及`返回值`闭包.

## 函数指针

我们讨论过了如何向`函数`传递`闭包`; 也可以向函数传递`常规函数`!
这在我们希望传递已经定义的函数, 而不是重新定义闭包作为参数时很有用.

`函数指针` 允许我们使用`函数`作为另一个函数的`参数`.
函数的`type`是 `fn` (使用小写的 `f` ), 以免与 闭包 trait `Fn` 相混淆.
`fn` 被称为 `函数指针`(function pointer). 指定参数为函数指针的语法类似于闭包, 如示例 19-27 所示:

文件名: src/main.rs

```rust
fn add_one(x: i32) -> i32 {
    x + 1
}

fn do_twice(f: fn(i32) -> i32, arg: i32) -> i32 { // 指定 fn(i32)->i32 type
    f(arg) + f(arg)
}

fn main() {
    let answer = do_twice(add_one, 5);

    println!("The answer is: {}", answer);
}
```

示例 19-27: 使用 `fn` 类型, 从而接受函数指针作为参数

这会打印出 `The answer is: 12`. `do_twice` 中的 `f` 被指定为:
接受 `i32` 参数并返回 `i32` 值 的 `fn`. 接着就可以在 `do_twice` 函数体中调用 `f`.
在 `main` 中, 可以将函数名 `add_one` 作为首参数传递给 `do_twice`.

不同于闭包, `fn` 是类型而不是 `trait`,
所以直接指定 `fn` 作为参数, 而不是声明带有 `Fn` 作为 `trait` bound 的泛型参数.

函数指针实现了所有三个 `闭包` trait(`Fn`, `FnMut` 和 `FnOnce`),
所以在调用函数, 而它期望接收闭包时, 总是可以传递`函数指针`作为参数.
我们倾向于编写使用`泛型`和`闭包 trait` 的函数, 这样它就能接受`函数`或`闭包`作为参数.

只期望接受 `fn`, 而不接受`闭包`的情况的例子是, 与不存在`闭包`的外部代码交互时:
`C 语言`的函数可以接受函数作为参数, 但 C 语言没有`闭包`.

让我们看个`map`的例子, 说明既可以使用内联(inline)定义的闭包, 又可以使用命名函数:
使用 `map` 函数将数字 `vector` 转换为字符串 `vector`, 我们可以使用闭包, 例如:

```rust
let list_of_numbers = vec![1, 2, 3];
let list_of_strings: Vec<String> = list_of_numbers
    .iter()
    .map(|i| i.to_string())
    .collect();
```

或者我们可以将函数作为 `map` 的参数来代替闭包, 像是:

```rust
let list_of_numbers = vec![1, 2, 3];
let list_of_strings: Vec<String> = list_of_numbers
    .iter()
    .map(ToString::to_string)
    .collect();
```

注意这里必须使用 ["高级 trait" 部分](https://kaisery.github.io/trpl-zh-cn/ch19-03-advanced-traits.html)
讲到的完全限定语法, 因为存在多个叫做 `to_string` 的函数;
这里使用了定义于 `ToString` trait 的 `to_string` 函数, 标准库为所有实现了 `Display` 的类型实现了这个 `trait`.

还有一个实用的模式, 它利用了`元组结构体`和`元组结构体枚举成员`的实现细节.
这些 types 使用 `()` 作为初始化的语法, 看起来就像`函数调用`.
这些`初始化器`实际上是作为`函数`实现的, 它返回一个由`参数`构造的`实例`.
我们可以将这些`初始化函数`作为`函数指针`, 它们实现了闭包 traits,
这意味着我们可以将 `初始化函数` 作为`参数`,  传递给接受闭包的方法, 就像这样:

```rust
enum Status {
    Value(u32),
    Stop,
}

let list_of_statuses: Vec<Status> =
    (0u32..20)
    .map(Status::Value) // 传递初始化函数
    .collect();
```

在这里, 我们通过在范围上调用 `map`, 并传递 `Status::Value` 的初始化函数作为参数,
用范围内的每个 `u32` 值创建了 `Status::Value` 的实例.

一些人倾向于函数风格, 一些人喜欢闭包.
这两种形式最终会编译成相同的代码, 所以请使用对你更直白的形式.

## 返回闭包

`闭包` 用 `trait` 表示, 这意味着不能直接返回闭包.
对于大部分想返回 `trait` 的情况, 你可以使用具体类型替代, 来作为函数的返回值,
只要具体类型实现了期望返回的 `trait`.

但是你不能返回`闭包`, 因为他们没有具体的类型, 可以作为返回值(returnable);
例如, 你不能使用函数指针 `fn` 作为返回值类型.

这段代码尝试直接返回闭包, 它并不能编译:

```rust
fn returns_closure() -> Fn(i32) -> i32 {
    |x| x + 1
}
```

编译器给出的错误是:

```rust
$ cargo build
   Compiling functions-example v0.1.0 (file:///projects/functions-example)
error[E0746]: return type cannot have an unboxed trait object
 --> src/lib.rs:1:25
  |
1 | fn returns_closure() -> dyn Fn(i32) -> i32 {
  |                         ^^^^^^^^^^^^^^^^^^ doesn't have a size known at compile-time // 不知道返回值的大小
  |
  = note: for information on `impl Trait`, see <https://doc.rust-lang.org/book/ch10-02-traits.html#returning-types-that-implement-traits>
help: use `impl Fn(i32) -> i32` as the return type, as all return paths are of type `[closure@src/lib.rs:2:5: 2:14]`, which implements `Fn(i32) -> i32`
  |
1 | fn returns_closure() -> impl Fn(i32) -> i32 {
  |                         ^^^^^^^^^^^^^^^^^^^
...
```

错误又一次指向了 `Sized` trait! `Rust` 并不知道需要多少空间来储存`闭包`.
不过我们在上一部分见过这种情况的解决办法: 可以使用 `trait 对象`:

```rust
fn returns_closure() -> Box<dyn Fn(i32) -> i32> {
    Box::new(|x| x + 1)
}
```

这段代码可以无痛编译. 关于 trait 对象的更多内容,
请回顾第十七章的 ["为使用不同类型的值而设计的 trait 对象" 部分](https://kaisery.github.io/trpl-zh-cn/ch17-02-trait-objects.html).

接下来让我们学习宏!
