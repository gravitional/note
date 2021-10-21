# 引用与借用

[ch04-02-references-and-borrowing.md](https://github.com/rust-lang/book/blob/main/src/ch04-01-what-is-ownership.md)

示例 4-5 中的元组代码有这样一个问题:
我们必须将 `String` 返回给调用函数, 以便在调用 `calculate_length` 后仍能使用 `String`, 因为 `String` 被移动到了 `calculate_length `内.

下面是如何定义并使用一个(新的) `calculate_length` 函数, 它以对象的`引用`作为参数, 而不是获取 `值` 的 `所有权`:

```rust
fn main() {
    let s1 = String::from("hello");

    let len = calculate_length(&s1);

    println!("The length of '{}' is {}.", s1, len);
}

fn calculate_length(s: &String) -> usize {
    s.len()
}
```

首先, 注意 `变量声明` 和 `函数返回值` 中的所有 `元组代码` 都消失了.
其次, 注意我们传递 `&s1` 给 `calculate_length`, 同时在函数定义中, 我们获取 `&String` 而不是 `String`.

这些 `&` 符号就是 `引用`, 它们允许你使用值但 `不获取` 其所有权. 图 4-5 展示了一张示意图.

![&String s pointing at String s1](https://kaisery.github.io/trpl-zh-cn/img/trpl04-05.svg)

图 4-5: `&String s` 指向 `String s1` 示意图

> 注意: 与使用 `&` 引用相反的操作是 `解引用`(dereferencing), 它使用 `解引用运算符`, `*`.
>我们将会在第八章遇到一些解引用运算符, 并在第十五章详细讨论解引用.

仔细看看这个函数调用:

```rust
let s1 = String::from("hello");
let len = calculate_length(&s1);
```

`&s1` 语法让我们创建一个 指向 `值 s1` 的引用, 但是 `并不拥有它`. 因为并不拥有这个值, 当 `引用` 离开作用域时其指向的值也不会被丢弃.

同理, 函数签名使用 `&` 来表明参数 `s` 的类型是一个 `引用`. 让我们增加一些解释性的注释:

```rust
fn calculate_length(s: &String) -> usize { // s 是对 String 的引用
    s.len()
} // 这里, s 离开了作用域. 但因为它并不拥有引用值的所有权,
  // 所以什么也不会发生
```

变量 `s` 有效的作用域与函数参数的 `作用域` 一样, 不过当引用离开作用域后并不丢弃它指向的数据, 因为我们没有 `所有权`.
当函数使用 `引用` 而不是 `实际值` 作为参数, 无需返回值来交还 `所有权`, 因为就不曾拥有 `所有权`.

我们将获取 `引用` 作为函数参数称为 `借用`(borrowing).
正如现实生活中, 如果一个人拥有某样东西, 你可以从他那里借来. 当你使用完毕, 必须还回去.

如果我们尝试 `修改` 借用的变量呢? 尝试示例 4-6 中的代码. 剧透: 这行不通!

```rust,ignore,does_not_compile
fn main() {
    let s = String::from("hello");

    change(&s);
}

fn change(some_string: &String) {
    some_string.push_str(", world");
}
```

示例 4-6: 尝试修改借用的值

错误信息:

```log
error[E0596]: cannot borrow immutable borrowed content `*some_string` as mutable
 --> error.rs:8:5
  |
7 | fn change(some_string: &String) {
  |                        ------- use `&mut String` here to make mutable
8 |     some_string.push_str(", world");
  |     ^^^^^^^^^^^ cannot borrow as mutable
```

正如 `变量` 默认是不可变的, `引用`也一样. (默认)不允许修改 `引用` 的值.

### 可变引用

我们通过一个小调整就能修复示例 4-6 代码中的错误:

```rust
文件名: src/main.rs

fn main() {
    let mut s = String::from("hello");

    change(&mut s);
}

fn change(some_string: &mut String) {
    some_string.push_str(", world");
}
```

首先, 必须将 `s` 改为 `mut`. 然后必须创建一个可变引用 `&mut s` 和接受一个可变引用 `some_string: &mut String`.

不过 `可变引用` 有一个很大的限制: 在`特定作用域`中的特定数据只能有`一个`可变引用. 这些代码会失败:

```rust
let mut s = String::from("hello");

let r1 = &mut s;
let r2 = &mut s;

println!("{}, {}", r1, r2);
```

错误如下:

```log
error[E0499]: cannot borrow `s` as mutable more than once at a time
 --> src/main.rs:5:14
  |
4 |     let r1 = &mut s;
  |              ------ first mutable borrow occurs here
5 |     let r2 = &mut s;
  |              ^^^^^^ second mutable borrow occurs here
6 |
7 |     println!("{}, {}", r1, r2);
  |                        -- first borrow later used here
```

这个限制允许可变性, 不过是以一种受限制的方式允许. 新 `Rustacean` 们经常难以适应这一点, 因为大部分语言中变量 `任何时候` 都是可变的.

这个限制的好处是 `Rust` 可以在编译时就避免 `数据竞争`. 数据竞争(data race)类似于竞态条件, 它可由这三个行为造成:

+ 两个或更多指针同时访问同一数据.
+ 至少有一个指针被用来写入数据.
+ 没有同步数据访问的机制.

`数据竞争` 会导致未定义行为, 难以在运行时追踪, 并且难以诊断和修复;
`Rust` 避免了这种情况的发生, 因为它甚至不会编译存在数据竞争的代码!

一如既往, 可以使用 `大括号` 来创建一个新的作用域, 以允许拥有多个 `可变引用`, 只是不能 同时 拥有:

```rust
let mut s = String::from("hello");

{
    let r1 = &mut s;

} // r1 在这里离开了作用域, 所以我们完全可以创建一个新的引用

let r2 = &mut s;
```

类似的规则也存在于 `同时` 使用 `可变`与 `不可变引用` 中. 这些代码会导致一个错误:

```rust
let mut s = String::from("hello");

let r1 = &s; // 没问题
let r2 = &s; // 没问题
let r3 = &mut s; // 大问题

println!("{}, {}, and {}", r1, r2, r3);
```

错误如下:

```log
error[E0502]: cannot borrow `s` as mutable because it is also borrowed as immutable
 --> src/main.rs:6:14
  |
4 |     let r1 = &s; // no problem
  |              -- immutable borrow occurs here
5 |     let r2 = &s; // no problem
6 |     let r3 = &mut s; // BIG PROBLEM
  |              ^^^^^^ mutable borrow occurs here
7 |
8 |     println!("{}, {}, and {}", r1, r2, r3);
  |                                -- immutable borrow later used here
```

哇哦! 我们 **也** 不能在拥有 `不可变引用` 的同时拥有 `可变引用`.
`不可变引用`的用户可不希望在他们的眼皮底下值就被意外的改变了!
然而, `多个` 不可变引用是可以的, 因为没有哪个 `只能读取数据` 的人有能力影响其他人读取到的数据.

注意一个引用的 `作用域` 从声明的地方开始一直持续到 `最后一次使用` 为止.
例如, 因为最后一次使用 `不可变引用`, 在声明 `可变引用 `之前, 所以如下代码是可以编译的:

```rust
let mut s = String::from("hello");

let r1 = &s; // 没问题
let r2 = &s; // 没问题
println!("{} and {}", r1, r2);
// 此位置之后 r1 和 r2 不再使用

let r3 = &mut s; // 没问题
println!("{}", r3);
```

不可变引用 `r1` 和 `r2` 的作用域在 `println!` 最后一次使用之后结束, 这也是创建可变引用 `r3` 的地方.
它们的 `作用域` 没有重叠, 所以代码是可以编译的.

尽管这些错误有时使人沮丧, 但请牢记这是 Rust 编译器在提前指出一个潜在的 bug(在编译时而不是在运行时), 并精准显示问题所在.
这样你就不必去跟踪为何数据并不是你想象中的那样.

### 悬垂引用(Dangling References)

在具有 `指针` 的语言中, 很容易通过释放内存时, 保留指向它的指针而错误地生成一个 `悬垂指针`(dangling pointer),
所谓 `悬垂指针` 是其指向的内存可能已经被分配给其它持有者.
相比之下, 在 `Rust` 中编译器确保引用永远也不会变成 `悬垂状态`: 当你拥有一些数据的 `引用`, 编译器确保数据不会在其引用之前 `离开作用域`.

让我们尝试创建一个 `悬垂引用`, `Rust` 会通过一个编译时错误来避免:

```rust
fn main() {
    let reference_to_nothing = dangle(); // 返回一个悬垂指针
}

fn dangle() -> &String {
    let s = String::from("hello");

    &s // s 出作用域会被释放
}
```

错误信息是:

```log
error[E0106]: missing lifetime specifier
 --> main.rs:5:16
  |
5 | fn dangle() -> &String {
  |                ^ expected lifetime parameter
  |
  = help: this function's return type contains a borrowed value, but there is no value for it to be borrowed from
  = help: consider giving it a 'static lifetime
```

`错误信息`引用了一个我们还未介绍的功能: `生命周期`(lifetimes).
第十章会详细介绍生命周期. 不过, 如果你不理会 `生命周期` 部分, 错误信息中确实包含了为什么这段代码有问题的关键信息:

    this function's return type contains a borrowed value, but there is no value for it to be borrowed from.

让我们仔细看看我们的 `dangle` 代码的每一步到底发生了什么:

```rust
fn dangle() -> &String { // dangle 返回一个字符串的引用

    let s = String::from("hello"); // s 是一个新字符串

    &s // 返回字符串 s 的引用
} // 这里 s 离开作用域并被丢弃. 其内存被释放.
  // 危险!
```

因为 `s` 是在 `dangle` 函数内创建的, 当 `dangle` 的代码执行完毕后, `s` 将被释放.
不过我们尝试返回它的 `引用`. 这意味着这个 `引用` 会指向一个无效的 `String`, 这可不对! `Rust` 不会允许我们这么做.

这里的解决方法是直接返回 `String`:

```rust
fn no_dangle() -> String {
    let s = String::from("hello");

    s
}
```

这样就没有任何错误了. 所有权被移动出去, 所以没有 `值` 被释放.

### 引用的规则

让我们概括一下之前对引用的讨论:

+ 在任意给定时间, **要么** 只能有一个可变引用, **要么** 只能有多个不可变引用.
+ 引用必须总是有效的.

接下来, 我们来看看另一种不同类型的引用: `slice`.
