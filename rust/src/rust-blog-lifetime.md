# Rust - 生命周期解释

[Rust - 生命周期](https://rustcc.cn/article?id=0d606476-0a98-4f5a-afba-951f999408e6)

## 介绍

对于很多 Rust 的初学者来说, 生命周期 (lifetime) 是一个很难掌握的概念.
我也为此挣扎了一段时间, 才开始明白它们对 Rust 编译器执行其职责 (move/borrow) 是多么重要.
lifetime 本身并不难. 只是它们是看似新颖的结构, 以至大多数程序员都没在其他语言中见过它.
更糟糕的是, 人们过多地使用 "lifetime" 这个词来谈论很多密切相关的问题.
在本文中, 我将把这些想法分开, 这样做是为了给你提供清晰地思考 lifetime 的工具.

## 目的

在讨论细节之前, 让我们先了解一下为什么需要生命周期. 它们的目的是什么?
生命周期可以帮助编译器执行一个简单的规则: 任何引用本身都不能比它引用的对象存活地更久.
换句话说, 生命周期帮助编译器消除悬空指针错误(注: 也就是说在引用的情况下才会出现生命周期标注).

在下面的例子中你将看到, 编译器通过分析相关变量的生命周期来实现这一点.
如果引用的 lifetime小于被引用的 lifetime, 编译成功, 否则编译失败.

## "lifetime"

生命周期如此令人困惑的部分原因是在 Rust 的大部分文章中, 生命周期这个词被松散地用于指代三种不同的东西:

    变量的实际生命周期
    生命周期约束
    生命周期注释

下面让我们一个一个地来谈谈.
variables 生命周期

代码中变量之间的交互模式对它们的 lifetime 产生了一些限制.
例如, 在下面的代码中: `x = &y;` 这一行添加了一个约束, 即:
`x` 的 `lifetime` 应该包含在 `y` 的 `lifetime` 内 ( `x ≤ y`):

```rust
//error:`y` does not live long enough
{
let x: &Vec<i32>;
    {
                 let y =Vec::new();//----+
//                               | y's lifetime
//                               |
        x = &y;//----------------|--------------+
//                               |              |
    }// <------------------------+              | x's lifetime
    println!("x's length is {}", x.len());//    |
}// <-------------------------------------------+
```

如果没有添加这个约束, x 就会在 println! 代码块中访问无效的内存.
因为 x 是对 y 的引用, 它将在前一行被销毁.

需要注意的是: 约束不会改变实际的生存期
--  例如, x 的 lifetime 实际上仍然会扩展到外部块的末尾
--  lifetime 只是编译器用来禁止悬空引用的工具.
在上面的例子中, 实际的生存期不满足约束:x 的 lifetime 已经超出了 y 的 lifetime. 因此, 这段代码无法编译.

## 生命周期注释

如上一节所示, 很多时候编译器会(自动)生成所有的 `lifetime` 约束.
但是随着代码变得越来越复杂, 编译器会要求开发者手动添加约束.
程序员通过生命周期注释来实现这一点.
例如, 在下面的代码中, 编译器需要知道 `print_ret()` 返回的引用是借用了 `s1` 还是 `s2`, 所以编译器要求程序员显式地添加这个约束:

```rust
// error:missing lifetime specifier
// this function's return type contains a borrowed value,
// but the signature does not say whether it is borrowed from `s1` or `s2`
fn print_ret(s1: &str,s2: &str) -> &str{
    println!("s1 is {}", s1);
    s2
}
fn main() {
        let some_str:String= "Some string".to_string();
        let other_str:String= "Other string".to_string();
        let s1 = print_ret(&some_str, &other_str);
}
```

>📒: 如果您想知道为什么编译器不能看到输出引用是从 `s2` 中借来的, 可以看下文.
>要查看编译器何时可以省略 `lifetime` 注释, 请参阅下面的 lifetime 省略部分.

然后, 开发者用 `'a` 标记 `s2` 和返回的引用, 用来告诉编译器, 返回值是从 `s2` 中借来的.

```rust
fn print_ret<'a>(s1: &str,s2: &'astr) -> &'astr{
    println!("s1 is {}", s1);
    s2
}
fn main() {
        let some_str:String= "Some string".to_string();
        let other_str:String= "Other string".to_string();
        let s1 = print_ret(&some_str, &other_str);
}
```

不过我想强调的是, 仅仅因为 `'a` 标记在参数 `s2` 和返回的引用上, 并不意味着 `s2` 和返回的引用都有完全相同的 `lifetime`.
相反, 这应该被理解为: 返回的带有 `'a` 标记的引用, 是从具有相同标记的参数中借用来的.

由于 `s2` 进一步借用了 `other_str`, `lifetime` 约束是返回的引用不能超过 `other_str` 的 `lifetime`.
这里满足约束, 编译成功:

```rust
fn print_ret<'a>(s1: &str, s2: &'a str) -> &'a str {
    println!("s1 is {}", s1);
    s2
}
fn main() {
    let some_str: String = "Some string".to_string();
    let other_str: String = "Other string".to_string();//-------------+
    let ret = print_ret(&some_str, &other_str);//---+                 | other_str's lifetime
    //                                              | ret's lifetime  |
}// <-----------------------------------------------+-----------------+
```

在展示更多示例之前, 简要介绍一下 `lifetime` 注释语法.

要创建 lifetime 注释, 必须首先声明 `lifetime` 参数. 例如, `<'a>` 是一个生命周期声明.
`lifetime参数` 是一种通用的参数, 您可以将 `<'a>` 读为 "for some Lifetime 'a... " .
一旦声明了 `lifetime` 参数, 就可以在其他引用中使用它来创建 lifetime 约束.

记住, 通过用 `'a` 标记引用, 程序员只是在构造一些约束;
然后, 编译器的工作就是为 `'a` 找到满足其约束的具体生存期.

## 示例

接下来, 考虑一个求出两个值的最小值的函数:

```rust
fn min<'a>(x: &'a i32, y: &'a i32) -> &'a i32 {
    if x < y {
        x
    } else {
        y
    }
}
fn main() {
    let p = 42;
    {
        let q = 10;
        let r = min(&p, &q);
        println!("Min is {}", r);
    }
}
```

在这里, 'a lifetime 注释标记了参数 x, y 和返回值. 这意味着返回值可以从 x 或 y 中借用.
而又因为 x 和 y 分别进一步从 p 和 q 中借用, 所以返回的引用的生命周期应该包含在 p 和 q 的生命周期中.
这段代码也可以编译, 满足了约束:

```rust
fn min<'a>(x: &'a i32, y: &'a i32) -> &'a i32 {
    if x < y {
        x
    } else {
        y
    }
}
fn main() {
    let p = 42;//-------------------------------------------------+
    {//                                                           |
        let q = 10;//------------------------------+              | p's lifetime
        let r = min(&p, &q);//------+              | q's lifetime |
        println!("Min is {}", r);// | r's lifetime |              |
    }// <---------------------------+--------------+              |
}// <-------------------------------------------------------------+
```

通常, 当同一 lifetime 参数标记一个函数的两个或多个形参时, 返回的引用不能超过形参最小的lifetime.

最后一个例子, 这是许多 C++ 新手开发者犯的一个错误, 即返回一个指向局部变量的指针.
而在 Rust 中不允许有类似的行为:

```rust
//Error:cannot return reference to local variable `i`
fn get_int_ref<'a>() -> &'a i32 {
    let i: i32 = 42;
    &i
}
fn main() {
    let j = get_int_ref();
}
```

由于 `get_int_ref()` 没有参数, 因此编译器知道返回的引用得从局部变量中借用, 而这是不允许的.
编译器正确地避免了 `bug`, 因为当返回的引用试图访问它指向的内存时, 局部变量将被清理掉.

```rust
fn get_int_ref<'a>() -> &'a i32 {
    let i: i32 = 42;//-------+
    &i//                     | i's lifetime
}// <------------------------+
fn main() {
    let j = get_int_ref();//-----+
//                               | j's lifetime
}// <----------------------------+
```

## 省略的场景

当编译器允许开发者省略 lifetime 注释时, 称为 lifetime省略.
再说一遍, "生命周期省略"一词也具有误导性  --  lifetime 与变量的产生和销毁有着密不可分的关系, 又怎么可能省略 lifetime 呢?

所以被省略的不是 lifetime, 而是 lifetime 注释和扩展的 lifetime 约束.
在 Rust 编译器的早期版本中, 不允许省略, 并且需要对每个生命周期进行标注.
但随着时间的推移, 编译器团队观察到生命周期注释的相同模式被重复, 因此修改编译器规则, 从而推断它们.

在以下情况下, 程序员可以省略标记:

    当只有一个输入参数引用时: 在这种情况下, 输入参数的生命周期注释会被赋值给所有输出参数引用.
    例如: fn some_func(s: &str) -> &str 被推断为 fn some_func<'a>(s: &'a str) -> &'a str
    当有多个传入参数引用时, 但第一个参数是: &self/&mut self: 在这种情况下, 输入参数的生命周期注释也被赋值给所有输出引用.
    例如: fn some_method(&self) -> &str 等价于 fn some_method<'a>(&'a self) -> &'a str

lifetime 标注省略减少了代码中的混乱, 未来编译器可能会推断出更多模式的 lifetime 约束.

## 总结

许多 Rust 新手发现 lifetime 这个话题很难理解.
但 lifetime 本身并不是问题所在, 而是这个概念在 Rust 很多文章中所呈现的方式.
在本文中, 我试图梳理出 "lifetime" 这个词的过度使用背后隐藏的含义.

变量的生命周期必须满足编译器和开发者对它们施加的某些约束, 然后编译器才能确保代码是合理的.
如果没有 lifetime 这种机制, 编译器将无法保证大多数 Rust 程序的安全性.

## 为什么必须显式声明生命周期

[fjh的具体例子展示需要显式生命周期]: https://stackoverflow.com/a/31609892/924313

其他答案都有突出的观点([fjh的具体例子展示需要显式生命周期][]),
但缺少一个关键点: 既然编译器会告诉你弄错了, 为什么还需要显式寿命?

这实际上与 `为什么在编译器可以推断类型的情况下还需要显式类型`是同一个问题.
假设一个例子:

```rust
fn foo() -> _ {
    ""
}
```

当然, 编译器可以看到我返回的是 `&'static str`, 那么为什么程序员要输入它呢?

主要原因是, 虽然编译器可以看到你的代码做了什么, 但它不知道 `你的意图` 是什么.

函数是一个天然的边界, 可以对改变代码的效果建立 `防火墙`.
如果我们允许完全从代码中推测 `lifetime`,
那么看似无害的改变可能会影响到 `lifetime`, 这可能会在很远的函数中引起错误. 这不是一个假想的例子.
据我所知, 当你依赖顶层函数的类型推理时, `Haskell` 也有这个问题.
`Rust` 把这个特殊的问题消灭在萌芽状态.

对编译器来说, 还有一个效率上的好处--为了验证 types 和 lifetimes, 只需要对 `函数签名` 进行解析.
更重要的是, 它对程序员的效率也有好处.
如果我们没有明确的 `lifetime`, 这个函数会做什么?

```rust
fn foo(a: &u8, b: &u8) -> &u8
```

如果不检查源代码, 是不可能知道的, 这就违背了大量的编码最佳实践.

>通过推断出, 程序 `引用` 非法赋值到更大的 scope 内

从本质上讲, 作用域就是 生命周期.
更清楚一点,  lifetime `'a` 是 泛型生命周期参数,
在编译时可以根据 `调用地点`用 特定的 作用域 来具现(specialized).

Scopes are lifetimes, essentially.
A bit more clearly, a lifetime 'a is a generic lifetime parameter that can be specialized with a specific scope at compile time,
based on the call site.

>为了防止 `[...]` 错误, 是否真的需要明确的 lifetime 标注?

根本不需要. lifetime 是用来防止错误的, 但明确的 lifetime 标注 是用来保护程序员仅有的一点理智的.

## 静态生命周期

[Rust 生命周期](https://www.runoob.com/rust/rust-lifetime.html)

生命周期注释有一个特别的: `'static` .
所有用 `双引号` 包括的 `字符串常量` 所代表的精确数据类型都是 `&'static str`,
`'static` 所表示的生命周期从程序运行开始到程序运行结束.

### 泛型, 特性与生命周期协同作战

```rust
use std::fmt::Display;

fn longest_with_an_announcement<'a, T>(x: &'a str, y: &'a str, ann: T) -> &'a str
    where T: Display
{
    println!("Announcement! {}", ann);
    if x.len() > y.len() {
        x
    } else {
        y
    }
}
```
