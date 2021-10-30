# 为使用不同类型的值而设计的 trait 对象

在第八章中, 我们谈到了 `vector` 一个限制是: 只能存储`同种类型`的元素.

示例 8-10 中提供了一个变通方法: 定义 `SpreadsheetCell` 枚举, 来储存 `整型`, `浮点型` 和 `文本` 成员.
这意味着可以在每个`单元格` 中储存不同类型的数据, 并仍能拥有一个代表一行`单元格`的 vector.
如果当我们编译代码时, 我们就知道这些类型, 即 `可用子项` 是一组固定的类型时, 这是一个完美的解决方案, .

然而, 有时我们希望, 代码的库用户在特定情况下, 能够扩展有效的类型集合.

为了展示如何实现这一点, 这里将创建一个`图形用户接口`(Graphical User Interface,  GUI)工具的例子,
它通过遍历列表, 并调用每一个项目的 `draw` 方法来将其绘制到屏幕上 —— 此乃 GUI 工具的常见技术.
我们将要创建一个叫做 `gui` 的库 `crate`, 它含一个 `GUI` 库的结构.
这个 `GUI` 库包含一些可供开发者使用的类型, 比如 `Button` 或 `TextField`.
在此之上, `gui` 的用户希望创建自定义的, 可以绘制于屏幕上的`类型`:
比如, 一个程序员可能会增加 `Image`, 另一个可能会增加 `SelectBox`.

这个例子中并不会实现一个功能完善的 `GUI` 库, 不过会展示其中各个部分是如何结合在一起的.
编写库的时候, 我们不可能知晓并定义, 所有其他程序员希望创建的类型.
我们所知晓的是, `gui` 需要记录一系列不同类型的`值`, 并需要能够对其中每一个`值`调用 `draw` 方法.
这里无需知道调用 `draw` 方法时具体会发生什么, 只需要该`值`会有那个方法可供我们调用.

在拥有`继承`的语言中, 可以定义一个名为 `Component` 的类, 该类上有一个 `draw` 方法.
其他的类比如 `Button`, `Image` 和 `SelectBox` 会从 `Component` 派生, 并因此继承 `draw` 方法.
它们各自都可以覆盖 `draw` 方法来定义自己的行为, 但是框架会把所有这些类型当作是 `Component` 的实例, 并在其上调用 `draw`.
不过 `Rust` 并没有继承, 我们得另寻出路.

## 定义通用行为的 trait

[动态大小类型和 Sized trait]: https://kaisery.github.io/trpl-zh-cn/ch19-04-advanced-types.html#dynamically-sized-types-and-the-sized-trait

为了实现 `gui` 所期望的行为, 让我们定义一个 `Draw` trait, 其中包含名为 `draw` 的方法.
接着可以定义一个存放 `trait 对象`(trait object) 的 `vector`.

`trait 对象` 指向某个`type` 的 实例, 它实现了我们指定的 `trait`,
同时 `trait 对象` 还指向一个表, 用于在运行时查找作用于该`type`的 `trait 方法`.

我们通过指定某种`指针`来创建 `trait 对象`,
例如 `&` 引用或 `Box<T>` 智能指针, 然后是 `dyn` keyword,  然后指定相关的 `trait`.

>第十九章 [动态大小类型和 Sized trait][] 部分会介绍,  `trait` 对象必须使用指针的原因.

我们可以使用 `trait 对象`代替`泛型`或`具体类型`(concrete type).
在任何使用 `trait 对象`的位置, Rust 的`type 系统`会在编译时确保, 任何在此上下文中使用的`值`会实现了 `trait 对象`的 trait.
因此, 我们不需要在编译时知道所有可能遇到的类型.

之前提到过, Rust 刻意不将`结构体`与`枚举`称为 `对象`, 以便与其他语言中的对象相区别.

在`结构体`或`枚举`中, 结构体字段中的`数据`和 `impl` 块中的行为是分开的,
不同于其他语言中将`数据`和`行为`组合进一个称为`对象`的概念中.
`trait 对象` 将`数据`和`行为`两者相结合, 从这种意义上说, `trait objects` 更类似其他语言中的对象.
不过 `trait 对象`不同于传统的对象, 因为不能向 `trait 对象`增加数据.
`trait 对象` 并不像其他语言中的对象那么通用: `trait 对象`具体的作用是对`通用行为`进行抽象.

示例 17-3 展示了如何定义一个带有 `draw` 方法的 `trait Draw`:

文件名: src/lib.rs

```rust
pub trait Draw {
    fn draw(&self);
}
```

示例 17-3: Draw trait 的定义

因为第十章已经讨论过如何定义 trait, 其语法看起来应该比较眼熟. 接下来就是新内容了:
示例 17-4 定义了名为 `Screen` 的结构体:  它存放了名为 `components` 的 `vector`.
这个 `vector` 的类型是 `Box<dyn Draw>`, 后者是一个 `trait 对象`:
它是一个占位符(stand-in), 表示处于 `Box` 中的任何 `type`, 但必须实现了 `Draw trait`.

文件名: src/lib.rs

```rust
pub struct Screen {
    pub components: Vec<Box<dyn Draw>>,
}
```

示例 17-4:  `Screen` 结构体的定义, 它带有字段 `components`, 它是 `vector`, 包含实现了 `Draw` trait 的 trait 对象.

在 `Screen` 结构体上, 我们将定义 `run` 方法, 该方法会对其 `components` 上的每个组件调用 `draw` 方法, 如示例 17-5 所示:

文件名: src/lib.rs

```rust
impl Screen {
    pub fn run(&self) {
        for component in self.components.iter() {
            component.draw();
        }
    }
}
```

示例 17-5: 在 `Screen` 上实现 `run` 方法, 该方法在每个 `component` 上调用 `draw` 方法

这不同于定义一个结构体, 并带有 `trait bounds` 的 `泛型参数`.
This works differently from defining a struct that uses a generic type parameter with trait bounds

`泛型类型`参数一次只能替代一个具体类型, 而 `trait 对象` 则允许在运行时替代多种具体类型.
例如, 可以定义 `Screen` 结构体来使用`泛型`和 `trait bound`, 如示例 17-6 所示:

文件名: src/lib.rs

```rust
pub struct Screen<T: Draw> {
    pub components: Vec<T>,
}

impl<T> Screen<T>
    where T: Draw {
    pub fn run(&self) {
        for component in self.components.iter() {
            component.draw();
        }
    }
}
```

示例 17-6: 一种 `Screen` 结构体的替代实现, 其 `run` 方法使用`泛型`和 `trait bound`

这限制了 `Screen` 实例必须拥有一个全是 `Button` 类型或者全是 `TextField` 类型的组件列表.
如果只需要`同质`(相同类型)集合, 则倾向于使用 `泛型` 和 `trait bound`, 因为在`编译时`, 其定义会被具体类型`单态化`.

另一方面, 通过使用 `trait 对象` 的方式,  `Screen` 实例可以存放一个既能包含 `Box<Button>`, 也能包含 `Box<TextField>` 的 `Vec<T>`.
让我们看看它是如何工作的, 接着会讲到它对`运行时`性能的影响.

## 实现 trait

现在来增加一些实现了 `Draw trait` 的类型. 我们将提供 `Button` 类型.
再一次重申, 真正实现 GUI 库超出了本书的范畴, 所以 draw 方法体中不会有任何有意义的实现.
来想象一下这个实现看起来什么样,  `Button` 结构体可能会拥有 `width`, `height` 和 `label` 字段, 如示例 17-7 所示:

文件名: src/lib.rs

```rust
pub struct Button {
    pub width: u32,
    pub height: u32,
    pub label: String,
}

impl Draw for Button {
    fn draw(&self) {
        // 实际绘制按钮的代码
    }
}
```

示例 17-7: 一个实现了 `Draw` trait 的 `Button` 结构体

在 `Button` 上的 `width`, `height` 和 `label` 字段会和其他组件不同,
比如 `TextField` 可能有 `width`, `height`, `label` 以及 `placeholder` 字段.
每一个我们希望能在屏幕上绘制的`type`, 都会使用不同的代码来实现 Draw trait 的 `draw` 方法, 来定义如何绘制此特定的`type`,
像这里的 `Button` 类型(并不包含任何实际的 `GUI` 代码, 这超出了本章的范畴).
除了实现 `Draw` trait 之外, 比如 `Button` 还可能有另一个`impl` 块, 包含如何响应点击按钮的方法.
这类方法并不适用于像 `TextField` 这样的类型.

如果一些库的使用者决定实现一个包含 `width`, `height` 和 `options` 字段的结构体 `SelectBox`,
并且也为其实现了 Draw trait, 如示例 17-8 所示:

文件名: src/main.rs

```rust
use gui::Draw;

struct SelectBox {
    width: u32,
    height: u32,
    options: Vec<String>,
}

impl Draw for SelectBox {
    fn draw(&self) {
        // code to actually draw a select box
    }
}
```

示例 17-8: 另一个使用 `gui` 的 `crate` 中, 在 `SelectBox` 结构体上实现 Draw trait

库使用者现在可以在他们的 `main` 函数中创建一个 `Screen` 实例.
至此可以通过将 `SelectBox` 和 `Button` 放入 `Box<T>` 转变为 `trait 对象`来增加组件.
接着可以调用 `Screen` 的 `run` 方法, 它会调用每个组件的 `draw` 方法.
示例 17-9 展示了这个实现:

文件名: src/main.rs

```rust
use gui::{Screen, Button};

fn main() {
    let screen = Screen {
        components: vec![
            Box::new(SelectBox {
                width: 75,
                height: 10,
                options: vec![
                    String::from("Yes"),
                    String::from("Maybe"),
                    String::from("No")
                ],
            }),
            Box::new(Button {
                width: 50,
                height: 10,
                label: String::from("OK"),
            }),
        ],
    };

    screen.run();
}
```

示例 17-9: 使用 `trait` 对象来存储, 实现了相同 `trait` 的不同 `type` 的值

当编写库的时候, 我们不知道何人会在何时增加 `SelectBox` 类型,
不过 `Screen` 的实现能够操作并绘制这个新类型, 因为 `SelectBox` 实现了 `Draw` trait, 这意味着它实现了 `draw` 方法.

这个概念--只关心一个`值`所响应的`消息`, 而不是值的具体类型--类似于`动态类型语言`中的`鸭子类型`(duck typing)的概念:
如果它走路像鸭子, 叫声像鸭子, 那么它就是鸭子.

在示例 17-5 中 Screen 上的 run 实现中, `run` 不需要知道每个组件的具体 `type` 是什么.
它并不检查组件到底是 `Button` 还是 `SelectBox` 的实例, 它只是调用该组件的 `draw` 方法.
通过指定 `Box<dyn Draw>` 作为`components` vector 中`值`的类型,
我们已经将 `Screen` 定义为, 需要接收具有 `draw` 方法的`值`.

使用 `trait` 对象和 `Rust` 类型系统来编写代码, 实现类似`鸭子类型` 的操作,其优势是:
无需在`运行时`检查一个`值`是否实现了`特定方法`, 或者担心, 因为`值`没有实现方法, 而在调用时产生错误.
如果 `值` 没有实现 `trait 对象`所需的 `trait`, 则 Rust `不会编译`这些代码.

例如, 示例 17-10 展示了, 用 `String` 做为组件, 创建 `Screen` 时发生的情况:

文件名: src/main.rs

```rust
[这些代码不能编译! ]
use gui::Screen;

fn main() {
    let screen = Screen {
        components: vec![
            Box::new(String::from("Hi")),
        ],
    };

    screen.run();
}
```

示例 17-10: 尝试使用一种没有实现 `trait 对象`的 trait 的类型

我们会遇到这个错误, 因为 `String` 没有实现 `rust_gui::Draw` trait:

```log
error[E0277]: the trait bound `std::string::String: gui::Draw` is not satisfied
  --> src/main.rs:7:13
   |
 7 |             Box::new(String::from("Hi")),
   |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ the trait gui::Draw is not
   implemented for `std::string::String`
   |
   = note: required for the cast to the object type `gui::Draw`
```

这告诉了我们, 要么是我们传递了并不应该传递给 `Screen` 的类型, 所以应该提供其他类型,
要么应该在 `String` 上实现 `Draw`, 以便 `Screen` 可以调用其上的 `draw`.

## trait 对象执行动态分发

[泛型代码的性能]: https://kaisery.github.io/trpl-zh-cn/ch10-01-syntax.html#performance-of-code-using-generics

回忆一下第十章 [泛型代码的性能][] 部分讨论过的, 当对泛型使用 `trait bound` 时编译器所进行单态化处理(monomorphization):
对于每个被`泛型 type 参数` 代表的 `具体类型`, 编译器生成`非泛型`的函数和方法实现.

`单态化`所产生的代码进行 `静态分发`(static dispatch).
`静态分发` 需要在编译时, `编译器`就知道要调用哪个方法.
这与 `动态分发` (dynamic dispatch)相对, 这时编译器在编译时无法知晓调用了什么方法.
在`动态分发`的情况下, `编译器`会生成特定代码, 后者在运行时确定调用哪个方法.

当使用 `trait` 对象时, Rust 必须使用 `动态分发`.
`编译器` 不知道 所有可能的`type`,  它们都可能用于包含 `trait 对象` 的代码 ,
所以`编译器`也不知道应该调用, 哪个类型实现的哪个方法.

为此, Rust 在运行时使用 `trait 对象` 内部的指针, 来知晓需要调用哪个方法.
当这种查找发生时, 会有一个运行时消耗, 而`静态分发`则没有这种消耗.

`动态分发`也阻止编译器将方法的代码变成内联的(inline), 这会相应的禁用一些优化.
尽管在编写示例 17-5 的代码时确实获得了额外的灵活性, 并且可以支持示例 17-9 ,
但仍然需要权衡取舍.

## Trait 对象要求对象安全

只有 `对象安全`(object safe)的 `trait` 才可以组成 `trait 对象`.
存在一些复杂的规则控制对象的属性, 来保证 `trait 对象` 是安全的, 不过在实践中, 只涉及到两条规则.
如果 trait 中所有的方法都具有如下属性时, 则该 trait 是`对象安全`的:

+ 返回值类型不为 `Self`
+ 方法没有任何泛型类型参数

`Self` 关键字是某个`type`的别名, 我们要在这个`type`上实现 `trait`s 或 `方法`.

`trait 对象` 必须是 `对象安全` 的, 因为你一旦使用了 `trait 对象`, Rust 就不再知道实现该`trait`的具体类型了.
如果我们令 `trait` 方法返回具体的 `Self` 类型, 然而 `trait 对象` 并不记录 `Self` 的具体类型,
那么这个方法就无法使用原来的具体类型.

对于`泛型type参数`来说也是同理, 当使用 `trait` 时, 会将具体的`type 参数` 代入 `T`:
此`具体 type A`变成了, 实现该 `trait` 的`type B`的一部分.
当使用 `trait 对象`时, 这个`具体 type A` 被抹去了, 所以没有办法知道用何种`type` 填入`泛型 type 参数 ` 的位置.

举一个标准库中的 `Clone` trait 的例子, 它的方法不是 `对象安全` 的:
`Clone` trait 的 `clone` 方法的`参数签名`看起来像这样:

```rust
pub trait Clone {
    fn clone(&self) -> Self;
}
```

`String` 实现了 `Clone` trait, 当在 `String` 实例上调用 `clone` 方法时会得到一个 `String` 实例.
类似的, 当调用 `Vec<T>` 实例的 `clone` 方法会得到一个 `Vec<T>` 实例.
`clone` 的签名需要知道什么类型会代替 `Self`, 因为这是它的`返回值`.

如果尝试做一些违反有关 `trait` 对象的`对象安全`规则的事情, 编译器会提示你.
例如, 如果尝试实现示例 17-4 中的 `Screen` 结构体, 来存放实现了 `Clone` trait 而不是 `Draw` trait 的类型, 像这样:

```rust
pub struct Screen {
    pub components: Vec<Box<dyn Clone>>,
}
```

将会得到如下错误:

```log
error[E0038]: the trait `std::clone::Clone` cannot be made into an object
 --> src/lib.rs:2:5
  |
2 |     pub components: Vec<Box<dyn Clone>>,
  |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ the trait `std::clone::Clone`
  cannot be made into an object
  |
  = note: the trait cannot require that `Self : Sized`
```

这意味着不能以这种方式使用此 `trait` 作为 `trait对象`.
如果你对 `对象安全` 的更多细节感兴趣, 请查看 [Rust RFC 255](https://github.com/rust-lang/rfcs/blob/master/text/0255-object-safety.md).
