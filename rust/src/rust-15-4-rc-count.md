# `Rc<T>` 引用计数智能指针

大部分情况下 `所有权` 是非常明确的: 可以准确地知道哪个 `变量` 拥有某个 `值`.

然而, 有些情况 `单个值` 可能会有多个 `所有者`.
例如, 在 `图数据结构` 中, 多个 `边` 可能指向相同的 `节点`, 而这个 `节点` 从概念上讲为所有指向它的 `边` 所拥有.
`节点` 直到没有任何 `边` 指向它之前都不应该被清理.

为了启用 `多所有权`, `Rust` 有一个叫做 `Rc<T>` 的类型. 其名称为 `引用计数`(reference counting)的缩写.
`引用计数` 意味着记录 `值的引用` 的 `数量`, 来知晓这个值是否仍在被使用.
如果某个值 `引用数` 为零, 就代表它没有任何有效引用, 所以可以被清理.

可以将其想象为客厅中的电视. 当一个人进来看电视时, 他打开电视. 其他人也可以进来看电视.
当最后一个人离开房间时, 他关掉电视, 因为它不再被使用了.
如果某人在其他人还在看的时候就关掉了电视, 正在看电视的人肯定会抓狂的!

`Rc<T>` 用于: 当我们希望在`堆上` 分配一些`内存`, 供程序的多个部分读取,
而且无法在编译时确定, 哪部分使用它的程序是 `最后结束的`.
如果确实知道哪部分最后结束使用的话, 就可以令其成为数据的 `所有者`, 正常的 `所有权规则` 就可以在编译时生效.

注意 `Rc<T>` 只能用于 `单线程` 场景;
第十六章 `并发` 会涉及到如何在 `多线程` 程序中进行 `引用计数`.

## 使用 `Rc<T>` 共享数据

让我们回到示例 15-5 中使用 `Box<T>` 定义 `cons list` 的例子.
这一次, 我们希望让两个列表 `共享` 第三个列表的 `所有权` , 其概念看起来如图 15-3 所示:

![Two lists that share ownership of a third list](https://kaisery.github.io/trpl-zh-cn/img/trpl15-03.svg)

图 15-3: 两个列表, `b` 和 `c`, 共享第三个列表 `a` 的所有权

`列表 a` 包含 `5`, 之后是 `10`, 之后是另两个列表:
`b` 从 `3` 开始, 而 `c` 从 `4` 开始. `b` 和 `c` 会接上包含 `5` 和 `10` 的列表 `a`.
换句话说, 这两个列表会尝试 `共享` 第一个列表包含的 `5` 和 `10`.

尝试使用 `Box<T>` 定义的 `List` 实现并不能工作, 如示例 15-17 所示:

文件名: src/main.rs

```rust
[这些代码不能编译! ]
enum List {
    Cons(i32, Box<List>),
    Nil,
}

use crate::List::{Cons, Nil};

fn main() {
    let a = Cons(5,
                    Box::new(Cons(10,
                        Box::new(Nil))));
    let b = Cons(3, Box::new(a));
    let c = Cons(4, Box::new(a));
}
```

示例 15-17: 展示不能用两个 `Box<T>` 的列表, 尝试共享第三个列表的 `所有权`.
编译会得出如下错误:

```log
error[E0382]: use of moved value: `a`
  --> src/main.rs:13:30
   |
12 |     let b = Cons(3, Box::new(a));
   |                              - value moved here
13 |     let c = Cons(4, Box::new(a));
   |                              ^ value used here after move
   |
   = note: move occurs because `a` has type `List`, which does not implement the `Copy` trait
```

`Cons成员` 拥有其储存的数据, 所以当创建 `b` 列表时, `a` 被移动进了 `b`, 这样 `b` 就拥有了 `a`.
接着, 当再次尝试用 `a` 创建 `c` 时, 就不被允许了, 因为 `a` 的 `所有权` 已经被移动.

可以改变 `Cons` 的定义, 保存元素的 `引用`, 不过接着必须指定 `生命周期参数`.
通过指定 `生命周期参数`, 表明列表中的每个`元素`, 都至少与 `列表本身` 存在的一样久.
例如, `借用检查器` 不会允许 `let a = Cons(10, &Nil);` 编译, 因为 `临时值 Nil` 会在 `a` 获取其 `引用` 之前就被丢弃.

相反, 我们修改 `List` 的定义, 使用 `Rc<T>` 代替 `Box<T>`, 如列表 15-18 所示.
现在每个 `Cons` 变量都包含一个`值`, 和一个指向 `List` 的 `Rc<T>`.
当创建 `b` 时, 不同于获取 `a` 的 `所有权`, 这里会克隆 `a` 所包含的 `Rc<List>`,
这会将 `引用计数` 从 `1` 增加到 `2`, 并允许 `a` 和 `b` 共享 `Rc<List>` 中数据的 `所有权`.

创建 `c` 时也会克隆 `a`, 这会将 `引用计数` 从 `2` 增加为 `3`.
每次调用 `Rc::clone`, `Rc<List>` 中数据的引用计数都会增加, 直到`引用`到达`零`之前, 其数据都不会被清理.

文件名: src/main.rs

```rust
enum List {
    Cons(i32, Rc<List>),
    Nil,
}

use crate::List::{Cons, Nil};
use std::rc::Rc; // 将 Rc 引入作用域

fn main() {
    let a = Rc::new(Cons(5,
                    Rc::new(Cons(10,
                        Rc::new(Nil)))));
    let b = Cons(3, Rc::clone(&a));
    let c = Cons(4, Rc::clone(&a));
}
```

示例 15-18: 使用 `Rc<T>` 定义的 `List`

需要使用 `use` 语句将 `Rc<T>` 引入作用域, 因为它不在 `prelude` 中.
在 `main` 中创建了存放 `5` 和 `10` 的列表, 并将其存放在 `a` 的新的 `Rc<List>` 中.
接着当创建 `b` 和 `c` 时, 调用 `Rc::clone` 函数, 并传递 `a` 中 `Rc<List>` 的 `引用` 作为参数.

也可以调用 `a.clone()` 而不是 `Rc::clone(&a)`, 不过在这里 `Rust` 的习惯是使用 `Rc::clone`.
`Rc::clone` 的实现并不像大部分类型的 `clone` 实现那样, 对所有数据进行 `深拷贝`.
`Rc::clone` 只会增加 `引用计数`, 这并不会花费多少时间. `深拷贝` 可能会花费很长时间.
通过使用 `Rc::clone` 进行 `引用计数`, 可以明显的区别 `深拷贝类` 的克隆和 `增加引用计数类` 的克隆.
当查找代码中的 `性能问题` 时, 只需考虑 `深拷贝类` 的克隆而无需考虑 `Rc::clone` 调用.

## 克隆 `Rc<T>` 会增加引用计数

让我们修改示例 15-18 的代码, 以便观察 `创建` 和 `丢弃` a 中 `Rc<List>` 的 `引用` 时, `引用计数`的变化.

在示例 15-19 中, 修改了 `main`, 以便将列表 `c` 置于 `内部作用域` 中, 这样就可以观察当 `c` 离开`作用域`时, `引用计数` 如何变化.

文件名: src/main.rs

```rust
fn main() {
    let a = Rc::new(Cons(5,
                    Rc::new(Cons(10,
                        Rc::new(Nil)))));
    println!("count after creating a = {}", Rc::strong_count(&a));
    let b = Cons(3, Rc::clone(&a));
    println!("count after creating b = {}", Rc::strong_count(&a));
    { // 将列表 `c` 置于 `内部作用域` 中
        let c = Cons(4, Rc::clone(&a));
        println!("count after creating c = {}", Rc::strong_count(&a));
    }
    println!("count after c goes out of scope = {}", Rc::strong_count(&a));
}
```

示例 15-19: 打印出 `引用计数`

在程序中每个 `引用计数` 变化的点, 都会打印出 `引用计数`, 其 `值` 可以通过调用 `Rc::strong_count` 函数获得.
这个函数叫做 `strong_count` 而不是 `count` 是因为,  `Rc<T>` 也有 `weak_count`;
在 "避免引用循环: 将 `Rc<T>` 变为 `Weak<T>`" 部分会讲解 `weak_count` 的用途.

这段代码会打印出:

```log
count after creating a = 1
count after creating b = 2
count after creating c = 3
count after c goes out of scope = 2
```

我们能够看到 `a` 中 `Rc<List>` 的初始 `引用计数` 为1, 接着每次调用 `clone`, 计数会增加`1`.
当 `c` 离开作用域时, 计数减`1`.
不必像调用 `Rc::clone` 增加 `引用计数` 那样, 调用函数来 `减少计数`;
由于实现了 `Drop` trait, 当 `Rc<T>` 值离开 `作用域` 时, 自动减少 `引用计数`.

在例子中, 我们未看到的是,
在 `main` 的结尾, 当 `b` 然后是 `a` 离开 `作用域` 时, `计数` 会变成 `0`, 同时 `Rc<List>` 被完全清理.
使用 `Rc<T>` 允许`值`有多个 `所有者`, `引用计数` 则确保, 只要任何 `所有者` 依然存在其 `值` 也保持有效.

通过 `不可变引用`,  `Rc<T>` 允许在程序的多个部分之间只读地 `共享数据`.
如果 `Rc<T>` 也允许多个 `可变引用`, 则会违反第四章讨论的 `借用规则` 之一:
相同位置的 `多个 可变借用` 可能造成 `数据竞争` 和不一致.

不过可以修改数据是非常有用的!
在下一部分, 我们将讨论 `内部可变性模式` 和 `RefCell<T>` 类型,
它可以与 `Rc<T>` 结合使用来处理 `不可变性` 的限制.
