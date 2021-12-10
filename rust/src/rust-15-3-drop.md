# 使用 Drop Trait 运行清理代码

对于 `智能指针` 模式来说第二个重要的 `trait` 是 `Drop`,
它允许我们在 `值` 要离开`作用域`时执行一些代码.

可以为任何类型实现 `Drop` trait , 所指定的代码被用于释放类似于 `文件` 或 `网络连接` 的资源.
我们在 `智能指针` 上下文中讨论 `Drop` 是因为, 其功能几乎总是用于实现 `智能指针`.
例如, `Box<T>` 自定义了 `Drop` 用来释放 `box` 所指向的`堆空间`.

在其他一些语言中, 我们不得不记住在每次使用完 `智能指针实例后`, 调用清理内存或资源的代码.
如果忘记的话, 运行代码的系统可能会因为负荷过重而崩溃.
在 Rust 中, 可以指定每当值离开 `作用域` 时被执行的代码, 编译器会自动插入这些代码.
于是我们就不需要在程序中到处编写样板代码, 以便在实例结束时清理这些变量 -- 而且还不会泄漏资源.

指定在值离开`作用域`时应该执行的代码的方式是实现 `Drop` trait.
`Drop` trait 要求实现一个叫做 `drop` 的方法, 它获取 `self` 的 `可变引用`.
为了能够看出 `Rust` 何时调用 `drop`, 让我们暂时使用 `println!` 语句实现 `drop`.

示例 15-14 展示的唯一定制功能就是,
当结构体 `CustomSmartPointer` 的实例离开作用域时,打印出 `Dropping CustomSmartPointer!`.
这会演示 `Rust` 何时运行 `drop` 函数:

文件名: src/main.rs

```rust
struct CustomSmartPointer {
    data: String,
}

impl Drop for CustomSmartPointer {
    fn drop(&mut self) { // self 表示 CustomSmartPointer
        println!("Dropping CustomSmartPointer with data `{}`!", self.data); // println! 在打印末尾附加新行
    }
}

fn main() {
    let c = CustomSmartPointer { data: String::from("my stuff") };
    let d = CustomSmartPointer { data: String::from("other stuff") };
    println!("CustomSmartPointers created.");
}
```

示例 15-14: 结构体 `CustomSmartPointer`, 其实现了放置清理代码的 `Drop` trait

`Drop` trait 包含在 `prelude` 中, 所以无需导入它.
我们在 `CustomSmartPointer` 上实现了 `Drop` trait, 并实现了调用 `println!` 的 `drop` 方法.
`drop` 函数体放置善后逻辑, 即当 `类型实例` 离开作用域时, 期望执行的代码.
这里选择打印一些文本, 以展示 `Rust` 何时调用 `drop`.

在 `main` 中, 我们新建了两个 `CustomSmartPointer` 实例, 并在 main 的结尾打印了 `CustomSmartPointer created..` ,
`CustomSmartPointer` 的实例会离开 `作用域`, 而 `Rust` 会调用放置于 `drop` 方法中的代码, 打印出最后的信息.
注意无需显式调用 `drop` 方法:

当运行这个程序, 会出现如下输出:

```log
CustomSmartPointers created.
Dropping CustomSmartPointer with data `other stuff`!
Dropping CustomSmartPointer with data `my stuff`!
```

当 `实例` 离开 `作用域`, Rust 会自动调用 `drop`, 并调用我们指定的代码.
`变量` 按照创建时相反的顺序被丢弃, 所以 `d` 在 `c` 之前被丢弃.
这个例子以可视化的方式展现了 `drop` 方法的工作方式,
不过通常需要给定 `类型` 需要执行的`清理代码`而不是打印信息.

## 通过 `std::mem::drop` 提早丢弃值

不幸的是, 我们并不能直截了当的禁用 `drop` 这个功能.
通常也不需要禁用 `drop` ; 整个 `Drop` trait 存在的意义在于其是自动处理的.

然而, 有时你可能需要 `提早清理` 某个值. 一个例子是当使用 `智能指针` 管理锁时;
你可能希望强制运行 `drop` 方法来释放 `锁`, 以便 `作用域` 中的其他代码可以获取 `锁`.
`Rust` 并不允许我们主动调用 `Drop` trait 的 `drop` 方法;
当我们希望在作用域结束之前就 `强制释放变量` 的话, 我们应该使用 `标准库` 提供的 `std::mem::drop`.

如果我们像是示例 15-14 那样尝试调用 `Drop` trait 的 drop 方法, 就会得到像示例 15-15 那样的编译错误:

文件名: src/main.rs

```rust
[这些代码不能编译! ]
fn main() {
    let c = CustomSmartPointer { data: String::from("some data") };
    println!("CustomSmartPointer created.");
    c.drop();
    println!("CustomSmartPointer dropped before the end of main.");
}
```

示例 15-15: 尝试手动调用 `Drop` trait 的 `drop` 方法提早清理

如果尝试编译代码会得到如下错误:

```log
error[E0040]: explicit use of destructor method
  --> src/main.rs:14:7
   |
14 |     c.drop();
   |       ^^^^ explicit destructor calls not allowed
```

错误信息表明 `不允许显式调用 drop`.
错误信息使用了术语 `析构函数`(destructor), 这是个通用编程概念, 代表 `清理` 类型实例的函数.
与 `析构函数` 对应的是创建实例的 `构造函数`.
Rust 中的 `drop` 函数就是这样一种 `析构函数`.

`Rust` 不允许我们显式调用 `drop`, 因为 Rust 仍然会在 `main` 的结尾对`值`自动调用 `drop`,
如此会导致 `double free` 错误, 因为 `Rust` 会尝试清理相同的 `值` 两次.

因为无法禁用,  当值离开作用域时自动插入的 `drop`, 也`不能显式调用 drop`,
所以若我们需要 `强制提早清理` 值, 可以使用 `std::mem::drop` 函数.

`std::mem::drop` 函数不同于 `Drop` trait 中的 `drop` 方法.
可以传递期望提早强制丢弃的 `值` 作为 `参数`.
`std::mem::drop` 位于 `prelude`, 所以我们可以修改示例 15-15 中的 `main` 来调用 `drop` 函数.
如示例 15-16 所示:

文件名: src/main.rs

```rust
fn main() {
    let c = CustomSmartPointer { data: String::from("some data") };
    println!("CustomSmartPointer created.");
    drop(c);
    println!("CustomSmartPointer dropped before the end of main.");
}
```

示例 15-16: 在值离开作用域之前调用 `std::mem::drop` 显式清理

运行这段代码会打印出如下:

```log
CustomSmartPointer created.
Dropping CustomSmartPointer with data `some data`!
CustomSmartPointer dropped before the end of main.
```

`` Dropping CustomSmartPointer with data `some data`! `` 出现在 `CustomSmartPointer created.`
和 `CustomSmartPointer dropped before the end of main.` 之间,
表明了 `drop` 方法被调用, 并在此丢弃了 `c`.

`Drop` trait 实现中指定的代码可以用于许多方面, 来使得清理变得方便和安全:
比如可以用它创建我们自己的 `内存分配器`!
通过 `Drop` trait 和 Rust 所有权系统, 你无需担心之后的 `代码清理`, Rust 会自动考虑这些问题.

我们也无需担心意外清理掉仍在使用的值, 这会造成 `编译器` 错误:
`所有权`系统确保 `引用` 总是有效的, 也会确保 `drop` 只在值不再被使用时精确调用一次.

现在我们学习了 `Box<T>` 和一些 `智能指针` 的特性, 让我们聊聊标准库中定义的其他几种智能指针.
