# 引用循环与内存泄漏

Rust 的`内存安全性`保证程序员难以意外地制造, 永远也不会被清理的内存(被称为 `内存泄漏`(memory leak)), 但并非不可能.

与在编译时拒绝`数据竞争`不同,  `Rust` 并不保证完全避免内存泄漏, 这意味着 `内存泄漏` 在 Rust 被认为是内存安全的.
通过使用 `Rc<T>` 和 `RefCell<T>` 可以看出:  有可能创建`引用循环`, 引用指向的对象相互引用.
这就造成了内存泄漏, 因为循环中每个项目的引用计数永远不会达到`0`, 而且这些值也不会被丢弃.

## 制造引用循环

让我们看看引用循环是如何发生的, 以及如何避免它. 以示例 15-25 中的 `List` 枚举和 `tail` 方法的定义开始:

文件名: src/main.rs

```rust
use std::rc::Rc;
use std::cell::RefCell;
use crate::List::{Cons, Nil};

#[derive(Debug)]
enum List {
    Cons(i32, RefCell<Rc<List>>),
    Nil,
}

impl List {
    fn tail(&self) -> Option<&RefCell<Rc<List>>> {
        match self {
            Cons(_, item) => Some(item),
            Nil => None,
        }
    }
}
```

示例 15-25: 一个存放 `RefCell` 的 `cons list` 定义, 这样可以修改 Cons 成员所引用的数据

这里采用了示例 15-25 中 List 定义的另一种变体.
现在 `Cons` 成员的第二个元素是 `RefCell<Rc<List>>`,
这意味着不同于示例 15-24 那样修改 `i32` 的值, 现在我们希望能够修改 `Cons` 成员所指向的 `List`.
这里还增加了一个 `tail` 方法, 来方便我们访问Cons 成员的第二项, 如果存在的话.

在示例 15-26 中增加了一个 `main` 函数, 其中使用了示例 15-25 中的定义.
这些代码在 `a` 中创建了一个列表, 一个指向 `a` 中列表的 `b` 列表,
接着令 `a` 中的列表指向 `b` 中的列表, 这会创建一个引用循环.
在这个过程中的多个位置用 `println!` 语句展示引用计数.

Filename: src/main.rs

```rust
fn main() {
    let a = Rc::new(Cons(5, RefCell::new(Rc::new(Nil)))); //在 `a` 中创建了一个列表

    println!("a initial rc count = {}", Rc::strong_count(&a));
    println!("a next item = {:?}", a.tail());

    let b = Rc::new(Cons(10, RefCell::new(Rc::clone(&a)))); // `b` 列表, 指向 `a` 中列表

    println!("a rc count after b creation = {}", Rc::strong_count(&a));
    println!("b initial rc count = {}", Rc::strong_count(&b));
    println!("b next item = {:?}", b.tail());

    if let Some(link) = a.tail() {
        *link.borrow_mut() = Rc::clone(&b);
    }

    println!("b rc count after changing a = {}", Rc::strong_count(&b));
    println!("a rc count after changing a = {}", Rc::strong_count(&a));

    // Uncomment the next line to see that we have a cycle;
    // it will overflow the stack
    // println!("a next item = {:?}", a.tail());
}
```

示例 15-26: 创建一个引用循环: 两个 `List` 值互相指向彼此

这里在变量 `a` 中创建了一个 `Rc<List>` 实例: 存放初值为 `5, Nil `的 `List` 值.
接着在变量 `b` 中创建了另一个 `Rc<List>` 实例: 存放值 `10` , 以及指向列表 `a` 的指针.

最后, 修改 `a` 的尾元素, 使其指向 `b` 而不是 `Nil`, 这就创建了一个循环.
为此需要使用 `tail` 方法获取 `a` 中 `RefCell<Rc<List>>` 的引用, 并放入变量 `link` 中.
接着使用 `RefCell<Rc<List>>` 的 `borrow_mut` 方法, 将其值从存放 `Nil` 的 `Rc<List>` 修改为 `b` 中的 `Rc<List>`.

如果去掉最后的 `println!` 行的注释, 并运行代码, 会得到如下输出:

```log
a initial rc count = 1
a next item = Some(RefCell { value: Nil })
a rc count after b creation = 2
b initial rc count = 1
b next item = Some(RefCell { value: Cons(5, RefCell { value: Nil }) })
b rc count after changing a = 2
a rc count after changing a = 2
```

可以看到将列表 `a` 修改为指向 `b` 之后,  `a` 和 `b` 中的 `Rc<List>` 实例的引用计数都是 `2`.
在 `main` 的结尾, `Rust` 首先丢弃变量 `b`, 这会使 `b` 中 `Rc<List>` 实例的引用计数减 `1`.
然而, 因为 `a` 仍然引用 `b` 中的 `Rc<List>`, `Rc<List>` 的引用计数是 `1` 而不是 `0`,
所以 `b` 中的 `Rc<List>` 在堆上的内存不会被丢弃.

接下来 `Rust` 会丢弃 `a`, 这同理会将 `a` 中 `Rc<List>` 实例的引用计数从 `2` 减为 `1`.
这个实例的内存也不能被丢弃, 因为其他的 `Rc<List>` 实例仍在引用它.
这些列表的内存将永远保持未被回收的状态. 为了更形象的展示引用循环, 我们创建了一个图 15-4 :

![Reference cycle of lists](https://doc.rust-lang.org/book/img/trpl15-04.svg)

图 15-4: 列表 a 和 b 彼此互相指向形成引用循环

如果取消最后 `println!` 的注释并运行程序, `Rust` 会尝试打印出 `a` 指向 `b` 指向 `a` 这样的循环直到栈溢出.

这个特定的例子中, 创建了引用循环之后程序立刻就结束了. 这个循环的结果并不可怕.
如果在更为复杂的程序中并在循环里分配了很多内存并占有很长时间, 这个程序会使用多于它所需要的内存, 并有可能压垮系统并造成没有内存可供使用.

创建引用循环并不容易, 但也不是不可能.
如果你有包含 `Rc<T>` 的 `RefCell<T>` 值, 或类似的嵌套结合了`内部可变性`和`引用计数`的类型,
请务必小心确保你没有形成一个引用循环; 你无法指望 `Rust` 帮你捕获它们.

创建引用循环是一个程序上的逻辑 bug, 你应该使用 `自动化测试`, `代码评审` 和其他软件开发最佳实践来减少发生.

另一个解决方案是重新组织数据结构, 使得一部分引用拥有`所有权`, 而另一部分不拥有.
换句话说, 循环将由一些`拥有所有权`的关系, 和一些`无所有权`的关系组成, 只有 `所有权关系` 才能影响`值`是否被丢弃.

在示例 15-25 中, 我们总是希望 `Cons` 成员拥有其列表, 所以重新组织数据结构是不可能的.

让我们看看一个由`父节点`和`子节点`构成图的例子, 观察何时适合使用`无所有权`的关系来避免引用循环.

## 避免引用循环: 将 `Rc<T>` 变为 `Weak<T>`

到目前为止, 我们已经展示了调用 `Rc::clone` 会增加 `Rc<T>` 实例的 `strong_count`,
只有在其 `strong_count` 为 `0` 时, `Rc<T>` 实例才会被清理.
你也可以通过调用 `Rc::downgrade` 并传递 `Rc<T>` 实例的引用, 来创建其值的 `弱引用`(weak reference).

调用 `Rc::downgrade`, 将会得到 `Weak<T>` 类型的智能指针.
不同于将 `Rc<T>` 实例的 `strong_count` 加 `1`, 调用 `Rc::downgrade` 会将 `weak_count` 加 `1`.

`Rc<T>` 类型使用 `weak_count` 来记录其存在多少个 `Weak<T>` 引用, 类似于 `strong_count`.
其区别在于 `weak_count` 无需计数为 `0`, 就允许 `Rc<T>` 实例被清理.

`强引用`代表如何共享 `Rc<T>` 实例的`所有权`, 但`弱引用`并不表示`所有权关系`.
他们不会造成`引用循环`, 因为任何弱引用的循环会在其相关的 `强引用` 计数为` 0` 时被打断.

因为 `Weak<T>` 引用的值可能已被丢弃, 为了使用 `Weak<T>` 所指向的值, 我们必须确保其值仍然有效.
为此可以调用 `Weak<T>` 实例的 `upgrade` 方法, 这会返回 `Option<Rc<T>>`.
如果 `Rc<T>` 值还未被丢弃, 则结果是 `Some`; 如果 `Rc<T>` 已被丢弃, 则结果是 `None`.
因为 `upgrade` 返回一个 `Option<T>`,  Rust 会保证 `Some` 和 `None` case 被处理, 所以它不会返回`非法指针`.

作为例子, 我们会创建一个树形结构, 其中某项知道其子项和父项, 而非像列表那样, 只知道其下一项.

### 创建树形数据结构: 带有子节点的 Node

首先, 我们将建立一棵有`节点`的树, 这些节点知道它们的`子节点`.
我们将创建一个名为 `Node` 的结构体, 它持有自己的`i32`值, 以及对其子节点值的引用:

文件名: src/main.rs

```rust
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
struct Node {
    value: i32,
    children: RefCell<Vec<Rc<Node>>>,
}
```

我们希望能够 `Node` 拥有其子节点, 同时也希望将所有权共享给其他变量, 以便可以直接访问树中的每一个 `Node`,
为此 `Vec<T>` 的项的类型被定义为 `Rc<Node>`.
我们还希望能修改`节点`的`子节点`, 所以在 `children` 中, 我们把 `Vec<Rc<Node>>` 放进了 `RefCell<T>`.

接下来, 使用此`结构体`定义来创建一个叫做 `leaf` 的 `Node` 实例, 它带有值 `3` 且没有子节点,
我们还创建另一个带有值 `5`, 并以 `leaf` 作为子节点的 `branch` 实例, 如示例 15-27 所示:

文件名: src/main.rs

```rust
fn main() {
    let leaf = Rc::new(Node {
        value: 3,
        children: RefCell::new(vec![]),
    });

    let branch = Rc::new(Node {
        value: 5,
        children: RefCell::new(vec![Rc::clone(&leaf)]),
    });
}
```

示例 15-27: 创建没有子节点的 leaf 节点和以 leaf 作为子节点的 branch 节点

这里克隆了 `leaf` 变量中的 `Rc<Node>` 并储存在了 `branch` 中,
这意味着 `leaf` 中的 `Node` 现在有两个所有者: `leaf` 和 `branch`.
从 `branch` 中获得 `leaf`, 我们可以使用 `branch.children`; 不过我们无法从 `leaf` 到 `branch`.
`leaf` 没有到 `branch` 的引用, 所以并不知道他们相互关联.
我们希望 `leaf` 知道 `branch` 是其父节点. 接下来我们来实现.

### 增加从子到父的引用

为了使`子节点`知道其`父节点`, 需要在 `Node` 结构体定义中增加一个 `parent` 字段. 问题是 `parent` 的类型应该是什么.

我们知道其不能包含 `Rc<T>`, 因为这样会形成`引用循环`: `leaf.parent` 指向 `branch`, 而 `branch.children` 指向 `leaf` ,
会造成它们的 `strong_count` 永远也不等于 `0`.

现在换一种方式思考这个关系, 父节点应该拥有其子节点:
如果父节点被丢弃了, 其子节点也应该被丢弃.
然而子节点不应该拥有其父节点: 如果丢弃子节点, 其父节点应该依然存在. 这正是`弱引用`的例子!

所以 `parent` 字段应该使用 `Weak<T>`, 而不是 `Rc<T>` 类型, 具体来说是 `RefCell<Weak<Node>>`.
现在 `Node` 的结构体定义看起来像:

文件名: src/main.rs

```rust
use std::rc::{Rc, Weak};
use std::cell::RefCell;

#[derive(Debug)]
struct Node {
    value: i32,
    parent: RefCell<Weak<Node>>,
    children: RefCell<Vec<Rc<Node>>>,
}
```

这样, `节点`就能够引用其`父节点`, 但不`拥有`其父节点.
在示例 15-28 中, 我们更新 `main` 来使用新定义, 以便 `leaf` 节点可以通过 `branch` 引用其父节点:

文件名: src/main.rs

```rust
fn main() {
    let leaf = Rc::new(Node { // leaf node
        value: 3,
        parent: RefCell::new(Weak::new()), // 空的 `Weak` 引用实例.
        children: RefCell::new(vec![]),
    });

    println!("leaf parent = {:?}", leaf.parent.borrow().upgrade());

    let branch = Rc::new(Node { //branch node
        value: 5,
        parent: RefCell::new(Weak::new()),
        children: RefCell::new(vec![Rc::clone(&leaf)]),
    });

    *leaf.parent.borrow_mut() = Rc::downgrade(&branch); // 使 leaf 拥有 `Weak<Node>` 引用, 指向父节点.

    println!("leaf parent = {:?}", leaf.parent.borrow().upgrade());
}
```

示例 15-28: 一个 `leaf` 节点, 其拥有指向其父节点 `branch` 的 `Weak` 引用

创建 `leaf` 节点的过程类似于示例 15-27, 除了 `parent` 字段有所不同:
一开始, `leaf` 还没连接到`父节点`, 所以我们新建了一个空的 `Weak` 引用实例.

此时, 当尝试使用 `upgrade` 方法获取 `leaf` 的父节点引用时, 会得到一个 `None` 值.
如第一个 `println!` 输出所示:

```log
leaf parent = None
```

当创建 `branch` 节点时, 也新建了一个空 `Weak<Node>` 引用, 因为 `branch` 也没有父节点.

然后我们把 `leaf` 作为 `branch` 的一个子节点.
一旦在变量 `branch` 中建立了 `Node` 实例, 就可以修改 `leaf`, 使其拥有 `Weak<Node>` 引用, 指向父节点.

这里使用了 `leaf` 中, `parent` 字段中 `RefCell<Weak<Node>>` 的 `borrow_mut` 方法,
接着使用了 `Rc::downgrade` 函数, 创建了指向`branch` 的 `Weak<Node>` 引用, 通过 `branch`变量中的 `Rc<Node>`.

当再次打印 `leaf` 的父节点时, 这一次将会得到存放 `branch` 的 `Some` 值: 现在 `leaf` 可以访问其父节点了!
当打印出 `leaf` 时, 我们也避免了如示例 15-26 中的循环, 后者最终将导致栈溢出;
`Weak<Node>` 引用被打印为 `(Weak)`:

```log
leaf parent = Some(Node { value: 5, parent: RefCell { value: (Weak) },
children: RefCell { value: [Node { value: 3, parent: RefCell { value: (Weak) },
children: RefCell { value: [] } }] } })
```

没有不停的输出, 表明这段代码并没有造成引用循环.
这一点也可以从观察 `Rc::strong_count` 和 `Rc::weak_count` 调用的结果看出.

### 可视化 strong_count 和 weak_count 的改变

让我们通过创建了一个新的`内部作用域`, 并将创建 `branch` 的过程放入其中,
来观察 `Rc<Node>` 实例的 `strong_count` 和 `weak_count` 值的变化过程.
这会展示 `branch` 被创建和离开作用域被丢弃时, 发生的事情.
修改如示例 15-29 所示:

文件名: src/main.rs

```rust
fn main() {
    let leaf = Rc::new(Node {
        value: 3,
        parent: RefCell::new(Weak::new()), //父节点
        children: RefCell::new(vec![]), // 子节点
    });

    println!( // 打印 leaf 的强弱引用计数
        "leaf strong = {}, weak = {}",
        Rc::strong_count(&leaf),
        Rc::weak_count(&leaf),
    );

    {
        let branch = Rc::new(Node { // 创建 branch
            value: 5,
            parent: RefCell::new(Weak::new()),
            children: RefCell::new(vec![Rc::clone(&leaf)]),
        });

        *leaf.parent.borrow_mut() = Rc::downgrade(&branch); // 设置 lead 的父节点为 branch

        println!( // 打印 branch 的强弱引用计数
            "branch strong = {}, weak = {}",
            Rc::strong_count(&branch),
            Rc::weak_count(&branch),
        );

        println!( // 打印 leaf 的强弱引用计数
            "leaf strong = {}, weak = {}",
            Rc::strong_count(&leaf),
            Rc::weak_count(&leaf),
        );
    }

    println!("leaf parent = {:?}", leaf.parent.borrow().upgrade());
    println!(
        "leaf strong = {}, weak = {}",
        Rc::strong_count(&leaf),
        Rc::weak_count(&leaf),
    );
}
```

示例 15-29: 在内部作用域创建 `branch` 并检查其强弱引用计数

一旦创建了 `leaf`, 其 `Rc<Node>` 的强引用计数为 `1`, 弱引用计数为 `0`.

在内部作用域中创建了 `branch` 并与 `leaf` 相关联, 此时 `branch` 中 `Rc<Node>` 的强引用计数为 `1`, 弱引用计数为 `1`
(因为 `leaf.parent` 通过 `Weak<Node>` 指向 `branch`).
这里 `leaf` 的强引用计数为 `2`, 因为现在 `branch` 的 `branch.children` 中储存了 `leaf` 的 `Rc<Node>` 的拷贝, 不过弱引用计数仍然为 `0`.

当内部作用域结束时, `branch` 离开作用域, `Rc<Node>` 的强引用计数减少为 `0`, 所以其 `Node` 被丢弃.
来自 `leaf.parent` 的弱引用计数为 `1`, 但这和 `branch Node` 是否被丢弃无关, 所以并没有产生任何`内存泄漏`!

如果在内部作用域结束后尝试访问 `leaf` 的父节点, 会再次得到 `None`.
在程序的结尾, `leaf` 中 `Rc<Node>` 的强引用计数为 `1`, 弱引用计数为 `0`, 因为现在 `leaf` 又是 `Rc<Node>` 唯一的引用了.

所有这些`管理计数`和`value dropping` 的逻辑,  都内置于 `Rc<T>` 和 `Weak<T>`, 以及它们对 `Drop` 特性的实现中.
通过在 `Node` 的定义中, 指定从子节点到父节点的关系是 `Weak<T>` 引用,
就能够拥有父节点和子节点之间的双向引用, 而且不会造成`引用循环` 和 `内存泄漏` (reference cycle and memory leaks).

## 总结

本章介绍了如何使用智能指针,来实现与 `Rust` 默认的`普通引用`不同的保证与取舍(guarantees and trade-offs).

+ `Box<T>` 类型有已知的大小, 指向在堆上分配的数据.
+ `Rc<T>` 类型记录了对`堆上数据`的引用计数, 因此数据可以有多个所有者.
+ `RefCell<T>` 类型及其内部可变性, 使我们可以根据需要, 创建一个不可变的`type`, 但能够修改该`type`的内部值(inner value);
它在`运行时`, 而不是`编译时`检查借用规则.

我们还讨论了 `Deref` 和 `Drop` trait, 这些`trait` 实现了智能指针的很多功能.
我们探讨了可能导致`内存泄漏`的`引用循环`, 以及如何使用 `Weak<T>` 来避免它们.

如果本章引起了你的兴趣, 并且你想实现自己的智能指针, 请查看 "The Rustonomicon", 以获得更多有用的信息.

接下来, 让我们谈谈 `Rust 的并发`. 你甚至会了解到一些新的智能指针.
