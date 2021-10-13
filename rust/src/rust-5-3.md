# 方法语法

`方法` 与 `函数` 类似: 
它们使用 `fn` 关键字和 `名称` 声明, 可以拥有 `参数` 和 `返回值`, 同时包含在某处调用该方法时会执行的代码. 
不过`方法`与 `函数` 是不同的, 因为它们在 `结构体` 的上下文中被定义(或者是 `枚举` 或 `trait` 对象的上下文, 将分别在第六章和第十七章讲解), 
并且它们第一个参数总是 `self`, 它代表调用该`方法`的 `结构体实例`.

## 定义方法

让我们把前面实现的, 获取一个 `Rectangle` 实例作为参数的 `area` 函数, 
改写成一个定义于 `Rectangle` 结构体上的 `area` 方法, 如示例 5-13 所示:

```rust
#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

fn main() {
    let rect1 = Rectangle { width: 30, height: 50 };

    println!(
        "The area of the rectangle is {} square pixels.",
        rect1.area()
    );
}
```

示例 5-13: 在 `Rectangle` 结构体上定义 `area` 方法

为了使函数定义于 `Rectangle` 的上下文中, 我们开始了一个 `impl` 块(`impl` 是 `implementation` 的缩写). 
接着将 `area` 函数移动到 `impl` 大括号中, 并将`签名`中的第一个(在这里也是唯一一个)参数和`函数体`中其他地方的对应`参数`改成 `self`. 
然后在 `main` 中将我们先前调用 `area` 方法, 并传递 `rect1` 作为参数的地方, 改成使用 `方法语法`(method syntax), 在 `Rectangle` 实例上调用 `area` 方法.
`方法语法`获取一个`实例`并加上一个`点号`, 后跟`方法名`, `圆括号`以及`任何参数`.

在 `area` 的签名中, 使用 `&self` 来替代 `rectangle: &Rectangle`, 
因为该方法位于 `impl Rectangle` 上下文中, 所以 `Rust` 知道 `self` 的类型是 `Rectangle`. 
注意仍然需要在 `self` 前面加上 `&`, 就像 `&Rectangle` 一样. 
`方法`可以选择获取 `self` 的`所有权`, 或者像我们这里一样不可变地借用 `self`, 或者可变地借用 `self`, 就跟其他`参数`一样.

这里选择 `&self` 的理由跟在函数版本中使用 `&Rectangle` 是相同的: 我们并不想获取`所有权`, 只希望能够读取结构体中的数据, 而不是写入. 
如果想要在`方法`中, 改变这个调用方法的`实例`, 需要将第一个参数改为 `&mut self`. 
通过仅使用 `self` 作为第一个参数, 来使方法获取 `实例` 的`所有权`是很少见的; 
这种技术通常用在, 当方法将 `self` 转换成别的`实例`的时候, 这时我们想要防止 `调用者` 在转换之后使用**原始的**实例.

使用`方法`替代`函数`, 除了可使用`方法语法`和不需要在每个`函数签名`中重复 `self` 的类型之外, 其主要好处在于`组织性`. 
我们将某个`类型实例`能做的所有事情都一起放入 `impl` 块中, 而不是让将来的用户在我们的`库`中到处寻找 `Rectangle` 的功能.

>`->` 运算符到哪去了?
>
>在 `C/C++` 语言中, 有两个不同的`运算符`来调用方法: `.` 直接在`对象`上调用方法, 而 `->` 在一个对象的`指针`上调用方法, 这时需要先`解引用`(dereference)指针. 
>换句话说, 如果 `object` 是一个`指针`, 那么 `object->something()` 就像 `(*object).something()` 一样.
>
>`Rust` 并没有一个与 `->` 等效的运算符; 相反, `Rust` 有一个叫 `自动引用和解引用`(automatic referencing and dereferencing)的功能. 
>`方法调用`是 `Rust` 中少数几个拥有这种行为的地方.
>
>他是这样工作的: 当使用 `object.something()` 调用方法时, `Rust` 会自动为 `object` 添加 `&`, `&mut` 或 `*` 以便使 `object` 与`方法签名`匹配. 
>也就是说, 这些代码是等价的:
>
>```rust
>p1.distance(&p2);
>(&p1).distance(&p2);
>```
>
>第一行看起来简洁的多. 这种`自动引用`的行为之所以有效, 是因为方法有一个明确的接收者  --  `self` 类型. 
>在给出`接收者`和`方法名`的前提下, `Rust` 可以明确地计算出方法是仅仅`读取`(`&self`), `做出修改`(`&mut self`)或者是`获取所有权`(`self`). 
>事实上, `Rust` 对`方法接收者`的`隐式借用`让所有权在实践中更友好.

## 带有更多参数的方法

让我们通过实现 `Rectangle` 结构体上的另一方法来练习使用方法. 
这回, 我们让一个 `Rectangle` 的实例获取另一个 `Rectangle` 实例, 如果 `self` 能完全包含第二个长方形则返回 `true`; 否则返回 `false`. 
一旦定义了 `can_hold` 方法, 就可以编写示例 5-14 中的代码.

```rust
fn main() {
    let rect1 = Rectangle { width: 30, height: 50 };
    let rect2 = Rectangle { width: 10, height: 40 };
    let rect3 = Rectangle { width: 60, height: 45 };

    println!("Can rect1 hold rect2? {}", rect1.can_hold(&rect2));
    println!("Can rect1 hold rect3? {}", rect1.can_hold(&rect3));
}
```

示例 5-14: 使用还未实现的 `can_hold` 方法

同时我们希望看到如下输出, 因为 `rect2` 的两个维度都小于 `rect1`, 而 `rect3` 比 `rect1` 要宽:

```log
Can rect1 hold rect2? true
Can rect1 hold rect3? false
```

因为我们想定义一个方法, 所以它应该位于 `impl Rectangle` 块中. 
方法名是 `can_hold`, 并且它会获取另一个 `Rectangle` 的`不可变借用`作为参数. 
通过观察调用方法的代码可以看出参数是什么类型的: `rect1.can_hold(&rect2)` 传入了 `&rect2`, 它是一个 `Rectangle` 的实例 `rect2` 的`不可变借用`. 
这是可以理解的, 因为我们只需要读取 `rect2`(而不是`写入`, 这意味着我们需要一个不可变借用), 
而且希望 `main` 保持 `rect2` 的`所有权`, 这样就可以在调用这个方法后继续使用它. 
`can_hold` 的返回值是一个`布尔值`, 其实现会分别检查 `self` 的宽高是否都大于另一个 `Rectangle`. 
让我们在示例 5-13 的 `impl` 块中增加这个新的 `can_hold` 方法, 如示例 5-15 所示:

```rust
impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }

    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}
```

示例 5-15: 在 `Rectangle` 上实现 `can_hold` 方法, 它获取另一个 `Rectangle` 实例作为参数

如果结合示例 5-14 的 `main` 函数来运行, 就会看到期望的输出. 
在方法签名中, 可以在 `self` 后增加多个参数, 而且这些参数就像函数中的参数一样工作.

## 关联函数

`impl` 块的另一个有用的功能是: 允许在 `impl` 块中定义 不 以 `self` 作为参数的函数. 
这被称为 `关联函数`(associated functions), 因为它们与`结构体`相关联. 
它们仍是`函数`而不是`方法`, 因为它们并不作用于一个`结构体`的`实例`. 你已经使用过 `String::from` 关联函数了.

`关联函数`经常被用作返回一个`结构体新实例`的`构造函数`. 
例如我们可以提供一个`关联函数`, 它接受一个维度参数并且同时作为`宽`和`高`, 这样可以更轻松的创建一个正方形 `Rectangle` 而不必指定两次同样的值:

```rust
impl Rectangle {
    fn square(size: u32) -> Rectangle {
        Rectangle { width: size, height: size }
    }
}
```

使用`结构体`名和 `:: `语法来调用这个`关联函数`: 比如 `let sq = Rectangle::square(3);`. 
这个方法位于`结构体`的`命名空间`中: `::` 语法用于`关联函数`和`模块`创建的命名空间. 第七章会讲到模块.

## 多个 impl 块

每个`结构体`都允许拥有多个 `impl` 块. 例如, 示例 5-16 中的代码等同于示例 5-15, 但每个方法有其自己的 `impl` 块.

```rust
impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }
}

impl Rectangle {
    fn can_hold(&self, other: &Rectangle) -> bool {
        self.width > other.width && self.height > other.height
    }
}
```

示例 5-16: 使用多个 `impl` 块重写示例 5-15

这里没有理由将这些方法分散在多个 `impl` 块中, 不过这是有效的语法. 
第十章讨论`泛型`和 `trait` 时会看到实用的多 `impl` 块的用例.

## 总结

`结构体`让你可以创建出在你的领域中有意义的自定义类型. 
通过`结构体`, 我们可以将相关联的`数据片段`联系起来并命名它们, 这样可以使得代码更加清晰. 
`方法`允许为结构体`实例`指定行为, 而`关联函数`将特定功能置于`结构体`的命名空间中并且无需一个实例.

但`结构体`并不是创建自定义类型的唯一方法: 让我们转向 `Rust` 的`枚举功能`, 为你的工具箱再添一个工具. 
