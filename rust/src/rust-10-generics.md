# 泛型,trait 和生命周期

每一个编程语言都有高效处理重复概念的工具. 在 `Rust` 中其工具之一就是 `泛型`(generics).
`泛型`是具体`类型`(types)或其他属性的`抽象替代`.
我们可以表达`泛型`的属性, 比如他们的`行为`或如何与其他泛型`相关联`, 而不需要在编写和编译代码时知道他们在这里实际上代表什么.

同理为了编写一份可以用于`多种具体值`的代码, 函数并不知道其参数为何值, 这时就可以让函数获取`泛`型而不是像 `i32` 或 `String` 这样的具体类型.
我们已经使用过第六章的 `Option<T>`, 第八章的 `Vec<T>` 和 `HashMap<K, V>`, 以及第九章的 `Result<T, E>` 这些泛型了.
本章会探索, 如何使用`泛型`定义我们自己的类型, 函数和方法!

首先, 我们将回顾一下提取函数以减少代码重复的机制.
接下来, 我们将使用相同的技术, 从两个仅`参数类型`不同的函数中创建一个泛型函数.
我们也会讲到`结构体`和`枚举`定义中的泛型.

之后, 我们讨论 `trait`, 这是一个定义`泛型行为`的方法.
`trait` 可以与`泛型`结合来将泛型限制为`拥有特定行为`的类型, 而不是任意类型.

最后介绍 `生命周期`(lifetimes), 它是一类泛型, 允许我们告知编译器, `引用`是如何相互关联的.
`Rust` 的生命周期功能在很多场景下, 允许我们在借用值的同时, 仍然使编译器能够检查这些`引用`的`有效性`.

## 提取函数来减少重复

在介绍泛型语法之前, 首先来回顾一个不使用`泛型`的, 处理`重复`的技术: 提取一个函数.
当熟悉了这个技术以后, 我们将使用相同的机制来提取一个`泛型函数`!
如同你识别出一些重复代码, 可以把它们提取到函数中那样, 你也会开始识别出能够用`泛型`简化的重复代码.

考虑下面的程序,寻找列表中的最大值, 如示例 10-1 所示:

```rust
fn main() {
    let number_list = vec![34, 50, 25, 100, 65];

    let mut largest = number_list[0];

    for number in number_list {
        if number > largest {
            largest = number;
        }
    }

    println!("The largest number is {}", largest);
}
```

示例 10-1: 在数字列表中寻找最大值的函数

这段代码获取一个`整型列表`, 存放在变量 `number_list` 中.
它将列表的第一项放入变量 `largest` 中. 接着遍历列表中的所有数字, 如果当前值大于 `largest` 中储存的值, 将 `largest` 替换为这个值.
如果当前值小于, 或者等于目前为止的最大值, `largest` 保持不变.
当列表中所有值都被考虑之后, `largest` 将会是最大值, 在此处也就是 `100`.

如果需要在两个不同的列表中寻找最大值, 我们可以重复示例 10-1 中的代码,
这样程序中就会存在两段相同逻辑的代码, 如示例 10-2 所示:

```rust
fn main() {
    let number_list = vec![34, 50, 25, 100, 65];

    let mut largest = number_list[0];

    for number in number_list {
        if number > largest {
            largest = number;
        }
    }

    println!("The largest number is {}", largest);

    let number_list = vec![102, 34, 6000, 89, 54, 2, 43, 8];

    let mut largest = number_list[0];

    for number in number_list {
        if number > largest {
            largest = number;
        }
    }

    println!("The largest number is {}", largest);
}
```

示例 10-2: 寻找 两个 数字列表最大值的代码

虽然代码能够执行, 但是重复的代码是冗余且容易出错的, 并且意味着当更新逻辑时需要修改多处地方的代码.

为了消除重复, 我们可以创建一层抽象, 在这个例子中将表现为一个函数, 它获取任意整型列表作为参数并对其进行处理.
这将增加代码的简洁性, 并能使我们将表达和推导 `寻找列表最大值` 的位置, 与使用此概念的位置相互独立.

在示例 10-3 的程序中, 将寻找最大值的代码提取到了一个叫做 `largest` 的函数中.
这不同于示例 10-1 中的代码只能在一个特定的列表中找到最大的数字, 这个程序可以在两个不同的列表中找到最大值.

```rust
fn largest(list: &[i32]) -> i32 {
    let mut largest = list[0]; // 这里 list 本身就是 ref

    for &item in list {
        if item > largest {
            largest = item;
        }
    }

    largest
}

fn main() {
    let number_list = vec![34, 50, 25, 100, 65];

    let result = largest(&number_list);
    println!("The largest number is {}", result);

    let number_list = vec![102, 34, 6000, 89, 54, 2, 43, 8];

    let result = largest(&number_list);
    println!("The largest number is {}", result);
}
```

示例 10-3: 抽象后的寻找两个数字列表最大值的代码

`largest` 函数有一个参数 `list`, 它代表会传递给函数的任何具体的 `i32` 值的 `slice`.
函数定义中的 `list` 代表任何 `&[i32]`. 当调用 `largest` 函数时, 其代码实际上运行于我们传递的特定值上.

总的来说, 从示例 10-2 到示例 10-3 中涉及的机制经历了如下几步:

+ 找出重复代码.
+ 将重复代码提取到了一个函数中, 并在`函数签名`中指定了代码中的输入和返回值.
+ 将重复代码的两个实例, 改为调用函数.

在不同的场景使用不同的方式, 我们也可以利用相同的步骤和`泛型`来减少重复代码.
与函数体可以在抽象 `list` 而不是特定值上操作的方式相同, `泛型`允许代码对 `抽象类型` 进行操作.

如果我们有两个函数, 一个寻找 `i32` 值的 `slice` 中的最大项, 而另一个寻找 `char` 值的 `slice` 中的最大项该怎么办?
该如何消除重复呢? 让我们拭目以待!
