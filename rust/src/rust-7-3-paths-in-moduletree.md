# 路径用于模块树中的项

来看一下 `Rust` 如何在`模块树`中找到一个项的位置, 我们使用`路径`的方式, 就像在`文件系统`使用路径一样.
如果我们想要调用一个函数, 我们需要知道它的路径.

路径有两种形式:

+ 绝对路径(absolute path) ; 从 `crate` 根开始, 以 `crate 名称` 或者 `字面的 crate` 开头.
+ 相对路径(relative path) ; 从当前`模块`开始, 以 `self`, `super` 或 当前模块的 `标识符` 开头.

`绝对路径`和 `相对路径` 都后跟一个或多个由 `双冒号`(`::`)分割的标识符.

让我们回到示例 7-1.
我们如何调用 `add_to_waitlist` 函数? 还是同样的问题, `add_to_waitlist` 函数的路径是什么?
在示例 7-3 中, 我们通过删除一些`模块`和`函数`, 稍微简化了一下我们的代码.
我们在 `crate root` 定义了一个新函数 `eat_at_restaurant`, 并在其中展示调用 `add_to_waitlist` 函数的两种方法.
`eat_at_restaurant` 函数是我们 `crate library` 的一个公共 `API`, 所以我们使用 `pub` 关键字来标记它.
在 `使用pub关键字暴露路径` 一节, 我们将详细介绍 `pub`.
注意, 这个例子无法编译通过, 我们稍后会解释原因.

```rust
[这些代码不能编译! ]
mod front_of_house {
    mod hosting {
        fn add_to_waitlist() {}
    }
}

pub fn eat_at_restaurant() {
    // 绝对路径
    crate::front_of_house::hosting::add_to_waitlist();

    // 相对路径
    front_of_house::hosting::add_to_waitlist();
}
```

示例 7-3: 使用绝对路径和相对路径来调用 `add_to_waitlist` 函数

第一种方式, 我们在 `eat_at_restaurant` 中调用 `add_to_waitlist` 函数, 使用的是`绝对路径`.
`add_to_waitlist` 函数与 `eat_at_restaurant` 被定义在同一 `crate` 中, 这意味着我们可以使用以 `crate` 关键字为起始的绝对路径.

在 `crate` 后面, 我们持续地嵌入`模块名称`, 直到我们找到 `add_to_waitlist`.
你可以想象出一个相同结构的文件系统, 我们通过指定路径 `/front_of_house/hosting/add_to_waitlist` 来执行 `add_to_waitlist` 程序.
使用 `字面值 crate `, 从 `crate` 根开始书写路径, 就类似于在 `shell` 中使用 `/` 从文件系统根开始.

第二种方式, 我们在 `eat_at_restaurant` 中调用 `add_to_waitlist`, 使用的是相对路径.
这个路径以 `front_of_house` 为起始, 这个`模块`在`模块树`中, 与 `eat_at_restaurant` 定义在`同一层级`.
与之等价的文件系统路径就是 `front_of_house/hosting/add_to_waitlist`. 以 `front_of_house` 这种名称开始, 意味着该路径是`相对路径`.

选择使用相对路径还是绝对路径, 还是要取决于你的项目.
取决于你是更倾向于将定义项的代码, 与使用该项的代码分开来移动, 还是一起移动.
举一个例子, 如果我们要将 `front_of_house` 模块和 `eat_at_restaurant` 函数一起移动到一个名为 `customer_experience` 的模块中,
我们需要更新 `add_to_waitlist` 的绝对路径, 但是 `相对路径` 还是可用的.
然而, 如果我们要将 `eat_at_restaurant` 函数单独移到一个名为 `dining` 的模块中, 还是可以使用原本的绝对路径来调用 `add_to_waitlist`, 但是`相对路径`必须要更新.
我们更倾向于使用 `绝对路径`, 因为把 `代码定义` 和 `项调用` 各自独立地移动是更常见的.

让我们试着编译一下示例 7-3, 并查明为何不能编译! 示例 7-4 展示了这个错误.

```log
$ cargo build
   Compiling restaurant v0.1.0 (file:///projects/restaurant)
error[E0603]: module `hosting` is private
 --> src/lib.rs:9:28
  |
9 |     crate::front_of_house::hosting::add_to_waitlist();
  |                            ^^^^^^^

error[E0603]: module `hosting` is private
...
```

示例 7-4: 构建示例 7-3 出现的编译器错误

错误信息说 `hosting` 模块是私有的.
换句话说, 我们拥有 `hosting` 模块和 `add_to_waitlist` 函数的的正确路径, 但是 `Rust` 不让我们使用, 因为它不能访问 `私有片段`.

`模块`不仅对于你组织代码很有用.
他们还定义了 `Rust` 的 `私有性边界`(privacy boundary): 这条界线不允许外部代码了解, 调用和依赖被封装的实现细节.
所以, 如果你希望创建一个`私有函数`或`结构体`, 你可以将其放入`模块`.

`Rust` 中默认`所有项`(函数, 方法, 结构体, 枚举, 模块和常量)都是`私有的`.
`父模块`中的项不能使用`子模块`中的私有项, 但是子模块中的项可以使用他们父模块中的项.
这是因为子模块封装并隐藏了他们的实现详情, 但是子模块可以看到他们被定义的`上下文`.
继续拿餐馆作比喻, 把`私有性规则`想象成餐馆的后台办公室:
餐馆内的事务对餐厅顾客来说是不可知的, 但办公室经理可以洞悉其经营的餐厅并在其中做任何工作.

`Rust` 选择以这种方式来实现模块系统功能, 因此`默认`隐藏内部实现细节.
这样一来, 你就知道可以更改哪些内部代码, 而不会破坏外部代码.
你还可以通过使用 `pub` 关键字来创建公共项, 使子模块的内部部分`暴露`给上级模块.

## 使用 pub 关键字暴露路径

让我们回头看一下示例 7-4 的错误, 它告诉我们 `hosting` 模块是私有的.
我们想让父模块中的 `eat_at_restaurant` 函数可以访问子模块中的 `add_to_waitlist` 函数,
因此我们使用 `pub` 关键字来标记 `hosting` 模块, 如示例 7-5 所示.

```rust
[这些代码不能编译! ]
mod front_of_house {
    pub mod hosting {
        fn add_to_waitlist() {}
    }
}

pub fn eat_at_restaurant() {
    // Absolute path
    crate::front_of_house::hosting::add_to_waitlist();

    // Relative path
    front_of_house::hosting::add_to_waitlist();
}
```

示例 7-5: 使用 `pub` 关键字声明 `hosting` 模块使其可在 `eat_at_restaurant` 使用

不幸的是, 示例 7-5 的代码编译仍然有错误, 如示例 7-6 所示.

```log
$ cargo build
   Compiling restaurant v0.1.0 (file:///projects/restaurant)
error[E0603]: function `add_to_waitlist` is private
 --> src/lib.rs:9:37
...
```

示例 7-6: 构建示例 7-5 出现的编译器错误

发生了什么? 在 `mod hosting` 前添加了 `pub` 关键字, 使其变成公有的.
伴随着这种变化, 如果我们可以访问 `front_of_house`, 那我们也可以访问 `hosting`.
但是 `hosting` 的 内容(contents) 仍然是`私有的`; 这表明使模块公有并不使其`内容`也是公有的.
模块上的 `pub` 关键字只允许其父模块引用它.

示例 7-6 中的错误说, `add_to_waitlist` 函数是私有的. 私有性规则不但应用于`模块`, 还应用于`结构体`, `枚举`, `函数` 和 `方法`.

让我们继续将 `pub` 关键字放置在 `add_to_waitlist` 函数的定义之前, 使其变成公有. 如示例 7-7 所示.

```rust
mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {}
    }
}

pub fn eat_at_restaurant() {
    // Absolute path
    crate::front_of_house::hosting::add_to_waitlist();

    // Relative path
    front_of_house::hosting::add_to_waitlist();
}
```

示例 7-7: 为 `mod hosting` 和 `fn add_to_waitlist` 添加 `pub` 关键字使他们可以在 `eat_at_restaurant` 函数中被调用

现在代码可以编译通过了! 让我们看看`绝对路径`和`相对路径`, 并根据私有性规则, 再检查一下为什么增加 `pub` 关键字使得我们可以在 `add_to_waitlist` 中调用这些路径.

在绝对路径, 我们从 `crate`, 也就是 `crate root`开始.
然后 `crate 根` 中定义了 `front_of_house` 模块. `front_of_house` 模块不是公有的, 不过因为 `eat_at_restaurant` 函数与 `front_of_house` 定义于同一模块中
(即, `eat_at_restaurant` 和 `front_of_house` 是兄弟), 我们可以从 `eat_at_restaurant` 中引用 `front_of_house`.
接下来是使用 `pub` 标记的 `hosting` 模块. 我们可以访问 `hosting` 的父模块, 所以可以访问 `hosting`.
最后, `add_to_waitlist` 函数被标记为 `pub` , 我们可以访问其`父模块`, 所以这个函数调用是有效的!

在`相对路径`的情形, 其逻辑与`绝对路径`相同, 除了第一步: 不同于从 `crate 根` 开始, 路径从 `front_of_house` 开始.
`front_of_house` 模块与 `eat_at_restaurant` 定义于同一模块, 所以从 `eat_at_restaurant` 中开始定义的该模块相对路径是有效的.
接下来因为 `hosting` 和 `add_to_waitlist` 被标记为 `pub`, 路径其余的部分也是有效的, 因此函数调用也是有效的!

## 使用 super 起始的相对路径

我们还可以使用 `super` 开头来构建从父模块开始的相对路径.
这么做类似于文件系统中以 `..` 开头的语法. 我们为什么要这样做呢?

考虑一下示例 7-8 中的代码, 它模拟了厨师更正了一个错误订单, 并亲自将其提供给客户的情况.
`fix_incorrect_order` 函数, 通过指定以 `super` 起始的 `serve_order` 路径, 来调用 `serve_order` 函数:

```rust
fn serve_order() {}

mod back_of_house {
    fn fix_incorrect_order() {
        cook_order();
        super::serve_order();
    }

    fn cook_order() {}
}
```

示例 7-8: 使用以 `super` 开头的相对路径从父目录开始调用函数

`fix_incorrect_order` 函数在 `back_of_house` 模块中, 所以我们可以使用 `super` 进入 `back_of_house` 父模块, 也就是本例中的 `crate root`.
在这里, 我们可以找到 `serve_order`. 成功! 我们认为 `back_of_house` 模块和 `serve_order` 函数之间可能具有某种关联,
并且当我们要重新组织这个 `crate` 的模块树, 需要一起移动它们.
因此我们使用 `super`, 这样一来, 如果这些代码被移动到了其他模块, 我们只需要进行很少的更新.

## 创建公有的结构体和枚举

我们还可以使用 `pub` 来设计公有的 `结构体` 和 `枚举`, 不过有一些额外的细节需要注意.
如果我们在一个结构体定义的前面使用了 `pub` , 这个`结构体`会变成公有的, 但是这个结构体的`字段`仍然是`私有的`. 我们可以根据情况决定每个`字段`是否公有.

在示例 7-9 中, 我们定义了一个公有结构体 `back_of_house:Breakfast`, 其中有一个公有字段 `toast` 和私有字段 `seasonal_fruit`.
这个例子模拟的情况是, 在一家餐馆中, 顾客可以选择随餐附赠的面包类型, 但是厨师会根据季节和库存情况来决定随餐搭配的水果.
餐馆可用的水果变化是很快的, 所以顾客不能选择水果, 甚至无法看到他们将会得到什么水果.

```rust
mod back_of_house {
    pub struct Breakfast {
        pub toast: String,
        seasonal_fruit: String,
    }

    impl Breakfast {
        pub fn summer(toast: &str) -> Breakfast {
            Breakfast {
                toast: String::from(toast),
                seasonal_fruit: String::from("peaches"),
            }
        }
    }
}

pub fn eat_at_restaurant() {
    // Order a breakfast in the summer with Rye toast
    let mut meal = back_of_house::Breakfast::summer("Rye");
    // Change our mind about what bread we'd like
    meal.toast = String::from("Wheat");
    println!("I'd like {} toast please", meal.toast);

    // The next line won't compile if we uncomment it; we're not allowed
    // to see or modify the seasonal fruit that comes with the meal
    // meal.seasonal_fruit = String::from("blueberries");
}
```

示例 7-9: 带有公有和私有字段的结构体

因为 `back_of_house::Breakfast` 结构体的 `toast` 字段是公有的, 所以我们可以在 `eat_at_restaurant` 中使用 `点号` 来随意的读写 `toast` 字段.
注意, 我们不能在 `eat_at_restaurant` 中使用 `seasonal_fruit` 字段, 因为 `seasonal_fruit` 是私有的.
尝试去掉 修改 `seasonal_fruit` 字段值的那行代码的注释, 看看是否会报错.

还请注意一点, 因为 `back_of_house::Breakfast` 具有 `私有字段`,
所以这个结构体需要提供一个 `公共的关联函数` 来构造 `Breakfast` 的实例(这里我们命名为 `summer`).
如果 `Breakfast` 没有这样的函数, 我们将无法在 `eat_at_restaurant` 中创建 `Breakfast` 实例,
因为我们不能在 `eat_at_restaurant` 中设置私有字段 `seasonal_fruit` 的值.

与之相反, 如果我们将`枚举`设为公有, 则它的所有成员`都`将变为公有.
我们只需要在 `enum` 关键字前面加上 `pub`, 就像示例 7-10 展示的那样.

```rust
mod back_of_house {
    pub enum Appetizer { //公有枚举
        Soup,
        Salad,
    }
}

pub fn eat_at_restaurant() {
    let order1 = back_of_house::Appetizer::Soup;
    let order2 = back_of_house::Appetizer::Salad;
}
```

示例 7-10: 设计`公有枚举`, 使其所有成员公有

因为我们创建了名为 `Appetizer` 的公有枚举, 所以我们可以在 `eat_at_restaurant` 中使用 `Soup` 和 `Salad` 成员.
如果枚举成员不是公有的, 那么枚举会显得用处不大; 给枚举的所有成员挨个添加 `pub` 是很令人恼火的, 因此枚举成员`默认`就是公有的.

`结构体`通常使用时, 不必将它们的字段公有化, 因此`结构体`遵循常规, 内容全部是私有的, 除非使用 `pub` 关键字.

还有一种使用 `pub` 的场景我们还没有涉及到, 那就是我们最后要讲的模块功能: `use` 关键字.
我们将先单独介绍 `use`, 然后展示如何结合使用 `pub` 和 `use`.
