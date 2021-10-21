# 泛型数据类型

我们可以使用 `泛型` 为像 `函数签名` 或 `结构体` 这样的项创建定义, 这样它们就可以用于多种不同的具体`数据类型`.
让我们看看如何使用 `泛型` 定义 `函数`, `结构体`, `枚举` 和 `方法`, 然后我们将讨论泛型如何影响代码性能.

## 在函数定义中使用泛型

当使用`泛型`定义函数时, 本来在`函数签名`中指定参数和返回值的类型的地方, 会改用`泛型`来表示.
采用这种技术, 使得代码适应性更强, 从而为函数的调用者提供更多的功能, 同时也避免了代码的重复.

回到 `largest` 函数, 示例 10-4 中展示了两个函数, 它们的功能都是寻找 `slice` 中最大值.

```rust
fn largest_i32(list: &[i32]) -> i32 {
    let mut largest = list[0];

    for &item in list.iter() {
        if item > largest {
            largest = item;
        }
    }

    largest
}

fn largest_char(list: &[char]) -> char {
    let mut largest = list[0];

    for &item in list.iter() {
        if item > largest {
            largest = item;
        }
    }

    largest
}

fn main() {
    let number_list = vec![34, 50, 25, 100, 65];

    let result = largest_i32(&number_list);
    println!("The largest number is {}", result);

    let char_list = vec!['y', 'm', 'a', 'q'];

    let result = largest_char(&char_list);
    println!("The largest char is {}", result);
}
```

示例 10-4: 两个函数, 不同点只是`名称`和`签名类型`

`largest_i32` 函数是从示例 10-3 中摘出来的, 它用来寻找 `slice` 中最大的 `i32`.
`largest_char` 函数寻找 `slice` 中最大的 `char`.
因为两者函数体的代码是一样的, 我们可以定义一个函数, 再引进`泛型`参数来消除这种重复.

为了参数化新函数中的这些`类型`, 我们也需要为表示`类型`的参数取个名字, 道理和给函数的形参起名一样.
任何`标识符` 都可以作为类型参数的名字. 这里选用 `T`, 因为传统上来说, `Rust` 的参数名字都比较短, 通常就只有一个字母,
同时, `Rust` 类型名的命名规范是 `骆驼命名法`(CamelCase). `T` 作为 `type` 的缩写是大部分 `Rust` 程序员的首选.

如果要在`函数体`中使用参数, 就必须在`函数签名`中声明它的名字, 好让编译器知道这个名字指代的是什么.
同理, 当在`函数签名`中使用一个`类型参数`时, 必须在使用它之前就`声明它`.
为了定义`泛型`版本的 `largest` 函数, `类型参数`声明位于`函数名称`与`参数列表`中间的尖括号 `<>` 中, 像这样:

```rust
fn largest<T>(list: &[T]) -> T {
```

可以这样理解这个定义: 函数 `largest` 有泛型类型 `T`.
它有个参数 `list`, `list`是: `T`类型的元素组成的 `slice`.
`largest` 函数的返回值类型也是 T.

示例 10-5 中的 `largest` 函数在它的签名中使用了泛型, 统一了两个实现.
该示例也展示了如何调用 `largest` 函数, 把 `i32` 值的 `slice` 或 `char` 值的 `slice` 传给它.
请注意这些代码还不能编译, 不过稍后在本章会解决这个问题.

```rust
[这些代码不能编译! ]
fn largest<T>(list: &[T]) -> T {
    let mut largest = list[0];

    for &item in list.iter() {
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

    let char_list = vec!['y', 'm', 'a', 'q'];

    let result = largest(&char_list);
    println!("The largest char is {}", result);
}
```

示例 10-5: 一个使用泛型参数的 `largest` 函数定义, 尚不能编译

如果现在就编译这个代码, 会出现如下错误:

```log
error[E0369]: binary operation `>` cannot be applied to type `T`
 --> src/main.rs:5:12
  |
5 |         if item > largest {
  |            ^^^^^^^^^^^^^^
  |
  = note: an implementation of `std::cmp::PartialOrd` might be missing for `T`
```

注释中提到了 `std::cmp::PartialOrd`, 这是一个 `trait`. 下一部分会讲到 `trait`.
不过简单来说, 这个错误表明 `largest` 的函数体不能适用于 `T` 的所有可能的类型.
因为在函数体需要`比较` T 类型的值, 不过它只能用于我们知道`如何排序`的类型.
为了开启`比较功能`, 你可以在类型上`实现`(implement) 标准库中定义的 `std::cmp::PartialOrd` trait (特性)
(查看附录 C 获取该 trait 的更多信息).

在 `trait 作为参数` 部分会讲解, 如何声明(要求)`泛型` 需要实现了特定的 `trait`,
不过让我们先探索其他使用`泛型参数`的方法.

## 结构体定义中的泛型

同样也可以用 `<>` 语法来定义`结构体`, 它包含一个或多个`泛型`参数类型字段.
示例 10-6 展示了如何定义和使用一个, 可以存放任何类型的 `x` 和 `y` 坐标值的结构体 `Point`:

```rust
struct Point<T> {
    x: T,
    y: T,
}

fn main() {
    let integer = Point { x: 5, y: 10 };
    let float = Point { x: 1.0, y: 4.0 };
}
```

示例 10-6: `Point` 结构体存放了两个 `T` 类型的值 `x` 和 `y`

其语法类似于`函数`定义中使用`泛型`. 首先, 必须在结构体名称后面的`尖括号`中声明`泛型参数`的名称, 例如`T`.
接着在`结构体`定义中需要指定具体`数据类型`的位置, 使用`泛型类型 T`.

注意 `Point<T>` 的定义中只使用了`一个`泛型类型, 这个定义表明结构体 `Point<T>` 对于一些类型 `T` 是泛型的,
而且字段 `x` 和 `y` 都是 相同类型的, 无论它具体是何类型.
如果尝试创建一个有不同类型值的 `Point<T>` 的实例, 像示例 10-7 中的代码就不能编译:

```rust
[这些代码不能编译! ]
struct Point<T> {
    x: T,
    y: T,
}

fn main() {
    let wont_work = Point { x: 5, y: 4.0 };
}
```

示例 10-7: 字段 `x` 和 `y` 的类型必须相同, 因为他们都有相同的泛型类型 `T`

在这个例子中, 当把整型值 `5` 赋值给 `x` 时, 就告诉了编译器这个 `Point<T>` 实例中的泛型 `T` 是`整型`的.
接着指定 `y` 为 `4.0`, 它被定义为与 `x` 相同类型, 就会得到一个像这样的类型不匹配错误:

```log
error[E0308]: mismatched types
 --> src/main.rs:7:38
  |
7 |     let wont_work = Point { x: 5, y: 4.0 };
  |                                      ^^^ expected integer, found
floating-point number
  |
  = note: expected type `{integer}`
             found type `{float}`
```

如果想要定义一个 x 和 y 可以有不同类型且仍然是泛型的 Point 结构体, 我们可以使用多个泛型类型参数.
在示例 10-8 中, 我们修改 Point 的定义为拥有两个泛型类型 T 和 U. 其中字段 x 是 T 类型的, 而字段 y 是 U 类型的:

```rust
struct Point<T, U> {
    x: T,
    y: U,
}

fn main() {
    let both_integer = Point { x: 5, y: 10 };
    let both_float = Point { x: 1.0, y: 4.0 };
    let integer_and_float = Point { x: 5, y: 4.0 };
}
```

示例 10-8: 使用两个泛型的 Point, 这样 x 和 y 可能是不同类型

现在所有这些 `Point` 实例都合法了! 你可以在定义中使用`任意多`的泛型类型参数, 不过太多的话, 代码将难以阅读和理解.
当你的代码中需要许多泛型类型时, 它可能表明你的代码需要重构, 分解成更小的结构.

## 枚举定义中的泛型

和结构体类似, 枚举也可以在成员中存放泛型数据类型.
第六章我们曾用过标准库提供的 `Option<T>` 枚举, 这里再回顾一下:

```rust
enum Option<T> {
    Some(T),
    None,
}
```

现在这个定义应该更容易理解了.
如你所见 `Option<T>` 是一个拥有泛型 T 的枚举, 它有两个成员: `Some`, 它存放了一个类型 T 的值, 和不存在任何值的 `None`.
通过 `Option<T>` 枚举可以表达有一个可能的值的抽象概念, 同时因为 `Option<T>` 是泛型的, 无论这个可能的值是什么类型都可以使用这个抽象.

枚举也可以拥有多个泛型类型. 第九章使用过的 `Result` 枚举定义就是一个这样的例子:

```rust
enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

`Result` 枚举有两个泛型类型, T 和 E. `Result` 有两个成员: `Ok`, 它存放一个类型 T 的值, 而 Err 则存放一个类型 E 的值.
这个定义使得 `Result` 枚举能很方便的表达任何可能成功(返回 T 类型的值)也可能失败(返回 E 类型的值)的操作.
实际上, 这就是我们在示例 9-3 用来打开文件的方式:
当成功打开文件的时候, T 对应的是 std::fs::File 类型; 而当打开文件出现问题时, E 的值则是 std::io::Error 类型.

当你意识到代码中定义了多个结构体或枚举, 它们不一样的地方只是其中的值的类型的时候, 不妨通过泛型类型来避免重复.

## 方法定义中的泛型

在为结构体和枚举实现方法时(像第五章那样), 一样也可以用泛型.
示例 10-9 中展示了示例 10-6 中定义的结构体 `Point<T>`, 和在其上实现的名为 x 的方法.

```rust
struct Point<T> {
    x: T,
    y: T,
}

impl<T> Point<T> {// 这里定义结构体的方法,
    fn x(&self) -> &T {
        &self.x
    }
}

fn main() {
    let p = Point { x: 5, y: 10 };

    println!("p.x = {}", p.x());
}
```

示例 10-9: 在 `Point<T>` 结构体上实现方法 x, 它返回 T 类型的字段 x 的引用

这里在 `Point<T>` 上定义了一个叫做 x 的方法来返回字段 x 中数据的引用:

注意必须在 `impl` 后面声明 T, 这样就可以在 `Point<T>` 上实现的方法中使用它了.
在 `impl` 之后声明 `泛型 T` , 这样 `Rust` 就知道 `Point` 的尖括号中的类型是`泛型`而不是具体类型.

例如, 可以选择为 `Point<f32>` 实例实现方法, 而不是为泛型 Point 实例. 示例 10-10 展示了一个没有在 `impl` 之后(的尖括号)声明泛型的例子, 这里使用了一个具体类型, `f32`:

```rust
impl Point<f32> {
    fn distance_from_origin(&self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}
```

示例 10-10: 构建一个只用于拥有泛型参数 T 的结构体的具体类型的 impl 块

这段代码意味着 `Point<f32>` 类型会有一个方法 `distance_from_origin`, 而其他 T 不是 f32 类型的 `Point<T>` 实例则没有定义此方法.
这个方法计算点实例与坐标 `(0.0, 0.0)` 之间的距离, 并使用了只能用于浮点型的数学运算符.

结构体定义中的泛型类型参数并不总是与结构体方法签名中使用的泛型是同一类型.
示例 10-11 中在示例 10-8 中的结构体 `Point<T, U>` 上定义了一个方法 mixup.
这个方法获取另一个 Point 作为参数, 而它可能与调用 mixup 的 self 是不同的 Point 类型.
这个方法用 self 的 Point 类型的 x 值(类型 T)和参数的 Point 类型的 y 值(类型 W)来创建一个新 Point 类型的实例:

```rust
struct Point<T, U> {
    x: T,
    y: U,
}

impl<T, U> Point<T, U> {
    fn mixup<V, W>(self, other: Point<V, W>) -> Point<T, W> {
        Point {
            x: self.x,
            y: other.y,
        }
    }
}

fn main() {
    let p1 = Point { x: 5, y: 10.4 };
    let p2 = Point { x: "Hello", y: 'c'};

    let p3 = p1.mixup(p2);

    println!("p3.x = {}, p3.y = {}", p3.x, p3.y);
}
```

示例 10-11: 方法使用了与结构体定义中不同类型的泛型

在 main 函数中, 定义了一个有 i32 类型的 x(其值为 5)和 f64 的 y(其值为 10.4)的 Point.
p2 则是一个有着字符串 slice 类型的 x(其值为 "Hello")和 char 类型的 y(其值为c)的 Point.
在 p1 上以 p2 作为参数调用 mixup 会返回一个 p3, 它会有一个 i32 类型的 x,
因为 x 来自 p1, 并拥有一个 char 类型的 y, 因为 y 来自 p2. println! 会打印出 p3.x = 5, p3.y = c.

这个例子的目的是展示一些泛型通过 impl 声明而另一些通过方法定义声明的情况.
这里泛型参数 T 和 U 声明于 impl 之后, 因为他们与结构体定义相对应.
而泛型参数 V 和 W 声明于 fn mixup 之后, 因为他们只是相对于方法本身的.

## 泛型代码的性能

在阅读本部分内容的同时, 你可能会好奇使用泛型类型参数是否会有运行时消耗.
好消息是: Rust 实现了泛型, 使得使用泛型类型参数的代码相比使用具体类型并没有任何速度上的损失.

Rust 通过在编译时进行泛型代码的 单态化(monomorphization)来保证效率.
单态化是一个通过填充编译时使用的具体类型, 将通用代码转换为特定代码的过程.

编译器所做的工作正好与示例 10-5 中我们创建泛型函数的步骤相反.
编译器寻找所有泛型代码被调用的位置并使用泛型代码针对具体类型生成代码.

让我们看看一个使用标准库中 Option 枚举的例子:

```rust
let integer = Some(5);
let float = Some(5.0);
```

当 Rust 编译这些代码的时候, 它会进行单态化.
编译器会读取传递给 `Option<T>` 的值并发现有两种 `Option<T>`: 一个对应 i32 另一个对应 f64.
为此, 它会将泛型定义 `Option<T>` 展开为 `Option_i32` 和 `Option_f64`, 接着将泛型定义替换为这两个具体的定义.

编译器生成的单态化版本的代码看起来像这样, 并包含将泛型 `Option<T>` 替换为编译器创建的具体定义后的用例代码:

文件名: src/main.rs

```rust
enum Option_i32 {
    Some(i32),
    None,
}

enum Option_f64 {
    Some(f64),
    None,
}

fn main() {
    let integer = Option_i32::Some(5);
    let float = Option_f64::Some(5.0);
}
```

我们可以使用泛型来编写不重复的代码, 而 Rust 将会为每一个实例编译其特定类型的代码.
这意味着在使用泛型时没有运行时开销; 当代码运行, 它的执行效率就跟好像手写每个具体定义的重复代码一样.
这个单态化过程正是 Rust 泛型在运行时极其高效的原因.
