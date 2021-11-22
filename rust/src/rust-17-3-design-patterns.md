# 面向对象设计模式的实现

    ch17-03-oo-design-patterns.md
    commit 7e219336581c41a80fd41f4fbe615fecb6ed0a7d

`状态模式`(state pattern)是一种面向对象的`设计模式`.
该模式的核心是, 某个`值`有一些内部状态, 这些状态由一组`状态对象`表示, `值`的`行为`根据`内部状态`而改变.

这些`状态对象`共享功能(functionality):  当然, 在Rust中, 我们使用 `结构体` 和`trait`, 而不是 `对象` 和 `继承`.
每个`状态对象`负责其自身的行为, 以及它何时应该转变到另一个状态.
持有`状态对象`的`值`不需要了解状态的不同`行为`, 以及何时在状态间进行转换.

使用`状态模式`意味着当程序的业务需求改变时,
我们不需要改变与保存`状态` 的 `值`相关的代码, 或使用此`值`的代码.
我们只需要更新其中某个`状态对象`内部的代码, 以改变其规则, 或者增加更多的`状态对象`.
让我们来看看状态设计模式的一个例子, 以及如何在Rust中使用它.

为了探索这个概念, 我们将实现一个增量式的发布博文的工作流. 这个博客的最终功能将是这样的:

+ 博文从空白的草稿开始.
+ 当草稿完成后, 请求审核博文.
+ 一旦博文过审, 它将被发表.
+ 只有被发表的博文的内容会被打印, 所以未经批准的文章不会意外地被发表.

任何其他对博文的修改尝试都是没有作用的.
例如, 如果尝试在请求审核之前通过一个博文草稿, 博文应该保持未发布的状态.

示例 17-11 以代码的形式展示了这个工作流:
这是一个API的使用实例, 我们将在名为`blog`的库中实现.
这还不能编译, 因为我们还没有实现 `blog crate`:

文件名: src/main.rs

```rust
use blog::Post;

fn main() {
    let mut post = Post::new();

    post.add_text("I ate a salad for lunch today");
    assert_eq!("", post.content());

    post.request_review();
    assert_eq!("", post.content());

    post.approve();
    assert_eq!("I ate a salad for lunch today", post.content());
}
```

示例 17-11: 展示了 blog crate 期望行为的代码

我们希望允许用户使用 `Post::new` 创建一个新的博文草稿. 接着希望能为 `草稿`状态的博文添加一些文本.
如果尝试在`审核`之前立即打印出博文的内容, 则什么也不会发生, 因为博文仍然是草稿.
这里增加的 `assert_eq!` 出于演示目的.
可以编写一个不错的单元测试: 断言草稿博文的 `content` 方法返回`空字符串`, 不过我们并不准备为这个例子编写单元测试.

接下来, 我们希望能够请求`审核`博文, 而在等待`审核`的阶段 `content` 应该仍然返回空字符串.
最后当博文通过`审核`, 它应该被`发表`(published), 这意味着当调用 `content` 时, 博文的文本将被返回.

注意我们与 `crate` 交互的唯一 type是 `Post` .
这个 type 会使用`状态模式`, 它持有的`值`将会是博文可能处于的三种状态  --  `draft`, `waiting for review` 和 `published`.

状态的改变将由 `Post` type 内部自行管理.
状态的改变, 将取决于库用户对 `Post 实例`调用的方法, 但是用户不需要直接管理状态变化.
这也意味着用户不会在状态上犯错, 比如在过审前发布博文.

## 定义 Post 并新建一个草稿状态的实例

让我们开始实现这个`库`吧!
我们知道需要一个`公有 Post 结构体`来存放一些文本,
所以让我们先定义此`结构体`, 并创建 `Post 实例`的公有关联函数 `new`, 如示例 17-12 所示.

还需定义一个私有 `State` trait.
然后 `Post` 将在私有字段 `state` 中, 存放 `Option<T>` 类型的 `trait 对象 Box<dyn State>`.
稍后将会看到为何必须是 `Option<T>` 类型的.

文件名: src/lib.rs

```rust
pub struct Post {
    state: Option<Box<dyn State>>, // trait 对象, 要求 State trait
    content: String,
}

impl Post {
    pub fn new() -> Post { // 公有关联函数 `new`
        Post {
            state: Some(Box::new(Draft {})),
            content: String::new(),
        }
    }
}

trait State {}

struct Draft {}

impl State for Draft {}
```

示例 17-12: Post 结构体的定义, 和新建 `Post` 实例的 `new` 函数, `State` trait 和结构体 `Draft`

`State` trait 定义了由所有不同状态的博文共享的`行为`,
并且 `Draft`, `PendingReview` 和 `Published` 状态都会实现 `State` trait.
现在这个 `trait` 暂时没有任何方法, 一开始我们将只定义 `Draft` 状态,
因为我们期望它是博文的初始状态.

当创建新的 `Post` 时, 我们将它的 `state` 字段设置为一个`Some 值`, 后者持有 `Box`.
这个 `Box` 指向一个 `Draft` 结构体的新实例.

这确保了, 无论何时新建一个 `Post` 实例, 它都会从`草稿`开始.
因为 `Post` 的 `state` 字段是私有的, 所以无法创建处于任何其他状态的 `Post`.
在 `Post::new` 函数中, 我们将 `content` 设置为新建的`空 String`.

### 存放博文内容的文本

示例 17-11 展示了: 我们希望能够调用叫做 `add_text` 的方法, 并向它传递一个 `&str`, 来将文本增加到博文内容.
我们选择实现为一个方法, 而不是将 `content` 字段暴露为 `pub` .
这意味着之后可以实现一个方法来控制 `content` 字段如何被读取.
`add_text` 方法是非常直观的, 让我们在示例 17-13 的 `impl Post` 块中增加实现:

文件名: src/lib.rs

```rust
impl Post {
    // --snip--
    pub fn add_text(&mut self, text: &str) {
        self.content.push_str(text);
    }
}
```

示例 17-13: 实现方法 `add_text` 来向博文的 `content` 增加文本

`add_text` 获取一个 `self` 的可变引用, 因为我们需要改变 `add_text` 作用的 `Post` 实例.
接着我们调用 `content` 中 `String` 的 `push_str`关联方法, 并传递 `text` 参数来保存到 `content` 中.

这些不是`状态模式`的一部分, 因为它的行为并不依赖于博文所处的状态.
`add_text` 方法完全不与 `state` 字段交互, 不过我们希望支持这些行为.

### 确保博文草稿的内容是空的

即使我们调用 `add_text`, 并向博文增加一些内容之后, 我们仍然希望 `content` 方法返回一个空字符串 `slice`,
因为博文仍然处于`草稿状态`, 如示例 17-11 的第 8 行所示.

现在, 让我们使用最简单的方式来实现 `content` 方法, 暂时满足要求: 总是返回一个空字符串 `slice`.
当实现了将`博文状态`改为发布的功能之后, 我们再改变这一做法.
目前博文只能是`草稿状态`, 这意味着其内容应该总是空的.
示例 17-14 展示了这个占位符实现(placeholder implementation):

文件名: src/lib.rs

```rust
impl Post {
    // --snip--
    pub fn content(&self) -> &str {
        ""
    }
}
```

列表 17-14: 增加一个 `Post` 的 `content` 方法的占位实现, 它总是返回一个`空字符串 slice`

通过增加这个 `content` 方法, 示例 17-11 中直到第 8 行的代码能如期运行.

### 请求审核博文来改变其状态

接下来需要增加 `请求审核博文` 的功能, 这应当将其状态由 `Draft` 改为 `PendingReview`.
示例 17-15 展示示例代码:

文件名: src/lib.rs

```rust
impl Post {
    // --snip--
    pub fn request_review(&mut self) { // 获取可变引用的公有方法
        if let Some(s) = self.state.take() { // `take` 方法将 `state` 字段中的 `Some` 值取出,并留下一个 `None`, 同时获取 state 的所有权
            self.state = Some(s.request_review()) // 调用当前状态 s 的 request_review 方法
        }
    }
}

trait State {
    fn request_review(self: Box<Self>) -> Box<dyn State>; // 给 `State` trait 增加了 `request_review` 方法
}

struct Draft {}

impl State for Draft {
    fn request_review(self: Box<Self>) -> Box<dyn State> { // 函数签名需要一致
        Box::new(PendingReview {})
    }
}

struct PendingReview {}

impl State for PendingReview {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        self
    }
}
```

示例 17-15: 实现 `Post` 和 `State` trait 的 `request_review` 方法

我们给 `Post` 增加了一个公有方法 `request_review`, 获取 `self` 的可变引用.
接着在 `Post` 的当前状态下, 我们调用当前状态内部的 `request_review` 方法,
并且第二个 `request_review` 方法会消费当前的状态, 并返回一个新状态.

这里给 `State` trait 增加了 `request_review` 方法;
所有实现了此 `trait` 的 type, 现在都需要实现 `request_review` 方法.

注意不同于使用 `self`,  `&self` 或者 `&mut self` 作为方法的首参数, 这里使用了 `self: Box<Self>`.
这个语法意味着此方法, 只能在持有此 `type` 的 `Box` 上调用.
这个语法获取了 `Box<Self>` 的所有权, 从而无效化`老的状态`, 以便将 `Post`  转换为新状态.

为了消耗老状态, `request_review` 方法需要获取`state value`的所有权.
这也就是 `Post` 的 `state` 字段中 `Option` 的来历:
我们调用 `take` 方法, 将 `state` 字段中的 `Some` value取出,
并在原地留下一个 `None`, 因为 `Rust` 不允许在结构体中存在 `未填充的字段`.

这使得我们将 `state` 值移出 `Post` 而不是`借用`它(move out rather borrowing).
接着我们将博文的 `state` 值设置为这个操作的结果.

在这里, 我们需要将 `state` 临时设置为 `None`,
而不能用类似 `self.state = self.state.request_review();`这样的代码, 直接设置 `state` 字段.从而获取 `state` value 的所有权.
这确保了当 `Post` 被转换为新状态后, 不能再使用老的 `state` 值.

`Draft` 的方法 `request_review` 需要返回一个新的, boxed 的 `PendingReview` 结构体的实例, 用来代表博文处于等待审核状态.
结构体 `PendingReview` 同样也实现了 `request_review` 方法, 不过它不进行状态转换.
相反它返回自身, 因为当我们请求审核, 已处于 `PendingReview` 状态的博文时,
博文应该保持 `PendingReview` 的状态.

现在能够看出状态模式的优势了:
无论 `state` value 是什么, `Post` 的 `request_review` 方法都是一样的. 每个状态只负责它自己的规则.

我们将继续保持 `Post` 的 `content` 方法不变, 返回一个`空字符串 slice`.
现在我们可以拥有 `PendingReview` 状态, 而不仅仅是 `Draft` 状态的 `Post` 了,
不过我们希望在 `PendingReview` 状态下也能有相同的行为. 现在示例 17-11 中直到 10 行的代码都是可以执行的!

### 增加改变 content 行为的 approve 方法

[当我们比编译器知道更多的情况]: https://kaisery.github.io/trpl-zh-cn/ch09-03-to-panic-or-not-to-panic.html#cases-in-which-you-have-more-information-than-the-compiler

`approve` 方法与 `request_review` 方法类似: 它将 `state` 设置为 `审核通过`的状态, 如示例 17-16 所示.

文件名: src/lib.rs

```rust
impl Post {
    // --snip--
    pub fn approve(&mut self) {
        if let Some(s) = self.state.take() {
            self.state = Some(s.approve())
        }
    }
}

trait State {
    fn request_review(self: Box<Self>) -> Box<dyn State>; // 虽然后文返回了self, 但签名中的返回类型是 Box<dyn State>
    fn approve(self: Box<Self>) -> Box<dyn State>;
}

struct Draft {}

impl State for Draft {
    // --snip--
    fn approve(self: Box<Self>) -> Box<dyn State> {
        self // 这里返回了self, 但显式的返回类型是 Box<dyn State>
    }
}

struct PendingReview {}

impl State for PendingReview {
    // --snip--
    fn approve(self: Box<Self>) -> Box<dyn State> {
        Box::new(Published {})
    }
}

struct Published {}

impl State for Published {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        self
    }

    fn approve(self: Box<Self>) -> Box<dyn State> {
        self
    }
}
```

示例 17-16: 为 `Post` 和 `State` trait 实现 `approve` 方法

我们为 `State` trait 增加了 `approve` 方法, 并新增了一个结构体, 实现 `State` 的 `Published` 状态 .

类似于 `request_review`, 如果对 `Draft` 调用 `approve` 方法, 并没有任何效果, 因为它会返回 `self`.
当对 `PendingReview` 调用 `approve` 时, 它返回一个新的, `boxed` 的 `Published` 结构体的实例.
`Published` 结构体实现了 `State` trait, 并且对于 `request_review` 和 `approve` 两个方法,
它都返回自身, 因为在这两种情况下, 博文都应该保持 `Published` 状态.

现在更新 `Post` 的 `content` 方法:
如果状态为 `Published`, 我们希望返回博文 `content` 字段的值;
否则希望返回 `空字符串 slice`, 如示例 17-17 所示:

文件名: src/lib.rs

```rust
impl Post {
    // --snip--
    pub fn content(&self) -> &str {
        self.state.as_ref().unwrap().content(self) // 这里的 self 是 结构体 Post
    }
    // --snip--
}
```

示例 17-17: 更新 `Post` 的 `content` 方法, 来委托调用 `State` 的 `content` 方法(delegate)

因为目标是, 将所有类似这样的规则保持在实现 `State` 的`结构体`中,
我们调用 `state` value 的 `content` 方法, 并传递博文实例(也就是 `self`)作为参数.
接着我们返回, `state` value 的 `content` 方法的返回值.

这里调用 `Option` 的 `as_ref` 方法是因为, 我们需要 `Option` 中值的`引用`, 而不是获取其所有权.
因为 `state` 是一个 `Option<Box<State>>`, 调用 `as_ref` 会返回一个 `Option<&Box<State>>`.
如果不调用 `as_ref`, 将会得到一个错误, 因为不能将 `state` 从借用的函数参数 `&self` 移出(move out).

接着调用 `unwrap` 方法, 这里我们知道它永远也不会 `panic`,
因为 `Post` 的所有方法都确保在运行完毕时, `state` 会有一个 `Some` 值.
这就是第十二章的一个例子, 在 [当我们比编译器知道更多的情况][]部分讨论过,
我们知道 `None` 是不可能的, 而编译器却不能理解这种情况.

接着, 当我们对 `&Box<State>`调用 `content` 方法时, `Deref` 强制转换会作用于 `&` 和 `Box` ,
最终`content` 方法会作用在 实现了 `State` trait 的类型上.
这意味着需要在 `State` trait 的定义中增加 `content`,
这也是放置以下逻辑的地方, 根据所处状态返回相应内容的, 如示例 17-18 所示:

文件名: src/lib.rs

```rust
trait State {
    // --snip--
    fn content<'a>(&self, post: &'a Post) -> &'a str {
        ""
    }
}

// --snip--
struct Published {}

impl State for Published {
    // --snip--
    fn content<'a>(&self, post: &'a Post) -> &'a str { // 这里的 self 是 结构体 Published, 而我们需要返回 Post 的内容
        &post.content
    }
}
```

示例 17-18: 为 `State` trait 增加 content 方法

在 `State` trait 中, 我们为 `content` 方法添加了一个`默认实现`, 返回 `空字符串 slice`.
这意味着无需为 `Draft` 和 `PendingReview` 结构体实现 `content` 了.
在 `Published` 结构体中我们覆盖了 `content` 方法, 返回 `post.content` 的值.

注意这个方法需要`生命周期注解`, 如第十章所讨论的.
我们获取了 `post` 的引用作为参数, 并返回 `post` 某部分的`引用`, 所以返回的引用的生命周期与 `post` 参数相关.

现在示例完成了  --  现在示例 17-11 中所有的代码都能工作! 我们通过发布博文工作流的规则, 实现了`状态模式`.
围绕这些规则的逻辑都存在于`状态对象`中, 而不是分散在 `Post` 之中.

### 状态模式的权衡取舍

我们展示了 `Rust` 能够实现面向对象的`状态模式`, 以便能根据博文所处的`状态`来封装不同类型的行为.
`Post` 的方法无需了解这些不同类型的行为. 通过这种组织代码的方式,
要找到所有`已发布博文`的不同行为, 只需查看一处代码: `Published` 结构体的 `State` trait 的实现.

如果要创建一个不使用`状态模式`的替代实现, 则可能会在 `Post` 的方法中,
或者甚至于在 `main` 代码中用到 `match` 语句, 来检查`博文状态`, 并在此处改变其行为.
这意味着, 为了理解`已发布博文`的所有逻辑, 我们需要查看很多位置!
在增加更多状态时, 事情会变得更糟: 每一个 `match` 语句都会需要另一个分支(arm).

对于`状态模式`来说, `Post` 的方法, 和使用 `Post` 的位置无需 `match` 语句,
如果需要增加新状态, 我们只需增加一个新 `struct`, 并为其实现 trait 的方法.

这个基于`状态模式`的实现, 易于扩展增加更多功能.
为了体会使用此模式维护代码的简洁性, 请尝试实现如下功能:

+ 增加 `reject` 方法, 将博文的状态从 `PendingReview` 变回 `Draft`
+ 在将状态变为 `Published` 之前, 需要两次 `approve` 调用
+ 只允许在博文处于 `Draft` 状态时, 增加文本内容. 提示: 让状态对象负责 `content` 能够发生的改变, 但不负责修改 `Post`.

`状态模式`的一个缺点是: 因为状态实现了状态之间的转换, 一些状态会相互联系.

如果在 `PendingReview` 和 `Published` 之间增加另一个状态, 比如 `Scheduled`,
则不得不修改 `PendingReview` 中的代码来转移到 `Scheduled`.
如果 `PendingReview` 无需因为`新增状态`而改变就更好了, 不过这意味着切换到另一种设计模式.

另一个缺点是我们会发现一些`重复的逻辑`.
为了消除他们, 可以尝试为 `State` trait 中, 返回 `self` 的 `request_review` 和 `approve` 方法增加默认实现,
不过这会违反`对象安全`性, 因为 `trait` 不知道 `self` 具体是什么, `trait object` 不记录具体的对象 type.
我们希望能够将 `State` 作为一个 `trait object`, 所以需要其方法是`对象安全`的.

另一个重复是 `Post` 中 `request_review` 和 `approve` 这两个类似的实现.
他们都委托调用了 `state` 字段中 `Option` 值的同一方法, 并在结果中为 `state` 字段设置了新值.
如果 `Post` 中的很多方法都遵循这个模式, 我们可能会考虑定义一个`宏`来消除重复(查看第十九章的 "宏" 部分).

如果完全按照`面向对象`语言的定义来实现`状态模式`, 我们就不能充分利用Rust的优势了.
让我们看看一些代码中可以做出的修改, 来将`无效的状态`和`状态转移`变为编译时错误.

## 将状态和行为编码为类型

我们将展示如何稍微反思`状态模式`, 来进行一系列不同的权衡取舍(trade--offs).
不同于完全封装`状态`和`状态转移`, 使得外部代码对其毫不知情, 我们将`状态`编码进不同的`type`s.
如此, `Rust` 的类型检查就会将任何在只能使用发布博文的地方使用草稿博文的尝试变为编译时错误.
如此, Rust 的类型检查系统将通过发出`编译时`错误, 来防止试图在只允许`published`博文的地方使用`draft` 博文.

让我们考虑一下示例 17-11 中 main 的第一部分:

文件名: src/main.rs

```rust
fn main() {
    let mut post = Post::new();

    post.add_text("I ate a salad for lunch today");
    assert_eq!("", post.content());
}
```

我们仍然希望能够使用 `Post::new` 创建一个新的草稿博文, 并能够增加博文的内容.

但是, 我们不会在`draft` post 上设置一个返回`空字符串`的 `content` 方法, 而是让`draft`根本没有 `content` 方法.
这样一来, 如果我们试图获取一个`draft`文章的内容, 我们会得到一个`编译时`错误, 告诉我们这个方法不存在.
因此, 我们将不可能在写作过程中意外地显示草稿文章的内容, 因为代码甚至不会被编译.
示例 17-19 展示了 `Post` 结构体, `DraftPost` 结构体以及各自的方法的定义:

文件名: src/lib.rs

```rust
pub struct Post {
    content: String,
}

pub struct DraftPost {
    content: String,
}

impl Post {
    pub fn new() -> DraftPost {
        DraftPost {
            content: String::new(),
        }
    }

    pub fn content(&self) -> &str {
        &self.content
    }
}

impl DraftPost {
    pub fn add_text(&mut self, text: &str) {
        self.content.push_str(text);
    }
}
```

示例 17-19: 带有 `content` 方法的 `Post`, 和没有 `content` 方法的 `DraftPost`

`Post` 和 `DraftPost` 结构体都有一个私有的 `content` 字段来储存博文的文本.
这些结构体不再有 `state` 字段, 因为我们将`状态`编码改为`结构体 type`.
`Post` 代表已发布的博文, 它具有返回 `content` 字段的 `content` 方法.

仍然有一个 `Post::new` 函数, 不过不同于返回 `Post` 实例, 它返回 `DraftPost` 的实例.
现在不可能创建一个 `Post` 实例, 因为 `content` 是私有的同时没有任何函数返回 `Post`.

`DraftPost` 上定义了一个 `add_text` 方法, 这样就可以像之前那样向 `content` 增加文本, 不过注意 `DraftPost` 并没有定义 `content` 方法!
如此现在程序确保了所有博文都从`草稿`开始, 同时`草稿`博文没有任何可供展示的内容.
任何绕过这些限制的尝试都会产生编译错误.

### 将状态转移实现为不同类型的转换

那么如何得到发布的博文呢?
我们希望强制执行的规则是, 草稿博文在发布之前必须被审核通过. 等待审核状态的博文应该仍然不会显示任何内容.
让我们通过增加另一个结构体 `PendingReviewPost` 来实现这个限制,
在 `DraftPost` 上定义 `request_review` 方法来返回 `PendingReviewPost`,
并在 `PendingReviewPost` 上定义 `approve` 方法来返回 `Post`, 如示例 17-20 所示:

文件名: src/lib.rs

```rust
impl DraftPost {
    // --snip--

    pub fn request_review(self) -> PendingReviewPost {
        PendingReviewPost {
            content: self.content,
        }
    }
}

pub struct PendingReviewPost {
    content: String,
}

impl PendingReviewPost {
    pub fn approve(self) -> Post {
        Post {
            content: self.content,
        }
    }
}
```

列表 17-20: `PendingReviewPost` 通过调用 `DraftPost` 的 `request_review` 创建, `approve` 方法将 `PendingReviewPost` 变为发布的 `Post`

`request_review` 和 `approve` 方法获取 `self` 的所有权,
因此会消费 `DraftPost` 和 `PendingReviewPost` 实例, 并分别转换为 `PendingReviewPost` 和发布的 `Post`.
这样在调用 `request_review` 之后就不会遗留任何 `DraftPost` 实例, 后者同理.
`PendingReviewPost` 并没有定义 `content` 方法, 所以尝试读取其内容会导致编译错误, `DraftPost` 同理.

因为只有`Post` 实例定义了`content` 方法, 得到此实例的唯一途径是,
调用 `PendingReviewPost` 的 `approve` 方法, 而得到 `PendingReviewPost` 的唯一办法是,
调用 `DraftPost` 的 `request_review` 方法, 如此, 我们就把发博文的工作流编码进了类型系统.

这也意味着不得不对 `main` 做出一些小的修改.
因为 `request_review` 和 `approve` 返回新实例, 而不是修改被作用的结构体,
所以我们需要增加更多的 `let post =` 覆盖赋值, 来保存返回的实例.

我们也不再能断言`草稿`和`等待审核`的博文的内容为`空字符串`了, 我们也不再需要他们:
尝试在这些状态下使用博文内容的代码不能通过编译.
更新后的 `main` 的代码如示例 17-21 所示:

文件名: src/main.rs

```rust
use blog::Post;

fn main() {
    let mut post = Post::new();

    post.add_text("I ate a salad for lunch today");

    let post = post.request_review();

    let post = post.approve();

    assert_eq!("I ate a salad for lunch today", post.content());
}
```

示例 17-21: main 中使用新的博文工作流实现的修改

不得不修改 `main` 来重新赋值 `post` , 使得这个实现不再完全遵守`面向对象`的`状态模式`: 状态间的转换不再完全封装在 `Post` 实现中.
然而, 得益于`类型系统`和`编译时`类型检查, 我们保证不会得到无效状态!
这确保了某些特定的 `bug`, 比如显示未发布博文的内容, 将在部署到生产环境之前被发现.

尝试为示例 17-20 之后的 `blog crate`, 实现这这一小节开始的时候, 建议增加的额外需求,
来体会使用这个版本的代码是何等感觉. 注意在这个设计中一些需求可能已经完成了.

我们已经看到, 尽管Rust能够实现`面向对象`的设计模式,
但其他模式, 例如将状态编码到`type 系统`中, 也可以在Rust中使用. 这些模式有不同的取舍.
尽管你可能对面向对象的模式非常熟悉, 但重新思考问题以利用 `Rust` 的特性可以带来好处, 比如在编译时防止一些错误.
在Rust中, 由于某些面向对象语言不具备的特性, 如`所有权`, 面向对象的模式并不总是最好的解决方案.

## 总结

不管你在读完本章后是否认为 `Rust` 是一种面向对象的语言,
现在你都知道了, 可以使用 `trait` 对象来获得Rust中的一些面向对象的特性.
`动态调度` 可以给你的代码带来一些灵活性, 通过牺牲少量`运行时`的性能.
你可以利用这种灵活性来实现面向对象的模式, 这有助于你的代码的可维护性.
Rust也有其他的功能, 比如`所有权`, 这是面向对象语言所没有的.
面向对象的模式并不总是利用Rust的优势的最佳方式, 但也是一种可用的选择.

接下来, 让我们看看另一个提供了多样灵活性的 Rust 功能: `模式`.
贯穿全书的模式, 我们已经和它们打过照面了, 但并没有见识过它们的全部本领. 让我们开始探索吧!
