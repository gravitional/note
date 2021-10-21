# 将模块分割进不同文件

到目前为止, 本章所有的例子都把多个模块定义在同一个文件中.
当模块变得更大时, 你可能想要将它们的定义移动到单独的文件, 从而使代码更容易阅读.

例如, 我们从示例 7-17 开始, 将 `front_of_house` 模块移动到属于它自己的文件 `src/front_of_house.rs` 中,
并改写 `crate 根`文件, 使其包含示例 7-21 所示的代码.
在这个例子中, `crate 根` 文件是 `src/lib.rs`, 这也同样适用于以 `src/main.rs` 为 `crate 根`文件的`二进制 crate`s.

```rust
mod front_of_house;

pub use crate::front_of_house::hosting;

pub fn eat_at_restaurant() {
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
    hosting::add_to_waitlist();
}
```

示例 7-21: 声明 `front_of_house` 模块, 其内容将位于 `src/front_of_house.rs`

`src/front_of_house.rs` 会获取 `front_of_house` 模块的定义内容, 如示例 7-22 所示.

```rust
pub mod hosting {
    pub fn add_to_waitlist() {}
}
```

示例 7-22: 在 `src/front_of_house.rs` 中定义 `front_of_house` 模块

在 `mod front_of_house` 后使用`分号`, 而不是`代码块`, 这将告诉 `Rust` 在另一个与模块同名的`文件`中加载模块的内容.
继续重构我们例子, 将 `hosting` 模块也提取一个单独的文件中, 我们修改 `src/front_of_house.rs`, 让它只包含 `hosting` 模块的声明:

```rust
pub mod hosting;
```

接着我们创建一个 `src/front_of_house` 目录和一个包含 `hosting` 模块定义的 `src/front_of_house/hosting.rs` 文件:

文件名: src/front_of_house/hosting.rs

```rust
pub fn add_to_waitlist() {}
```

模块树依然保持相同, `eat_at_restaurant` 中的`函数调用`也无需修改, 继续保持有效, 即便其定义存在于不同的文件中.
这个技巧让你可以在模块代码增长时, 将它们移动到新文件中.

注意, `src/lib.rs` 中的 `pub use crate::front_of_house::hosting` 语句不需要改变,
`use` 不影响, 选择哪种文件组织组成 `crate` 参与编译.
`mod` 关键字声明了模块, `Rust` 会在与模块同名的文件中查找模块的代码.

## 总结

`Rust` 提供了将`包`分成多个 `crate`, 将 `crate` 分成`模块`, 以及通过指定`绝对`或`相对路径`, 在模块`A`中引用模块`B`中的定义的方式.
你可以通过使用 `use` 语句将路径引入作用域, 这样在多次使用时可以使用更短的路径.
模块定义的代码默认是`私有的`, 不过可以选择增加 `pub` 关键字使其定义变为公有.

接下来, 让我们看看一些标准库提供的`集合数据`类型, 你可以利用它们编写出漂亮整洁的代码.
