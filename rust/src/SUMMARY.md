# Summary

- [教程,示例](rust-1-basic.md)

- [猜猜看游戏教程](rust-2-game.md)

- [常见编程概念](rust-3-usual.md)
    - [变量与可见性](rust-3-1-variable.md)
    - [数据类型](rust-3-2-datatype.md)
    - [函数如何工作](rust-3-3-function.md)
    - [注释](rust-3-4-comment.md)
    - [控制流](rust-3-5-control-flow.md)

- [认识所有权](rust-4-ownership.md)
    - [什么是所有权?](rust-4-1-what-is-ownership.md)
    - [引用与借用](rust-4-2-reference.md)
    - [Slices](rust-4-3-slices.md)

- [使用结构体来组织相关联的数据](rust-5-struct.md)
    - [定义并实例化结构体](rust-5-1-defining-structs.md)
    - [一个使用结构体的示例程序](rust-5-2-example-structs.md)
    - [方法语法](rust-5-3-method-syntax.md)

- [枚举与模式匹配](rust-6-enums.md)
    - [定义枚举](rust-6-1-defining-enums.md)
    - [match 控制流运算符](rust-6-2-match.md)
    - [if let 简洁控制流](rust-6-3-if-let.md)

- [使用包, Crate 和模块管理不断增长的项目](rust-7-crates.md)
    - [包和crate](rust-7-1-packages-crates.md)
    - [定义模块来控制作用域和私有性](rust-7-2-modules.md)
    - [路径用于模块树中的项](rust-7-3-paths-in-moduletree.md)
    - [使用 use 关键字将名称引入作用域](rust-7-4-use-keyword.md)
    - [将模块分割进不同文件](rust-7-5-multi-files.md)

- [常见集合](rust-8-collections.md)
    - [vector](rust-8-1-vectors.md)
    - [字符串](rust-8-2-strings.md)
    - [哈希 map](rust-8-3-hash-maps.md)

- [错误处理](rust-9-errors.md)
    - [panic! 与不可恢复的错误](rust-9-1-panic.md)
    - [Result 与可恢复的错误](rust-9-2-result.md)
    - [panic! 还是不 panic!](rust-9-3-panic-or-not.md)

- [泛型,trait 和生命周期](rust-10-generics.md)
    - [泛型数据类型](rust-10-1-syntax.md)
    - [trait:定义共享的行为](rust-10-2-traits.md)
    - [生命周期与引用有效性](rust-10-3-lifetime.md)

- [测试](rust-11-testing.md)
    - [编写测试](rust-11-1-writing-tests.md)
    - [运行测试](rust-11-2-running-tests.md)
    - [测试的组织结构](rust-11-3-test-organization.md)

- [一个I/O 项目: 构建命令行程序](rust-12-io-project.md)
    - [接受命令行参数](rust-12-1-cmd-arguments.md)
    - [读取文件](rust-12-2-reading-file.md)
    - [重构以改进模块化与错误处理](rust-12-3-improving-error.md)
    - [采用测试驱动开发完善库的功能](rust-12-4-testing-library.md)
    - [处理环境变量](rust-12-5-env-variables.md)
    - [将错误信息输出到标准错误而不是标准输出](rust-12-6-writting-to-stderr.md)

- [Rust 中的函数式语言功能:迭代器与闭包](rust-13-functional-features.md)
    - [闭包: 可以捕获其环境的匿名函数](rust-13-1-closures.md)
    - [时用迭代器处理元素序列](rust-13-2-iterators.md)
    - [改进之前的I/O项目](rust-13-3-improving-io.md)
    - [性能比较: 循环对迭代器](rust-13-4-performance.md)

- [更多关于 Cargo 和 Crates.io 的内容](rust-14-cargo-more.md)
    - [采用发布配置自定义构建](rust-14-1-release-profile.md)
    - [将 crate 发布到 Crates.io](rust-14-2-publish.md)
    - [Cargo 工作空间](rust-14-3-workspace.md)
    - [使用 cargo install 从 Crates.io 安装二进制文件](rust-14-4-binaries.md)
    - [Cargo 自定义拓展命令](rust-14-5-extending.md)

- [智能指针](rust-15-smart-pointers.md)
    - [Box 指向堆上数据，并且可确定大小](rust-15-1-box.md)
    - [通过 Deref trait 将智能指针当作常规引用处理](rust-15-2-deref.md)
    - [Drop Trait 运行清理代码](rust-15-3-drop.md)
    - [Rc 引用级数智能指针](rust-15-4-rc-count.md)
    - [RefCell 与内部可变性模式](rust-15-5-interior-mutability.md)
    - [引用循环与内存泄漏是安全的](rust-15-6-reference-cycles.md)

- [无畏并发](rust-16-concurrency.md)
    - [线程](rust-16-1-threads.md)
    - [消息传递](rust-16-2-message-passing.md)
    - [共享状态](rust-16-3-shared-state.md)
    - [可扩展的并发: Sync 与 Send](rust-16-4-sync-send.md)

- [Rust 的面向对象编程特性](rust-17-oop.md)
    - [面向对象语言的特点](rust-17-1-what-is-oop.md)
    - [为使用不同类型的值而设计的 trait 对象](rust-17-2-trait-objects.md)
    - [面向对象设计模式的实现](rust-17-3-design-patterns.md)

- [模式与模式匹配](rust-18-patterns.md)
    - [所有可能会用到模式的位置](rust-18-1-places.md)
    - [Refutability(可反驳性):模式是否会匹配失败](rust-18-2-refutability.md)
    - [模式的全部语法](rust-18-3-pattern-syntax.md)

- [高级特征](rust-19.md)
    - [不安全的 Rust](rust-19-1.md)
    - [高级 trait](rust-19-2.md)
    - [高级类型](rust-19-3.md)
    - [高级函数与闭包](rust-19-4.md)
    - [宏](rust-19-5.md)

- [最后的项目: 构建多线程 web server](rust-20.md)
    - [单线程 web server](rust-20-1.md)
    - [将单线程 server 变为多线程 server](rust-20-2.md)
    - [优雅停机与清理](rust-20-3.md)

- [附录](rust-21.md)
    - [A - 关键字](rust-21-1.md)
    - [B - 运算符与符号](rust-21-2.md)
    - [C - 可派生的trait](rust-21-3.md)
    - [D - 实用开发工具](rust-21-4.md)
    - [E - 版本](rust-21-5.md)
    - [F - 书本译名](rust-21-6.md)
    - [G - Rust 是如何开发的与 "Nightly Rust"](rust-21-7.md)

***

- [rust sd](rust-sd.md)
- [rust ripgrep](rust-ripgrep.md)

***

- [rust 命令行工具](rust-cmd-1.md)
- [建立项目](rust-cmd-1.1-setup.md)
- [解析命令行参数](rust-cmd-1.2-parsing.md)
- [第一个实现](rust-cmd-1.3-1st-implement.md)
- [更好看的错误输出](rust-cmd-1.4-nicer-error.md)
- [输出](rust-cmd-1.5-output.md)
- [测试](rust-rust-cmd-1.6-testing.md)
- [打包和发布](rust-cmd-1.7-packaging.md)
- [有用的箱子](rust-cmd-3-userful-crates.md)
