# Cargo 自定义扩展命令

`Cargo` 的设计使得开发者可以通过新的 `子命令` 来对 `Cargo` 进行扩展, 而无需修改 `Cargo` 本身.

如果 `$PATH` 中有类似 `cargo-something` 的二进制文件, 就可以通过 `cargo something` 来像 `Cargo` 子命令一样运行它.
像这样的自定义命令也可以运行 `cargo --list` 来展示出来.

能够通过 `cargo install` 向 `Cargo` 安装扩展, 并可以如内建 `Cargo` 工具那样运行, 是 `Cargo` 设计上的一个非常方便的优点!

## 总结

通过 `Cargo` 和 `crates.io` 来分享代码, 是使得 Rust 生态环境可以用于许多不同的任务的重要组成部分.
`Rust` 的标准库是小而稳定的, 不过 `crate` 易于分享和使用, 并采用一个不同于语言自身的时间线来提供改进.

不要羞于在 `crates.io` 上共享对你有用的代码; 因为它很有可能对别人也很有用!
