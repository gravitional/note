# 进一步认识 Cargo 和 Crates.io

目前为止我们只使用过 Cargo 构建, 运行和测试代码这些最基本的功能, 不过它还可以做到更多.
本章会讨论 Cargo 其他一些更为高级的功能, 我们将展示如何:

+ 使用发布配置来自定义构建
+ 将库发布到 [crates.io](https://crates.io/)
+ 使用工作空间来组织更大的项目
+ 从 `crates.io` 安装二进制文件
+ 使用自定义的命令来扩展 `Cargo`

Cargo 的功能不止本章所介绍的, 关于其全部功能的详尽解释, 请查看 [文档](doc.rust-lang.org/cargo/)

## cargo test 打印输出

[rust的单元测试cargo test显示println!宏的打印信息](https://blog.csdn.net/lizhe_dashuju/article/details/108737366)

cargo test运行单元测试, 默认不显示输出信息,
因此 `println!` 的信息也不显示, 如果要显示,
需要使用

```bash
cargo test -- --nocapture
```
