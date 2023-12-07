# rust 构建脚本

[Build Scripts](https://doc.rust-lang.org/cargo/reference/build-scripts.html#rustc-link-arg)

在 `Cargo.toml` 中添加

```toml
[package] #在 package 字段添加
...
build = "build.rs"
```

在 package 根目录添加 `build.rs`,
使用如下语句指定 传递给 `link.exe`的参数,
以及 `库的搜索目录`,

```rs
fn main() {
    println!("cargo:rustc-link-arg=liblapack.lib");
    println!("cargo:rustc-link-args=libblas.lib");
    println!("cargo:rustc-link-search=native=c:/cppLibs/LAPACK-openBLAS-ucrt/lib");
}
```

参考 [build-scripts: rustc-link-arg](https://doc.rust-lang.org/cargo/reference/build-scripts.html#rustc-link-arg)
[link-arg](https://doc.rust-lang.org/rustc/codegen-options/index.html?highlight=link-args#link-arg)
[link-args](https://doc.rust-lang.org/rustc/codegen-options/index.html?highlight=link-args#link-args)
This flag lets you append multiple extra arguments to the linker invocation. The options should be separated by spaces.

`link-arg`;
此标记可让您在链接器调用中 append 一个额外参数.
`append` 这点很重要;
您可以多次传递此标志以追加多个参数.

`link-args`
此标记允许您在链接器调用中 append 多个额外参数.
选项之间应以空格分隔.

因此, 手动指定 rustc 参数的命令行如下

```bash
cargo rustc --bin rust_test -- -C link-args='c:\cppLibs\LAPACK-openBLAS-ucrt\lib\liblapack.lib c:\cppLibs\LAPACK-openBLAS-ucrt\lib\libblas.lib' --print link-args
```

其中编译目标是 `rust_test`, 要链接的库是 `liblapack.lib`, `libblas.lib`,
`--print link-args` 打印传递给 `link.exe`(Win平台)的具体命令, 用于查错

如果用到的 lib 已经加入

```bash
cargo rustc --bin rust_test -- -C link-args='liblapack.lib libblas.lib' --print link-args
```
