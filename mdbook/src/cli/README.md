# 命令行工具

`mdBook` 既可以作为一个命令行工具, 也可以作为一个 [Rust crate](https://crates.io/crates/mdbook)使用.
让我们先关注一下命令行工具的功能.

## 从二进制文件安装

预编译的二进制文件, 是在尽力而为的基础上为主要平台提供的.
请访问[发布页面](https://github.com/rust-lang/mdBook/releases), 下载适合你的平台的版本.

## 从源代码安装

`mdBook` 也可以通过在你的本地机器上`编译源代码`来安装.

### 前提条件

`mdBook` 是用 [Rust](https://www.rust-lang.org/) 编写的, 因此需要用 `Cargo` 编译.
如果你还没有安装Rust, 请现在就去[安装它](https://www.rust-lang.org/tools/install).

### 安装Crates.io版本

如果你已经安装了 `Rust` 和 `Cargo`, 安装 `mdBook` 就相对容易了. 你只需要在你的终端输入这个片段.

```bash
cargo install mdbook
```

这将从 `Crate.io` 获取最新版本的源代码并进行编译. 你必须将 `Cargo` 的 `bin` 目录添加到你的`PATH`中.

在你的终端运行`mdbook help`来验证它是否工作. 恭喜你, 你已经安装了`mdBook`!

### 安装Git版本

[git版本](https://github.com/rust-lang/mdBook)包含了所有最新的 `bug-fix` 和功能, 这些功能将在 `Crates.io` 上发布.
如果你不能等到下一个版本,  你可以自己构建 `git` 版本.

打开你的终端, 导航到你选择的目录. 我们需要克隆` git`仓库, 然后用 `Cargo` 构建它.

```bash
git clone --depth=1 https://github.com/rust-lang/mdBook.git
cd mdBook
cargo build --release
```

可执行的 `mdbook` 将在 `./target/release` 文件夹中, 这应该被添加到`PATH`中.
