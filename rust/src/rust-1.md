# 入门指南

[The Rust Programming Language](https://doc.rust-lang.org/book/title-page.html)
[Rust 程序设计语言](https://kaisery.github.io/trpl-zh-cn/ch00-00-introduction.html)

## 安装 rust

[安装 Rust](https://www.rust-lang.org/zh-CN/tools/install)

第一步是安装 `Rust`. 我们会通过 `rustup` 下载 `Rust`, 这是一个管理 `Rust` 版本和相关工具的命令行工具. 下载时需要联网.

```zsh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

另外, 你还需要一个`链接器`(linker), 这是 `Rust` 用来将其编译的`输出`连接到一个文件中的程序.
很可能你已经有一个了. 如果你遇到了链接器错误, 请尝试安装一个 `C 编译器`, 它通常包括一个`链接器`.
`C 编译器`也很有用, 因为一些常见的 `Rust` 包依赖于 `C 代码`, 因此需要安装 `C 编译器`.

在 `macOS` 上, 你可以通过运行以下命令获得 C 语言编译器:

```bash
$ xcode-select --install
```

根据发行版的文档, `Linux` 用户通常应该安装 `GCC` 或 `Clang`. 例如, 如果你使用 `Ubuntu`, 则可以安装 则可以安装 `build-essential` 包.

### 卸载 Rust

在任何时候如果您想卸载 `Rust`, 您可以运行 `rustup self uninstall`.

### 本地文档

安装程序也自带一份`文档`的本地拷贝, 可以离线阅读. 运行 `rustup doc` 在浏览器中查看本地文档.
任何时候, 如果你拿不准`标准库`中的`类型`或`函数`的用途和用法, 请查阅`应用程序接口`(application programming interface, API)文档!

官方教程的地址: [rust-lang/book](https://github.com/rust-lang/book/)

markdown 组织工具:

从 `markdown` 文件创建书籍. 像 `Gitbook` 一样, 但用 `Rust` 实现

[rust-lang/mdBook](https://github.com/rust-lang/mdBook)
[mdBook Documentation](https://rust-lang.github.io/mdBook/index.html)

### vscode 配置

[在VSCode配置Rust环境](https://zhuanlan.zhihu.com/p/353917073)

按照上面的官网教程, 安装好 `rust`运行时, 以及 [Rust support for Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=rust-lang.rust) 之后.

#### 构建和运行项目

有两种方式, 一种是`命令行`, 一种是利用`VSCode`提供的`任务`

通过菜单->`Terminal`, 或者快捷键->`` control +` `` 打开命令行

```bash
cargo build #构建:
cargo run #运行:
```

+ 通过任务;

`Ctrl+Shift+P` 打开命令板,输入 `Task: Configure Default Build Task`,
按照提示选择例如: `Rust: cargo build - xxxx`, 或者 `Rust: cargo run - xxxx`,
就会自动创建配置文件`.vscode/tasks.json`, 大概如下:

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "cargo build",
      "type": "shell",
      "command": "cargo build",
      "args": [],
      "group": {
        "kind": "build",
        "isDefault": true
      }
    }
  ]
}
```

#### 通过 VSCode Debug项目

+ 安装 `CodeLLDB` ; 搜索 `CodeLLDB`, 或者快捷键: `Ctrl + P`, 输入: `ext install vadimcn.vscode-lldb`, 回车执行.

> `CodeLLDB` 是一个`VSCode`本地`Debug`的扩展:  [vscode-lldb](https://github.com/vadimcn/vscode-lldb) .

`F5`或者`运行(Run) -> 开始debg(Start Debugging)`, 如果你之前没有配置过, 将会提示你创建配置文件 `launch.json`,
点击确定, 将会根据`Cargo.toml`自动创建创建`launch.json`.

进入`src/main.rs`, 在 `main` 函数的任意一段位置设置`断点`, 按下`F5`开始调试.

如果遇到 `VSCode`报错:

    Couldn"t start client Rust Language Server

先尝试重启`VSCode`, 不行的话, 再试下配置文件: `setting.json` 中添加: `"rust-client.rustupPath": "$HOME/.cargo/bin/rustup"`.

## Hello, World!

接下来, 新建一个源文件, 命名为 `main.rs`. `Rust` 源文件总是以 `.rs` 扩展名结尾.
如果文件名包含多个单词, 使用下划线分隔它们. 例如命名为 `hello_world.rs`, 而不是 `helloworld.rs`.

现在打开刚创建的 `main.rs` 文件, 输入示例 1-1 中的代码.

```rust
fn main() {
    println!("Hello, world!");
}
```

保存文件, 并回到终端窗口. 在 `Linux` 或 `macOS` 上, 输入如下命令, 编译并运行文件:

```bash
$ rustc main.rs
$ ./main
Hello, world!
```

现在, 让我们回过头来仔细看看 `Hello, world!` 程序中到底发生了什么. 这是第一块拼图:

```rust
fn main() {

}
```

这几行定义了一个 `Rust` 函数. `main` 函数是一个特殊的函数: 在可执行的 `Rust` 程序中, 它总是最先运行的代码.
第一行代码声明了一个叫做 `main` 的函数, 它没有`参数`也没有`返回值`. 如果有`参数`的话, 它们的名称应该出现在小括号中, `()`.

还须注意, `函数体`被包裹在花括号中, `{}`. `Rust` 要求所有函数体都要用`花括号`包裹起来.
一般来说, 将`左花括号`与`函数声明`置于`同一行`并以`空格`分隔, 是良好的代码风格.

如果你希望在 `Rust` 项目中保持一种标准风格, 可以使用名为 `rustfmt` 的自动格式化工具将代码格式化为特定的风格.
`Rust` 团队已经在标准的 `Rust` 发行版中包含了这个工具, 就像 `rustc`. 所以它应该已经安装在你的电脑中了! 检查在线文档以了解更多细节.

在 `main()` 函数中是如下代码:

```rust
println!("Hello, world!");
```

这行代码完成这个简单程序的所有工作: 在屏幕上打印文本.
这里有四个重要的细节需要注意. 首先 `Rust` 的缩进风格使用 `4 个空格`, 而不是 `1 个制表符(tab)`.

第二, `println!` 调用了一个 `Rust` 宏(`macro`). 如果是调用函数, 则应输入 `println`(没有`!`).
我们将在第十九章详细讨论宏. 现在你只需记住, 当看到符号 `!` 的时候, 就意味着调用的是`宏`而不是普通`函数`, 并且`宏`并不总是遵循与函数相同的规则.

第三, `"Hello, world!"` 是一个`字符串`. 我们把这个`字符串`作为一个`参数`传递给 `println!`, `字符串`将被打印到屏幕上.

第四, 该行以`分号`结尾(`;`), 这代表一个`表达式`的结束和下一个`表达式`的开始. 大部分 `Rust` 代码行以`分号`结尾.

如果你更熟悉动态语言, 如 `Ruby`, `Python` 或 `JavaScript`, 则可能不习惯将`编译`和`运行`分为两个单独的步骤.
Rust 是一种 `预编译静态类型`(ahead-of-time compiled)语言, 这意味着你可以`编译程序`, 并将可执行文件送给其他人, 他们甚至不需要安装 `Rust` 就可以运行.
如果你给他人一个 `.rb`, `.p`y 或 `.js` 文件, 他们需要先分别安装 `Ruby`, `Python`, `JavaScript` 实现(运行时环境, VM).
不过在这些语言中, 只需要一句命令就可以`编译`和`运行`程序. 这一切都是语言设计上的权衡取舍.

仅仅使用 `rustc` 编译简单程序是没问题的, 不过随着项目的增长, 你可能需要管理你项目的方方面面, 并让代码易于分享.
接下来, 我们要介绍一个叫做 `Cargo` 的工具, 它会帮助你编写真实世界中的 `Rust` 程序.

## cargo

我们使用 `Cargo` 创建一个新项目, 然后看看与上面的 `Hello, world!` 项目有什么不同.
回到 `projects` 目录(或者你存放代码的目录). 接着, 可在任何操作系统下运行以下命令:

```bash
$ cargo new hello_cargo
$ cd hello_cargo
```

第一行命令新建了名为 `hello_cargo` 的目录. 我们将项目命名为 `hello_cargo`, 同时 `Cargo` 在一个同名目录中创建项目文件.

进入 `hello_cargo` 目录并列出文件.
将会看到 `Cargo` 生成了两个文件和一个目录: 一个 `Cargo.toml` 文件, 一个 `src` 目录, 以及位于 `src` 目录中的 `main.rs` 文件.
它也在 `hello_cargo` 目录初始化了一个 `git` 仓库, 以及一个 `.gitignore` 文件.
如果你在现有的 `git` 仓库中运行 `cargo new`, 则不会生成 `git` 文件; 你可以通过使用 `cargo new --vcs=git` 来覆盖此行为.

>注意: `Git` 是一个常用的版本控制系统(version control system,  VCS).
>可以通过 `--vcs` 参数使 `cargo new` 切换到其它版本控制系统(`VCS`), 或者不使用 `VCS`.
>运行 `cargo new --help` 参看可用的选项.

### Cargo 参考

[更换 cargo 源](https://cargo.budshome.com/reference/source-replacement.html)

本文档是关于更换 `crate 索引`(注册表). 您可以阅读有关`重写依赖项`的信息, 它在本文档的`重写依赖`关系部分.

`Cargo` 支持用更换`来源`, 可根据`镜像`或 `vendoring` 依赖关系来表达`倾向`.
要配置这些, 目前通过 `.cargo/config` 配置机制完成, 像这样:

```conf
# `source` 表下, 存储要更换的源的名称
[source]

# 在`source` 表格下, 填写源的名称.
# 示例: 下面定义了一个新源,  叫 `my-awesome-source`,  其内容来自本地 `vendor`目录 , 路径相对于`.cargo/config`所在的目录
[source.my-awesome-source]
directory = "vendor"

# Git sources 也可以指定 branch/tag/rev
git = "https://example.com/path/to/repo"
# branch = "master"
# tag = "v1.0.1"
# rev = "313f44e8"

# 默认源 在 "crates-io"名称下,  在这里可以使用 `replace-with` 指示把 默认源更换成"my-awesome-source"源
[source.crates-io]
replace-with = "my-awesome-source"
```

使用此配置, `Cargo` 会尝试在 `"vendor"` 目录中, 查找`所有包`, 而不是 查询`在线注册表`( crates.io). `Cargo` 有两种更换`源`的表达 :

+ 供应(Vendoring) - 可以定义自定义源, 它们表示本地文件系统上的包. 这些源是它们正在更换的源的子集, 并在需要时可以检入包中.
+ 镜像(Mirroring) - 可以更换为等效版本的源, 行为表现为 crates.io 本身的缓存.

`Cargo` 有一个关于更换`源`的核心假设: 从两个`源`得到的`源代码`完全相同.
在上面的例子中, `Cargo` 假设所有的箱子都来自`my-awesome-source`, 并且与`crates-io`的副本完全相同.
请注意, 这也意味着不允许 `my-awesome-source`, 有 `crates-io` 源不存在的箱.

因此, `换源`不适用于依赖项补丁(fix bug), 或私有注册表等情况.
`Cargo` 通过使用 `[replace]` 字段支持`依赖项补丁`, 计划在未来版本的 `Cargo` 支持 `私人注册表`.

### 配置

更换源的配置通过完成 `.cargo/config`, 下面为全套可用字段:

```conf
# 每个源都有自己的表格, 名称即表名
[source.the-source-name]

# 命令 , `the-source-name` 会被`another-source`取代
replace-with = "another-source"

# 有几种可用的源定义(接下来有所描述)
registry = "https://example.com/path/to/index"
local-registry = "path/to/registry"
directory = "path/to/vendor"
```

`crates-io` 即在线注册表(箱的默认来源), 可以更换为:

```conf
[source.crates-io]
replace-with = 'another-source'
```

### 注册表源

[Rust crates.io 索引镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/crates.io-index.git/)

`注册表源` 与 `crates.io` 本身相同.
也就是说, 它也有一个在 `git` 存储库中提供的`索引`, 该存储库匹配 `crates.io index`的格式.
然后该`存储库`的配置可以指示从哪里下载`包`.

目前还没有设置 `crates.io` 的镜像的可用项目. 请继续关注!

>以下配置由[芽之家书馆](https://budshome.com/books.html)搜集验证, 并持续更新 --
>目前国内 `cargo` 镜像源有: 中国科学技术大学源, 上海交通大学源, 清华大学源, 以及 rustcc 社区源.
>自定义 `cargo` 源可以是全局的, 也可以对每个 工程 因地制宜.
> 创建 `$HOME/.cargo/config` 文件(各操作系统及版本均大致相同), 然后在 `config` 文件内写入下述配置内容.
>其中协议推荐使用 `git`, 但对于 `https` 和 `git` 协议, 一般各镜像源都支持, 并且是可以互换的.
>如果你所处的环境中不允许使用 `git` 协议, 或者配置 `git` 协议后不能正常获取和编译 `crate`, 可以换 `https` 协议再试试.

```conf
[source.crates-io]
registry = "https://github.com/rust-lang/crates.io-index"
# 指定镜像
replace-with = '镜像源名' # 如: tuna, sjtu, ustc, 或者 rustcc

# 注: 以下源配置一个即可, 无需全部
# 中国科学技术大学
[source.ustc]
registry = "https://mirrors.ustc.edu.cn/crates.io-index"
# >>> 或者 <<<
registry = "git://mirrors.ustc.edu.cn/crates.io-index"
# 上海交通大学
[source.sjtu]
registry = "https://mirrors.sjtug.sjtu.edu.cn/git/crates.io-index/"
# 清华大学
[source.tuna]
registry = "https://mirrors.tuna.tsinghua.edu.cn/git/crates.io-index.git"
# rustcc社区
[source.rustcc]
registry = "https://code.aliyun.com/rustcc/crates.io-index.git"
```

或者在工程目录中, 在和  `Cargo.toml` 同目录的 `.cargo` 文件夹下创建 `config` 文件, `config` 文件配置方法和内容同上.

### 本地注册表源

`本地注册表源` 旨在成为另一个注册表源的子集, 但可在本地文件系统(也称为 `vendoring`)上使用.
本地注册表是提前下载, 通常与一个 `Cargo.lock` 同步, 并由一组 `*.crate` 文件和像普通注册表一样的索引组成.

管理和创建本地注册表源的主要方法是通过 `cargo-local-registry` 子命令, 可在 `crates.io` 上找到, 并用cargo install cargo-local-registry安装.
本地注册表包含在一个目录, 其中包含许多从 crates.io 下载的*.crate文件, 以及index目录, 它与 crates.io-index 项目目录具有相同格式(仅填充有存在的 crates).

### 目录源

`目录源` 类似于本地注册表源, 其中包含本地文件系统上许多的可用包, 适用于 `vendoring` 依赖项.
与本地注册表一样, 目录源主要由外部子命令管理 `cargo-vendor`, 可用 `cargo install cargo-vendor` 安装.

目录源与本地注册表不同, 但它们包含 `*.crate` 文件的解压缩版本, 使其在某些情况下, 更适合检查所有内容到源代码控制工具.
目录源只是一个包含许多其他目录的目录, 其中包含 crates 的源代码(解压缩版本的 `*.crate` 文件).
目前, 对每个目录的名称没有限制.

目录源中的每个包也有一个关联的元数据文件, 指示包中每个文件的校验和, 以防止意外修改.
