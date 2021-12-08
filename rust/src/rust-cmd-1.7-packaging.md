# 打包和发布一个Rust工具

如果你觉得你的程序已经可以供其他人使用了, 那么现在就是打包和发布的时候了!

有几种方法, 我们将从 "设置最快捷 "到 "对用户最方便" 这三个方面来看.

## 最快: cargo publish

[crates.io]: https://crates.io/
[crates.io 账户]: https://crates.io/me
[发布指南]: https://doc.rust-lang.org/1.39.0/cargo/reference/publishing.html
[]: https://doc.rust-lang.org/1.39.0/cargo/reference/manifest.html

发布你的应用程序最简单的方法是使用 `cargo`. 你还记得我们是如何向我们的项目添加外部依赖的吗?
`Cargo` 从其默认的 "crate注册表" ([crates.io][])中下载了它们.
有了 `cargo publish`, 你也可以把 `crate` 发布到 `crates.io`. 这适用于所有的 `crate`, 包括那些有二进制目标的 `crate`.

向 `crates.io` 发布一个 crate 是非常直接的. 如果你还没有, 请在 [cates.io][] 上创建一个账户.
目前, 这是在 `GitHub` 上通过授权完成的, 所以你需要有一个 `GitHub` 账户(并在那里登录).
接下来, 你要在你的本地机器上使用 `cargo` 登录. 为此, 去你的 [crates.io 账户][] 页面, 创建一个新的令牌, 然后运行 `cargo login <your-new-token>`.
在每台电脑上你只需要做一次这样的操作. 你可以在 `cargo` 的 [发布指南][] 中了解更多这方面的信息.

现在 `cargo` 和 `crates.io` 都认识你了, 你可以准备发布 `crates` 了.
在你匆匆忙忙地去发布新的 `crate`(版本)之前, 最好再一次打开你的 `Cargo.toml`, 确保你添加了必要的 `元数据`(metadata).
你可以在 [cargo's manifest format][] 的文档中找到所有可以设置的字段. 下面是一些常见条目的简要介绍.

```toml
[package]
name = "grrs"
version = "0.1.0"
authors = ["Your Name <your@email.com>"]
license = "MIT OR Apache-2.0"
description = "A tool to search files"
readme = "README.md"
homepage = "https://github.com/you/grrs"
repository = "https://github.com/you/grrs"
keywords = ["cli", "search", "demo"]
categories = ["command-line-utilities"]
```

注意: 这个例子包括强制(mandatory)字段: license, 有一个 `Rust` 项目的共同选择, 与编译器本身使用的许可相同.
它还应该包含一个 `README.md` 文件.
它应该包括对你的项目内容的 `快速描述`, 不仅会包含在你的 `crate.io` 页面上, 也会包含 `GitHub` 在仓库页面上默认显示的内容.

## 如何从 crates.io 安装二进制文件

我们已经看到了如何将一个 crate 发布到 `crates.io`, 你可能想知道如何安装它.
当你运行 `cargo build`(或类似的命令)时, `cargo` 会为你下载并编译库, 与 `库` 不同的是, 你需要告诉它明确安装二进制文件.

这可以用 `cargo install <crate-name>` 来完成.
默认情况下, 它将下载 `crate`, 编译其中包含的所有二进制目标(在 "release" 模式下, 所以可能需要一些时间),
并将它们复制到 `~/.cargo/bin/` 目录中. (请确保你的 `shell` 知道在那里寻找二进制文件! ).

也可以从 `git 仓库`安装装箱, 或者只安装某个 crate 的特定二进制文件, 或指定另一个目录来安装它们.
请看 `cargo install --help` 以了解详情.

### 何时使用它

`cargo install` 是一种安装二进制 crate 的简单方法. 它对Rust开发者来说非常方便, 但也有一些明显的缺点.
由于它总是从头开始编译你的源代码, 你的工具的用户需要在他们的机器上安装 `Rust`, `cargo` 以及你的项目需要的所有其他系统依赖.
编译大型 `Rust` 代码库也会花费一些时间.

最好把它用于发布供其他 Rust 开发者使用的工具.
比如说: 很多 cargo 子命令, 如 `cargo-tree` 或 `cargo-outdated` 都可以用它来安装.

## 分发二进制文件

Rust是一种可以编译成 `native 代码` 的语言, 默认情况下静态链接所有的依赖关系.
当你在包含 `grrs` 二进制文件的项目上运行 `cargo build` 时, 你会得到一个名为 `grrs` 的二进制文件.
试试吧: 使用 `cargo build`, 它将生成 `target/debug/grrs`, 而当你运行 `cargo build --release`, 它将生成 `target/release/grrs`.

除非你使用了明确依赖目标系统上的外部库的 `crate`(比如使用系统上的 `OpenSSL`), 否则这个二进制文件将只依赖于普通的 system libraries.
这意味着, 你拿着这一个文件, 把它发送给和你运行相同操作系统的人, 他们就能运行它.

这已经非常强大了, 它解决了我们刚才看到的 `cargo install` 的两个缺点.
不需要在用户的机器上安装 `Rust`, 也不需要花一分钟的时间来编译, 他们可以立即运行二进制文件.

所以, 正如我们所见, `cargo build` 已经为我们构建了二进制文件. 唯一的问题是, 这些二进制文件不能保证在所有平台上都能运行.
如果你在 `Windows` 机器上运行 `cargo build`, 你不会得到一个默认能在 `Mac` 上运行的二进制文件.
有没有办法为所有感兴趣的平台自动生成这些二进制文件?

### 在CI上构建二进制版本分发

[Travis]: https://travis-ci.com
[适用于Linux和macOS]: https://github.com/rustwasm/wasm-pack/blob/51e6351c28fbd40745719e6d4a7bf26dadd30c85/.travis.yml#L74-L91
[适用于Windows]: https://github.com/rustwasm/wasm-pack/blob/51e6351c28fbd40745719e6d4a7bf26dadd30c85/.appveyor.yml
[trust]: https://github.com/japaric/trust
[cross]: https://github.com/rust-embedded/cross

如果你的工具是开源的, 并且托管在 `GitHub` 上, 那么很容易建立一个免费的 `CI`(持续集成)服务, 比如 [Travis CI][].
(也有其他服务可以在其他平台上使用, 但Travis是非常流行的).
简单说就是, 在你每次推送修改到你的版本库时, 它在虚拟机中运行 setup commands.
这些命令的具体内容, 以及它们运行在什么类型的机器上, 是可以配置的.
比如说: 在安装了 `Rust` 和一些常用构建工具的机器上运行 `cargo test`. 如果失败了, 你就知道最近的修改有问题了.

我们也可以用它来构建二进制文件, 并将其上传到 `GitHub` 上!
的确, 如果我们运行 `cargo build --release` 并将二进制文件上传到某个地方, 我们应该就可以了, 对吗? 并非如此.
我们仍然需要确保我们构建的二进制文件能与尽可能多的系统兼容.
例如, 在 `Linux` 上, 我们可以不为当前系统编译, 而是为 `x86_64-unknown-linux-musl` 目标编译, 以不依赖默认的系统库.
在 `macOS` 上, 我们可以将 `MACOSX_DEPLOYMENT_TARGET` 设置为 `10.7`, 以便只依赖 `10.7` 及以前版本的系统特性.

使用这种方法构建二进制文件的例子: [适用于Linux和macOS][], [适用于Windows][] (使用AppVeyor).

另一种方法是使用 `预先构建`(Docker)镜像, 其中包含我们构建二进制文件所需的所有工具.
这让我们也可以轻松地针对更多的 exotic 平台.
[trust][] 项目包含了可以囊括到你的项目中的脚本, 以及关于如何设置的说明. 它还可以使用 AppVeyor 支持 Windows.

如果你更喜欢在本地设置, 并在自己的机器上生成发布文件, 仍然可以看看 `trust`.
它在内部使用 [cross][], 其工作原理与 `cargo` 类似, 但将命令转发给 `Docker` 容器内的 `cargo` 进程.
镜像的定义也可以在 [cross的存储库](https://github.com/rust-embedded/cross)中找到.

### 如何安装这些二进制文件

把你的用户引导至发布页面, 看起来[像这样的页面](https://github.com/rustwasm/wasm-pack/releases/tag/v0.5.1), 他们可以下载我们刚刚创建的工件.
我们刚刚生成的发布工件并不特别. 在最后, 它们只是包含我们的二进制文件的归档文件.
这意味着用户可以用浏览器下载, 提取(通常会自动发生), 并把二进制文件复制到他们喜欢的地方.

这确实需要一些手动 `安装` 程序的经验, 所以你要在你的 `README` 文件中添加一个关于如何安装这个程序的部分.

注意: 如果你用 [trust][] 来构建你的二进制文件, 并把它们添加到 `GitHub` 发布中,
如果你认为命令行更方便, 你也可以告诉人们运行

```bash
curl -LSfs https://japaric.github.io/trust/install.sh | sh -s -- --git your-name/repo-name
```

### 何时使用它

一般来说, 拥有二进制版本是个好主意, 几乎没有任何坏处.
它不能解决用户必须手动安装和更新你的工具的问题, 但他们可以快速获得最新的发布版本, 而不需要安装 `Rust`.

### 除了你的二进制文件之外, 还应该打包什么

现在, 当用户下载我们的发布版构建时, 他们会得到一个只包含二进制文件的 `.tar.gz` 文件.
因此, 在我们的例子项目中, 他们将只得到一个可以运行的 `grrs` 文件.
但还有一些文件是我们在仓库中已经有的, 他们可能想拥有.
例如, 告诉他们如何使用这个工具的 `README` 文件, 以及许可证文件.
由于我们已经有了这些文件, 所以它们很容易被添加.

不过, 还有一些更有趣的文件, 特别是对命令行工具而言:
除了 `README` 文件之外, 我们还可以发送一个 `手册页`, 以及为你的 `shell` 添加可能的标志的配置文件, 怎么样?
你可以手工编写这些文件, 但我们使用的参数解析库(`structopt` 基于此)`clap`有办法为我们生成所有这些文件.
更多细节请见[本章的深入介绍](https://rust-cli.github.io/book/in-depth/docs.html).

## 将你的应用程序放入软件包库

[Formula file]: https://github.com/BurntSushi/ripgrep/blob/31adff6f3c4bfefc9e77df40871f2989443e6827/pkg/brew/ripgrep-bin.rb

到目前为止, 我们看到的两种方法都不是你通常在机器上安装软件的方式.
特别是命令行工具, 你在大多数操作系统上使用 `全局软件包管理器` 来安装.
对用户来说, 好处是相当明显的: 如果你的程序可以和他们安装其他工具的方式一样, 就不需要考虑如何安装.
这些软件包管理器还允许用户在有新版本的时候更新他们的程序.

可悲的是, 支持不同的系统意味着你必须要看这些不同的系统是如何工作的.
对于某些系统来说, 这可能就像向你的仓库添加一个文件一样简单(比如为 `macOS` 的 `brew` 添加[Formula file][]),
但对于其他系统, 你往往需要自己发送 patches, 并将你的工具添加到他们的仓库中.
有一些有用的工具, 如 `cargo-rpm`, `cargo-deb` 和 `cargo-aur`,
但描述它们如何工作, 以及如何为这些不同的系统正确打包你的工具已经超出了本章的范围.

相反, 让我们来看看一个用 `Rust` 编写的工具, 它可以在许多不同的软件包管理器中使用.

### 一个例子: ripgrep

[README中的"安装"部分]: https://github.com/BurntSushi/ripgrep/tree/31adff6f3c4bfefc9e77df40871f2989443e6827#installation

`ripgrep` 是grep/ack/ag的替代品, 用Rust编写.
它相当成功, 并被打包到许多操作系统中. 看看它的 [README中的"安装"部分][] 就知道了.

注意, 它列出了几个不同的安装选项:
首先它提供了一个包含二进制文件的 `GitHub` 版本的链接, 所以你可以直接下载它们;
然后它列出了如何使用一堆不同的软件包管理器来安装它;
最后, 你也可以使用 `cargo install` 来安装它.

一个好的开端是: 不要徘徊在选择哪个方法上, 而是从 `cargo install` 开始, 添加二进制版本, 最后开始使用 `系统包管理器` 分发你的工具.
