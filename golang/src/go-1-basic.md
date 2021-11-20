# GoLang

[Go 语言教程](https://www.runoob.com/go/go-environment.html)

[Golang Documentation ](https://golang.google.cn/doc/)

## 安装

+ 先下载 [Go的二进制包] (https://golang.google.cn/dl/)

如果您安装了以前的 `Go` 版本, 请确保在安装另一个版本之前将其删除.

### 安装到全局

下载`archive`并将其解压缩到`/usr/local`, 在`/usr/local/go` 处创建`Go tree`.
例如, 以`root`用户或通过`sudo`运行以下命令:
> 重要提示: 在解压之前, 此步骤将删除之前在 `/usr/local/go` 的旧安装, 如果存在的话. 请在继续进行之前备份任何数据.

例如, 以 `root` 身份或通过 `sudo` 运行以下程序.

```bash
rm -rf /usr/local/go && tar -C /usr/local -xzf go1.14.3.linux-amd64.tar.gz
```

+ 将`/usr/local/go/bin`添加到`PATH`环境变量中.
    您可以通过将以下行添加到`$HOME/.profile`或 `/etc/profile` 中(对于系统范围的安装)来执行此操作:

    ```bash
    export PATH=$PATH:/usr/local/go/bin
    ```

注意: 对配置文件的更改可能要等到下一次登录计算机后才能应用.
要立即应用更改, 只需直接在`shell`中运行命令, 或使用`source $HOME/.profile`之类的命令, 从配置文件中执行它们即可.

+ 通过打开命令提示符并键入以下命令来验证是否已安装`Go`:

    ```bash
    $ go version
    ```

    确认命令打印出已安装的`Go`版本.

### Go 环境

[Go 环境](https://studygolang.gitbook.io/learn-go-with-tests/go-ji-chu/install-go#go-huan-jing)

Go 有一套特别的惯例, 所有 Go 代码都存在于一个`工作区`(文件夹)中.
这个工作区可以在你的机器的任何地方. 如果你不指定, `Go` 将假定 `$HOME/go` 为默认工作区.
工作区由环境变量 [GOPATH][] 标识并修改.

你应该设置环境变量, 便于以后可在`脚本`或 `shell` 中使用它.
在你的 `.bash_profile`(`.zshrc`) 中添加以下导出语句:

```bash
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
```

>注意: 你应该打开一个新的 `shell` 来使这些变量生效, 或运行 `. ~/.bash_profile`.

`Go` 假设你的工作区包含一个特定的目录结构.
`Go` 把文件放到三个目录中: 所有源代码位于 `src`, 包对象位于 `pkg`, 编译好的程序位于 `bin`.
你可以参照以下方式创建目录.

```bash
mkdir -p $GOPATH/src $GOPATH/pkg $GOPATH/bin
```

此时, 你可以用 `go get`, 它会把下载到的资源按 `src/pkg/bin` 结构正确安装在相应的 `$GOPATH/xxx` 目录中.

[GOPATH]: https://golang.org/cmd/go/#hdr-GOPATH_environment_variable)

### Go 编辑器

编辑器是可根据个人口味定制化的, 可能你的编辑器已经配置了 Go 的支持.
如果还没有, 你可以考虑使用 [vscode](https://code.visualstudio.com/) 这类对 Go 有良好支持的编辑器.

默认 VS Code 只自带了少量功能集成, 你可以通过安装扩展以集成更多功能.
VS Code 有大量这类扩展, 其中一个很棒的扩展是 [Go Team at Google](https://marketplace.visualstudio.com/items?itemName=golang.go)
可通过以下方法安装:
打开 `VS Code Quick Open` (Ctrl+P), 粘贴以下命令, 回车:

```code
ext install golang.Go
```

当你第一次用 VS Code 打开 Go 文件时, 它会提示缺少分析工具, 点击按钮来安装它们.
由 VS Code 安装(和使用)的工具列表可在[这里](https://github.com/golang/vscode-go/blob/master/docs/tools.md)找到.

### Go 调试

在 VS Code 中调试 Go 代码建议使用 [Delve](https://github.com/go-delve/delve).
在 Go 版本 1.16 及以上, 可以用以下命令安装:

```bash
$ go install github.com/go-delve/delve/cmd/dlv@latest
```

老版本可以通过以下 go get 命令安装:

```bash
go get -u github.com/go-delve/delve/cmd/dlv
```

### Go 语法检查

对默认的语法检查进行增强可以用 [golangci/golangci-lint](https://github.com/golangci/golangci-lint),
[可通过以下方式安装](https://golangci-lint.run/usage/install/#local-installation):

```bash
# Go 1.16+
go install github.com/golangci/golangci-lint/cmd/golangci-lint@v1.43.0

# Go version < 1.16
go get -u github.com/golangci/golangci-lint/cmd/golangci-lint@v1.43.0
```

### 重构和工具

[本书的重心][] 在于重构的重要性. 好的工具可以帮你放心地进行大型的重构.
你应该足够熟悉你的编辑器, 以便使用简单的组合键执行以下操作:

+ 提取/内联变量. 能够找出魔法值(magic value)并给他们一个名字可以让你快速简化你的代码.
+ 提取方法/功能. 能够获取一段代码并提取函数/方法至关重要.
+ 改名. 你应该能够自信地对多个文件内的符号批量重命名.
+ 格式化. Go 有一个名为 go fmt 的专有格式化程序. 你的编辑器应该在每次保存文件时都运行它.
+ 运行测试. 毫无疑问, 你应该能够做到以上任何一点, 然后快速重新运行你的测试, 以确保你的重构没有破坏任何东西.

另外, 为了对你处理代码更有帮助, 你应该能够:

+ 查看函数签名 - 在 Go 中调用某个函数时, 你应该了解并熟悉它. 你的 IDE 应根据其文档, 参数以及返回的内容描述一个函数.
+ 查看函数定义 - 如果仍不清楚函数的功能, 你应该能够跳转到源代码并尝试自己弄清楚.
+ 查找符号的用法 - 能够查看被调用函数的上下文可以在重构时帮你做出决定.

运用好你的工具将帮助你专注于代码并减少上下文切换.

[本书的重心]: https://studygolang.gitbook.io/learn-go-with-tests/go-ji-chu/install-go#go-huan-jing

### helloword

[第一个 Go 程序](https://www.runoob.com/go/go-tutorial.html)

保存到`hello.go`:

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, World!")
}
```

要执行 `Go` 语言代码可以使用 `go run` 命令.

执行以上代码输出:

```bash
$ go run hello.go
Hello, World!
```

此外我们还可以使用 `go build` 命令来生成二进制文件:

```bash
$ go build hello.go
$ ./hello
```

## 设置代理

[设置go get 国内镜像](https://blog.csdn.net/y1534414425/article/details/107632750)
[Go 国内加速镜像](https://learnku.com/go/wikis/38122)
[Goproxy.cn](https://goproxy.cn/)

### Goproxy

Go 1.13 及以上(推荐), 打开你的终端并执行

```bash
$ go env -w GO111MODULE=on
$ go env -w GOPROXY=https://goproxy.cn,direct
```

macOS 或 Linux, 在配置文件添加:

```bash
export GO111MODULE=on
export GOPROXY=https://goproxy.cn,direct
```

+ Windows

打开你的 PowerShell 并执行

```powershell
$env:GO111MODULE = "on"
$env:GOPROXY = "https://goproxy.cn"
```

或者新建如上环境变量.

### 其他镜像

+ 阿里云:  https://mirrors.aliyun.com/goproxy
+ 微软:  https://goproxy.io
+ 七牛云:  https://goproxy.cn
+ GoCenter:  https://gocenter.io

## 从源代码安装Go

[Installing Go from source](https://golang.google.cn/doc/install/source#go14)

本主题描述了如何从源代码构建和运行 Go.
要使用安装程序进行安装, 请参阅[下载和安装][]

[下载和安装]: (https://golang.google.cn/doc/install)

### 简介

Go 是一个开源项目, 在 [BSD 风格的许可证](https://golang.google.cn/LICENSE)下发布.
本文件解释了如何检出源码, 在自己的机器上构建源码, 并运行它们.

大多数用户不需要这样做, 他们会按照[下载和安装]中的描述, 从预编译的二进制包中进行安装, 这个过程要简单得多.
如果你想帮助开发这些预编译包中的内容, 请继续阅读.

有两个官方Go编译器工具链. 本文档主要介绍 `gc` Go 编译器和工具.
关于如何在 `gccgo` 上工作的信息, 这是一个使用 `GCC` 后端的更传统的编译器,
请参阅[设置和使用gccgo]https://golang.google.cn/doc/install/gccgo().

`Go` 编译器支持以下指令集.

+ `amd64, 386`; x86指令集, 64位和32位.
+ `arm64, arm`; ARM指令集, 64位(AArch64)和32位.
+ `mips64, mips64le, mips, mipsle` ; MIPS指令集, 大, 小二进制, 64位和32位.
+ `ppc64, ppc64le`; 64位PowerPC指令集, big-和 little-endian.
+ `riscv64` ; 64位RISC-V指令集.
+ `s390x` ; IBM z/Architecture.
+ `wasm` ; [WebAssembly](https://webassembly.org/).

编译器可以针对 `AIX`, `Android`, `DragonFly BSD`, `FreeBSD`, `Illumos`, `Linux`, `macOS/iOS (Darwin)`,
`NetBSD`, `OpenBSD`, `Plan` 9, `Solaris`, 和 `Windows` 操作系统(尽管不是所有操作系统都支持所有架构).

+ 被认为是 "第一类 "的端口列表可以在[第一类端口wiki](https://golang.google.cn/wiki/PortingPolicy#first-class-ports)页面上找到.
+ 支持的全部组合在下面关于[环境变量](https://golang.google.cn/doc/install/source#environment)的讨论中列出.
+ 关于[整个系统的要求](https://golang.google.cn/doc/install#requirements), 见主安装页面.
    以下额外的限制适用于只能从源代码构建的系统:
    对于 `PowerPC` 64 位的 `Linux`, 最小支持的内核版本是 2.6.37, 这意味着在这些系统上, Go 不支持 CentOS 6.

### 安装用于 bootstrap 的 Go 编译器二进制文件

`Go` 工具链是用 `Go` 编写的. 要构建它, 你需要安装一个 `Go` 编译器.
对工具进行初始构建的脚本会在 `$PATH` 中寻找 `go` 命令,
所以只要你的系统中安装了 `Go` 并在 `$PATH` 中配置了 `Go`, 你就可以从源代码构建 `Go`.
或者如果你愿意, 你可以将 `$GOROOT_BOOTSTRAP` 设置为 `Go` 安装的根目录, 用来构建新的 `Go` 工具链;
`$GOROOT_BOOTSTRAP/bin/go` 将是要使用的 `go` 命令.

有四种可能的方法来获得引导工具链.

+ 下载Go的最新二进制版本.
+ 使用已安装 `Go` 的系统交叉编译(cross-compile)一个工具链.
+ 使用 `gccgo`.
+ 从 Go 1.4 编译一个工具链, 这是最后一个用 C 语言编写的 Go 编译器.

这些方法详见下文.

### 从二进制版本启动工具链

要使用二进制发布版作为 bootstrap 工具链, 请参阅[下载页面][] 或使用任何其他打包的 Go 发行版.

[下载页面]: https://golang.google.cn/dl/

### 从交叉编译的源代码中启动工具链

要从源码交叉编译引导 bootstrap 链, 这在 Go 1.4 不针对(target)的系统(例如 `linux/ppc64le`)上是必要的,
在不同的系统上安装 `Go` 并运行 [bootstrap.bash](https://golang.google.cn/src/bootstrap.bash).

当以下列方式运行时(例如)

```bash
$ GOOS=linux GOARCH=ppc64 ./bootstrap.bash
```

`bootstrap.bash` 为 `GOOS/GOARCH` 的组合交叉编译一个工具链, 将生成的树放在 `../../go-${GOOS}-${GOARCH}-bootstrap`.
该树可以被复制到指定目标类型的机器上, 并作为 `GOROOT_BOOTSTRAP` 来 bootstrap 本地 build.

### 使用gccgo引导工具链

要使用 `gccgo` 作为引导工具链, 你需要安排 `$GOROOT_BOOTSTRAP/bin/go` 作为 `gccgo 5` 中的go工具.
例如, 在 Ubuntu Vivid 上.

```bash
$ sudo apt-get install gccgo-5
$ sudo update-alternatives --set go /usr/bin/go-5
$ GOROOT_BOOTSTRAP=/usr ./make.bash
```

### 从C源码建立引导工具链

要从C源代码构建一个 bootstrap 工具链, 可以使用 `git` 分支 `release-branch.go1.4`
或 [`go1.4-bootstrap-20171003.tar.gz`](https://dl.google.com/go/go1.4-bootstrap-20171003.tar.gz),
其中包含 `Go 1.4` 的源代码和累积的修正, 以保证工具在新的操作系统上运行.
(Go 1.4是最后一个用C语言编写工具链的发行版. )

解压 Go 1.4源代码后, `cd` 到 `src` 子目录,
在环境中设置 `CGO_ENABLED=0`, 然后运行 `make.bash` (或者, 在 `Windows` 中, `make.bat`).

一旦 Go 1.4 源代码被解压到你的 `GOROOT_BOOTSTRAP` 目录中,
你必须将这个 `git` 克隆实例 checkout 到分支 `releas-branch.go1.4`.
特别是, 不要试图在后面的 `fetch the repository` 步骤中重复使用这个 `git` 克隆.
`go1.4 bootstrap` 工具链必须能够正确地遍历(tranverse) `go1.4` 的源代码, 它假定这些源代码存在于这个版本库根目录下.

请注意, `Go 1.4` 并不像后来的 `Go` 版本那样可以在所有系统上运行.
特别是, `Go 1.4` 不支持当前版本的 `macOS`. 在这样的系统上, 必须使用其他方法获得 bootstrap 工具链.

### 如果需要, 安装Git

要执行下一个步骤, 你必须安装 `Git`. (在继续进行之前, 请检查你是否有 `git` 命令).

如果你没有安装好 `Git`, 请按照 [Git下载页面](https://git-scm.com/downloads)上的说明进行操作.

### (可选)安装一个C语言编译器

要建立一个支持 [cgo](https://golang.google.cn/cmd/cgo/) 的 `Go` 安装,
即允许 `Go` 程序导入 `C` 库, 必须先安装一个 `C` 编译器, 如 `gcc` 或 `clang`.
使用系统上的任何标准安装方法进行安装.

要在不使用 `cgo` 的情况下进行编译, 在运行 `all.bash` 或 `make.bash` 之前设置环境变量 `CGO_ENABLED=0`.

### 获取资源库

换到你打算安装 `Go` 的目录, 并确保 `goroot` 目录不存在.
然后克隆版本库并查看最新的发布标签(例如go1.12).

```bash
git clone https://github.com/golang/go.git goroot
# 也可以使用国内镜像 https://gitee.com/mirrors/go
$ cd goroot
$ git checkout <tag
```

其中 `<tag>` 是 发行版(release) 的版本字符串.

`Go` 将被安装在被检出的目录中.
例如, 如果 `Go` 被检出在 `$HOME/goroot`, 可执行文件将被安装在 `$HOME/goroot/bin`.
该目录可以有任何名称, 但请注意, 如果 `Go` 被检出在 `$HOME/go`, 它将与 `$GOPATH` 的默认位置冲突.
参见下面的 [GOPATH](https://golang.google.cn/doc/install/source#gopath).

>提醒一下: 如果你选择同时从源码编译 `bootstrap` 二进制文件(在前面的章节中),
此时你仍然需要再次 `git clone`( 来检出最新的 `<tag>`), 因为你必须保持你的 `go1.4` 仓库的独立性(distinct).

### (可选)切换到主分支

如果你打算修改 `go` 源代码, 并把你的修改贡献给项目,
那么就把你的仓库从 `release` tag 上移到 `master`(开发)分支上. 否则, 跳过这一步.

```bash
$ git checkout master
```

### 安装 Go

要构建 `Go` 发行版, 请运行

```bash
$ cd src
$ ./all.bash
```

(要在Windows下构建, 请使用 `all.bat`. )
如果一切顺利, 它将完成打印输出, 如.

```log
所有测试通过

---
在/home/you/go中安装了用于linux/amd64的Go.
在/home/you/go/bin中安装了命令.
*** 你需要将/home/you/go/bin添加到你的$PATH中. ***
```

其中最后几行的细节反映了安装时使用的操作系统, 架构和根目录.

有关控制构建的方法的更多信息, 请参见下面关于环境变量的讨论.
`all.bash`(或 `all.bat`)为 `Go` 运行重要的测试, 这可能比简单地构建 `Go` 要花费更多的时间.
如果您不想运行测试套件, 请使用 `make.bash` (或 `make.bat`) 来代替.

### 测试您的安装

通过构建一个简单的程序来检查 `Go` 的安装是否正确.

创建一个名为 `hello.go` 的文件, 并将以下程序放入其中.

```go
package main

import "fmt"

func main() {
    fmt.Printf("hello, world\n")
}
```

然后用 `go` 工具运行它.

```bash
$ go run hello.go
hello, world
```

如果你看到 `"hello, world"` 的信息, 那么Go已经正确安装.

### 设置你的工作环境

你几乎已经完成了. 你只需要再做一些设置.

[How to Write Go Code](https://golang.google.cn/doc/code) 文档提供了使用 Go 工具的基本设置说明.

### 安装其他工具

一些Go工具(包括 godoc)的源代码被保存在 `go.tools` 仓库中. 要安装其中一个工具(本例中为 `godoc`).

```bash
$ go install golang.org/x/tools/cmd/godoc@latest
```

### 社区资源

[帮助页面](https://golang.google.cn/help)上列出的通常的社区资源都有活跃的开发者, 他们可以帮助你解决安装或开发工作中的问题.
对于那些想了解最新情况的人来说, 还有一个邮件列表, [golang-checkins](https://groups.google.com/group/golang-checkins),
它可以收到总结每一次对 Go 仓库的检查的消息.

错误可以通过 [Go问题跟踪器](https://golang.org/issue/new)报告.

### 跟进发布

新的版本会在 [golang-announce](https://groups.google.com/group/golang-announce) 邮件列表中公布.
每个公告都会提到最新的发布标签, 例如 `go1.9`.

要将现有的树更新到最新的版本, 你可以运行.

```bash
$ cd go/src
$ git fetch
$ git checkout <tag>
$ ./all.bash
```

其中 `<tag>` 是版本的字符串.

### 可选的环境变量

`Go` 编译环境可以通过环境变量来定制. *build 时不需要任何环境变量*, 但您可能希望设置一些来覆盖默认值.

+ `$GOROOT`;
    `Go 树`的根, 通常是 `$HOME/go1.X`. 它的值在编译时被内置到树中, 并默认为运行 `all.bash` 的目录的父目录.
    除非你想在版本库的多个本地副本之间切换, 否则没有必要设置这个.

+ `$GOROOT_FINAL`; 当 `$GOROOT` 没有明确设置时, 安装的二进制文件和脚本所假定的值.
    它默认为 `$GOROOT` 的值. 如果您想在一个地方构建 `Go` 树,
    但在构建后将其转移到其他地方, 请将 `$GOROOT_FINAL` 设置为最终的位置.

+ `$GOPATH`; `Go` 发行版以外的 `Go` 项目通常被检查出来的目录.
    例如, `golang.org/x/tools` 可能被签出到 `$GOPATH/src/golang.org/x/tools`.
    `Go` 发行版以外的可执行文件被安装在 `$GOPATH/bin` (或 `$GOBIN`, 如果已设置).
    模块被下载并缓存在 `$GOPATH/pkg/mod` 中.
    `$GOPATH`的默认位置是 `$HOME/go`, 通常没有必要明确设置 `GOPATH`.
    然而, 如果你已经将 `Go` 发行版检出到 `$HOME/go`, 你必须将 `GOPATH` 设置到其他位置以避免冲突.

+ `$GOBIN`;
    使用 [go命令](https://golang.google.cn/cmd/go/)安装 `Go` 发行版以外的可执行文件的目录.
    例如, `go install golang.org/x/tools/cmd/godoc@latest` 下载, 构建并安装 `$GOBIN/godoc`.
    默认情况下, `$GOBIN` 是 `$GOPATH/bin`(如果没有设置 `GOPATH`, 则是 `$HOME/go/bin`).
    安装后, 你要把这个目录添加到你的 `$PATH` 中, 这样你就可以使用已安装的工具.

    注意 `Go` 发行版的可执行文件安装在 `$GOROOT/bin` (用于由人调用的可执行文件)
    或 `$GOTOOLDIR` (用于由 `go`命令调用的可执行文件; 默认为`$GOROOT/pkg/$GOOS_$GOARCH`)而不是 `$GOBIN`.

+ `$GOOS` 和 `$GOARCH`;
    目标操作系统和编译架构的名称. 这些默认值分别为 `$GOHOSTOS` 和 `$GOHOSTARCH` 的值(如下所述).

   ` $GOOS` 的选择是 android, darwin, dragonfly, freebsd, illumos, ios, js, linux, netbsd, openbsd, plan9, solaris 和 windows.

    `$GOARCH` 的选择是 `amd64`(64位x86, 最成熟的端口), `386`(32位x86), `arm`(32位ARM), `arm64`(64位ARM),
    `ppc64le`(PowerPC 64位, 小编码), `ppc64`(PowerPC 64位, 大编码).
    `mips64le` (MIPS 64-bit, little-endian), `mips64` (MIPS 64-bit, big-endian), `mipsle` (MIPS 32-bit, little-endian),
    `mips` (MIPS 32-bit, big-endian), `s390x` (IBM System z 64-bit, big-endian) 和 `wasm` (WebAssembly 32-bit).

+ `$GOOS` 和 `$GOARCH` 的有效组合是:
    + aix   ppc64
    + android   386
    + android   amd64
    + android   arm
    + android   arm64
    + darwin    amd64
    + darwin    arm64
    + dragonfly  amd64
    + freebsd   386
    + freebsd   amd64
    + freebsd   arm
    + illumos   amd64
    + ios   arm64
    + js    wasm
    + linux 386
    + linux amd64
    + linux arm
    + linux arm64
    + linux ppc64
    + linux ppc64le
    + linux mips
    + linux mipsle
    + linux mips64
    + linux mips64le
    + linux riscv64
    + linux s390x
    + netbsd    386
    + netbsd    amd64
    + netbsd    arm
    + openbsd   386
    + openbsd   amd64
    + openbsd   arm64
    + Plan9 386
    + plan9 amd64
    + plan9 arm
    + solaris   amd64
    + windows   386
    + windows   amd64
    + windows  arm
    + windows   arm64

+ `$GOHOSTOS` 和 `$GOHOSTARCH`
    主机操作系统和编译架构的名称. 这些默认为本地系统的操作系统和架构.

    有效选择与上面列出的 `$GOOS` 和 `$GOARCH` 相同. 指定的值必须与本地系统兼容.
    例如, 在 `x86` 系统上, 你不应该把 `$GOHOSTARCH` 设置为 `arm`.

+ `$GO386` (仅适用于 `386`, 默认为 `sse2`)
    这个变量控制 `gc` 实现浮点计算的方式.
    + `GO386=softfloat`: 使用软件浮点运算; 应支持所有 `x86` 芯片(Pentium MMX或更高版本).
    + `GO386=sse2`: 使用 `SSE2` 进行浮点运算; 有更好的性能, 但只适用于 Pentium 4/Opteron/Athlon 64或更高版本.

+ `$GOARM` (仅适用于 `arm`; 如果在目标处理器上构建, 默认为自动检测, 如果不是, 则为 `6`)
    这设置了运行时应针对的 `ARM` 浮点协处理器架构版本. 如果你在目标系统上编译, 它的值会被自动检测到.
    + `GOARM=5`: 使用软件浮点; 当 CPU 没有 VFP 协处理器的时候
    + `GOARM=6`: 只使用 VFPv1; 如果交叉编译, 则默认; 通常是 ARM11 或更好的内核(也支持VFPv2或更好的).
    + `GOARM=7`: 使用 VFPv3; 通常是 Cortex-A内核

    如果有疑问, 可以不设置这个变量, 如果需要, 可以在第一次运行Go可执行文件时调整它.
    [Go社区维基](https://golang.org/wiki)上的[GoARM页面](https://golang.org/wiki/GoArm)包含了关于Go的ARM支持的进一步细节.

+ `$GOMIPS` (仅适用于 `mips` 和 `mipsle`)
    `$GOMIPS64` (仅适用于 `mips64` 和 `mips64le`)
    这些变量设置是否使用浮点指令.     设置为 `hardfloat` 可以使用浮点指令; 这是默认的.
    设置为 `softfloat` 则使用软浮点.

+ `$GOPPC64` (仅适用于 `ppc64` 和 `ppc64le`)
    这个变量设置编译器将针对的处理器级别(即指令集架构版本). 默认是 `power8`.
    + `GOPPC64=power8`: 生成 ISA v2.07 指令
    + `GOPPC64=power9`: 生成 ISA v3.00 指令.

+ `$GOWASM` (仅适用于 `wasm`)
    这个变量是一个逗号分隔的实验性 `WebAssembly` 特性列表, 允许编译后的 `WebAssembly` 二进制文件使用. 默认情况是不使用实验性特征.
    + `GOWASM=satconv`: [生成饱和(非诱导)的浮点数到int的转换][].
    + `GOWASM=signext`: [生成符号扩展运算符][]

注意, `$GOARCH` 和 `$GOOS` 标识的是目标环境, 而不是你正在运行的环境. 实际上, 你总是在进行交叉编译.
所谓`架构`(archtecture), 我们指的是目标环境可以运行的二进制文件的种类:
运行`32-bit-only` 操作系统的 `x86-64` 系统必须将 `GOARCH` 设置为 `386`, 而不是 `amd64`.

如果你选择覆盖默认值, 请在你的 `shell` 配置文件(`$HOME/.bashrc`, `$HOME/.profile`, 或类似的)中设置这些变量.
设置的内容可能是这样的:

```bash
export GOARCH=amd64
export GOOS=linux
```

不过, 要重申的是: build, install 和 develop Go 树, 都不需要设置这些变量.

[生成饱和(非诱导)的浮点数到int的转换]: https://github.com/WebAssembly/nontrapping-float-to-int-conversions/blob/master/proposals/nontrapping-float-to-int-conversion/Overview.md
[生成符号扩展运算符]: https://github.com/WebAssembly/sign-extension-ops/blob/master/proposals/sign-extension-ops/Overview.md
