# 如何编写 Go Code

## 简介

本文演示了, 在模块中开发简单的 Go 包, 并介绍了[go工具](https://golang.google.cn/cmd/go/),
这是获取, 构建和安装Go模块, 包和命令的标准方法.

注意: 本文档假设您使用的是Go 1.13或更高版本, 并且未设置 `GO111MODULE` 环境变量.
如果你要找的是本文档的旧版, 即模块之前的版本, 它被[存档在这里](https://golang.google.cn/doc/gopath_code).

## 代码组织

`Go` 程序被组织成`包`(packages).
`package` 是同一目录下的源文件的集合, 这些文件被编译在一起.
在某个源文件中定义的 `函数`, `type`s, `变量`和`常量`对同一包内的所有其他源文件都是可见的.

`repository` 包含一个或多个`模块`(modules).
`模块` 是相关 Go `package`s的集合, 它们被一起发布.

通常,  一个 `Go` repository 只包含一个`模块`, 并位于代码库的根部.
根目录有一个名为 `go.mod` 的文件声明了`模块路径`(module path), 它是模块内所有包的 "导入路径前缀".
与 `go.mod` 处在同一目录层次的`包`, 隶属于该 `模块` , 也包括下面的子目录;
直到子目录中出现另一个 `go.mod` 文件(如果有的话), 划分出新的模块.

注意, 在能够 `build` 之前, 你不需要将你的代码发布到一个远程仓库.
`模块`可以在本地定义, 而不属于一个`repository`.
然而, 组织你的代码是一个好习惯, 就像你有一天会发布它一样.

每个`模块`的路径不仅作为其软件包的导入路径`前缀`, 而且还指出 `go` 命令应该在哪里下载它.
例如, 为了下载 `golang.org/x/tools` 模块, `go` 命令会查阅 `https://golang.org/x/tools`所指示的仓库.
[这里有更多描述](https://golang.org/cmd/go/#hdr-Relative_import_paths)

`import path` 是用于导入 `package` 的字符串. `package` 的导入路径是它的`模块路径`与模块中的子目录的结合.
例如, 模块 `github.com/google/go-cmp` 包含一个在 `cmp/` 目录下的包.
则该包的 `import path` 是 `github.com/google/go-cmp/cmp`.
标准库中的包没有模块路径前缀.

## 你的第一个程序

要编译和运行一个简单的程序, 首先选择一个`模块路径`
我们将使用 `example/user/hello`, 并创建一个 `go.mod` 文件来声明它.

```bash
$ mkdir hello # 或者, 如果它已经存在于版本控制中, 可以克隆它.
$ cd hello
$ go mod init example/user/hello
go: creating new go.mod: module example/user/hello
$ cat go.mod
module example/user/hello

go 1.16
$
```

`Go` 源代码中的第一条语句必须是 `package name`.
对于要编译成可执行命令的代码, 首行必须始终使用 `package main`.

接下来, 在该目录下创建一个名为 `hello.go` 的文件, 包含以下 `Go` 代码:

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, world.")
}
```

现在你可以用 `go` 工具构建和安装该程序.

```bash
$ go install
```

这个命令构建了 `hello` 命令, 产生了一个可执行的二进制文件.
然后它将该二进制文件安装为 `$HOME/go/bin/hello`
(或者, 在 `Windows` 下, `%USERPROFILE%\gobinhello.exe`).

安装目录是由 `GOPATH` 和 `GOBIN` 环境变量控制的.
如果设置了 `GOBIN`, 二进制文件将被安装到该目录.
如果设置了 `GOPATH`, 二进制文件将被安装到 `GOPATH` 列表中第一个目录的 `bin` 子目录中.
否则, 二进制文件将被安装到默认的 `GOPATH`(`$HOME/go` 或 `%USERPROFILE%\go`)的 `bin` 子目录.

你可以使用 `go env` 命令, 可移植地为未来的 `go` 命令设置环境变量的默认值.

```bash
$ go env -w GOBIN=/somewhere/else/bin
```

要取消先前由 `go env -w` 设置的变量, 请使用 `go env -u`.

```bash
$ go env -u GOBIN
```

像 `go install` 这样的命令应用到当前模块的上下文, 该模块包含当前工作目录.
如果 shell 的`工作目录`不在 `example/user/hello` 模块内, `go install` 可能会失败.

为了方便起见, `go` 命令接受相对于 shell `工作目录`的路径, 如果没有给出其他路径,
则默认为当前工作目录下的 package. 因此在我们的工作目录中, 以下命令都是等价的.

```bash
# go install example/user/hello
$ go install .
$ go install
```

接下来, 让我们运行该程序, 以确保它的工作.
为了方便起见, 我们将安装目录添加到我们的PATH中, 以使运行二进制文件更加容易.

```bash
# Windows用户应该参考 https://github.com/golang/go/wiki/SettingGOPATH
# 用于设置%PATH%.
$ export PATH=$PATH:$(dirname $(go list -f '{{.Target}}' . ))
$ hello
Hello, world.
```

如果你使用的是源码控制系统, 现在是好时机: 初始化存储库, 添加文件, 并提交你的第一个改动.
同样, 这一步是可选的: 你不需要使用源码控制来编写Go代码.

```bash
$ git init
Initialized empty Git repository in /home/user/hello/.git/
$ git add go.mod hello.go
$ git commit -m "initial commit"
```

`go` 命令通过请求相应的 `HTTPS` URL, 并读取嵌入 `HTML` 响应中的元数据, 来定位包含特定 `模块路径` 的 git 仓库.
见 [go help importpath](https://golang.google.cn/cmd/go/#hdr-Remote_import_paths).
许多托管服务已经为包含 `Go` 代码的仓库提供了 `元数据`,
因此让你的模块供他人使用的最简单方法通常是, 使其`模块路径`与仓库的`URL`相匹配.

## 在你的模块中导入软件包

让我们写一个 `morestrings` 包并从 `hello` 程序中使用它.
首先, 为该包创建一个名为 `$HOME/hello/morestrings` 的目录,然后在该目录下创建一个名为 `reverse.go` 的文件, 内容如下.

```go
// 软件包 morestrings 实现了额外的函数来操作UTF-8 编码的字符串,
// 超出了标准 "strings "包所提供的内容.
package morestrings

// ReverseRunes 返回其参数字符串从左到右的反转.
func ReverseRunes(s string) string {
    r := []rune(s)
    for i, j := 0, len(r)-1; i < len(r)/2; i, j = i+1, j-1 {
        r[i], r[j] = r[j], r[i]
    }
    return string(r)
}
```

因为我们的 `ReverseRunes` 函数以大写字母开头, 所以它是[exported](https://golang.google.cn/ref/spec#Exported_identifiers),
可以在其他导入我们的 `morestrings` 包的包中使用.

>`标识符`可以被导出, 以允许从另一个包中对其进行访问. 一个标识符在以下两种情况下被导出.
> 标识符名称的第一个字符是一个Unicode大写字母(Unicode类 "Lu"); 以及
> 该标识符在包块中被声明, 或者它是一个字段名或方法名.
>所有其他标识符都不会被导出.

让我们用 `go build` 来测试一下这个包的编译情况.

```bash
$ cd $HOME/hello/morestrings
$ go build
```

这不会产生输出文件. 相反, 它会把编译好的包保存在本地 build cache 中.

在确认了 `morestrings` 包构建成功之后, 让我们在 `hello` 程序中使用它.
要做到这一点, 请修改你原来的 `$HOME/hello/hello.go` 以使用 `morestrings` 包.

```bash
package main

import (
    "fmt"

    "example/user/hello/morestrings"
)

func main() {
    fmt.Println(morestrings.ReverseRunes("!oG ,olleH"))
}
```

安装 `hello` 程序:

```bash
$ go install
```

运行新版本的程序, 你应该看到一个新的, 反转的信息.

```bash
$ hello
Hello, Go!
```

## 从远程模块导入软件包

`import path` 可以描述如何使用 `Git` 或 `Mercurial` 等 revision控制系统获得软件包的源代码.
`go` 工具使用这个`属性`来自动从远程仓库获取包.
例如, 要在你的程序中使用 `github.com/google/go-cmp/cmp`.

```go
package main

import (
    "fmt"

    "example/user/hello/morestrings"
    "github.com/google/go-cmp/cmp"
)

func main() {
    fmt.Println(morestrings.ReverseRunes("!oG ,olleH"))
    fmt.Println(cmp.Diff("Hello World", "Hello Go"))
}
```

现在你有了对外部模块的依赖, 你需要下载该模块并在你的go.mod文件中记录其版本.
go mod tidy 命令为导入的包添加缺少的模块需求, 并删除不再使用的模块的需求.

```bash
$ go mod tidy
go: finding module for package github.com/google/go-cmp/cmp
go: found github.com/google/go-cmp/cmp in github.com/google/go-cmp v0.5.4
$ go install
$ hello
Hello, Go!
  string(
-     "Hello World",
+     "Hello Go",
  )
$ cat go.mod
module example/user/hello

go 1.16

require github.com/google/go-cmp v0.5.4
```

模块的`依赖`被自动下载到 `GOPATH` 环境变量所指示的目录的 `pkg/mod` 子目录中.
特定版本的模块的下载内容, 在所有需要该版本的其他`模块`中共享, 因此 `go` 命令将这些文件和目录标记为只读.
要删除所有下载的模块, 你可以在 `go clean` 中传递 `-modcache` 标志.

```bash
$ go clean -modcache
```

## 测试

`Go` 有一个轻量级测试框架, `由go test` 命令和 `testing` 包组成.

你可以通过创建一个名字以 `_test.go` 结尾的文件来编写测试, 该文件包含名为 `TestXXX` 的函数, 其签名为 `func(t *testing.T)`.
测试框架运行每个这样的函数; 如果该函数调用一个失败的函数, 如 `t.Error` 或 `t.Fail`, 则认为测试失败.

通过创建包含以下 Go 代码的 `$HOME/hello/morestrings/reverse_test.go` 文件, 向 `morestrings` 包添加一个测试.

包 morestrings

输入 "测试"

func TestReverseRunes(t *testing.T) {
    cases := []struct {
        in, want string
    }{
        {"你好, 世界", "dlrow ,olleH"},
        {"Hello, 世界", "界世 ,olleH"},
        {"", ""},
    }
    for _, c := range cases {
        got := ReverseRunes(c.in)
        if got != c.want {
            t.Errorf("ReverseRunes(%q) == %q, want %q", c.in, got, c.want)
        }
    }
}

然后用 `go test` 运行该测试.

```bash
$ cd $HOME/hello/morestrings
$ go test
PASS
ok      example/user/hello/morestrings 0.165s
```

运行 [go help test](https://golang.google.cn/cmd/go/#hdr-Test_packages), 更多细节请看[测试包的文档](https://golang.google.cn/pkg/testing/).

## 下一步工作

订阅 golang-announce 邮件列表, 以便在 Go 的新稳定版本发布时获得通知.

请参阅 [Effective Go](https://golang.google.cn/doc/effective_go), 了解有关编写清晰, 简洁的Go代码的技巧.

参加 Go 之旅来学习这门语言.

访问[文档页面](https://golang.google.cn/doc/#articles), 了解有关 Go 语言及其库和工具的一系列深度文章.

获得帮助

要获得实时帮助, 请向社区管理的 [gophers Slack][] 服务器中的热心 gophers 咨询, 在此获取[邀请][]

用于讨论 Go 语言的官方邮件列表是 [Go Nuts](https://groups.google.com/group/golang-nuts).

使用 [Go 问题跟踪器](https://golang.org/issue)报告错误.

[gophers Slack]: https://gophers.slack.com/?redir=%2Fmessages%2Fgeneral%2F
[邀请]: https://invite.slack.golangbridge.org
