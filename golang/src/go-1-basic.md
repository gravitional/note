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

### Go 环境变量

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

### Go 文档

[golang.org/x/tools/cmd/godoc](https://pkg.go.dev/golang.org/x/tools/cmd/godoc)
[Godoc: 为你的 Go 代码写文档 ](https://learnku.com/docs/go-blog/godoc-documenting-go-code/6578)

使用如下命令安装:

```bash
go install golang.org/x/tools/cmd/godoc@latest
```

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
[Go Tools Github 镜像](https://github.com/golang/tools)

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
