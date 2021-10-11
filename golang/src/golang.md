# GoLang

[Go 语言教程](https://www.runoob.com/go/go-environment.html)

[Golang Documentation ](https://golang.google.cn/doc/)

## 安装

下载`Go`的二进制包之后：

如果您安装了以前的Go版本, 请确保在安装另一个版本之前将其删除. 

下载`archive`并将其解压缩到`/usr/local`, 在`/usr/local/go`处创建`Go tree`. 
例如, 以`root`用户或通过`sudo`运行以下命令：

```bash
tar -C /usr/local -xzf go1.14.3.linux-amd64.tar.gz
```

将`/usr/local/go/bin`添加到`PATH`环境变量中. 

您可以通过将以下行添加到`$HOME/.profile`或`/etc/profile`e中(对于系统范围的安装)来执行此操作：

```bash
export PATH=$PATH:/usr/local/go/bin
```

注意：对配置文件的更改可能要等到下一次登录计算机后才能应用.  
要立即应用更改, 只需直接在`shell`中运行命令或使用`source $HOME/.profile`之类的命令从配置文件中执行它们即可. 

通过打开命令提示符并键入以下命令来验证是否已安装`Go`：

```bash
$ go version
```

确认命令打印出已安装的`Go`版本. 

***
[第一个 Go 程序](https://www.runoob.com/go/go-tutorial.html)

保存到`hello.go`

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

此外我们还可以使用 `go build` 命令来生成二进制文件：

```bash
$ go build hello.go 
$ ./hello 
```

## Go 语言结构
