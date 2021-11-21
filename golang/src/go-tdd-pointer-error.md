# 指针和错误

[指针和错误](https://studygolang.gitbook.io/learn-go-with-tests/go-ji-chu/pointers-and-errors)

正则表达式使用`\u200B` 匹配隐藏空白.

## 指针

结构中需要一些 balance(余额)变量来存储状态

```go
type Wallet struct {
    balance int
}
```

在 Go 中, 如果一个符号(例如变量, 类型, 函数等)是以小写符号开头, 那么它在 定义它的包之外就是`私有`的.
在我们的例子中, 我们只想让自己的方法修改这个值, 而其他的不可以.

记住, 我们可以使用 `receiver` 变量访问结构体内部的 `balance` 字段.

```go
func (w Wallet) Deposit(amount int) {
    w.balance += amount
}

func (w Wallet) Balance() int {
    return w.balance
}
```

当你创建一个值, 例如 wallet, 它就会被存储在内存的某处. 你可以用 `&wallet` 找到那块内存的地址.

我们可以用 `指针` 来解决这个问题. 指针让我们 `指向` 某个值, 然后修改它.
所以, 我们不是拿钱包的副本, 而是拿一个指向钱包的指针, 这样我们就可以改变它.

```go
func (w *Wallet) Deposit(amount int) {
    w.balance += amount
}

func (w *Wallet) Balance() int {
    return w.balance
}
```

不同之处在于, 接收者类型是 `*Wallet` 而不是 `Wallet`, 你可以将其解读为 `指向 wallet 的指针`.

## 类型别名

我们在制做一个比特币钱包, 一直在使用 `int`, 因为当用来计数时它是不错的类型!
为此创建一个结构体似乎有点过头了. 就 `int` 的表现来说已经很好了, 但问题是它不具有"描述性".
`Go` 允许从现有的类型创建新的类型.

语法是 type MyName OriginalType

```go
type Bitcoin int

type Wallet struct {
    balance Bitcoin
}

func (w *Wallet) Deposit(amount Bitcoin) {
    w.balance += amount
}

func (w *Wallet) Balance() Bitcoin {
    return w.balance
}
func TestWallet(t *testing.T) {

    wallet := Wallet{}

    wallet.Deposit(Bitcoin(10))

    got := wallet.Balance()

    want := Bitcoin(10)

    if got != want {
        t.Errorf("got %d want %d", got, want)
    }
}
```

要生成 `Bitcoin(比特币)`, 你只需要用 `Bitcoin(999)` 的语法就可以了.

类型别名有一个有趣的特性, 你还可以对它们声明 `方法`.
当你希望在现有类型之上添加一些领域内的特定功能时, 这将非常有用.

```go
type Stringer interface {
    String() string
}
```

这个接口是在 `fmt` 包中定义的. 当使用 `%s` 打印格式化的字符串时, 你可以定义此类型的打印方式.

```go
func (b Bitcoin) String() string {
    return fmt.Sprintf("%d BTC", b)
}
```

如你所见, 在类型别名上创建方法的语法与结构上的语法相同.

接下来, 我们需要更新测试中的格式化字符串, 以便它们将使用 `String()` 方法.

```go
    if got != want {
        t.Errorf("got %s want %s", got, want)
    }
```

## nil

如果测试失败, 我们检查错误是否为 `nil`. `nil` 是其他编程语言的 `null`.
错误可以是 `nil`, 因为返回类型是 `error`, 它是一个接口.
如果你看到一个函数, 它接受参数或返回值的类型是 `接口`, 它们就可以是 `nil`.

如果你尝试访问一个值为 `nil` 的值, 它将会引发 运行时的 `panic`.
这很糟糕! 你应该确保你检查了 `nil` 的值.

## 全局值

`var` 关键字允许我们定义包的全局值.
这是一个积极的变化, 因为现在我们的 `Withdraw` 函数看起来很清晰.
接下来, 我们可以重构我们的测试代码来使用这个值而不是特定的字符串.

测试的另一个有用的特性是, 它帮助我们理解代码的真实用途, 从而使我们的代码更具交互性.
我们可以看到, 开发人员可以简单地调用我们的代码, 并对 InsufficientFundsError 进行相等的检查, 并采取相应的操作.

## 未经检查的错误

虽然 Go 编译器对你有很大帮助, 但有时你仍然会忽略一些事情, `错误处理`有时会很棘手.
`errcheck` 工具可以帮助寻找遗漏的错误检查, 它是许多可用的 linters(代码检测工具)之一.

```bash
go get -u github.com/kisielk/errcheck
```

然后, 在你的代码目录中运行 `errcheck .`, 你应该会得到如下类似的内容:

```log
wallet_test.go:17:18: wallet.Withdraw(Bitcoin(10))
```

这告诉我们的是, 我们没有检查在代码行潜在的`错误`.

## 总结

+ `指针`; 当你传`值`给函数或方法时, Go 会`复制`这些值.
    因此, 如果你写的函数需要更改状态(产生副作用), 你就需要用`指针`指向你想要更改的值.
+ Go 取值的副本在大多数时候是有效的, 但是有时候你不希望你的系统只使用副本, 在这种情况下你需要传递一个`引用`.
    例如, 非常庞大的数据或者你只想有一个实例(比如数据库连接池)

+ `nil`; 指针可以是 `nil`. 当函数返回一个的指针, 你需要确保检查过它是否为 nil, 否则你可能会抛出一个执行异常, 编译器在这里无法帮到你
+ nil 非常适合描述一个可能丢失的值

+ `错误`; `错误`用来在调用函数或方法时表示失败
    通过测试我们得出结论, 在错误中检查 raw 字符串会导致测试不稳定.
    因此, 我们用一个有意义的值重构了, 这样就更容易测试代码, 同时对于我们 API 的用户来说也更简单.
+ 错误处理的故事远远还没有结束, 你可以做更复杂的事情, 这里只是抛砖引玉. 后面的部分将介绍更多的策略.
+ [不要只是检查错误, 优雅的处理它们](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)

+ 从现有的类型中创建新的类型:
    + 用于为`值`添加更多的, 领域内的特定含义
    + 可以让你实现接口

指针和错误是 Go 开发中重要的组成部分, 你需要适应这些.
幸运的是, 如果你做错了, 编译器通常会帮你解决问题, 你只需要花点时间读一下错误信息.
