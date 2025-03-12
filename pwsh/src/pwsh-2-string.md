# powershell 字符串

[在 PowerShell 中连接字符串](https://www.delftstack.com/zh/howto/powershell/concatenate-strings-using-powershell/)

在 PowerShell 中, 我们通过多种方式实现字符串连接;
然而, 虽然 PowerShell 有它自己的内置 `concat()` 函数(我们也将在后面讨论),
有更直接的方法来连接各种字符串变量.

在枚举所有字符串连接方法之前, 最好先声明一些我们将用作示例的字符串变量.

```powershell
$str1 = "First"
$str2 = "Second"
$str3 = "Third"
```

以下是在 PowerShell 中连接字符串的所有可能方法.

## 使用 + 运算符连接字符串

连接字符串的最基本方法是使用 `+` 运算符. 
如果两个变量都是字符串变量, 则连接仅适用于 `+` 运算符. 
如果 `+` 运算符用于两个或多个整数类型变量, 脚本将以数学方式处理表达式.

示例代码:

```powershell
$concatString = $str1 + $str2 + $str3
Write-Output $concatString
输出:

FirstSecondThird
```

## 分隔符连接字符串

我们不仅必须使用 `+` 运算符来连接字符串, 还可以使用逗号 (`,`) 等分隔符.
请记住用双引号 `""` 将你的字符串变量括起来, 否则该变量会将你分隔的字符串值解释为列表属性.
示例代码:

```powershell
$concatString = "$str1 , $str2 , $str3"
Write-Output $concatString
输出:

First , Second , Third
```

另外, 请记住, 这不仅适用于逗号.
用双引号括起你的 `字符串值` 使你的字符串变量成为文字表达式. 所见即所得
示例代码:

```powershell
$concatString = "Who came in $str1 , $str2 , and $str3 Place?"
Write-Output $concatString
输出:

Who came in First , Second, and Third Place?
```

## 连接字符串和整数

如果我们尝试将字符串与整数连接起来, 则会发生无效类型错误.
在下面的示例中, 新变量 `$concatString` 将采用 `$int1` 变量的类型作为其数据类型, 即整数.
发生这种情况是因为在表达式 `$int1 + $str1` 中首先调用了 `$int1`.

示例代码:

```powershell
$int1 = 0
$concatString = $int1 + $str1 #int variable before string variable
Write-Output $concatString
输出:

Cannot convert value "First" to type "System.Int32". Error: "Input string was not in a correct format."
...
```

如果我们将字符串变量 `$str1` 放在表达式的最前面, 连接将成功, `$int1` 将自动转换为字符串值.
示例代码:

```powershell
$int1 = 0
$concatString = $str1 + $int1 #str variable before int variable
Write-Output $concatString

输出:
First0
```

## 字符串替换

或者, 我们可以使用字符串替换执行另一种连接方式.
当连接具有不同数据类型的字符串时, 此方法也将起作用.
示例代码:

```powershell
$int1 = 0
$concatString = "$($int1) , $($str2)"
Write-Output $concatString

输出:
0 , Second
```

## -f 运算符

连接字符串的另一种方法是使用 -f 运算符.
`-f` 运算符使用传递字符串变量作为预构建字符串值的参数.
示例代码:

```powershell
$concatString = "{0}.{1}.{2}." -f $str1,$str2,$str3
Write-Output $concatString

输出:
First.Second.Third
```

## -join 运算符

`-join` 运算符与分隔符的作用类似, 因为我们需要传入一个字符串分隔符作为参数.
示例代码:

```powershell
$concatString = $str1,$str2,$str3 -join "!"
Write-Output $concatString

输出:
First!Second!Third
```

## String Builder 和 append() 函数

字符串构建器是连接字符串的复杂方法之一,
因为我们需要调用一个单独的对象类型 `System.Text.StringBuilder` 来启动这个过程.
然后该对象使用 append() 函数连接字符串, 然后使用 `ToString()` 函数将新对象转换回字符串值.
示例代码:

```powershell
$concatString = New-Object -TypeName System.Text.StringBuilder
$null = $concatString.Append($str1)
$null = $concatString.Append($str2)
$null = $concatString.Append($str3)
$concatString = $concatString.ToString()

Write-Output $concatString

输出:
FirstSecondThird
```

## 使用 concat() 函数

最后, 我们有 PowerShell 自己的 `concat()` 函数.
示例代码:

```powershell
$concatString = [System.String]::Concat($str1,".",$str2,".",$str3)
Write-Output $concatString

输出:
First.Second.Third
```

## 查找字符串

`Select-String `: 查找字符串和文件中的文本. 别名为`sls`.

[Select-String](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.utility/select-string)

+ `-InputObject`; 指定要搜索的文本. 输入一个包含该文本的变量, 或者输入命令或表达式.

使用`InputObject`参数与通过管道向`Select-String`发送字符串是不同的.

当你用管道向`Select-String cmdlet`发送一系列字符串时, 它在每个字符串中搜索指定的文本, 并返回包含搜索文本的每个字符串.
当你使用`InputObject`参数来提交一个字符串的集合时, `Select-String`将该集合视为一个组合字符串.
如果`Select-String`在任何一个字符串中搜索到指定文本, 它会将这些字符串作为一个单元返回.

+ `-Path`; 指定要搜索的文件的路径. 允许使用通配符. 默认位置是本地目录.
指定目录中的文件, 如`log1.txt`, `*.doc`, 或`*.*`. 如果你只指定一个目录, 命令会失败.

```powershell
Select-String -Path .\*.txt -Pattern 'Get-'
```

+ `-AllMatches`; 表示在每行文本中搜索一个以上的匹配.
如果没有这个参数, `Select-String` 在每行文本中只寻找第一个匹配.
当 `Select-String` 在一行文本中找到多个匹配项时,
对这一行它仍然只发出一个 `MatchInfo` 对象, 该对象的 `Matches` 属性包含所有匹配项.
备注:
当与 `SimpleMatch` 参数一起使用时, 这个参数被忽略.
如果你希望返回所有的匹配结果, 并且你要搜索的模式包含 `正则表达式字符`,
你必须转义这些字符, 而不是使用`SimpleMatch`.
关于转义正则表达式的更多信息, 见 about_Regular_Expressions.

+ `-Context` ; 捕获匹配模式的行前后的指定行数.

如果你输入一个数字作为这个参数的值, 同时作为匹配之前和之后捕获的行数.
如果输入两个数字, 第一个数字决定了匹配前的行数, 第二个数字决定了匹配后的行数. 例如, `-Context 2,3`.

在默认显示中, 匹配成功的行在第一列中用直角括号表示(`>`, ASCII 62), 未标记的行是上下文.

上下文参数不会改变 `Select-String` 生成的对象的数量.
`Select-String` 为每个匹配生成一个 `MatchInfo` 对象. 上下文作为一个字符串数组存储在对象的 `Context` 属性中.

当 `Select-String` 命令的输出被管道向另一个 `Select-String` 命令时, 接收的命令只搜索`匹配行`中的文本.
匹配的行是 `MatchInfo` 对象的 `Line` 属性的值, 而不是`context `中的文本.
因此, 对管道后面的`Select-String` 命令使用`-Context` 参数是无效的

当上下文包括匹配时, 每个匹配的 `MatchInfo` 对象包括所有上下文行, 但重叠的行在显示中只出现一次.

+ `-Encoding`; 指定目标文件的编码类型. 默认值是 `utf8NoBOM`.
这个参数的可接受值如下.
  + `ascii` ;  使用`ASCII(7位)`字符集的编码.
  + `bigendianunicode` ;  使用`big-endian`字节顺序的`UTF-16`格式进行编码.
  + `oem` ;  使用`MS-DOS`和控制台程序的默认编码.
  + `unicode` ;  以`UTF-16`格式编码, 使用`little-endian`的字节顺序.
  + `utf7` ;  以`UTF-7`格式编码.
  + `utf8` ;  以`UTF-8`格式编码.
  + `utf8BOM` ;  以`UTF-8`格式编码, 使用BOM(Byte Order Mark, 字节顺序标记).
  + `utf8NoBOM` ;  以`UTF-8`格式编码, 没有BOM.
  + `utf32` ; 以`UTF-32` 格式编码.

从`PowerShell 6.2`开始,
`-Encoding`参数还允许注册代码页的`数字ID`, 如`-Encoding 1251`.
或注册代码页的字符串名称, 如 `-Encoding "windows-1251"` .
欲了解更多信息, 请参见`Encoding.CodePage`的`.NET`文档.

+ `-List` ; 在每个输入文件中只返回匹配文本的第一个实例.
这是检索匹配正则表达式内容的文件列表的最有效方式.
默认情况下, `Select-String` 为它找到的每个匹配项返回一个 `MatchInfo` 对象.
+ `-Quiet` ; 返回一个布尔值(`True`或`False`), 而不是 `MatchInfo` 对象.

+ `-SimpleMatch`; 使用简单匹配而不是正则表达式匹配.
在简单匹配中, `Select-String` 在输入中搜索 `Pattern` 参数中的文本.
它不把 `Pattern` 参数的值解释为正则表达式语句.
另外, 当使用 `SimpleMatch` 时, 返回的 `MatchInfo` 对象的 `Matches` 属性是空的.

+ `-LiteralPath` ; 指定要搜索的文件的路径. `LiteralPath` 参数的值将完全按照输入的内容使用.
字符不会被解释为通配符. 如果路径包括转义字符, 请用单引号将其括起来.
单引号告诉`PowerShell`不要把任何字符解释为转义序列.
欲了解更多信息, 请参见 about_Quoting_Rules.

+ `-NotMatch` ; 查找与指定模式不匹配的文本.
