# pwsh_2

## 运算符

[about_Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_operators)
[关于算术运算符](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_arithmetic_operators)

1. `.\` : 执行一个脚本或命令
2. `&` : 将字符串直接解释成命令并执行
3. `[]` : 类型转换
4. `.` : 调用`.NET`对象的成员,或`global`执行脚本(当前作用域中执行)
5. `::` : 调用`.NET`**类**中的**静态成员**
6. `..` : 创建一个范围闭区间
7. `-f` : 格式化数据
8. `$` : 将字符串内部的变量转换为实际的值
9. `@()` : 将一系列值转换为一个数组
10. `,` : 数组分隔,或创建单元素数组
11. `#` : 添加注释,单行

备注：

+ `.\`运算符用于执行一个脚本或命令.  如果执行的是`Powershell`脚本,那么脚本会在自己的作用域中执行, 也就是说在当前环境下无法访问被执行的脚本中的变量.
+ `&`默认键入一个字符串,`powershell`会将它原样输出,如果该字符串是一个`命令`或者`外部程序`,在字符串前加‘`&`’可以执行命令,或者启动程序.
+ 如果你之前将`Powershell`命令存储在了一个字符串中,或者一个变量中. 此时,`&`将字符串直接解释成命令并执行
+ 事实上,`&`可以直接执行一个`CommandInfo`对象,绕过自身的内部`get-command`, 如

```powershell
&(Get-Command tasklist)
```

### 分组运算符()

和其他语言一样, `(...)` 在表达式中用于覆盖运算符优先级. 例如. `(1+2)/3`. 然而, 在`PowerShell`中, 还有一些额外的行为.

+ `(...)`允许你让一个命令的输出参与到表达式中, 例如:

```powershell
PS> (Get-Item *.txt).Count -gt 10
True
```

当作为管道的第一段使用时, 用圆括号`()`包裹命令或表达式必然会引起表达式结果的`枚举`.
用圆括号`()`包裹命令, 那么在结果通过管道发送之前, 该命令将被运行至完成, 所有输出都被收集在内存中.

### 子表达式运算符$()

返回一个或多个`语句`的结果. 
对于单个结果, 返回一个`标量`. 对于多个结果, 返回一个`数组`.
当你想在`A表达式`中使用`B表达式`时, 可以使用这个操作. 
例如, 将命令的结果嵌入到一个字符串表达式中. 
与圆括号`()`的明显区别是，当表达式出现字符串中时，也能"激活计算". 例如:

```powershell
# 单纯圆括号不输入日期.
"Today is (Get-Date)"; "Today is $(Get-Date)"
"Folder list: $((Get-ChildItem C:\ -Directory).Name -join ', ')"
Folder list: Program Files, Program Files (x86), Users, Windows
```

### 常用运算符

+ 基本的数学运算符都是支持的.

```powershell
$i=5;  $sum=3+4*($i-3)/2; $sum
```

+ 也支持前置后置自增自减运算符.

```powershell
$i=0;
$i--
$i++
++$i
--$i
```

+ `>`和`>>`; 重定向运算符,用于将标准输出流重定向到文件,前者会覆盖已有文件,后者则是追加到已有文件末尾.
+ 逻辑运算符有与(`-and`), 或(`-or`), 非(`-not`或`!`)以及异或(`xor`)几个, 并且支持短路运算. 如果需要使用真值和假值字面量,可以使用`$true`和`$false`.
+ `命令 &`的形式可以将命令设置为后台运行,当运行的命令会阻塞当前终端的时候很有用.
+ `-split`和`-join`用于将一个字符串分为几个子部分,或者将几个子部分组合为一个字符串.

```powershell
'A B C DE' -split ' '
# 连接
-join ("a", "b", "c")
 'A','B','C' -join ','
```

+ `[string]`形式的运算符可以转换变量的类型,比如说下面的代码,就将`pi`变量转换为了`Float`类型.

```powershell
[Float]$pi = 3.14
$pi -is [Float]
```

+ `.`运算符用于调用`.NET`对象的成员,它也可以用于执行脚本. 当它用于执行脚本的时候,脚本会在当前作用域中执行,所以脚本结束之后,我们可以访问脚本中的元素.
+ `::`运算符用于调用类中的`静态成员`, 例如下面调用`.NET`平台中`DateTime`类的`Now`属性.

```powershell
PS D:\Desktop> [DateTime]::Now
out:
2017年5月18日 22:45:42
```

+ `..`运算符用于创建一个范围闭区间,例如下面这样.

```powershell
PS D:\Desktop> 1..3
out:
1
2
3
PS D:\Desktop> 3..1
```

+ `-f`运算符用于格式化数据,例如下面这样.格式化方法和`C#`中的完全相同,所以如果不熟悉的话直接看在`C#`中如何格式化数据就行了.

```powershell
PS D:\Desktop> 'My name is {0}, I am {1} years old' -f 'yitian',24
My name is yitian, I am 24 years old
```

+ `$var` 形式可以用来插入变量的值. 例如下面.需要注意使用内插操作符的时候,外部字符串需要使用双引号,否则`Powershell`会直接输出字符串内容.

```powershell
PS D:\Desktop> $name='yitian'
PS D:\Desktop> $age=24
PS D:\Desktop> "My name is $name, I am $age years old."
out:
My name is yitian, I am 24 years old.
```

+ `@()`运算符用于将一系列值转换为一个数组.  假如在脚本中有一个函数可能返回`0`, `1`或多个值, 就可以使用这个操作符,将一系列值合并为一个数组,方便后续处理.
+ `,`逗号运算符如果放置在单个值前面,就会创建一个包含这个值的单元素数组.

### Format 运算符 -f

For more information, see the String.Format method and Composite Formatting.

[String.Format Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-5.0#insert-a-string)
[composite-formatting](https://docs.microsoft.com/en-us/dotnet/standard/base-types/composite-formatting)
[Standard 数字格式字符串](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-numeric-format-strings)
[Custom 数字格式字符串](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-numeric-format-strings)
[Standard 日期和时间格式字符串](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)

使用字符串对象的 `format` 方法设置字符串的格式.  在运算符左侧输入格式字符串, 并输入要设置其右侧格式的对象.

+ `{0}`表示要插入的第一个对象,
+ `{0:d}`将 `d` 格式字符串应用于对象列表中的第一个对象.
+ `{0,12}`表示结果中的字符串的宽度, 超过的话将会忽略指定, 完整显示.
+ `{0,-12}`来定义一个12个字符的左对齐字段, 默认情况下字符串是右对齐的.
+ `{1,15:N0}`指定宽度和`N0`格式化, 数字, `0`个小数位.

示例:

```powershell
# 日期格式化
"It is now {0:d} at {0:t}" -f (Get-Date)
1 hello      3.14
# 数字格式化
"{0,6} {1,15:N0}" -f 2021,10256323434543
"{0,-6} {1,-15:N0}" -f 2021,10256323434543
# 圆周率
"{0} {1,-10} {2:N}" -f 1,"hello",[math]::pi
# 如果需要将输出大括号字符, 可以输入双层大括号来转义
"{0} vs. {{0}}" -f 'foo'
```

***
格式说明
Format specifier   Description  Examples

+ `"C "或 "c",` : 货币, `123.456 ("C", en-US) -> $123.46`.
+ `"D "或 "d"` : 十进制, 可以带有负号的整数, `1234 ("D")-> 1234`, `1234 ("D6")-> -001234`.
+ `"E "或 "e"` : 指数,科学计数法,  `1052.0329112756 ("E", en-US)-> 1.052033E+003`.
+ `"F "或 "f"` : 定点, 整数和小数位, 可选择负号. `1234.567 ("F", en-US)-> 1234.57`.
+ `"G "或 "g"` : 一般. 定点符号或科学符号中更紧凑的一种. `-123.456 ("G", en-US)-> -123.456`.
+ `"N "或 "n"` : 数字. `3`位一组, 以及可选择负号和小数点. `1234.567 ("N", en-US)-> 1,234.57`, `-1234.56 ("N3", n-US)-> -1,234.560`.
+ `"P "或 "p"` : 百分比. 数字乘以100并显示百分比符号.  `1 ("P", en-US)-> 100.00 %`.
+ `"R "或 "r"` :  往返的. 一个字符串可以四舍五入到一个相同的数字. `123456789.12345678 ("R")-> 123456789.12345678`
+ `"X "或 "x"` : 十六进制. `255 ("x")-> ff`.

### Quoting_Rules

[about_Quoting_Rules](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_quoting_rules).

`here-strings`的 quotation 规则略有不同.

`here-string`是一个`单引号`或`双引号`包裹的字符串, 其中出现的引号按字面意思解析 .
一个`here-string`可以横跨多行, `here-string`中所有的行都被解释为字符串, 尽管它们没有用`引号`括起来.

像普通的字符串一样, 在双引号的`here-strings`中, `变量`被替换成它们的值. 在单引号的`here-strings`中, 变量不会替换成它们的值.
你可以对任何文本使用`here-strings`, 但它们对以下类型的文本特别有用.

+ 包含字面值引号的文本
+ 多行文本, 如`HTML`或`XML`中的文本
+ 脚本或函数文档的帮助文本

`here-string`可以有以下两种格式, 其中`<Enter>`代表换行或隐藏的换行字符, 当你按下`Enter`键时, 它就会被添加,

双引号:

```powershell
@"<Enter>
<字符串> [字符串] ...<Enter>
"@
```

单引号:

```powershell
@'<Enter
<字符串> [字符串] ...<Enter>
'@
```

不管哪种格式, 结尾的引号必须是该行的第一个字符.
`here-string`包含两个隐藏`<Enter>`之间的所有文本. 在`here-string`中, 所有引号都按字面意思解释. 比如说:

```powershell
@"
要使用帮助, 请输入 "get-help"
"@
# 会原样输出文字
@"
使用引号 (') 来开始一个字符串.
"@
```

双引号包裹的`here-strings`, 变量才会被替换. 例如:

```powershell
@"
即使你没有创建自己的配置文件,  它的路径依然是:
$profile
"@
"即使你没有创建自己的配置文件,  它的路径依然是:
C:\Users\User1\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1"
```

`Here-strings`通常用于将多行内容分配给一个变量. 例如, 下面的`here-string`将一页`XML`分配给`$page`变量.

```powershell
$page = [XML] @"
<command:command xmlns:maml="http://schemas.microsoft.com/maml/2004/10"
...
</command:command>
"@
```

`Here-strings`也是`ConvertFrom-StringData` 的一种方便的输入格式, 此命令将 `Here-strings` 转换为哈希表.

### 正则表达式

[about_Regular_Expressions](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_regular_expressions)

正则表达式是一种用于匹配文本的模式.  它可以由文本字符, 运算符和其他构造组成.
`PowerShell` 具有多个使用正则表达式的运算符和 cmdlet.  可以在以下链接中阅读有关其语法和用法的详细信息.

+ `Select-String`
+ `-match` 和 `-replace` 运算符
+ `-split`
+ 带 `-regex` 选项的 `switch` 语句

默认情况下, `PowerShell` 正则表达式不区分大小写.  上面所示方法可以通过选项强制区分大小写.

方法     区分大小写

+ Select-String     使用 `-CaseSensitive` 开关
+ switch 语句     使用 `-casesensitive` 选项
+ 运算符     前缀为 `c` (`-cmatch`, `-csplit`, 或 `-creplace`)

#### 查找字符串

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

+ `-AllMatches`; 表示在每行文本中搜索一个以上的匹配. 如果没有这个参数, `Select-String` 在每行文本中只寻找第一个匹配.
当 `Select-String` 在一行文本中找到多个匹配项时, 对这一行它仍然只发出一个 `MatchInfo` 对象, 该对象的 `Matches` 属性包含所有匹配项.

备注:

当与 `SimpleMatch` 参数一起使用时, 这个参数被忽略.
如果你希望返回所有的匹配结果, 并且你要搜索的模式包含正则表达式字符, 你必须转义这些字符, 而不是使用`SimpleMatch`.
关于转义正则表达式的更多信息, 见about_Regular_Expressions.

+ `-Context` ; 捕获匹配模式的行前后的指定行数.

如果你输入一个数字作为这个参数的值, 同时作为匹配之前和之后捕获的行数.
如果输入两个数字, 第一个数字决定了匹配前的行数, 第二个数字决定了匹配后的行数. 例如, `-Context 2,3`.

在默认显示中, 匹配成功的行在第一列中用直角括号表示(`>`, ASCII 62), 未标记的行是上下文.

上下文参数不会改变 `Select-String` 生成的对象的数量.
`Select-String` 为每个匹配生成一个 `MatchInfo` 对象. 上下文作为一个字符串数组存储在对象的 `Context` 属性中.

当 `Select-String` 命令的输出被管道向另一个 `Select-String` 命令时, 接收的命令只搜索`匹配行`中的文本.
匹配的行是 `MatchInfo` 对象的 `Line` 属性的值, 而不是`context `中的文本. 因此, 对管道后面的`Select-String` 命令使用`-Context` 参数是无效的

当上下文包括匹配时, 每个匹配的 `MatchInfo` 对象包括所有上下文行, 但重叠的行在显示中只出现一次.

+ `-Encoding`; 指定目标文件的编码类型. 默认值是 `utf8NoBOM`.  这个参数的可接受值如下.

+ `ascii` ;  使用`ASCII(7位)`字符集的编码.
+ `bigendianunicode` ;  使用`big-endian`字节顺序的`UTF-16`格式进行编码.
+ `oem` ;  使用`MS-DOS`和控制台程序的默认编码.
+ `unicode` ;  以`UTF-16`格式编码, 使用`little-endian`的字节顺序.
+ `utf7` ;  以`UTF-7`格式编码.
+ `utf8` ;  以`UTF-8`格式编码.
+ `utf8BOM` ;  以`UTF-8`格式编码, 使用BOM(Byte Order Mark, 字节顺序标记).
+ `utf8NoBOM` ;  以`UTF-8`格式编码, 没有BOM.
+ `utf32` ; 以`UTF-32` 格式编码.

从`PowerShell 6.2`开始, `-Encoding`参数还允许注册代码页的`数字ID`, 如`-Encoding 1251`.
或注册代码页的字符串名称, 如 `-Encoding "windows-1251"` . 欲了解更多信息, 请参见`Encoding.CodePage`的`.NET`文档.

+ `-List` ; 在每个输入文件中只返回匹配文本的第一个实例. 这是检索匹配正则表达式内容的文件列表的最有效方式.
默认情况下, `Select-String` 为它找到的每个匹配项返回一个 `MatchInfo` 对象.
+ `-Quiet` ; 返回一个布尔值(`True`或`False`), 而不是 `MatchInfo` 对象.
+ `-SimpleMatch`; 使用简单匹配而不是正则表达式匹配. 在简单匹配中, `Select-String` 在输入中搜索 `Pattern` 参数中的文本.
它不把 `Pattern` 参数的值解释为正则表达式语句. 另外, 当使用 `SimpleMatch` 时, 返回的 `MatchInfo` 对象的 `Matches` 属性是空的.
+ `-LiteralPath` ; 指定要搜索的文件的路径. `LiteralPath` 参数的值将完全按照输入的内容使用.
字符不会被解释为通配符. 如果路径包括转义字符, 请用单引号将其括起来. 单引号告诉`PowerShell`不要把任何字符解释为转义序列.
欲了解更多信息, 请参见about_Quoting_Rules.
+ `-NotMatch` ; 查找与指定模式不匹配的文本.

### equality运算符

[about_Comparison_Operators](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_comparison_operators)

默认情况下, 字符串的比较是不区分大小写的.
`equality `运算符有明确的大小写敏感和不敏感的形式. 要使一个比较运算符区分大小写, 在`-`后面加`c`.
例如, `-ceq`是`-eq`的大小写敏感版本. 要使大小写不敏感, 在-后面加一个`i`. 例如, `-ieq`是`-eq`的不区分大小写版本.

当运算符的输入是一个`标量值`时, 运算符会返回一个`布尔值`. 当输入是一个`集合`时, 运算符返回集合中与表达式右边的值相匹配的元素.
如果集合中没有匹配的元素, 比较运算符会返回一个`空数组`. 比如说

```powershell
$a = (1, 2 -eq 3)
$a.GetType().Name
$a.Count
Out:Object[]
0
```

有几个例外情况.

+ `containment `和`type `类型运算符总是返回一个`布尔值`
+ `-replace`操作符返回替换结果
+ 除非表达式的左边是一个集合, 否则`-match`和`-notmatch`操作符还会填充`$Matches`自动变量.

+ `-gt`,`-ge`
+ `-lt`,`-le` `
+ `-eq`,`-ne`

```powershell
2 -eq 2                 # Output: True
2 -eq 3                 # Output: False
"abc" -eq "abc"         # Output: True
"abc" -eq "abc", "def"  # Output: False
"abc" -ne "def"         # Output: True
"abc" -ne "abc"         # Output: False
"abc" -ne "abc", "def"  # Output: True
```

当左手边是一个集合时, `-eq`返回那些与右手边匹配的成员, 而`-ne`则过滤掉它们.

```powershell
1,2,3 -eq 2             # Output: 2
"abc", "def" -eq "abc"  # Output: abc
"abc", "def" -ne "abc"  # Output: def
```

比较任意对象的一个突出例子是要找出它们是否为空.
但是如果你需要确定一个变量是否为`$null`, 你必须把`$null`放在`-eq`运算符的左边. 把它放在右手边并不能达到你所期望的效果.
例如, 令`$a`为包含空元素的数组.

```powershell
$a = 1, 2, $null, 4, $null, 6
$null -ne $a #True
```

当左边是集合时, 将每个成员与右边的成员进行比较. 根据他们的逻辑, 要么保留要么丢弃.

```powershell
$a=5, 6, 7, 8, 9
Write-Output "Test collection:";$a
Write-Output "`nMembers greater than 7"; $a -gt 7
Write-Output "`nMembers greater than or equal to 7";$a -ge 7
Write-Output "`nMembers smaller than 7"; $a -lt 7
Write-Output "`nMembers smaller than or equal to 7"; $a -le 7
```

这些操作符对任何实现`System.IComparable`的类都有效.

```powershell
# Date comparison
[DateTime]'2001-11-12' -lt [DateTime]'2020-08-01' # True

# Sorting order comparison
'a' -lt 'z'           # True; 'a' comes before 'z'
'macOS' -ilt 'MacOS'  # False
'MacOS' -ilt 'macOS'  # False
'macOS' -clt 'MacOS'  # True; 'm' comes before 'M'
```

下面的例子表明, 在美式`QWERTY`键盘上没有任何标点符号会被排在`'a'`之后.

```powershell
$a=' ','`','~','!','@','#','$','%','^','&','*','(',')','_','+','-','=',
   '{','}','[',']',':',';','"','''','\','|','/','?','.','>',',','<'
$a -gt 'a'
# Output: Nothing
```

### match运算符

匹配运算符(`-like, -notlike, -match, -notmatch`)可以找到符合或不符合指定模式的元素.
`-like`和`-notlike`的模式是通配符表达式(包含`*`, `?`和`[ ]`), 而`-match`和`-notmatch`接受正则表达式`Regex`.语法是,

```powershell
<string[]> -like    <wildcard-expression>
<string[]> -notlike <wildcard-expression>
<string[]> -match    <regular-expression>
<string[]> -notmatch <regular-expression>
```

当这些运算符的输入是标量值时, 它们返回布尔值. 当输入的是值的集合时, 运算符返回任何匹配的成员.
如果在集合中没有匹配的成员, 运算符会返回空数组.

#### -like,-notlike

```powershell
"PowerShell" -like    "*shell"           # Output: True
"PowerShell" -notlike "*shell"           # Output: False
"PowerShell" -like    "Power?hell"       # Output: True
"PowerShell" -notlike "Power?hell"       # Output: False
"PowerShell" -like    "Power[p-w]hell"   # Output: True
"PowerShell" -notlike "Power[p-w]hell"   # Output: False
"PowerShell", "Server" -like "*shell"    # Output: PowerShell
"PowerShell", "Server" -notlike "*shell" # Output: Server
```

#### -match,-notmatch

`-match`和`-notmatch`使用正则表达式来搜索左侧值中的模式.
正则表达式可以匹配复杂的模式, 如电子邮件地址, UNC路径, 或格式化的电话号码.
右侧的字符串必须遵守正则表达式的规则.

```powershell
# 部分匹配测试, 显示-match和-like 行为的不同.
"PowerShell" -match 'shell'        # Output: True
"PowerShell" -like  'shell'        # Output: False
# Regex syntax test
"PowerShell" -match    '^Power\w+' # Output: True
'bag'        -notmatch 'b[iou]g'   # Output: True
```

如果输入是集合, 运算符会返回该集合的匹配成员.

```powershell
"PowerShell", "Super PowerShell", "Power's hell" -match '^Power\w+' # Output: PowerShell
"Rhell", "Chell", "Mel", "Smell", "Shell" -match "hell" # Output: Rhell, Chell, Shell
"Bag", "Beg", "Big", "Bog", "Bug"  -match 'b[iou]g' #Output: Big, Bog, Bug
"Bag", "Beg", "Big", "Bog", "Bug"  -notmatch 'b[iou]g' #Output: Bag, Beg
```

`-match`和`-notmatch`支持`捕获组`.
每次它们在标量输入上运行, 并且`-match`结果为`True`, 或者`-notmatch`结果为`False`, 它们都会覆盖`$Matches`这个自动变量.
`$Matches`是一个`Hashtable`, 它总是有一个名为`'0'`的键, 用来存储整个匹配.
如果正则表达式包含`捕获组`, `$Matches`包含每个组的额外`key`.

```powershell
$string = 'The last logged on user was CONTOSO\jsmith'
$string -match 'was (?<domain>.+)\\(?<user>.+)'; $Matches;
Write-Output "`nDomain name:";$Matches.domain
Write-Output "`nUser name:";$Matches.user
```

当`-match`的结果为`False`, 或`-notmatch`的结果为`True`, 或输入是一个集合时, `$Matches`自动变量不会被覆盖.
因此, 它将包含先前设置的值, 如果变量没有被设置, 则为`$null`.
如果在调用这些运算符之后, 需要引用`$Matches`, 可以使用条件语句来判断该变量是否被设置.

```powershell
if ("<version>1.0.0</version>" -match '<version>(.*?)</version>') {
    $Matches
}
```

### -replace

+ `<input> -replace <正则表达式>, <目标>` ; 使用正则表达式匹配特定模式, 然后替换成目标.
默认情况下,  `-replace` 运算符不区分大小写. 若要使其区分大小写, 请使用 `-creplace`. 若要使其显式不区分大小写, 请使用 `-ireplace`.

```powershell
"book" -ireplace "B", "C" # Case insensitive
"book" -creplace "B", "C" # Case-sensitive; hence, nothing to replace
```

`-replace`使用给出的目标替换表达式中所有匹配模式的部分, 例如把`.txt`改成`.log`

```powershell
Get-ChildItem *.txt | Rename-Item -NewName { $_.name -replace '\.txt$','.log' }
```

#### 捕获组

也可以使用使用正则表达式的捕获组, 来动态地替换文本. 捕获组可以在`<替换>`字符串中使用, 用`$`加`group identifier`的形式引用.

在下面的例子中, `-replace`操作符收到`DomainName/Username`形式的用户名, 并转换为`Username@DomainName`的格式.

```powershell
$SearchExp = '^(?<DomainName>[\w-.]+)\\(?<Username>[\w-.]+)$'
$ReplaceExp = '${Username}@${DomainName}'
'Contoso.local\John.Doe' -replace $SearchExp,$ReplaceExp
Out: John.Doe@Contoso.local
```

`$`字符在`PowerShell`和`正则表达式`中都有语法上的作用.

+ 在PowerShell中, 在双引号之间, 它指定变量并作为一个子表达式操作符.
+ 在`Regex`匹配字符串中, 它表示`行的结束`.
+ 在`Regex`替换字符串中, 它表示被捕获的组. 请确保将你的正则表达式放在单引号之间, 或者用`` ` ``转义, 也就是`` `$ ``.

```powershell
$1 = 'Goodbye'

'Hello World' -replace '(\w+) \w+', "$1 Universe"
# Output: Goodbye Universe

'Hello World' -replace '(\w+) \w+', '$1 Universe'
# Output: Hello Universe
```

Regex中的`$$`表示一个字面的`$`. 在替换字符串中的`$$`产生一个字面的`$`. 比如,

```powershell
'5.72' -replace '(.+)', '$ $1' # Output: $ 5.72
'5.72' -replace '(.+)', '$$$1' # Output: $5.72
'5.72' -replace '(.+)', '$$1'  # Output: $1
```

#### 替换集合

当`-replace`的`<input>`是一个集合时, `PowerShell`会将替换应用到集合中的每个值. 比如说

```powershell
"B1","B2","B3","B4","B5" -replace "B", 'a'
a1
a2
a3
a4
a5
```

#### 用脚本块替换

在`PowerShell 6`及以后的版本中, `-replace`操作符也接受一个执行替换的`脚本块`. 每一次匹配成功, 脚本块都会运行一次. 语法:

```powershell
<String> -replace <regular-expression>, {<Script-block>}
```

在脚本块中, 使用`$_`自动变量来访问被替换的输入文本和其他有用信息, 这个变量的类型是`System.Text.RegularExpressions.Match`.

下面的例子将每三位数的序列替换为对应的字符:

```powershell
"072101108108111" -replace "\d{3}", {return [char][int]$_.Value}
Out: Hello
```

### 包含运算符

包含运算符(`-contains`, `-notcontains`, `-in`, 和 `-notin`)与`equality `运算符相似, 只是它们总是返回一个布尔值, 即使输入是一个集合.
这些运算符在检测到第一个匹配时就停止比较, 而`equality `运算符则是计算所有输入的成员.
在一个非常大的集合中, 这些运算符比`equality `运算符返回得更快.

#### -contains,-notcontains

测试集合中是否包括某个元素. 当右侧(测试对象)与集合中的一个元素相匹配时, `-contains` 返回 `True`. `-notcontains`则返回`False`.
当测试对象是一个集合时, 这些运算符使用`reference equality`, 也就是说, 它们检查集合中是否有某个元素, 是测试对象的同一个实例.

```powershell
'aabcc' -match 'a*b?c+'
True
'aab' -match 'a*b?c+'
False
# afas
"abc", "def" -contains "def"                  # Output: True
"abc", "def" -notcontains "def"               # Output: False
"Windows", "PowerShell" -contains "Shell"     # Output: False
"Windows", "PowerShell" -notcontains "Shell"  # Output: True
"abc", "def", "ghi" -contains "abc", "def"    # Output: False
"abc", "def", "ghi" -notcontains "abc", "def" # Output: True
###
$DomainServers = "ContosoDC1","ContosoDC2","ContosoFileServer","ContosoDNS","ContosoDHCP","ContosoWSUS"
$thisComputer  = "ContosoDC2"
$DomainServers -contains $thisComputer
# Output: True
$a = "abc", "def"
"abc", "def", "ghi" -contains $a # Output: False
$a, "ghi" -contains $a           # Output: True
```

#### -in and -notin

`PowerShell 3`中引入了`-in`和`-notin`操作符, 作为`-contains`和`-notcontains`操作符的反向语法.
当左边的`<test-object>`与集合中的某个元素匹配时, `-in`返回`True`, `-notin`则返回`False`.
当测试对象是一个集合时, 这些运算符使用`reference equality`来检查集合中的元素是否是测试对象的同一个实例.

下面的例子与`-contains`和`-notcontains`的例子功能相同:

```powershell
"def" -in "abc", "def"                  # Output: True
"def" -notin "abc", "def"               # Output: False
"Shell" -in "Windows", "PowerShell"     # Output: False
"Shell" -notin "Windows", "PowerShell"  # Output: True
"abc", "def" -in "abc", "def", "ghi"    # Output: False
"abc", "def" -notin "abc", "def", "ghi" # Output: True
```

```powershell
$DomainServers = "ContosoDC1","ContosoDC2","ContosoFileServer","ContosoDNS","ContosoDHCP","ContosoWSUS"
$thisComputer  = "ContosoDC2"
$thisComputer -in $DomainServers # Output: True
$a = "abc", "def"
$a -in "abc", "def", "ghi" # Output: False
$a -in $a, "ghi"           # Output: True
```

### 类型比较,-is

`Powershell`和`.NET`平台绑定, 所以它是一门强类型的脚本. 类型比较运算符, `-is`和`-isnot`用于确定对象是否属于特定的类型.
类型需要写到方括号中, 这里的类型可以是所有合适的`.NET`类型. Syntax:

```powershell
<object> -is <type-reference>
<object> -isnot <type-reference>
```

Example:

```powershell
$a = 1
$b = "1"
$a -is [int]           # Output: True
$a -is $b.GetType()    # Output: False
$b -isnot [int]        # Output: True
$a -isnot $b.GetType() # Output: True
```

## 创建脚本

### 传递参数

+ 传递给一个函数或者一个脚本的参数都保存在`$args`变量中. 默认情况下,传递给一个`Powershell`脚本的参数类型为数组,例如：

```powershell
PS E:> .MyScript.ps1 My Website      Is        www.mossfly.com
Hello,My Website Is www.mossfly.com
```

上面的文本中包含多个连续的空格,可是当脚本把参数输出时却不存在连续的空格了.那是因为脚本会把文本根据白空格截断并转换成数组.
如果不想文本被当成数组那就把它放在引号中.

```powershell
PS E:> ./MyScript.ps1 "My Website      Is        www.mossfly.com"
Hello,My Website      Is        www.mossfly.com
```

+ 因为`$args`是一个数组,自然可以通过索引访问数组的每一个元素. 可以将`MyScript.sp1`的内容改为：

```powershell
For($i=0;$i -lt $args.Count; $i++)
{
    Write-Host "parameter $i : $($args[$i])"
}
```

然后在控制台测试：

```powershell
PS E:> .\MyScript.ps1 www moss fly com
parameter 0 : www
...
```

+ 在脚本中使用参数名

通过`Powershell`传递参数固然方便,但是如果用户不知道参数的传递顺序,也是很郁闷的.
所以最好的方式给参数指定名称,输入以下的脚本：

```powershell
param($Directory,$FileName)

"Directory= $Directory"
"FileName=$FileName"
```

其中`param`给参数指定名称.

执行脚本：

```powershell
PS E:> .\MyScript.ps1 -Directory $env:windir -FileName config.xml
Directory= C:windows
FileName=config.xml
PS E:> .\MyScript.ps1 -FileName config.xml -Directory $env:windir
Directory= C:windows
FileName=config.xml
```

## 流程控制

### Throw异常

[about_Throw](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_throw)
[about_Try_Catch_Finally](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_try_catch_finally)

+ 长描述;  `Throw` 关键字会导致一个`terminating`错误. 你可以使用`Throw`关键字来停止一个命令, 函数或脚本的处理.

例如, 你可以在`If`语句的脚本块中使用`Throw`关键字来响应一个条件, 或者在`Try-Catch-Finally`语句的`Catch`块中使用.
你也可以在一个参数声明中使用`Throw`关键字, 使一个函数参数成为强制性的.
`Throw`关键字可以抛出任何对象, 如用户信息字符串或导致错误的对象.

Throw关键字的语法如下.

```powershell
Throw [<表达式>]
```

`Throw`语法中的表达式是可选的. 当`Throw`语句没有出现在`Catch`块中, 并且不包括表达式时, 它会产生一个`ScriptHalted`错误.

```powershell
C:\PS> throw
Exception: ScriptHalted
```

如果在`Catch`块中使用`Throw`关键字, 且没有表达式, 它会再次抛出当前的`RuntimeException`. 更多信息请参见`about_Try_Catch_Finally`.

+ `Throw` 语句中的可选表达式可以是一个字符串:

```powershell
C:\PS> throw "This is an error."
```

+ 表达式也可以抛出代表PowerShell进程的对象, 如下例所示,

```powershell
C:\PS> throw (get-process Pwsh)
```

### if 条件判断

`Where-Object` 进行条件判断很方便,如果在判断执行代码段,可以使用`IF-ELSEIF-ELSE`语句.语句模板：

```powershell
If (条件1){
如果条件1满足就执行代码
}
elseif (条件2)
{
如果条件2满足执行代码
}
else
{还不满足}
```

条件判断必须放在圆括号中,执行的代码必须紧跟在后面的花括号中.

```powershell
$n=8
if($n -gt 15) {"$n  大于 15 " }
if($n -gt 5) {"$n  大于 5 " }
8  大于 5
if($n -lt 0 ){"-1" } elseif($n -eq 0){"0"} else {"1"}
1
```

### Switch

[about_Switch](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_switch)
[about_Automatic_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)

`If`语句可以检查多种类型的条件, 包括变量的值和对象的属性.
若要检查多个条件, 请使用 `Switch` 语句.  `Switch`语句等效于一系列语句 `If` , 但更简单.
语句 `Switch` 列出每个条件和可选操作.  如果某个条件满足, 则执行该操作. `Switch`语句可以使用`$_ `,  `$switch` 自动变量.

+ `$switch`; 包含`enumerator`而不是`Switch`语句的结果值.
`$switch`变量只在`Switch`语句运行时存在；当`Switch`语句执行完毕时, 它将被删除. 欲了解更多信息, 请参见`about_Switch`.
枚举器包含属性和方法, 你可以用来检索循环值和改变当前循环迭代. 更多信息, 请参见`Using Enumerators`.

#### 基本用法

```powershell
Switch (<test-value>)
{
    <condition> {<action>}
    <condition> {<action>}
}
```

默认情况下, 所有的条件都会被检测,

```powershell
switch (3)
{
    1 {"It is one."}
    2 {"It is two."}
    3 {"It is three."}
    4 {"It is four."}
    3 {"Three again."}
}
```

如果不想检测所有条件, 使用`Break`

```powershell
switch (3)
{
    1 {"It is one."}
    2 {"It is two."}
    3 {"It is three."; Break}
    4 {"It is four."}
    3 {"Three again."}
}
```

如果输入是一个`collection`, 每个元素会按照顺序测试,

```powershell
switch (4, 2)
{
    1 {"It is one." }
    2 {"It is two." }
    3 {"It is three." }
    4 {"It is four." }
    3 {"Three again."}
}
```

`Break`对整个集合生效, 而不是对单个元素

```powershell
switch (4, 2)
{
    1 {"It is one."; Break}
    2 {"It is two." ; Break }
    3 {"It is three." ; Break }
    4 {"It is four." ; Break } # 输出 It is four. 跳出 switch
    3 {"Three again."}
}
```

#### 完全体switch

```powershell
switch [-regex|-wildcard|-exact][-casesensitive] (<value>)
{
    "string"|数字|变量| { 表达式 }    { 执行的操作 }
    default { 执行的操作 }
}
# 或者
switch [-regex|-wildcard|-exact][-casesensitive] -file filename
{
    "string"|数字|变量|{ 表达式 } { 执行的操作 }
    default { 执行的操作 }
}
```

如果没有使用参数, `Switch`的行为相当于使用`Exact`参数.  它对`<value>`进行不区分大小写的`match`.
如果值是一个集合, 按照出现每个元素都的顺序对其运算.

`Switch`语句必须包括至少一个条件语句. 当`<value>`不符合任何条件时, 会触发`Default`子句.
每个`Switch`语句中只允许有一个`Default`子句.

`Switch` 有以下参数;

+ `Wildcard` ; 表示条件是通配符字符串. 如果匹配子句不是字符串, 该参数将被忽略. 比较是不区分大小写的.
+ `Exact` ; 如果匹配子句是字符串, 则必须完全匹配. 如果匹配子句不是字符串, 这个参数将被忽略. 比较是不区分大小写的.
+ `CaseSensitive` ; 执行大小写敏感的匹配. 如果匹配子句不是字符串, 这个参数被忽略.
+ `File` ; 从文件, 而不是从值语句中获取. 如果包含多个`File`参数, 只使用最后参数. 文件的每一行都被读取并由`Switch`语句进行运算, 不分大小写的.
+ `Regex` ; 执行值与条件的正则表达式匹配. 如果匹配子句不是字符串, 这个参数将被忽略. 比较是不区分大小写的. `$matches`自动变量可在匹配语句块中使用.

注意事项 : 当指定冲突的值时, 如`Regex`和`Wildcard`, 最后指定的参数优先, 所有冲突的参数被忽略. 也允许参数的多个实例. 然而, 只有最后使用的参数才是有效的.

在这个例子中, 若传递不是字符串或数字对象给`Switch`. `Switch`对该对象进行强制字符串转换, 再对结果进行运算.

```powershell
$test = @{
    Test  = 'test'
    Test2 = 'test2'
}
$test.ToString() # 输出 System.Collections.Hashtable
switch -Exact ($test)
{
    'System.Collections.Hashtable' {'Hashtable string coercion'}
    'test' {'Hashtable value'}
} # 输出 Hashtable string coercion
```

使用正则表达式的例子, 检测网站协议.

```powershell
$target = 'https://bing.com'
switch -Regex ($target)
{
    '^ftp\://.*$' { "$_ is an ftp address"; Break }
    '^\w+@\w+\.com|edu|org$' { "$_ is an email address"; Break }
    '^(http[s]?)\://.*$' { "$_ is a web address that uses $($matches[1])"; Break }
}
```

`Switch`语句的`statement`条件可以是:

+ 表达式, 它的值将会与输入值进行比较
+ 脚本块, 如果条件得到满足, 它应该返回`$true`.

`$_`自动变量包含传递给`switch`语句的值, 可在条件语句的范围内进行运算和使用. 每个条件的动作都与其他条件的动作无关.
下面的例子演示了使用脚本块作为开关语句的条件.

```powershell
switch ("Test")
{
    {$_ -is [String]} {"Found a string"}
    "Test" {"This $_ executes as well" }
}
```

如果值符合多个条件, 则执行每个条件的动作. 要改变这种行为, 可以使用`Break`或`Continue`关键字.

+ `Break`关键字停止处理并退出`Switch`语句.
+ `Continue` 关键字停止处理当前值, 但继续处理任何后续值.

下面的例子是处理一个数组并显示它们是奇数还是偶数. 负数用`Continue`关键字跳过. 如果遇到一个非数字, 则用`Break`关键字终止执行. `[Int32]`表示`32`位整数类型.

```powershell
switch (1,4,-1,3,"Hello",2,1)
{
    {$_ -lt 0} { Continue }
    {$_ -isnot [Int32] } { Break }
    {$_ % 2} { "$_ is Odd" }
    {-not ($_ % 2)} {"$_ is Even"}
}
```

### for, foreach

+ `for`循环可以看做是`while`循环的另一种形式,常用于固定次数的循环.

```powershell
for ($i = 0; $i -ne 3; $i++) {
    Write-Output $i
}
```

+ `foreach`循环用于遍历一个集合中的所有元素.

[关于 ForEach](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_foreach)
[流控制](https://docs.microsoft.com/zh-cn/powershell/scripting/learn/ps101/06-flow-control)

```powershell
$array = @(1, 2, 3, 4)
foreach ($i in $array) {
    Write-Output $i
}
```

值得一提的是,`foreach-object`语句用在管道上时,还有以下一种用法.

```powershell
<command> | foreach {<beginning command_block>}{<middle command_block>}{<ending command_block>}
```

使用这种方法时,`for-each`后面可以跟三个语句块,第一个语句块是开始语句块,
在循环前执行一次,常用来初始化一些数据；
第三个是结束语句块,在循环结束之后执行一次,常用于统计一些循环数据；
第二个就是正常的循环语句块,会循环多次.

### while循环

`while`循环是先判断循环条件, 满足条件时执行循环.
$i = 0
while ($i -lt 3) {
    Write-Output $i
    $i++
}

## 筛选对象

### 筛选管道中的对象

通过管道可以过滤某些对象和对象的属性,这个功能很实用,因为很多时候我们并不是对所有的结果感兴趣,可能只会对某些结果感兴趣.

+ `Select-Object`：`select`,选取前几个对象如`-First 3`, 或者对象的属性. 可以用它先查看对象都有什么属性,
+ `Where-Object`:  `where`,根据对象的属性, 从对象集合中挑选特定的几个. 例如, 选择在某个日期之后创建的文件, 具有特定ID的事件, 或者使用特定版本Windows的计算机.
+ `ForEach-Object`：`foreach`,对输入对象集合中的每个项目执行操作. 输入对象可以通过管道进入`cmdlet`, 也可以通过使用`InputObject`参数指定.
+ `Get-Uinque`：`gu`,从排序过的列表中返回不重复的对象.

+ `Select-Object -Index`: 根据`index`从一个数组中选择对象. 在一个逗号分隔的列表中输入索引. 数组中的索引从`0`开始, 其中`0`代表第一个值, `n-1`代表最后一个值.

比如过滤正在运行的服务,可以通过每个服务的属性`Status`进行过滤.
首先我们看看服务的属性,可以通过`Format-List *`,也可以通过`Get-memeber`.

```powershell
Get-service | Select-Object -First 1 | Format-List * #Format-List 输出对象的属性, 每行一个
```

找出`Status`为`Running`的程序, 这里是 `where-object` 的脚本块用法

```powershell
get-service | Where-Object {$_.Status -eq "Running"}
```

### 比较操作符

+ `-Contains` 包含

```powershell
Get-Process | where ProcessName -Contains "Svchost"
```

+ `-GE` 大于;

```powershell
Get-Process | Where-Object -Property Handles -GE -Value 1000
Get-Process | where Handles -GE 1000
```

第一条命令使用`comparison`语句格式. 没有使用别名, 并且所有参数前带上了参数名称.
第二个命令是更常用的格式, 使用`where`代替了`Where-Object ` cmdlet, 并且省略了所有可选参数名称.

+ `-Like` 通配符; 如果`property`值与包含通配符的值匹配, 则此cmdlet将获取对象.

```powershell
Get-Process | where ProcessName -Like "*host"
```

```powershell
Get-Process | where ProcessName -Like "*.pdf"
```

+ `-match` 正则表达式;

用`Get-ChildItem`显示当前当前文件的时候,会显示所有文件.有时候我们可能仅仅需要搜索或者过滤部分文件.

首先,如果是比较简单的需求,可以使用`?*`通配符来搞定,`?`用于匹配任意单个字符,`*`用于匹配任意多个字符.
比方说,我想要列出所有`.md`格式的文件,就可以使用下面的命令.

```powershell
Get-ChildItem *.md
```

有时候可能需要使用正则表达式来查找文件,考虑使用 `Get-ChildItem`+`Where-Object`,比如查找所有`.md`格式的文件,
`Where-Object`里面的`$_`是形式变量,代表每次迭代的文件. 如果了解过`C#`的`LINQ`,或者`Java 8`的流类库,应该对这种形式会比较熟悉.

```powershell
Get-ChildItem | Where-Object {$_ -match '\w*.md$'}
```

+ 查找大于`5kb`的所有`.md`格式文件,

```powershell
 Get-ChildItem | Where-Object {$_ -match '\w*.md$' -and $_.Length/1kb -gt 5}
```

这里到了`Powershell`的一个方便的特性,文件大小单位,`KB GB MB TB`等单位都支持.
当然其实并不仅仅可以查询文件大小属性,基本上所有文件信息都可以用来查询.

+ `Get-ChildItem` 不仅可以列出当前文件夹下的内容,还可以递归查询子文件夹. 比如查找文件夹下所有可执行文件:

```powershell
Get-ChildItem -Recurse *.exe
```

通过添加`-Depth`参数, 还可以指定递归深度.

### 拷贝特定文件类型

+ 找出所有`jpg`和`png`

```powershell
Get-ChildItem -Recurse | Where-Object {$_ -match '\w*.jpg$' -or $_ -match '\w*.png$'}
```

+ 找出当前文件夹下所有图片并复制到指定路径

```powershell
Get-ChildItem -Path . -Recurse | Where-Object {$_ -match '\w*.jpg$'} | ForEach-Object {Copy-Item $_.FullName -Destination (-Join("C:\Users\Tom\Desktop\test\",$_.Name)) }
```

+ 找出当前文件夹下所有图片并复制到指定路径,并以父文件夹命名. 若要重命名项目而不复制它, 请使用`Rename-Item`.

```powershell
Get-ChildItem -Path . -Recurse | Where-Object {$_ -match '\w*.jpg$'} |
ForEach-Object {Copy-Item $_.FullName -Destination (-Join("C:\Users\Tom\Desktop\test\",$_.Directory.Name)) }
```

其中`$a[0].Directory.Name` 会给出父文件夹的名字.

## 结构体

### 数组

[about_Arrays](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_arrays)

数组是一种数据结构, 用于存储项的集合.  项可以是同一类型, 也可以是不同的类型.
若要创建一个名为 `$A` 的数组, 该数组包含七个数值 (`int`) 值, 请键入：

```powershell
$A = 22,5,10,8,12,9,80
```

若要创建名为 `$B` , 值为`7`的单项数组, 请键入：

```powershell
$B = ,7
```

还可以通过使用范围运算符 `()` 来创建和初始化数组 .. .  下面的示例创建一个包含值5到8的数组.

```powershell
$C = 5..8
```

如果未指定数据类型, 则 `PowerShell` 会创建`object`的数组 (`system.object []`) .  
若要确定数组的数据类型, 请使用 `GetType ()` 方法.  例如, `$A.GetType()`.

若要创建强类型数组(即只包含特定类型值的数组), 请将该变量强制转换为数组类型, 如 `string[]`, `long[]`, or `int32[]`.
若要强制转换数组, 请在变量名称之前加上`[类型]`.  例如, 若要创建一个`32`位整数数组, 请键入：

```powershell
[int32[]]$ia = 1500,2230,3350,4000
```

`$ia` 数组只能包含整数.

可以创建强制转换为 `.NET` 中任何受支持的类型的数组.  例如,  `Get-Process` 检索以表示进程的对象属于 `system.object` 类型.  若要创建进程对象的强类型数组：

```powershell
[Diagnostics.Process[]]$zz = Get-Process
```

#### 子表达式运算符

数组 sub-expression 运算符根据它内部的语句创建一个数组.  运算符将语句生成的结果放在数组中, 即使是零个或一个对象. 数组运算符的语法如下所示：

```powershell
@( ... )
```

可以使用 `array` 运算符创建零个或一个对象的数组.  例如：

```powershell
$a = @("Hello World");$a.Count
$b = @();$b.Count
```

当获取未知数量的对象时, array operator 很有用. 例如:

```powershell
$p = @(Get-Process Notepad)
```

数组支持常用的切片方法. 还可以使用`+`号来组合指标范围. 例如:

```powershell
#创建数组
$a = 0 .. 9;$a
# 数组切片
$a[0]
$a[1..4]
$a[-3..-1]
$a[0,2+4..6]
$a[+0..2+4..6+8]
```

可以使用`ForEach`, `For`, `While`循环来遍历数组:

```powershell
# for each
$a = 0..9
foreach ($element in $a) {
  $element
}
# for
$a = 0..9
for ($i = 0; $i -le ($a.length - 1); $i += 2) {
  $a[$i]
}
# while
$a = 0..9
$i=0
while($i -lt 4) {
  $a[$i]
  $i++
}
```

#### Rank

返回数组中的维数.  `PowerShell` 中的大多数数组是一维的. 即使您认为生成多维数组, 如以下示例中所示：

```powershell
$a = @(
  @(0,1),
  @("b", "c"),
  @(Get-Process)
)

"`$a rank: $($a.Rank)"
"`$a length: $($a.Length)"
"`$a[2] length: $($a[2].Length)"
"Process `$a[2][1]: $($a[2][1].ProcessName)"
```

在此示例中, 您将创建一个包含其他数组的一维数组.  这也称为jagged array(交错数组).  `Rank` 属性证明这是一维的.
若要访问交错数组中的项, 索引必须位于单独的方括号中, 例如 `$a[2][1]` .

多维数组按`行顺序`(row major order)存储.  下面的示例演示如何创建一个真正的多维数组.

```powershell
[string[,]]$rank2 = [string[,]]::New(3,2)
$rank2.rank
$rank2.Length
$rank2[0,0] = 'a'
$rank2[0,1] = 'b'
$rank2[1,0] = 'c'
$rank2[1,1] = 'd'
$rank2[1,1]
```

若要访问多维数组中的项, 请使用`[1,2,3]`的索引形式.

对多维数组的某些运算(例如复制和串联)要求对数组进行展平.  展平将数组转换为无类型的一维数组. 生成的数组按行顺序列出所有元素.  请考虑以下示例：

```powershell
$a = "red",$true
$b = (New-Object 'int[,]' 2,2)
$b[0,0] = 10
$b[0,1] = 20
$b[1,0] = 30
$b[1,1] = 40
$c = $a + $b
$a.GetType().Name
$b.GetType().Name
```

输出显示的 `$c` 是包含`$a`和`$b`的项的`1`维数组,  按行顺序排列.

#### 获取数组的成员

通过管道将数组发送到`Get-Member`时, `PowerShell` 一次发送一个项, 并返回数组中每个项的类型,  忽略重复项目.

若要获取数组的属性和方法(如`Length`属性和 `SetValue` 方法), 使用 `Get-Member` 的 `InputObject`  参数.
使用 `InputObject` 参数 时, ` Get-Member` 返回`array`的属性, 而不是数组元素的属性和方法. 例如,

```powershell
Get-Member -InputObject $a
```

也可以在`array`前面加上一个`,`逗号, 再管道给`Get-Member`, 即组成一个套娃数组. PowerShell 一次管道一个对象, 所以会返回这个数组的属性.  如下所示.

```powershell
,$a | Get-Member
,(1,2,3) | Get-Member
```

#### 操作数组

可以更改数组中的元素, 将元素添加到数组, 以及将两个数组中的值合并到第三个数组中. 例如:

```powershell
$a[1] = 10
```

还可使用数组的 `SetValue` 方法更改值.  以下示例将数组`$a`的元素`2`更改为`500`：

```powershell
$a.SetValue(500,1)
```

可以使用运算符 `+=` 将元素添加到数组.

```powershell
$a = @(0..4)
$a += 5
```

备注: 使用`+=`运算符时,  `PowerShell` 实际上会创建一个新数组.  如果多次重复操作或数组太大, 则可能会导致性能问题.

从数组中删除元素并不简单, 但可以创建一个新数组, 该数组仅包含现有数组的选定元素.
例如, 若要创建不包含元素 `2` 的新数组,请键入：

```powershell
$t = $a[0,1 + 3..($a.length - 1)]
```

若要将两个数组合并为单个数组, 请使用加号运算符 `+` :

```powershell
$x = 1,3
$y = 5,9
$z = $x + $y
```

数组`$z`包含  `1`, `3`, `5` 和 `9`.

若要删除数组, 请为数组 `$null` 分配值 .  以下命令删除`$a`变量中的数组.

```powershell
$a = $null
```

也可使用`Remove-Item`, 但分配`$null的值速度更快, 尤其是对于大型数组.

#### 零或一的数组

从 `Windows PowerShell 3.0` 开始, 零个或一个对象的集合具有 和 `Count`, `Length` 属性.  此外, 可以索引只有一个对象的数组.
此功能可帮助你避免脚本错误, 如果某个输入为集合的命令, 只获取到两个以下的项目. 以下示例演示了此功能.

```powershell
# 0 个对象
$a = $null
$a.Count
$a.Length
# 1个对象
$a = 4
$a.Count
$a.Length
$a[0]
$a[-1]
```

#### 成员枚举

您可以使用成员枚举从集合的所有成员中获取属性.  当使用成员访问运算符 `.` 作用于数组时, 如果数组对象没有该名称的成员,
则 PowerShell 枚举集合中的项, 并在每个项上查找该成员.  这同时适用于属性和方法成员.

下面的示例创建两个新文件, 并将生成的对象存储在数组变量中 `$files` .  由于数组对象不具有 `LastWriteTime` 成员, 因此返回数组中每个项的 `LastWriteTime`.

```powershell
$files = (New-Item -Type File -Force '/temp/t1.txt'),
         (New-Item -Force -Type File '/temp/t2.txt')
$files.LastWriteTime
```

成员枚举可用于`get`集合中项目的值, 但不能用于`set`项目的值.  例如：

```powershell
$files.LastWriteTime = (Get-Date).AddDays(-1)
InvalidOperation: The property 'LastWriteTime' cannot be found on this object.
Verify that the property exists and can be set.
```

若要设置这些值, 必须使用方法.

```powershell
$files.set_LastWriteTime((Get-Date).AddDays(-1))
$files.LastWriteTime
```

`set_LastWriteTime()` 方法是 `FileInfo` 对象的`hidden`成员.  下面的示例演示如何查找具有隐藏`set`方法的成员 .

```powershell
$files | Get-Member | Where-Object Definition -like '*set;*'
   TypeName: System.IO.FileInfo

Name              MemberType Definition
----              ---------- ----------
...
LastWriteTime     Property   datetime LastWriteTime {get;set;}
LastWriteTimeUtc  Property   datetime LastWriteTimeUtc {get;set;}
```

### 哈希表

[about_Hash_Tables](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_hash_tables)

哈希表也称为字典或关联数组, 是存储一个或多个键/值对的 compact 数据结构.
例如, 哈希表可能包含一系列 `IP` 地址和计算机名称, 其中 `IP` 地址是密钥, 计算机名称是值, 反之亦然.

在 PowerShell 中, 每个哈希表都是`Hashtable`对(`System.Collections.Hashtable`).  可以在 PowerShell 中使用`Hashtable`对象的属性和方法.
从 PowerShell 3.0 开始, 可以使用`[ordered]`属性在 PowerShell 中创建一个排序的字典(`(System.Collections.Specialized.OrderedDictionary)`).

有序哈希表不同于普通哈希表, 它们的键始终按你给出的顺序出现, 普通哈希表中的键顺序不确定.

哈希表中的键和值也是 `.NET` 对象.  它们最常见的是字符串或整数, 但它们可以有任何对象类型.  您还可以创建嵌套哈希表, 令键的值为另一个哈希表.

通常使用哈希表, 因为它们非常适合用于查找和检索数据.  您可以使用哈希表来存储列表, 并在 `PowerShell` 中创建带计算的属性.
此外, `PowerShell` 有一个 `cmdlet`: `ConvertFrom-StringData`, 它将字符串转换为哈希表.

哈希表的语法如下所示：

```powershell
@{ <name> = <value>; [<name> = <value> ] ...}
```

排序字典的语法如下所示：

```powershell
[ordered]@{ <name> = <value>; [<name> = <value> ] ...}
```

#### 创建哈希表

若要创建哈希表, 请遵循以下准则：

+ 使用 `at` 符号 `@`.
+ 将哈希表放在大括号中`{}`.
+ 为哈希表的内容输入一个或多个键/值对, 使用等号 (`=`) 将每个键与其值分隔开.
+ 使用分号 (`;`) 或换行符分隔键/值对.
+ 包含空格的键必须用引号引起来.  值必须是有效的 PowerShell 表达式.  字符串必须用引号引起来, 即使它们不包含空格.
+ 若要管理哈希表, 请将它保存在变量中.
+ 将有序哈希表分配给变量时, 请将`[ordered]`属性置于`@`符号之前.  如果将其放在变量名称之前, 则该命令将失败.

若要使`$hash`变量的值为空的哈希表, 请键入：

```powershell
$hash = @{}
```

创建有序字典, 紧靠在`@` 符号前面放置属性.

```powershell
$hash = [ordered]@{ Number = 1; Shape = "Square"; Color = "Blue"}
```

您可以将有序的字典转换为哈希表, 但不能恢复排序, 即使您清除该变量并输入新值.  若要重新建立顺序, 必须删除并重新创建变量.

```powershell
[hashtable]$hash = [ordered]@{  Number = 1; Shape = "Square"; Color = "Blue"}
$hash
```

#### 显示哈希表

若要显示保存在变量中的哈希表, 请键入变量名称.  默认情况下, 哈希表显示为一个表, 其中包含一个键列和一个用于值的列.

```powershell
$hash
```

哈希表具有`Keys`和`Values`属性.  使用点表示法显示所有键或所有值.

```powershell
$hash.keys
$hash.values
```

每个`key`也是哈希表的属性, 其值为`key`对应的`value`.  使用以下格式来显示属性值.

```powershell
$hashtable.<key>
# 例如：
$hash.Number
```

如果`key`名称与哈希表类型的某个属性名称冲突, 则可以使用 `PSBase` 来访问这些属性.
例如, 如果哈希表中某个`key`的名字为`keys`, 覆盖掉了原本的`keys`属性, 请使用以下语法：

```powershell
$hashtable.PSBase.Keys
```

哈希表具有一个 `Count` 属性, 该属性指示哈希表中的键/值对的数目.

```powershell
$hash.count
```

哈希表表不是数组, 因此不能将整数用作哈希表中的索引, 但可以使用键名来索引到哈希表中.  如果该密钥是一个字符串值, 则将该键名称用引号引起来.

```powershell
$hash["Number"]
```

#### 添加和删除键和值

若要将键和值添加到哈希表, 请使用以下命令格式.

```powershell
$hash["<key>"] = "<value>"
```

例如, 若要添加内容为`"Time"->"Now"`的键值对, 请使用以下语句格式.

```powershell
$hash["Time"] = "Now"
```

还可以通过使用 `System.Collections.Hashtable` 对象的 `Add` 方法, 将键和值添加到哈希表中.  `Add` 方法具有以下语法：

```powershell
Add(Key, Value)
# 例如
$hash.Add("Time", "Now")
```

$hash.Add("Time", "Now")

而且, 您可以使用加法运算符将键和值添加到哈希表中:

```powershell
$hash = $hash + @{Time="Now"}
```

您还可以添加变量中存储的值.

```powershell
$t = "Today"
$now = (Get-Date)
$hash.Add($t, $now)
```

不能使用减法运算符从哈希表中删除键/值对, 但可以使用哈希表对象的 Remove 方法:

```powershell
Remove(Key)
# 例如：
$hash.Remove("Time")
```

可以在 PowerShell 中使用哈希表对象的所有属性和方法, 包括 `Contains`, `Clear``, Clone` 和 `CopyTo`.  有关哈希表对象的详细信息, 请参阅[System.Collections.Hashtable](https://docs.microsoft.com/en-us/dotnet/api/system.collections.hashtable?view=net-5.0).

#### 哈希表中的对象类型

哈希表中的键和值可具有任何 `.NET` 对象类型, 单个哈希表可以具有多个类型的键和值.

下面的语句创建哈希表, 键为进程名称的字符串, 值为进程对象, 并将其保存在 `$p` 变量中.

```powershell
$p = @{"PowerShell" = (Get-Process PowerShell);
"Notepad" = (Get-Process notepad)}
```

您可以显示`$p`中的哈希表  , 并使用`$p.<key-name>`来显示对应的值.

```powershell
$p
...
$p.PowerShell
...
$p.keys | foreach {$p.$_.handles}
441
251
```

哈希表中的键还可以是任意 `.NET` 类型.  下面的语句将一个键/值对添加到变量中的哈希表 $p .  密钥是表示 WinRM 服务的服务对象, 值是该服务的当前状态.

```powershell
$p = $p + @{(Get-Service WinRM) = ((Get-Service WinRM).Status)}
```

您可以使用相同的方法来显示和访问新的键/值对.

```powershell
$p
$p.keys
$p.keys | foreach {$_.name}
```

哈希表中的键和值也可以是哈希表对象:

```powershell
$p = $p + @{"Hash2"= @{a=1; b=2; c=3}}
```

您可以通过使用相同的方法来显示和访问新值.

```powershell

$p
$p.Hash2
$p.Hash2.b
```

#### 对键和值进行排序

哈希表中的项在本质上是无序的.  每次显示键/值对时, 它们的显示顺序可能不同.

尽管不能对哈希表进行排序, 但可以使用哈希表的 `GetEnumerator` 方法枚举键和值, 然后使用 `Sort-Object` cmdlet 对要显示的枚举值进行排序.

```powershell
$p.GetEnumerator() | Sort-Object -Property key
```

也可以按降序排列.

```powershell
$p.getenumerator() | Sort-Object -Property Value -Descending
```

#### 从哈希表创建对象

从 PowerShell 3.0 开始, 可以从`属性`和`属性值`组成的哈希表创建对象. 语法如下：

```powershell
[<class-name>]@{
  <property-name>=<property-value>
  <property-name>=<property-value>
}
```

此方法仅适用于具有 `null` 构造函数的类(即没有参数的构造函数).  对象属性必须是`public`且可设置的(`settable`). 有关详细信息, 请参阅 about_Object_Creation.

#### ConvertFrom-StringData

`ConvertFrom-StringData` Cmdlet 将一个包含`键/值`对的`string`或者`here-string`转换为哈希表.

可以在脚本的数据部分中安全地使用 `ConvertFrom-StringData` , 并将其与`Import-LocalizedData` 结合,  以在`UI`中显示当前区域的用户消息.

此外, 当哈希表中的值包含引号时, `here-string`特别有用.
有关`here-string`的详细信息, 请参阅[about_Quoting_Rules](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_quoting_rules).

下面演示了如何创建一个用户消息的`here-string `, 然后用`ConvertFrom-StringData`它们从字符串转换为哈希表.

```powershell
$string = @"
Msg1 = Type "Windows".
Msg2 = She said, "Hello, World."
Msg3 = Enter an alias (or "nickname").
"@
ConvertFrom-StringData $string
```
