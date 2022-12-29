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

备注:

+ `.\`运算符用于执行一个脚本或命令.  如果执行的是`Powershell`脚本,那么脚本会在自己的作用域中执行, 也就是说在当前环境下无法访问被执行的脚本中的变量.
+ `&`默认键入一个字符串,`powershell`会将它原样输出,如果该字符串是一个`命令`或者`外部程序`,在字符串前加"`&`"可以执行命令,或者启动程序.
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
与圆括号`()`的明显区别是, 当表达式出现字符串中时, 也能"激活计算". 例如:

```powershell
# 单纯圆括号不输入日期.
"Today is (Get-Date)"; "Today is $(Get-Date)"
"Folder list: $((Get-ChildItem C:\ -Directory).Name -join ', ')"
Folder list: Program Files, Program Files (x86), Users, Windows
```

### 常用运算符@()

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

## 正则表达式

[about_Regular_Expressions](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_regular_expressions)

正则表达式是一种用于匹配文本的模式. 它可以由文本字符, 运算符和其他构造组成.
`PowerShell` 具有多个使用正则表达式的运算符和 cmdlet.
可以在以下链接中阅读有关其语法和用法的详细信息.

+ `Select-String`
+ `-match` 和 `-replace` 运算符, `-notmatch`
+ `-split`
+ 带 `-regex` 选项的 `switch` 语句

默认情况下, `PowerShell` 正则表达式不区分大小写.  上面所示方法可以通过选项强制区分大小写.

方法     区分大小写

+ Select-String     使用 `-CaseSensitive` 开关
+ switch 语句     使用 `-casesensitive` 选项
+ 运算符     前缀为 `c` (`-cmatch`, `-csplit`, 或 `-creplace`)

### 查找字符串

[Select-String](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.utility/select-string)

`Select-String `: 查找字符串和文件中的文本. 别名为`sls`.

+ `-InputObject`; 指定要搜索的文本. 输入一个包含该文本的变量, 或者输入命令或表达式.

使用`InputObject`参数与通过管道向`Select-String`发送字符串是不同的.
当你用管道向`Select-String cmdlet`发送一系列字符串时,
它在每个字符串中搜索指定的文本, 并返回包含搜索文本的每个字符串.
当你使用`InputObject`参数来提交一个字符串的集合时, `Select-String`将该集合视为一个组合字符串.
如果`Select-String`在任何一个字符串中搜索到指定文本, 它会将这些字符串作为一个单元返回.

+ `-Path`; 指定要搜索的文件的路径. 允许使用通配符. 默认位置是本地目录.
指定目录中的文件, 如`log1.txt`, `*.doc`, 或`*.*`. 如果你只指定一个目录, 命令会失败.

```powershell
Select-String -Path .\*.txt -Pattern 'Get-'
```

### -eq,equality运算符

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

+ `包含 `和 `type ` 类型运算符总是返回一个`布尔值`
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

当左手边是一个`集合`时, `-eq`返回那些与右手边匹配的成员, 而`-ne`则过滤掉它们.

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

## match运算符

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

### -like,-notlike

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

### -match,-notmatch

`-match` 和 `-notmatch` 使用正则表达式来搜索左侧值中的模式.
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

## -replace

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

### 捕获组

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

### 替换集合

当`-replace`的`<input>`是一个集合时, `PowerShell`会将替换应用到集合中的每个值. 比如说

```powershell
"B1","B2","B3","B4","B5" -replace "B", 'a'
a1
a2
a3
a4
a5
```

### 用脚本块替换

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

## 包含运算符

包含运算符(`-contains`, `-notcontains`, `-in`, 和 `-notin`)与`equality `运算符相似,
只是它们总是返回一个布尔值, 即使输入是一个集合.
这些运算符在检测到第一个匹配时就停止比较, 而`equality `运算符则是计算所有输入的成员.
在一个非常大的集合中, 这些运算符比 `equality ` 运算符返回得更快.

### -contains,-notcontains

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

### -in and -notin

`PowerShell 3`中引入了 `-in` 和 `-notin` 操作符, 作为`-contains`和`-notcontains`操作符的反向语法.
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

## 类型比较,-is

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
