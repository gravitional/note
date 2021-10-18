# 正则表达式

简要说明 ; 描述了PowerShell中的正则表达式.

## 长描述

[正则表达式语言--快速参考]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference

> 注意事项:
>本文将向你展示PowerShell中使用正则表达式的语法和方法, 并没有讨论所有的语法.
>要获得更完整的参考资料, 请参见 [正则表达式语言--快速参考][]

正则表达式是一种用于匹配文本的模式. 它可以由 `字面字符`, `运算符` 和 `其他结构` 组成.

本文演示了 `PowerShell` 中的正则表达式语法. `PowerShell`有几个使用正则表达式的操作符和 `cmdlets`. 你可以在下面的链接中阅读更多关于它们的语法和用法.

+ Select-String
+ -match 和 -replace 运算符
+ -split
+ 带有 `-regex` 选项的`switch`语句

`PowerShell`正则表达式默认是不区分大小写的. 上面显示的每一种方法都有不同的方式来强制大小写敏感.

方法 区分大小写

+ `Select-String`   ; 使用 `-CaseSensitive` 开关
+ `switch` 语句 ;   使用 `-`casesensitive 选项
+ `运算符`  ;   前缀为 'c'  (`-cmatch`, `-csplit`, 或 `-creplace`)

### 字面文本,Character literals

正则表达式可以是一个字面的字符或一个字符串. 该表达式使引擎与指定的文本完全匹配.

```PowerShell
# 这个语句返回true, 因为book包含字符串 "oo".
'book' -match 'oo'
```

## 字符类,Character classes

如果你知道确切的模式, `字面字符`就可以工作, 而字符类允许你不那么具体.

### 字符组

`[character group]`允许你一次匹配任意数量的字符, 而 `[^character group]` 只匹配不在该组中的字符.

```PowerShell
# 如果模式匹配big, bog或者bug, 这个表达式将返回true.
'big' -match 'b[iou]g'
```

如果你要匹配的字符列表包括连字符(`-`), 那么它必须在列表的开头或结尾, 以区别于字符范围表达式.

### 字符范围

`模式`也可以是一系列字符. 字符可以是字母 `[A-Z]`, 数字 `[0-9]`, 甚至是基于ASCII的`[ -~]`(所有可打印的字符).

```PowerShell
# 如果该模式匹配任何2位数的数字, 该表达式将返回真.
42 -match '[0-9][0-9]'
```

### 数字

`\d`字符类将匹配任何十进制的数字. 反之, `\D` 将匹配任何非十进制的数字.

```PowerShell
# 如果这个表达式匹配了一个服务器名称, 那么它就会返回true.
# (Server-01 - Server-99).
'Server-01' -match 'Server-\d\d'
```

### 单词字符

`\w`字符类将匹配任何单词字符 `[a-zA-Z_0-9]`. 要匹配任何非单词字符, 请使用 `\W`.

```PowerShell
# 这个表达式返回true.
# 该模式匹配第一个单词字符'B'.
'Book' -match '\W'
```

### 通配符

句号(`.`)是正则表达式中的一个通配符. 它可以匹配除 `换行符`(`\n`)以外的任何字符.

```PowerShell
# 这个表达式返回真.
# 该模式匹配除换行之外的任何4个字符.
'a1\' -match '....'
```

### 空白

使用 `\s` 字符类匹配空白. 任何非空格的字符都使用 `\S` 匹配. 也可以使用字面的空格字符`' '`.

```PowerShell
# 这个表达式返回true.
# 该模式使用两种方法来匹配空格.
' - ' -match '/s-'
```

## 数量词,Quantifiers

`限定符`控制每个元素在输入字符串中应该有多少个实例.
下面是PowerShell中可用的一些限定符量化器.

限定符 描述

+ `*`   ;   零次或多次.
+ `+`   ;   一次或多次.
+ `?`   ;   零次或一次.
+ `{n,m}`  ;   至少n次, 但不超过m次.

`星号`(`*`)与前一个元素的零次或多次匹配. 其结果是, 即使没有该元素的输入字符串也会被匹配.

```PowerShell
# 这对所有的账户名字符串都返回真, 即使名称不存在.
'ACCOUNT NAME:    Administrator' -match 'ACCOUNT NAME:\s*\w*'
```

`加号`(`+`)与前一个元素匹配一次或多次.

```PowerShell
# 如果它匹配任何服务器名称, 则返回真.
'DC-01' -match '[A-Z]+-\d\d'
```

问号`?` 匹配前一个元素的零次或一次. 和星号`*`一样, 它甚至可以匹配没有元素的字符串.

```PowerShell
# 对于任何服务器名称, 甚至是没有破折号的服务器名称, 这都会返回true.
'SERVER01' -match '[A-Z]+-?\d\d'
```

`{n, m}` 量词可以用几种不同的方式来实现对量词的细化控制. 第二个元素 `m` 和逗号`,`是可选的.

量词描述

+ `{n} `    ;   准确地匹配n个次数.
+ `{n,} `   ;   至少匹配n次.
+ `{n,m}`   ;    匹配n和m之间的次数.

```PowerShell
# 如果匹配任何电话号码, 则返回true.
'111-222-3333' -match '\d{3}-\d{3}-\d{4}'
```

## 锚点,Anchors

`锚点`允许你根据输入字符串中的匹配位置, 使匹配成功或失败.

两个常用的锚点是`^`和`$`. `^`(caret)匹配字符串的开始, `$` 匹配字符串的结束.
`锚点`允许你在一个特定的位置匹配你的文本, 同时还会丢弃不需要的字符.

```PowerShell
# 该模式希望字符串 "fish "是该行的唯一内容.
# 这会返回FALSE.
'fishing' -match '^fish$'
```

>注意

当定义包含`$`锚点的 `regex`时, 一定要用 `单引号`(`'`)而不是 `双引号`(`"`)来包围 `regex`, 否则PowerShell会把 `$xx` 表达式展开为一个变量.

在 `PowerShell` 中使用锚点时, 你应该了解 `单行` 和 `多行` 正则表达式选项之间的区别.

+ `多行` ; `多行模式` 强制 `^` 和 `$` 匹配每一行的开头结尾, 而不是输入字符串的开头和结尾. `.` 字符匹配除 `换行` 以外的每个字符.
+ `单行` ; `单行模式` 将输入字符串作为单行处理. 它强迫 `.` 字符匹配每个字符(包括`换行 \n`), 而不是匹配除 `换行` 以外的每个字符.

要阅读更多关于这些选项和如何使用它们的信息, 请访问 [正则表达式语言--快速参考][].

## 转义字符

`反斜杠`(`\`)用于转义字符, 这样它们就不会被正则表达式引擎解析.

以下字符作为保留字符,

    []  ()  .   \   ^   $   |   ?   *   +   {}

如果你需要匹配它们本身, 你需要在`模式`中转义这些字符.

```PowerShell
# 这将返回true, 并匹配至少有2位精度的数字.
# 小数点是用反斜杠转义的.
'3.141' -match '3\.\d{2,}'
```

`regex` 类的一个静态方法可以为你转义文本.

```PowerShell
[regex]::escape('3.\d{2,}')

Output:
3\.\\d\{2,}
```

>注意
这将转义正则表达式所有的`保留字符`, 包括`字符类`中使用的`反斜线`.
请确保只在模式中需要转义的部分使用它.

### 其他字符转义

还有一些保留的字符转义, 你可以用它来匹配特殊的字符类型.
下面是几个常用的字符转义.

字符转义 描述

+ `\t`  ;   匹配一个制表符
+ `\n`  ;   匹配一个换行符
+ `\r`  ;   匹配一个回车键

## 分组,捕获和替换

`分组构造`将一个输入字符串分成, 被捕获与被忽略的子字符串. 分组的子字符串被称为`子表达式`.
默认情况下, 子表达式被捕获到用数字编号的组中, 尽管你也可以给它们指定名称.

分组结构是一个由圆括号包围的正则表达式. 任何被包围的正则表达式匹配的文本都会被捕获.
下面的例子将把输入文本分成两个捕获组.

```PowerShell
'The last logged on user was CONTOSO\jsmith' -match '(.+was )(.+)'
输出
True
```

使用 `$Matches` Hashtable 自动变量来检索捕获的文本. 键`0`处存储`整个被匹配的文本`.

```PowerShell
$Matches.0
输出
The last logged on user was CONTOSO\jsmith
```

捕获的信息被存储在整数键中, 从左到右增加. 捕获 `1` 包含所有的文本, 直到用户名, 捕获 `2` 只包含用户名.

```PowerShell
$Matches

输出
Name                           Value
----                           -----
2                              CONTOSO\jsmith
1                              The last logged on user was
0                              The last logged on user was CONTOSO\jsmith
```

>重要
> 键`0`是一个整数. 你可以使用任何 `Hashtable` 允许的方法来访问存储的值.

```PowerShell
PS> 'Good Dog' -match 'Dog'
True
PS> $Matches[0]
Dog
PS> $Matches.Item(0)
Dog
PS> $Matches.0
Dog
```

### 命名的捕获

默认情况下, `捕获`是以升序的数字顺序存储的, 从左到右.
你也可以给捕获组指定一个名称. 这个名字会成为 `$Matches` Hashtable自动变量的`键`.

在捕获组内, 使用 `?<keyname>` 将捕获的数据存储在一个命名的键下.

```PowerShell
PS> $string = 'The last logged on user was CONTOSO\jsmith'
PS> $string -match 'was (?<domain>.+)\\(?<user>.+)'
True

PS> $Matches

Name                           Value
----                           -----
domain                         CONTOSO
user                           jsmith
0                              was CONTOSO\jsmith

PS> $Matches.domain
CONTOSO

PS> $Matches.user
jsmith
```

在下面的例子中,  我们保存了 Windows安全日志中最新的日志条目.
所使用的正则表达式从消息中提取了用户名和域, 并将它们存储在键中: `N`代表名字, `D`代表域.

```PowerShell
$log = (Get-WinEvent -LogName Security -MaxEvents 1).message
$r = '(?s).*Account Name:\s*(?<N>.*).*Account Domain:\s*(?<D>[A-Z,0-9]*)'
$log -match $r
输出
True

$Matches
输出
Name                           Value
----                           -----
D                              CONTOSO
N                              jsmith
0                              A process has exited....
```

欲了解更多信息, 请参阅[正则表达式中的分组结构][].

[正则表达式中的分组结构]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/grouping-constructs-in-regular-expressions

## 正则表达式中的替换

使用带有 `-replace` 操作符的正则表达式, 可以使用 `捕获的文本` 动态地替换 `原先的文本`.

    <input> -replace <original>, <substitute>

+ `<input>` ;   要搜索的字符串
+ `<original>` ;    用来搜索输入字符串的正则表达式
+ `<替代>` ;    用来作替换的正则表达式, 用于替换在输入字符串中发现的匹配.

>注意
>`<original>` 和 `<substitute>` 操作符要遵守正则表达式引擎的规则, 如字符转义.

可以在 `<替代>` 字符串中引用 `捕获组`. 通过使用`$`加上组的标识符, 例如默认情况下就是`$0, $1, $2, ...`

有两种方法来引用捕获组, 即通过编号和通过名称.

+ 通过编号 - 捕获组从左到右进行编号.

```PowerShell
'John D. Smith' -replace '(\w+) (\w+)\. (\w+)', '$1.$2.$3@contoso.com'

输出
John.D.Smith@contoso.com
```

+ 通过名字 - 捕获组也可以通过名字来引用.

```PowerShell
'CONTOSO\Administrator' -replace '\w+\\(?<user>\w+)', 'FABRIKAM\${user}'

输出
FABRIKAM\Administrator
```

+ `$&` 表达式代表所有匹配的文本.

```PowerShell
'Gobble' -replace 'Gobble', '$& $&'

输出
Gobble Gobble
```

>注意事项
由于`$`字符被用于`字符串展开`, 你需要用`字面字符串`作为替换表达式(也就是用单引号`'`包裹),
或者在使用`双引号`时转义`$`字符(powershell 的转义字符是 `` ` ``).

```PowerShell
'Hello World' -replace '(\w+) \w+', '$1 Universe'
"Hello World" -replace "(\w+) \w+", "`$1 Universe"

输出
Hello Universe
Hello Universe
```

此外, 如果你想让 `$` 作为一个字面字符, 使用 `$$`, 而不是普通的转义字符.
当使用 `双引号` 时, 仍然要转义所有的 `$` 以避免错误的替换.

```PowerShell
'5.72' -replace '(.+)', '$$$1'
"5.72" -replace "(.+)", "`$`$`$1"

输出
$5.72
$5.72
```

更多信息, 请参阅[正则表达式中的替换](https://docs.microsoft.com/en-us/dotnet/standard/base-types/substitutions-in-regular-expressions).
