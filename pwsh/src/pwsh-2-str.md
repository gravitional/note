# powershell 字符串

[在 PowerShell 中连接字符串](https://www.delftstack.com/zh/howto/powershell/concatenate-strings-using-powershell/)

在 PowerShell 中, 我们通过多种方式实现字符串连接;然而, 虽然 PowerShell 有它自己的内置 concat() 函数(我们也将在后面讨论), 有更直接的方法来连接各种字符串变量.

在枚举所有字符串连接方法之前, 最好先声明一些我们将用作示例的字符串变量.

$str1 = "First"
$str2 = "Second"
$str3 = "Third"
以下是在 PowerShell 中连接字符串的所有可能方法.

在 PowerShell 中使用 + 运算符连接字符串
连接字符串的最基本方法是使用 + 运算符. 如果两个变量都是字符串变量, 则连接仅适用于 + 运算符. 如果 + 运算符用于两个或多个整数类型变量, 脚本将以数学方式处理表达式.

示例代码:

$concatString = $str1 + $str2 + $str3
Write-Output $concatString
输出:

FirstSecondThird
在 PowerShell 中使用分隔符连接字符串
我们不仅必须使用 + 运算符来连接字符串, 还可以使用逗号 (,) 等分隔符. 请记住用双引号 "" 将你的字符串变量括起来, 否则该变量会将你分隔的字符串值解释为列表属性.

示例代码:

$concatString = "$str1 , $str2 , $str3"
Write-Output $concatString
输出:

First , Second , Third
另外, 请记住, 这不仅适用于逗号. 用双引号括起你的字符串值使你的字符串变量成为文字表达式. 所见即所得(所见即所得. )

示例代码:

$concatString = "Who came in $str1 , $str2 , and $str3 Place?"
Write-Output $concatString
输出:

Who came in First , Second, and Third Place?
在 PowerShell 中连接字符串和整数
如果我们尝试将字符串与整数连接起来, 则会发生无效类型错误. 在下面的示例中, 新变量 $concatString 将采用 $int1 变量的类型作为其数据类型, 即整数. 发生这种情况是因为在表达式 $int1 + $str1 中首先调用了 $int1.

示例代码:

$int1 = 0
$concatString = $int1 + $str1 #int variable before string variable
Write-Output $concatString
输出:

Cannot convert value "First" to type "System.Int32". Error: "Input string was not in a correct format."
At line:3 char:1
+ $concatString = $int1 + $str1
+ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    + CategoryInfo          : InvalidArgument: (:) [], RuntimeException
    + FullyQualifiedErrorId : InvalidCastFromStringToInteger
如果我们将字符串变量 $str1 放在表达式的最前面, 连接将成功, $int1 将自动转换为字符串值.

示例代码:

$int1 = 0
$concatString = $str1 + $int1 #str variable before int variable
Write-Output $concatString
输出:

First0
在 PowerShell 中使用字符串替换连接字符串
或者, 我们可以使用字符串替换执行另一种连接方式. 当连接具有不同数据类型的字符串时, 此方法也将起作用.

示例代码:

$int1 = 0
$concatString = "$($int1) , $($str2)"
Write-Output $concatString
输出:

0 , Second
在 PowerShell 中使用 -f 运算符连接字符串
连接字符串的另一种方法是使用 -f 运算符. -f 运算符使用传递字符串变量作为预构建字符串值的参数.

示例代码:

$concatString = "{0}.{1}.{2}." -f $str1,$str2,$str3
Write-Output $concatString
输出:

First.Second.Third
在 PowerShell 中使用 -join 运算符连接字符串
-join 运算符与分隔符的作用类似, 因为我们需要传入一个字符串分隔符作为参数.

示例代码:

$concatString = $str1,$str2,$str3 -join "!"
Write-Output $concatString
输出:

First!Second!Third
在 PowerShell 中使用 String Builder 和 append() 函数连接字符串
字符串构建器是连接字符串的复杂方法之一, 因为我们需要调用一个单独的对象类型 System.Text.StringBuilder 来启动这个过程. 然后该对象使用 append() 函数连接字符串, 然后使用 ToString() 函数将新对象转换回字符串值.

示例代码:

$concatString = New-Object -TypeName System.Text.StringBuilder
$null = $concatString.Append($str1)
$null = $concatString.Append($str2)
$null = $concatString.Append($str3)
$concatString = $concatString.ToString()

Write-Output $concatString
输出:

FirstSecondThird
在 PowerShell 中使用 concat() 函数连接字符串
最后, 我们有 PowerShell 自己的 concat() 函数.

示例代码:

$concatString = [System.String]::Concat($str1,".",$str2,".",$str3)
Write-Output $concatString
输出:

First.Second.Third
