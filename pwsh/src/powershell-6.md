# learn.powershell.6.md

[收集和分享 Windows PowerShell 相关教程,技术和最新动态](https://www.pstips.net/)
版权归原作者所有

## Powershell tricks

### 转义通配符

当你使用`–like`操纵符, 它支持`3`个通配符, 
”`*`“代表任意个数的字符, ”`?`“代表一个字符, “`[a-z]`”代表一个字符列表. 

然而`pwsh`中转义字符“`”(反引号)很少有人知道, 它可以转义通配符. 

所以, 当你想验证“`*`”是否在某个字符串中, 你可能会写成这样, 实际上这样恰恰错了. 

```powershell
'*abc' -like '*abc'
```

之所以错, 是因为下面的字符串也会被匹配返回`true`：

```powershell
'xyzabc' -like '*abc'
```

因此如果要验证 “`*`”, 一定得保证它不是通配符, 此时就需要转义字符`` ` ``：

```powershell
PS> '*abc' -like '`*abc'
True
```

```powershell
PS> 'xyzabc' -like '`*abc'
False
```

当你想匹配反引号时, 同样也需要转义字符

```powershell
# 错误:
PS> "xyzabc" -like "`*abc"
True

# 正确:
PS> "xyzabc" -like "``*abc"
False

PS> "*abc" -like "``*abc"
True
```

### 任意键退出

`pwsh`控制台退出,推荐3种方法：

+ 直接鼠标关闭窗口
+ 输入命令`exit` 退出
+ `Stop-Process -Id $pid` 退出

但是有时需要向用户提示并退出, 推荐`2`种方法：

#### 使用read-host和exit命令

```powershell
"Any key to exit"  ;
 Read-Host | Out-Null ;
Exit
```

缺点：只有输入回车后才能退出控制台. 

#### 使用C#中的readkey方法

```powershell
"Any key to exit." ;
[Console]::Readkey() |　Out-Null ;
Exit ;
```

这些命令都可以写在同一行, 使用分号隔开. 

除了`Ctrl`, `Alt`,  `Shift` 等键, 其它基本都可以识别并退出控制台.

### 短斜杠

在`pwsh`中短斜杠是个特殊字符, 如果一个函数名中包含了特殊字符就应当把它放在花括号中. 

### 缩短文件路径

将很长的文件路径缩短, 并且在`pwsh`和`windows`中能够识别. 下面有一个函数：

```powershell
function Get-ShortPath($Path)
{
    $code = @'
    [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError=true)]
    public static extern uint GetShortPathName(string longPath,
    StringBuilder shortPath,uint bufferSize);
'@
    $API = Add-Type -MemberDefinition $code -Name Path -UsingNamespace System.Text -PassThru
    $shortBuffer = New-Object Text.StringBuilder ($Path.Length * 2)
    $rv = $API::GetShortPathName( $Path, $shortBuffer, $shortBuffer.Capacity )
    if ($rv -ne 0)
    {
        $shortBuffer.ToString()
    }
    else
    {
        Write-Warning "Path '$path' not found."
    }
}
#测试

PS C:mossfly> md "Powershell 缩短文件路径缩短文件路径缩短文件路径缩短文件路径缩短文件路径"

    目录: C:mossfly

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----         2012/3/30     23:10      Powershell 缩短文件路径缩短文件路径缩短文件路径缩短文件路径缩短文件路径

PS C:mossfly> Get-ShortPath "Powershell 缩短文件路径缩短文件路径缩短文件路径缩短文件路径缩短
文件路径" POWERS~1
PS C:mossfly> Test-Path POWERS~1
True
PS C:mossfly> cd POWERS~1
PS C:mossflyPowershell 缩短文件路径缩短文件路径缩短文件路径缩短文件路径缩短文件路径>
```

### 获取星期和月份

```powershell
PS C:> [Enum]::GetNames([DayOfWeek])

PS C:> [Globalization.DatetimeFormatInfo]::CurrentInfo.MonthNames
```

### 朗读文本

通过`.net`对象`System.Speech.Synthesis.SpeechSynthesizer`
可以朗读文本, 可以调节朗读的语速和音量, 还可以将文本转换成音频. 

```powershell
# 添加 System.speech.dll 引用
Add-Type -AssemblyName System.speech
# 创建 SpeechSynthesizer 对象
$syn=New-Object System.Speech.Synthesis.SpeechSynthesizer
$syn.Speak("飞苔博客")

#设置朗读的语速
$syn.Rate=-5
$syn.Speak("飞苔博客")

#设置朗读的音量
$syn.Volume=80
$syn.Speak("飞苔博客")

#将文本转换成音频
$syn.SetOutputToWaveFile("e:a.wav")
$syn.Speak("飞苔博客")
```

### 算术运算符主题

说明在 `pwsh` 中执行算术运算的运算符. 

#### 详细说明

算术运算符计算数字值. 
可以使用一个或多个算术运算符对值进行加, 减, 乘和除运算, 还可以计算除法运算中的余数(模). 
此外, 加法运算符 (`+`) 和乘法运算符 (`*`) 还可对字符串, 数组和哈希表进行运算. 

+ 加法运算符将输入连接起来. 
+ 乘法运算符返回输入的多个副本. 
+ 甚至可以在一个算术语句中混合使用对象类型. 
+ 用于计算语句的方法由表达式最左边的对象的类型确定. 

`pwsh` 支持以下算术运算符：

|运算符| 说明| 示例|
|-----|----- |-----|
| `+` |将整数相加；连接字符串, 数组和哈希表. | `6+2, “file” + “name”`
| `–` |用一个值减去另一个值. | `6-2, (get-date).date – 1`|
| `–` |对数字取负. | `-6+2==-4`|
| `*` |将整数相乘；按指定次数复制字符串和数组 |`6*2 “w” * 3`|
|`/`| 将两个值相除. | `6/2` |
| `%` |返回除法运算的余数. | `7%2` |

#### 运算符优先级

`pwsh` 按照以下顺序处理算术运算符：

+ `()` 圆括号
+ `–` (负数)
+ `*, /, %`
+ `+, -` (减法)

`pwsh` 根据优先级规则从左到右处理表达式. 以下示例演示优先级规则的效果：

```powershell
C:PS> 3+6/3*4
11

C:PS> 10+4/2
12

C:PS> (10+4)/2
7

C:PS> (3+3)/ (1+1)
3
```

`pwsh` 计算表达式的顺序可能与您使用过的其他编程和脚本语言不同. 
下面的示例演示一条复杂赋值语句.

```powershell
C:PS> $a = 0
C:PS> $b = 1,2
C:PS> $c = -1,-2

C:PS> $b[$a] = $c[$a++]

C:PS> $b
1
-1
```

在此示例中, 

```powershell
$c[$a++] --> $c[0]==-1
$a++ --> $a==1
$b[$a] --> $b[1]
$b[1] =-1
```

表达式 `$a++` 在 `$c[$a++]` 之前进行计算. 
计算 `$a++` 会更改 `$a` 的值. 
`$b[$a]` 中的变量 `$a` 等于 `1`(而不是 `0`), 因此该语句对 `$b[1]`(而不是 `$b[0]`)赋值. 

#### 对非数字类型进行加法和乘法运算

可以对数字, 字符串, 数组和哈希表进行加法运算. 
也可以对数字, 字符串和数组进行乘法运算. 
但是, 不能对哈希表进行乘法运算. 
在对字符串, 数组或哈希表进行加法运算时, 会将元素连接起来. 
在连接集合(如数组或哈希表)时, 会创建包含两个集合中的对象的新对象. 
如果试图连接具有相同键的哈希表, 则运算会失败. 

例如, 下面的命令创建两个数组, 然后将这两个数组相加：

```powershell
C:PS> $a = 1,2,3
C:PS> $b = "A","B","C"
C:PS> $a + $b
1
2
3
A
B
C
```

此外, 还可以对不同类型的对象执行算术运算.  `pwsh` 执行的运算由运算中最左边的对象的 `.NET Framework` 类型确定. 
`pwsh` 会尝试将运算中的所有对象都转换为第一个对象的 `.NET Framework` 类型. 
如果转换对象成功, 则执行适用于第一个对象的`.NET Framework`类型的运算. 如果在转换任何对象时失败, 则运算失败. 

下面的示例演示在包含不同对象类型的运算中使用加法和乘法运算符：

```powershell
C:PS> "file" + 16
file16

C:PS> $array = 1,2,3
C:PS> $array + 16
1
2
3
16

C:PS> $array + "file"
1
2
3
file

C:PS> "file" * 3
filefilefile
```

因为用于计算语句的方法由最左边的对象确定, 
所以 `pwsh` 中的加法和乘法不是严格可交换的. 
例如, `(a + b)` 并不总是等于 `(b + a)`, 而 `(a * b)`也不总是等于`(b * a)`. 

以下示例演示这一原则：

```powershell
C:PS> "file" + 2
file2

C:PS> 2 + "file"
Cannot convert value "file" to type "System.Int32".Error:

C:PS> "file" * 3
filefilefile

C:PS> 3 * "file"
Cannot convert value "file" to type "System.Int32".Error:
```

哈希表的情况稍有不同. 可以对哈希表进行加法运算. 
也可以将哈希表与数组相加. 但是, 不能将任何其他类型与哈希表相加. 

下面的示例演示如何将哈希表相加以及如何与其他对象相加：

```powershell
C:PS> $array = 1,2,3
C:PS> $array + $hash1
1
2
3

Name                           Value
----                           -----
a                              1
b                              2
c                              3

C:PS> $sum = $array + $hash1
C:PS> $sum.count
4

C:PS> $sum[3]
Name                           Value
----                           -----
a                              1
b                              2
c                              3

PS C:ps-test> $sum + $hash2
1
2
3

Name                           Value
----                           -----
a                              1
b                              2
c                              3
c2                             Server02
```

下面的示例演示不能对包含相同键的哈希表进行加法运算:

```powershell
C:PS> $hash1 = @{a=1; b=2; c=3}
C:PS> $hash2 = @{c="red"}
C:PS> $hash1 + $hash2
Bad argument to operator '+': Item has already been added.Key
```

尽管加法运算符十分有用, 在向哈希表和数组添加元素时, 请使用赋值运算符. 
有关详细信息, 请参阅`about_assignment_operators`. 
以下示例使用 `+=` 赋值运算符将项添加到数组：

```powershell
C:PS> $array
1
2
3

C:PS> $array + "file"
1
2
3
file

C:PS> $array
1
2
3

C:PS> $array += "file"
C:PS> $array
1
2
3
file

C:PS> $hash1

Name                           Value
----                           -----
a                              1
b                              2
c                              3

C:PS> $hash1 += @{e = 5}
C:PS> $hash1

Name                           Value
----                           -----
a                              1
b                              2
e                              5
c                              3
```

`pwsh` 自动选择最适于表示结果而不丢失精度的 `.NET Framework` 数值类型. 例如：

```powershell
C:PS> 2 + 3.1
5.1
C:PS> (2).GetType().FullName
System.Int32
C:PS> (2 + 3.1).GetType().FullName
System.Double
```

如果运算结果超出类型的表示范围, 则扩大结果的类型以容纳结果, 如下面的示例所示：

```powershell
C:PS> (512MB).GetType().FullName
System.Int32
C:PS> (512MB * 512MB).GetType().FullName
System.Double
```

结果的类型不一定与其中某个操作数相同. 
在下面的示例中, 负值不能转换为无符号整数, 而无符号整数太大, 不能转换为`Int32`：

```powershell
C:PS> ([int32]::minvalue + [uint32]::maxvalue).gettype().fullname
System.Int64
```

在此示例中, `Int64`可以容纳这两个类型. 

`System.Decimal`类型是一个例外. 如果任意一个操作数是 `Decimal` 类型, 则结果将是 `Decimal` 类型. 如果结果超出 `Decimal` 类型的表示范围, 将不会转换为 `Double` . 而会生成错误. 

```powershell
C:PS> [Decimal]::maxvalue
79228162514264337593543950335
C:PS> [Decimal]::maxvalue + 1
Value was either too large or too small for a Decimal.
```

#### 算术运算符和变量

此外, 还可以将算术运算符用于变量. 
运算符对变量的值执行运算. 下面的示例演示将算术运算符用于变量：

```powershell
C:PS> $intA = 6
C:PS> $intB = 4
C:PS> $intA + $intB
10

C:PS> $a = "Windows "
C:PS> $b = "PowerShell "
C:PS> $c = 2
C:PS> $a + $b + $c
Windows PowerShell 2
```

#### 算术运算符和命令

在表达式中, 算术运算符通常用于数字, 字符串和数组. 
不过, 算术运算符也可以用于命令返回的对象以及这些对象的属性. 
下面的示例演示如何通过 `pwsh` 命令在表达式中使用算术运算符：

```powershell
C:PS> get-date
Wednesday, January 02, 2008 1:28:42 PM

C:PS> $day = new-timespan -day 1
C:PS> get-date + $day
Thursday, January 03, 2008 1:34:52 PM

C:PS> get-process | where {($_.ws * 2) -gt 50mb}
Handles  NPM(K)    PM(K)      WS(K) VM(M)   CPU(s)     IdProcessName
-------  ------    -----      ----- -----   ------     -- -----------
  1896      39    50968      30620   264 1,572.55   1104 explorer
  12802      78   188468      81032   753 3,676.39   5676 OUTLOOK
...
```

#### 示例

下面的示例演示如何在` Windows PowerShell` 中使用算术运算符：

```powershell
C:PS> 1 + 1
2

C:PS> 1 - 1
0

C:PS> 6 * 2
12

C:PS> 7 / 2
3.5

C:PS> 7 % 2
1

C:PS> w * 3
www

C:PS> 3 * "w"
Cannot convert value "w" to type "System.Int32"...

PS C:ps-test> "Windows" + " " + "PowerShell"
Windows PowerShell

PS C:ps-test> $a = "Windows" + " " + "PowerShell"
PS C:ps-test> $a
Windows PowerShell

C:PS> $a[0]
W

C:PS> $a = "TestFiles.txt"
C:PS> $b = "C:Logs"
C:PS> $b + $a
C:LogsTestFiles.txt

C:PS> $a = 1,2,3
C:PS> $a + 4
1
2
3
4

C:PS> $servers = @{0 = "LocalHost"; 1 = "Server01"; 2 = "Server02"}
C:PS> $servers + @{3 = "Server03"}
Name Value
---- -----
3 Server03
2 Server02
1 Server01
0 LocalHost

C:PS> $servers
Name Value
---- -----
2 Server02
1 Server01
0 LocalHost

C:PS> $servers += @{3 = "Server03"} #Use assignment operator
C:PS> $servers
Name Value
---- -----
3 Server03
2 Server02
1 Server01
0 LocalHost
```

另请参阅

+ `about_arrays`
+ `about_assignment_operators`
+ `about_comparison_operators`
+ `about_hash_tables`
+ `about_operators`
+ `about_variables`
+ `Get-Date`
+ `New-TimeSpan`
