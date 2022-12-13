# 结构体

## 数组

[about_Arrays](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_arrays)
[Everything you wanted to know about arrays](https://learn.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-arrays)

数组是一种数据结构, 用于存储项的集合.  `项` 可以是同一类型, 也可以是 `不同类型`.
若要创建名为 `$A` 的数组, 该数组包含七个数值 (`int`) 值, 请键入:

```powershell
$A = 22,5,10,8,12,9,80
```

若要创建名为 `$B` , 值为`7`的单项数组, 请键入:

```powershell
$B = ,7
```

还可以通过使用范围运算符 `..` 来创建和初始化数组.
下面的示例创建包含值 `5` 到 `8` 的数组.

```powershell
$C = 5..8
```

如果未指定数据类型, 则 `PowerShell` 会创建`object`的数组 (`system.object []`) .
若要确定数组的数据类型, 请使用 `GetType ()` 方法.  例如, `$A.GetType()`.

We can create an array and seed it with values just by placing them in the @() parentheses.

```PowerShell
$data = @('Zero','One','Two','Three')
$data.count
$data
```

This array has 4 items. When we call the $data variable,
we see the list of our items. If it's an array of strings, then we get one line per string.

We can declare an array on multiple lines. The comma is optional in this case and generally left out.

```PowerShell
$data = @(
    'Zero'
    'One'
    'Two'
    'Three'
)
```

I prefer to declare my arrays on multiple lines like that.
Not only does it get easier to read when you have multiple items,
it also makes it easier to compare to previous versions when using source control.

若要创建强类型数组(即只包含特定类型值的数组), 请将该变量强制转换为数组类型, 如 `string[]`, `long[]`, or `int32[]`.
若要强制转换数组, 请在变量名称之前加上`[类型]`.  例如, 若要创建`32`位整数数组, 请键入:

```powershell
[int32[]]$ia = 1500,2230,3350,4000
```

`$ia` 数组只能包含整数.

可以创建强制转换为 `.NET` 中任何受支持的类型的数组.
例如,  `Get-Process` 检索以表示进程的对象属于 `system.object` 类型.  若要创建进程对象的强类型数组:

```powershell
[Diagnostics.Process[]]$zz = Get-Process
```

### 子表达式运算符@()

数组 sub-expression 运算符根据它内部的 `语句` 创建 `数组`.
运算符将语句生成的结果放在数组中, 即使是零个或一个对象. 数组运算符的语法如下所示:

```powershell
@( ... )
```

可以使用 `array` 运算符创建零个或一个对象的数组.  例如:

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

### Rank

返回数组中的维数.  `PowerShell` 中的大多数数组是一维的.
即使您认为生成多维数组, 如以下示例中所示:

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

在此示例中, 您将创建包含其他数组的 `一维` 数组.
这也称为 jagged array(交错数组).  `Rank` 属性证明这是一维的.
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

对多维数组的某些运算(例如复制和串联)要求对数组进行展平.  展平将数组转换为无类型的一维数组. 生成的数组按行顺序列出所有元素.  请考虑以下示例:

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

### 获取数组的成员

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

### 操作数组

可以更改数组中的元素, 将元素添加到数组, 以及将两个数组中的值合并到第三个数组中. 例如:

```powershell
$a[1] = 10
```

还可使用数组的 `SetValue` 方法更改值.  以下示例将数组`$a`的元素`2`更改为`500`:

```powershell
$a.SetValue(500,1)
```

可以使用运算符 `+=` 将元素添加到数组.

```powershell
$a = @(0..4)
$a += 5
```

备注: 使用`+=`运算符时,  `PowerShell` 实际上会创建一个新数组.
如果多次重复操作或数组太大, 则可能会导致性能问题.

从数组中删除元素并不简单, 但可以创建 `新数组`, 该数组仅包含现有数组的选定元素.
例如, 若要创建不包含元素 `2` 的新数组,请键入:

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

### 零或一的数组

从 `Windows PowerShell 3.0` 开始, 零个或一个对象的集合具有 和 `Count`, `Length` 属性.
此外, 可以索引只有一个对象的数组.
此功能可帮助你避免脚本错误, 如果某个输入为集合的命令, 只获取到两个以下的项目.
以下示例演示了此功能.

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

### 成员枚举

您可以使用 `成员枚举` 从集合的所有成员中获取`属性`.
当使用成员访问运算符 `.` 作用于数组时, 如果数组对象没有该名称的成员,
则 PowerShell 枚举集合中的项, 并在每个项上查找该成员.
这同时适用于 `属性` 和 `方法` 成员.

下面的示例创建两个新文件, 并将生成的对象存储在数组变量中 `$files` .
由于数组对象不具有 `LastWriteTime` 成员, 因此返回数组中每个项的 `LastWriteTime`.

```powershell
$files = (New-Item -Type File -Force '/temp/t1.txt'),
         (New-Item -Force -Type File '/temp/t2.txt')
$files.LastWriteTime
```

成员枚举可用于`get`集合中项目的值, 但不能用于 `set` 项目的值.  例如:

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

`set_LastWriteTime()` 方法是 `FileInfo` 对象的`hidden`成员.
下面的示例演示如何查找具有隐藏`set`方法的成员 .

```powershell
$files | Get-Member | Where-Object Definition -like '*set;*'
   TypeName: System.IO.FileInfo

Name              MemberType Definition
----              ---------- ----------
...
LastWriteTime     Property   datetime LastWriteTime {get;set;}
LastWriteTimeUtc  Property   datetime LastWriteTimeUtc {get;set;}
```

### `Where()`

[关于数组的各项须知内容](https://learn.microsoft.com/zh-cn/powershell/scripting/learn/deep-dives/everything-about-arrays)
[PowerShell过滤数组中的空值](https://www.pstips.net/remove-null-from-array.html)

数组中有一个 `Where()` 方法, 允许你为筛选器指定一个 `scriptblock`.

```PowerShell
$data.Where({$_.FirstName -eq 'Kevin'})
```

由于 `$null` 会被默认转型成 `False`, 所以从数组中过滤空元素可以写成

```powershell
$array="abc",3,8,$null,10
$array=$array | Where-Object { $_ -ne $null }
# 或者简写成; PowerShell 4.0 以上
$array.Where( {$_ })
```

### 数组切片

[Generating array slices](https://learn.microsoft.com/en-us/powershell/scripting/lang-spec/chapter-07?view=powershell-7.3#7145-generating-array-slices)

When primary-expression designates an object of a type that is enumerable (§4) or a Hashtable, and expression is a 1-dimensional array, the result is an array slice (§9.9) containing the elements of primary-expression designated by the elements of expression.

In the case of a Hashtable, the array slice contains the associated values to the keys provided, unless no such key exists, in which case, the corresponding element is $null. If $null is used as any key name the behavior is implementation defined.
Examples:

```PowerShell

$a = [int[]](30,40,50,60,70,80,90)
$a[1,3,5]                 # slice has Length 3, value 40,60,80
++$a[1,3,5][1]            # preincrement 60 in array 40,60,80
$a[,5]                    # slice with Length 1
$a[@()]                   # slice with Length 0
$a[-1..-3]                # slice with Length 0, value 90,80,70
$a = New-Object 'int[,]' 3,2
$a[0,0] = 10; $a[0,1] = 20; $a[1,0] = 30
$a[1,1] = 40; $a[2,0] = 50; $a[2,1] = 60
$a[(0,1),(1,0)]           # slice with Length 2, value 20,30, parens needed
$h1 = @{ FirstName = "James"; LastName = "Anderson"; IDNum = 123 }
$h1['FirstName']          # the value associated with key FirstName
$h1['BirthDate']          # no such key, returns $null
$h1['FirstName','IDNum']  # returns [object[]], Length 2 (James/123)
$h1['FirstName','xxx']    # returns [object[]], Length 2 (James/$null)
$h1[$null,'IDNum']        # returns [object[]], Length 1 (123)
```

Windows PowerShell: When expression is a collection of two or more key names,
if $null is used as any key name that key is ignored and has no corresponding element in the resulting array.
