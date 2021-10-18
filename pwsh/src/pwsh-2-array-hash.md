# 结构体

## 数组

[about_Arrays](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_arrays)

数组是一种数据结构, 用于存储项的集合.  项可以是同一类型, 也可以是不同的类型.
若要创建一个名为 `$A` 的数组, 该数组包含七个数值 (`int`) 值, 请键入:

```powershell
$A = 22,5,10,8,12,9,80
```

若要创建名为 `$B` , 值为`7`的单项数组, 请键入:

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
若要强制转换数组, 请在变量名称之前加上`[类型]`.  例如, 若要创建一个`32`位整数数组, 请键入:

```powershell
[int32[]]$ia = 1500,2230,3350,4000
```

`$ia` 数组只能包含整数.

可以创建强制转换为 `.NET` 中任何受支持的类型的数组.  例如,  `Get-Process` 检索以表示进程的对象属于 `system.object` 类型.  若要创建进程对象的强类型数组:

```powershell
[Diagnostics.Process[]]$zz = Get-Process
```

### 子表达式运算符@()

数组 sub-expression 运算符根据它内部的语句创建一个数组.  运算符将语句生成的结果放在数组中, 即使是零个或一个对象. 数组运算符的语法如下所示:

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

返回数组中的维数.  `PowerShell` 中的大多数数组是一维的. 即使您认为生成多维数组, 如以下示例中所示:

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

备注: 使用`+=`运算符时,  `PowerShell` 实际上会创建一个新数组.  如果多次重复操作或数组太大, 则可能会导致性能问题.

从数组中删除元素并不简单, 但可以创建一个新数组, 该数组仅包含现有数组的选定元素.
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

### 成员枚举

您可以使用成员枚举从集合的所有成员中获取属性.  当使用成员访问运算符 `.` 作用于数组时, 如果数组对象没有该名称的成员,
则 PowerShell 枚举集合中的项, 并在每个项上查找该成员.  这同时适用于属性和方法成员.

下面的示例创建两个新文件, 并将生成的对象存储在数组变量中 `$files` .  由于数组对象不具有 `LastWriteTime` 成员, 因此返回数组中每个项的 `LastWriteTime`.

```powershell
$files = (New-Item -Type File -Force '/temp/t1.txt'),
         (New-Item -Force -Type File '/temp/t2.txt')
$files.LastWriteTime
```

成员枚举可用于`get`集合中项目的值, 但不能用于`set`项目的值.  例如:

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

## 哈希表

[about_Hash_Tables](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_hash_tables)

哈希表也称为字典或关联数组, 是存储一个或多个键/值对的 compact 数据结构.
例如, 哈希表可能包含一系列 `IP` 地址和计算机名称, 其中 `IP` 地址是密钥, 计算机名称是值, 反之亦然.

在 PowerShell 中, 每个哈希表都是`Hashtable`对(`System.Collections.Hashtable`).  可以在 PowerShell 中使用`Hashtable`对象的属性和方法.
从 PowerShell 3.0 开始, 可以使用`[ordered]`属性在 PowerShell 中创建一个排序的字典(`(System.Collections.Specialized.OrderedDictionary)`).

有序哈希表不同于普通哈希表, 它们的键始终按你给出的顺序出现, 普通哈希表中的键顺序不确定.

哈希表中的键和值也是 `.NET` 对象.  它们最常见的是字符串或整数, 但它们可以有任何对象类型.  您还可以创建嵌套哈希表, 令键的值为另一个哈希表.

通常使用哈希表, 因为它们非常适合用于查找和检索数据.  您可以使用哈希表来存储列表, 并在 `PowerShell` 中创建带计算的属性.
此外, `PowerShell` 有一个 `cmdlet`: `ConvertFrom-StringData`, 它将字符串转换为哈希表.

哈希表的语法如下所示:

```powershell
@{ <name> = <value>; [<name> = <value> ] ...}
```

排序字典的语法如下所示:

```powershell
[ordered]@{ <name> = <value>; [<name> = <value> ] ...}
```

### 创建哈希表

若要创建哈希表, 请遵循以下准则:

+ 使用 `at` 符号 `@`.
+ 将哈希表放在大括号中`{}`.
+ 为哈希表的内容输入一个或多个键/值对, 使用等号 (`=`) 将每个键与其值分隔开.
+ 使用分号 (`;`) 或换行符分隔键/值对.
+ 包含空格的键必须用引号引起来.  值必须是有效的 PowerShell 表达式.  字符串必须用引号引起来, 即使它们不包含空格.
+ 若要管理哈希表, 请将它保存在变量中.
+ 将有序哈希表分配给变量时, 请将`[ordered]`属性置于`@`符号之前.  如果将其放在变量名称之前, 则该命令将失败.

若要使`$hash`变量的值为空的哈希表, 请键入:

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

### 显示哈希表

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
# 例如:
$hash.Number
```

如果`key`名称与哈希表类型的某个属性名称冲突, 则可以使用 `PSBase` 来访问这些属性.
例如, 如果哈希表中某个`key`的名字为`keys`, 覆盖掉了原本的`keys`属性, 请使用以下语法:

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

### 添加和删除键和值

若要将键和值添加到哈希表, 请使用以下命令格式.

```powershell
$hash["<key>"] = "<value>"
```

例如, 若要添加内容为`"Time"->"Now"`的键值对, 请使用以下语句格式.

```powershell
$hash["Time"] = "Now"
```

还可以通过使用 `System.Collections.Hashtable` 对象的 `Add` 方法, 将键和值添加到哈希表中.  `Add` 方法具有以下语法:

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
# 例如:
$hash.Remove("Time")
```

可以在 PowerShell 中使用哈希表对象的所有属性和方法, 包括 `Contains`, `Clear``, Clone` 和 `CopyTo`.  有关哈希表对象的详细信息, 请参阅[System.Collections.Hashtable](https://docs.microsoft.com/en-us/dotnet/api/system.collections.hashtable?view=net-5.0).

### 哈希表中的对象类型

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

### 对键和值进行排序

哈希表中的项在本质上是无序的.  每次显示键/值对时, 它们的显示顺序可能不同.

尽管不能对哈希表进行排序, 但可以使用哈希表的 `GetEnumerator` 方法枚举键和值, 然后使用 `Sort-Object` cmdlet 对要显示的枚举值进行排序.

```powershell
$p.GetEnumerator() | Sort-Object -Property key
```

也可以按降序排列.

```powershell
$p.getenumerator() | Sort-Object -Property Value -Descending
```

### 从哈希表创建对象

从 PowerShell 3.0 开始, 可以从`属性`和`属性值`组成的哈希表创建对象. 语法如下:

```powershell
[<class-name>]@{
  <property-name>=<property-value>
  <property-name>=<property-value>
}
```

此方法仅适用于具有 `null` 构造函数的类(即没有参数的构造函数).  对象属性必须是`public`且可设置的(`settable`). 有关详细信息, 请参阅 about_Object_Creation.

### ConvertFrom-StringData

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
