
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
+ 使用分号 (`;`) 或 `换行符` 分隔键/值对.
+ 包含空格的键必须用引号引起来.
值必须是有效的 PowerShell 表达式.  字符串必须用引号引起来, 即使它们不包含空格.
+ 若要管理哈希表, 请将它保存在变量中.
+ 将有序哈希表分配给变量时, 请将 `[ordered]` 属性置于`@`符号之前.
如果将其放在 `变量名称` 之前, 则该命令是无效的.

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
