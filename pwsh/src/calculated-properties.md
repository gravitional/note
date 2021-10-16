# 关于计算属性

[about_Calculated_Properties](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_calculated_properties?view=powershell-7)

+ 简短说明

`PowerShell` 可以动态添加 `新属性` ,或者更改输出到`管道`的`对象`的`格式`.

+ 长说明

使用许多 `PowerShell` cmdlet 时 , 可以通过使用 `参数` , 向 `输出对象` 添加`新属性`,
或对 `输入对象` 进行变形, 聚合, 或 处理成新对象.

使用这些`参数`, 可以基于`输入对象`的值, 在输出对象上生成新的`计算属性`.
`计算属性`由[哈希表](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_hash_tables?view=powershell-7)定义,
该哈希表包含一些键值对, 指定`新属性名`, 用于计算值的`表达式`, 以及可选的`格式化`信息.

## 受支持的 cmdlet

以下 `cmdlet` 支持设置 `Property` 参数的值, 来产生`计算属性`.
`Format-*cmdlet` 还支持 `GroupBy` 参数的计算值.

以下列出了支持`计算属性 `的 ` cmdlet`, 以及每个 `cmdlet` 支持的`键值对`.

+ `Compare-Object`
    + `expression`
+ `ConvertTo-Html`
    + `name/label` - 可选(在 PowerShell 6.x 中添加)
    + `expression`
    + `width` - 可选
    + `alignment` - 可选

+ `Format-Custom`
    + `expression`
    + `depth` - 可选

+ `Format-List`
    + `name/label` - 可选
    + `expression`
    + `formatstring` - 可选

    这组`键值对`也适用于, 所有Format-* cmdlet 的 `GroupBy` 参数的`计算属性`值.

+ `Format-Table`
    + `name/label` - 可选
    + `expression`
    + `formatstring` - 可选
    + `width` - 可选
    + `alignment` - 可选

+ `Format-Wide`
    + `expression`
    + `formatstring` - 可选

+ `Group-Object`
    + `expression`

+ `Measure-Object`
    + 仅支持`脚本块`形式的 `表达式`, 不支持`哈希表`.
    + PowerShell 5.1 及更旧版本不支持.

+ `Select-Object`
    + `name/label` - 可选
    + `expression`

+ `Sort-Object`
    + `expression`
    + `ascending/descending` - 可选

> 备注; `expression` 的值可以是 `脚本块`, 而不是`哈希表`.  有关详细信息, 请参阅 `备注` 部分.

## 哈希表键定义

+ `name/label` ; 指定要创建的`属性`的`名称 `.  ` name`和它的别名 `label`, 可以互换使用.
+ `expression` ; 用于计算`新属性`的`值`的`脚本块`.
+ `alignment` ; 由生成`表格输出 `的 ` cmdlet` 使用, 用于定义`值`在`列`中的显示方式.  值必须为 `'left'`, `'center'` 或 `'right'`.
+ `formatstring` ; 指定一个`格式化字符串`, 该`字符串`定义如何为`输出`设置`值`的`格式`.  有关格式字符串详细信息,
请参阅[.NET 中的格式类型](https://docs.microsoft.com/en-us/dotnet/standard/base-types/formatting-types).
+ `width` ; 指定显示`值`时表中列的`最大宽度`.  该值必须大于 `0` .
+ `depth` ; `Format-Custom` 的 `Depth` 参数, 指定所有`属性`的`展开深度`.  使用  `depth key` 可以指定每个`属性`的`展开深度`.
+ `ascending/descending` ; 允许指定一个或多个属性的`排列次序`.  它们是`布尔值`.

只要指定的`名称前缀`是明确的, 不需要写出`哈希表键`的全称, 例如,  n 可用于代替 `Name` , `e`可用于代替 `Expression`.

## 示例

### Compare-Object

使用计算属性, 可以控制如何比较输入对象的属性.  本示例将值与算术运算的结果进行比较, 而不是直接比较值 (2) .

```powershell
Compare-Object @{p=1} @{p=2} -property @{ Expression = { $_.p % 2 } }
Output
 $_.p % 2  SideIndicator
---------- -------------
         0 =>
         1 <=
```

### ConvertTo-Html

`ConvertTo-Html` 可以将 `对象` 的集合转换为 `HTML` 表.  使用 `计算属性` 可以控制表的显示方式.

```powershell
Get-Alias |
  ConvertTo-Html Name,
                 Definition,
                 @{
                    name='ParameterCount'
                    expr={$_.Parameters.Keys.Count}
                    align='center'
                 } |
    Out-File .\aliases.htm -Force
```

此示例创建一个 `HTML` 表, 其中包含每个`PowerShell` 命令的, `别名` 和 `参数`数目, ` ParameterCount` 列的`值`居中.

### Format-Custom

`Format-Custom` 提供对象的自定义视图, 其格式类似于 `class 定义`.
更复杂的 `对象` 可以包含, 深度嵌套复杂 `type` 的`成员`.
`Format-Custom` 的 `Depth` 参数指定所有`属性`的`展开深度`.
使用 `depth key` 可以指定每个 `属性` 的 `展开深度`.

此示例中,  `depth key` 简化了 `Get-Date` cmdlet 的自定义输出.
`Get-Date` 返回 `DateTime` 对象.  此对象的 `Date` 属性也是 `DateTime` 对象, 因此该对象是`嵌套的`.

```powershell
Get-Date | Format-Custom @{expr={$_.Date};depth=1},TimeOfDay
Output
class DateTime
{
  $_.Date =
  class DateTime
  {...}
  TimeOfDay =
  class TimeSpan
  {...}
}
```

### Format-List

本示例使用`计算属性`, 更改 `Get-ChildItem`, 即 `ls` , 输出的 `名称` 和 `格式`.

```PowerShell
Get-ChildItem *.json -File |
  Format-List Fullname,
              @{
                 name='Modified'
                 expression={$_.LastWriteTime}
                 formatstring='O'
              },
              @{
                 name='Size'
                 expression={$_.Length/1KB}
                 formatstring='N2'
              }

Output
FullName : C:\Git\PS-Docs\PowerShell-Docs\.markdownlint.json
Modified : 2020-07-23T10:26:28.4092457-07:00
Size     : 2.40
...
```

### Format-Table

此示例中, 用 `计算属性` 添加 `Type` 属性, 用于按`内容类型`对文件进行`分类`.

```PowerShell
Get-ChildItem -File |
  Sort-Object extension |
    Format-Table Name, Length -GroupBy @{
      name='Type'
      expression={
        switch ($_.extension) {
          '.md'   {'Content'}
          ''      {'Metacontent'}
          '.ps1'  {'Automation'}
          '.yml'  {'Automation'}
          default {'Configuration'}
        }
      }
    }

Output
   Type: Metacontent

Name              Length
----              ------
ThirdPartyNotices   1229
LICENSE-CODE        1106
LICENSE            19047
...
```

### Format-Wide

使用 `Format-Wide`  cmdlet, 你可以将`集合中`的`对象` 的某个 `属性的值` 显示为`多列`列表.

在这个示例中, 我们希望将 `文件名`和`大小 (kilobytes)` 输出为`宽列表`.
由于 `Format-Wide` 不显示`多个属性`, 因此我们使用 `计算属性` 将两个`属性的值`合并为 `单个值`.

```PowerShell
Get-ChildItem -File |
  Format-Wide -Property @{e={'{0} ({1:N2}kb)' -f $_.name,($_.length/1kb)}}

Output
.editorconfig (0.18kb)                          .gitattributes (0.41kb)
...
```

### Group-Object

`Group-Object` cmdlet 基于指定`属性的值`, 将对象分组显式. 此示例中, `计算属性` 计算每个 `内容类型` 的文件数.

```powershell
Get-ChildItem -File |
  Sort-Object extension |
    Group-Object -NoElement -Property @{
      expression={
        switch ($_.extension) {
          '.md'   {'Content'}
          ''      {'Metacontent'}
          '.ps1'  {'Automation'}
          '.yml'  {'Automation'}
          default {'Configuration'}
        }
      }
    }

Output
Count Name
----- ----
    5 Automation
    7 Configuration
...
```

### Measure-Object

`Measure-Object` cmdlet 计算 `对象` 的 `数值属性`.  本示例使用 `计算属性` 获取,  `1..10` 的 `count`, `sum`, 以及可以被 `3` 整除的数字的 `计数`.

```PowerShell
1..10 | Measure-Object -Property {($_ % 3) -eq 0} -Sum

Output
Count             : 10
...
Sum               : 3
Property          : ($_ % 3) -eq 0
```

> 备注: 与其他 cmdlet 不同,  `Measure-Object` 不接受 `哈希表` 作为 `计算属性`, 必须使用`脚本块`.

### Select-Object

可以使用 `计算属性`, 将其他成员添加到 `Select-Object` cmdlet 的 对象输出中. 本例列出了以字母 `C` 开头的 `PowerShell` 别名.
使用 `Select-Object`, 我们可以输出 `别名`, 它映射的 `cmdlet`, 以及为 cmdlet 定义的参数的`数目`.
使用 `计算属性`, 我们可以创建 `ParameterCount` 属性.

```PowerShell
$aliases = Get-Alias c* |
  Select-Object Name,
                Definition,
                @{
                    name='ParameterCount'
                    expr={$_.Parameters.Keys.Count}
                }
$aliases | Get-Member
$aliases

Output
   TypeName: Selected.System.Management.Automation.AliasInfo

Name           MemberType   Definition
----           ----------   ----------
Equals         Method       bool Equals(System.Object obj)
...
```

### Sort-Object

使用 `计算属性`, 可以按数据的各个属性进行排序.  此例按 `日期升序` 对 `CSV` 文件中的数据进行排序.
但在每个`日期` 内, 它按 `UnitsSold 降序` 进行 `行排序`.

```PowerShell
Import-Csv C:\temp\sales-data.csv |
  Sort-Object Date, @{expr={$_.UnitsSold}; desc=$true}, Salesperson  |
    Select-Object Date, Salesperson, UnitsSold

Output
Date       Salesperson UnitsSold
----       ----------- ---------
2020-08-01 Sally       3
2020-08-01 Anne        2
...
```

## 备注

+ 可以直接将` 脚本块表达式` 指定为 `参数`, 而不是将 `表达式脚本块` 指定为 `哈希表` 中的 `Expression` 条目.  例如:

    ```PowerShell
    '1', '10', '2' | Sort-Object { [int] $_ }
    ```

    对于不需要通过`Name key` 命名 `属性` 的 cmdlet, 例如 `Sort-Object`, `Group-Object`, 和 `Measure-Object` 是很方便的.
    对于支持命名`属性`的 cmdlet, `脚本块`将转换为`字符串`, 并用作输出中`属性`的名称.
+ `Expression` 脚本块在 `子作用域`(child scopes) 中运行, 意味着`调用方`的变量不能被直接修改.
+ `管道逻辑` 适用于 `Expression` 脚本块的输出.  这意味着, 输出 `单元素数组` 会导致该数组解包(unwrapped).
+ 对于大多数 `cmdlet`, 将静默忽略 `表达式脚本块` 中的错误.  对于 `Sort-Object`, `语句终止` 和 `脚本终止` 错误被输出 , 但它们不会终止`语句`.
