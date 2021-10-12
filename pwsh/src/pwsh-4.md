# 关于系列

## 关于计算属性

[about_Calculated_Properties](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_calculated_properties?view=powershell-7)

+ 简短说明

PowerShell 提供动态添加新属性和更改输出到管道的对象的格式. 

+ 长说明

许多 PowerShell cmdlet 使用允许向这些输出对象添加新属性的参数将输入对象转换, 聚合或处理到输出对象中.  
这些参数可用于基于输入对象的值在输出对象上生成新的计算属性.  
计算属性由哈希表定义, 该哈希表包含指定新属性名称的键值对, 用于计算值的表达式以及可选的格式设置信息. 

### 受支持的 cmdlet

以下 cmdlet 支持 Property 参数的计算 属性值.  Format-*cmdlet 还支持 GroupBy 参数的计算值. 

以下列表对支持计算属性的 cmdlet 以及每个 cmdlet 支持的键值对进行项化. 

    Compare-Object
        expression

    ConvertTo-Html
        name/label - 在 PowerShell 6.x (中添加的可选)
        expression
        width - 可选
        alignment - 可选

    Format-Custom
        expression
        depth - 可选

    Format-List
        name/label - 可选
        expression
        formatstring - 可选

    这组相同的键值对也适用于传递给所有 cmdlet 的 GroupBy 参数的计算 Format-* 属性值. 

    Format-Table
        name/label - 可选
        expression
        formatstring - 可选
        width - 可选
        alignment - 可选

    Format-Wide
        expression
        formatstring - 可选

    Group-Object
        expression

    Measure-Object
        仅支持表达式的脚本块, 而只支持哈希表. 
        PowerShell 5.1 及更旧版本不支持. 

    Select-Object
        name/label - 可选
        expression

    Sort-Object
        expression
        ascending/descending - 可选

+ 备注

的值可以是 expression 脚本块, 而不是哈希表.  有关详细信息, 请参阅 备注 部分. 

### 哈希表键定义

    name/label - 指定要创建的属性的名称.  可以互换 name 使用 或其别名 label . 
    expression - 用于计算新属性的值的脚本块. 
    alignment - 由生成表格输出的 cmdlet 使用, 用于定义值在列中的显示方式.  值必须为 'left', 'center' 或 'right'. 
    formatstring - 指定一个格式字符串, 该字符串定义如何为输出设置值的格式.  有关格式字符串详细信息, 请参阅 .NET 中的格式类型. 
    width - 指定显示值时表中的最大宽度列.  该值必须大于 0 . 
    depth - 的 Depth Format-Custom 参数指定所有属性的扩展深度.  depth使用 键可以指定每个属性的扩展深度. 
    ascending / descending - 允许指定一个或多个属性的排序顺序.  这些是布尔值. 

只要指定的名称前缀是明确的, 就不需要拼写哈希表键.  例如,  n 可用于代替 ,  Name e 并可用于代替 Expression . 
示例

### Compare-Object

使用计算属性, 可以控制如何比较输入对象的属性.  本示例将值与算术运算的结果进行比较, 而不是直接比较值 (2) . 
PowerShell

Compare-Object @{p=1} @{p=2} -property @{ Expression = { $_.p % 2 } }

Output

 $_.p % 2  SideIndicator
---------- -------------
         0 =>
         1 <=

### ConvertTo-Html

ConvertTo-Html 可以将 对象的集合转换为 HTML 表.  使用计算属性可以控制表的显示方式. 
PowerShell

Get-Alias |
  ConvertTo-Html Name,
                 Definition,
                 @{
                    name='ParameterCount'
                    expr={$_.Parameters.Keys.Count}
                    align='center'
                 } |
    Out-File .\aliases.htm -Force

此示例创建一个 HTML 表, 其中包含每个别名命令的 PowerShell 别名列表和数字参数.  ParameterCount 列 的值居中. 
Format-Custom

Format-Custom 提供对象的自定义视图, 其格式类似于类定义.  更复杂的对象可以包含使用复杂类型深度嵌套的成员.  的 Depth Format-Custom 参数指定所有属性的扩展深度.  depth使用 键可以指定每个属性的扩展深度. 

此示例中,  depth 键简化了 Get-Date cmdlet 的自定义输出.  Get-Date 返回 DateTime 对象.  此对象的 Date 属性也是 DateTime 对象, 因此该对象是嵌套的. 

Get-Date | Format-Custom @{expr={$_.Date};depth=1},TimeOfDay

Output

class DateTime
{
  $_.Date =
  ...
    }
  TimeOfDay =
   ...
}

### Format-List

本示例使用计算属性更改 中输出的名称和格式 Get-ChildItem . 
PowerShell

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

### Format-Table

此示例中, 计算属性添加 Type 属性, 该属性用于按内容类型对文件进行分类. 
PowerShell

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

### Format-Wide

Format-Wide使用 cmdlet, 你可以将集合中对象的一个属性的值显示为多列列表. 

对于此示例, 我们希望将文件名和大小 (千字节) 一个宽列表.  由于 不显示多个属性, 因此我们使用计算属性将两 Format-Wide 个属性的值合并为单个值. 
PowerShell

Get-ChildItem -File |
  Format-Wide -Property @{e={'{0} ({1:N2}kb)' -f $_.name,($_.length/1kb)}}

Output

.editorconfig (0.18kb)                          .gitattributes (0.41kb)
.gitignore (0.22kb)                             .localization-config (0.23kb)
...

### Group-Object

Group-Objectcmdlet 基于指定属性的值在组中显示对象.  此示例中, 计算属性计算每个内容类型的文件数. 

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
```

Output

Count Name
----- ----
    5 Automation
    7 Configuration
    2 Content
    3 Metacontent

### Measure-Object

Measure-Objectcmdlet 计算 对象的数值属性.  本示例使用计算属性获取介于 1 和 10 之间的 (总和) 3 可求和的计数. 
PowerShell

1..10 | Measure-Object -Property {($_ % 3) -eq 0} -Sum

Output

Count             : 10
Average           :
Sum               : 3
...

备注

与其他 cmdlet 不同,  Measure-Object 不接受计算属性的哈希表.  必须使用脚本块. 

### Select-Object

可以使用计算属性通过 cmdlet 将其他成员添加到对象 Select-Object 输出.  本示例列出了以字母 开头的 PowerShell 别名 C .  使用 , 输出别名, 它映射到的 Select-Object cmdlet, 以及为 cmdlet 定义的参数数的计数.  使用计算属性, 可以创建 ParameterCount 属性. 
PowerShell

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

Name    Definition         ParameterCount
----    ----------         --------------
cat     Get-Content                    21
...

### Sort-Object

使用计算属性, 可以按每个属性的不同顺序对数据进行排序.  此示例按日期 按升序对 CSV 文件 中的数据进行排序.  但在每个日期内, 它按 UnitsSold 按降序对行进行排序. 
PowerShell

Import-Csv C:\temp\sales-data.csv |
  Sort-Object Date, @{expr={$_.UnitsSold}; desc=$true}, Salesperson  |
    Select-Object Date, Salesperson, UnitsSold

Output

Date       Salesperson UnitsSold
----       ----------- ---------
2020-08-01 Sally       3
...
2020-08-04 Sally       2

说明

    可以直接将表达式 脚本块指定为 参数, 而不是将表达式脚本块指定为哈希 Expression 表中条目.  例如：
    PowerShell

'1', '10', '2' | Sort-Object { [int] $_ }

此示例适用于不需要对属性 (或) 键命名属性的 cmdlet, 例如 ,  Name Sort-Object 和 Group-Object Measure-Object . 

对于支持命名属性的 cmdlet, 脚本块将转换为字符串, 并用作输出中的属性名称. 

Expression 脚本块在 子 作用域中运行, 这意味着不能直接修改调用方的变量. 

管道逻辑应用于脚本块 Expression 的输出.  这意味着, 输出单元素数组会导致该数组解包. 

对于大多数 cmdlet, 将忽略表达式脚本块中的错误.  对于 Sort-Object , 语句终止和脚本终止错误是 输出 , 但它们不会终止 语句. 
