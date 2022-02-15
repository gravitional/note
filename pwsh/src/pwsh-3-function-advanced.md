# Functions_Advanced

[about_Functions_Advanced][]
[about_Functions_Advanced]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced

简要说明; 介绍了高级函数, 这是一种使用脚本创建 cmdlet 的方法.

## 长描述

`cmdlet` 是参与 PowerShell 管道语义(pipeline semantics)的单一命令.
这包括二进制cmdlet, 高级脚本函数, CDXML和工作流(Workflows).

`高级函数`(Advanced functions) 允许你 以 `PowerShell 函数` 的形式创造 cmdlet.
高级函数使创建cmdlet更容易, 而不需要编写和编译 `二进制cmdlet`.
`二进制 cmdlet` 是用 `.NET语言`(如`C#`) 编写的 `.NET类`.

`高级函数` 使用 `CmdletBinding` 属性(attribute)来表明, 它们是像cmdlet一样的函数.

`CmdletBinding` 属性与 `Cmdlet` 属性类似, 后者在编译的 `cmdlet` 类中使用, 以确定该类是一个 `cmdlet`.
关于这个属性的更多信息, 请看 [about_Functions_CmdletBindingAttribute][].

下面的例子显示了一个函数, 它接受一个名字, 然后使用提供的名字打印一个问候语.
还注意到这个函数定义中的名称, 包括一个动词(`Send`)和名词(Greeting)对, 就像 `编译的cmdlet` 的 `动词-名词对` 一样.
然而, 函数不强制要求具有 `动词-名词` 的名字.

```PowerShell
function Send-Greeting
{
    [CmdletBinding()]
    Param(
        [Parameter(Mandatory=$true)]
        [string] $Name
    )

    Process
    {
        Write-Host ("Hello " + $Name + "!")
    }
}
```

函数的 `参数` 是通过使用 `Parameter` 属性来声明的.
这个属性可以单独使用, 也可以与 `Alias属性` (attribute) 或其他几个 `参数验证` 属性结合使用.
关于如何声明参数(包括在运行时添加的动态参数)的更多信息, 请参阅 [about_Functions_Advanced_Parameters][].

上述函数的实际工作是在 `Process 块` 中进行的, 它相当于 `ProcessingRecord` 方法, `编译形式的 cmdlet` 用来处理传递给 `cmdlet` 的数据.
`Process 块`, 以及 `Begin` 和 `End块`, 在 [about_Functions_Advanced_Methods][] 主题中描述.

高级函数在以下方面与 `编译的cmdlet` 不同.

+ 当`字符串数组`被绑定到一个`布尔参数`时, 高级函数的`参数绑定`不会抛出异常.
+ `ValidateSet` 属性和 `ValidatePattern` 属性不能传递 `命名参数`.
+ 高级函数不能在 `transaction`s(交易) 中使用.

[about_Functions_Advanced_Parameters]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters
[about_Functions_Advanced_Methods]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_methods
[about_Functions_CmdletBindingAttribute]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_cmdletbindingattribute

## 函数的高级参数

[about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters)

简要说明; 解释了如何为`高级函数`添加参数.

长描述

你可以向你编写的`高级函数`添加参数, 并使用参数 `attributes` and `arguments`, 来限制函数用户提交的参数值.

除了 PowerShell 自动添加到所有 cmdlet 和高级函数中的`常用参数`外, 你添加到函数中的参数也可以供用户使用.
关于 PowerShell 通用参数的更多信息, 请参阅about_CommonParameters.

从 PowerShell 3.0开始, 你可以使用 `@Args` 的 splitting, 来表示命令中的参数.
`Splatting` 对简单和高级函数都有效. 欲了解更多信息, 请参见 about_Functions 和 about_Splatting .

## 参数拼接 Splatting

[about_Splatting](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_splatting)

简要说明 ; 描述了如何在 `PowerShell` 中使用 `splitting` 向命令传递参数.

## 长描述

`Splatting` 是一种将 `参数值` 的集合作为 `单元`, 传递给命令的方法.
`PowerShell` 将集合中的每个`值`与一个`命令参数`相关联.
`拼合的参数值`存储在被命名的 `拼合变量` 中, 这些变量看起来像标准变量, 但以 `At` 符号(`@`)而不是美元符号(`$`)开头.
`At` 符号告诉 `PowerShell`, 你传递的是 `值的集合`, 而不是 `单一的值`.

`拼接`(Splatting)使你的命令更短, 更容易阅读.
你可以在不同的命令调用中重复使用`拼接值`,
并使用`拼接`将参数值从 `$PSBoundParameters` 自动变量传递给其他脚本和函数.

从 Windows PowerShell 3.0 开始, 你也可以用`splatting`来表示一个命令的所有参数.

## 语法

```powershell
<CommandName> <optional parameters> @<HashTable> <optional parameters>
<CommandName> <optional parameters> @<Array> <optional parameters>
```

要为`位置参数`提供`参数值`, 其中不需要`参数名`, 使用`数组语法`.
要提供`参数名称`和`值`组成的对, 请使用`哈希表`语法. `拼接的值`可以出现在参数列表的任何地方(顺序无关).

当拼接时, 你不需要使用`哈希表`或`数组`来传递`所有参数`.
你可以通过使用`拼接`来传递一些参数, 并通过`位置`或 `参数名称` 来传递其他参数.
另外, 你可以在一个命令中拼接`多个对象`, 这样你就不用为每个`参数`传递一个以上的`值`.

从 PowerShell 7.1 开始, 你可以通过在命令中明确定义一个参数, 来`覆盖`一个`拼接`的参数.

## 用哈希表拼接

使用`哈希表`来拼接参数名称和值对.
你可以对所有参数类型使用这种格式, 包括`位置参数`和`switch 参数`. `位置参`数必须按`名称`分配.

下面的例子比较了两个`Copy-Item`命令, 将 `Test.txt` 文件复制到同一目录下的 `Test2.txt` 文件.

第一个例子使用传统的格式, 其中包括参数名称.

```PowerShell
Copy-Item -Path "test.txt" -Destination "test2.txt" -WhatIf
```

第二个例子使用`哈希表拼接`.
第一条命令创建了一个参数名和参数值对的`哈希表`, 并将其存储在 `$HashArguments` 变量中.
第二条命令在命令中使用 `$HashArguments` 变量 with `splatting`.
`At`符号(`@HashArguments`)取代了命令中的美元符号(`$HashArguments`).

要为 `WhatIf` 开关参数提供一个值, 使用 `$True` 或 `$False`.

```PowerShell
$HashArguments = @{
  Path = "test.txt"
  Destination = "test2.txt"
  WhatIf = $true
}
Copy-Item @HashArguments # 注意这里要使用 @ 符号开头
```

> 注意;
>在第一条命令中, `At`符号(`@`)表示`哈希表`, 而不是一个`splatted`的值.
>PowerShell 中哈希表的语法是: `@{<name>=<value>; <name>=<value>; ...}`
