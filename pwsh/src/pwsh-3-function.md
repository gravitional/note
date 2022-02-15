# 函数

[Chapter 9 - Functions](https://docs.microsoft.com/en-us/powershell/scripting/learn/ps101/09-functions)

## 关于函数

[about_Functions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions)

简要说明; 描述了如何在 PowerShell 中创建和使用函数.

## 长描述

`函数` 是 `PowerShell` 语句的列表, 它有你指定的名字. 当你运行函数时, 你输入函数名称.
列表中的语句依次运行, 就像你在命令提示符下输入的那样.

函数可以像这样简单:

```powershell
function Get-PowerShellProcess { Get-Process PowerShell }
```

函数也可以像 `cmdlet` 或应用程序一样复杂.

像 `cmdlet` 一样, 函数可以有 `parameters`.
`参数`可以是 `命名参数`, `位置参数`, `开关参数` 或 `动态参数`(dynamic).
`函数参数` 可以从命令行或管道中读取.

函数可以返回 `值`, 这些`值`可以显示, 分配给 `变量`, 或传递给其他函数或 `cmdlets`.
你也可以使用 `return` 关键字指定 `返回值`.
`return` 关键字不会影响或压制从你的函数返回的其他输出.
但是, 在运行到 `return` 关键字这行时会 `退出函数`. 更多信息请参见 [about_Return][].

函数的 `语句列表`(statement list)可以包含不同类型的语句列表,
使用关键字为 `Begin`, `Process` 和 `End` 表明.
这些语句列表以不同的方式处理来自管道的输入(pipeline).

`过滤器`(filter)是一种特殊的函数, 使用 `Filter` 关键字.

`函数`也可以像 `cmdlet` 一样使用.
你可以创建一个和 `cmdlet` 一样工作的函数, 而不需要使用 `C#` 编程.
欲了解更多信息, 请参见 [about_Functions_Advanced][].

[about_Return]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_return

> 重要信息
>在 `脚本文件` 和基于脚本的 `模块` 中, 必须先定义函数才能调用.

## 语法

下面是函数的语法:

```powershell
function [<scope:>]<name> [ ([type]$parameter1[,[type]$parameter2]) ]
{
  begin {<statement list>}
  process {<statement list>}
  end {<statement list>}
}
```

```powershell
function [<scope:>]<name>
{
  param([type]$parameter1 [,[type]$parameter2])
  dynamicparam {<statement list>}
  begin {<statement list>}
  process {<statement list>}
  end {<statement list>}
}
```

`函数`包括以下项目:

+ `Function` 关键字
+ `scope`(可选)
+ 你选择的`name`
+ 任何数量的 `命名参数`(可选)
+ 一个或多个用大括号`{}`括起来的PowerShell命令

关于 `Dynamicparam关键字` 和函数中的动态 parameters 的更多信息, 请参见 [about_Functions_Advanced_Parameters][]

## 简单的函数

函数不一定要复杂才有用. 最简单的函数有以下格式:

```powershell
function <function-name> {statements}
```

例如, 下面的函数用 `以管理员身份运行` 选项启动PowerShell:

```PowerShell
function Start-PSAdmin {Start-Process PowerShell -Verb RunAs}
```

要使用该函数, 请键入: `Start-PSAdmin`

要在函数中添加语句, 请在单独一行中输入每个语句, 或者使用分号`;`来分隔语句.

例如, 下面这个函数可以找到当前用户的目录中, 所有在开始日期后被改变的 `.jpg` 文件.

```PowerShell
function Get-NewPix
{
  $start = Get-Date -Month 1 -Day 1 -Year 2010
  $allpix = Get-ChildItem -Path $env:UserProfile\*.jpg -Recurse
  $allpix | Where-Object {$_.LastWriteTime -gt $Start}
}
```

你可以创建一个有用函数的`工具箱`.
将这些函数添加到你的 `PowerShell` 配置文件中, 如 [about_Profiles][] 和本主题后面的描述.

[about_Profiles]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_profiles

## 函数名称

你可以为函数指定 `任何名字`,
但你与他人共享的函数应该遵循为所有 PowerShell 命令制定的命名规则.

函数名称应该由 `动词-名词对` 组成,
其中 `动词` 标识了函数执行的操作, `名词` 标识了cmdlet执行操作的项目.

函数应该使用已被批准用于所有 PowerShell 命令的 `标准动词`.
这些动词帮助我们保持命令名称的简单, 一致, 并使用户易于理解.

关于标准 PowerShell 动词的更多信息, 请参见 Microsoft 文档中的 [Approved Verbs][].

[Approved Verbs]: https://docs.microsoft.com/en-us/powershell/scripting/developer/cmdlet/approved-verbs-for-windows-powershell-commands

## 带参数的函数

你可以在函数中使用 `parameters`, 包括命名参数, 位置参数, 开关参数和动态参数.
关于函数中的动态参数的更多信息, 请看 [about_Functions_Advanced_Parameters][].

### 命名的参数

你可以定义任意数量的 `命名参数`.
你可以为命名参数包含 `默认值`, 如本主题后面所描述的.

你可以使用 `param` 关键字在大括号内定义参数, 如下面的语法示例所示.

```Syntax
function <name> {
  param ([type]$parameter1 [,[type]$parameter2])
  <statement list>
}
```

你也可以在大括号外定义`参数`, 而不使用 `Param` 关键字, 如下面的语法示例所示.

```Syntax
function <name> [([type]$parameter1[,[type]$parameter2])] {
  <statement list>
}
```

下面是这个额外语法的例子:

```PowerShell
function Add-Numbers([int]$one, [int]$two) {
    $one + $two
}
```

虽然第一种方法是首选, 但这两种方法之间没有区别.

当你运行该 `函数` 时, 你为 `parameter` 提供的 `值` 会被分配给包含参数名称 的`变量`.
该 `变量的值` 可以在函数中使用.

下面的例子是名为 `Get-SmallFiles` 的函数. 这个函数有个 `$Size` parameter.
该函数显示所有小于 `$Size` 参数值的文件, 并且不包括目录.

```PowerShell
function Get-SmallFiles {
  Param($Size)
  Get-ChildItem $HOME | Where-Object {
    $_.Length -lt $Size -and !$_.PSIsContainer
  }
}
```

在该函数中, 你可以使用 `$Size` 变量, 这是为参数定义的名称.
要使用这个函数, 请输入以下命令.

```PowerShell
Get-SmallFiles -Size 50
```

你也可以为 `命名参数` 输入`值`, 而不输入`parameter 名称`.
例如, 下面的命令给出的结果, 与命名了 `Size` 参数的命令相同:

```PowerShell
Get-SmallFiles 50
```

要为`parameter`定义`默认值`, 可以在参数名称后面输入`等号`和`值`,
如下面 `Get-SmallFiles` 例子的变体所示:

```PowerShell
function Get-SmallFiles ($Size = 100) {
  Get-ChildItem $HOME | Where-Object {
    $_.Length -lt $Size -and !$_.PSIsContainer
  }
}
```

如果你输入 `Get-SmallFiles` 而不提供 `值`, 该函数会将 `100` 分配给 `$size`.
如果你提供了`值`, 该`函数`就会使用该`值`.

另外, 你可以提供简短的`帮助字符串`, 描述你的 parameter 的 `默认值`,
方法是将 `PSDefaultValue` attribute 添加到你的 `参数描述` 中, 并指定 `PSDefaultValue` 的帮助属性.

要提供`帮助字符串`, 描述 `Get-SmallFiles` 函数中 `Size` 参数的 `默认值`(`100`),
添加 `PSDefaultValue` 属性, 如下例所示:

```PowerShell
function Get-SmallFiles {
  param (
      [PSDefaultValue(Help = '100')]
      $Size = 100
  )
}
```

关于 `PSDefaultValue` attribute class 的更多信息, 请参阅 [PSDefaultValue Attribute Members].

[PSDefaultValue Attribute Members]: https://docs.microsoft.com/en-us/dotnet/api/system.management.automation.psdefaultvalueattribute

## 位置参数

`位置参数` 是没有 `参数名称` 的 `参数`.
`PowerShell` 依靠 `参数值顺序`, 将每个 `参数值` 与函数中的`参数` 联系起来.

当你使用位置参数时, 在 `函数名` 后面输入一个或多个`值`.
`位置参数`的值被分配到 `$args` 数组变量中.
在函数名后面的值被分配到 `$args` 数组的 `首位置`, 即 `$args[0]`.

下面的 `Get-Extension` 函数, 将 `.txt` 文件名扩展名添加到你提供的文件名中:

```PowerShell
function Get-Extension {
  $name = $args[0] + ".txt"
  $name
}
```

```powershell
Get-Extension myTextFile

Out: myTextFile.txt
```

## 开关参数

`开关参数` 是不需要 `值` 的`参数`.
取而代之的是, 你在输入函数名称后, 跟着输入 `开关参数` 的名称.

要定义 `开关参数`, 在参数名称前指定 type `[switch]`, 如下面的例子所示:

```PowerShell
function Switch-Item {
  param (  [switch]$on )
  if ($on) { "Switch on" }
  else { "Switch off" }
}
```

当你在函数名称后面输入 `On` 开关参数时, 该函数显示 `"Switch on"`.
如果没有开关参数, 则显示 `"Switch off"`.

```powershell
Switch-Item -on
Out:: Switch on

Switch-Item
Out:: Switch off
```

你也可以在运行函数时给 `开关` 分配 `布尔值`, 如下面的例子中所示:

```powershell
Switch-Item -on:$true
Out:: Switch on

Switch-Item -on:$false
Out:: Switch off
```

## 使用 Splatting 表示命令参数

你可以使用 `splitting` 来表示 `命令`的`参数`.
这个功能是在Windows PowerShell 3.0中引入的.

在 `调用会话中的命令` 的 `函数` 中使用这种技术.
你不需要声明或列举 `命令参数`, 也不需要在 `命令参数` 改变时改变 `函数`.

下面的示例函数调用 `Get-Command` cmdlet.
该命令使用 `@Args` 来表示 `Get-Command` 的参数.

```PowerShell
function Get-MyCommand { Get-Command @Args }
```

当你调用 `Get-MyCommand` 函数时, 你可以使用 `Get-Command` 的所有参数.
参数和参数值使用 `@Args` 传递给命令.

```PowerShell
Get-MyCommand -Name Get-ChildItem

Out::
CommandType     Name                ModuleName
-----------     ----                ----------
Cmdlet          Get-ChildItem       Microsoft.PowerShell.Management
```

`@Args` 功能使用 `$Args` 自动参数,
它代表未声明的 `cmdlet` parameters 和其余 arguments 的值.

关于拼接的更多信息, 请参阅 [about_Splatting][].

[about_Splatting]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_splatting

## 将对象输送到函数

任何函数都可以从 `管道` 中获取输入.
你可以使用 `Begin`, `Process` 和 `End` 关键字控制 `函数` 如何处理来自管道的输入.
下面的示例语法显示了这三个关键字:

```Syntax
function <name> {
  begin {<statement list>}
  process {<statement list>}
  end {<statement list>}
}
```

`Begin` 语句列表只运行一次, 在函数开始运行时.

>重要提示
如果你的函数定义了 `Begin`, `Process` 或 `End` 块, 你所有的代码必须在这些块内.
如果定义了`任何`一个块, 在这些块之外的代码将不会被识别.

`Process` 语句列表为 `管道` 中的每个对象运行一次.
当 `Process` 块运行时, 每个管道对象被分配到 `$_` 自动变量, 每次一个 `管道对象`.

在函数接收完所有 `管道` 中的对象后, `End` 语句列表运行一次.
如果没有使用 `Begin`, `Process` 或 `End` 关键字,
所有的语句会被当作 `End` 语句列表(statement list).

下面的函数使用了 `Process` 关键字. 该函数 displays 管道中的例子:

```PowerShell
function Get-Pipeline
{
  process {"The value is: $_"}
}
```

为了演示这个函数, 请输入由逗号分隔的 `数字列表`, 如下面的例子中所示:

```PowerShell
1,2,4 | Get-Pipeline

Out::
The value is: 1
The value is: 2
The value is: 4
```

当你在 `管道`序列中使用 `函数` 时, `管道` 中的 `对象` 被分配到 `$input` 自动变量中.
在任何对象自管道发送之前, 该 `函数` 运行带有 `Begin` 关键字的语句.
在所有对象从管道中接收完毕后, 函数运行带有 `End` 关键字的语句.

下面的例子显示了带有 `Begin` 和 `End` 关键字的 `$input` 自动变量.

```PowerShell
function Get-PipelineBeginEnd
{
  begin {"Begin: The input is $input"}
  end {"End:   The input is $input" }
}
```

如果通过使用 `管道` 来运行这个函数, 会显示以下结果:

```PowerShell
1,2,4 | Get-PipelineBeginEnd

Out::
Begin: The input is
End:   The input is 1 2 4
```

当 `Begin` 语句运行时, 该函数没有来自管道的输入.
`End` 语句在函数拥有这些`值`后运行.

如果函数有 `Process` 关键字, `$input` 中的每个对象都会从 `$input` 中删除, 并分配给 `$_`.
下面的例子有 `Process` 语句列表:

```PowerShell
function Get-PipelineInput
{
  process {"Processing:  $_ " }
  end {"End:   The input is: $input" }
}
```

在这个例子中, 每个被输送到 `函数` 的 `对象` 都被送到 `Process` 语句列表中.
`Process` 语句在每个对象上运行, 每次一个对象.
当函数到达 `End` 关键字时, `$input` 自动变量为 空.

```PowerShell
1,2,4 | Get-PipelineInput

Out::
Processing:  1
Processing:  2
Processing:  4
End:   The input is:
```

欲了解更多信息, 请参见[Using Enumerators][]

[Using Enumerators] :https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.2#using-enumerators

## 过滤器,Filters

`过滤器`是一种函数, 在管道中的每个对象上运行.
过滤器类似于 所有语句都在 `Process` 块中的函数.

过滤器的语法如下:

```powershell
filter [<scope:>]<name> {<statement list>}
```

下面的 `过滤器` 从管道中获取 `日志条目`, 然后显示整个条目或只显示条目的消息部分.

```PowerShell
filter Get-ErrorLog ([switch]$message)
{
  if ($message) { Out-Host -InputObject $_.Message }
  else { $_ }
}
```

## 函数范围

`函数` 存在于它被创建的范围内.

如果 `函数` 是 `脚本` 的一部分, 该函数对该脚本中的语句是可用的.
默认情况下, `脚本` 中的函数在 `命令提示符` 下是不可用的.

你可以指定 `函数` 的 `范围`. 例如, 在下面的例子中, 该函数被添加到全局范围.

```powershell
function global:Get-DependentSvs {
  Get-Service | Where-Object {$_.DependentServices}
}
```

当 `函数` 在全局范围内时, 你可以在 `脚本`, `函数` 和 `命令行` 中使用该函数.

函数通常会创建 `作用域`. 在函数中创建的项目, 如`变量`, 只存在于函数作用域中.

关于 `PowerShell` 中作用域的更多信息, 请看 [about_Scopes][].

[about_Scopes]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_scopes

## 使用 Function: 驱动器 查找和管理函数

`PowerShell` 中所有的函数和过滤器都自动存储在 `Function:` 驱动器中.
这个驱动器是由 `PowerShell` Function provider 公开的.

要引用 `Function:` 驱动器时, 在 `Function` 后面键入`冒号`,
就像你提到计算机的 `C` 或 `D` 驱动器时一样.

下面的命令显示 `PowerShell` 当前会话中的所有函数.

```PowerShell
Get-ChildItem function:
```

函数中的命令以 `脚本块` 的形式存储在函数的 `definition` property 中.
例如, 要显示 `PowerShell` 自带的 `Help` 函数中的命令, 请输入.

```PowerShell
(Get-ChildItem function:help).Definition
```

你也可以使用下面的语法:

```PowerShell
$function:help
```

关于 `Function:` 驱动器 的更多信息, 请参阅 `Function provider` 的帮助主题.
键入 `Get-Help Function`.

## 在新会话中重复使用函数

当你在 `PowerShell` 命令提示符下输入函数时, 该函数会成为当前会话的一部分.
它在会话结束前都是可用的.

要在所有 `PowerShell` 会话中使用你的函数, 请将该函数添加到你的 `PowerShell` 配置文件.
关于配置文件的更多信息, 见 [about_Profiles][].

你也可以将你的函数保存在 `PowerShell` 脚本文件中.
在文本文件中键入你的函数, 然后用 `.ps1` 的文件名扩展名保存该文件.

## 为函数编写帮助

Get-`Help` cmdlet 可以为 `函数` 以及 cmdlet, providers 和 scripts 获得帮助.
要获得函数的帮助, 键入 `Get-Help` 和函数名称.

例如, 要获得 `Get-MyDisks` 函数的帮助, 键入:

```PowerShell
Get-Help Get-MyDisks
```

你可以通过使用以下两种方法之一, 为函数编写帮助.

+ 基于注释的函数帮助
    通过在 `注释` 中使用特殊的关键字来创建帮助主题.
    要为函数创建基于 `注释` 的帮助, 注释必须放在函数主体的开头或结尾,
    或者放在函数关键词前面的行中.
    关于 `基于注释` 的帮助的更多信息, 请参阅 [about_Comment_Based_Help].

+ 基于XML的函数帮助
    创建 `基于XML` 的帮助主题, 比如通常为 `cmdlets` 创建的类型.
    如果你需要将帮助主题本地化为多种语言, 则需要基于 XML 的帮助.

    为了将函数与 `基于XML` 的帮助主题联系起来, 请使用 `.ExternalHelp` 注释式 help 关键字.
    如果没有这个关键字, `Get-Help` 就不能找到该函数的帮助主题, 对该函数的 `Get-Help` 调用只能返回自动生成的帮助.

关于 `ExternalHelp` 关键字的更多信息, 请参阅 [about_Comment_Based_Help][].
关于 `基于XML` 的帮助的更多信息, 见 [How to Write Cmdlet Help][].

[about_Comment_Based_Help]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comment_based_help
[How to Write Cmdlet Help]: https://docs.microsoft.com/en-us/powershell/scripting/developer/help/writing-help-for-windows-powershell-cmdlets
