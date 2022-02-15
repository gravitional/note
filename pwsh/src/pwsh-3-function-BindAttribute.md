# Cmdlet 属性绑定

[Functions_CmdletBindingAttribute](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_cmdletbindingattribute)

短描述; 描述一个属性, 使函数像编译过的 `cmdlet` 一样工作.

## 长描述

`CmdletBinding` 属性是函数的一个属性, 使其像用 `C#` 编写的编译的 `cmdlet` 一样工作.
它提供了对 `cmdlet` 功能的访问.

`PowerShell` 对具有 `CmdletBinding属性` 的函数的 `parameters` 进行`绑定`,
就像它对编译的cmdlet的参数进行绑定一样.
`$PSCmdlet` 自动变量对具有 `CmdletBinding属性` 的函数可用, 但 `$Args` 变量则不可用.

在具有 `CmdletBinding属性` 的函数中,
`未知参数`, 和匹配不到 `positional parameters` 的 `positional arguments` 会导致参数绑定失败.

## 语法

下面的例子显示了, 如何在函数中指定 `CmdletBinding` 属性的所有 `可选arguments`的格式.
每个参数的简要描述在这个例子后面.

```powershell
{
    [CmdletBinding(ConfirmImpact=<String>,
    DefaultParameterSetName=<String>,
    HelpURI=<URI>,
    SupportsPaging=<Boolean>,
    SupportsShouldProcess=<Boolean>,
    PositionalBinding=<Boolean>)]

    Param ($Parameter1)
    Begin{}
    Process{}
    End{}
}
```

`CmdletBinding` 属性的 `布尔参数` 类型在 `CmdletBinding` 属性中省略时默认为 `False`.
将参数值设置为 `$true`, 或者只是列出参数名称(也设置为 `$true`).
例如, 下面的 `CmdletBinding` 属性是等价的:

```powershell
{
    [CmdletBinding(SupportsPaging=$true)]

    Param ($Parameter1)
    Begin{}
    Process{}
    End{}
}

# 布尔参数可以用这种缩写语法来定义

{
    [CmdletBinding(SupportsPaging)]

    Param ($Parameter1)
    Begin{}
    Process{}
    End{}
}
```

## ConfirmImpact

`ConfirmImpact` argument 指定何时应通过调用 `ShouldProcess` 方法来确认该函数的动作.

只有当 `ConfirmImpact` 参数等于或大于 `$ConfirmPreference` 偏好变量的`值`时,
对 `ShouldProcess` 方法的调用才会显示 `prompt`. (该参数的默认值是 `Medium`)
只有当 `SupportedShouldProcess` argument 也被指定时, 才能指定这个 argument.
关于 `确认请求` 的更多信息, 请参阅[Requesting Confirmation][]

[Requesting Confirmation]: https://docs.microsoft.com/en-us/powershell/scripting/developer/cmdlet/requesting-confirmation

## DefaultParameterSetName

`DefaultParameterSetName` argument 指定了 `parameter set` 的名称,
当 `PowerShell` 无法确定使用哪个 `parameter set` 时, 它将尝试使用该参数集.
你可以通过, 将每个 `参数集` 中的 `独有参数` 设置为 `强制参数`(`mandatory`), 来避免这个问题.

## 帮助URI

`HelpURI` argument 指定了描述该函数的帮助主题的在线版本的互联网地址.
`HelpURI` argument 的值必须以 `http` 或 `https` 开头.

`HelpURI` argument value, 用于 `Get-Command` 为该函数返回的 `CommandInfo` `对象的HelpURI` 属性的值.

然而, 当计算机上安装了帮助文件, 并且帮助文件的 `RelatedLinks` 部分的第一个链接的值是 `URI`,
或者基于注释的帮助中第一个 `.Link` 指令的值是 `URI` 时,
帮助文件中的 `URI` 被用作函数的 `HelpUri` 属性的值.

当 `Get-Help` 的 `Online` 参数在命令中被指定时,
`Get-Help` cmdlet 使用 `HelpURI` 属性的值, 来定位函数帮助主题的在线版本.

## 支持分页

`SupportsPaging` argument 为函数添加了 `First`, `Skip` 和 `IncludeTotalCount` 参数.
这些参数允许用户从一个非常大的结果集中选择输出.
这个argument 是为那些从支持 `数据选择` 的大型 `数据存储` 中返回数据的cmdlet和函数设计的, 例如 `SQL` 数据库.

这个参数是在 Windows PowerShell 3.0 中引入的.

+ `First`: 只获取前 `n` 个对象.
+ `Skip`: 忽略前 `n` 个对象, 然后获取其余的对象.
+ `IncludeTotalCount`: 报告数据集中的对象的数量(`整数`), 后面是对象.
如果 `cmdlet` 不能确定总计数, 它会返回 `Unknown total count`.

`PowerShell` 包括 `NewTotalCount`, 这是一个辅助方法, 可以获得要返回的 `总计数` 值,
并包括对 `总计数` 值的准确性的估计.

下面的示例函数显示了, 如何在高级函数中添加对 `分页参数` 的支持:

```powershell
function Get-Numbers {
    [CmdletBinding(SupportsPaging)]
    param()

    $FirstNumber = [Math]::Min($PSCmdlet.PagingParameters.Skip, 100)
    $LastNumber = [Math]::Min($PSCmdlet.PagingParameters.First +
      $FirstNumber - 1, 100)

    if ($PSCmdlet.PagingParameters.IncludeTotalCount) {
        $TotalCountAccuracy = 1.0
        $TotalCount = $PSCmdlet.PagingParameters.NewTotalCount(100,
          $TotalCountAccuracy)
        Write-Output $TotalCount
    }
    $FirstNumber .. $LastNumber | Write-Output
}
```

## SupportsShouldProcess

`SupportsShouldProcess` argument 为该函数添加了 `Confirm` 和 `WhatIf` parameters.
`Confirm` parameter 在对管道中的每个对象运行命令前会提示用户.
`WhatIf` parameter 列出了该命令将做出的改变, 但不运行该命令.

## PositionalBinding

`PositionalBinding` argument 决定了默认情况下, 函数中的 `parameters` 是否是 `位置参数`.
默认值是 `$True`. 你可以使用 `PositionalBinding` argument的值为 `$False` 来禁用位置绑定.

`PositionalBinding` argument 是在 Windows PowerShell 3.0 中引入的.

当 parameters 是位置性的, `parameter名` 是可选的.
PowerShell 根据 `未命名参数值` 出现在函数命令中的顺序或位置,
将 `未命名参数值` 与 `函数参数` 绑定.

当 `parameters` 不是 `位置的`(它们是 `有名字的`), 参数名`(或名称的缩写,别名)在命令中是必需的.

当 `PositionalBinding` 为 `$True` 时, `函数参数` 默认是 `位置参数`.
`PowerShell` 按照 `parameters` 在函数中声明的顺序为其分配 `位置号`.

当 `PositionalBinding` 为 `$False` 时, 函数参数默认是 `非位置的`.
除非在 `parameter` 上声明了 `Parameter` 属性的 `Position argument`,
否则在函数中使用该 parameter 时, 必须包含 parameter 名称(或别名或缩写).

`Parameter attribute` 的 `Position argument` 优先于 `PositionalBinding 默认值`.
你可以使用 `Position argument` 来为 parameter 指定 `位置值`.
关于 `Position argument` 的更多信息, 请参阅 [about_Functions_Advanced_Parameters][].

[about_Functions_Advanced_Parameters]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.2

## Notes

高级函数中不支持 `SupportsTransactions` 参数.

## 函数注释文档

[about_Comment_Based_Help](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comment_based_help?view=powershell-7.2)

## 简要说明

描述了如何为函数和脚本编写 `基于注释` 的帮助主题.

## 长描述

您可以通过使用特殊的 `帮助注释关键字`, 为函数和脚本编写 `基于注释` 的帮助主题.

`Get-Help` cmdlet 显示基于注释的帮助, 其格式与显示从 `XML` 文件生成的 `cmdlet 帮助主题` 相同.
用户可以使用 `Get-Help` 的所有参数, 如 `Detailed`, `Full`, `Examples` 和 `Online`, 来显示基于注释的帮助的内容.

你也可以为函数和脚本编写 `基于XML` 的帮助文件.
为了使 `Get-Help` cmdlet 能够为 函数或脚本 找到 `基于 XML` 的帮助文件, 请使用 `.ExternalHelp` 关键字.
没有这个关键字, `Get-Help` 不能为函数或脚本找到基于 XML 的帮助主题.

本主题解释了如何为函数和脚本编写 `帮助主题`. 关于如何显示函数和脚本的帮助主题的信息, 请参见 [Get-Help][].

[Update-Help][] 和 [Save-Help][] cmdlet 只对 `XML` 文件起作用.
`可更新的帮助` 不支持基于注释的 `帮助主题`.

[Get-Help]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/get-help
[Update-Help]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/update-help
[Save-Help]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/save-help
