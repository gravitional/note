# Parameters 默认值

[about_Parameters_Default_Values]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_parameters_default_values

简要说明; 描述了如何为 cmdlet 参数和高级函数设置 `自定义默认值`.

## 长描述

通过 `$PSDefaultParameterValues` 这个偏好变量, 你可以为任何 `cmdlet` 或高级函数指定自定义 `默认值`.
除非你在命令中指定另一个值, 否则 cmdlet 和高级函数会使用自定义默认值.

`cmdlet` 和高级函数的作者为他们的参数设置了 `标准默认值`.
通常情况下, `标准默认值` 是有用的, 但它们可能不适合所有环境.

当你几乎每次使用命令都必须指定相同的 `其他参数值` 时,
或者当某个参数值难以记住时, 例如 `电子邮件服务器名称` 或 `项目GUID`, 这个功能就特别有用.

如果所需的 `默认值` 可预测地变化, 你可以指定一个 `脚本块`,
在不同条件下为参数提供不同的默认值.

`$PSDefaultParameterValues` 是在PowerShell 3.0中引入的.

## 语法

`$PSDefaultParameterValues` 变量是个 `哈希表`,
它验证`键`的格式是`System.Management.Automation.DefaultParameterDictionary` 的对象类型.
该哈希表包含 `键/值对`. 键的格式是 `CmdletName:ParameterName`.
`值` 是分配给该 `键` 的 `DefaultValue` 或 `ScriptBlock`.

`$PSDefaultParameterValues` 偏好变量的语法如下:

```powershell
$PSDefaultParameterValues=@{"CmdletName:ParameterName"="DefaultValue"}

$PSDefaultParameterValues=@{ "CmdletName:ParameterName"={{ScriptBlock}} }

$PSDefaultParameterValues["Disabled"]=$True | $False
```
