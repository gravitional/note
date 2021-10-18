# 自动变量

[about_Automatic_Variables](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_automatic_variables)
[Determine the OS version](https://stackoverflow.com/questions/44703646/determine-the-os-version-linux-and-windows-from-powershell)
[Use PowerShell to 寻找操作系统版本](https://devblogs.microsoft.com/scripting/use-powershell-to-find-operating-system-version/)

+ 简短说明; Automatic_Variables 是存储 `PowerShell` 的状态信息的变量.  这些变量由 `PowerShell` 创建和维护.
+ 长说明 ; 从概念上讲, 这些变量被视为只读.  即使它们 `可以` 写入, 但为了向后兼容 , `不应写入` 它们.

下面介绍 PowerShell 中的自动变量.

### `$$`,`$?`

+ `$$`; 包含会话收到的最后一行中的, 最后一个令牌.

+ `$?`; 包含最后一个命令的`执行状态`.  如果最后一个命令成功, 它包含 `True`; 如果失败, 它包含 `False`.

对于在管道中各个阶段运行的 cmdlet 和高级函数(例如在`process `和`end `块中),
在任意位置调用`this.WriteError()` or `$PSCmdlet.WriteError()` 将设置`$?`为`False`,
`this.ThrowTerminatingError()`和`$PSCmdlet.ThrowTerminatingError()` 类似.

`Write-Error` 在执行后总是立即将`$?`设置为 `False`, 但对于调用它的函数, 它不会将`$?`设置为`False`:

```powershell
function Test-WriteError
{
    Write-Error "Bad"
    $? # $false
}
```

对于后一种 用途, 应该使用`$PSCmdlet.WriteError()`.

对于本机命令 (二进制可执行程序) , 当`$LASTEXITCODE`为 `0`时, `$?`被设置为 `True`, 如果`$LASTEXITCODE` 为其他任意值, 则设置为`False`.

> 备注:
>在 `PowerShell 7`之前, 包含在括号内的语句`(...)`, 子表达式语法`$(...)`或数组表达式`@(...)`总是将`$?`重置为`True`, 因此`(Write-Error)`显示`$?`为`True`. 这一点在 `PowerShell 7` 中有所改变, `$?` 将总是反映表达式中运行的最后一条命令的实际结果.

## `$^`,`$_`,`$args`

+ `$^`; 包含会话收到的最后一行中的第一个token(令牌).
+ `$_`; 与`$PSItem`相同. 包含管道对象中的当前对象. 你可以使用这个变量, 对管道中的每个对象, 或选定的对象执行动作.

+ `$args`; 包含一个`数组`, 传递未声明的`参数值`给`函数`, `脚本`或`脚本块`.
当你创建`函数`时, 你可以通过使用`param`关键字来声明参数, 或者在`函数`名称后面的括号中添加一个`逗号`分隔的`参数列表`.

在`event action`(事件动作)中, `$args` 变量包含代表正在处理的事件参数的对象.
这个变量只在事件注册命令的`Action`块中被填充.
这个变量的值也可以在`Get-Event`返回的`PSEventArgs`对象的`SourceArgs`属性中找到.

## `$ConsoleFileName`

包含会话中最近使用的控制台文件`(.psc1`)的路径.
当你用 `PSConsoleFile` 参数启动 `PowerShell` , 或使用 `Export-Console` cmdlet 将 `snap-in` 名称导出到控制台文件时, 这个变量被填充.

当你不带参数使用 `Export-Console` cmdlet 时, 它会自动更新会话中最近使用的`控制台文件`.
你可以使用这个`自动变量`来决定哪个文件将被更新.

## `$Error`,`$ErrorView`

`$Error` 为包含错误对象的`数组`, 代表最近的`错误`. 最近的错误是数组 `$Error[0]` 中的第一个错误对象.

为防止`错误`被添加到 `$Error` 数组中, 使用 `ErrorAction` 通用参数, 其值为 `Ignore`.
更多信息, 请参阅 [about_CommonParameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_commonparameters?view=powershell-7.1).

`$ErrorView` 包含的`值` 控制错误显示在哪个视图.
`$ErrorView` 变量接受 `字符串` 或 `ErrorView` 对象, 其默认值为 `ConciseView`.
如果定义了一个`非可接受值`的字符串, 就会产生错误.

接受的值.

+ `CategoryView` ; 只显示 `错误类别` 信息.
+ `ConciseView` ; 只显示 `错误信息`. 如果错误是一个`解析器错误`或来自`脚本`, 则包括一个位置指针. 这个视图是在PowerShell 7.0中添加的.
+ `NormalView` ; 提供一个标准的 `PowerShell` 错误视图, 包含 `错误信息`, `位置`, `类别信息` 等.

## `$Event`,`$EventArgs`,`$ExecutionContext`

`$Event` 包含一个 `PSEventArgs` 对象, 代表正在处理的事件.
这个变量只在 `event registration`(事件注册) 命令的 `Action块` 中被填充, 比如 `Register-ObjectEvent`.
这个变量的值与 `Get-Event` cmdlet 返回的对象相同.
因此, 你可以在 `Action` 脚本块中使用 `Event` 变量的属性, 如 `$Event.TimeGenerated`.

`$EventArgs` 包含代表第一个`事件参数`的对象, 该`参数`源自正在处理的事件的 `EventArgs`.
这个变量只在事件注册命令的 `Action` 块中被填充.
这个变量的值也可以在 `Get-Event` 返回的 `PSEventArgs` 对象的 `SourceEventArgs` 属性中找到.

`$EventSubscriber` ; 包含 `PSEventSubscriber` 对象, 代表正在处理的事件的`事件订阅者`.
这个变量只在事件注册命令的 `Action` 块中被填充.
这个变量的值与 `Get-EventSubscriber` cmdlet 返回的对象相同.

`$ExecutionContext` ; 包含 `EngineIntrinsics` 对象, 代表 `PowerShell` 主机的执行环境.
你可以使用这个变量来寻找对 `cmdlets` 可用的执行对象.

## $false,$foreach

+ `$false` ; 包含 `False`. 你可以在命令和脚本中使用这个变量来表示 `False`, 而不是使用字符串 `"false"`.
如果 `字符串` 被转换为`非空的`字符串或者`非零的`整数, 它可能被解释为 `True`.

+ `$true` ; 包含True. 你可以在命令和脚本中使用这个变量来表示True.

+ `$foreach` ; 包含 `ForEach` 循环的`枚举器`(enumerator, 而不是`结果值`).
`$ForEach` 变量只在 `ForEach` 循环运行时存在; 在循环结束后它会被删除.

`枚举器` 包含 `属性` 和 `方法`, 你可以用来检索`循环值`和改变当前循环的迭代.
更多信息, 请参见 [使用枚举器][]

[使用枚举器]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1#using-enumerators.

## `$HOME`,`$Host`

`$HOME` 包含用户的主目录的完整路径.
这个变量相当于 `"$env:homedrive$env:homepath"` 的 `Windows` 环境变量, 通常是 `C:\Users\<UserName>`.

`$Host` ; 包含一个对象, 代表 `PowerShell` 的当前`主机应用`(host application).
你可以用这个变量在命令中代表当前的主机, 或者显示或改变主机的属性, 比如 `$Host.version` 或 `$Host.CurrentCulture`,
或者 `$host.ui.rawui.setbackgroundcolor("Red")`.

## `$IsCoreCLR`,`$IsLinux`,`$IsMacOS`

`$IsCoreCLR` ; 如果当前会话在 `.NET Core Runtime(CoreCLR)` 上运行, 则包含 `$True`, 否则包含 `$False`.

`CLR`, 全名 `Common Language Runtime`, 是微软为 `.NET` 实现的托管运行时环境(或者叫"虚拟机").
`CLR` 也是ECMA-335 Common Language Infrastructure规范的一个实现.

`$IsLinux` ; 如果当前会话在Linux操作系统上运行, 则包含 `$True`, 否则包含 `$False`.

`$IsMacOS` ; 如果当前会话运行在 `MacOS` 操作系统上, 则包含 `$True`, 否则包含 `$False`.

`$IsWindows` ; 如果当前会话运行在 `Windows` 操作系统上, 则包含 `$TRUE`, 否则包含 `$FALSE`.

`$LastExitCode` ; 包含最后运行的本地程序的退出代码.

## `$Matches`

`$Matches` 变量与 `-match` 和 `-notmatch` 操作符一起工作.
当你向 `-match` 或 `-notmatch` 操作符提交 `标量输入` 时, 如果这两个操作符检测到`匹配`, 它们会返回一个布尔值,
并在 `$Matches` 自动变量中填充任何匹配的字符串值的`哈希表`.
当你使用 `正则表达式` 和 `-match` 操作符时, `$Matches 哈希表` 也将用 `捕获的数据` 填充.

关于 `-match` 操作符的更多信息, 见 [about_Comparison_Operators][].
关于正则表达式的更多信息, 请看 [about_Regular_Expressions][].

`$Matches` 变量也可以在带有 `-Regex` 参数的 `switch` 语句中使用.
它的填充方式与 `-match` 和 `-notmatch` 操作符的填充方式相同.
关于`switch`语句的更多信息, 请参见 [about_Switch][].

[about_Switch]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_switch?view=powershell-7.1

> 注意

在会话中, 当 `$Matches` 被填充时, 它保留了匹配的值, 直到它被另一个匹配值覆盖.
如果再次使用 `-match` 而没有找到匹配值, 它不会将 `$Matches` 重置为 `$null`.
之前匹配的值会保留在`$Matches`中, 直到找到另一个匹配值.

[about_Comparison_Operators]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1
[about_Regular_Expressions]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1

## `$MyInvocation`

`$MyInvocation` 包含了关于当前命令的信息, 如 `名称`, `参数`, `参数值`,
以及关于该命令如何被启动, 调用的信息, 如调用当前命令的`脚本`的名称.

`$MyInvocation` 只在脚本, 函数和脚本块的情形被填充值(populated).
你可以使用 `$MyInvocation` 在当前脚本中返回的, `System.Management.Automation.InvocationInfo` 对象中的信息,
如脚本的 `路径` 和 `文件名`(`$MyInvocation.MyCommand.Path`), 或函数的名称(`$MyInvocation.MyCommand.Name`)来识别当前命令.
这对于查找`当前脚本的名称`特别有用.

从PowerShell 3.0开始, `MyInvocation` 有以下新属性.

+ `PSScriptRoot` ; 包含调用当前命令的脚本的完整路径. 这个属性的值只有在`调用者`是`脚本`时才会被填充.
+ `PSCommandPath` ; 包含调用当前命令的脚本的完整路径和文件名. 这个属性的值只有在调用者是脚本时才会被填入.

与 `$PSScriptRoot` 和 `$PSCommandPath` 自动变量不同,
`$MyInvocation` 自动变量的 `PSScriptRoot` 和 `PSCommandPath` 属性包含关于`调用者`或调用脚本的信息, 而不是`当前脚本`(current script).

## `$NestedPromptLevel`

`$NestedPromptLevel` 包含了当前的 `prompt` 级别.
值为 `0` 表示原始 `prompt`级别. 当你进入一个嵌套级别时, 该值会被递增, 当你退出该级别时, 会被递减.

例如, 当你使用 `$Host.EnterNestedPrompt` 方法时, `PowerShell` 呈现一个嵌套的命令`prompt`.
当你在 `PowerShell` 调试器中达到一个断点时, `PowerShell` 也会呈现一个嵌套命令`prompt`.

当你进入一个嵌套`prompt`时, `PowerShell` 会暂停当前命令, 保存执行环境, 并增加 `$NestedPromptLevel` 变量的值.
要创建额外的嵌套命令提示(最多128级)或返回原始命令提示, 请完成命令, 或键入`exit`.

`$NestedPromptLevel` 变量帮助你跟踪`prompt`级别.
你可以创建一个包括这个值的, alternative `PowerShell command prompt`, 以便它总是可见.

## `$null`

`$null` 是一个自动变量, 它包含`null`或空值(empty value).
你可以在命令和脚本中, 使用这个变量来表示一个`不存在`或`未定义`的值.

`PowerShell`把 `$null` 当作一个有`值`的对象, 也就是一个明确的`占位符`, 所以你可以用 `$null` 来代表一系列值中的空值.

例如, 当 `$null` 被包含在一个集合中时, 它被算作是其中的一个对象.

```powershell
$a = "one", $null, "three"
$a.count

OutPut
3
```

如果你把 `$null` 变量输送给 `ForEach-Object` cmdlet, 它就会为 `$null` 生成一个值, 就像对待其他对象一样

```PowerShell
"one", $null, "three" | ForEach-Object { "Hello " + $_}

Output
Hello one
Hello
Hello three
```

因此, 你不能用 `$null` 来表示没有参数值. 值为 `$null` 的参数, 会覆盖参数的默认值.

然而, 由于 `PowerShell` 将 `$null` 变量视为占位符, 你可以在下面这样的脚本中使用它, 如果忽略了 `$null`, 就无法工作.

```PowerShell
$calendar = @($null, $null, "Meeting", $null, $null, "Team Lunch", $null)
$days = "Sunday","Monday","Tuesday","Wednesday","Thursday",
        "Friday","Saturday"
$currentDay = 0
foreach($day in $calendar)
{
    if($day -ne $null)
    {
        "Appointment on $($days[$currentDay]): $day"
    }

    $currentDay++
}

Output
Appointment on Tuesday: Meeting
Appointment on Friday: Team lunch
```

## `$PID`,`$PROFILE`

`$PID`包含承载当前 `PowerShell` 会话的进程的, `进程标识符`(`PID`).

`$PROFILE`, 包含`当前用户` 和 `当前 host 应用程序` 的 `PowerShell` 配置文件的完整路径.
你可以在命令中使用这个变量来表示配置文件.
例如, 你可以在命令中使用它来确定是否已经创建了一个配置文件.

```PowerShell

Test-Path $PROFILE
```

或者, 你可以在一个命令中使用它来创建一个配置文件.

```PowerShell
New-Item -ItemType file -Path $PROFILE -Force
```

你可以在命令中使用它, 在 `notepad.exe` 中打开配置文件.

```PowerShell
notepad.exe $PROFILE
```

## `$PSBoundParameters`

`$PSBoundParameters` 包含传给`脚本`或`函数`的 `参数的字典`(hash-map), 以及它们的当前值.
这个变量只在声明该参数的范围内 有值, 比如脚本或函数.
你可以用它来显示或改变参数的当前值, 或者将参数值传递给另一个脚本或函数.

在这个例子中, `Test2` 函数将 `$PSBoundParameters` 传递给 `Test1` 函数.
`$PSBoundParameters` 是以 `Key` 和 `Value` 的格式显示的.

```PowerShell
function Test1 {
   param($a, $b)

   # Display the parameters in dictionary format.
   $PSBoundParameters
}

function Test2 {
   param($a, $b)

   # Run the Test1 function with $a and $b.
   Test1 @PSBoundParameters
}

Test2 -a Power -b Shell

Output
Key   Value
---   -----
a     Power
b     Shell
```

## `$PSCmdlet`,`$PSCommandPath`,`$PSCulture`,`$PSDebugContext`

+ `$PSCmdlet` 包含一个代表 `正在运行` 的 cmdlet 或 高级函数的对象.

你可以在你的 `cmdlet` 或函数代码中, 使用该对象的`属`性和`方法`来响应`使用条件`.
例如, `ParameterSetName` 属性包含正在使用的`参数集`的名称,
`ShouldProcess` 方法动态地将 `WhatIf` 和 `Confirm` 参数添加到 cmdlet 中.

关于 `$PSCmdlet` 自动变量的更多信息, 请参阅 about_Functions_CmdletBindingAttribute 和 [about_Functions_Advanced_Parameters][].

+ `$PSCommandPath` ; 包含`正在运行`的脚本的完整路径和文件名. 这个变量在所有脚本中都有效.

+ `$PSCulture` ; 从 PowerShell 7 开始, `$PSCulture` 反映了当前 `PowerShell` 运行空间(会话)的文化.
如果 PowerShell 运行空间中的文化被改变, 该运行空间的 `$PSCulture` 值也会被更新.

`culture` 决定了 `数字`, `货币` 和 `日期` 等项目的显示格式, 并被存储在 `System.Globalization.CultureInfo` 对象中.
使用 `Get-Culture` 来显示计算机的文化. `$PSCulture` 包含 `Name` 属性的值.

+ 在调试时, `$PSDebugContext` 包含调试环境的信息. 否则, 它包含一个`null`值.
因此, 你可以用它来表示`debugger`是否有控制权.
当填充时, 它包含一个 `PsDebugContext` 对象, 它有 `Breakpoints` 和 `InvocationInfo` 属性.
`InvocationInfo` 属性有几个有用的属性, 包括 `Location` 属性. `Location` 属性表示被调试的脚本的路径.

## `$PSHOME`,`$PSItem`,`$PSScriptRoot`

+ `$PSHOME` 包含 `PowerShell` 安装目录的完整路径,

在 `Windows` 系统中通常为 `$env:windir\System32\PowerShell\v1.0`.
你可以在 `PowerShell` 文件的路径中使用这个变量.
例如, 下面的命令在概念帮助主题中搜索`variable`这个词.

```PowerShell
Select-String -Pattern Variable -Path $pshome\*.txt
```

+ `$PSItem` 与 `$_` 等同. 包含管道对象中的`当前对象`, 你可以在那些逐个操纵管道中对象的命令, 或操纵管道中选定对象的命令中使用这个变量.

+ `$PSScriptRoot` ; 包含执行脚本的父目录的完整路径.
在 PowerShell 2.0 中, 这个变量只在`脚本模块`(`.psm1`)中有效. 从PowerShell 3.0开始, 它在所有脚本中都有效.

+ `$PSSenderInfo` ; 包含启动 `PSSession` 的用户的信息, 包括用户身份和`发送端`计算机的时区.

这个变量只在 `PSSession`中可用.
`$PSSenderInfo` 变量包括一个用户可配置的属性, `ApplicationArguments`, 默认情况下, 它只包含来自起始会话的 `$PSVersionTable`.
要向 `ApplicationArguments` 属性添加`数据`, 使用 `New-PSSessionOption` cmdlet的 `ApplicationArguments` 参数.

+ `$PSUICulture` ; 包含当前在操作系统中使用的用户界面(UI)文化的名称.

`UI Culture` 决定了哪些文本字符串被用于用户界面元素, 如菜单和信息.
这是系统的 `System.Globalization.CultureInfo.CurrentUICulture.Name` 属性的值.
要获得系统的 `System.Globalization.CultureInfo` 对象, 请使用 `Get-UICulture` cmdlet.

## `$PSVersionTable`,`$PWD`,`$Sender`

包含一个只读的`哈希表`, 显示关于当前会话中正在运行的 `PowerShell` 版本的详细信息.
该表包括以下项目.

+ `PSVersion` ; `PowerShell` 的版本号
+ `PSEdition` ; 对于 `PowerShell 4` 及以下版本, 以及全功能 `Windows` 版本的PowerShell 5.1, 该属性的值为`'Desktop'`.
对于 `PowerShell 6` 及以上版本以及在 `Windows Nano Server` 或 `Windows IoT` 等减缩版上的 `PowerShell PowerShell 5.1`, 该属性的值为 `Core`.
+ `GitCommitId` ; `源文件`的 commit id, 在 `GitHub` 中.
+ `OS` ; `PowerShell` 所运行的操作系统的描述.
+ `Platform` ; 操作系统所运行的平台. 在 `Linux` 和 `macOS` 上的值是 `Unix`. 参见 `$IsMacOs` 和 `$IsLinux`.
+ `PSCompatibleVersions` ; 与当前版本兼容的 `PowerShell` 的版本.
+ `PSRemotingProtocolVersion` ;  `PowerShell 远程管理协议`的版本.
+ `SerializationVersion` ;  `序列化方法`的版本
+ `WSManStackVersion` ;  `WS-Management 堆栈` 的版本号

+ `$PWD` 包含一个路径对象, 表示当前 PowerShell 运行空间的, 当前目录位置的完整路径.

>注意:`PowerShell` 支持`每个进程`拥有多个`运行空间`(runspaces). 每个`运行空间`都有自己的 `当前目录`.
>这与`进程`的`当前目录`不一样: `[System.Environment]::CurrentDirectory`.

+ `$Sender` ; 包含产生此事件的`对象`.

这个变量只在事件注册命令的 `Action` 块中被填充.
这个变量的值也可以在 `Get-Event` 返回的 `PSEventArgs` 对象的 `Sender` 属性中找到.

+ `$ShellId` ; 包含当前`shell`的`标识符`.

+ `$StackTrace` ; 包含最近一次`错误`的`堆栈跟踪`(stack trace).

## `$switch`

包含`枚举器`(enumerator), 不是 `Switch` 语句的结果值. `$switch` 变量只在 `Switch` 语句运行时存在;
当 `switch` 语句执行完毕时, 它将被删除.
欲了解更多信息, 请参见 [about_Switch](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_switch?view=powershell-7.1).

`枚举器`包含属性和方法, 你可以用来检索`循环值`和改变当前`循环迭代`.
更多信息, 请参见 [使用枚举器][].

## `$this`

在一个定义了脚本`属性`或脚本 `方法` 的脚本块中, `$this` 变量指的是正在被扩展的 `对象`.
在一个`自定义类`中, `$this` 变量指的是 `类对象` 本身, 允许访问类中定义的`属性`和`方法`.

## 使用枚举器

`$input`, `$foreach`, 和 `$switch` 变量都是`枚举器`, 用于`遍历` 它们所包含的代码块要处理的值.

`枚举器` 包含属性和方法, 你可以用来`推进`或`重置`迭代, 或检索迭代值.
直接操作`枚举器`并不被认为是最佳做法.

### `$input`

包含一个`枚举器`, 用于枚举所有传递给函数的`输入`.
`$input` 变量只对函数和脚本块(属于`未命名的函数`)有效.

+ 在一个没有 `Begin`, `Process` 或 `End` 块的函数中, `$input` 变量枚举, 该函数所有输入组成的集合.
+ 在 `Begin` 块中, `$input` 变量不包含任何数据.
+ 在 `Process` 块中, `$input` 变量包含当前在 `管道` 中的对象.
+ 在 `End` 块中, `$input` 变量枚举, 该函数的所有`输入` 组成的集合.

>注意:
>你不能在同一个`函数`或`脚本块` 中的 `Process`块 和 `End`块中, 同时使用 `$input` 变量.

因为 `$input` 是一个枚举器(enumerator), 访问它的任何属性都会导致 `$input` 不再可用.
你可以将 `$input` 存储在另一个变量中, 以便重复使用 `$input` 的属性.

枚举器包含属性和方法, 你可以用来检索`循环值`和改变当前循环的迭代.
更多信息, 请参见 [使用枚举器][].

当从命令行调用时, `$input` 变量也可用于 `pwsh` 的 `-Command` 参数指定的命令.
下面的例子是从 Windows Command shell 中运行的.

```CMD
echo Hello | pwsh -Command ""$input World!""
```

+ 在循环内, 应首选流控制关键字 `break` 和 `continue`.
+ 在接受管道输入的函数中, 最佳做法是使用具有 `ValueFromPipeline` 或 `ValueFromPipelineByPropertyName` 属性的参数.
+ 欲了解更多信息, 请参阅 [about_Functions_Advanced_Parameters][].

[about_Functions_Advanced_Parameters]: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced?view=powershell-7.1

### MoveNext

`MoveNext` 方法将 `枚举器` 推进到集合的下一个元素.
如果`枚举器`被成功推进, `MoveNext` 返回 `True`; 如果枚举器已经超过了集合的末端, 则返回 `False`.

>注意:
由 `MoveNext` 返回的`布尔值`被发送到`输出流`.
你可以通过将其类型转换为`[void]`, 或将其输送到 `Out-Null` 来抑制输出.

```PowerShell
$input.MoveNext() | Out-Null # or
[void]$input.MoveNext()
```

### Reset

[Reset][] 方法将`枚举器`设置到它的初始位置,
也就是在集合的第一个元素`之前`.

[Reset]: (https://docs.microsoft.com/en-us/dotnet/api/system.collections.ienumerator.reset?view=net-5.0)

### Current

[Current][] 属性获得集合或管道中的元素 , 在`枚举器`的当前位置.

在调用 `MoveNext` 之前, `Current` 属性会继续返回同一属性.

[Current]: https://docs.microsoft.com/en-us/dotnet/api/system.collections.ienumerator.current?view=net-5.0

## 例子

### 使用`$input`变量

在下面的例子中, 访问 `$input` 变量会清除该变量, 直到下一次执行该`进程块`时, `$input` 才会被重新赋值.
使用 `Reset` 方法将 `$input` 变量重置为当前管道值.

```powershell
function Test
{
    begin
    {
        $i = 0
    }

    process
    {
        "Iteration: $i"
        $i++
        "`tInput: $input" # `t 表示指标符, `是 powershell 的转义符号
        "`tAccess Again: $input"
        $input.Reset()
        "`tAfter Reset: $input"
    }
}

"one","two" | Test
```

```log
Iteration: 0
    Input: one
    Access Again:
    After Reset: one
Iteration: 1
    Input: two
    Access Again:
    After Reset: two
```

即使你不访问 `$input` 变量, 进程块也会自动推进它.

```powershell
$skip = $true
function Skip
{
    begin
    {
        $i = 0
    }

    process
    {
        "Iteration: $i"
        $i++
        if ($skip)
        {
            "`tSkipping"
            $skip = $false
        }
        else
        {
            "`tInput: $input" # 会自动前进到下一个输入
        }
    }
}

"one","two" | Skip
```

```log
Iteration: 0
    Skipping
Iteration: 1
    Input: two
```

### 在进程块外使用`$input`

在进程块之外, `$input` 变量代表所有进入函数的值.

+ 访问 `$input` 变量将清除所有的值.
+ `Reset` 方法重置整个集合.
+ `Current` 属性从未被填充.
+ `MoveNext` 方法返回 `false`, 因为这个集合无法被推进.
    + 调用 `MoveNext` 会清除 `$input` 变量.

```powershell
Function All
{
    "All Values: $input"
    "Access Again: $input"
    $input.Reset()
    "After Reset: $input"
    $input.MoveNext() | Out-Null
    "After MoveNext: $input"
}

"one","two","three" | All

All Values: one two three
Access Again:
After Reset: one two three
After MoveNext:
```

### 使用`$input.Current`属性

通过使用 `Current` 属性, 可以多次访问当前的`管道值`, 而无需使用 `Reset` 方法.
过程块不会自动调用 `MoveNext` 方法.

除非你明确地调用 `MoveNext`, 否则 `Current` 属性将永远不会被填充.
在进程块内可以多次访问 `Current` 属性, 而不清除其值.

```powershell
function Current
{
    begin
    {
        $i = 0
    }

    process
    {
        "Iteration: $i"
        $i++
        "`tBefore MoveNext: $($input.Current)"
        $input.MoveNext() | Out-Null
        "`tAfter MoveNext: $($input.Current)"
        "`tAccess Again: $($input.Current)"
    }
}

"one","two" | Current
out:

Iteration: 0
    Before MoveNext:
    After MoveNext: one
    Access Again: one
Iteration: 1
    Before MoveNext:
    After MoveNext: two
    Access Again: two
```

### 使用`$foreach`变量

与 `$input` 变量不同, 在直接访问时, `$foreach` 变量总是代表集合中的所有项目.
使用 `Current` 属性来访问当前的集合元素, 并使用 `Reset` 和 `MoveNext` 方法来改变其值.

>注意:
>`foreach` 循环的每一次迭代都会自动调用 `MoveNext` 方法.

下面这个循环只执行了两次.
在第二次迭代中, 集合在迭代完成前被移动到第三个元素.
在第二次迭代之后, 现在已经没有更多的值可以迭代了, 循环终止.

`MoveNext` 属性并不影响, 被选择在集合上迭代的变量(`$Num`).

```powershell
$i = 0
foreach ($num in ("one","two","three"))
{
    "Iteration: $i"
    $i++
    "`tNum: $num"
    "`tCurrent: $($foreach.Current)"

    if ($foreach.Current -eq "two")
    {
        "Before MoveNext (Current): $($foreach.Current)"
        $foreach.MoveNext() | Out-Null
        "After MoveNext (Current): $($foreach.Current)"
        "Num has not changed: $num"
    }
}

out:
Iteration: 0
        Num: one
        Current: one
Iteration: 1
        Num: two
        Current: two
Before MoveNext (Current): two
After MoveNext (Current): three
Num has not changed: two
```

使用 `Reset` 方法可以重置集合中的`当前元素`.
下面的例子在前两个元素中循环了两次, 因为调用了 `Reset` 方法.
在前两次循环后, `if` 语句失效, 循环正常遍历所有三个元素.

>重要:
>这可能会导致无限循环. `Reset` 会改变迭代变量 `$num` 的值.

```powershell
$stopLoop = 0
foreach ($num in ("one","two", "three"))
{
    ("`t" * $stopLoop) + "Current: $($foreach.Current)"

    if ($num -eq "two" -and $stopLoop -lt 2)
    {
        $foreach.Reset() | Out-Null
        ("`t" * $stopLoop) + "Reset Loop: $stopLoop"
        $stopLoop++
    }
}

Current: one
Current: two
Reset Loop: 0
        Current: one
        Current: two
        Reset Loop: 1
                Current: one
                Current: two
                Current: three
```

### 例5: 使用$switch变量

`$switch` 变量与 `$foreach` 变量的规则完全相同. 下面的例子演示了所有的枚举器概念.

>注意:
>注意 `NotEvaluated` case 从未被执行, 即使在 `MoveNext` 方法之后没有使用 `break` 语句.

```powershell
$values = "Start", "MoveNext", "NotEvaluated", "Reset", "End"
$stopInfinite = $false
switch ($values)
{
    "MoveNext" {
        "`tMoveNext"
        $switch.MoveNext() | Out-Null
        "`tAfter MoveNext: $($switch.Current)"
    }
    # This case is never evaluated.
    "NotEvaluated" {
        "`tNotEvaluated: $($switch.Current)"
    }

    "Reset" {
        if (!$stopInfinite)
        {
            "`tReset"
            $switch.Reset()
            $stopInfinite = $true
        }
    }

    default {
        "Default (Current): $($switch.Current)"
    }
}

OutPut:
Default (Current): Start
    MoveNext
    After MoveNext: NotEvaluated
    Reset
Default (Current): Start
    MoveNext
    After MoveNext: NotEvaluated
Default (Current): End
```
