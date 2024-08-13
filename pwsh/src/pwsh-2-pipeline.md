# about_Pipelines

[about_Pipelines](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_pipelines)

## `-passthru`

[out-gridview](https://learn.microsoft.com/zh-cn/powershell/module/microsoft.powershell.utility/out-gridview)

某些 cmdlet 具有 `-PassThru` 选项, 例如对于 `out-gridview`,
作用是指示 `out-gridview` 将用户通过 UI 选中的项, 作为管道右边命令的输入.
默认情况下, `out-gridview` 没有返回值.
此参数等效于使用 `OutputMode` 参数的 `Multiple` 值.

若要将交互式窗口中的项通过管道向下发送, 请单击以选择项, 然后单击"确定".
支持通过 Shift 和 Ctrl 多选. 例子

```powershell
test ls | Out-GridView -PassThru | ls
```

## 简要说明

在PowerShell中把命令组合成 `管道`(pipelines)

## 长描述

`管道` 是一系列由 `管道操作符`(`|`)(ASCII 124)连接的命令.
每个 `管道运算符` 都会将前一命令的结果发送到下个命令.

第一条命令的输出 可以作为 第二条命令的输入 被送去处理.
而这个输出又可以被发送到另一个命令.
结果是个复杂的 `命令链` 或 `pipeline`, 由一系列的简单命令组成.
比如说.:

```powershell
Command-1 | Command-2 | Command-3
```

在这个例子中, `Command-1` 发出的对象被发送到 `Command-2`.
`Command-2` 处理这些对象并把它们送到 `Command-3`.
`Command-3` 处理这些对象, 并将其发送到管道中.
因为管道中没有更多的命令了, 所以结果显示在控制台.

在流水线中, 命令是按从左到右的顺序处理的.
每个处理过程被当作单一的操作来处理, 输出在生成时被显示出来.

下面是一个简单的例子. 下面的命令获得记事本进程, 然后停止它:

```powershell
Get-Process notepad | Stop-Process
```

第一条命令使用 `Get-Process` cmdlet来获取代表 `记事本进程` 的对象.
然后使用管道操作符 `|` 将进程对象发送到 `Stop-Process` cmdlet,
从而停止记事本进程.
注意, `Stop-Process` 命令无需指定进程的 `Name` 或 `ID` 参数,
因为指定的进程是通过 `pipeline` 提交的.

下面这个管道的例子 获取当前目录中的文本文件,
只选择长度超过 `10,000` 字节的文件,
按 `长度` 排序, 并在表格中显示每个文件的名称和长度:

```powershell
Get-ChildItem -Path *.txt |
  Where-Object {$_.length -gt 10000} |
    Sort-Object -Property length |
      Format-Table -Property name, length
```

## 使用管道

大多数 `PowerShell` cmdlet被设计为支持管道.
在大多数情况下, 你可以将 `Get-xxx` cmdlet的结果管道到另一个同名的cmdlet.
例如, 你可以将 `Get-Service` cmdlet的输出管道到 `Start-Service` 或 `Stop-Service` cmdlet.

这个例子的管道启动计算机上的WMI服务.

```PowerShell
Get-Service wmi | Start-Service
```

在另一个例子中, 你可以把 `PowerShell` 注册表provider 中的
`Get-Item` 或 `Get-ChildItem` 的输出 `pipe` 到 `New-ItemProperty` cmdlet.
这个例子为 `MyCompany` 注册表键 添加 新的注册表项 `NoOfEmployees`,
其值为 `8124`.

```PowerShell
Get-Item -Path HKLM:\Software\MyCompany |
  New-ItemProperty -Name NoOfEmployees -Value 8124
```

许多实用的 cmdlet, 如 `Get-Member`, `Where-Object`, `Sort-Object`,
`Group-Object` 和 `Measure-Object`, 几乎只在管道中使用.
你可以用管道将任何对象类型传送给这些 cmdlets.
这个例子显示了如何按照每个进程 中 打开的句柄数量,
对计算机上的所有进程进行排序.

```PowerShell
Get-Process | Sort-Object -Property handles
```

你可以用管道将对象输送到 `格式化`,  `export` 和 `output ` 类型的cmdlet,
如 `Format-List`, `Format-Table`, `Export-Clixml`, `Export-CSV` 和`Out-File`.
这个例子显示了如何使用 `Format-List` cmdlet来显示一个进程对象的属性列表.

```PowerShell
Get-Process winlogon | Format-List -Property *
```

你也可以把本地命令的输出管道到PowerShell cmdlet. 比如说.

```PowerShell
PS> ipconfig.exe | Select-String -Pattern 'IPv4'

   IPv4 Address. . . . . . . . . . . : 172.24.80.1
   IPv4 Address. . . . . . . . . . . : 192.168.1.45
   IPv4 Address. . . . . . . . . . . : 100.64.108.37
```

>重要信息

`Success`  和 `Error`  流类似于其他 shell 的 `stdin` 和 `stderr` 流.
然而, stdin 并没有连接到 PowerShell 的 输入管道.
欲了解更多信息, 请看 [about_Redirection](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_redirection).

经过一段时间的练习, 你会发现将简单的命令组合成管道可以节省时间和打字,
并使你的脚本更有效率.

## 管道的工作原理

本节解释了输入对象如何与 cmdlet 参数绑定, 并在管道执行期间进行处理.

### 接受管道输入

为了支持 pipeline, 接收方的cmdlet必须有接受管道输入的参数.
使用带有 `-Full` 或 `-Parameter` 选项的 Get-Help 命令,
来确定 cmdlet 的哪些参数接受管道输入.

例如, 要确定 `Start-Service` 的哪些参数接受管道输入, 请输入:

```PowerShell
Get-Help Start-Service -Full
# 或
Get-Help Start-Service -Parameter *
```

`Start-Service` cmdlet的帮助显示, 只有 `InputObject` 和 `Name` 参数接受管道输入.

当你通过管道向 `Start-Service` 发送对象时, PowerShell 会尝试将对象与 `InputObject` 和 `Name` 参数相关联.

### 接受管道输入的方法

Cmdlets参数可以以两种不同的方式之一接受管道输入:

+ `ByValue`: 该参数接受符合预期的 `.NET` 类型, 或可转换为该类型的值.
    例如, `Start-Service` 的Name参数接受管道输入的值.
    它可以接受字符串对象或可以转换为字符串的对象.

+ `ByPropertyName`. 只有输入对象具有同名参数时, 该参数才接受输入.
    例如, `Start-Service` 的 `Name` 参数可以接受具有 `Name` 属性的对象.
    要列出一个对象的属性, 请把对象 管道给 `Get-Member`.

一些参数可以通过 `值` 或 `属性` 接受对象, 使之更容易从管道中获取输入.

### 参数绑定

当你把对象从一个命令输送到另一个命令时,
PowerShell会尝试把输送的对象与接收的cmdlet的一个参数联系起来.
PowerShell 的参数绑定组件根据以下标准将输入对象与cmdlet参数联系起来.

+ 该参数需要能够接收 来自管道的输入.
+ 该参数必须能够接收 被发送对象的类型, 或可以转换为预期类型的类型.
+ 该参数没有在命令中使用.

例如, `Start-Service` cmdlet有许多参数, 但只有两个, `Name` 和 `InputObject` 接受管道输入.
`Name` 参数接受字符串, `InputObject` 参数接受服务对象.
因此, 你可以用管道输入字符串,
`服务对象` 以及具有可转换为 字符串 或 服务对象 的属性的对象.

PowerShell尽可能有效地管理参数绑定.
你不能建议或强迫 PowerShell 绑定到一个特定的参数.
如果PowerShell不能绑定管道对象, 则命令失败.

有关排除绑定错误的更多信息, 请参阅本文后面的调查管道错误.

### 逐一处理

使用管道向命令传递对象, 很像使用命令的参数来提交对象.
让我们看看一个管道的例子. 在这个例子中,
我们用一个管道来显示一个服务对象的表格.

```PowerShell
Get-Service | Format-Table -Property Name, DependentServices
```

在功能上, 这就像使用Format-Table的InputObject参数来提交对象集合.
例如, 我们可以将服务的集合保存到使用InputObject参数传递的一个变量中.

```PowerShell
$services = Get-Service
Format-Table -InputObject $services -Property Name, DependentServices
```

或者我们可以把命令嵌入到InputObject参数中.

```PowerShell
Format-Table -InputObject (Get-Service) -Property Name, DependentServices
```

然而, 有一个重要的区别. 当你用管道将多个对象输送到一个命令时,
PowerShell会将对象 `逐个地` 发送到命令中.
当你使用命令参数时, 这些对象会作为 `单一的数组` 对象被整体发送.
这个微小的差别有很大的影响.

当执行管道时, PowerShell会自动枚举任何实现IEnumerable接口的类型,
并通过管道一个一个地发送成员.
但 `[hashtable]` 是个例外, 它需要调用 `GetEnumerator()` 方法.

在下面的例子中, 数组 和 hashtable 被输送到 Measure-Object cmdlet,
以计算从管道收到的对象的数量.
数组有多个成员, 而hashtable有多个键值对.
只对数组进行 逐个遍历.

```PowerShell
@(1,2,3) | Measure-Object
```

```Output
计数 : 3
...
```

```PowerShell
@{"One"=1;"Two"=2} | Measure-Object

out:
计数 : 1
...
```

同样, 如果你从Get-Process cmdlet向Get-Member cmdlet输送多个进程对象, PowerShell会将每个进程对象, 一次一个, 发送到Get-Member.
Get-Member显示进程对象的.NET类(类型), 以及它们的属性和方法.

```PowerShell
Get-Process | Get-Member

Output:

Name      MemberType     Definition
----      ----------     ----------
Handles   AliasProperty  Handles = Handlecount
...
```

>注意事项
>Get-Member可以消除重复, 所以如果对象都是同一类型, 它只显示一种对象类型.

然而, 如果你使用 `Get-Member` 的InputObject参数,
那么Get-Member会接收一个System.Diagnostics.Process对象的数组作为一个单元.
它显示一个数组对象的属性. (注意System.Object类型名称后面的数组符号(`[]`)).
比如说

```PowerShell
Get-Member -InputObject (Get-Process)

Name               MemberType    Definition
----               ----------    ----------
Count              AliasProperty Count = Length
Address            Method        System.Object& Address(Int32 )
Clone              Method        System.Object Clone()
...
```

这个结果可能不是你想的那样. 但是在你理解之后, 你可以使用它.
例如, 所有数组对象都有一个Count属性.
你可以用它来计算计算机上运行的进程的数量. 比如说.

```PowerShell
(Get-Process).count
```

重要的是要记住, 在管道中发送的对象是 `一次一个` 的.

## 在管道中使用本地命令

PowerShell允许你在管道中包含本地外部命令.
然而, 需要注意的是, PowerShell的管道是面向对象的, 不支持原始字节数据.

从输出原始字节数据的本地程序中输送或重定向输出, 会将输出转换为.NET字符串.
这种转换可能导致原始数据输出的损坏.

作为一种变通方法, 使用 `cmd.exe /c` 或 `sh -c` 调用本地命令,
并使用本地shell提供的 `|` 和 `>` 操作.

## 调查管道错误

当PowerShell 不能将管道对象与接收cmdlet的参数联系起来时, 命令就会失败.

在下面的例子中, 我们试图将一个注册表项从一个注册表键移动到另一个注册表键.
Get-Item cmdlet获得目标路径, 然后将其输送到Move-ItemProperty cmdlet. Move-ItemProperty命令指定了要移动的注册表项的当前路径和名称.

```PowerShell
Get-Item -Path HKLM:\Software\MyCompany\sales | 挪动项.
Move-ItemProperty -Path HKLM:\Software\MyCompany\design -Name product
```

该命令失败了, PowerShell显示以下错误信息.

```output
Move-ItemProperty: 输入对象不能被绑定到命令的任何参数上.
因为该命令不接受管道输入, 或者该输入和它的属性不匹配.
输入和它的属性与任何接受管道输入的参数不一致.
管道输入的参数.
在第1行char:23
+ $a | Move-ItemProperty <<<< -Path HKLM:\Software\MyCompany\design -Name p
```

为了调查, 使用Trace-Command cmdlet来追踪PowerShell的参数绑定组件.
下面的例子是在管道执行时跟踪参数绑定.
PSHost参数在控制台中显示跟踪结果,
FilePath参数将跟踪结果发送到debug.txt文件中供以后参考.

```PowerShell
Trace-Command -Name ParameterBinding -PSHost -FilePath debug.txt -Expression {
  Get-Item -Path HKLM:\Software\MyCompany\sales !
    Move-ItemProperty -Path HKLM:\Software\MyCompany\design -Name product
}
```

追踪的结果很冗长, 但它们显示了被绑定到Get-Item cmdlet的值,
然后是被绑定到Move-ItemProperty cmdlet的命名值.

```output
...
BIND NAMED cmd行args [`Move-ItemProperty`]
BIND arg [HKLM:\Software\MyCompany\design]到参数[Path].
...
BIND arg [product]到参数[Name]
...
BIND POSITIONAL cmd line args [`Move-ItemProperty`] ...
...
```

最后, 它显示将路径与Move-ItemProperty的Destination参数绑定的尝试失败了.

```output
...
绑定PIPELINE对象到参数. [`Move-ItemProperty`]].
PIPELINE对象类型=[Microsoft.Win32.RegistryKey]
重新恢复管道参数的原始值
参数 [目的地] PIPELINE INPUT ValueFromPipelineByPropertyName NO
许可证
参数[凭证] PIPELINE INPUT ValueFromPipelineByPropertyName NO
许可证
...
```

使用 `Get-Help` cmdlet 查看 Destination 参数的属性.

```output
Get-Help Move-ItemProperty -Parameter Destination

-Destination <string>

    Required?                    true
    Position?                    1
    Accept pipeline input?       true (ByPropertyName)
    Parameter set name           (All)
    Aliases                      None
    Dynamic?                     false
    Accept wildcard characters?  false
```

结果显示, Destination只接受管道输入 "通过属性名称".
因此, 管道输入的对象必须有一个名为Destination的属性.

使用Get-Member来查看来自Get-Item的对象的属性.

```PowerShell
Get-Item -Path HKLM:\Software\MyCompany\sales | Get-Member
```

输出显示, 该项目是一个Microsoft.Win32.RegistryKey对象,
没有Destination属性. 这解释了为什么命令失败.

Path参数接受管道输入的名称或值.

```Output
Get-Help Move-ItemProperty -Parameter Path

-Path <String[]>
    Specifies the path to the current location of the property. Wildcard
    characters are permitted.

    Required?                    true
    Position?                    0
    Default value                None
    Accept pipeline input?       True (ByPropertyName, ByValue)
    Accept wildcard characters?  true
为了修复该命令, 我们必须在Move-ItemProperty cmdlet中指定目的地, 并使用Get-Item来获得我们要移动的项目的Path.
```

比如说

```PowerShell
Get-Item -Path HKLM:\Software\MyCompany\design |
Move-ItemProperty -Destination HKLM:\Software\MyCompany\sales -Name product
```

## 内部行延续

正如已经讨论过的, 流水线是由流水线操作符(|)连接的一系列命令, 通常写在一行.
然而, 为了可读性, PowerShell允许你将流水线分成多行.
当管道操作符是该行的最后一个标记时, PowerShell解析器会将下一行与当前命令连接起来, 继续构建管道.
例如, 下面的单行管道.

```PowerShell
Command-1 | Command-2 | Command-3
```

可以写成:

```PowerShell
Command-1 |
  Command-2 |
    Command-3
```

后面几行的前导空格并不重要. 缩进增强了可读性.

PowerShell 7增加了对管道延续的支持, 管道字符在行的开头.
下面的例子显示了你如何使用这个新功能.

```PowerShell
# 在行首用管道符续接(不需要回车键)
Get-Process | Where-Object CPU | Where-Object Path
    | Get-Item | Where-Object FullName -match "AppData"
    | Sort-Object FullName -Unique

# 在一行中用管子包裹自己的内容
Get-Process | Where-Object CPU | Where-Object Path
    |
    Get-Item | Where-Object FullName -match "AppData"
    |
    Sort-Object FullName -Unique
```

>重要提示
>当在 shell 中 交互式 工作时, 只有在使用 `Ctrl+V` 粘贴时,
>才能粘贴 在 `行的开头带有管道` 的代码.
>而 使用 右键单击 进行 粘贴时, 一次插入一行.
>由于该行没有以管道字符结束, PowerShell 认为输入已经完成,
>并按输入的内容执行该行.
