# 系统管理的示例脚本

[系统管理的示例脚本][], 可以快速 get 到 pwsh 的风格.

[系统管理的示例脚本]: https://docs.microsoft.com/zh-cn/powershell/scripting/samples/sample-scripts-for-administration?view=powershell-7.1

## 使用对象

### 查看对象结构 (Get-Member)

若要查看进程对象的所有成员并分页显示输出, 以便于你可以全部查看, 请键入:

```powershell
Get-Process | Get-Member | Out-Host -Paging
```

备注:

+ `MemberType` 的允许值有 `AliasProperty`, `CodeProperty`, `Property`, ..., `MemberSet` 以及 `All`.
一个进程有 `60` 多个属性.  对于任何已知的对象, Windows PowerShell 通常仅显示少许属性, 这是因为显示所有属性会导致产生无法管理的信息量.
+ Windows PowerShell 通过使用存储在以 `.format.ps1xml` 结尾的 `XML` 文件中的信息来决定某种类型的对象的显示方式.
进程对象(即 `.NET System.Diagnostics.Process` 对象)的格式设置数据存储在 `DotNetTypes.format.ps1xml` 中.

### 选择对象部件 (Select-Object)

可以使用 `Select-Object` cmdlet 创建新的自定义 `PowerShell` 对象(可以指定需要属性). 键入下面的命令以创建仅包括 `Win32_LogicalDisk WMI` 类的 `Name` 和 `FreeSpace` 属性的新对象:

```powershell
Get-CimInstance -Class Win32_LogicalDisk | Select-Object -Property Name,FreeSpace
```

可以使用 `Select-Object` 创建计算属性.  这样即可以以十亿字节为单位显示 `FreeSpace`, 而非以字节为单位.

```powershell
Get-CimInstance -Class Win32_LogicalDisk |
    Select-Object -Property Name, @{
        label='FreeSpace';expression={($_.FreeSpace/1GB).ToString('F2')}
    }
```

## 通用语法

[PowerShell 术语表](https://docs.microsoft.com/zh-cn/powershell/scripting/learn/glossary)
[命令行语法关键字](https://docs.microsoft.com/zh-cn/windows-server/administration/windows-commands/command-line-syntax-key)
[Windows 10 and Windows Server 2016 PowerShell Module Reference](https://docs.microsoft.com/en-us/powershell/module/?view=win10-ps)

命令行语法关键字
表示法  说明

+ 不含方括号或大括号的文本  必须按如下所示键入项.
+ `<尖括号中的文字>`  必须为其提供值的占位符.
+ `[方括号中的文字]`  可选项.
+ `{大括号中的文字}`  所需项的集合. 您必须选择一个.
+ 竖线 `(|)`  互斥项的分隔符. 您必须选择一个.
+ 省略 `(... )`  可以重复并多次使用的项.

使用`update-help -UICulture en-us`更新帮助文档, 一般英文文档比较多.

经常使用的参考文档: [Microsoft.PowerShell.Management](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.management/?view=powershell-7)
本节包含与`PowerShell Microsoft.PowerShell.Management`模块一起安装的`cmdlet`的帮助主题. 管理模块包含`cmdlet`,可帮助您在PowerShell中管理`Windows`.

***
查看pwsh 版本

```powershell
$PSVersionTable.PSVersion
```

### 语法表 sytax diagrams

```powershell
<command-name> -<Required Parameter Name> <Required Parameter Value> # 命令名,必选键值对
[-<Optional Parameter Name> <Optional Parameter Value>] # 可选的键值对
[-<Optional Switch Parameters>] # 可选的开关
[-<Optional Parameter Name>] <Required Parameter Value> # 可匿名的键值对
```

***
命令说明中,每一段都代表一种可能的语法格式.

命令采用 `-参数 值`的形式,每个参数前面必须有一个`-`.
`pwsh`是基于`Microsoft .NET`框架的,所以参数值是用它们的`.NET`类型表示的.

***
参数集合(Parameter Set),语法表中的每一段是一种可能的命令使用形式.如果参数不能放在一起使用,它们会出现在不同的参数集合中(不同的段落中).有些参数可能出现在多个 Parameter Sets 中.

通过使用的参数名称,你隐式地指明了你想使用的参数集合.

在每一个参数集合中,参数按照位置顺序出现.如果你省略了参数名称(键值),pwsh将会按照位置和类型对命令参数赋值.

***
语法表中的符号

+ 连字符`-`(hyphen)-表明接着的是一个参数的名字.如:

```powershell
New-Alias -Name
```

+ 尖括号`<>`: 表示一个占位符,需要把其中的数据类型换成具体的用户输入.

+ 中括号`[]`: 表示一个可选项.表示参数键值都可以省略,或者参数值可以省略.如:

```powershell
New-Alias [-Description <string>]
New-Alias [-Name] <string>
```

如果在一个`.NET`类型后面接上一个`[]`,表示这个参数可以接受多个同类型的值,用一个逗号分隔列表

```powershell
New-Alias [-Name] <string> # 接收一个值
Get-Process [-Name] <string[]> # 接受多个值
Get-Process -Name Explorer, Winlogon, Services
```

在语法示例中,`[]`也用于命名和强制转换为`.NET Framework`类型.这种时候,`[]`的意思不是一个元素可以省略.

+ 大括号`{}`:表明一个`枚举`,列出一个参数所有可能的选项,其中的值用`|`分隔,`|`是`exclusive OR`的意思,表示只能有一个.如:

```powershell
New-Alias -Option {None | ReadOnly | Constant | Private | AllScope}
```

### 自动变量

[about_Automatic_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)

+ `$HOME`; 包含用户的主目录的完整路径. 这个变量相当于Windows环境变量`$env:homedrive`,`$env:homepath`, 通常是`C:\Users\<UserName>`.
+ `$Host`; 包含一个对象, 代表`PowerShell`的当前`host`应用. 你可以用这个变量在命令中代表当前的主机, 显示或改变主机的属性.
比如`$Host.version`或`$Host.CurrentCulture`, 或者`$host.ui.rawui.setbackgroundcolor("Red")`.

#### 使用枚举器

`$input`, `$foreach`, 和 `$switch` 变量都是枚举器, 用于遍历包含它们的代码块所处理的值.
枚举器包含属性和方法, 你可以用来推进或重置迭代, 或检索迭代值. 直接操作枚举器并不被认为是最佳做法.

+ 在循环内, 应首选流控制关键字`break`和`continue`.
+ 在接受管道输入的函数中, 最佳做法是给`ValueFromPipeline`或`ValueFromPipelineByPropertyName`属性指定参数.

欲了解更多信息, 请参阅 about_Functions_Advanced_Parameters.

+ `MoveNext`; `MoveNext`方法将枚举器推进到集合的下一个元素. 如果枚举器成功前进, `MoveNext`返回`True`; 如果枚举器已经超过了集合的末端, 则返回`False`.
`MoveNext`返回的布尔值被发送到输出流. 你可以通过将其类型转换为`[void]`或将其输送到`Out-Null`来抑制该输出.

```powershell
$input.MoveNext() | Out-Null
[void]$input.MoveNext()
```

+ `Reset`; `Reset`方法将枚举器设置到它的初始位置, 也就是在集合的第一个元素`之前`.
+ `Current` ; `Current`属性获得集合或者管道中的当前元素, 即枚举器的当前位置.

在调用`MoveNext`之前, `Current`属性会持续返回相同的结果.

## 一些概念

### providers

PowerShell `providers` 是一些特定的`.NET`程序,用来提供对特性`data stores` 的访问,方便查看和管理.
数据出现在一个`driver`里,你可以像是访问硬盘中的文件那样访问它们. 你也可以使用自定义的 `cmdlet`

有时`provider`也会给`built-in cmdlets`提供动态参数.只有`cmdlets`作用在这些`provider`上面时,参数才是可用的.

*****
复制并整理文件的脚本

```powershell
$originpath=Get-Location;

$tepath='C:\Users\Thomas\Desktop\paper.ff\'

if(-not ( Test-Path $tepath )){ mkdir $tepath } else {}
copy-item .\* -Destination $tepath -Force

cd $tepath

remove-item -Path ('.\temp\','.\*.aux','.\*.lof','.\*.log','.\*.lot','.\*.fls','.\*.out','.\*.toc','.\*.fmt','.\*.fot','.\*.cb','.\*.cb2','.\*.ptc','.\*.xdv','.\*.fdb_latexmk','.\*.synctex.gz','.\*.swp','.\*.ps1','.\*.bib','.\*.bbl','.\*.blg')

7z a ..\paper.7z $tepath

cd $originpath

```

### 执行策略限制

`Powershell`一般初始化情况下都会禁止脚本执行.脚本能否执行取决于`Powershell`的执行策略.

```powershell
.\MyScript.ps1
无法加载文件 E:MyScript.ps1,因为在此系统中禁止执行脚本...
```

只有管理员才有权限更改这个策略.非管理员会报错.

```powershell
Get-ExecutionPolicy #查看脚本执行策略, 默认为Restricted
Set-ExecutionPolicy UnRestricted #更改脚本执行策略
```

脚本执行策略类型为: `Microsoft.PowerShell.ExecutionPolicy`
查看所有支持的执行策略:

```powershell
[System.Enum]::GetNames([Microsoft.PowerShell.ExecutionPolicy])
Unrestricted
RemoteSigned
AllSigned
Restricted
Default
Bypass
Undefined
```

+ `Unrestricted` :权限最高,可以不受限制执行任何脚本.
+ `Default` :为`Powershell`默认的策略: `Restricted`,不允许任何脚本执行.
+ `AllSigned` : 所有脚本都必须经过签名才能在运行.
+ `RemoteSigned` : 本地脚本无限制,但是对来自网络的脚本必须经过签名.

关于`Powershell`脚本的签名在后续会谈到.

### 查找别名,alias

当然,如果想查找特定动词/名词的命令也是可以的.比方说,如果我想查找所有以`Get`开头的命令,可以使用下面的命令.

```powershell
Get-Command -Verb Get
```

相应的,如果我想获取所有名词是`Help`的命令,可以使用下面的命令. 也可以用来查找二进制程序, 如`unzip.exe`

```powershell
Get-Command -Noun Help
```

```powershell
Get-Command -CommandType Alias | where {$_.DisplayName -like -join("*", "Get-Command", "*") }
```

+ select -> Select-Object
+ where-> Where-Object
+ gcm -> Get-Command

### 命令行历史

我们通常会在`Console`界面中运行多次命令或者命令行,
在`PowerShell`中我们可以使用管理历史记录的命令来管理那些之前使用过的命令行,目前在`PowerShell`中有如下四个管理历史相关的命令.

| Cmdlet (Alias) ||       Description |
| -------------------|  ---------------------- |
| `Get-History` (`h`)   |   Gets the command history. |
| `Invoke-History` (`r)` |  Runs a command in the command history. |
| `Add-History`      |   Adds a command to the command history. |
| `Clear-History` (`clh`) | Deletes commands from the command history. |

如上面的`Description`介绍所描述的那样, 我们如下使用了`Get-History`命令得到如下从我们打开`PowerShell Console`界面开始记录的第一条命令.

 ```powershell
 C:\Users\Administrator> Get-History
 ```

当然,`PowerShell`并不会无止境的记录历史命令,你可以通过使用如下保留自变量来查看系统默认可以记录多少历史命令:

```powershell
PS C:\Users\Administrator> $MaximumHistoryCount
```

你也可以直接给这个变量赋一个阿拉伯数字设置你想设置的上限值,比如我设置为`5`:

```powershell
PS C:\Users\Administrator> $MaximumHistoryCount = 5
PS C:\Users\Administrator> $MaximumHistoryCount
```

当你在用`Get-History`命令查看记录了多少命令的时候你会发现,它只自动截取了最近的`5`行命令

我们可以使用`Invoke-History`或者别名`r` 来调用历史命令:

```powershell
PS C:\Users\Administrator> `Invoke-History -id 51`
```

好了,大致是这样,非常简单的几个命令,对了你还可以用`Add-History`添加命令或用`Clear-History`来清除之前的命令行.

[itanders-command-history](https://blog.csdn.net/itanders/article/details/51344419)

### 输出到控制台

Module : Microsoft.PowerShell.Utility

`Write-Output`
将指定的对象发送到管道中的下一个命令.
如果该命令是管道中的最后一个命令,则对象将显示在控制台中.

Syntax:

```powershell
Write-Output
     [-InputObject] <PSObject[]>
     [-NoEnumerate]
     [<CommonParameters>]
```

示例: 将输出传递到另一个cmdlet

```powershell
Write-Output "test output" | Get-Member
```

## 日常应用

+ 输出分页: `Get-Help Get-Help | Out-Host -Paging`
+ `Get-ChildItem -Filter`: 指定一个过滤器来限定路径参数. 目前只有`FileSystem`provider 支持. 过滤器字符串被传递给`.NET` API以列举文件, API只支持`*`和`?`通配符.

### 解压缩

`unzip`配合`pwsh`将文件解压到相同名字的附录中.

```powershell
$array = dir; foreach ($i in $array) { unzip $i -d $i.basename  }
```

当然, 也可以使用`pwsh`自带的`Expand-Archive`命令. 默认情况下, `Expand-Archive`在当前位置创建一个与`ZIP`文件同名的文件夹.

```powershell
Expand-Archive -DestinationPath
```

该参数允许你指定一个不同的文件夹的路径. 如果目标文件夹不存在, 则会被创建.

### 查看文件大小并指定单位

[select-object](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.utility/select-object)

这个例子演示了使用`Select-Object`来为你的输入添加计算属性. 向Property参数传递一个`ScriptBlock`, 使`Select-Object`对传递的每个对象计算表达式, 并将结果添加到输出中.
在`ScriptBlock`中, 你可以使用`$_`变量来引用管道中的当前对象.

默认情况下, `Select-Object`将使用`ScriptBlock`字符串作为属性的名称. 使用`Hashtable`, 可以自定义属性的名字, 和计算内容. 可以为每个对象添加多个计算属性.

```powershell
# 创建一个名为 $_.StartTime.DayOfWeek 的计算属性
Get-Process | Select-Object -Property ProcessName,{$_.StartTime.DayOfWeek}
```

下面添加一个自定义属性来计算你传入的每个`FileInfo`对象的大小, 以`Million Bytes`为单位.  使用管道变量, 将每个文件的长度除以`1MB`
你也可以把键的名字缩短为`l`和`e`, 或者用`Name`代替`Label`.

```powershell
$unit="MB";$size = @{label="Size($unit)";expression={$_.length/"1$unit"}}
$days = @{l="Days";e={((Get-Date) - $_.LastAccessTime).Days}}
Get-ChildItem $PSHOME -File | Select-Object Name, $size, $days
```

可以添加到配置文件中:

```powershell
function lsh{
    $unit="MB";
    Get-ChildItem -File $args | Select-Object Name, @{label="Size($unit)";expression={($_.length/"1$unit").ToString('F2')}}, @{l="Days";e={((Get-Date) - $_.LastAccessTime).Days}}
}
```

### 计算文件夹的大小

[技能连载](https://blog.vichamp.com/2017/09/26/calculating-folder-file-size/)
[获取windows子文件夹的大小](https://www.cnblogs.com/20e8/p/9994212.html)
[about_CommonParameters](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_commonparameters)

```powershell
function psdu {
    param ([switch] $total, [switch] $inclueFile, [string] $path = '.', [string] $unit) # 使用 total 开关可以指定求总空间占用
    if ($args) { $path = $args }
    function selectUnit {
        # 根据命令行参数, 以及文件大小选择显示单位
        if ($unit) { $selUnit = $unit }
        elseif ($args -gt '1TB' ) { $selUnit = 'TB' }
        elseif ($args -gt '1GB' ) { $selUnit = 'GB' }
        elseif ($args -gt '1MB' ) { $selUnit = 'MB' }
        elseif ($args -gt '1KB' ) { $selUnit = 'KB' }
        else { $selUnit = 's' }
        return $selUnit
    }
    # 执行文件体积统计任务
    if ($total ) {
        # 如果只统计总量
        $totalSize = (Get-ChildItem -Path $path -Force -Recurse -ErrorAction SilentlyContinue | Measure-Object -Property Length -Sum).Sum
        $selUnit = selectUnit($totalSize)
        write-host -fore green ("{0,8:N2} {1,-2} {2} {3,-20}" -f ($totalSize / "1$selUnit"), $selUnit, ' -- ', ( Resolve-Path $path))
    }
    # 用循环处理子项目, 如果 $inclueFile=True, 除了统计文件夹, 还统计文件
    elseif ($inclueFile) {
        $colItems = (Get-ChildItem $path | Sort-Object)
    }
    else {
        $colItems = (Get-ChildItem $path | Where-Object { $_.PSIsContainer -eq $True } | Sort-Object)
    }
    foreach ($i in $colItems) {
        $subFolderItems = (Get-ChildItem $i.FullName -recurse | Measure-Object -Sum { $_.Length } ) # 计算求和
        $selUnit = selectUnit($subFolderItems.Sum)
        $FileSize = ("{0:N2}" -f ($subFolderItems.Sum / "1$selUnit")) # 格式化字符串
        write-host -fore green ("{0,8:N2} {1,-2} {2} {3,-20}" -f $FileSize, $selUnit, ' -- ', $i.FullName)
    }
}
```

参数说明:

+ `Get-ChildItem -Force`: 允许 `cmdlet` 获得用户一般不能访问的项目, 如隐藏文件或系统文件.
强制参数并不覆盖安全限制. 不同`providers`的实现方式不同. 欲了解更多信息, 请参见 about_Providers.
+ `-Recurse`: 获取指定位置, 位置的所有子项中的项目.
+ `-ErrorAction`: 这是一个通用参数. 确定 `cmdlet` 如何响应命令中的非终止错误.  此参数仅在命令生成非终止错误时有效,  例如来自 `Write-Error` 的错误.
+ `-ErrorAction:SilentlyContinue`: 禁止显示错误消息并继续执行命令.

***

+ `PSIsContainer` 属性用来判断是否为容器, 即文件夹. `True` 表示对象为文件夹.

***

+ `Measure-Object`: 计算对象的数字属性, 以及字符串对象中的字符, 词和行, 如文本文件.
+ `-Sum`: 表示该cmdlet显示指定属性的值的总和. 其他例如:

```powershell
Get-ChildItem | Measure-Object -Property length -Minimum -Maximum -Sum -Average
```

从`PowerShell 6`开始, `Measure-Object`支持`ScriptBlock`属性.
下面的例子演示了如何使用`ScriptBlock`属性来确定一个目录中所有文件的大小, 单位是`MegaBytes`.

```powershell
Get-ChildItem | Measure-Object -Sum {$_.Length/1MB}
```

### about_Numeric_Literals

有两种数字字元: 整数和实数. 两者都可以有类型和乘数的后缀.

[about_Numeric_Literals](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_numeric_literals)

```powershell
PS> 1kb
1024

PS> 1.30Dmb
1363148.80

PS> 0x10Gb
17179869184

PS> 1.4e23tb
1.5393162788864E+35

PS> 0x12Lpb
20266198323167232
```

### Get-ChildItem

默认情况下, `Get-ChildItem` 列出项目的模式(Attributes), `LastWriteTime`, 文件大小(Length)和`Name`. `Mode`属性中的字母含义为:

+ `l` ; 链接, link
+ `d` ; 目录, directory
+ `a` ; 存档, archive
+ `r` ; 只读, read-only
+ `h` ; 隐藏, hidden
+ `s` ; 系统, system

+ `[[-Path] <string[]>]`; 指定一个或多个路径, 如`C:\Test`. 可以使用通配符.  默认是当前目录 `.`.
+ `Name` ; 只返回文件或目录的名称.
+ `Recurse`; 搜索子目录
+ `Force`; 展示隐藏文件, 文件具有模式`h`.

```powershell
Get-ChildItem -Path C:\Test -Name
Get-ChildItem -Path C:\Test\*.txt -Recurse -Force
```

`C:\Test\*.txt` 表示所有后缀名为`.txt`的文件. `*`是通配符, wildcard.

+ 排除指定的文件

```powershell
Get-ChildItem -Path C:\Test\Logs\* -Exclude A*
```

`Get-ChildItem` 使用`Path`参数来指定目录`C:\Test\Logs`. 排除参数使用星号`*`通配符指定任何以`A`或`a`开头的文件或目录被排除在输出之外.
当使用排除参数时, 路径参数中的尾部星号`*`是可选的. 例如, `-Path C:\Test\Logs`或`-Path C:\Test\Logs\*`.

+ 如果在`Path`参数中没有包含尾部的星号`*`, 就会显示`Path`目录的内容. 与排除参数值相符的文件名或子目录名不显示
+ 如果在`Path`参数中包含尾部的星号`*`, 则命令会递归`Path`的子目录. 与排除参数值相符的文件名或子目录名不显示
+ 如果在命令中加入`Recurse`参数, 无论`Path`参数中是否包含尾部星号`*`, 递归输出都是一样的.

### 批量重命名

[Rename-Item](https://docs.microsoft.com/zh-cn/powershell/module/Microsoft.PowerShell.Management/Rename-Item)

例4: 重命名多个文件

这个例子把当前目录下的所有`*.txt`文件重命名为`*.log` :

```powershell
Get-ChildItem *.txt
Get-ChildItem *.txt | Rename-Item -NewName { $_.Name -replace '.txt', '.log' }
```

`Get-ChildItem` 获取当前文件夹中所有以`.txt`文件为扩展名的文件, 然后将它们输送到`Rename-Item`.
`NewName`的值是一个脚本块, 在提交给`NewName`参数的值之前运行.

在脚本块中, `$_`自动变量代表每个文件对象, 因为它通过管道来到命令中.
该脚本块使用`-replace`操作符将每个文件的扩展名替换为`.log`. 注意, 使用`-replace`操作符进行匹配是不分大小写的.

### 查看驱动器

+ `Get-PSDrive` ; 查看所有虚拟驱动器
+ `get-psprovider` ;  查看所有的 `provider`

### 查看子项目

+ 获取文件管理相关命令

```powershell
Get-Command -noun Item
```

+ 复制文件

```powershell
Copy-Item
```

+ 批量重命名文件

```powershell
# 先查看原始文件
dir *.pdf
# 重命名
dir *.pdf | foreach { Rename-Item $_ -NewName ($_.BaseName+"_123.pdf")  }
```

+ 用正则表达式替换

```powershell
"Mr. Miller, Mrs. Meyer and Mr. Werner"-replace "(Mr.|Mrs.)\s*(Miller|Meyer)", "Our client `$2"
# 替换 xx.nb 文件
dir *.nb | foreach { Rename-Item $_ -NewName ($_.Name -replace "rencon2", "rencon3" )  }
```

### 测试变量,Test-Path

验证一个变量是否存在,仍然可以像验证文件系统那样, 使用`cmdlet Test-Path`. 因为变量存在变量驱动器中.

```powershell
# 测试变量
Test-Path variable:value1
# 测试路径
Test-Path -Path "C:\Documents and Settings\DavidC"
# 测试配置文件的路径
Test-Path -Path $profile
# 只检查路径语法是否正确
Test-Path -Path $profile -IsValid
#  除了某种类型之外, 是否还有其他文件
Test-Path -Path "C:\CAD\Commercial Buildings\*" -Exclude *.dwg #False
# 检查文件, 而不是目录
Test-Path -Path $profile -PathType leaf #True
# 测试注册表中的路径
Test-Path -Path "HKLM:\Software\Microsoft\PowerShell\1\ShellIds\Microsoft.PowerShell"
Test-Path -Path "HKLM:\Software\Microsoft\PowerShell\1\ShellIds\Microsoft.PowerShell\ExecutionPolicy"
# 测试文件是否比某个日期新
Test-Path $pshome\pwsh.exe -NewerThan "July 13, 2009"
# 测试空白路径
Test-Path ' ' # 总是 False
Test-Path '' # 总是 False
```

***
参数说明:

+ `-Path`; 指定一个要测试的路径. 允许使用通配符. 如果路径包括空格, 请用引号括起来.
+ `-Exclude`; 检查除指定类型外是否还有文件
+ `-Filter` ; 指定 provider 语法的过滤器. 这个参数的值限制`Path`参数. 过滤器的语法, 包括通配符的使用, 取决于provider.
`-Filter` 比其他参数更高效, 因为provider 在检索对象时已经应用了限制. 而不是检索完所有对象后, 再交给PowerShell过滤.

```powershell
Test-Path (Get-location) -Filter *.bib
```

+ `-IsValid`; 测试路径的语法, 不管路径的元素是否真实存在. 如果路径的语法有效, 则返回`$True`, 如果无效, 则返回`$False`.
+ `-NewerThan`; 指定一个时间为`DateTime`对象.
+ `-OlderThan`; 将时间指定为`DateTime`对象.
+ `-PathType`; 指定路径中末尾元素的类型. 如果元素是指定的类型, 则返回`$True`, 如果不是, 返回`$False`. 这个参数的可接受值是,

+ `Container` ; 一个包含其他元素的元素, 如目录或注册表键.
+ `Leaf` ; 一个不包含其他元素的元素, 比如一个文件.
+ `Any`; 容器或叶子.

### 格式化磁盘

[PowerShell格式化磁盘](https://www.sysgeek.cn/windows-10-format-hard-drive-powershell/)
[如何在Windows 10中更专业地格式化U盘](https://www.sysgeek.cn/format-usb-drives-windows-10/)

#### diskpart格式化U盘

格式化 U 盘或磁盘的另一种方式便是使用 `diskpart` 命令行工具.
`diskpart` 命令可集成于 `Windows PE` 中方便管理员操作, 也可以写到 MDT 的部署脚本当中进行使用;
使用 `diskpart` 格式化的 U 盘还可以直接将 Windows Vista 至 Windows 10 的 ISO 解压上去用于 U 盘启动引导系统, 免去了用其它工具制作 Windows 10 安装 U 盘的麻烦.

1. 使用 `Windows+X` 快捷键打开`命令提示符(管理员)`工具:
1. 执行 `diskpart` 命令进行交互环境:
1. 执行 `list disk` 命令查看当前 PC 连接的磁盘:
1. 使用 `select` 命令选中你的 U 盘, 在我们的演示环境中 U 盘是磁盘 1, 所以使用 `select disk 1` 命令将它选中:
1. 执行 `clean` 命令清空 `U` 盘:
1. 使用如下命令创建一个新的主分区并标记为活动分区:

```diskpart
create partition primary
active
```

之所以标记为活动分区, 是为了方便将 `Windows` 系统 `ISO` 映像放上去当启用 `U` 盘来用.

1. 使用如下命令对 U 盘进行快速格式化:

```diskpart
format fs=ntfs label="卷标" quick
```

1. 如果你要将 `U` 盘格式化为 `FAT` 或 `exFAT` 格式, 只需替换 `fs=` 后面的 `ntfs` 即可, 最后的 `quick` 表示执行快速格式化.
2. 格式化完成之后, 使用 `assign` 命令自动为 U 盘分配一个盘符即可使用了.

#### PowerShell格式化磁盘

[Windows Storage Management-specific cmdlets](https://docs.microsoft.com/en-us/powershell/module/storage/?view=win10-ps)

普通磁盘和动态磁盘: [Basic and Dynamic Disks](https://docs.microsoft.com/en-us/windows/win32/fileio/basic-and-dynamic-disks)

+ 使用 `Windows+X` 快捷键打开`命令提示符(管理员)`工具:
+ 执行 `Get-Disk` 可以查看到连接到当前 `Windows`电脑的所有物理磁盘`和 `U` 盘.
+ 执行如入命令清理驱动器:

```powershell
Get-Disk 4 | Clear-Disk -RemoveData
```

执行上述命令时, 请确保要清理和格式化的磁盘编号填写正确, 否则清除了错误的驱动器会导致数据丢失.

+ 执行如下命令以使用 `NTFS` 文件系统来创建新分区, 并为磁盘分配名称:

```powershell
New-Partition -DiskNumber 4 -UseMaximumSize | Format-Volume -FileSystem NTFS -NewFileSystemLabel Udisk
```

上述命令中, 要格式化的驱动器磁盘号 `-DiskNumber` 为` 4`, 要分配的磁盘名称也就是磁盘卷标 `-NewFileSystemLabel` 为 `Udisk`, 请按你自己的情况更改.

+ 执行如下 PowerShell 命令为格式化好的磁盘分配一个驱动器号:

```powershell
Get-Partition -DiskNumber 4 | Set-Partition -NewDriveLetter G
```

### pwsh 锁屏

[更改计算机状态](https://docs.microsoft.com/zh-cn/powershell/scripting/samples/changing-computer-state)

定义函数:

```powershell
Function Lock-WorkStation {
$signature = @"
[DllImport("user32.dll", SetLastError = true)]
public static extern bool LockWorkStation();
"@

$LockWorkStation = Add-Type -memberDefinition $signature -name "Win32LockWorkStation" -namespace Win32Functions -passthru
$LockWorkStation::LockWorkStation() | Out-Null
}
```

使用标准可用工具直接锁定计算机的唯一方法是调用 `user32.dll` 中的 `LockWorkstation()` 函数:

```powershell
rundll32.exe user32.dll,LockWorkStation
```

你还可以使用具 `shutdown.exe` 工具及其 `logoff` 选项:

```powershell
shutdown.exe -l
```

另一种方法是使用 `WMI` . `Win32_OperatingSystem` 类具有 `Shutdown` 方法. 调用具有 0 标志的方法将启动注销:

```powershell
Get-CimInstance -Classname Win32_OperatingSystem | Invoke-CimMethod -MethodName Shutdown
```

#### powershell重启计算机

关闭和重启计算机通常是相同类型的任务. 关闭计算机的工具通常也可以重启计算机,反之亦然.
你可以从 `tsshutdn.exe /?` 或 `shutdown.exe /?` 获取详细的使用信息. 也可以直接从 `PowerShell` 执行关闭和重启操作.

+ 要关闭计算机,请使用

```powershell
Stop-Computer
```

+ 要重启操作系统,请使用

```powershell
Restart-Computer
```

+ 要强制立即重新启动计算机,请使用 `-Force` 参数.

```powershell
Restart-Computer -Force
```

### 计算Hash,哈希值

[Win10自带PowerShell命令校验Hash值](https://www.windows10.pro/windows-powershell-get-filehash-algorithm-md5-sha1-format-list/)

+ 校验文件`Hash`值的命令格式如下:

```powershell
Get-FileHash 文件路径 -Algorithm hash-type | Format-List
```

如果需要校验的文件路径比较复杂,例如路径中包含空格, 括号等特殊符号,则需要在路径前后加上英文双引号.

pwsh 命令可以校验的`Hash`值类型包括: `SHA1`, `SHA256`, `SHA384`, `SHA512`, `MACTripleDES`, `MD5`, `RIPEMD160`,暂不支持校验`CRC32`值.
如果想要校验它的`SHA1`值,则运行如下命令:

```powershell
Get-FileHash C:\Windows\notepad.exe -Algorithm SHA1| Format-List
```

如果想要校验`SHA256`值,则不需要带`-Algorithm`参数即可,命令如下:

```powershell
Get-FileHash C:\Windows\notepad.exe | Format-List
```

### 暂停脚本Start-Sleep

`Start-Sleep`使`shell`, 脚本, 或运行空间的活动挂起指定的时间.
可以在脚本使用此命令来等待一个操作的结束, 或者在循环中等待一段指定时间后继续迭代.

+ 暂停Windows PowerShell 10秒:

```powershell
Start-Sleep –s 10
```

+ 暂停脚本`10`秒(10,000毫秒)

```powershell
Start-Sleep –m 10000
```

语法

+ `tart-Sleep [-seconds] <int> [<CommonParameters>]`
+ `Start-Sleep -milliseconds <int> [<CommonParameters>]`
+ `-seconds <int>`;指定睡眠源需要睡眠的秒数. 你可以忽略此参数名称(`-Seconds`), 你也可以使用此参数缩写`-s`.
+ `-milliseconds <int>`; 指定睡眠源需要睡眠的毫秒数. 此参数缩写`-m`.
+ `<公共参数>`; 此命令支持公共参数:` -Verbose`, `-Debug`, `-ErrorAction`, `-ErrorVariable`, and `-OutVariable`.
+ 输入类型`Int32`; 如果需要为该命令提供多个参数, 请使用逗号进行分隔. 例如, `<parameter-name> <value1>, <value2>`.
更多信息, 输入, `get-help about_commonparameters`.
+ 你可以使用`Start-Sleep`内建别名`sleep`. 需要更多信息, 查看`About_Alias`.

### 监视文件夹

```powershell
$i=0
while ($i -le 1000)
{
    Clear-Host
    Write-Host "`n`n ++++++++++++ `n`n"
    dir data*
    Start-Sleep -Seconds 10
    $i++
}
```

### 查看所有中文字体

```powershell
fc-list :lang=zh-cn
```

### eval

将命令行作为整体执行

在`Powershell`中,`&`操作符不但可以执行一条单独的命令,还可以执行`命令行`. 最方便的方式就是将你的命令行放在一个语句块中,作为整体.
在之前的文章中说过,调用操作符只能执行一条命令,但是借助语句块(`{}`)的这把利器,可以让调用操作符执行,多条`Powershell`命令,例如:

```powershell
& {$files=ls;Write-Host "文件数: " $files.Count }
文件数:  29
```

***
执行表达式

另外还有一条Powershell cmdlet,`Invoke-Expression`,这条命令的逻辑就是将一条字符串传递给调用操作符.例如:

```powershell
Invoke-Expression 'Get-Process | Where-Object { $_.Name -like "e*"}'
```

***
打开所有pdf

```powershell
& 'C:\Program Files\SumatraPDF\SumatraPDF.exe'  (Get-ChildItem | where Name -Like "*.pdf")
```

### 查看进程占用

[Find and kill processes that lock a file or directory](https://dandraka.com/2019/08/13/find-and-kill-processes-that-lock-a-file-or-directory/)
[Handle ](https://docs.microsoft.com/en-us/sysinternals/downloads/handle)

有没有想过哪个程序打开了一个特定的文件或目录? 现在你可以找到了. `Handle`是一个显示系统中任何进程的`open handles`信息的工具.
你可以用它来查看打开文件的程序, 或者查看一个程序的所有句柄的对象类型和名称.

你也可以在`Sysinternals`获得这个程序的GUI版本, 即[Process Explorer](https://docs.microsoft.com/en-us/sysinternals/downloads/process-explorer).

下载解压之后, 你可以通过输入 `handle` 来运行`Handle`. 你必须有管理权限才能运行`Handle`.

[Handle locked files](https://octopus.com/blog/how-to-handle-locked-files);
当`Handle.exe`第一次运行时, 你会被提示接受最终用户许可协议(End User License Agreement).
你可以在调用`Handle.exe`前运行以下命令, 自动接受许可协议.

```powershell
& reg.exe ADD "HKCU\Software\Sysinternals\Handle" /v EulaAccepted /t REG_DWORD /d 1 /f
```

`Handle`将搜索`open file reference`, 所以如果你不指定任何命令行参数, 它将列出系统中所有引用打开文件的句柄的值和文件的名称.
它的参数可以修改默认行为.

```powershell
usage: handle [[-a] [-u] | [-c <handle> [-l] [-y]] | [-s]] [-p <processname>|<pid>> [name]
```

参数说明

+ `-a` ; Dump所有类型的句柄信息, 而不仅仅是那些指向文件的信息. 其他类型包括端口, 注册表键, `synchronization primitives`, 线程和进程.
+ `-c` ; 关闭指定的句柄(解释为十六进制数字). 你必须通过其`PID`来指定进程. 警告: 关闭句柄可能导致应用程序或系统不稳定.
+ `-l` ;  Dump `pagefile-backed`部分的大小.
+ `-y` ; 不提示关闭句柄的确认.
+ `-s` ; 打印每一种打开的句柄的数量.
+ `-u` ; 在搜索句柄时显示拥有者的用户名.
+ `-p` ; 不检查系统中的所有句柄, 这个参数将Handle的扫描范围缩小到那些以名称以`process`开始的进程.
例如`handle -p exp`将转储所有以 `exp` 开头的进程的开放文件, 其中包括资源管理器.
+ `name` ; 这个参数的存在是为了让你可以指示Handle搜索对具有特定名称的对象的引用.

例如, 如果你想知道哪个进程(如果有的话)打开了 `c:\windows\system32` 打开, 你可以输入.

```powershell
handle windows/system
```

名称匹配是不分大小写的, 指定的片段可以是感兴趣的文件路径的任何部分.

#### 处理程序输出

当不在搜索模式时(通过指定一个名称片段作为参数来启用), `Handle` 将其输出分成若干部分, 用于打印每个进程的句柄信息.
虚线被用作分隔符, 在虚线下面, 你将看到进程名称和它的`进程ID`(PID).
在进程名称下面列出了`句柄值`(十六进制), 句柄所关联的对象的类型, 以及对象的名称(如果它有的话).

当处于搜索模式时, `handle`将`进程名称`和`ID`列在左边, 匹配到的对象名称列在右边.

```powershell
# Source: DotJim blog (http://dandraka.com)
# Jim Andrakakis, August 2019
param([string]$PathToCheck = "c:\temp")
Clear-Host
$ErrorActionPreference = "Stop"
# handle 程序的下载地址
$url = "https://download.sysinternals.com/files/Handle.zip"
# === 从microsoft下载 handle.exe, 存放到临时文件夹, 并解压  ===
$tempDir = [System.IO.Path]::GetTempPath()
$handlePath = [System.IO.Path]::Combine($tempDir, "handle64.exe")
if (-not (Test-Path $handlePath)) {
    $output = [System.IO.Path]::Combine($tempDir, "handle.zip")
    Invoke-WebRequest -Uri $url -OutFile $output
    Expand-Archive -LiteralPath $output -DestinationPath $tempDir
}
# === 运行 handle.exe ===
# see https://octopus.com/blog/how-to-handle-locked-files to see why the reg entry is needed
& reg.exe ADD "HKCU\Software\Sysinternals\Handle" /v EulaAccepted /t REG_DWORD /d 1 /f | Out-Null
$handleOutput = & $handlePath -a $PathToCheck
# === 查找是否需要杀进程  ===
if ($handleOutput -match "no matching handles found") {
    Write-Host "不需要杀进程, 那我走"
    exit
}
# === 从handle.exe 输出中查找进程 pid ===
$pidList = New-Object System.Collections.ArrayList
$lines = $handleOutput | Split-String -RemoveEmptyStrings -separator "`n"
foreach($line in $lines) {
  # handle 输出格式:
  # chrome.exe         pid: 11392  type: File           5BC: C:\Windows\Fonts\timesbd.ttf
  # 使用正则表达式匹配进程名称: (.*)\b(?:.*)(?:pid: )(\d*)
  $matches = $null
  $line -match "(.*)\b(?:.*)(?:pid: )(\d*)" | Out-Null
  if (-not $matches) { continue }
  if ($matches.Count -eq 0) { continue }
  $pidName = $matches[1]
  $pidStr = $matches[2]
  if ($pidList -notcontains $pidStr) {
    Write-Host "将会停止进程: $pidStr $pidName"
    $pidList.Add($pidStr) | Out-Null
  }
}
# === 停止进程 ===
foreach($pidStr in $pidList) {
    $pidInt = [int]::Parse($pidStr)
    Stop-Process -Id $pidInt -Force
    Write-Host "停止进程 $pidInt"
}
Write-Host "完事儿"
```

### 7z 解压缩

批量压缩文件

```powershell
function  to7z {
    $lst = (Get-ChildItem -Path $args)
    foreach ($f in $lst ) {
        7z a -pmua -mx=0 ( $f.BaseName + '.7z' ) $f
    }
}
```

### 打开exe所在的目录

例如`cmd.exe`

```powershell
Start-Process (Split-Path -Parent (gcm cmd).Path)
```
