# learn.powershell.1.md

[收集和分享 Windows PowerShell 相关教程,技术和最新动态](https://www.pstips.net/)
版权归原作者所有

## introduction

与大多数 `Shell`(它们接受和返回文本)不同, 
`Windows PowerShell` 是在 [.NET Framework 公共语言运行时 (CLR) ](https://docs.microsoft.com/zh-cn/dotnet/core/introduction)
和`.NET Framework` 的基础上生成的, 它将接受和返回 `.NET Framework` 对像.  环境中的这一基本更改为 `Windows` 的管理和配置带来了全新的工具和方法. 

`Windows PowerShell` 引入了 `cmdlet`(读作“command-let”)的概念, 它是内置于 Shell 的简单的单一函数命令行工具.  
可以分别使用每个 `cmdlet`, 但只有组合使用这些简单的工具来执行复杂的任务时, 你才会意识到它们的强大功能.  
`Windows PowerShell` 包括一百多个基本核心 `cmdlet`, 你可以编写自己的 `cmdlet` 并与其他用户共享. 

与许多 Shell 类似, `Windows PowerShell` 允许你访问计算机上的文件系统.  
此外, `Windows PowerShell` 提供程序使你能够像访问文件系统一样方便地访问其他数据存储(例如注册表和数字签名证书存储). 

[microsoft-powershell](https://docs.microsoft.com/zh-cn/powershell/scripting/getting-started/getting-started-with-windows-powershell?view=powershell-6)

`PowerShell` 是一个脚本引擎 `dll`, 嵌入到多个主机中.  启动的最常见主机是交互式命令行 `PowerShell.exe` 和交互式脚本环境 `PowerShell_ISE.exe`. 

## 认识Powershell

### 管道和重定向

#### 管道

即把上一条命令的输出作为下一条命令的输入. 

例如通过`ls`获取当前目录的所有文件信息, 
然后通过`Sort -Descending`对文件信息按照`Name`降序排列, 
最后将排序好的文件的`Name`和`Mode`格式化成`Table`输出. 

```powershell
Get-ChildItem | Sort-Object -Descending Name | Format-Table Name,Mode
```

#### 重定向

把命令的输出保存到文件中, `>`为覆盖, `>>`追加. 

```powershell
PS C:\PStest> "Powershell Routing" >test.txt
PS C:\PStest> Get-Content .\test.txt
Powershell Routing
PS C:\PStest> "Powershell Routing" >>test.txt
PS C:\PStest> "Powershell Routing" >>test.txt
PS C:\PStest> "Powershell Routing" >>test.txt
PS C:\PStest> "Powershell Routing" >>test.txt
PS C:\PStest> "Powershell Routing" >>test.txt
PS C:PStest\> Get-Content .\test.txt
Powershell Routing
Powershell Routing
Powershell Routing
Powershell Routing
Powershell Routing
Powershell Routing
PS C:\PStest>
```

## 交互式

### 数学运算

我们可以把`powershell`当成一个计算器. 
像键入命令行那样输入数学表达式, 回车, `powershell`会自动计算并把结果输出. 
常用的加减乘除模 `+,-,*,/,%` 运算和小括号表达式都支持. 

`PowerShell`也能自动识别计算机容量单位, 包括`KB, MB, GB, TB, PB`

### 执行外部命令

`Powershell` 能够像`CMD`一样很好的执行外部命令. 

#### 通过`netstat`查看网络端口状态

#### 通过`IPConfig`查看自己的网络配置

#### `route print`查看路由信息

#### 启动CMD控制台

启动`CMD`控制台键入`cmd`或者`cmd.exe`,退出`cmd`可以通过命令`exit`. 

#### 查找可用的`Cmd`控制台命令

`Cmd.exe` 通过 `/c` 来接收命令参数, 在`Cmd`中`help`可以查看可用的命令, 
所以可以通过`Cmd /c help` 查找可用的Cmd控制台命令

#### 启动外部程序

为什么可以通过`notpad`打开记事本, 不能通过`wordpad`打开写字板？

因为`notepad.exe`位于`C:Windows\system32` 这个目录, 
而这个目录已经默认被包含在`Powershell`的环境变量`$env:Path`中. 

而`wordpad.exe` 所在的`“%ProgramFiles%\Windows NT\Accessories\wordpad.exe“`目录却没有包含, 
可以先进入这个目录, 再运行`wordpad`, 或者将`wordpad`所在的目录加入到环境变量中,   `$env:Path=$env:Path+”%ProgramFiles%\Windows NT\Accessories”`. 

默认键入一个字符串, `powershell`会将它原样输出, 
如果该字符串是一个命令或者启动程序, 在字符串前加`‘&’`可以执行命令, 或者启动程序. 

```powershell
PS C:\PS> "ls"
ls
PS C:\PS> &"ls"

```

#### 命令集 cmdlets

`cmdlets`是`Powershell`的内部命令, `cmdlet`的类型名为`System.Management.Automation.CmdletInfo`, 包含下列属性和方法：

下面是全部的`Cmdlets`命令

每个命令有一个动词和名词组成, 命令的作用一目了然. 

[Powershell 命令集](https://www.pstips.net/powershell-cmdlets.html)

### 别名

`cmdlet`的名称由一个动词和一个名词组成, 其功能对用户来讲一目了然. 
但是对于一个经常使用`powershell`命令的人每天敲那么多命令也很麻烦啊. 
能不能把命令缩短一点呢？于是“**别名**”就应运而生了. `Powershell`内部也实现了很多常用命令的别名. 
例如`Get-ChildItem`, 列出当前的子文件或目录. 它有两个别名：`ls` 和 `dir`, 这两个别名来源于`unix` 的`shell`和`windows`的`cmd`. 

因此别名有两个作用：

1. 继承：继承unix-shell和windows-cmd. 
2. 方便：方便用户使用. 

#### 处理别名

查询别名所指的真实`cmdlet`命令. 

```powershell
PS C:\PS> Get-Alias -name ls
```

一些经常用到的别名为:

```powershell
?? -> Invoke-NullCoalescing
#object相关
? -> Where-Object       where -> Where-Object       % -> ForEach-Object         foreach -> ForEach-Object       
group -> Group-Object       measure -> Measure-Object       select -> Select-Object     sls -> Select-String
# 常用操作
gcm -> Get-Command      gh -> Get-Help      gcs -> Get-PSCallStack      gerr -> Get-Error       gm -> Get-Member        
# location 相关
cd -> Set-Location      chdir -> Set-Location       sl -> Set-Location      Get-ThemesLocation      gl -> Get-Location      
pwd -> Get-Location     pushd -> Push-Location      popd -> Pop-Location
# 输出内容
clc -> Clear-Content        gc -> Get-Content       type -> Get-Content     cls -> Clear-Host   oh -> Out-Host
echo -> Write-Output        edit -> vim epcsv -> Export-Csv
# 历史记录
r -> Invoke-History     clhy -> Clear-History       ghy -> Get-History      ihy -> Invoke-History       h -> Get-History        history -> Get-History
# 排版
fc -> Format-Custom     fhx -> Format-Hex       fl -> Format-List    ft -> Format-Table  fw -> Format-Wide
# 文件管理
gci -> Get-ChildItem        dir -> Get-ChildItem         ll -> Get-ChildItem        
cli -> Clear-Item       copy -> Copy-Item       cpi -> Copy-Item        gi -> Get-Item         del -> Remove-Item       rd -> Remove-Item     
ri -> Remove-Item       rni -> Rename-Item      ren -> Rename-Item      si -> Set-Item      
move -> Move-Item       mi -> Move-Item     erase -> Remove-Item        ii -> Invoke-Item   ni -> New-Item
# 文件属性
gp -> Get-ItemProperty      rnp -> Rename-ItemProperty      gpv -> Get-ItemPropertyValue        mp -> Move-ItemProperty
rp -> Remove-ItemProperty       sp -> Set-ItemProperty      clp -> Clear-ItemProperty
# 模块
gmo -> Get-Module       ipmo -> Import-Module       nmo -> New-Module       rmo -> Remove-Module
# 进程
open -> Start-Process       saps -> Start-Process       spps -> Stop-Process        gps -> Get-Process
# 任务管理
gjb -> Get-Job      sajb -> Start-Job     wjb -> Wait-Job       rcjb -> Receive-Job       rjb -> Remove-Job     spjb -> Stop-Job
# 别名
gal -> Get-Alias        epal -> Export-Alias        ipal -> Import-Alias        nal -> New-Alias        sal -> Set-Alias
# 驱动器
ndr -> New-PSDrive      rdr -> Remove-PSDrive       gdr -> Get-PSDrive
# 调用
iex -> Invoke-Expression    irm -> Invoke-RestMethod        iwr -> Invoke-WebRequest        icm -> Invoke-Command
# 会话
nsn -> New-PSSession    etsn -> Enter-PSSession     exsn -> Exit-PSSession      gsn -> Get-PSSession    
rcsn -> Receive-PSSession   rsn -> Remove-PSSession
# 变量
clv -> Clear-Variable       gv -> Get-Variable      nv -> New-Variable      rv -> Remove-Variable       set -> Set-Variable     sv -> Set-Variable
# 剪贴板
gcb -> Get-Clipboard        scb -> Set-Clipboard
# 路径
cvpa -> Convert-Path        rvpa -> Resolve-Path
#
dbp -> Disable-PSBreakpoint     ebp -> Enable-PSBreakpoint     gbp -> Get-PSBreakpoint      rbp -> Remove-PSBreakpoint      sbp -> Set-PSBreakpoint
# 其他
gtz -> Get-TimeZone        gu -> Get-Unique    ipcsv -> Import-Csv     md -> mkdir  
# oh-my-posh
Set-Prompt      Show-Colors     Show-ThemeColors        Show-ThemeSymbols       Write-ColorPreview
```

#### 查看可用的别名

查看可用的别名, 可以通过” `ls alias:`” 或者 ”`Get-Alias`“
如何查看所有以`Remove`打头的`cmdlet`的命令的别名呢？

```powershell
PS C:\PS> dir alias: | where {$_.Definition.Startswith("Remove")}
```

说明：`dir alias:`获取的是别名的数组, 通过`where`对数组元素进行遍历, `$_`代表当前元素, `alias`的`Definition`为`String`类型, 因为`powershell`支持`.net`, `.net`中的`string`类有一个方法`Startswith`. 通过`where`过滤集合在`powershell`中使用非常广泛. 

有的`cmdlet`命令可能有2-3个别名, 我们可以通过下面的命令查看所有别名和指向`cmdlet`的别名的个数. 

```powersehll
PS C:\PS> ls alias: | Group-Object definition | sort -Descending Count
```

#### 创建自己的别名

给记事本创建一个别名, 并查看该别名；

```powershell
PS C:\PS> Set-Alias -Name Edit -Value notepad
PS C:\PS> Edit
PS C:\PS> $alias:Edit
notepad
```

#### 删除自己的别名

别名不用删除, 自定义的别名在`powershell`退出时会自动清除. 但是请放心, `powershell`内置别名(诸如`ls,dir,fl`等)不会清除. 如果你非得手工删除别名. 请使用

```powershell
PS C:\PS> del alias:Edit
```

#### 保存自己的别名

可以使用Export-Alias将别名导出到文件, 需要时再通过`Import-Alias`导入. 但是导入时可能会有异常, 提示别名已经存在无法导入：

```powershell
PS C:\PS> Import-Alias alias.ps1
Import-Alias : Alias not allowed because an alias with the name 'ac' already exists.
At line:1 char:13
+ Import-Alias <<<<  alias.ps1
    + CategoryInfo          : ResourceExists: (ac:String) [Import-Alias], SessionStateException
    + FullyQualifiedErrorId : AliasAlreadyExists,Microsoft.PowerShell.Commands.ImportAliasCommand
```

这时可以使用`Force`强制导入. 

```powershell
PS C:\PS> Export-Alias alias.ps1
PS C:\PS> Import-Alias -Force alias.ps1
```

### 通过函数扩展别名

在`Powershell`中设置别名的确方便快捷, 但是在设置别名的过程中没有参数的相关信息. 
尽管别名会自动识别参数, 但是如何把经常使用的参数默认设定在别名里面呢？
例如`Test-Connection -Count 2 -ComputerName`, 让 `-Count 2` 固化在别名中. 

这时简单的别名无法完成上述需求, 可以通过函数来完成它, 
并且一旦把函数拉过来, 定义别名会变得更加灵活. 

```powershell
PS C:\PS> function test-conn { Test-Connection  -Count 2 -ComputerName $args}
PS C:\PS> Set-Alias tc test-conn
PS C:\PS> tc localhost

Source        Destination     IPV4Address      IPV6Address                              Bytes    Time(ms)
------        -----------     -----------      -----------                              -----    --------
test-me-01   localhost       127.0.0.1        ::1                                      32       0
test-me-01   localhost       127.0.0.1        ::1                                      32       0
```

有了函数牵线, 别名可以完成更高级更强大的功能, 其中`$args`为参数的占位符. 

### 执行文件和脚本

像运行可执行文件一样, `Powershell`运行文件和脚本, 
也必须使用绝对路径或者相对路径, 或者要运行的文件必须定义在可受信任的环境变量中. 

#### 关于脚本

脚本和批处理都属于伪可执行文件, 它们只是包含了若干命令行解释器能够解释和执行的命令行代码. 

#### 执行批处理文件

批处理是扩展名为”`.bat`”的文本文件, 它可以包含任何`cmd`控制台能够处理的命令. 
当批处理文件被打开, `Cmd`控制台会逐行执行每条命令. 那`Powershell`能够直接执行批处理吗？
将下列命令保存为`ping.bat`

```powershell
@echo off
echo batch File Test
pause
Dir %windir%/system
```

然后执行`ping`
屏幕会打印`ping`命令帮助, 说明调用的`ping` 而不是`ping.bat`. 
改为：

```powershell
PS C:\PS> ./ping
```

这时运行的是批处理. 

通过`进入`控制台输入发现执行的不是命令, 而是直接运行`ping.bat` , 也就是说可以通过`.bat` 覆盖`cmd`命令. 
这种机制很危险, 如果有人侵入电脑, 并将系统内部命令篡改成自己批处理, 那就太悲剧了. 
这种命令与脚本的混淆不会发生在`powershell`中, 因为`powershell`有更安全的机制. 

#### 执行powershell脚本

`Powershell` 拥有自己的脚本, 扩展名为“`.ps1`”

```powershell
PS C:\PS> echo "dir;Get-PSProvider;help dir" >test.ps1
PS C:\PS> Get-Content ./test.ps1
dir;Get-PSProvider;help dir
PS C:\PS> ./test.ps1
```

初次执行脚本时, 可能会碰到一个异常：

```powershell
File ” C:\PS\test.ps1″ cannot be loaded because the
execution of scripts is disabled on this system. Please see
“get-help about_signing” for more details.
At line:1 char:10
+ .test.ps1 <<<<
```

这是`powershell`的默认安全设置禁用了执行脚本, 要启用这个功能需要拥有管理员的权限. 

#### 调用入口的优先级

别名：控制台首先会寻找输入是否为一个别名, 如果是, 执行别名所指的命令. 因此我们可以通过别名覆盖任意`powershell`命令, 因为别名的优先级最高. 

函数：如果没有找到别名, 会继续寻找函数, 函数类似别名, 只不过它包含了更多的`powershell`命令. 
因此可以自定义函数扩充 `cmdlet` 把常用的参数给固化进去. 

命令：如果没有找到函数, 控制台会继续寻找命令, 即 `cmdlet` , `powershell`的内部命令. 

脚本：没有找到命令, 继续寻找扩展名为“`.ps1`”的`Powershell`脚本. 

文件：没有找到脚本, 会继续寻找文件, 如果没有可用的文件, 控制台会抛出异常. 

```powershell
The term 'now' is not recognized as the name of a cmdlet, function, script file, or operable program...
```

## 变量

### 定义变量

变量可以临时保存数据, 因此可以把数据保存在变量中, 以便进一步操作. 

```powershell
#定义变量
$a=10
$b=4
#计算变量
$result=$a*$b
$msg="保存文本"

#输出变量
$result
$msg

40
保存文本
```

`powershell` 不需要显示地去声明, 可以自动创建变量, 只须记住变量的前缀为`$`.
创建好了变量后, 可以通过变量名输出变量, 也可以把变量名存在字符串中. 
但是有个例外---单引号中的字符串不会识别和处理变量名. 

#### 选择变量名

在`powershell`中变量名均是以美元符”`$`”开始, 剩余字符可以是数字, 字母, 下划线的任意字符, 
并且`powershell`变量名大小写不敏感(`$a`和`$A` 是同一个变量). 
某些特殊的字符在`powershell`中有特殊的用途, 一般不推荐使用这些字符作为变量名. 
当然你硬要使用, 请把整个变量名后缀用花括号括起来. 

```powershell
PS C:\test> ${"I"like $}="mossfly"
PS C:\test> ${"I"like $}
mossfly
```

#### 赋值和返回值

赋值操作符为`=`, 几乎可以把任何数据赋值给一个变量, 甚至一条`cmdlet`命令, 
为什么, 因为`Powershell`支持对象, 对象可以包罗万象. 

```powershell
PS C:\test> $item=Get-ChildItem .
PS C:\test> $item

    Directory: C:\test

PS C:\test> $result=3000*(1/12+0.0075)
PS C:\test> $result
272.5
```

#### 给多个变量同时赋值

赋值操作符不仅能给一个变量赋值, 还可以同时给多个变量赋相同的值. 

```powershell
PS C:\test> $a=$b=$c=123
PS C:\test> $a
123
PS C:\test> $b
123
PS C:\test> $c
123
```

#### 交换变量的值

要交换两个变量的值, 传统的程序语言至少需要三步, 并且还需定义一个中间临时变量. 
在powershell中, 交换两个变量的值, 这个功能变得非常简单. 

```powershell
PS C:\test> $value1=10
PS C:\test> $value2=20
PS C:\test> $value1,$value2=$value2,$value1
PS C:\test> $value1
20
PS C:\test> $value2
10
```

#### 查看正在使用的变量

`Powershell`将变量的相关信息的记录存放在名为`variable:`的驱动中. 
如果要查看所有定义的变量, 可以直接遍历`variable:`

```powershell
PS C:\test> ls variable:
```

#### 查找变量

因为有虚拟驱动`variable:`的存在, 可以像查找文件那样使用通配符查找变量. 
例如要查询以`value`打头的变量名. 

```powershell
PS C:\test> ls variable:value*
```

#### 验证变量是否存在

验证一个变量是否存在, 仍然可以像验证文件系统那样, 使用cmdlet `Test-Path`. 
为什么？因为变量存在变量驱动器中. 

```powershell
PS C:\test> Test-Path variable:value1
```

#### 删除变量

因为变量会在`powershell`退出或关闭时, 自动清除. 
一般没必要删除, 但是你非得删除, 也可以像删除文件那样删除它. 

```powershell
PS C:\test> Test-Path variable:value1
True
PS C:\test> del variable:value1
PS C:\test> Test-Path variable:value1
False
```

#### 使用专用的变量命令

为了管理变量, `powershell`提供了五个专门管理变量的命令
`Clear-Variable`, `Get-Variable`, `New-Variable`, `Remove-Variable`, `Set-Variable`. 
因为虚拟驱动器`variable:`的存在, `clear, remove, set`打头的命令可以被代替. 
但是`Get-Variable, New-Variable`,却非常有用. 
`new-variable`可以在定义变量时, 指定变量的一些其它属性, 比如访问权限. 
同样`Get-Variable`也可以获取这些附加信息. 

#### 变量写保护

可以使用`New-Variable` 的`option`选项 在创建变量时, 给变量加上只读属性, 
这样就不能给变量重新赋值了. 

```powershell
PS C:\test> New-Variable num -Value 100 -Force -Option readonly
PS C:\test> $num=101
Cannot overwrite variable num because it is read-only or constant.
```

但是可以通过删除变量, 再重新创建变量更新变量内容. 

```powershell
PS C:\test> del Variable:num -Force
PS C:\test> $num=101
PS C:\test> $num
101
```

有没有权限更高的变量, 有, 那就是：选项`Constant`, 常量一旦声明, 不可修改

```powershell
PS C:\test> new-variable num -Value "strong" -Option constant

PS C:\test> $num="why? can not delete it."
Cannot overwrite variable num because it is read-only or constant.
```

#### 变量描述

在 `New-Variable`可以通过 `-description` 添加变量描述, 
但是变量描述默认不会显示, 可以通过`Format-List` 查看. 

```powershell
PS C:\test> new-variable name -Value "me" -Description "This is my name"
PS C:\test> ls Variable:name | fl *
```

### 自动化变量

`Powershell` 自动化变量 是那些一旦打开`Powershell`就会自动加载的变量. 这些变量一般存放的内容包括

+ 用户信息：例如用户的根目录`$home`
+ 配置信息:例如`powershell`控制台的大小, 颜色, 背景等. 
+ 运行时信息：例如一个函数由谁调用, 一个脚本运行的目录等. 

```powershell
PS> $HOME
C:\Users\test
PS> $currentProcessID=$pid
PS> $currentProcessID
5356
PS> Get-Process -Id $pid
```

`powershell`中的某些自动化变量只能读, 不能写. 例如:`$Pid`. 
可以通过`Get-Help about_Automatic_variables`查看`Automatic_variables`的帮助. 

[detail on automatic](https://www.pstips.net/powershell-automatic-variables.html)

### 环境变量

传统的控制台一般没有象`Powershell`这么高级的变量系统. 
它们都是依赖于机器本身的环境变量, 进行操作. 
环境变量对于`powershell`显得很重要, 因为它涵盖了许多操作系统的细节信息. 
此外, `powershell`中的变量只存在于`powershell`内部的会话中, 一旦`powershell`关闭, 这些变量就会自生自灭. 
但是如果环境变量被更新了, 它会继续保存在操作系统中, 即使其它程序也可以调用它. 

#### 读取特殊的环境变量

通过环境变量读取`Windows`操作系统的安装路径, 和默认应用程序的安装路径. 

```powershell
PS> $env:windir
C:\Windows
PS> $env:ProgramFiles
C:\Program Files
```

通过`$env:`, 这就提示`powershell`忽略基本的`variable:`驱动器, 而是去环境变量`env:`驱动器中寻找变量. 
为了和其它变量保持一致, `powershell`环境变量也可以象其它变量那样使用. 
比如你可以把它插入到文本中. 

```powershell
PS> "My computer name $env:COMPUTERNAME"
My computer name MYHome-test-01
```

#### 查找环境变量

`Powershell`把所有环境变量的记录保存在`env:` 虚拟驱动中, 因此可以列出所有环境变量 . 
一旦查出环境变量的名字就可以使用`$env:name` 访问了. 

```powershell
PS> ls env:
```

#### 创建新的环境变量

创建新环境变量的方法和创建其它变量一样, 只需要指定`env:`虚拟驱动器即可

```powershell
PS> $env:TestVar1="This is my environment variable"
PS> $env:TestVar2="Hollow, environment variable"
PS> ls env:Test*
```

#### 删除和更新环境变量

在`powershell`删除和更新环境变量和常规变量一样. 例如要删除环境变量中的 `windir`, 

```powershell
PS> del env:windir
PS> $env:windir
```

可以更新环境变量$env:OS 为linux redhat. 

```powershell
PS> $env:OS
Windows_NT
PS>  $env:OS="Redhat Linux"
PS> $env:OS
Redhat Linux
```

这样直接操作环境变量, 会不会不安全？
事实上很安全, 因为`$env：`中的环境变量只是机器环境变量的一个副本, 
即使你更改了它, 下一次重新打开时, 又会恢复如初. (.NET方法更新环境变量除外)

我们可以将受信任的文件夹列表追加到环境变量的末尾, 
这样就可以直接通过相对路径执行这些文件下的文件或者脚本, 甚至省略扩展名都可以. 

```powershell
PS> md .myscript

    Directory:

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----        2011/11/29     18:20            myscript

PS> cd .myscript
PSmyscript> "write-host 'Hollow , Powershell'" > hollow.ps1
PSmyscript> .hollow.ps1
Hollow , Powershell
PSmyscript> cd ..
PS> $env:Path+=";C:PowerShell\myscript"
PS> hollow.ps1
Hollow , Powershell
PS> hollow
Hollow , Powershell
```

#### 环境变量更新生效

上述对于环境变量的操作只会影响当前`powershell`会话, 并没有更新在机器上. 
`.NET`方法`[environment]::SetEnvironmentvariable`操作可以立刻生效. 
下面的例子对当前用户设置环境变量, 经测试, 重新打开powershell仍然存在

```powershell
PS> [environment]::SetEnvironmentvariable("Path", ";c:\powershellscript", "User")
PS> [environment]::GetEnvironmentvariable("Path", "User")
;c:\powershellscript
```

### 驱动器变量

`Powershell`中所有不是我们自己的定义的变量都属于驱动器变量(比如环境变量), 
它的前缀只是提供给我们一个可以访问信息的虚拟驱动器. 
例如`env:windir`, 像`env：`驱动器上的一个”`文件`”, 我们通过`$`访问它, 就会返回”`文件`”的内容. 

#### 直接访问文件路径

通过驱动器直接访问文件路径, 也支持物理驱动器, 必须把文件路径放在封闭的大括号中, 
因为正常的文件路径包含两个特殊字符“`:`”和“`/`”, 有可能会被`powershell`解释器误解. 

```powershell
PS> ${c:/powershell/ping.bat}
@echo off
echo batch File Test
pause
Dir %windir%/system

PS> ${c:autoexec.bat}
REM Dummy file for NTVDM
```

上述的例子有一个限制, 就是`${$env:HOMEDRIVE/Powershellping.bat}`不能识别, 
原因是`$`后花括号中的路径必须是具体的路径, 而不能带返回值. 
解决方法：

```powershell
PS> Invoke-Expression "`${$env:HOMEDRIVE/Powershell/ping.bat}"
@echo off
echo batch File Test
pause
Dir %windir%/system
```

因为反引号”`` ` ``”放在`$`前, 会把`$`解析成普通字符, 解释器会继续去解析第二个`$`, 
发现`env:HOMEDRIVE`, 将其替换成`c`, 
到此 `Invoke-Expression`的参数就变成了`${C:/Powershell/ping.bat}`,
继续执行这个表达式就可以了. 
查看`Powershell`支持的驱动器, 可以使用`Get-PSDrive`查看. 

`PSDrive`中的大多都支持直接路径访问, 例如可以通过函数路径, 访问一个函数的具体实现. 

```powershell
PS> function hellow(){ Write-Host "Hellow,Powershell" }
PS> $function:hellow
param()
Write-Host "Hellow,Powershell"
```

#### 特殊的变量：子表达式

由 `$+圆括号+表达式` 构成的变量属于`子表达式变量`, 这样的变量会先计算表达式, 然后把表达式的值返回. 
例如 变量`$(3+6)`, 可以简写成`(3+6)`,甚至可以简写成`3+6`.
子表达式变量也可以嵌套在文本中, 例如`result=$(3+6)`. 在处理对象的属性时, 会大量的用到表达式变量. 例如：

```powershell
PS> $file=ls Powershell_Cmdlets.html
PS> $file.Length
735892
PS> "The size of Powershell_Cmdlets.html is $($file.Length)"
The size of Powershell_Cmdlets.html is 735892
```

其实上面的代码可以简化为：

```powershell
PS> "The size of Powershell_Cmdlets.html is $($(ls Powershell_Cmdlets.html).Length)"
The size of Powershell_Cmdlets.html is 735892
```

### 变量的作用域

`Powershell`所有的变量都有一个作用域,决定变量是否可用. 
`Powershell`支持四个作用域：全局, 当前, 私有和脚本. 
有了这些作用域就可以限制变量的可见性了, 尤其是在函数和脚本中. 

如果我们对变量不做特别的声明, `Powershell`解释器会自动处理和限制变量的作用域. 
将下面的内容命令保存着至`test1.ps1`

```powershell
$windows = $env:windir
“Windows Folder: $windows”
```

然后在控制台给变量`$windows`赋值, 并调用Test.ps1脚本. 

```powershell
PS> $windows="Hellow"
PS> .\test.ps1
Windows Folder: C:\Windows
PS> $windows
Hellow
```

调用脚本时, 会分配一个变量`$windows`, 在脚本调用结束后, 这个变量被回收, 
脚本中的变量不会影响脚本外的变量, 因为它们在不同的作用域中. `powershell`会针对每个函数和脚本给它们分配不同的作用域. 

#### 更改变量的可见性

你可以很容易的看到没有`Powershell`解释器自动限制可见性时会发生什么状况, 同样是刚才的脚本, 刚才的命令, 只是在运行脚本时多加上一个点”`.`” 和一个`空格`：

```powershell
PS> $windows="Hellow"
PS> . .\test.ps1
Windows Folder: C:\Windows
PS> $windows
C:Windows
```

在运行脚本时使用一个`圆点`和`空格`, `Powershell`解释器就不会为脚本本身创建自己的变量作用域, 
它会**共享当前控制台的作用域**, 这种**不太灵活但却简单**的方法, **使用时一定要格外小心**. 

#### 加强变量可见性限制的优点：清空初始化环境

可以假设一个场景, 如果你在当前控制台不小心定义了一个只读的常量, 这个常量既不能更新也不能删除, 很是麻烦. 
但是如果你在脚本中操作这个变量就不成问题, 因为脚本有自己的作用域. 
例如, 将下面文本保存为`test.ps1`, 并调用没有任何问题：

```powershell
New-Variable a -value 1 -option Constant
"Value: $a"

PS> .\test.ps1
Value: 1
```

但是如果你通过圆点禁用作用域限制, 调用`test.ps1`,就会有异常, 因为一个常量不能被创建两次. 

```powershell
PS> . .\test.ps1
Value: 1
PS> . .\test.ps1
New-Variable : A variable with name 'a' already exists.
```

所以这种变量的作用域限制可以把变量的冲突降到最小. 

#### 设置单个变量的作用域

到目前为止, 看到的变量作用域的改变都是全局的, 能不能针对某个具体变量的作用域做一些个性化的设置. 

+ `$global` 全局变量, 在所有的作用域中有效, 如果你在脚本或者函数中设置了全局变量, 即使脚本和函数都运行结束, 这个变量也任然有效. 
  
+ `$script` 脚本变量, 只会在脚本内部有效, 包括脚本中的函数, 一旦脚本运行结束, 这个变量就会被回收. 
  
+ `$private` 私有变量, 只会在当前作用域有效, 不能贯穿到其他作用域. 
  
+ `$local` 默认变量, 可以省略修饰符, 在当前作用域有效, 其它作用域只对它有只读权限. 

打开`Powershell`控制台后, `Powershell`会自动生成一个新的**全局作用域**. 
如果增加了函数和脚本, 或者特殊的定义, 才会生成其它作用域. 
在当前控制台, 只存在一个作用域, 通过修饰符访问, 其实访问的是同一个变量：

```powershell
PS> $logo="www.pstips.net"
PS> $logo
www.pstips.net
PS> $private:logo
www.pstips.net
PS> $script:logo
www.pstips.net
PS> $private:logo
www.pstips.net
PS> $global:logo
www.pstips.net
```

当调用一个已定义的函数, `Powershell`会生成第二个作用域, 
它可以对**调用者的作用域**(即`$global`)中的变量执行读操作, 但是不能执行写操作. 

``` powershell
PS> function f(){ "var=$var";$var="function inner";$var }
PS> $var="I am in console."
PS> $var
I am in console.
PS> f
var=I am in console.
function inner
PS> $var
I am in console.
```

怎样把当前控制台中的变量保护起来, 不让它在函数和脚本中被访问, `Private`修饰符就派上了用场. 

```powershell
PS> function f(){ "var=$var";$var="function inner";$var }
PS> $private:var="i am a private variable in console,other scope can not access me."
PS> f
var=
function inner
PS> $private:var
i am a private variable in console,other scope can not access me.
```

对于`$private`限制的变量能不能在函数中通过`$global`修改呢？
不但不能修改, 还会删除当前的`$private`变量. **此处存疑**

```powershell
PS> Function f(){ "var=$var";$global:var=" Try to change variable in function"}
PS> $private:var="I am a private variable"
PS> $private:var
I am a private variable
PS> $var
I am a private variable
PS> f
var=
PS> $private:var
PS> $var
PS>
PS> $private:var -eq $null
True
```

#### local变量

但是`$local`修饰的变量则可以通过`$global`在函数内部更改

```powershell
PS> Function f(){ "var=$var";$global:var=" Try to change variable in function"}
PS> $var="I am a local variable."
PS> $var
I am a local variable.
PS> $private:var
I am a local variable.
PS> f
var=I am a local variable.
PS> $var
 Try to change variable in function
PS> $local:var
 Try to change variable in function
```

### 变量的类型和强类型

变量可以自动存储任何`Powershell`能够识别的类型信息, 
可以通过`$variable`的`GetType().Name`查看和验证`Powershell`分配给变量的数据类型. 

```powershell
PS> (10).gettype().name
Int32
PS> (9999999999999999).gettype().name
Int64
PS> (3.14).gettype().name
Double
PS> (3.14d).gettype().name
Decimal
PS> ("WWW.MOSSFLY.COM").gettype().name
String
PS> (Get-Date).gettype().name
DateTime
```

`Powershell`会给数据分配一个最佳的数据类型；
如果一个整数超出了32位整数的上限(`[int32]::MaxValue`),它就会分配一个64位整数的数据类型；
如果碰到小数, 会分配一个`Double`类型；
如果是文本, `Powershell`会分配一个`String`类型；
如果是日期或者时间, 会被存储为一个`Datetime`对象. 

这种类型自适应也称作“弱类型”,虽然使用起来方便, 但是也会有一些限制, 甚至危险. 
如果`powershell`选择了一个错误的类型赋给变量, 可能会引发一些奇怪的现象. 
例如有一个变量要存储的是即将拷贝文件的个数, 可是在赋值时赋了一个字符串, `Powershell`不会去做过多的判断, 它会更新这个变量的类型, 并且存储新的数据. 
所以一般专业的程序员或者脚本开发者更喜欢使用“**强类型**”, 哪怕在赋值时类型不兼容的报错, 他们也乐意接受. 

喜欢使用强类型的另一个原因是：每一个数据类型都有属于自己的函数. 
例如`DateTime`,和`XML`, 尽管这两种类型都可以用纯文本表示, 
但是使用强类型`[DateTime]`和`[XML]`,对于数据操作起来更方便, 这两个类型的方法可是很丰富噢！

#### 指定类型定义变量

定义变量时可以在变量前的中括号中加入数据类型. 
例如定义一个`Byte`类型的变量, 因为`Byte`的定义域为`[0,255]`, 一旦尝试使用一个不在定义域中的值赋给该变量就会显示一条错误信息. 

```powershell
PS> [byte]$b=101
PS> $b
101
PS> $b=255
PS> $b
255
PS> $b.gettype()

IsPublic IsSerial Name                                     BaseType
-------- -------- ----                                     --------
True     True     Byte                                     System.ValueType

PS> $b=256

Cannot convert value "256" to type "System.Byte". Error: "Value was either too large or too small for an unsigned byte.
"
```

#### 使用固定类型的优点

手动地定义类型的一个重要原因是每个特殊的数据类型都有自己的特殊命令和特殊方法. 
比如把一个日期字符串赋给一个变量, `Powershell`不会自动把这个字符串转换成日期对象赋给一个变量, 因为`Powershell`毕竟是机器, 没有人那么智能. 
当你在赋值时指定`DateTime`类型时, 你会发现几乎所有的`.Net` 中`DateTime`类型的方法在这里都得到支持. 

```powershell
PS> [DateTime]$date="2012-12-20 12:45:00"
PS> $date

2012年12月20日 12:45:00

PS> $date.DayOfWeek
Thursday
PS> $date.DayOfYear
355
PS> $date.AddDays(-10)

2012年12月10日 12:45:00
```

`Powershell`处理`Xml`文档也很方便. 

例如有如下`LogoTest.xml`

```xml
<logotest>
  <extensions>
    <e>.exe</e>
    <e>.dll</e>
  </extensions>
  <files>
    <f></f>
  </files>
  <dirs></dirs>
</logotest>
```

查询`.exe` 和 `.dll`结点

```powershell
PS> [ XML ]$xml=(Get-Content .LogoTestConfig.xml)
PS> $xml.LogoTest.Extensions.E
.exe
.dll
```

`Powershell` 默认支持的`.NET`类型如下. 

`[array]`,`[bool]`,`[byte]`,

`[char]`,`[datetime]`,`[decimal]`,

`[double]`,`[guid]`,`[hashtable]`,

`[int16]`,`[int32]`,`[int]`,`[int64]`,

`[long]`,`[nullable]`,`[psobject]`,`[regex]`,

`[sbyte]`.`[scriptblock]`,`[single]`,`[float]`,

`[string]`,`[switch]`,`[timespan]`,`[type]`,

`[uint16]`,`[uint32]`,`[uint64]`,

`[ XML ]`

### 强类型数组

`Powershell`数组一般具有多态性, 如果你不指定元素的具体类型, 解释器会自动选择合适的类型存储每个元素. 
如果要统一限制所有元素的类型, 可是使用**类型名和一对方括号**作为数组变量的类型. 
这样每当赋值时, 会自动类型检查. 如果目标数据类型不能转换成功, 就会抛出一个异常. 

```powershell
PS C:Powershell> [int[]] $nums=@()
PS C:Powershell> $nums+=2012
PS C:Powershell> $nums+=12.3
PS C:Powershell> $nums+="999"
PS C:Powershell> $nums+="can not convert"
Cannot convert value "can not convert" to type "System.Int32". Error: "Input string was not in a correct format."
```

### 变量的幕后管理

在`Powershell`中创建一个变量, 会在后台生成一个`PSVariable`对象, 
这个对象不仅包含变量的值, 也包含变量的其它信息, 例如”**只写保护**”这样的描述. 
如果在`Powershell`中输出一个变量, 只会输出这个变量的值. 
不能够显示它的其它信息, 如果想查看一个变量的其它保留信息, 就需要变量的基类`PSVariable`对象.

这个可以通过`Get-Variable`命令得到, 下面的例子演示如何查看一个变量的全部信息. 

```powershell
PS> $a=get-date
PS> Get-Variable a
```

#### 修改变量的选项设置

`Powershell`处理一个变量的`PSVariable`对象, 主要是为了能够更新变量的选项设置. 
既可以使用命令`Set-Variable`, 也可以在获取`PSvariable`对象后直接更改. 

比如更改一个变量的描述：

```powershell
PS> $str="我是一个变量"
PS> $var=Get-Variable str
PS> $var

Name                           Value
----                           -----
str                            我是一个变量

PS> $var | fl *

Name        : str
Description :
...

PS> $var.Description="我知道你是一个变量"
PS> $var | fl *
Name        : str
Description : 我知道你是一个变量
...
```

如果你不想多加一个临时变量`$var`来存储`PSVariable`,可以使用`Powershell`子表达式

```powershell
PS> (Get-Variable str).Description="变量的描述已更改;"
PS> Get-Variable str | Format-Table Name,Description

Name                                                        Description
----                                                        -----------
str                                                         变量的描述已更改;
```

#### 激活变量的写保护

可以操作一个变量的选项设置 , 比如给一个变量加上写保护, 需要将`Option`设置为“`ReadOnly`”

```powershell
PS> $var="mossfly"
PS> Set-Variable var -Option "ReadOnly"
PS> (Get-Variable var).Options
ReadOnly
PS> Set-Variable var -Option "None" -Force
PS> (Get-Variable var).Options
None
```

#### 变量的选项

变量的选项是一个枚举值, 包含:

(枚举值: 通过预定义列出所有值的标识符来定义一个有序集合, 这些值的次序和枚举类型说明中的标识符的次序是一致的. )

+ “`None`”:默认设置
+ “`ReadOnly`”：变量只读, 但是可以通过-Force 选项更新. 
+ “`Constant`”：常量一旦声明, 在当前控制台不能更新. 
+ “`Private`”:只在当前作用域可见, 不能贯穿到其它作用域
+ “`AllScope`”：全局, 可以贯穿于任何作用域

#### 变量的类型规范

每个变量的都有自己的类型, 
这个具体的类型存放在`PsVariable`对象的
`Attributes[System.Management.Automation.PSVariableAttributeCollection]`属性, 
如果这个`Attributes`为空, 可以给这个变量存放任何类型的数据, `Powershell`会自己选择合适的类型. 
一旦这个`Attributes`属性确定下来, 就不能随意存放数据了. 

例如给`$var`存放一个整数, 属于弱类型, 所以`Attributes`属性为空, 这时还可以给它赋值一个字符串. 
但是如果给`$var`增加强类型, 存放一个整数, 再给它赋值一个其它类型, 解释器会自动尝试转换, 如果不能转换就会抛出异常. 
这时如果你非得更新`$var`变量的类型, 可以使用`(Get-Variable var).Attributes.Clear()`,清空`Attributes`, 这样强类型就又转换成弱类型了. 

***
conclusion:

+ `$var.GetType().FullName`
+ `(Get-Variable var).Attributes`
+ `(Get-Variable var).Attributes.Clear()`

***

```powershell
PS> $var=123
PS> (Get-Variable var).Attributes
PS> $var.GetType().FullName
System.Int32
PS> $var="字符串"
PS>  (Get-Variable var).Attributes
PS>  $var.GetType().FullName
System.String
PS> [int]$var=123
PS> (Get-Variable var).Attributes

TypeId
------
System.Management.Automation.ArgumentTypeConverterAttribute

PS> $var.GetType().FullName
System.Int32
PS> $var="2012"
PS> $var
2012
PS> $var.GetType().FullName
System.Int32
PS> $var="2012世界末日"
Cannot convert value "2012世界末日" to type "System.Int32". Error: "Input string was not in a correct format."

PS> (Get-Variable var).Attributes.Clear()
PS> (Get-Variable var).Attributes
PS> $var="2012世界末日"
PS> $var.GetType().FullName
System.String
```

#### 验证和检查变量的内容

变量`PSVariable`对象的`Attributes`属性能够存储一些附件条件, 例如限制变量的长度, 
这样在变量重新赋值时就会进行验证, 下面演示如何限制一个字符串变量的长度为位于`2-5`之间. 

```powershell
PS> $var="限制变量"
PS> $condition= New-Object System.Management.Automation.ValidateLengthAttribute -ArgumentList 2,5
PS> (Get-Variable var).Attributes.Add($condition)
PS> $var="限制"
PS> $var="射雕英雄传"
PS> $var="看射雕英雄传"
The variable cannot be validated because the value 看射雕英雄传 is not a valid value for the var variable.
```

常用的变量内容验证还有`5`种, 分别为：

+ `ValidateNotNullAttribute`：限制变量不能为空
+ `ValidateNotNullOrEmptyAttribute`：限制变量不等为空, 不能为空字符串, 不能为空集合
+ `ValidatePatternAttribute`:限制变量要满足制定的正则表达式
+ `ValidateRangeAttribute`：限制变量的取值范围
+ `ValidateSetAttribute`：限制变量的取值集合

#### ValidateNotNullAttribute 例子

```powershell
PS> $a=123
PS> $con=New-Object System.Management.Automation.ValidateNotNullAttribute
PS> (Get-Variable a).Attributes.Add($con)
PS> $a=8964
PS> $a=$null
无法验证此变量, 因为值  不是变量 a 的有效值. 
```

#### ValidateNotNullOrEmptyAttribute 例子

注意`@()`为一个空数组. 

```powershell
PS> $con=New-Object System.Management.Automation.ValidateNotNullOrEmptyAttribute
PS> (Get-Variable a).Attributes.clear()
PS> (Get-Variable a).Attributes.add($con)
PS> $a=$null
The variable cannot be validated because the value  is not a valid value for the a variable.
```

`ValidatePatternAttribute` 例子

验证`Email`格式

```powershell
PS> $email="test@mossfly.com"
PS> $con=New-Object System.Management.Automation.ValidatePatternAttribute "\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b"
PS> (Get-Variable email).Attributes.Add($con)
PS> $email="abc@abc.com"
PS> $email="abc@mossfly.com"
PS> $email="author@gmail.com"
PS> $email="www@mossfly"
The variable cannot be validated because the value www@mossfly is not a valid value for the email variable.
```

## 管道

### 使用管道

管道并不是什么新事物, 以前的`Cmd`控制台也有重定向的命令, 
例如`Dir | More`可以将结果分屏显示. 
传统的`Cmd`管道是基于文本的, 但是`Powershell`是基于对象. 

```powershell
PS> ls | Sort-Object -Descending Name | Select-Object Name,Length,LastWriteTime | ConvertTo-Html | Out-File ls.html

PS> Get-Content .\ls.html
```

首先列出当前目录下的目录和文件, 
然后根据文件名降序排列, 再投影文件名, 文件大小, 文件的修改时间, 转换成`Html`格式, 输出到当前目录的`ls.html`

#### 查看文件夹的owner属性

```powershell
PS D:\> (Get-Item . | Get-Acl).Owner
NT AUTHORITY\SYSTEM
```

#### 面向对象的管道

上面的例子属于面向对象的管道, 每个命令的末尾可以使用新的命令对上个命令的结果做进一步处理, 除非管道是以输出命令结束的. 
就像`Sort-Object`一样, 对文件的列表进行排序, 需要告诉它排序的关键字, 按照升序还是降序. 
`ls`的返回值为一个数组, 数组中的每一个元素都是一个对象, 对象的每一个属性都可以作为`Sort-Object`的排序关键字. 
但是排序时必须指定一个具体的关键字, 因为`Powershell`所传递的对象可能有很多属性. 
不像普通的文本, 对象的信息都是结构化的, 因此也使得`Powershell`的管道变得更加强大和方便. 

#### 转换命令执行的结果为文本

在执行`Powershell`命令时, 解释器会默认在命令的结尾追加一个管道命令, `Out-Default`, 
这样可以将原来的对象结果以文本的形式显示在控制台上, 但是并没有将结果进行转换, 所以可以继续使用其它管道面向对象的结果进行操作, 
但是一旦使用了诸如`ConvertTo-Html`这样的命令后, 就会将结果转换成固定格式的纯文本. 

常用的对管道结果进一步处理的命令有：

+ `Compare-Object`: 比较两组对象. 
+ `ConvertTo-Html`: 将 Microsoft .NET Framework 对象转换为可在 Web 浏览器中显示的 HTML. 
+ `Export-Clixml`: 创建对象的基于 XML 的表示形式并将其存储在文件中. 
+ `Export-Csv`: 将 Microsoft .NET Framework 对象转换为一系列以逗号分隔的, 长度可变的 (CS) 字符+ 串, 并将这些字符串保存到一个 CSV 文件中. 
+ `ForEach-Object`: 针对每一组输入对象执行操作. 
+ `Format-List`: 将输出的格式设置为属性列表, 其中每个属性均各占一行显示. 
+ `Format-Table`: 将输出的格式设置为表. 
+ `Format-Wide`: 将对象的格式设置为只能显示每个对象的一个属性的宽表. 
+ `Get-Unique`: 从排序列表返回唯一项目. 
+ `Group-Object`: 指定的属性包含相同值的组对象. 
+ `Import-Clixml`: 导入 CLIXML 文件, 并在 Windows PowerShell 中创建相应的对象. 
+ `Measure-Object`: 计算对象的数字属性以及字符串对象(如文本文件)中的字符数, 单词数和行数. 
+ `more`: 对结果分屏显示. 
+ `Out-File`: 将输出发送到文件. 
+ `Out-Null`: 删除输出, 不将其发送到控制台. 
+ `Out-Printer`: 将输出发送到打印机. 
+ `Out-String`: 将对象作为一列字符串发送到主机. 
+ `Select-Object`: 选择一个对象或一组对象的指定属性. 它还可以从对象的数组中选择唯一对象, 也可从对象数组的开头或末尾选择指定个数的对象. 
+ `Sort-Object`: 按属性值对象进行排序. 
+ `Tee-Object`: 将命令输出保存在文件或变量中, 并将其显示在控制台中. 
+ `Where-Object`: 创建控制哪些对象沿着命令管道传递的筛选器. 

#### 管道的处理模式

当我们把许多命名组合成一个管道时, 可能会感兴趣每一个命令的执行时是顺序执行还是同时执行？
通过管道处理结果实际上是实时的. 这就是为什么存在两个管道模式：

**顺序模式**(较慢)：在顺序模式中管道中同一时间只执行一条命令, 只有当前一条命令的所有执行完毕, 才会把所有结果交付给下一条命令. 
这种模式速度慢并且耗内存, 因为必须需要很多次分配空间存储中间结果. 

**流模式**(较快)：流模式会立即执行所有命令, 同一时间可能在执行多条命令. 
前一条命令可能会产生多个结果, 但是一旦产生其中一个结果, 就会立即交付给下一条命令处理. 
这样的流模式节省比较节省内存, 可能管道的某个任务还在执行, 但是已经有部分结果输出了. 
减少了中间结果的保存. 

#### 管道命令的阻塞

可以使用`Sort-Object`对管道的结果进行排序, 
但是有时候排序可能导致整个操作系统阻塞, 因为排序命令的的执行属于顺序模式, 必须得上一条命令的结果全部完成, 才能排序. 

因此在使用这类命令时, 要注意操作对象的大小, 和它们需要的内存. 例如这条命令：

```powershell
Dir C: -recurse | Sort-Object
```

`-recurse` 选项是递归查询子目录, 可想而知系统盘的文件和目录有多大. 
这条命令一旦运行起来, 需要等很长很长的时间, 甚至可能导致系统崩溃, 得重启电脑. 
你可以在执行这条命令时, 打开任务管理器查看`Powershell`进程的内存占用在以每秒种几十兆的速率增加. 

到底哪些命令可能系统阻塞, 要视命令的实现方式以及处理的对象大小决定. 

例如`Sort-object`导致阻塞的原因肯定是由于技术实现上采用的是内排序, 没有使用外排序. 
但是像`Out-Host -paging` 这样的命令属于流出来模式, 就一般不会导致系统阻塞. 

### 对象转换成文本

怎样将`Powershell`的对象结果转换成文本并显示在控制台上. 
`Powershell`已经内置`Out-Default`命令追加在管道的命令串的末尾. 因此你使用`dir` 和d`ir | out-default`的结果是相同的. 

`Out-Default`可以将对象转换成可视的文本. 事实上`Out-Default`会首先调用`Format-Table`, 将更多的属性默认隐藏. 
再调用`Out-Host`将结果输出在控制台上. 因此下面的三组命令执行结果是相同的. 

```powershell
ls
ls | Format-Table | Out-Host
ls | Out-Default
```

#### 显示隐藏的对象属性

要查看对象结果的所有属性, 可是使用

```powershell
ls | Format-Table *
```

这样因为属性和属性的内容太多可能不会显示完全, 可以使用文本换行参数

```powershell
ls | Format-Table * -Wrap
```

#### 格式化管道结果

首先可是使用下面的命令查看所有以`Format`打头的命令

```powershell
PS C:Powershell> Get-Command -Verb format

CommandType     Name                            Definition
-----------     ----                            ----------
Cmdlet          Format-Custom                   Format-Custom [[-Property]
Cmdlet          Format-List                     Format-List [[-Property]
Cmdlet          Format-Table                    Format-Table [[-Property]
Cmdlet          Format-Wide                     Format-Wide [[-Property]
```

+ `Format-Custom`: 使用自定义视图来设置输出的格式. 
+ `Format-List`: 将输出的格式设置为属性列表, 其中每个属性均各占一行显示. 
+ `Format-Table`: 将输出的格式设置为表. 
+ `Format-Wide`: 将对象的格式设置为只能显示每个对象的一个属性的宽表. 

#### 显示指定的属性

要显示指定的属性, 你首先得知道结果对象中的属性名, 例如:

```powershell
PS C:Powershell> ls | Format-Table Name,Length,LastWriteTime

Name                       Length                     LastWriteTime
----                       ------                     -------------
ABC                                                   2011/11/23 17:25:53
myscript                                              2011/11/29 18:21:28
a.html                     67580                      2011/11/24 18:30:13
a.txt                      26384                      2011/11/24 20:04:31
alias                      12060                      2011/11/24 20:26:36
```

#### 使用通配符

例如要查看当前以`i`打头的进程, 并显示进程的名字和其它以”`pe`”打头, 以”`64`″结尾的属性. 

```powershell
PS C:Powershell> Get-Process i* | Format-Table Name,pe*64

Name                PeakPagedMemorySize    PeakWorkingSet64 PeakVirtualMemorySi
                                     64                                    ze64
----                -------------------   ----------------------------------
Idle                                  0                   0                   0
IMECFMUI                      946176                4292608             48054272
IMECMNT                       1564672              5320704             65482752
IMEDICTUPDATE             1224704              4579328             31965184
```

#### 脚本块作为属性

在`Powershell`中文件的`Length`默认以`byte`作为单位如果你想让它输出时以`KB`显示, 
可以考虑下面的方法. 

```powershell
PS C:Powershell> ls | Format-Table Name,{ [int]($_.Length/1kb) }

Name                        [int]($_.Length/1kb)
----                       ----------------------
function.ps1                          21
LogoTestConfig.xml                     0
ls.html                                3
name.html                              7
```

#### 修改列标题

使用合成的属性, 如果使用脚本块作为标题, 看着很不爽. 
可以使用Lable设置. 同样是上面的例子, 稍作修改. 

```powershell
PS C:Powershell> $column = @{Expression={ [int]($_.Length/1KB) }; Label="KB" }
PS C:Powershell> Dir | Format-Table Name, $column

Name                                     KB
----                       ----------------------
function.ps1                             21
LogoTestConfig.xml                        0
ls.html                                   3
name.html
```

注：
`PowerShell`将任何以逗号分隔的列表视为一个数组：

```powershell
"server1","server2"
```

所以在这些情况下`@`是可选的. 但是, 对于关联数组, `@`是必需的：

```powershell
@{"Key"="Value";"Key2"="Value2"}
```

所以, `@`是“数组运算符”. 

#### 优化列宽度

因为`Powershell`的绝大多数输出都是实时的流模式, 所以下一条结果的宽度未知, 
`Powershell`的结果会默认采用分散对齐, 这样可以最大限度利用控制台的宽度, 
但是可以通过`-auto`参数对列的宽带进行优化, 会将属性值的最大宽带作为每一列的宽度, 对比一下吧：

```powershell
PS C:Powershell> ls

    目录: C:Powershell

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----        2011/11/23     17:25            ABC
d----        2011/11/29     18:21            myscript
-a---        2011/11/24     18:30      67580 a.html
-a---        2011/11/24     20:04      26384 a.txt
PS C:Powershell> ls | Format-Table -AutoSize

    目录: C:Powershell

Mode         LastWriteTime Length Name
----         ------------- ------ ----
d---- 2011/11/23     17:25        ABC
d---- 2011/11/29     18:21        myscript
-a--- 2011/11/24     18:30  67580 a.html
```

### 排序和分组管道结果

使用`Sort-Object`和`Group-Object`可以对管道结果进行分组. 
其实每条命令执行后的结果已经排过序了. 
例如通过`ls`查看文件列表, 默认会根据`Name`属性进行排序, 但是你可以通过指定属性进行排序例如：

```powershell
PS C:Powershell> ls | Sort-Object Length
Mode         LastWriteTime Length Name
----         ------------- ------ ----
-a--- 2011/11/28     15:30     63 ping.bat
-a---  2011/12/2     18:47    140 test.ps1
```

这样默认会根据`length`进行升序排序, 如果要降序排列, 可使用`Descending`选项. 

```powershell
PS C:Powershell> ls | Sort-Object Length -Descending
Mode         LastWriteTime Length Name
----         ------------- ------ ----
-a--- 2011/11/24     17:44 735892 Powershell_Cmdlets.html
-a--- 2011/11/24     18:30  67580 a.html
```

#### 给对象和哈希表进行排序

如果要完成主要关键字降序, 次要关键字升序的排序, 可能首先想到的是：

```powershell
PS C:Powershell> Dir | Sort-Object Length, Name -descending, -ascending
Sort-Object : 找不到接受实际参数“System.Object[]”的位置形式参数. 
```

但是上面的方法行不通, 可以这样操作：

```powershell
PS C:Powershell> Dir | Sort-Object @{expression="Length";Descending=$true},@{expression="Name";Ascending=$true}

    目录: C:Powershell

Mode         LastWriteTime Length Name
----         ------------- ------ ----
-a--- 2011/11/24     17:44 735892 Powershell_Cmdlets.html
-a--- 2011/11/24     18:30  67580 a.html
-a--- 2011/11/24     20:04  26384 a.txt
```

#### 对数据进行分组

如果想查看当前关闭和开启的所有服务, 并且通过状态进行分组. 可使用：

```powershell
PS C:Powershell> Get-Service | Group-Object Status
Count Name    Group
----- ----    -----
   87 Running {System.ServiceProcess.ServiceController, System.ServiceProcess.S
              erviceController, System.ServiceProcess.ServiceController, System
              .ServiceProcess.ServiceController...}
```

再举一例, 把当前目录的文件以扩展名进行分组. 

```powershell
PS C:Powershell> ls | Group-Object Extension
Count Name  Group
----- ----  -----
    2       {ABC, alias}
    5 .html {a.html, ls.html, name.html, Powershell_Cmdlets.html...}
    2 .txt  {a.txt, test.txt}
```

#### 使用表达式分组

如果要查看当前目录的文件, 根据文件的大小是否大于`1kb`分组. 

```powershell
PS C:Powershell> ls | Group-Object {$_.Length -gt 1kb}

Count Name                      Group
----- ----                      -----
    7 False                     {ABC, employee.xml, LogoTestConfig.xml, ping...
    8 True                      {a.html, a.txt, alias, function.ps1...}
```

#### 如果按照文件名的首字母分组

```powershell
PS C:Powershell> ls | Group-Object {$_.name.SubString(0,1).toUpper()}
Count Name Group
----- ---- -----
    3 A    {a.html, a.txt, alias}
    1 E    {employee.xml}
...
```

#### 根据当前应用程序的发布者分组

PS C:Powershell> Get-Process | Group-Object Company -NoElement

```powershell
Count Name
----- ----
    2 Adobe Systems Incorpor...
   52
    2 微软
    ...
```

#### 使用格式化命令分组

`Group-Object`并不是唯一可以完成分组功能的命令, 
事实上格式化命令例如`Format-Object`支持一个`GroupBy`的参数, 也可以完成分组. 

```powershell
PS C:Powershell> Dir | Sort-Object Extension, Name | Format-Table -groupBy Extension

    目录: C:Powershell

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
-a---        2011/11/24     20:26      12060 alias

    目录: C:Powershell

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
-a---        2011/11/28     15:30         63 ping.bat
```

### 过滤管道结果

通过管道可以过滤某些对象和对象的属性, 这个功能很实用, 因为很多时候我们并不是对所有的结果感兴趣, 可能只会对某些结果感兴趣. 
如果要过滤对象可以使用`Where-Object`；
如果要过滤对象的属性, 可以使用`Select-Object`；
如果要自定义个性化的过滤效果可以使用`ForEach-Object`. 
最后如果想过滤重复的结果, 可以使用`Get-Uinque`. 

#### 筛选管道结果中的对象

如果你只对管道结果的特定对象感兴趣, 可以使用`Where-Object`对每个结果进行严格筛选, 
一旦满足你的标准才会保留, 不满足标准的就会自动丢弃. 

例如你通过`Get-service`查看运行在机器上的当前服务. 

但是可能只关心那些正在运行的服务, 这时你就可以通过每个服务的属性`Status`进行过滤. 
但是前提条件是你得事先知道待处理的对象拥有哪些属性. 
你可以通过`Format-List *`, 也可以通过`Get-memeber`. 

```powershell
PS C:Powershell> Get-service | Select-Object -First 1 | Format-List *
out:
Name                : AdobeARMservice
RequiredServices    : {}
CanPauseAndContinue : False
...

PS C:Powershell> Get-service | Select-Object -First 1 | Get-Member -MemberType Property

   TypeName: System.ServiceProcess.ServiceController

Name                MemberType Definition
----                ---------- ----------
CanPauseAndContinue Property   System.Boolean CanPauseAndContinue {get;}
CanShutdown               Property   System.Boolean CanShutdown {get;}
...
```

知道了对象有哪些属性, 要完成上面提到的需求就很容易了. 

```powershell
PS C:Powershell> get-service | Where-Object {$_.Status -eq "Running"}

Status   Name               DisplayName
------   ----               -----------
Running  AdobeARMservice    Adobe Acrobat Update Service
Running  AppHostSvc             Application Host Helper Service
```

这里稍微解释一下, `Where-Object`的参数的是一个**布尔表达式**, 
`$_`代表过滤过程中经过管道的当前结果. 
另外`Where-Object`还有一个别名 “`?`” 更形象. 

#### 选择对象的属性

包含在每一个对象中的属性可能有很多, 但是并不是所有的属性你都感兴趣, 
这时可以使用`Select-Object` 限制对象的属性. 
接下来的例子演示如果获取机器上匿名帐号的完整信息. 

```powershell
PS C:Usersv-bali.FAREAST> Get-WmiObject Win32_UserAccount -filter "LocalAccount=True AND Name='guest'"

AccountType  : 512
Caption          : myhomeguest
Domain          : myhome
SID                : S-1-5-21-3064017030-3269374297-2491181182-501
FullName       :
Name        : guest
```

如果你只对用户名, 描述, 启用感兴趣. 

```powershell
PS C:Powershell> Get-WmiObject Win32_UserAccount -filter "LocalAccount=True AND
 Name='guest'" | Select-Object Name,Description,Disabled

Name                       Description                                 Disabled
----                           -----------                                 --------
guest                      Built-in account for gu...                      True
```

#### Select-Object也支持通配符

```powershell
Dir | Select-Object * -exclude *A*
```

#### 限制对象的数量

列出最后修改的5个文件

```powershell
PS C:Powershell> Dir | Select-Object -ExcludeProperty "*N*" -First 5

    目录: C:Powershell

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
-a---        2011/11/24     18:30      67580 a.html
-a---        2011/11/24     20:04      26384 a.txt
-a---        2011/11/24     20:26      12060 alias
-a---        2011/11/25     11:20        556 employee.xml
-a---        2011/11/29     19:23      21466 function.ps1
```

列出占用CPU最大的5个进程

```powershell
PS C:Powershell> get-process | sort -Descending cpu | select -First 5

Handles NPM(K)  PM(K)  WS(K) VM(M) CPU(s)   Id ProcessName
------- ------  -----  ----- ----- ------   -- -----------
   1336     98 844304 809388  1081 164.69 3060 iexplore
    224     10  74676  62468   188  81.10 4460 AcroRd32
    130      9  28264  39092   167  70.57 3436 dwm
    169      8   7576  29568   134  65.22 3364 notepad
    989     34  72484  35996   393  62.67 4724 BingDict
```

#### 逐个处理所有管道结果

如果想对管道结果进行逐个个性化处理可以使用`ForEach-Object`

```powershell
PS C:Powershell> ls | ForEach-Object {"文件名：{0} 文件大小{1}KB: " -f $_.Name,
($_.length/1kb).tostring()}
文件名：a.html 文件大小65.99609375KB:
文件名：a.txt 文件大小25.765625KB:
```

#### 删除重复对象

`Get-Unique`可以从已排序的对象列表中删除重复对象. 
`Get-Unique`会逐个遍历对象, 每次遍历时都会与前一个对象进行比较, 如果和前一个对象相等就会抛弃当前对象, 否则就保留. 
所以如果对象列表中没有排序, `Get-Unique`不能完全发挥作用, 只能保证相邻对象不重复. 

```Powershell
S C:Powershell> 1,2,1,2 | Get-Unique
1
2
1
2
PS C:Powershell> 1,2,1,2 | Sort-Object |Get-Unique
1
2
PS C:Powershell> ls | foreach{$_.extension} | Sort-Object |Get-Unique

.bat
.html
.ps1
.txt
.vbs
.xml
```

### 分析和比较管道结果

使用`Measure-Object`和`Compare-Object`可以统计和对比管道结果. `Measure-Object`允许指定待统计对象的属性. `Compare-Object`可以对比对象前后的快照. 

#### 统计和计算

使用`Measure-Object`可以对对象的属性求最小值, 最大值, 平均值, 和. 
例如要查看当前目录文件占用空间的情况. 

```powershell
PS C:Powershell> ls | measure length
Count    : 19
Average  :
Sum      :
Maximum  :
Minimum  :
Property : length

PS C:Powershell> ls | measure length -Average -Sum -Maximum -Minimum
Count    : 19
Average  : 53768.8421052632
Sum      : 1021608
Maximum  : 735892
Minimum  : 0
Property : length
```

使用`Measure-Object`还可以统计文本文件中的字符数, 单词数, 行数
例如我们可以把下面的文本保存到：`word.txt`. 

```powershell
Retirement Anxiety Spreads Among the One Percent
Report: Green Monday a Boon for Online Shopping
5 Lesser-Known Ways to Boost Your Credit Score

PS C:Powershell> Get-Content .\word.txt | measure -Line -Word -Character
Lines Words Characters Property
----- ----- ---------- --------
    3    23        141
```

#### 比较对象

有时需要比较前后两个时间段开启了那些进程, 服务状态有什么变化. 
类似这样的工作可以交给`Compare-Object`. 

#### 比较不同的时间段

可以先将所有开启的进程信息快照保存到一个变量中, 过一段时间, 再保存一份新的进程快照, 然后就可以通过`Compare-Object`进行对比了. 

```powershell
PS C:Powershell> $before=Get-Process
PS C:Powershell> $after=get-process
PS C:Powershell> Compare-Object $before $after

InputObject                             SideIndicator
-----------                             -------------
System.Diagnostics.Process (notepad)    =>
System.Diagnostics.Process (notepad)    =>
System.Diagnostics.Process (AcroRd32)
```

`$before` 是一个数组存储了当前所有的`Process`对象, 
`Compare-Object`的结果有两个列：`InputObject`为前后不一致的对象, 
`SideIndicator`为不一致状态, `=>`表示新增的对象, 
结合上面的例子分析：在`before`和`after`的时间段有`3`个进程
(`AcroRd32, AcroRd32, prevhost`)关闭了, 有2个进程开启了(`notepad, notepad`). 

#### 检查对象的变化

`Compare-Object`并不仅仅能比较对象组中的是否新增和减少了对象, 
它还可以比较每个对象的属性变化, 因为它有一个参数`-property`. 

```powershell
PS C:PowerShell> Get-Service wsearch

Status   Name               DisplayName
------   ----               -----------
Running  wsearch            Windows Search

PS C:PowerShell> $svc1=Get-Service wsearch
PS C:PowerShell> $svc1.stop()
PS C:PowerShell> $svc2=Get-Service wsearch
PS C:PowerShell> Compare-Object $svc1 $svc2 -Property Status,Name

                    Status Name                       SideIndicator
                    ------ ----                       -------------
              StartPending wsearch                    =>
                   Running wsearch
```

#### 比较文件的内容

对于文本文件可以通过`Get-Content`进行读取, 并且将文件以行为单位保存为一个数组. 
这时依然可以通过`Compare-Object`进行比较. 

下面的例子创建两个不同的文本文件, 然后通过`Compare-Object`比较两个文件的`Get-Content`结果. 

```powershell
PS C:PowerShell> "Hellow
>> Power
>> Shell" >a.txt
>>
PS C:PowerShell> "Hollow
>> Shell
>> Linux" >b.txt
>>
PS C:PowerShell> Compare-Object (Get-Content .a.txt) (Get-Content .b.txt)
InputObject SideIndicator
----------- -------------
Hollow      =>
Linux         =>
Hellow
```

#### 保存快照以便后期使用

上面的例子都是把对象保存在变量中, 
变量有一个缺点就是一旦`Powershell`退出或者电脑关闭变量都会消失. 所以最好的方法就是把对象保存到磁盘文件中. 
怎样把对象序列化成一个文件, `Powershell`提供了一条命令：`Export-Clixml`, 可以完成此工作, 还有一条反序列化的命令`Import-Clixml`. 
这样可以使`Compare-object`的命令更方便. 
例如一个月前保存一个`$before`对象, 一个月后比较都可以. 

```powershell
PS C:PowerShell> Get-Process  | Export-Clixml before.xml
PS C:PowerShell> $before=Import-Clixml .before.xml
PS C:PowerShell> $after=Get-Process
PS C:PowerShell> Compare-Object -ReferenceObject $before -DifferenceObject $after
```

### 导出管道结果

可以将管道的结果转换成文本输出, 默认是`Out-Default`. 
可以通过`Get-Command -verb out`查看`Powershell`都有哪些输出的命令. 

```powershell
PS C:PowerShell> get-command -Verb out

CommandType Name         Definition
----------- ----         ----------
Cmdlet      Out-Default  Out-Default [-InputObject ]
Cmdlet      Out-File     Out-File [-FilePath]  [[-Encoding]
Cmdlet      Out-GridView Out-GridView [-InputObject ]
Cmdlet      Out-Host     Out-Host [-Paging] [-InputObject ]
Cmdlet      Out-Null     Out-Null [-InputObject ] [-Verbose]
Cmdlet      Out-Printer  Out-Printer [[-Name] ] [-InputObject
Cmdlet      Out-String   Out-String [-Stream] [-Width ]
```

+ `Out-Default` 将输出发送到默���的格式化程序和默认的输出`cmdlet`. 
+ `Out-File` 将输出发送到文件. 
+ `Out-GridView` 将输出发送到单独窗口中的交互表. 
+ `Out-Host` 将输出发送到命令行. 
+ `Out-Null` 删除输出, 不将其发送到控制台. 
+ `Out-Printer` 将输出发送到打印机. 
+ `Out-String` 将对象作为一列字符串发送到主机. 

#### 吸收输出结果

有的命令无论执行成功或失败都会有输出, 有时不需要这些输出时可以使用 `| Out-Null`, 这条命令的作用和 `>$null` 一样. 
尤其在函数中使用比较多, 因为如果没有特别指明`return` . 
`Powershell`函数会把输出结果作为函数的返回值. 
为了避免这种麻烦, 通常在管道后加一条命令`Out-Null` 或 `>$null`吸收输出结果. 

```powershell
PS C:PowerShell> md ABC

    目录: C:PowerShell

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----        2011/12/19     17:05            ABC

PS C:PowerShell> md ABD >$null
PS C:PowerShell> md ABE | Out-Null
```

#### 修改管道格式

之前讨论过, `Powershell`默认会在每行命令的末尾追加一条`Out-Default`, 
`Out-Default`默认包含了一条`Out-Host`, 那是不是`Out-Host`就是英雄无用武之地了. 
事实上, 可以通过`Out-Host`控制管道的版式. 
`Powershell`不但会自动把管道结果发送到输出设备上, 而且还会把管道结果转换成可读的文本. 
这个自动转换与`Format-Table`有点像. 但是完全依靠自动转换有时会碰到很奇怪的输出结果. 

例如当单独使用`Get-Service`时, 结果会以表格的形式输出, 
但是使用`pwd;Get-Service`时`Service`信息以列表形式输出. 

```powershell
PS C:PowerShell> Get-Service

Status   Name               DisplayName
------   ----               -----------
Running  AdobeARMservice    Adobe Acrobat Update Service
Stopped  AeLookupSvc        Application Experience
Stopped  ALG                Application Layer Gateway Service

PS C:PowerShell> pwd;Get-Service

Path
----
C:PowerShell

Status      : Stopped
Name        : THREADORDER
DisplayName : Thread Ordering Server
...
```

第二行使用了两条命令, 通过分号间隔. 
但是为什么`Service`信息会以列表显示呢？
因为经过`Powershell`的解释器处理, 上面例子中的第二条命令会变成：

```powershell
& { pwd;Get-Service} | Out-Default
```

`Powershell`在命令中没有发现特别指定的版式信息, 
就会尝试从第一条命令的第一个结果对象中寻找线索, 并且把这种版式强加给紧接着的其它命令. 

怎样避免上述问题, 最好的办法就是要明确指定. 

```powershell
pwd;Get-Service | Out-Host
```

#### 强制以文本显示

`Powershell`的文本转换一般发生在管道的末尾, 但是如果需要对文本处理可以强制转换成文本. 

```powershell
PS C:PowerShell> ls . -Recurse | Out-String

 目录: C:PowerShell

Mode         LastWriteTime   Length Name
----         -------------   ------ ----
d---- 2011/12/19     17:05          ABC
d---- 2011/12/19     17:06          ABD
...

PS C:PowerShell> (ls | Out-String -Stream).gettype()
IsPublic IsSerial Name     BaseType
-------- -------- ----     --------
True     True     Object[] System.Array
```

#### Excel导出对象

管道结果导出为文本文件看起来不规整, 阅读起来也不方便. 
所以最好导出为`Excel`格式“`csv`”, 这样的文件默认支持`Microsft Excel`程序打开, 并处理. 

```powershell
PS C:PowerShell> Get-Service | Export-Csv a.csv
PS C:PowerShell> .\a.csv
```

使用这些导出命令时, 切忌不要在管道中使用`Format-Table`, 否则导出的结果不正常, 自己可以测试. 那怎样选择属性呢？可使用`Select-Object`. 

#### Html导出对象

`Html`导出对象和`Excel`导出大同小异. 

```powershell
PS C:PowerShell> Get-Service | ConvertTo-Html -Title "ls result" | Out-File a.html
PS C:PowerShell> .a.html
```

### 扩展类型系统

`Powershell`一个最吸引人的功能是它能够将任何对象转换成文本, 
我们已经使用过将对象属性以不同的版式转换成文本, 并且输出. 
更令人惊奇的是`Powershell`会把最重要最能代表这个对象本质的信息输出. 
一个对象有很多属性, 为什么它单单就输出那几个属性呢？
如果使用：

```powershell
Dir | Format-Table * -wrap
PSP PSP PSC PSD PSP PSI Bas Mod Nam Par Exi Roo
ath are hil riv rov sCo eNa e   e   ent sts t
    ntP dNa e   ide nta me
    ath me      r   iner
--- --- --- --- --- --- --- --- --- --- --- ---
Mic Mic ABC C   Mic Tru ABC d-- ABC Pow Tru C:
ros ros         ros   e     --      ers   e
oft oft         oft                 hel
.Po .Po         .Po                 l
wer wer         wer
She She         She
ll. ll.         ll.
Cor Cor         Cor
```

`Powershell`会最大限度的输出每个属性, 但是这样的输出基本上没有意义, 不利于用户阅读. 
那到底是什么让`Powershell`默认只显示此属性不显示彼属性呢？
是“扩展类型系统”`Extended Type System (ETS)`, 
`ETS`会对管道中对象转换成文本的机制进行宏观调控. 
`ETS`由两部分组成, 一部分控制对象的版式, 一部分控制对象的属性, 今天主要关心第一部分. 

文本转换不可逆

在管道中将对象结果转换成文本后, 不能再将文本转换成对象, 因为`ETS`不能处理文本. 
如果通过`ConvertTo-String`将目录列表的转换成`String`后, 
使用`Format-Table`和`Format-List`这些命令就会无效. 

```powershell
PS C:Powershell> $text= dir | Out-String
PS C:Powershell> $text

    目录: C:Powershell

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----        2011/12/19     17:05            ABC
d----        2011/12/19     17:06            ABD
d----        2011/12/19     17:06            ABE

PS C:Powershell> $text | Format-Table

    目录: C:Powershell

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----        2011/12/19     17:05            ABC
d----        2011/12/19     17:06            ABD
d----        2011/12/19     17:06            ABE

PS C:Powershell> $text | Format-List

    目录: C:Powershell

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----        2011/12/19     17:05            ABC
d----        2011/12/19     17:06            ABD
d----        2011/12/19     17:06            ABE
```

#### 选择属性

在显示对象结果时如果使用了像`Format-Table`这样的命令, `ETS`也不会起作用, 
因为`Format-Table`将每个属性的值转换成了文本. 
所以有的时候, 显示那些属性最好自己指定清楚, 不要把生杀大权交给`ETS`. 

```powershell
PS C:Powershell> dir | Format-Table Mode,FullName

Mode  FullName
----  --------
d---- C:PowershellABC
d---- C:PowershellABD
d---- C:PowershellABE
d---- C:Powershellmyscript
-a--- C:Powershella.ccs
-a--- C:Powershella.csv
-a--- C:Powershella.html
-a--- C:Powershella.txt
-a--- C:Powershellalias
```

#### 已知对象格式化

如果使用了格式化的命令, 但是没有指定具体的属性(如：`dir | Format-Table`). 
`ETS`将会首次大展拳脚, 它会决定那些对象应当显示, 那些属性应当被自动选择. 
`ETS`在做这些工作之前, 首先应当弄清楚, 那些对象能够被转换成文本. 

```powershell
PS C:Powershell> (dir)[0].GetType().FullName
System.IO.DirectoryInfo
```

`Dir` 返回一个`System.IO.DirectoryInfo`对象, 
并且包含了这个对象里面的`System.IO.FileInfo`对象和`System.IO.DirectoryInfo`子对象. 
这样`ETS`就可以去检查自己的内部记录, 通过内部记录的配置, 将对象转换成文本. 
这些内部记录为`XML`文件, 扩展名为“`.ps1xml`”

```powershell
PS C:Powershell> dir $PSHOME *format.ps1xml

    目录: C:WindowsSystem32WindowsPowerShellv1.0

Mode         LastWriteTime Length Name
----         ------------- ------ ----
-a---  2009/6/11      5:24  27338 Certificate.format.ps1xml
-a---  2009/6/11      5:24  27106 Diagnostics.Format.ps1xml
-a---  2009/6/11      5:24  72654 DotNetTypes.format.ps1xml
-a---  2009/6/11      5:24  24857 FileSystem.format.ps1xml
-a---  2009/6/11      5:24 257847 Help.format.ps1xml
-a---  2009/6/11      5:24  89703 PowerShellCore.format.ps1xml
-a---  2009/6/11      5:24  18612 PowerShellTrace.format.ps1xml
-a---  2009/6/11      5:24  20120 Registry.format.ps1xml
-a---  2009/6/11      5:24  24498 WSMan.Format.ps1xml
```

每一个对象详细地被定义在这些`XML`文件中, 定义包括哪些对象属性支持转换成文本, 
那些对象应当默认显示在列表或者表格中. 

有一点之前说过, 对于一行上面的混合命令“ `Get-Process ; dir`”`ETS`不支持, 
要想避免最好的方式是每个命令明确地指定版式. 

```powershell
PS C:Powershell> Get-Process | Format-Table ; dir | Format-Table
```

#### 未知对象格式化

在`ps1xml`中定义过的对象属于已知对象, 那些未知对象`ETS`应当怎样处理呢？
对于未知对象, ETS遵循一个规律：

如果对象的属性少于`5`个则表格显示, 否则列表显示. 
下面的例子创建一个对象, 并向对象中逐个增加属性. 

```powershell
PS C:Powershell> $obj=New-Object PSObject
PS C:Powershell> Add-Member -Name A -Value 1 -InputObject $obj
MemberType: PS C:Powershell>
PS C:Powershell> Add-Member -MemberType NoteProperty -Name "A" -Value "1" -InputObject $obj
PS C:Powershell> $obj

A
-
1

PS C:Powershell> Add-Member -MemberType NoteProperty -Name "B" -Value "2" -InputObject $obj
PS C:Powershell> Add-Member -MemberType NoteProperty -Name "C" -Value "3" -InputObject $obj
PS C:Powershell> Add-Member -MemberType NoteProperty -Name "D" -Value "4" -InputObject $obj
PS C:Powershell> $obj

A B C D
- - - -
1 2 3 4

PS C:Powershell> Add-Member -MemberType NoteProperty -Name "E" -Value "5" -InputObject $obj
PS C:Powershell> $obj

A : 1
B : 2
C : 3
D : 4
E : 5
```

#### 应急模式

如果ETS从输出中发现临界状态, 会自动切换到列表显示. 
例如“`Get-Process; Dir`”, `ETS`正在以表格形式输出`Process`对象, 
但是突然碰到一个`FileInfo`对象, 就会直接切换到列表模式, 输出其它类型的对象. 

#### 隐藏列

如果碰到未知的对象, `ETS`会试着从管道输出的第一个结果寻找线索, 这样可能导致一个奇怪的现象. 
`ETS`会根据未知对象的第一个结果, 来判断属性, 但第一条结果的属性并不总会输出. 
可能再碰到包含更多属性的对象时, 当前选择的属性信息就可能会被抑制. 
接下来的例子演示那些信息会被抑制, `Get-Process` 返回正在运行的所有进程, 
然后通过`StartTime`进行排序, 最输出每个进程的名称和开启时间：

```powershell
PS C:Windowssystem32> Get-Process | Sort-Object StartTime | Select-Object Name
,StartTime

Sort-Object : 获取“StartTime”时发生异常:“拒绝访问. ”
```

当执行上面的命令行时, 会收到许多错误信息. 
这些错误信息并不是来源于命令, 而是可能因为当前控制台没有管理员权限, 某些系统进程拒绝访问. 
输出的进程中可能有一部分进程只有进程名(`Name`), 没有开启时间(`StartTime`), 开启时间被抑制了. 
使用`Select-Object`, 会删除对象的某些属性, 但是对象本身的属性是不能删除的, 
所以`ETS`会在管道中重新生成一个对象, 类型为：`System.Management.Automation.PSCustomObject`. 

```powershell
PS C:Powershell> Get-Process | foreach {$_.gettype().fullname} | select -f 1 System.Diagnostics.Process
PS C:Powershell> (Get-Process | foreach {$_.gettype().fullname} | select -f 1 Name ).getType().fullname
System.Management.Automation.PSCustomObject
```

因为`PSCustomObject`在`ETS`配置中没有记录, 就会输出全部属性. 
管道结果之前根据`StartTime`升序排列过, 所以前面的进程由于权限问题没有`StartTime`. 

#### 扩充ETS

`ETS`配置中包含的类型对象会以最佳的方式转换成文本. 
但是对于未知对象就表现不完美了, 表现不完美并不代表束手无策. 
幸运的是可以通过扩充`ETS`让`ETS`以最佳的方式处理新对象. 
扩充`ETS`的第一步是确定待扩充对象类型. 
我们可能经常通过`Get-WmiObject` 来获取`WMI`服务. 
但是不太喜欢`Powershell`对于它的默认输出, 就可以扩充`ETS`了. 

```powershell
PS C:Powershell> Get-WmiObject Win32_Processor

__GENUS                     : 2
__CLASS                     : Win32_Processor
__SUPERCLASS                : CIM_Processor
__DYNASTY                   : CIM_ManagedSystemElement
__RELPATH                   : Win32_Processor.DeviceID="CPU0"
__PROPERTY_COUNT            : 48
__DERIVATION                : {CIM_Processor, CIM_LogicalDevice, CIM_LogicalEle
```

首先确定命令返回结果的对象类型

```powershell
PS C:Powershell> $object = Get-WmiObject Win32_Processor | Select-Object -first 1
PS C:Powershell> $object.GetType().FullName
System.Management.ManagementObject
```

发现目标类型为：`System.Management.ManagementObject`

接下来创建一个配置文件：

```powershell

CustomView

System.Management.ManagementObject

<label>Name</label>
12

<label>Description</label>
30

<label>ID</label>

DeviceID

Description

ProcessorID
```

将文件保存为`Win32_Processor.format.ps1xml`, 然后使用命令`Update-FormatData`把它加载进`ETS`, 会立即生效

```powershell
PS C:Powershell> Update-FormatData .Win32_Processor.format.ps1xml
PS C:Powershell> Get-WmiObject win32_processor

Name         Description                    ID
----         -----------                    --
CPU0         x64 Family 6 Model 15 Stepp... BFEBFBFF000006FD
```

但是这样的定义可能有个缺点, 当我们获取其它`WMI`对象时, 也会根据我们定义的规则显示. 

```powershell
PS C:Powershell> Get-WmiObject Win32_Share

Name         Description                    ID
----         -----------                    --
             Remote Admin
             Default share
             HP LaserJet P2050 Series PCL6
             Remote IPC
             Printer Drivers
```

出现上面的情况, 是因为`WMI`的所有对象都会以`System.Management.ManagementObject`类型返回. 
因此`ETS`没有出错, 罪魁祸首是`WMI`这个特殊的类型. 
所以扩充`ETS`时一定要细化一个具体的类型. 
事实上`WMI`对象有一个`PSTypeNames`属性, 通过它就可以找到更具体的类型. 

```powershell
PS C:Powershell> $object = Get-WmiObject Win32_Processor | Select-Object -first1
PS C:Powershell> $object.PSTypeNames
System.Management.ManagementObject#rootcimv2Win32_Processor
System.Management.ManagementObject
System.Management.ManagementBaseObject
System.ComponentModel.Component
System.MarshalByRefObject
System.Object
```

上面显示了`WMI`对象类型的继承层次. 
所以我们需求中要扩展的对象类型应该为：`System.Management.ManagementObject#rootcimv2Win32_Processor`

所以应当修改配置文件, 重新加载更新. 
更新时会有一条异常

```powershell
Update-FormatData : 加载格式数据文件时出错:
Microsoft.PowerShell, C:PowershellWin32_Processor.format.ps1xml: 文件被跳过, 
因为该文件已在“Microsoft.PowerShell”中出现过. 
```

异常可以忽略, 然后重新测试. 

```powershell
PS C:Powershell> Get-WmiObject win32_Processor

Name         Description                    ID
----         -----------                    --
CPU0         x64 Family 6 Model 15 Stepp... BFEBFBFF000006FD

PS C:Powershell> Get-WmiObject win32_share

Name                       Path                       Description
----                       ----                       -----------
ADMIN$                C:Windows          Remote Admin
C$                         C:                        Default share
```

这样ETS的扩充只对`Win32_Processor`有效了. 不会影响到其他父类型对象. 
