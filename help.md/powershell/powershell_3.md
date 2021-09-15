# learn.powershell.3.md

For myself and for you

[PowerShell 相关教程](https://www.pstips.net/). 版权归原作者所有

## 函数

### 定义函数

函数是自定义的`Powershell`代码, 有三个原则：

+ **简短** ：函数名简短, 并且显而易见. 
+ **聚合** ：函数可以完成多个操作. 
+ **封装和扩展** ：将一批`Powershell`语句进行封装, 实现全新的功能需求. 

函数的结构由三部分组成：函数名, 参数, 函数体

```powershell
Function FuncName (args[])
{
      code;
}
```

#### 使用函数作为别名

假如`Powershell`不支持”`cd..`” 命令, 你可以通过定义函数实现这个功能：

```powershell
Powershell> Function cd.. { cd ..}
Powershell> cd..
>
```

假如`Powershell`不支持`Ping`命令, 也可以如法炮制：

```powershell
PowerShell> Function Ping2 { PING.EXE  -n 1 $args }
PowerShell> Ping2 www.mossfly.com

正在 Ping mossfly.com [116.255.205.70] 具有 32 字节的数据:
...
```

#### 控制台上多行输入定义函数

```powershell
PowerShell> Function MyPing
>> {
>> PING.EXE  -n 1 $args
>> }
>>
PowerShell>
```

`Shift+Enter` 换行但不执行

#### 把函数精简成一行

你可以将一个函数定义在一行上, 但是这样阅读和理解起来就不方便, 可以在每条命令后加分号进行分割. 

```powershell
PowerShelltest> Function cd...{ cd.. ; cd.. }
PowerShelltest> cd...
>
```

#### 使用文本编辑器

函数可以在文本编辑器上编写, 写完以后复制进`Powershell`控制台即可. 
如果控制台设置为快速编辑模式, 从记事本复制后, 直接在控制台鼠标右键即可完成黏贴. 

#### 更新函数

如果要更新已经定义好的函数, 简单的方法是重新定义, 这样新的定义会覆盖旧的定义. 
但是如果函数代码没有保存副本, 可以先将函数定义导出到`ps`文件, 然后就可以编辑了. 

PowerShell> $function:MyPing | Out-File myPing.ps1
PowerShell> $function:MyPing

PING.EXE  -n 1 $args

PowerShell> $function:MyPing | Out-File myPing.ps1
PowerShell> .\myPing.ps1
必须指定 IP 地址. 
PowerShell> notepad.exe $$

#### 删除函数

控制台定义的函数只会在当前会话生效, 一旦控制台退出, 会自动消失. 
在不关闭控制台的条件下删除一个已经定义好的函数, 可使用虚拟驱动器的方法：

```powershell
PowerShell> del Function:myPing
PowerShell> myPing
无法将“myPing”项识别为 cmdlet, 函数, 脚本文件或可运行程序的名称...
```

### 处理函数的参数

Powershell函数可以接受参数, 并对参数进行处理. 函数的参数有3个特性：

1. **任意参数**：内部变量`$args`接受函数调用时接受的参数, `$args`是一个数组类型. 
1. **命名参数**：函数的每一个参数可以分配一个名称, 在调用时通过名称指定对应的参数. 
1. **预定义参数**：函数在定义参数时可以指定默认值, 如果调用时没有专门指定参数的值, 就会保持默认值. 

#### $args万能参数

给一个函数定义参数最简单的是使用`$args`这个内置的参数.  它可以识别任意个参数. 尤其适用那些参数可有可无的函数. 

```powershell
function sayHello
{
    if($args.Count -eq 0)
    {
        "No argument!"
    }
    else
    {
        $args | foreach {"Hello,$($_)"}
    }
}
```

无参数调用时：

```powershell
Powershell> sayHello
No argument!
```

一个参数调用：

```powershell
Powershell> sayHello LiLi
Hello,LiLi
```

多个参数调用时：

```powershell
Powershell> sayHello LiLi Lucy Tom
Hello,LiLi
Hello,Lucy
Hello,Tom
```

因为`$arg`是一个数组,可以用它求和. 

```powershell
function Add
{
$sum=0
$args | foreach {$sum=$sum+$_}
$sum
}
Add 10 7 3 100
#120
```

#### 设置参数名称

```powershell
function StringContact($str1,$str2)
{
    return $str1+$str2
}

StringContact moss fly
StringContact -str1 word -str2 press
mossfly
wordpress
```

`"a"+"b"="ab"`

#### 给参数定义默认值

```powershell
function stringContact($str1="moss",$str2="fly")
{
    return $str1+$str2
}
stringContact Good Luck
stringContact
```

#### 使用强类型参数

通过之前的例子发现将用户的参数传递给函数显得比较混乱. 罪魁祸首就是`Powershell`的参数解释器, 它可以自动处理和分配参数给函数. 

函数的参数解释器比较傲慢, 它对你提供的参数的信息完全不关心. 
它只会粗略地将参数进行分割, 并且最大限度的进行**自动类型转换**. 
事实上, 这种类型转换很多时候并不完美. 所以最好提前能够对参数进行强类型限制. 

#### 限制数字类型

```powershell
function subtract([int]$value1,[int]$value2)
{
    return $value1-$value2
}
subtract moss fly
```

上面的函数执行后, 会抛出异常:

```powershell
subtract : 无法处理对参数“value1”的参数转换. 无法将值“moss”转换为类型“System.Int32”. 
错误:“输入字符串的格式不正确. ”
```

因为`subtract`参数定义了强类型, 参数的类型会影响函数的处理结果, 例如调用上面的函数

```powershell
subtract 8.1 7.9
```

结果为`0`

但是如果将上面的函数的参数定义为·型, 

```powershell
function subtract([double]$value1,[double]$value2)
{
    return $value1-$value2
}

subtract 8.1  7.9

0.199999999999999
```

#### 限制日期类型

函数的参数解释器会自动尝试将字符串转换成日期类型, 如果转换失败就抛出异常. 

```powershell
function DayOfWeek([datetime]$date)
{
    return  $date.DayOfWeek
}

DayofWeek '1927-8-1'
#Monday
DayofWeek 2008-8-1
#Friday

DayofWeek someday

DayOfWeek : 无法处理对参数“date”的参数转换. ...
```

#### Switch参数

`Powershell`函数最简单的参数类型为布尔类型, 除了使用`Bool`类型, 也可以使用`Switch`关键字. 
下面的函数逆转字符串, 但是可以通过`$try`参数进行控制, 如果没有指定`$try`的值, 默认值为`$false`

```powershell
function  tryReverse( [switch]$try , [string]$source )
{
    [string]$target=""
    if($try)
    {
        for( [int]$i = $source.length -1; $i -ge 0 ;$i--)
        {
            $target += $source[$i]
        }
        return $target
    }
    return $source
}
tryReverse -source www.mossfly.com
tryReverse -try $true -source www.mossfly.com

# www.mossfly.com
# moc.ylfssom.www
```

### 指定函数的返回值

#### 一个或多个返回值

`Powershell`不像其他编程语言, 它的函数可以有多个返回值. 
如果你直接调用函数, 返回值会在控制台输出. 
当然你也可以将结果存储在一个变量中进一步处理. 下面的例子演示返回一个值：

```powershell
function Square([double]$num)
{
    return $num*$num
}
#在控制台输出结果
Square 9.87
#97.4169

#将结果赋值给变量
$value=Square 9.87
$value
#97.4169

#返回值为Double类型
$value.GetType().FullName
#System.Double
```

下面的例子演示返回多个值

```powershell
function gbMeasure($amount)
{
    "$amount GB=$($amount) GB"
    "$amount GB=$($amount*1gb/1mb) MB"
    "$amount GB=$($amount*1gb/1kb) KB"
    "$amount GB=$($amount*1gb) B"
}
#函数返回`4`个值

gbMeasure 1

# 1 GB=1 GB
# 1 GB=1024 MB
# 1 GB=1048576 KB
# 1 GB=1073741824 B

#将所有的返回值存储在一个变量中
$result=gbMeasure 1
$result

# 1 GB=1 GB
# 1 GB=1024 MB
# 1 GB=1048576 KB
# 1 GB=1073741824 B

#所有的返回值会自动存储在一个数组中
$result=gbMeasure 1
$result.GetType().Name

# Object[]

#通过索引访问每个返回值
$result=gbMeasure 1
$result[3]
# 1 GB=1073741824 B
```

`Object[]` 对象数组

总结一下, 如果一个函数返回一个值, 像其它编程语言一样, 这个值包括它的类型信息会直接返回. 
但是如果遇到多个返回值, `Powershell`会将所有的返回值自动构造成一个`Object`数组. 
可以通过索引访问数组. 

#### Return语句

`Powershell`会将函数中所有的输出作为返回值, 但是也可以通过`return`语句指定具体的我返回值. 
`Return` 语句会将指定的值返回, 同时也会中断函数的执行, `return`后面的语句会被忽略. 

```powershell
function test($num)
{
    1
    9
    return 10
    4
    6
}
test
# 1 和 9 作为输出会返回
# return语句中的10 也会返回
# return 语句后的4和6会被忽略

#1
#9
#10
```

#### 访问返回值

一个函数返回了一个值还是多个值, 是可以验证的. 
下面的例子会产生随机数, 如果没有指定个数, 默认会返回一个随机数, 否则会返回指定个数的随机数. 

```powershell
Function lottery([int]$number=1)
{
$rand = New-Object system.random
For ($i=1; $i -le $number; $i++) {
$rand.next(1,50)
}
}
# 参数为空时, 返回值不是数组:
$result = lottery
$result -is [array]
# False
# 如果指定多个随机数是, 返回值是数组类型:
$result = lottery 10
$result -is [array]
# True
```

#### 从函数的返回值中消除输出

函数默认会将函数中的所有输出作为函数的返回值返回, 这样很方便. 
但有时可能会将不必要的输出误以为返回值. 
写脚本程序时, 可能需要自定义一些函数, 这个函数可能只需要一个返回值, 但是为了提高函数的可读性, 可能会在函数增加一些注释输出行. 

```powershell
Function Test()
{
    "Try to calculate."
    "3.1415926"
    "Done."
}

#保存在变量中输出,
$value=Test
$value
# Try to calculate.
# 3.1415926
# Done.

#如果要过滤注释, 只输出, 不作为返回值, 
#可以使用Write-Host命令
Function Test()
{
    Write-Host "Try to calculate."
    "3.1415926"
    Write-Host "Done."
}
# 在变量值中保存返回值, 在控制台输出注释行
$value=Test
# Try to calculate.
# Done.

# 测试返回值
$value
# 3.1415926
```

#### 使用调试信息报告

输出这些函数中临时提示信息, 可能给函数的返回值造成干扰. 
要解决这个问题, 除了上述的`Write-Host`, 也可以使用`Write-Debug`命令. 

```powershell
Function Test()
{
    Write-Debug "Try to calculate."
    "3.1415926"
    Write-Debug "Done."
}
# Debug调试信息只会在调试模式下被输出
$value=Test
# 3.1415926

#如果你想通过显示调试信息调试函数, 可以开启调试模式
$DebugPreference="Continue"
$value=Test
# 调试: Try to calculate.
# 调试: Done.

# 测试返回值
$value
# 3.1415926

#如果关闭调试模式, 这些调试信息自然不会输出
$DebugPreference="SilentlyContinue"
$value=Test
```

使用`Write-Debug`有两个优势, 首先调试信息会自动高亮显示, 便于分析. 
其次, 这些调试信息只会在调试模式开启时输出, 控制起来更加方便. 
当然最重要的是这些临时信息无论什么时候也不会混淆在返回值. 

#### 抑制错误信息

函数中的错误信息, 也有可能作为返回值的一部分, 因为默认这些错误信息会直接输出. 

```powershell
Function ErrorTest()
{
    #该进程不存在
    Stop-Process -Name "www.mossfly.com"
}
ErrorTest

# Stop-Process : 找不到名为“www.mossfly.com”的进程. 

#很明显, 类似这样的错误提示信息, 对调试程序很重要, 但如果你觉得它不重要, 特意要隐藏, 可以使用
#$ErrorActionPreference进行设置. 

Function ErrorTest()
{
    #从这里开始隐藏所有的错误信息
    $ErrorActionPreference="SilentlyContinue"
    Stop-Process -Name "www.mossfly.com"
    #该进程不存在
}

#错误信息不会输出
ErrorTest
```

但是上面的做法并不明智, 因为这样可能错过其它错误提示. 
所以最好的方式是处理完后, 对`$ErrorActionPreference`进行复位. 

```powershell
Function ErrorTest()
{
    #从这里开始隐藏所有的错误信息
    $ErrorActionPreference="SilentlyContinue"
    Stop-Process -Name "www.mossfly.com"
    #该进程不存在

    #恢复$ErrorActionPreference,错误开始输出
    $ErrorActionPreference="Continue"

    2/0
}

ErrorTest

#试图除以零. 
所在位置 行:9 字符: 7
```

### 查看支持的函数

`Powershell`已经提供了许多用户能够使用的预定义函数, 
这些函数可以通过`Function:PSDrive`虚拟驱动器查看. 

```powershell
PS E:mossfly.com> dir function: | ft -AutoSize

CommandType Name                Definition
----------- ----                ----------
Function    A:                  Set-Location A:
Function    B:                  Set-Location B:
...
```

从这些结果不但能够看出函数的名称, 还能通过`Definition`列查看函数的内容. 
如果你想深入查看函数的内部定义可以直接访问`Function`:

```powershell
PS E:mossfly.com> $function:prompt
$(if (test-path variable:/PSDebugContext) { '[DBG]: ' } else { '' }) + 'PS ' +
$(Get-Location) + $(if ($nestedpromptlevel -ge 1) { '>>' }) + '> '
```

`Powershell`中的这些预定义的函数可以做很多重要的工作. 

+ `Clear-Host` 清除屏幕的缓存
+ `help,man` 查看命令的帮助文档
+ `mkdir,md` 通过new-Item创建子目录
+ `more` 分屏输出管道结果
+ `prompt` 返回提示文本
+ `TabExpansion` `Tab`键的自动完成提示
+ `X` : 调用`Set-Location`定位到指定的驱动器根目录

如果你想查看当前`Powershell`环境中定义了多少个函数可以通过

```powershell
PS E:mossfly.com> (dir function:).count
37
PS E:mossfly.com>
```

#### 自定义Prompt

每次成功执行完一条命令, `Powershell`就会执行`Prompt`函数, 提示用户进行下一步输入. 
默认设置中, `prompt`显示“`PS`” 和当前的工作目录. 
再接着是”`>`”或”`>>`”,具体情况要看当前`Powershell`控制台的的层数. 
当然你可以自定义`prompt`的, 那就得覆盖`prompt`函数：

```powershell
PS E:mossfly.com> pwd

Path
----
E:mossfly.com

PS E:mossfly.com> cd ..
PS E:> Function Prompt {"www.mossfly.com"}
www.mossfly.com
www.mossfly.com pwd

Path
----
E:
www.mossfly.com $function:prompt
"www.mossfly.com"
www.mossfly.com
```

这样的覆盖安全吗, 显然安全, 
对预定义函数的重写, 只会在当前控制台会话中有效, 当你重新启动控制台时, 自然会恢复如初. 

#### 在控制台的任何位置输出文本(自定义光标的位置)

因为控制台的内容存放在控制台屏幕的缓存中, 因此你可以逐个访问内容的每一行或每一个字符. 
你甚至可以在控制台的屏幕的任何位置输出你想要输出的信息, 接下来的函数会演示这个功能. 

要完成这个功能, 需要使用`$Host.UI.Rawui`.

光标的位置通过屏幕的横坐标(`X`)和纵坐标(`Y`)确定.
下面的函数会首先记住当前光标的位置, 然后在横坐标上增加`60`个占位符, 
然后重置光标的位置至当前位置, 最后通过`prompt`函数回复光标的原始位置. 

```powershell
function prompt
{
    $curPos = $host.ui.rawui.CursorPosition
    $newPos = $curPos
    $newPos.X+=60
    $host.ui.rawui.CursorPosition = $newPos
    Write-Host ("{0:D} {0:T}" -f (Get-Date)) -foregroundcolor Yellow
    $host.ui.rawui.CursorPosition = $curPos
    Write-Host ("PS " + $(get-location) +">") -nonewline -foregroundcolor Green
" "
}
```

运行结果

```powershell
PS E:mossfly.com>                                          2012年2月28日 8:54:01
```

#### 使用窗口标题栏

在Windows控制台的标题栏有一部分空间, 可以放置一些有用的信息, 比如当前哪个用户登录在控制台, 
可以通过设置`$host.UI.RawUI.WindowTitle`来自定义控制台标题栏的文本. 

下面的例子就会演示设置标题栏文本, 
通过`.NET`方法获取当前用户信息, 由于该方法会有几秒钟执行时间, 
为了效率考虑首先将用户信息保存在全局变量中, 然后在`Prompt`函数中调用. 

```powershell
$global:CurrentUser = [System.Security.Principal.WindowsIdentity]::GetCurrent()
function prompt
{
$host.ui.rawui.WindowTitle = "Line: " + $host.UI.RawUI.CursorPosition.Y + " " + $CurrentUser.Name + " " + $Host.Name + " " + $Host.Version
Write-Host ("PS " + $(get-location) +">")  -nonewline -foregroundcolor Green
return " "
}
```

执行以后在标题栏会显示：`Line: 72 ComputerNameuser ConsoleHost 2.0`

如果你使用管理员权限运行控制台时, `Prompt`函数还可以给出警告. 
使用`WindowsPrincipal`辨别当前用户是否使用了管理员权限, 你不需要了解下面的`.NET`代码, 它会在全局变量中将布尔值赋值给`$Admin`. 

```powershell
$CurrentUser = [System.Security.Principal.WindowsIdentity]::GetCurrent()
$principal = new-object System.Security.principal.windowsprincipal($CurrentUser)
$global:Admin = $principal.IsInRole( [System.Security.Principal.WindowsBuiltInRole]::Administrator)
Function prompt
{
    # 输出标准的提示信息:
    Write-Host ("PS " + $(get-location)) -nonewline
    # The rest depends on whether you have admin rights or not:
    If ($admin)
    {
        $oldtitle = $host.ui.rawui.WindowTitle
        # 将"Administrator: " 显示在标题栏
        If (!$oldtitle.StartsWith("Administrator: "))
        {
            $host.ui.rawui.WindowTitle ="Administrator: " + $oldtitle
        }
        #  Prompt结尾显示红色的尖括号
        Write-Host ">" -nonewline -foregroundcolor Red
     }
     Else
     {
        Write-Host ">" -nonewline
      }
     return " "
}
```

没有管理员权限时, 标题栏文本：`Windows Powershell`
有管理员权限时, 标题栏文本:`Administrator ：管理员 : Windows Powershell

#### Clear-Host:删除屏幕缓存

很可能, 你已经注意到了, `cls`可以删除屏幕的缓存. 
事实上, `cls`只是`Clear-Host`函数的别名, 但是却看不到这个函数的内容. 

```powershell
PS E:mossfly.com> $function:Clear-Host

必须在“-”运算符的右侧提供值表达式...
```

在`Powershell`中短斜杠是个特殊字符, 如果一个函数名中包含了特殊字符就应当把它放在花括号中. 

```powershell
PS E:mossfly.com> ${function:Clear-Host}
$space = New-Object System.Management.Automation.Host.BufferCell
$space.Character = ' '
$space.ForegroundColor = $host.ui.rawui.ForegroundColor
$space.BackgroundColor = $host.ui.rawui.BackgroundColor
$rect = New-Object System.Management.Automation.Host.Rectangle
$rect.Top = $rect.Bottom = $rect.Right = $rect.Left = -1
$origin = New-Object System.Management.Automation.Host.Coordinates
$Host.UI.RawUI.CursorPosition = $origin
$Host.UI.RawUI.SetBufferContents($rect, $space)
```

#### 盘符名预定义函数`C:,D:,E:`

这些盘符名称可以作为单独的一个函数, 是怎么做到的呢？

```powershell
PS E:mossfly.com> $function:c:
Set-Location C:
PS E:mossfly.com> $function:d:
Set-Location D:
PS E:mossfly.com> $function:E:
Set-Location E:
PS E:mossfly.com> $function:A:
Set-Location A:
```

### 函数 过滤器 管道

一个函数能够访问和进一步处理另外一条命令的结果吗？
答案是肯定的, 这被称为管道. 

管道有两种模式, 一种是顺序处理模式, 一种是流处理模式. 

#### 低效率的顺序模式:$input

在最简单的情况下, 你的函数不是真正支持管道. 只能对前一个命令执行后的结果处理. 
前一个命令执行的结果通过被自动保存在`$input`变量中, `$input`是一个数组, 它可以包含许多元素, 一个元素, 甚至一个元素都没有, 这取决于具体的环境. 

下面的例子, 是一个函数, 仅仅输出`$input`的内容. 

```powershell
PS E:mossfly.com> Function output
>> {
>>    $input
>> }
>>
PS E:mossfly.com> output
PS E:mossfly.com> 1,2,3 | output
1
2
3
PS E:mossfly.com> dir | output

    目录: E:mossfly.com

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----         2012/2/28     23:34            a
d----         2012/2/28     23:35            b
d----         2012/2/28     23:35            c
```

到目前为止, 这个函数只是仅仅输出了管道的结果, 并没有其它比较强大的功能. 
在接下来的例子中, 函数将会对管道的结果做进一步处理. 
函数名`MarkEXE`,将会检查`Dir`的结果, 并高亮标记后缀名为`EXE`的文件名为红色. 

```powershell
Function MarkEXE
{
# 保存控制台当前的前景色
$oldcolor = $host.ui.rawui.ForegroundColor
# 通过循环逐条检查管道的结果
Foreach ($element in $input)
{
    # 如果后缀名为.exe,设置为前景色为红色
    If ($element.name.toLower().endsWith(".exe"))
    {
        $host.ui.Rawui.ForegroundColor = "red"
    }
    Else
    {
        # 否则恢复默认的前景色
        $host.ui.Rawui.ForegroundColor = $oldcolor
    }
    # 输出数组元素
    $element
}
# 最后, 重置控制台的前景色:
$host.ui.Rawui.ForegroundColor = $oldcolor
}
```

#### 过滤器：高效率 流模式

管道的低效率顺序模式在处理大容量数据时很容易出现问题, 其结果是巨大的内存占用和进程等待. 
如果你的函数支持高效率的流模式, 在处理管道结果时仅占用很小的内存. 

事实上, 针对之前`MarkEXE`函数, 你只需要替换”`function`” 关键字 为 “`filter`”, 
它就会开始流模式处理, 这样你再也不用过分的担心忍受程序的无休止的响应和崩溃的危险. 
你也可以递归处理全盘目录, 甚至处理极其庞大的数据. 例如：

```powershell
Dir c: -recurse | MarkEXE
```

当`MarkEXE`每次被调用时, 它只会对当前目录下的每个单独的元素进行处理. 
对于过滤器`filters`来说, `$input`一直都是一个独立的元素. 
这也就是为什么在过滤器中`$input`一点用也没有的道理. 
此时, 最好使用`$_`变量, 因为它代表了当前处理的数据. 
这样还可以简化`MarkExe`, 因为过滤器自身已经扮演了循环的角色了, 
**你没有必要再写专门的循环处理了**. 

```powershell
Filter MarkEXE
{
    # 记录当前控制台的背景色
    $oldcolor = $host.ui.rawui.ForegroundColor
    # 当前的管道元素保存在 $_ 变量中
    # 如果后缀名为 ".exe",
    # 改变背景色为红色:
    If ($_.name.toLower().endsWith(".exe"))
    {
        $host.ui.Rawui.ForegroundColor = "red"
    }
    Else
    {
        # 否则使用之前的背景色
        $host.ui.Rawui.ForegroundColor = $oldcolor
    }
    # 输出当前元素
    $_
    # 最后恢复控制台颜色:
    $host.ui.Rawui.ForegroundColor = $oldcolor
}
```

#### 开发真正的管道函数

过滤器在函数中属于高级应用, 因为它可以立即处理管道结果的每一个元素. 
但是过滤器必须每次重复执行预定义命令的结果. 
对于`MarkEXE`函数, 每次执行的过程中要记录和更新控制台的背景颜色, 也要花费资源和时间. 

事实上,过滤器只是特殊的函数. 如果一个函数内部使用了管道, 你就可以定义三个基础的任务区了：

+ 第一步, 完成函数的初始化, 完成函数执行的预备步骤;
+ 第二步处理递归调用所得的结果;
+ 最后进行收尾工作. 

这三个任务区分别可以使用`begin,process,end`语句块. 

接下来把`MarkEXE`按照上面的模式进行改装：

```powershell
Function MarkEXE
{
    begin
    {
        # 记录控制台的背景色
        $oldcolor = $host.ui.rawui.ForegroundColor
    }
    process
    {
        # 当前管道的元素 $_
        # 如果后缀名为 ".exe",
        # 改变背景色为红色:
        If ($_.name.toLower().endsWith(".exe"))
        {
            $host.ui.Rawui.ForegroundColor = "red"
        }
        Else
        {
            # 否则, 使用正常的背景色:
            $host.ui.Rawui.ForegroundColor = $oldcolor
         }
        # 输出当前的背景色
        $_
      }
    end
    {
        # 最后,恢复控制台的背景色:
        $host.ui.Rawui.ForegroundColor = $oldcolor
     }
}
```

## 脚本

### 编写和运行脚本

一个`Powershell`仅仅是一个包含`Powershell`代码的文本文件. 如果这个文本文件执行, `Powershell`解释器会逐行解释并执行它的的语句. 
`Powershell`脚本非常像以前`CMD`控制台上的批处理文件. 您可以通过非常简单的文本编辑工具创建`Powershell`脚本. 

#### 通过重定向创建脚本

如果您的脚本不是很长, 您甚至可以直接在控制台中将欲执行的语句重定向到一个脚本文件. 

```powershell
PS E:> '"Hello,Powershell Script"' > MyScript.ps1
PS E:> .\MyScript.ps1
Hello,Powershell Script
```

这样有个缺点, 就是您的代码必须放在闭合的引号中. 
这样的书写方式一旦在脚本内部也有引号时, 是一件很痛苦的事. 甚至您还可能希望在脚本中换行. 

下面的`Here-strings`例子不错, 也就是将脚本文件通过`@'`和`'@`闭合起来

```powershell
PS E:> @'
>> Get-Date
>> $Env:CommonProgramFiles
>> #Script End
>> "files count"
>> (ls).Count
>> #Script Really End
>>
>> '@ > myscript.ps1
>>
PS E:> .MyScript.ps1

2012年4月27日 8:15:10
C:\Program Files\Common Files
files count
20
```

`Here-String`以 `@'`开头, 以`'@`结束. 任何文本都可以存放在里面, 哪怕是一些特殊字符, 空号, 白空格. 
但是如果将单引号改成双引号, `Powershell`将解析`Here-String`出现的变量, 也就是代入它们绑定的值.

#### 通过编辑器创建脚本

其实非常方便的还是最地道的文本编辑器`Notepad`, 您可以直接在`Powershell`控制台中打开`Notepad`

```powershell
PS E:> notepad.exe .\MyScript.ps1
PS E:> notepad.exe
```

编辑完记得保存即可. 

运行Powershell脚本

当您的脚本编写成功后您可能第一次会像下面的方式运行它, 也就是只输入脚本的文件名, 会报错. 

```powershell
PS E:> MyScript.ps1
无法将“MyScript.ps1”项识别为 cmdlet, 函数, 脚本文件或可运行程序的名称...
```

除非您使用相对路径, 或者绝对路径

```powershell
PS E:> .\MyScript.ps1
>> # or
PS E:> E:MyScript.ps1
```

#### 执行策略限制

`Powershell`一般初始化情况下都会禁止脚本执行. 脚本能否执行取决于`Powershell`的执行策略. 

```powershell
PS E:> .\MyScript.ps1
无法加载文件 E:MyScript.ps1, 因为在此系统中禁止执行脚本...
```

只有管理员才有权限更改这个策略. 非管理员会报错. 

查看脚本执行策略, 可以通过：

```powershell
PS E:> Get-ExecutionPolicy
```

更改脚本执行策略, 可以通过

```powershell
PS E:> Get-ExecutionPolicy
Restricted
PS E:> Set-ExecutionPolicy UnRestricted
```

脚本执行策略类型为：`Microsoft.PowerShell.ExecutionPolicy`
查看所有支持的执行策略：

```powershell
PS E:>  [System.Enum]::GetNames([Microsoft.PowerShell.ExecutionPolicy])
Unrestricted
RemoteSigned
AllSigned
Restricted
Default
Bypass
Undefined
```

+ `Unrestricted` :权限最高, 可以不受限制执行任何脚本. 
+ `Default` :为`Powershell`默认的策略：`Restricted`, 不允许任何脚本执行. 
+ `AllSigned` ：所有脚本都必须经过签名才能在运行. 
+ `RemoteSigned` ：本地脚本无限制, 但是对来自网络的脚本必须经过签名. 

关于`Powershell`脚本的签名在后续会谈到. 

#### 像命令一样执行脚本

怎样像执行一个命令一样执行一个脚本, 不用输入脚本的相对路径或者绝对路径, 甚至`*.ps1`扩展名. 
那就将脚本的执行语句保存为别名吧：

```powershell
PS E:> Set-Alias Invok-MyScript .\MyScript.ps1
PS E:> Invok-MyScript


2012年4月28日 0:24:22
C:\Program Files\Common Files
files count
20
```

### 给脚本传递参数

怎样将一个脚本稍作润色, 让它能够根据用户的输入, 处理并输出相应的结果, 而不是只产生一成不变的输出. 
怎样将参数传递给脚本, 这是本篇讨论的内容. 

#### $args返回所有的参数

传递给一个函数或者一个脚本的参数都保存在`$args`变量中. 
可以先打开记事本, 输入脚本`Write-Host "Hello,$args"`,保存后, 通过控制台执行脚本:

```powershell
PS E:> notepad myscript.ps1
PS E:> .\MyScript.ps1
Hello,
PS E:> .\MyScript.ps1 "Mosser Lee"
Hello,Mosser Lee
```

#### $args数组参数

默认情况下, 传递给一个`Powershell`脚本的参数类型为数组, 例如：

```powershell
PS E:> .MyScript.ps1 My Website      Is        www.mossfly.com
Hello,My Website Is www.mossfly.com
```

上面的文本中包含多个连续的空格, 可是当脚本把参数输出时却不存在连续的空格了. 
那是因为脚本会把文本根据白空格截断并转换成数组. 
如果不想文本被当成数组那就把它放在引号中. 

```powershell
PS E:> ./MyScript.ps1 "My Website      Is        www.mossfly.com"
Hello,My Website      Is        www.mossfly.com
```

#### 在$args中逐个访问参数

因为`$args`是一个数组, 自然可以通过索引访问数组的每一个元素. 可以将`MyScript.sp1`的内容改为：

```powershell
For($i=0;$i -lt $args.Count; $i++)
{
    Write-Host "parameter $i : $($args[$i])"
}
```

然后在控制台测试：

```powershell
PS E:> .\MyScript.ps1 www moss fly com
parameter 0 : www
parameter 1 : moss
parameter 2 : fly
parameter 3 : com
```

#### 在脚本中使用参数名

通过`Powershell`传递参数固然方便, 但是如果用户不知道参数的传递顺序, 也是很郁闷的.
例如在`Myscript.ps1`中输入：`$args[0]-$args[1]`, 执行脚本发现参数的顺序不同, 结果也不同：

```powershell
PS E:> Get-Content .\MyScript.ps1
$args[0]-$args[1]
PS E:> .\MyScript.ps1 10 8
2
PS E:> .\MyScript.ps1 8 10
-2
```

所以最好的方式给参数指定名称, 输入以下的脚本：

```powershell
param($Directory,$FileName)

"Directory= $Directory"
"FileName=$FileName"
```

其中`param`给参数指定名称. 

执行脚本：

```powershell
PS E:> .\MyScript.ps1 -Directory $env:windir -FileName config.xml
Directory= C:windows
FileName=config.xml
PS E:> .\MyScript.ps1 -FileName config.xml -Directory $env:windir
Directory= C:windows
FileName=config.xml
```

#### 验证参数

给脚本的参数绑定数据类型, 绑定帮助信息. 一旦脚本缺少参数, 或者输入的参数类型不正确, 就提醒用户输入脚本：

```powershell
param(
[string]$Name=$(throw "Parameter missing: -name Name") ,
[int]$Age=$(throw "Parameter missing: -age x as number")
)

"Name= $Name"
"Age=$Age"
```

执行脚本:

```powershell
PS E:> .\MyScript.ps1
Parameter missing: -name Name
所在位置 E:MyScript.ps1:2 字符: 22...

PS E:> .\MyScript.ps1 -Name mosser -Age 100

Name= mosser
Age=100
```

#### 变量的作用域

`Powershell`默认使用全局作用域`global:` , 
但是在函数和脚本中分别使用函数作用域`function:`和脚本作用域`script:` . 
一旦脚本执行结束, 存在于脚本作用域的变量也会消失. 
但是有一点, 如果一个变量在脚本外定义, 在脚本内没有定义, 在脚本内使用时会把外面的变量引渡过来. 

在脚本中输入：

```powershell
$temp
```

执行脚本：

```powershell
PS E:> .\MyScript.ps1
PS E:> $temp="mosser lee"
PS E:> .\MyScript.ps1
mosser lee
```

在脚本中尝试改变变量`$temp`,但是脚本内的变量不会影响脚本外的变量, 输入脚本：

$temp="www.mossfly.com"
$temp

执行脚本：

```powershell
PS E:> .\MyScript.ps1
www.mossfly.com
PS E:> $temp
mosser lee
```

#### 增强脚本的可读性

如果你愿意, 你可以把一个脚本写的非常长, 问题是脚本的代码量越大, 可读性越差. 
最好的方式是在写脚本时融入**函数**和**类库**的概念：

**函数** ：把实现一些小功能的代码写成一个函数, 不仅可以增强代码的可读性, 还可以很方便的重用. 
一旦你创建了一个实现特定功能的函数, 也可以下次在其它脚本中使用. 

**类库** ：把需要的函数嵌入进类库中, 就不用每次在执行脚本时拷贝函数, 并且还可以在需要时扩充它. 
另外以函数的方式构建类库, 还可以让你更专注特定功能的具体实现, 降低脚本开发的复杂度. 

#### 在脚本中使用函数

要在脚本中使用函数, 最简单的方法自然是将函数直接写在脚本中：
在`MyScript.ps1`中输入：

```powershell
param([int]$n=$(throw "请输入一个正整数"))
Factorial $n
Function Factorial([int]$n)
{
    $total=1
    for($i=1;$i -le $n;$i++)
    {
        $total*=$i
    }
    return $total
}
```

这个脚本接收一个正整数参数, 然后通过`Factorial`函数求阶乘. 

```powershell
PS E:> .\MyScript.ps1
请输入一个正整数
所在位置 E:MyScript.ps1:1 字符: 22
```

不像其它脚本语言, `Powershell`中的函数必须先定义后使用. 
所以更改脚本为：

```powershell
param([int]$n=$(throw "请输入一个正整数"))
Function Factorial([int]$n)
{
    $total=1
    for($i=1;$i -le $n;$i++)
    {
        $total*=$i
    }
    return $total
}
Factorial $n
```

执行脚本：

```powershell
PS E:> .\MyScript.ps1 10
3628800
```

#### 将脚本分为工作脚本和类库

真正的脚本开发需要处理的问题可能包含许多函数. 
如果在一个脚本的开头定义许多函数, 脚本会显得很凌乱. 
把函数和工作脚本分开, 可以隔离函数, 使它不容易被修改. 

将`Factorial`函数保存在`PSLib.ps1`

```powershell
Function Factorial([int]$n)
{
    $total=1
    for($i=1;$i -le $n;$i++)
    {
        $total*=$i
    }
    return $total
}
```

将脚本修改为：

```powershell
param([int]$n=$(throw "请输入一个正整数"))
. .\PSLib.ps1
Factorial $n
```

执行脚本：

```powershell
PS E:> .\MyScript.ps1 10
3628800
```

脚本在执行时, 先加载类库中的函数. 
加载函数类库和执行脚本类似, 只需要在前面增加一个句号, 中间有空格. 

**note**:
`.` 运算符用于调用`.NET`对象的成员, 它也可以用于执行脚本. 
当它用于执行脚本的时候, 脚本会在当前作用域中执行. 
所以脚本结束之后, 我们可以访问脚本中的元素. 

#### 类库脚本集中存放

在开始使用类库脚本工作之前, 最好先制定出一个存储脚本类库的策略. 
一种方法是和工作脚本存放在一起, 可以使用相对路径；
另一种方法是分开存放, 加载时就得使用绝对路径了. 
最好在当前用户的私人目录中存放脚本, 相对来说比较安全. 

例如下面的例子：

```powershell
PS E:> md $env:appdataPSLib

    目录: C:UsersbaozhenAppDataRoaming

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----         2012/4/30     22:47            PSLib

PS E:> copy .PSLib.ps1 $env:APPDATAPSLib
```

### 创建管道脚本

我们可以像创建管道函数那样创建管道脚本, 
具体采用低速顺序模式, 还是高速流模式, 这取决于具体的编程实现. 

#### 低速顺序模式

如果你在脚本中使用管道, 脚本收集上一个语句的执行结果, 默认保存在`$input`自动变量中. 
**但是直到上一条语句完全执行彻底, 管道脚本才会执行**. 

创建脚本：

`pipeline.ps1`

```powershell
foreach ($element in $input)
{
    if($element.Extension -eq ".exe")
    {
        Write-Host -fore "red" $element.Name
    }
    else
    {
        Write-Host -fore "Green" $element.Name
    }
}
```

执行脚本：

`PS E:> ls $env:windir | .\pipeline.ps1`

如果这样执行：

`PS E:> ls $env:windir  -Recurse | .\pipeline.ps1`

控制台会被冻结, 因为存储的中间结果在玩命的吃内存. 这个也是低速顺序模式的缺点. 

#### 高速流模式

在`Powershell`脚本的处理中, 绝大多数情况下遇到的都是集合, 
一旦上一条命令产生一个中间结果, 下一条命令就对这个中间结果及时处理, 及时释放资源. 
这样可以节省内存, 也减少了用户的等待时间. 在处理大量数据时, 尤其值得推荐. 
高速流模式的管道定义包括三部分：
`begin,process,end`. 
上面的描述中提到了中间结果, 中间结果保存在`$_`自动化变量中. 

输入脚本文件:

```powershell
begin
{
    Write-Host "pipe script initilizing"
}
process
{
    $ele=$_
    if($_.Extension -ne "")
    {
        switch($_.Extension.tolower())
        {
            ".ps1" {"pscript："+ $ele.name}
            ".txt" {"text："+ $ele.Name}
            ".gz"  {"archived："+ $ele.Name}
        }
    }
}
end
{
    Write-Host "pipe script environment recovered"
}
```

执行脚本文件:

```powershell
PS E:> ls | .\pipeline.ps1
管道脚本环境初始化
文本文件：a.txt
压缩文件：Metrol.tar.gz
脚本文件：MyScript.ps1
脚本文件：pipeline.ps1
脚本文件：PSLib.ps1
管道脚本环境恢复
```

### 自动执行脚本之profile

在`Powershell`控制台的许多更改只会在当前会话有效. 
一旦关闭当前控制台, 你自定义地所有别名, 函数, 和其它改变将会消失, 除非将更改保存在`windows`环境变量中. 
这也就是为什么我们需要`profile`来保存一些基本的初始化工作. 

四种不同的`profile`脚本

`Powershell`支持四种可以用来初始化任务的`profile`脚本. 
应用之前要弄清楚你的初始化是当前用户个人使用, 还是所有用户. 
如果是个人使用, 可以使用”`当前用户profile`“, 
但是如果你的初始化任务是针对所有用户, 可是使用“`所有用户profile`”. 

|  `Profile` |  描述  |      位置   |
|----------|----------|---------|
| 所有用户 | 所有用户共有的`profile`  |   `$pshome\profile.ps1`   |
|所有用户(私有) | `powershell.exe` 中验证.  | `$pshome\Microsoft.PowerShell_profile.ps1` |
| 当前用户      | 当前用户的`profile`| `$((Split-Path $profile -Parent)+ “\profile.ps1”)`|
| 当前用户(私有)  | 当前用户的`profile`；只在`Powershell.exe`中验证  |`$profile`|

我们注意到上面的四种`profile`有两个`private`. 
一旦声明为`private`, 只有`microsoft`的`Powershell`自身才会去调用, 
不会对其它引用`powershell`的组件有效. 

#### 创建自己的`profile`

`Profile`脚本并不是强制性的, 换言之, `profile`可有可无. 下面会很方便的创建自己的`profile`. 

在控制台执行：

`notepad $((Split-Path $profile -Parent) + “\profile.ps1”) `

如果不存在`profile`默认会创建, 在打开的记事本中输入：`Set-Alias edit notepad.exe`

也就是给`notepad`添加`edit`别名, 保存关闭, 之后重启控制台, 输入：

`edit $((Split-Path $profile -Parent) + “\profile.ps1”)`

控制台会调用记事本打开之前的`profile`, 可见`edit`别名已经生效. 

#### 创建全局`profile`

创建全局的`profile`也是很容易的, 如上, 只是文件的位置稍有改变；

需要注意的是, 创建全局`profile`需要管理员权限, 没有管理员权限, 该文件或者文件夹拒绝访问. 

### 脚本数字签名

脚本很容易被冒名顶替或者更改, 因为它们是由纯文本构成的. 
数字签名为脚本提供了更高的安全性, 因为它能确定脚本和脚本的编辑者的唯一性, 并且不能被更改. 

作为脚本的发布者, 你能确定你的脚本没有被恶意篡改. 即使专家也无能为力, 因为这种机制是基于复杂逻辑的. 
幸运的是, 在实际应用中, 你不需要深究这些细节, 只需要掌握`Powershell`脚本签名的机制和过程. 

#### 准备一个合适的证书

因为不能使用传统的纸质签名给`Powershell`脚本进行签名, 你需要另一个工具“**证书**”. 
证书就像一把私有并且安全的钥匙. 证书是你的个人电子身份特征. 这把私密的钥匙确保只有证书的拥有者使用证书进行脚本签名. 

可以通过`mmc`添加管理单元查看证书, 但是在`Powershell`中有专门查看证书的支持. 
可以通过虚拟驱动器`cert：`查看本机支持的证书. 

#### 创建自签名证书

创建一个自签名证书, 需要用到`microsoft`的工具, `makecert.exe` . 
这个工具不能单独下载, 但是它包含在微软的`.NET framework`中, 如果你的电脑上已经安装了`Visual studio` 那就方便多了. 

`Visual Studio 命令提示`

```powershell
makecert.exe -pe -r -n "cn=TomPowershellCert" -eku 1.3.6.1.5.5.7.3.3 -ss "my"
Succeeded
```

这里要稍微注意 `-eku` 参数：`1.3.6.1.5.5.7.3.3`, 不能是其它, 否则证书的预期目的属性就不是代码签名了. 
上面创建的证书会自动保存在`CurrentUser\My` 路径下面. 可以在`Powershell`中查看：

```powershell
PS E:> ls cert:\CurrentUser\My | where {$_.subject -eq "cn=TomPowershellCert"}

    目录: Microsoft.PowerShell.SecurityCertificate::CurrentUserMy
```

#### 验证代码签名证书

查看支持代码签名的证书
查看证书的签发者, 代表, 序列号, 指纹

```powershell
## 查看预期目的为代码签名的证书:
$certs = @(Dir cert:\CurrentUser\My -codeSigningCert)
"找到 {0} 个代码签名证书" -f $certs.count
# 找到 1 个代码签名证书

## 选择 刚才创建的证书
$certificate=ls cert:\CurrentUser\My | where {$_.subject -eq "CN=TomPowershellCert"}

## 证书的代表
$certificate.subject
# CN=MosserPowerShellTestCert

## 证书的签发者
$certificate.issuer
# CN=MosserPowerShellTestCert

## 证书的序列号, 指纹
$certificate |  select SerialNumber,Thumbprint | fl *
# SerialNumber : C23F35EA85D9A5AB466C07A7C0469A78
# Thumbprint   : 586A4332F0528867DA6A0900FCF0938EDD277E22
```

#### 声明一个证书受信任

你会发现, 在你指定证书的类型, 颁发者的名称等信息后, 证书的原始数据(`RawData`)会自动生成. 
这样你不能假冒别人生成一个证书, 别人也不能假冒你的名字生成一个证书. 
如果通过`Powershell`查看之前生成的证书是否受信任, 答案为否. 

```powershell
PS E:> $certificate.Verify()
False
```

为什么我们刚才生成的证书不受信任呢？我们可以通过一个简单的步骤找到答案. 
在 `.NET` 中有一个方法：`DisplayCertificate()`
可以通过对话框显示证书, 位于`System.Security.dll`中. 
这个`dll`默认没有引用, 需要添加引用, 之后显示证书对话框. 

```powershell
PS E:> [System.Reflection.Assembly]::LoadWithPartialName("System.Security")

GAC    Version        Location
---    -------        --------
True   v2.0.50727     C:windowsassemblyGAC_MSILSystem.Security2.0.0.0__b03f5f7f11d50a3aSys...

[System.Security.Cryptography.x509Certificates.X509Certificate2UI]::DisplayCertificate($certificate)
```

 [System.Security.Cryptography.X509Certificates.X509Certificate2UI]

对话框提示：此`CA`根证书不受信任, 要启用信任, 请将该证书安装到”受信任的根证书颁发机构“存储区. 

所以接下来可以将该证书复制到受信任的存储区. 可以通过`certmgr.msc` 手动操作, 也可以通过`Powershell`自动化操作

```powershell
PS E:> $rootStore= New-Object system.security.cryptography.X509Certificates.x509Store("root","Currentuser")
$rootStore.Open("ReadWrite")
$rootStore.Add($certificate)
$rootStore.Close()
```

在执行`Add`操作时, 会有一个确认的对话框, 确定即可. 

接下来我们查看一下验证信息. 

```powershell
PS E:> $certificate.Verify()
True
```

#### 给Powershell 脚本签名

给`Powershell`脚本进行数字签名只需要两步：
找的一个受信任的代码签名证书, 剩下的工作请交给：`Set-AuthenticodeSignature`吧. 

```powershell
PS E:> 'Write-Host "我的第一个签名脚本"' > firstSignScript.ps1
PS E:> $certificate=ls cert:\CurrentUser\My | where {$_.subject -eq "CN=TomPowershellCert"}
PS E:> Set-AuthenticodeSignature .\firstSignScript.ps1 $certificate

    目录: E:

SignerCertificate                        Status Path
-----------------                        ------ ----
586A4332F0528867DA6A0900FCF0938EDD277E22 Valid  firstSignScript.ps1

PS E:> Get-Content .\firstSignScript.ps1
Write-Host "我的第一个签名脚本"

# SIG # Begin signature block
# MIIEIQYJKoZIhvcNAQcCoIIEEjCCBA4CAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
...
```

#### 递归给所有脚本文件签名

给当前文件下的所有脚本签名

```powershell
PS E:> Set-AuthenticodeSignature (ls *.ps1) $certificate
```

如果你喜欢你甚至可以递归使用

```powershell
Set-AuthenticodeSignature (Dir -recurse -include *.ps1) $certificate
```

#### 使用对话框选择证书

如果机器上安装了代码签名的证书有许多, 你可以通过friendName 或者证书的名称, 证书的指纹,过滤一个证书供脚本签名. 

 Dir cert:CurrentUserMy |
 where {$_.subject -eq "CN=MosserPowerShellTestCert"}

另一种方法是通过.NET中的内置的对话框进行选择. 将查询到的证书传递给SelectFromCollection()方法, 在在作此操作之前必须将证书放在一个特殊的集合中. 

```powershell
# 对话框文本:
$title = "可用的证书"
$text = "请选择用于代码签名的证书："
# Find certificates:
$certificates = Dir cert: -recurse -codeSigningCert
# 加载 System.Security 类库
# 将证书存放在特殊的集合(X509Certificate2Collection)中:
[Reflection.Assembly]::LoadWithPartialName("System.Security")
$collection = New-Object System.Security.Cryptography.X509Certificates.X509Certificate2Collection
$certificates | ForEach-Object { $collection.Add($_) }
# 显示选项:
$certificate =[System.Security.Cryptography.x509Certificates.X509Certificate2UI]::`
SelectFromCollection($collection, $title, $text, 0)
# 使用选择的证书进行数字签名
Set-AuthenticodeSignature -Certificate $certificate[0] -FilePath .firstSignScript.ps1
```

#### 脚本签名验证

在脚本中签名到底能带来什么好处, 那就是可以进行验证. 
可以手动验证, 也可以自动验证. 
签名验证会告诉你脚本是否信任, 或者是否包含了恶意篡改. 

用户自行验证：手动验证, 可以检查一个脚本是否包含签名代码, 签名者是谁？该签名者是否受信任. 

自动验证：如果你将`Powershell`的脚本执行策略设置为`AllSigned`.
`Powershell`会在你尝试运行脚本时自动验证, 代码和脚本签名是否一致. 并且会询问签名者是否受信任. 

#### 手动验证

`Get-AuthenticodeSignature`命令可以验证签名. 

例如创建一个脚本, 不进行签名, 通过该命令进行验证. 
属性`StatusMessage`会告诉你签名验证的结果. 

```powershell
"'未签名'" >notsign.ps1
$checkResult=Get-AuthenticodeSignature .\notsign.ps1
$checkResult.Status
NotSigned
$checkResult.StatusMessage
文件 E:notsign.ps1 未经数字签名. 系统将不执行该脚本...

$checkResult.Status.GetType().fullName
System.Management.Automation.SignatureStatus
```

#### 自动验证

你不须要去验证脚本的签名, 当你运行一个脚本时, `Powershell`会自动验证. 
即使验证过的脚本, 如果有部分内容更新, 自动验证也会给出警告. 

在用户将脚本执行策略设置为`AllSigned`和`RemoteSigned`时, 自动验证就会激活, 
如果将执行策略设置为`AllSigned`, 所有的脚本都会验证. 
如果你选择`RemoteSigned`, 从网络上下载的脚本执行会提示需要签名. 

```powershell
# 设置 ExecutionPolicy 为 AllSigned. 所有
# 脚本必须有正确的签名:
Set-ExecutionPolicy AllSigned
# 创建一个没有签名的脚本.
# 该脚本不会执行:
无法加载文件 E:unSigned.ps1. 文件 E:unSigned.ps1 未经数字签名...

即使签名可以通过验证, 也需要用户的批准, 才能执行. 
`.\firstSignScript.ps1`

是否要运行来自此不可信发布者的软件?
文件 E:firstSignScript.ps1 由 CN=MosserPowerShellTestCert
发布, 该文件对于您的系统是不可信的. 请只运行来自可信发布者的脚本. 
[V] 从不运行(V)  [D] 不运行(D)  [R] 运行一次(R)  [A] 始终运行(A)  [?] 帮助 (默认值为“D”): a
我的第一个签名脚本

#第二次执行, 不会询问
我的第一个签名脚本

```
