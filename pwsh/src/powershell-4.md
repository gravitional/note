# learn.powershell.4.md

For myself and for you

[收集和分享 Windows PowerShell 相关教程,技术和最新动态](https://www.pstips.net/)
版权归原作者所有

## 命令发现和脚本块

### Powershell 发现命令

从用户的角度来看, 在`Powershell`控制台上输入一条命令, 然后直接回车执行, 是一件简单的事情,
事实上`Powershell`在后台做了很多事情,
其中第一步, 就是查看用户输入的命令是否可用,这个步骤也被称作自动化发现命令.
使用`Get-Command`命令可以查看当前作用域支持的所有命令.
如果你想查看关于 `LS` 命令的信息, 请把它传递给 `Get-Command` .

```powershell
PS C:> Get-command LS

CommandType Name Definition
----------- ---- ----------
Alias       ls   Get-ChildItem
```

如果你想查看更加详细的信息可以使用:

```powershell
PS C:> Get-Command ls | fl *
HelpUri             : http://go.microsoft.com/fwlink/?LinkID=113308
ResolvedCommandName : Get-ChildItem
```

如果你想查看命令`IPConfig`的命令信息, 可以使用:

```powershell
PS C:> get-command ipconfig

CommandType Name         Definition
----------- ----         ----------
Application ipconfig.exe C:windowsSYSTEM32ipconfig.exe
```

事实上, `Get-Command` 返回的是一个对象`CommandInfo`, `ApplicationInfo`, `FunctionInfo`, 或者`CmdletInfo`;

```powershell
PS C:> $info = Get-Command ping
PS C:> $info.GetType().fullname
System.Management.Automation.ApplicationInfo
PS C:> $info = Get-Command ls
PS C:> $info.GetType().fullname
System.Management.Automation.AliasInfo
PS C:> $info = Get-Command Get-Command
PS C:> $info.GetType().fullname
System.Management.Automation.CmdletInfo
PS C:> $info=Get-Command more | select -First 1
PS C:> $info.GetType().fullname
System.Management.Automation.FunctionInfo
```

如果一条命令可能指向两个实体, `get-command`也会返回, 例如`more`.

```powershell
PS C:> Get-Command more

CommandType Name     Definition
----------- ----     ----------
Function    more     param([string[]]$paths)...
Application more.com C:windowsSYSTEM32more.com
```

这两条命令, 前者是`Powershell`的自定义函数, 后者是扩展的`Application`命令.

细心的读者可能会提问, 这两个会不会发生冲突. 当然不会, 默认会调用第一个,
是不是仅仅因为它排在第一个, 不是, 而是在`Powershell`中有一个机制, 就是函数永远处在最高的优先级.
不信, 看看下面的例子,
通过函数可以重写`ipconfig`, 一旦删除该函数, 原始的`ipconfig`才会重新登上历史的舞台:

```powershell
PS C:> function Get-Command () {}
PS C:> Get-Command
PS C:> del Function:Get-Command
PS C:> ipconfnig
PS C:> ipconfig.exe

Windows IP 配置

无线局域网适配器 无线网络连接 3:

   媒体状态  . . . . . . . . . . . . : 媒体已断开
   连接特定的 DNS 后缀 . . . . . . . :
...
```

### Powershell 调用操作符

调用操作符"`&`"虽然简短, 但是给我们执行`Powershell`命令提供了很大的方便.
如果你之前将`Powershell`命令存储在了一个字符串中, 或者一个变量中.
此时, 调用操作符就可以将字符串直接解释成命令并执行,
如果在`Powershell`控制台中, 你只须要输入即可.
具体, 如下:

```powershell
#将命令存储在变量中:
$command = "Dir"
# 如果直接输出变量, 字符串原样输出.
$command

#Dir

#如果使用调用操作符"&"
& $command

    目录: E:

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
d----          2012/5/9      0:42            Blog
...
```

#### 调用操作符只能接受单个命令

但是调用操作符不能接受全部的`Powershell`脚本或命令, 只能接受单个的一条命令, 例如使用:

```powershell
$command = "Dir $env:windir"
& $command

无法将"Dir C:windows"项识别为 cmdlet, 函数, 脚本文件或可运行程序的名称...
```

为什么会这样呢?
追根溯源, `Powershell`中的调用符, 首先会使用`get-command`去发现命令是否可用,
而`get-command`的确只支持单独的一条命令, 不支持命令串或者脚本串.

#### 调用操作符执行`CommandInfo`对象

调用操作符初始化时会将指定的文本传递给`get-command`, 然后由`get-command`去检索命令,
事实上, 调用操作符甚至可以直接执行一个`CommandInfo`对象, 绕过自身的内部`get-command`, 例如:

```powershell
PS E:> $command=Get-Command tasklist
PS E:> $command

CommandType     Name
-----------     ----
Application     tasklist.exe

PS E:> & $command

映像名称                       PID 会话名              会话#       内存使用
========================= ======== ================ =========== ============
System Idle Process              0 Services                   0         24 K
System                           4 Services                   0        560 K
```

#### 通过命令名称唯一标识一条命令

可能存在别名, 命令, 函数的的名称,
那Powershell会不会纠结到底执行哪个呢? 当然不会, 因为它们之间是有一个优先级的.
从高到底, 依次为:

1. Alias(1)
1. Function(2)
1. Filter(2)
1. Cmdlet(3)
1. Application(4)
1. ExternalScript(5)
1. Script (-)

下面举个例子:

```powershell
PS E:> function Ping(){"我是Ping函数"}
PS E:> Set-Alias -Name Ping -Value echo

CommandType Name     Definition
----------- ----     ----------
Alias       Ping     echo
Function    Ping     param()...
Application PING.EXE C:windowsSYSTEM32PING.EXE

PS E:> ping "测试我的Ping,估计执行的别名echo"
测试我的Ping,估计执行的别名echo

PS E:> del Alias:Ping
PS E:> ping ; #别名已经被删除, 估计执行的是函数PING
我是Ping函数

PS E:> del Function:Ping
PS E:> ping baidu.com ; #函数已经被删除, 估计执行的是函数ping 程序

正在 Ping baidu.com [220.181.111.85] 具有 32 字节的数据:
...
```

那怎样突破优先级的限制执行指定的命令呢?
方法都是大同小异, 通过特定信息过滤到指定的`CommandInfo`对象, 然后直接使用我们本篇的调用操作符.
例如当存在如下的命令`Ping`命令

```powershell
CommandType     Name
-----------     ----
Alias           Ping
Function        Ping
Application     PING.EXE
```

我想调用第三个, 可以使用:

```powershell
PS E:> $command=Get-Command -Name ping | where {$_.CommandType -eq "Application" }
PS E:> & $command

用法: ping [-t] [-a] [-n count] [-l size] [-f] [-i TTL] [-v TOS]
...
#或者& (Get-Command -Name ping)[2]
```

### Powershell 语句块

脚本块是一种特殊的命令模式. 一个脚本块可以包含许多的 `Powershell`命令和语句.
它通常使用大括号定义. 最小最短的脚本块, 可能就是一对大括号, 中间什么也没有.
可以使用之前的调用操作符"`&`"执行脚本块:

```powershell
PS E:> & {"当前时间:" + (get-date) }
当前时间:08/08/2012 22:30:24
```

#### 将命令行作为整体执行

可能你已经意识到, 在`Powershell`中,调用操作符不但可以执行一条单独的命令, 还可以执行`命令行`.
最方便的方式就是讲你的命令行放在一个语句块中, 作为整体.
在之前的文章中说过, 调用操作符只能执行一条命令,
但是借助语句块的这把利器, 可以让调用操作符执行, 多条`Powershell`命令, 例如:

```powershell
PS E:> & {$files=ls;Write-Host "文件数: " $files.Count }
文件数:  29
```

#### 执行表达式

另外还有一条`Powershell` cmdlet, `Invoke-Expression`,
这条命令的逻辑就是将一条字符串传递给调用操作符. 例如:

```powershell
PS E:> Invoke-Expression 'Get-Process | Where-Object { $_.Name -like "e*"}'

Handles  NPM(K)    PM(K)      WS(K) VM(M)   CPU(s)     Id ProcessName
-------  ------    -----      ----- -----   ------     -- -----------
    332      29    12280      24264   154     1.40   3236 egui
...
```

这里有一点需要注意, 在传递给`invoke-expression`的字符串使用了单引号, 单引号可以防止变量被替换.
如果上面的命令使用了双引号, 会先去解释`$_.name`, 但是当前作用域中, `$_.Name` 为`null`, 所以结果不是期望的.

#### 管道中的foreach-object语句块

管道中的`foreach-object`本身后面也会带语句块, 针对数组中的每一个元素分别传递给语句块处理.
 例如:

```powershell
Get-Process | ForEach-Object { $_.name }
```

#### 条件和循环中的语句块

在条件语句中, 如果条件满足, 做一件事, 条件不满足做另外一件事,
这一件事或者另外一件事本身应当是一个整体, 尽管本身可能包含了许多命令和语句.
所以会把它们放在一个语句块中.

在循环语句中, 如果条件满足循环做一件事, 这一件事本身, 也是一个整体.
这里就不举例了.

#### 函数本身是一个已命名的语句块

为什么把函数和语句块归结在一起呢? 请看下面的例子.

```powershell
#定义一个函数
Function SayHello([string]$people="everyone")
{
   write-host "Hello, $people "
}

#通过函数调用
SayHello "Mosser"
Hello, Mosser

#通过语句块调用

$scriptblocks

param([string]$people="everyone")
write-host "Hello, $people "

& $scriptblocks
Hello, everyone
```

#### 构建语句块

既然函数只是被命令的语句块, 那是不是也可以通过语句块就能实现函数的特性呢?
接下来就验证一下吧.

#### 传递参数给语句块

在函数中可以传递参数:

```powershell
Function SayHello([string]$people="everyone")
{
   write-host "Hello, $people "
}
```

能否在语句块中也传递参数?

```powershell
& { param($people="everyone") write-host "Hello, $people " } "Mosser"
Hello, Mosser
```

定义和传递参数一次性完成, 有点匿名函数的味道.

`Begin, Process, End` 管道语句块

之前讲过定义函数也可以按照`Begin, Process, End`的结构, 这样尤其可以实时处理管道数据.
那能不能也直接通过语句块定义呢?

```powershell
get-process | select -last 5 | & {
begin {
"开始准备环境"
}
process
{
 $_.Name
}
end {
"开始清理环境"
}
}
#输出结果为:
开始准备环境
wlcommsvc
WLIDSVC
WLIDSVCM
WmiPrvSE
XDict
开始清理环境
```

#### 验证变量

函数中的所有变量都是内置的, 属于函数定义域. 除非你指定给一个全局变量赋值. 首先通过函数实现:

```powershell
function Test
{
$value1 = 10
$global:value2 = 20
}
Test
$value1
$value2

20
```

那语句块也支持吗? 例如:

```powershell
& { $value1 = 10; $global:value2 = 20 }
$value1
$value2

20
```

通过语句块可以实现函数的3个特性, 再一次印证了函数确实是命名的语句块.

### Powershell 执行上下文

`Powershell` 提供了一个非常特别的自动化变量, `$ExecutionContext` .
这个变量可能会很少碰到, 但是理解它的机制, 有助于我们理解`Powershell`执行命令和脚本的内部机制.
这个对象主要包含两个属性: `InvokeCommand` 和 `SessionState`.

```powershell
PS E:> $ExecutionContext

Host           : System.Management.Automation.Internal.Host.InternalHost
Events         : System.Management.Automation.PSLocalEventManager
InvokeProvider : System.Management.Automation.ProviderIntrinsics
SessionState   : System.Management.Automation.SessionState
InvokeCommand  : System.Management.Automation.CommandInvocationIntrinsics
```

#### InvokeCommand

到目前为止,我们在`Powershell`控制台中遇到三个比较特殊的字符,
字符串标识`""`, 调用操作符 `&`, 和脚本块标识`{}`.

|特殊字符 |定义 |内部方法|
|---------|---------|---------|
|`"` |处理字符串中的变量 |`ExpandString()`|
|`&` |执行命令集 |`InvokeScript()`|
|`{}`| 创建一个新的代码块 |`NewScriptBlock()`|

#### 处理变量

每当你在`Powershell`的字符串中放置一个变量,
`Powershell`解释器会自动处理该变量, 并将变量替换成变量本身的值或者内容

```powershell
$site = '飞苔博客'
# 双引号中的变量会被自动解析成变量的值:
$text = "我的个人网站 $site"
$text
```

输出:

```powershell
我的个人网站 飞苔博客
```

既然双引号的机制是`ExpandString()`方法,那么也可以自己调用该方法

```powershell
$site = '飞苔博客'
# 双引号中的变量会被自动解析成变量的值:
$text = '我的个人网站 $site'
$text
#通过ExpandString()自动处理字符串中的变量
$executioncontext.InvokeCommand.ExpandString($text)
```

输出:

```powershell
我的个人网站 $site
我的个人网站 飞苔博客
```

#### 创建脚本块

如果将`Powershell`代码放置在花括号中,这样既可以使用调用操作符`&`执行脚本,
也可以将脚本块赋值给一个函数,因为之前的文章中说过,**函数是一个命令的脚本块**.

```powershell
# 创建新的脚本块
$block = {
$write=Get-Process WindowsLiveWriter
"$($write.Name) 占用内存: $($write.WorkingSet/1mb) MB"
}
$block.GetType().Name
& $block

# 使用NewScriptBlock方法创建脚本块:
$blockStr='$write=Get-Process WindowsLiveWriter
"$($write.Name) 占用内存: $($write.WorkingSet/1mb) MB"'
$block = $executioncontext.InvokeCommand.NewScriptBlock($blockStr)
$block.GetType().Name
& $block
```

输出:

```powershell
ScriptBlock
WindowsLiveWriter 占用内存: 150.734375 MB
ScriptBlock
WindowsLiveWriter 占用内存: 150.734375 MB
```

#### 执行命令行

输入的命令行可以通过`InvokeScript()`脚本执行,也可以使用`&`执行,
也可以使用`Invoke-Expression`命令执行

```powershell
$cmd='3*3*3.14'
& { 3*3*3.14}
$executioncontext.InvokeCommand.InvokeScript($cmd)
Invoke-Expression $cmd
```

输出:

```powershell
28.26
28.26
28.26
```

#### SessionState

`SessionState`是一个用来表现`Powershell`环境的对象,
你同样可以通过自动化变量`$ExecutionContext`访问这些信息.

```powershell
PS E:> $executioncontext.SessionState | Format-List *

Drive : System.Management.Automation.DriveManagementIntrinsics
Provider :
...
```

`PSVariable`,可以取出和更新`Powershell`中所有的变量.

```powershell
$value = "Test"
# Retrieve variable contents:
$executioncontext.SessionState.PSVariable.GetValue("value")
Test
# Modify variable contents:
$executioncontext.SessionState.PSVariable.Set("value", 100)
$value
100
```

输出:

```powershell
Powershell博客 飞苔博客
Powershell博客 网站http://www.mossfly.com
```

##### 管理驱动器

查看当前驱动器信息

```powershell
PS E:> $executioncontext.SessionState.Drive.Current

Name Used (GB) Free (GB) Provider   Root CurrentLocation
---- --------- --------- --------   ---- ---------------
E         1.67     78.33 FileSystem E:
```

查看所有驱动器信息

```powershell
PS E:> $executioncontext.SessionState.Drive.GetAll() | ft -

Name     Used (GB) Free (GB) Provider    Root
----     --------- --------- --------    ----
WSMan                        WSMan
Alias                        Alias
Env                          Environment
C            44.48     35.52 FileSystem  C:
```

如果你的只想关注特定的驱动器,可以使用下面的方法:

```powershell
PS E:> $executioncontext.SessionState.Drive.GetAllForProvider("FileSystem")

Name Used (GB) Free (GB) Provider   Root CurrentLocation
---- --------- --------- --------   ---- ---------------
C        44.48     35.52 FileSystem C:    Usersbaozhen
```

##### 路径操作

`SessionState`的`Path`包含几个特殊的方法,基本可以覆盖各种常用的路径操作了

|方法 |描述 |对应的命令|
|----- |------ |-----|
|`CurrentLocation` |当前路径 |`Get-Location`|
| `PopLocation()` |获取存储的路径 |`Pop-Location`|
|`PushCurrentLocation()`| 存储路径| |`Push-Location`|
|`SetLocation()` |定位路径 | `Set-Location`|
|`GetResolvedPSPathFromPSPath()` |相对路径转换成绝对路径 | `Resolve-Location`|

## PowerShell文件系统

### 文件系统前言

在 `PowerShell` 控制台中, 文件系统有很特别的重要性.
一个明显的原因是管理员需要执行许多涉及文件系统的任务.
另一个原因是文件系统是一个层次结构信息模型.
在接下来的章节中, 你还会看到`PowerShell`在此基础上控制其它层次信息系统.
你可以非常容易的将`PowerShell`中学到的驱动器, 目录和文件的知识点应用到其它地方, 其中就包括`注册表`或者微软的`Exchange`.

在下面表格中列出的`PowerShell`命令中, 其全名可能很少有人用到.
大家更倾向与使用它们非常实用的别名, 这些别名来自`Windows`和`Unix`系统.
可以让初学者可以非常快速地找到合适的命令.

| 别名 | 描述 |命令|
| ------ | ------ | ------ |
| `cp, cpi` | 复制文件或者目录 | `Copy-Item` |
| `Dir, ls, gci` | 列出目录的内容 | `Get-Childitem` |
| `type, cat, gc` | 基于文本行来读取内容 | `Get-Content` |
| `gi` | 获取指定的文件或者目录 | `Get-Item` |
| `gp` | 获取文件或目录的属性 | `Get-ItemProperty` |
| `ii` | 使用对应的默认windows程序运行文件或者目录 | `Invoke-Item` |
| `—` | 连接两个路径为一个路径 | `Join-Path` |
| `mi, mv, move` | 移动文件或者目录 | `Move-Item` |
| `ni` |创建新文件或者目录 | `New-Item` |
| `ri, rm, rmdir,del, erase, rd` |删除空目录或者文件| `Remove-Item` |
| `rni,ren` | 重命名文件或者路径 | `Rename-Item` |
| `rvpa` | 处理相对路径或者包含通配符的路径 | `Resolve-Path` |
| `sp` | 设置文件或路径的属性 | `Set-ItemProperty` |
| `Cd,chdir, sl` | 更改当前目录的位置 | `Set-Location` |
| `-` | 提取路径的特定部分, 例如父目录, 驱动器, 文件名 | `Split-Path` |
| `-` | 测试指定的路径是否存在 | `Test-Path` |

#### 访问文件和目录

使用`Get-ChildItem`列出目录的内容.
预定义的别名为`Dir`和`ls`, `Get-ChildItem`执行了一些很重要的任务:

+ 显示目录内容
+ 递归地搜索文件系统查找确定的文件
+ 获取文件和目录的对象
+ 把文件传递给其它命令, 函数或者脚本

注意:
因为`Windows`管理员一般在实践中, 使用`Get-ChildItem`的别名`Dir`, 所以接下来的例子都会使用`Dir`.
另外`ls`(来自`UNIX`家族)也可以代替下面例子中的`Dir`或者`Get-ChildItem`.

#### 列出目录的内容

一般情况下, 你可能只想知道在一个确定的目录中有什么文件, 如果你不指定其它参数.
`Dir`会列出当前目录的内容.
如果你在`Dir`后跟了一个目录, 它的内容也会被列出来,
如果你使用了`-recurse`参数, `Dir`会列出所有子目录的内容. 当然, 也允许使用通配符.

例如, 你想列出当前目录下的所有`PowerShell`脚本, 输入下面的命令:

```powershell
Dir *.ps1
```

`Dir`甚至能支持数组, 能让你一次性列出不同驱动器下的内容.
下面的命令会同时列出`PowerShell`根目录下的`PowerShell`脚本和`Windows`根目录下的所有日志文件.

```powershell
Dir $pshome\*.ps1, $env:windir\*.log
```

如果你只对一个目录下的项目名称感兴趣, 使用`-Name`参数, `Dir`就不会获取对象(`Files`和`directories`), 只会以纯文本的形式返回它们的名称.

```powershell
Dir -name
```

注意:
一些字符在`PowerShell`中有特殊的意义, 比如方括号, 方括号用来访问数组元素的.
这也就是为什么使用文件的名称会引起歧义.
当你使用`-literalPath`参数来指定文件的路径时, 所有的特殊字符被视为路径片段, `PowerShell`解释器也不会处理.

荔非苔注:
`Dir`默认的参数为`-Path`.
假如你当前文件夹下有个文件名为"`.\a[0].txt`", 因为方括号是`PowerShell`中的特殊字符, 会解释器被解析.
为了能正确获取到"`.\a[0].txt`"的文件信息, 此时可以使用`-LiteralPath`参数, 它会把你传进来的值当作纯文本.

```powershell
PS> Get-ChildItem .\a[0].txt
PS> Get-ChildItem -Path .\a[0].txt
PS> Get-ChildItem -LiteralPath .\a[0].txt

    Directory: C:\Users\mosser
Mode                LastWriteTime     Length Name
----                -------------     ------ ----
-a---          2014/1/2     14:04      80370 a[0].txt
```

#### 递归搜索整个文件系统

当你想搜索整个子目录时, 可以使用`-recurce`参数. 但是注意, 下面例子执行时会失败.

```powershell
Dir *.ps1 -recurse
```

你需要了解一点`-recurse`如何工作的细节来理解为什么会发生上面的情况.
`Dir`总是会获取目录中的内容为文件对象或者目录对象.
如果你设置了`-recurse`开关, `Dir`会递归遍历目录对象.
但是你在上面的例子中使用的通配符只获取扩展名为`ps1`的文件, 没有目录, 所以`-recurse`会跳过.
这个概念刚开始使用时可能有点费解, 但是下面的使用通配符例子能够递归遍历子目录, 正好解释了这点.

在这里, `Dir`获取了根目录下所有以字母"`D`"打头的项目.
递归开关起了作用, 那是因为这些项目中就包含了目录.

```powershell
Dir $home\d* -recurse
```

荔非苔注:
原文的作者写这篇文章时, 是基于`PowerShell 2.0`, 在高版本中的`PowerShell`中`Dir *.ps1 -recurse`也是可以工作的.

#### 过滤和排除标准

现在回到刚开始问题, 怎样递归列出同类型的所有文件, 比如所有`PowerShell scripts`.
答案是使用`Dir`完全列出所有目录内容, 同时指定一个过滤条件.
`Dir`现在可以过滤出你想要列出的文件了.

```powershell
Dir $home -filter *.ps1 -recurse
```

除了`-filter`, 还有一个参数乍一看和`-filter`使用起来很像:  `-include`

```powershell
Dir $home -include *.ps1 -recurse
```

你会看到这一戏剧性的变化, `-filter`的执行效率明显高于`-include`:

```powershell
(Measure-Command {Dir $home -filter *.ps1 -recurse}).TotalSeconds
4,6830099
(Measure-Command {Dir $home -include *.ps1 -recurse}).TotalSeconds
28,1017376
```

其原因在于`-include`支持正则表达式, 从内部实现上就更加复杂, 而`-filter`只支持简单的模式匹配.
这也就是为什么你可以使用`-include`进行更加复杂的过滤.
比如下面的例子, 搜索所有第一个字符为`A-F`的脚本文件, 显然已经超出了`-filter`的能力范围.

```powershell
# -filter 查询所有以 "[A-F]"打头的脚本文件, 没找到任何文件
Dir $home -filter [a-f]*.ps1 -recurse
# -include 能够识别正则表达式, 所以可以获取a-f打头, 以.ps1收尾的文件
Dir $home -include [a-f]*.ps1 -recurse
```

When the `Include` parameter is used, the Path parameter needs a trailing asterisk (`*`) wildcard to specify the directory's contents. For example,`dir -Path C:\Test\*`.

If the Recurse parameter is added to the command, the trailing asterisk (`*`) in the Path parameter is optional. The Recurse parameter gets items from the Path directory and its subdirectories. For example, `dir -Path C:\Test\ -Recurse -Include *.txt`

If a trailing asterisk (`*`) is not included in the Path parameter, the **command does not return any output** and returns to the PowerShell prompt. For example,
`dir -Path C:\Test\`.

与`-include`相反的是`-exclude`. 在你想排除特定文件时, 可以使用`-exclude`. 不像`-filter`, `-include`和`-exclude`还支持数组, 能让你获取你的家目录下所有的图片文件.

```powershell
Dir $home -recurse -include *.bmp,*.png,*.jpg, *.gif
```

做到一点即可: 不要混淆了`-filter` 和 `-include`.
选择这两个参数中的其中一个: 当你的过滤条件没有正则表达式时, 使用`-filter`, 可以显著提高效率.

注意:
`filters`无法在`Dir`的结果中, 列出确定大小的文件列表.
因为`Dir`的限制条件只在文件和目录的名称级别. 如果你想使用其它标准来过滤文件, 可以尝试第五章中讲到的`Where-Object`.

下面的例子会获取你家目录下比较大的文件, 指定文件至少要 `100MB` 大小.

```powershell
Dir $home -recurse | Where-Object { $_.length -gt 100MB }
```

如果你想知道`Dir`返回了多少个文件项, `Dir`会将结果保存为一个数组, 你可以通过数组的的`Count`属性来读取.
下面的命令会告诉你你的家目录下有多少图片文件(**这个操作可能会比较耗时**).

#### 获取文件和目录的内容

你可以使用`Dir`直接获取一个单独的文件, 因为`Dir`会返回一个目录下所有的文件和目录对象.
下面的例子会得到这个文件的`FileInfo`信息:

```powershell
$file = Dir C:\a.html
$file | Format-List *
....
```

你可以访问单个文件的属性, 如果它们的属性支持更改, 也可以更改.

```powershell
PS> $file.Attributes
Archive
PS> $file.Mode
-a---
```

`Get-Item`是访问单个文件的另外一个途径, 下面的`3`条命令都会返回同样的结果: 你指定的文件的文件对象.

```powershell
$file = Dir c:\autoexec.bat
$file = Get-Childitem c:\autoexec.bat
$file = Get-Item c:\autoexec.bat
```

但是在访问目录而不是文件时, `Get-Childitem` 和`Get-Item`表现迥异.

```powershell
# Dir 或者 Get-Childitem 获取一个目录下的内容:
$directory = Dir c:\windows
$directory = Get-Childitem c:\windows
$directory
```

```powershell
# Get-Item 获取的是目录对象本身:
$directory = Get-Item c:\windows
$directory
$directory | Format-List *
```

#### 向命令, 函数和文件脚本传递文件

因为`Dir`的结果中返回的是独立的文件或目录对象, `Dir`可以将这些对象直接交付给其它命令或者你自己定义的函数与脚本.
这也使得`Dir`成为了一个非常重要的的选择命令.
使用它你可以非常方便地在一个驱动盘下甚至多个驱动盘下递归查找特定类型的所有文件.

要做到这点, 在管道中使用`Where-Object`来处理`Dir`返回的结果,
然后再使用`ForEach-Object`, 或者你自定义的管道过滤(第九章).

小知识:
你还可以将多个`Dir` 命令执行的结果结合起来. 在下面的例子中, 两个分开的`Dir`命令, 产生两个分开的文件列表.
然后`PowerShell`将它们结合起来发送给管道进行深度处理.
这个例子获取`Windows`目录和安装程序目录下的所有的`dll`文件, 然后返回这些`dll`文件的名称, 版本, 和描述:

```powershell
$list1 = Dir $env:windir\system32\*.dll
$list2 = Dir $env:programfiles -recurse -filter *.dll
$totallist = $list1 + $list2
$totallist | ForEach-Object
   {
   $info =[system.diagnostics.fileversioninfo]::GetVersionInfo($_.FullName);
   "{0,-30} {1,15} {2,-20}" -f $_.Name, `
   $info.ProductVersion, $info.FileDescription
   }
```

因为`Dir`获取的文件和目录是一样的, 有时限制结果中只包含文件或者只包含目录很重要.
有很多途径可以做到这点.
你可以验证返回对象的属性, `PowerShell PSIsContainer`属性, 或者对象的类型.

```powershell
# 只列出目录::
Dir ~ | Where-Object { $_ -is [System.IO.DirectoryInfo] }
Dir ~ | Where-Object { $_.PSIsContainer }
Dir ~ | Where-Object { $_.Mode.Substring(0,1) -eq "d" }
# 只列出文件:
Dir ~ | Where-Object { $_ -is [System.IO.FileInfo] }
Dir ~ | Where-Object { $_.PSIsContainer -eq $false}
Dir ~ | Where-Object { $_.Mode.Substring(0,1) -ne "d" }
```

前面的例子(识别对象类型)是目前速度最快的, 而后面的(文本比较)比较复杂和低效.

`Where-Object`也可以根据其它属性来过滤.

比如下面的例子通过管道过滤`2007年5月12日`后更改过的文件:

```powershell
Dir | Where-Object { $_.CreationTime -gt [datetime]::Parse("May 12, 2007") }
```

也可以使用相对时间获取2周以内更改过的文件:

```powershell
Dir | Where-Object { $_.CreationTime -gt (Get-Date).AddDays(-14) }
```

### 导航文件系统

除非你通过第九章介绍的方式更改了`PowerShell`控制台的提示信息, 否则你工作的当前目录会在控制台的命令行开头显示.
你也可以使用`Get-Location`命令获取当前工作的目录.

如果你想导航到文件系统的另外一个位置, 可以使用`Set-Location`或者它的别名`Cd`:

```powershell
# 进入父目录 (相对路径):
Cd ..
# 进入当前盘的根目录 (相对路径):
Cd \
# 进入指定目录 (绝对路径):
Cd c:\windows
# 从环境变量中获取系统目录 (绝对路径):
Cd $env:windir
# 从普通变量中获取目录 (绝对路径):
Cd $home
```

#### 相对路径和绝对路径

路径的指定可以是相对路径, 也可以是绝对路径.
在上面的最后一个例子中, 兼而有之这两种路径.
相对路径依赖你当前的路径, 比如`.\test.txt`文件总是指定的是当前目录中的`test.txt`文件, 而`..\test.txt`指定的是父目录的`test.txt`文件.
相对路径通常比较实用, 比如你想使用的脚本库位于当前工作目录, 你就可以在不引入其它目录的情况下, 直接工作.
而绝对路径通常具有唯一性, 并且独立于你当前的目录.

用于指定相对路径的四个重要的特殊字符

| 字符 | 意义 | 示例 | 示例描述 |
| ----- | ----- |  ----- |----- |
| `.` | 当前目录 | `Ii .` | 用资源浏览器打开当前目录|
| `..` | 父目录 | `Cd ..` | 切换到父目录|
| `\` | 驱动器根目录 | `Cd \` | 切换到驱动器的顶级根目录|
| `~` | 家目录 | `Cd ~` |  切换到`PowerShell`初始化的目录|

#### 相对路径转换成绝对路径

当你使用相对路径时, `PowerShell`必须将这些相对转换成绝对路径.
在你使用相对路径执行一个文件或者一条命令时, 该转换会自动发生.
你也可以自己使用`Resolve-Path`命令来处理.

```powershell
PS C:\Users\Mosser> Resolve-Path .\a.png

Path
----
C:\Users\Mosser\a.png
```

然而, `Resolve-Path`命令只有在文件确实存在时, 才会有效.
如果你的当前文件夹中没有一个名为`a.png`的文件, `Resolve-Path`将会报错.

如果你指定的路径中包含了通配符, `Resolve-Path`还可以返回多个结果.
下面的命令执行后, 会获取`PowerShell`家目录下面的所有的`ps1xml`文件的名称.

```powershell
PS> Resolve-Path $pshome\*.ps1xml

Path
----
C:\Windows\System32\WindowsPowerShell\v1.0\Certificate.format.ps1xml
...
```

像`Dir`一样, `Resolve-Path`可以在下行函数中扮演选择过滤器的的角色.
下面的例子会演示在记事本中打开一个文件进行处理.
命令调用记事本程序通过`Resolve-Path`打开这个文件.

```powershell
notepad.exe (Resolve-Path  $pshome\types.ps1xml).providerpath
```

如果没有符合标准的文件, `Resolve-Path`会抛出一个异常, 记录在`$?`变量中(第十一章),
在错误发生时表达式`!$?`一直会统计, 在`True`的情况下, 代表可能没找到文件.

如果`Resolve-Path`找到了多个文件会把它保存在一个数组中, 这样会有很多不期望的文件被打开.

下面的函数使用了第六章讲到的`PowerShell`内部的函数`PromptForChoice()`, 来请求用户做出选择.

```powershell
function edit-file([string]$path=$(Throw "请输入相对路径!"))
{
# 处理相对路径, 并抑制错误
$files = Resolve-Path $path -ea SilentlyContinue
# 验证是否有错误产生:
if (!$?)
{
# 如果是, 没有找到符合标准的文件, 给出提醒并停止:
"没有找到符合标准的文件.";
break
}
# 如果返回结果为数组, 表示有多个文件:
if ($files -is [array])
{
# 此种情况下, 列出你想打开的文件:
Write-Host -foregroundColor "Red" -backgroundColor "White" `
"你想打开这些文件吗?"
foreach ($file in $files)
{
"- " + $file.Path
}

# 然后确认这些文件是否为用户想打开的:
$yes = ([System.Management.Automation.Host.ChoiceDescription]"&yes")
$no = ([System.Management.Automation.Host.ChoiceDescription]"&no")
$choices = [System.Management.Automation.Host.ChoiceDescription[]]($yes,$no)
$result = $host.ui.PromptForChoice('Open files','Open these files?',$choices,1)
# 如果用户确认, 使用"&"操作符启动所有的文件
if ($result -eq 0)
{
foreach ($file in $files)
{
& $file
}
}
}
else
{
# 如果是单个文件, 可以直接使用"&"启动:
& $files
}
}
```

#### 保存目录位置

当前的目录可以使用`Push-Location`命令保存到目录堆栈的顶部, 每一个`Push-Location`都可以将新目录添加到堆栈的顶部. 使用`Pop-Location`可以返回.

因此, 如果你要运行一个任务, 不得不离开当前目录, 可以在运行任务前将用`Push-Location`存储当前路径, 然后运行结束后再使用`Pop-Location`返回到当前目录.

`Cd $home`总是会返回到你的家目录, `Push-Location` 和 `Pop-Location`支持堆栈参数.
这使得你可以创建很多堆栈, 比如一个任务, 一个堆栈.

`Push-Location -stack job1`会把当前目录保存到`job1`堆栈中, 而不是标准堆栈中.
当然在你想重新回到这个位置时, 也需要在`Pop-Location`中指定这个参数`-stack job1`.

#### 查找特殊的目录

`Windows`使用了很多特殊的目录, 根据系统的安装, 可能稍有不同.
一些非常重要的目录的路径同时也保存在`Windows`环境变量中, 这样`PowerShell`可以非常方便和清晰的访问它们.
你也可以使用`.NET framework`中的`Environment`类去访问其它特殊目录.

|特殊目录|描述 |示例|
| ----- | ----- | ----- |
| `Application data` | 存储在本地机器上的应用程序数据  | `$env:localappdata` |
| `User profile`  |用户目录 | `$env:userprofile`|
| `Data used incommon` | 应用程序公有数据目录 | `$env:commonprogramfiles` |
| `Public directory` | 所有本地用户的公有目录 | `$env:public` |
| `Program directory` | 具体应用程序安装的目录 | `$env:programfiles` |
| `Roaming Profiles` | 漫游用户的应用程序数据 | `$env:appdata` |
| `Temporary files(private)` | 当前用户的临时目录 | `$env:tmp` |
| `Temporary files` | 公有临时文件目录|  `$env:temp` |
| `Windows directory` | `Windows`系统安装的目录 | `$env:windir` |

环境变量返回的只是其中一部分, 还不是全部的特殊目录.
比如如果你想将某个文件放到一个用户的桌面, 你需要的路径在环境变量中是无法获取的.
但是你可以使用`.NET`的方法`environment`类下面的`GetFolderPath()`方法.
下面会演示如何在桌面上创建一个快捷方式.

```powershell
# 在桌面上创建一个快捷方式:
$path = [Environment]::GetFolderPath("Desktop") + "\EditorStart.lnk"
$comobject = New-Object -comObject WScript.Shell
$link = $comobject.CreateShortcut($path)
$link.targetpath = "notepad.exe"
$link.IconLocation = "notepad.exe,0"
$link.Save()
```

`GetFolderPath()`目录的类型可以在枚举值`SpecialFolder`中找到.
你可以使用下面一行脚本查看它的内容.

```powershell
PS> [System.Environment+SpecialFolder] | Get-Member -static -memberType Property | select -ExpandProperty Name
AdminTools
ApplicationData
...
```

如果你想预览所有`GetFolderPath()`支持的目录内容, 可以使用下面的例子:

```powershell
[System.Environment+SpecialFolder] |
Get-Member -static -memberType Property |
ForEach-Object { "{0,-25}= {1}" -f $_.name, [Environment]::GetFolderPath($_.Name) }

AdminTools               = C:\Users\mosser\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Administrative Tool
ApplicationData          = C:\Users\mosser\AppData\Roaming
...
```

#### 构造路径

路径名称由文本构成, 能让你随心所欲地构造他们.
你也应当看到了上面例子中构造用户桌面快捷方式的过程了:

```powershell
$path = [Environment]::GetFolderPath("Desktop") + "\file.txt"
$path
C:\Users\mosser\Desktop\file.txt
```

一定要确保你的路径中的反斜杠个数正确.
这也就是为什么前面的例子中在`file.txt`前面使用了一个反斜杠.
还有一个更可靠的方式, 就是使用命令 `Join-Path`方法, 或者`.NET`中的`Path`静态类.

```powershell
$path = Join-Path ([Environment]::GetFolderPath("Desktop")) "test.txt"
$path
C:\Users\mosser\Desktop\test.txt

$path = [System.IO.Path]::Combine([Environment]::`
GetFolderPath("Desktop"), "test.txt")
$path
C:\Users\mosser\Desktop\test.txt
```

`Path`类还包含了许多用来合并或者获取目录特定信息的额外方法.
你只需要在下面表格中列出的方法中前加[System.IO.Path]::, 比如:

```powershell
[System.IO.Path]::ChangeExtension("test.txt", "ps1")
test.ps1
```

|方法 |描述 |示例 |
|----- |----- |----- |
|`ChangeExtension()` | 更改文件的扩展名 | `ChangeExtension("test.txt", "ps1")`|
|`Combine()` 拼接路径字符串; 对应Join-Path | `Combine("C:\test", "test.txt")`|
|`GetDirectoryName()` | 返回目录对象: 对应Split-Path -parent | `GetDirectoryName("c:\test\file.txt")`|
|`GetExtension()` | 返回文件扩展名 | `GetExtension("c:\test\file.txt")`|
|`GetFileName()` | 返回文件名: 对应Split-Path -leaf | `GetFileName("c:\test\file.txt")`|
|`GetFileNameWithoutExtension()` |返回不带扩展名的文件名 | `GetFileNameWithoutExtension(|"c:\test\file.txt")`|
|`GetFullPath()` | 返回绝对路径 | `GetFullPath(".\test.txt")`|
|`GetInvalidFileNameChars()` | 返回所有不允许出现在文件名中字符 | `GetInvalidFileNameChars()`|
|`GetInvalidPathChars()` | 返回所有不允许出现在路径中的字符 | `GetInvalidPathChars()`|
|`GetPathRoot()` | 返回根目录: 对应Split-Path -qualifier |`GetPathRoot("c:\test\file.txt")`|
|`GetRandomFileName()` | 返回一个随机的文件名 | `GetRandomFileName()`|
|`GetTempFileName()` | 在临时目录中返回一个临时文件名 | `GetTempFileName()`|
|`GetTempPath()` | 返回临时文件目录 | `GetTempPath()` |
|`HasExtension()` | 如果路径中包含了扩展名, 则返回True |`HasExtension("c:\test\file.txt")`|
|`IsPathRooted()` | 如果是绝对路径, 返回为`True`; `Split-Path -isAbsolute` | `IsPathRooted(|"c:\test\file.txt")`|

### 使用目录和文件工作

`Get-ChildItem` 和 `Get-Item` 命令可以获取已经存在的文件和目录.
你也可以创建自己的文件和目录, 重命名它们, 给它们填充内容, 复制它们, 移动它们, 当然也可以删除它们.

#### 创建新目录

创建一个新目录最方便的方式是使用`MD`函数, 它内部调用的是`New-Item`命令, 指定参数`–type`的值为"`Directory`":

```powershell
# "md"是一个内置的函数用来创建新目录
PS D:\> md Test1

# "New-Item", 也可以做这些, 但是得多花点功夫
PS D:\> New-Item Test2 -type Directory
```

注意:
你也可以一次性创建多层子目录, 如果你指定的目录不存在, `PowerShell`会自动创建这些目录:
`md test\subdirectory\somethingelse`
若`test`和`Subdirectory`目录都不存在, 就会创建三个子目录.

#### 创建新文件

可能之前你已经使用过`New-Item`来创建过文件, 但是它们完全是空的:

```powershell
PS> New-Item "new file.txt" -type File
...
-a---         2014/1/23     19:14          0 new file.txt
```

文件通常会在你保存数据时, 自动被创建.
因为空文件一般没多大用处. 此时重定向和`Out-File`, `Set-Content`这两个命令可以帮助你:

```powershell
Dir > info1.txt
.\info1.txt
Dir | Out-File info2.txt
.\info2.txt
Dir | Set-Content info3.txt
.\info3.txt
Set-Content info4.txt (Get-Date)
.\info4.txt
```

事实证明在操作上`重定向`和`Out-File`非常的类似:

当`PowerShell`转换管道结果时, 文件的内容就像它在控制台上面输出的一样.
`Set-Content`呢, 稍微有所不同, 它在文件中只列出目录中文件的名称列表.
因为在你使用`Set-Content`时, `PowerShell`不会自动将对象转换成文本输入.
相反, `Set-Content`会从对象中抽出一个标准属性. 上面的情况下, 这个属性就是`Name`了.

通常, 你可以将任何文本写入一个文本文件.
最后一行演示的是将一个日期对象写入到文件中.
比如你手动使用`ConvertTo-HTML`将管道结果转换后, `Out-File`和`Set-Content`会殊途同归.

```powershell
Dir | ConvertTo-HTML | Out-File report1.htm
.\report1.htm
Dir | ConvertTo-HTML | Set-Content report2.htm
.\report2.htm
```

如果你想决定对象的那个属性应当显示在`HTML`页面中,
可以使用第5章中提到的`Select-Object`在对象转换成`HTML`前过滤属性.

```powershell
Dir | Select-Object name, length, LastWriteTime |
ConvertTo-HTML | Out-File report.htm
.\report.htm
```

在重定向的过程中, 控制台的编码会自动指定特殊字符在文本中应当如何显示.
你也可以在使用`Out-File`命令时, 使用`-encoding`参数来指定.

如果你想将结果导出为逗号分割符列表, 可以使用`Export-CSV`代替`Out-File`.

你可以使用`双重定向`和`Add-Content`向一个文本文件中追加信息.

`SessionState`的`Path`包含几个特殊的方法

```powershell
Set-Content info.txt "First line"
"Second line" >> info.txt
Add-Content info.txt "Third line"
Get-Content info.txt
First Line
S e c o n d L i n e
Third line
```

这个结果让小伙伴们惊呆了: 双箭头重定向可以工作, 但是文本中显示的字符有间隔.

重定向操作符通常使用的是控制台的字符集, 如果你的文本中碰巧同时包含了`ANSI`和`Unicode`字符集, 可能会引起意外的结果.
相反, 使用`Set-Content`, `Add-Content`和`Out-File`这几条命令, 而不使用重定向,
可以有效地规避前面的风险. 这三条命令都支持`-encoding`参数, 你可以用它来选择字符集.

#### 创建新驱动器

你可能会惊讶, `PowerShell`允许你创建新的驱动器.
并且不会限制你只创建基于网络的驱动器.
你还可以使用驱动器作为你的文件系统中重要目录, 甚至你自定义的文件系统的一个方便的快捷方式.

使用`New-PSDrive`命令来创建一个新的驱动器. 可以像下面那样创建一个网络驱动器.

```powershell
PS> New-PSDrive -name network -psProvider FileSystem -root \\127.0.0.1\share
...

PS> dir network:
...
```

在工作目录中创建一个快捷方式也非常方便.
下面的命令行会创建一个名为`desktop:` 和 `docs:`的驱动器,
它可以代表你的"`桌面`"目录和"`Windows目录: 我的文档`".

```powershell
New-PSDrive desktop FileSystem `
([Environment]::GetFolderPath("Desktop")) | out-null
New-PSDrive docs FileSystem `
([Environment]::GetFolderPath("MyDocuments")) | out-null
```

然后你想更改当前目录为桌面时, 只须输入:

```powershell
Cd desktop:
```

使用`Remove-PSDrive`来删除你创建的驱动器,如果该驱动器正在使用则不能删除.
注意在使用`New-PSDrive`和`Remove-PSDrive`创建或删除驱动器时, 指定的字母不能包含冒号,
但是在使用驱动器工作时必须指定冒号.

```powershell
Remove-PSDrive desktop
```

读取文本文件的内容

使用`Get-Content`可以获取文本文件的内容:

```powershell
Get-Content $env:windir\windowsupdate.log
```

如果你知道文件的绝对路径, 还可以使用`变量符号`这个快捷方式读取文本内容:

```powershell
${c:\windows\windowsupdate.log}
```

通常, 这个符号不是很实用, 因为在括号中不允许适用任何变量. 而大多数情况下绝对路径不会适用所有机器的操作系统.

`Get-Content` 逐行读取文本的内容, 然后把文本的每一行传递给管道. 因此, 在你想读取一个长文件的前`10`行, 应当适用`Select-Object`:

```powershell
Get-Content $env:windir\windowsupdate.log | Select-Object -first 10
```

使用`Select-String`可以过滤出文本文件中的信息. 下面的命令行会从`windowsupdate.log`文件中过滤出包含"`added update`"短语的行.

```powershell
Get-Content $env:windir\windowsupdate.log | Select-String "Added update"
```

#### 处理逗号分隔的列表

在`PowerShell`中处理逗号分隔的列表文件中的信息时你须要使用`Import-Csv`文件.
为了测试, 先创建一个逗号分隔的文本文件.

```powershell
Set-Content user.txt "Username,Function,Passwordage"
Add-Content user.txt "Tobias,Normal,10"
Add-Content user.txt "Martina,Normal,15"
Add-Content user.txt "Cofi,Administrator,-1"
Get-Content user.txt
------
Username,Function,Passwordage
Tobias,Normal,10
Martina,Normal,15
Cofi,Administrator,-1
```

然后就可以使用`Import-Csv`输入列表文件了,

```powershell
PS> Import-Csv user.txt

Username Function      Passwordage
-------- --------      -----------
Tobias   Normal        10
```

如你所见, `Import-Csv`理解逗号文件的格式, 并且可以逐列显示数据.
所以在解析逗号分割的文本文件时, 你可以节省下很多工作量: `Import-Csv`会替你完成.
第一行被解析成列的标题. 然后你就可以将非常方便地将逗号分隔的值作为输入, 比如创建用户账号.

```powershell
Import-Csv user.txt | ForEach-Object { $_.Username }
Tobias
Martina
Cofi
```

高级主题:
除了使用`ForEach-Object`循环你还可以在括号中使用脚本块.
对于每一个管道内部的管道对象, 脚本块都会被执行.
在下面的例子中, 逗号分割文件中的每一个用户名都会通过`echo`的参数`-InputObject`返回并输出.

```powershell
Import-Csv user.txt | echo -InputObject {$_.Username }
```

### 解析文本内容和提取文本信息

经常会碰到的一个任务就是解析原始数据, 比如日志文件, 从所有的数据中获取结构化的目标信息.
比如日志文件: `windowsupdate.log`.
它记录了`windows`更新的细节信息(在之前的例子中我们已经多次用到过这个小白鼠).
该文件还有大量数据, 以至于乍一看没什么可读性.

初步分析表明该文件是逐行存储的信息, 并且每行的信息片段是以`Tab`字符分割的.

正则表达式为描述这类文件格式提供了最方便的方式, 之前在第13章已经提到过.
你可以按照下面的例子来使用正则表达式适当地描述文件`indowsupdate.log`的内容.

```powershell
# 文本模式包含了6个Tab字符分割的数组
$pattern = "(.*)\t(.*)\t(.*)\t(.*)\t(.*)\t(.*)"
# 输入日志
$text = Get-Content $env:windir\windowsupdate.log
# 从日志文件中提取出任意行(这里是第21行)
$text[20] -match $pattern

True

$matches

Name Value
---- -----
6      * Added update {17A5424C-4C70-4BB4-8F83-66DABE5E7CA2}.201 to search result
...
```

`$matches`返回了每个圆括号中定义的子正则表达式的匹配项, 这样你就可以使用数字索引来寻址每个文本数组元素了.
比如你只对某一行中的日期和描述感兴趣, 然后格式化输出它:

```powershell
PS > "On {0} this took place: {1}" -f $matches[1], $matches[6]

On 2014-02-10 this took place:...
```

这种情况下, 推荐给每一个子表达式取一个名字, 这样可以在后面通过该名字访问.

```powershell
# 这次子表达式拥有一个名称:
$pattern = "(?<Datum>.*)\t(?<time>.*)\t(?<Code1>.*)" + "\t(?<Code2>.*)\t(?<Program>.*)\t(?<Text>.*)"
# 输入日志:
$text = Get-Content $env:windir\windowsupdate.log

# 从日志中提取任意行来解析(这里取第21行):
$text[20] -match $pattern
True
# 从 $matches 中获取信息
# 可以访问指定的名称:
$matches.time + $matches.text

11:30:42:237  * Added update ...
```

现在你可以使用`Get-Content`一行一行读取整个日志文件了, 然后使用上面的方式逐行处理.
这意味着即使在一个庞大的文件中, 你也可以快速, 相对高效地收集所有你需要的信息.
下面的例子正好会列出那些日志行的描述信息中包含了短语"`woken up`"的文本行.
这可以帮助你找出一台机器是否曾经因为自动更新被从待机或者休眠模式唤醒.

```powershell
Get-Content $env:windir\windowsupdate.log |
ForEach-Object { if ($_ -match "woken up") { $_ } }

2013-05-24 03:00:34:609 1276 1490 AU The machine was woken up by Windows Update
2013-05-24 03:00:34:609 1276 1490 AU The system was woken up by Windows Update, but found to be running on battery power. Skip the forcedinstall.
2013-06-28 03:00:11:563 1272 fe0 AU The machine was woken up by Windows Update
```

如果进入循环, 会将保存在`$_`中的完整文本行输出.
你现在知道了如何使用正则表达式将一个包含特定信息片段的文本行分割成数组.

然而, 还有第二种, 更为精妙的方法, 从文件中选择个别文本行, 它就是`Switch`.
你只需要告诉语句块, 哪个文件你想检查, 那个模式你想匹配, 剩下的工作就交给`Switch`吧!
下面的语句会获取所有安装的自动更新日志,使用它比之前使用的`Get-Content`和`ForEach-Object`更快速.

你只需要记住正则表达式"`.*`"代表任意数量的任意字符.

```powershell
Switch -regex -file $env:windir\wu1.log {
'START.*Agent: Install.*AutomaticUpdates' { $_ }}

2013-05-19 09:22:04:113 1248 1d0c Agent **START**
Agent: Installing updates [CallerId = AutomaticUpdates]
2013-05-24 22:31:51:046 1276 c38 Agent **START**
Agent: Installing updates [CallerId = AutomaticUpdates]
2013-06-13 12:05:44:366 1252 228c Agent **START**
Agent: Installing updates [CallerId = AutomaticUpdates]
```

如果你想找到其它程序的更新, 比如`SMS`或者`Defender`.
只需要在你的正则表达式中使用"`SMS`"或者"`Defender`"替换"`automatic updates`"即可.

事实上, `Switch`可以接受多个模式, 按照下面声明在花括号中的那样, 依赖多个模式进行匹配.
这就意味着只需几行代码, 就可以找出多个程序的更新.

```powershell
# 为结果创建一个哈希表:
result = @{Defender=0; AutoUpdate=0; SMS=0}
# 解析更新日志, 并将结果保存在哈希表中:
Switch -regex -file $env:windir\wu1.log
{
'START.*Agent: Install.*Defender' { $result.Defender += 1 };
'START.*Agent: Install.*AutomaticUpdates' { $result.AutoUpdate +=1 };
'START.*Agent: Install.*SMS' { $result.SMS += 1}
}

# 输出结果:
$result

Name     Value
----     -----
SMS      0
Defender 1
AutoUpdate  8
```

#### 读取二进制的内容

不是所有的文件都包含文本. 有时, 我们需要读取二进制文件中的信息.
正常情况下一个文件的扩展名扮演的很重要的角色,因为它决定了`Windows`使用什么程序来打开这个文件.

然而在许多二进制文件中, 文件头也紧密的集成到文件中,这些文件头包含了该文件是属于哪一类文件的内部类型名称.
借助于参数`-readCount`和`-totalCount`, `Get-Content`可以获取这些"魔法字节".

参数`-readCount`指明每次读取多少字节, `-totalCount`决定了你想从文件中读取的总的字节数.

当前情况下, 你需要从文件中读取的应当是**前4个字节**.

```powershell
function Get-MagicNumber ($path)
{
Resolve-Path $path | ForEach-Object {
$magicnumber = Get-Content -encoding byte $_ -read 4 -total 4
$hex1 = ("{0:x}" -f ($magicnumber[0] * `
256 + $magicnumber[1])).PadLeft(4, "0")
$hex2 = ("{0:x}" -f ($magicnumber[2] * `
256 + $magicnumber[3])).PadLeft(4, "0")
[string] $chars = $magicnumber| %{ if ([char]::IsLetterOrDigit($_))
{ [char] $_ } else { "." }}
"{0} {1} '{2}'" -f $hex1, $hex2, $chars
}
}
```

results

```powershell
Get-MagicNumber "$env:windir\explorer.exe"
4d5a 9000 'M Z . .'
```

`Explorer`的前四个字节为`4d, 5a, 90`, 和 `00` 或者已经列出的文本`MZ`.
这是`Microsoft DOS`的开发者之一`Mark Zbikowski`的简称.
所以, 标记`MZ`就代表了可执行的程序. 这个标记和图片文件的标记不同:

```powershell
PS> Get-MagicNumber "$env:windir\Web\Wallpaper\Scenes\*"
ffd8 ffe0 'ÿ Ø ÿ à'
ffd8 ffe0 'ÿ Ø ÿ à'
....
```

如你所见, `Get-Content`也可以读取二进制文件, 一次只读一个字节.
参数`-readCount`指定每一步读取多少个字节.
`-totalCount`指定总共要读取的字节数, 一旦给它赋值为-1, 它会从头到尾读取所有文件内容.
你可以通过将数据输出为十六进制来预览可执行文件. 因为纯二进制文本不易阅读.

```powershell
function Get-HexDump($path,$width=10, $bytes=-1)
{
$OFS=""
Get-Content -encoding byte $path -readCount $width -totalCount $bytes | ForEach-Object {
$characters = $_
if (($characters -eq 0).count -ne $width)
{
$hex = $characters | ForEach-Object {
" " + ("{0:x}" -f $_).PadLeft(2,"0")}
$char = $characters | ForEach-Object {
if ([char]::IsLetterOrDigit($_))
{ [char] $_ } else { "." }}
"$hex $char"
}
}
}
```

results

```powershell
PS> Get-HexDump $env:windir\explorer.exe -width 15 -bytes 150

 4d 5a 90 00 03 00 00 00 04 00 00 00 ff ff 00 MZ..........ÿÿ.
 ...
```

#### 移动和复制文件和目录

`Move-Item` 和 `Copy-Item`用来执行移动和拷贝操作.
它们也支持通配符. 比如下面的脚本会将你家目录下的的所有`PowerShell`脚本文件复制到桌面上:

```powershell
Copy-Item $home\*.ps1 ([Environment]::GetFolderPath("Desktop"))
```

但是, 只有在家目录当下的脚本会被复制.
幸亏`Copy-Item`还有一个参数`-recurse`, 这个参数的效果类似`Dir`中的效果.
如果你的初始化目录不包含任何目录, 它也不会工作.

```powershell
Copy-Item -recurse $home\*.ps1 ([Environment]::GetFolderPath("Desktop"))
```

使用`Dir`也可以复制所有`PowerShell`脚本到你的桌面, 让我们先找出这些脚本, 然后将结果传递给`Copy-Item`:

```powershell
Dir -filter *.ps1 -recurse | ForEach-Object {
Copy-Item $_.FullName ([Environment]::GetFolderPath("Desktop")) }
```

小技巧: 你可能被诱惑去缩减脚本行, 因为文件对象整合了一个`CopyTo()`方法.

```powershell
Dir -filter *.ps1 -recurse | ForEach-Object {
$_.CopyTo([Environment]::GetFolderPath("Desktop")) }
```

但是结果可能会出错, 因为`CopyTo()`是一个低级的函数. 它需要文件的目标路径也被复制.
因为你只是想复制所有文件到桌面, 你已经指定了目标路径的目录. `CopyTo()`会尝试将文件复制这个精确的字符串路径(桌面)下, 但是肯定不会得逞, 因为桌面是一个已经存在的目录了.
相反的`Copy-Item`就聪明多了: 如果目标路径是一个目录, 它就会把文件复制到这个目录下.

此时, 你的桌面上可能已经堆满了`PowerShell`脚本, 最好的方式是将它们保存到桌面的一个子目录中.
你需要在桌面上创建一个新目录, 然后从桌面到这个子目录中移动所有的脚本.

```powershell
$desktop = [Environment]::GetFolderPath("Desktop")
md ($desktop + "\PS Scripts")
Move-Item ($desktop + "\*.ps1") ($desktop + "\PS Scripts")
```

此时, 你的桌面又恢复了往日的整洁, 也把脚本安全的保存到桌面了.

#### 重命名文件和目录

使用`Rename-Item`你可以给文件或者目录换个名字.
但是这样做时要格外小心, 因为如果把某些系统文件给重命名了, 可能会导致系统瘫痪.
甚至你只是更改了某些文件的扩展名, 也会导致它们不能正常打开或者显示它们的一些属性.

```powershell
Set-Content testfile.txt "Hello,this,is,an,enumeration"
# 在默认编辑器中打开文件:
.\testfile.txt
# 在Excel中打开文件:
Rename-Item testfile.txt testfile.csv
.\testfile.csv
```

#### 批量重命名

因为`Rename-Item`可以在管道中的语句块中使用, 这就给一些复杂的任务提供了令人惊讶的方便的解决方案.
比如, 你想将一个目录的名称和它的子目录的名称, 包括目录下的文件的名称中所有的"`x86`"词语移除掉. 下面的命令就够了:

```powershell
Dir | ForEach-Object {
Rename-Item $_.Name $_.Name.replace("-x86", "") }

```

然而, 上面的命令会实际上会尝试重命名所有的文件和目录, 即使你找的这个词语在文件名中不存在,产生错误并且非常耗时.
为了大大提高速度, 可是使用`Where-Object`先对文件名进行过滤, 然后对符合条件的文件进行重命名, 可以将速度增长`50`倍: (荔非苔注: 为什么是`50`倍呢? 我不知道. )

```powershell
Dir | Where-Object { $_.Name -contains "-x86" } | ForEach-Object {
Rename-Item $_.Name $_.Name.replace("-x86", "") }
```

#### 更改文件扩展名

如果你想更改文件的扩展名, 首先需要意识到后果:
文件随后会识别为其它文件类型, 而且可能被错误的应用程序打开, 甚至不能被任何应用程序打开.

下面的命令会把当前文件夹下的所有的`PowerShell`脚本的后缀名从"`.ps1`"改为"`.bak`".

```powershell
Dir *.ps1 | ForEach-Object { Rename-Item $_.Name `
([System.IO.Path]::GetFileNameWithoutExtension($_.FullName) + `
".bak") -whatIf }
What if: Performing operation "Rename file" on Target
"Element: C:\Users\Tobias Weltner\tabexpansion.ps1
Destination: C:\Users\Tobias Weltner\tabexpansion.bak".
```

由于`-whatIf`参数的缘故, 一开始语句只会表明可能会执行重命名操作.

#### 整理文件名

数据集往往随着时间的增长而增长.
如果你想整理一个目录, 你可以给定所有的文件一个统一的名称和序号.
你可以从文件的某些具体的属性中合成文件名.

还记得上面在桌面上为`PowerShell`脚本创建的那个子目录吗?
让我们对它里面的`PowerShell`脚本以数字序号重命名吧.

```powershell
Dir $directory\*.ps1 | ForEach-Object {$x=0} {
Rename-Item $_ ("Script " + $x + ".ps1"); $x++ } {"Finished!"}
Dir $directory\*.ps1
```

#### 删除文件和目录

使用`Remove-Item`和别名`Del`可以删除文件和目录, 它会不可恢复的删除文件和目录.
如果一个文件属于只读文件, 你需要指定参数`-force` :

```powershell
# 创建示例文件:
$file = New-Item testfile.txt -type file
# 文件不是只读:
$file.isReadOnly
False
# 激活只读属性:
$file.isReadOnly = $true
$file.isReadOnly
True
# 只读的文件需要指定-Force参数才能顺利删除:
del testfile.txt
Remove-Item : Cannot remove item C:\Users\Tobias Weltner\testfile.txt: Not enough permission to perform operation.
At line:1 char:4
+ del <<<< testfile.txt
del testfile.txt -force
Table
```

#### 删除目录内容

如果你只想删除某个目录下的内容而保留目录本身, 可以使用通配符.

比如下面的脚本行会删除`Recent`目录下的内容, 对应于启动菜单中的"`My Recent Documents`".
因为删除文件夹是一件掉以轻心就会产生严重后果的事情,
所有你可以使用`-whatIf`参数模拟一下删除过程, 看看可能会发生什么.

```powershell
$recents = [Environment]::GetFolderPath("Recent")
del $recents\*.* -whatIf
```

如果你已经确认你的命令操作无误, 将上面语句中的`-whatif`去掉即可删除这些文件.
另一方面, 如果你仍然不是很确定, 可以使用`-confirm`, 它会在每次删除操作执行前向你确认.

#### 删除目录和它的内容

如果一个目录被删除了, 它里面所有的内容都会丢失.
在你尝试去删除一个文件夹连同它的内容时, `PowerShell`都会请求你的批准.
这样是为了防止你无意间销毁大量数据. 只有空目录才不需要请求确认信息.

```powershell
# 新建一个测试目录:
md testdirectory

Directory: Microsoft.PowerShell.Core\FileSystem::C:\Users\Tobias Weltner\Sources\docs

Mode LastWriteTime Length Name
---- ------------- ------ ----
d---- 13.10.2007 13:31 testdirectory

# 在目录中新建一个文件
Set-Content .\testdirectory\testfile.txt "Hello"

# 删除目录 directory:
del testdirectory

Confirm
The item at "C:\Users\Tobias Weltner\Sources\docs\testdirectory" has children
...
```

但是, 如果你指定了参数`-recurse`,
`PowerShell`会将这个目录连同它里面的内容删除, 没有任何确认提示.

### 管理访问权限

对于`NTFS`驱动器来说, 访问权限决定着哪个用户可以访问文件和目录.

对于每一个文件和文件夹, 所谓的安全描述符(`SD`)规定了安全数据.
安全描述符决定安全设置是否只对当前目录有效, 或者它可以被传递给其它文件和目录.
真正的访问权限是在访问控制列表(`ACL`)中. 每一个访问权限的访问控制项(`ACE`)也在`ACL`中.

注意:
文件和目录访问权限相当于一个复杂的电子锁. 如果使用得当, 你可以把它变成一个有力的安全系统.
然而, 如果使用不当, 你可能很容易把自己锁在外面, 失去了访问重要数据的权限, 或者破坏了`Windows`操作系统(当你无意间禁止了访问关键系统目录的权限后).

作为文件和目录的所有者, 你总是有更正权限的选项;
作为一个管理员, 你也总能取得文件和目录的拥有权.
但这是不得已的后门, 你不能依赖它: 你应当在你能意识到后果的情况下更改权限.
最好一开始使用测试文件和目录做实验.

`PowerShell`使用`Get-Acl`, `Set-Acl` 来管理权限.
此外, 类似`cacls`这样的传统命令也可以在`PowerShell`的控制台上面使用.
通常他们更改起来访问权限会比`PowerShell`命令更快. 尤其在你处理非常多的文件和目录时.
由于`Windows Vista`的发布, `cacls`被视为过时, 如果可能的化, 你可以使用它的继任者`icacls`.

```powershell
PS> icacls /?
```

#### 检查有效的安全设置

文件和目录的有效安全设置在访问控制列表中, 使用`Get-Acl`时, 会获取列表中的内容.
因此如果你想找出谁能够访问某些文件或者目录, 可以这样处理:

```powershell
# 列出Windows目录的权限:
PS> Get-Acl $env:windir

    Directory: C:\

Path    Owner                       Access
----    -----                       ------
Windows NT SERVICE\TrustedInstaller CREATOR OWNER Allow  268435456...
```

#### 确认文件所有者的身份

文件和目录的所有者还有一些特殊的权限.
比如文件的所有者总是能够访问文件. 你可以通过`Owner`属性, 来获取所有者名称.

```powershell
(Get-Acl $env:windir).Owner

    NT SERVICE\TrustedInstaller
```

列出访问权限

实际上访问权限就是 -- 谁可以做什么, 下面输出访问属性:

```powershell
PS> (Get-Acl $env:windir).Access | Format-Table -wrap
```

在输出结果表格的`IdentityReference`列, 告诉你谁有特殊的权限.
`FileSystemRights`列告诉你权限的类型.
`AccessControlType`列格外重要, 如果它显示"拒绝"而不是"允许", 你懂的, 它会限制用户访问.

#### 创建新的权限

`Get-Acl`执行后返回的对象, 包含若干方法可以用来更新权限和设定所有权.

如果你只想设定自己的权限, 都没必要去安全描述符世界深究.
往往, 读取一个已经存在的文件安全描述符, 把它传递给另一个文件, 或者按照特殊`SDDL`语言文字的形式指定安全信息就够了.

技巧:

下面的例子会让你认识一些日常步骤.
注意两点即可: 别忘了`cacls`这个可靠的工具, 因为使用它会比`PowerShell`命令更高效.
此外, `Get-ACL`和`Set-ACL`不仅仅应用于文件层面, 还可以用于其它有访问控制的安全描述符的任何地方, 比如`Windows`注册表(会在下一章讲解).

#### 克隆权限

在一个初级的案例中, 你可能都不会创建任何新的权限,
只会从一个已经存在的文件或者目录的访问控制列表中克隆一个权限, 然后把它转让给其它文件.
优点是可以使用图形用户界面来设置那些通常比较复杂的权限.

注意:
因为手动调整安全设置是一项专业的工作, 各个`Windows`系统不通用(像`Windows XP Home`就没有这个选项).
尽管如此, 你却可以使用`PowerShell`在不同的`Windows`版本中设置权限.

开始之前, 先创建两个目录作为测试:

```powershell
md Prototype | out-null
md Protected | out-null
```

现在, 打开资源管理器, 设置Prototype目录的安全设置.

```powershell
explorer .
```

在资源管理器中, 右击`Prototype`目录, 选择**属性**, 然后点击**安全**选项卡, 点击编辑(**win7**和**win8**中).
通过添加其他用户来更改测试目录的安全设置.
在下面的对话框中给新用户设置权限.

注意:
你也可以通过勾选拒绝复选框来拒绝用户的权限. 这样做时, 可要留心了. 因为限制权限总是有高优先级.
比如, 你给了自己完全控制的权限, 但是拒绝了"`Everyone`"这个组来访问,这样就把自己关在文件系统的外面了.
因为你也属于"`Everyone`"这个组, 同时因为限制的优先级比较高,
哪怕你已经给了自己"完全控制"的权限, 这个限制也作用于你.

你更改了权限后, 捎带在资源管理器中看看第二个目录`Protected`.
这个目录仍旧是默认赋予的权限. 下一步, 我们会把`Prototype`刚才设置的权限转交到`Protected`目录.

```powershell
$acl = Get-Acl Prototype
Set-Acl Protected $acl
```

注意:
你本身需要特殊的权限去设置上面的权限. 如果你用的是`Windows Vista`操作系统, 并且启用了`UAC`, 使用`PowerShell`操作时, 会出现错误, 提示你没有权限.
这时可以通过让控制台以管理员权限运行来获取权限.

实验做完了, 现在呢, `Protected`和`Prototype`一样安全.
当你在资源管理器中查看它们的安全设置时, 你会发现所有的设置都是相同的.

#### 使用`SDDL`设置权限

前面的例子非常简单, 你所做的只是把已有目录的安全设置移交给其它目录.
但是你需要一个平时用不着的`Prototype`目录.

但是你可以通过文本格式的安全描述符来归纳安全设置.
每一个安全设置都是被特殊的**安全描述符描述语言**(`SDDL`)定义的.
它能让你以文本的形式读取`Prototype`目录的安全信息, 以后无须借助`Prototype`目录即可使用.

让我们删掉这个测试目录`Protected`吧, 然后在`SDDL`中保存`Prototype`目录的安全信息.

```powershell
PS> Del Protected
PS> $acl = Get-Acl Prototype
PS> $sddl = $acl.Sddl
PS> $sddl
```

然后把这个`SDDL`文本保存到脚本中, 可将该安全设置赋给任意目录.

```powershell
# 创建新目录
Md Protected
# 在 SDDL中的是安全描述符 (一行):
$sddl="O:S-1-5-21-1771974198-1635825341-3386386210-1001G:S-1-5-21-1771974198-1635825341-3386386210-1001D:AI(A;OICI;0x1301bf;;;S-1-5-3)(A;OICIID;FA;;;SY)(A;OICIID;FA;;;BA)(A;OICIID;FA;;;S-1-5-21-1771974198-1635825341-3386386210-1001)"
# 获取目录的安全描述:
$acl = Get-Acl Protected
# 使用SDDL定义替换安全描述 :
$acl.SetSecurityDescriptorSddlForm($sddl)
# 保存更新
Set-Acl Protected $acl
```

注意:

你的第二个目录是完全独立于`Prototype`目录的.
你所需要做的可能是, 借助`Prototype`目录使用图形用户界面, 临时生成一个`SDDL`安全设置定义.

然而, `SDDL`不能很方便的移交给其它机器.
如果你仔细看下, 每个授权用户不是根据用户名识别, 而是根据它们的安全标识符(`SID`)识别.
不同的机器上, 即使用户名相同, 这个`SID`也不会相同, 因为它们隶属不同的账户.
但是在一个域(`domain`)中, 相同名字的账号的`SID`是相同的, 因为域会集中管理.
其结果就是`SDDL`解决方案在基于域环境的公司网络中非常完美.
尽管如此, 如果你处在一个小型的对等网络中, `SDDL`也能非常有用.
你只需要使用"复制黏贴"去替换`SID`而已.
不过, 在对等网络中, `cacls` 或者`icacls`命令可能更简单一点.

#### 手动创建新权限

权限也可以被手动创建.
其优点就是, 即使没有集中域, 你也可以根据用户名来指定授权用户, 这样可以以相同的方式在任意机器上工作.
但是注意, 它引入了额外的工作, 因为你必须完全创建你自己的安全描述符, 接下来的例子会展示.
但是在实践中发现这个过程非常的耗时.
使用`cacls`和`icacls`都比它简单一点.
现在我们删除掉测试目录`Protected`, 再次创建一个新的目录, 让它只有默认的访问权限.

```powershell
$acl = Get-Acl Protected
# 添加第一个规则:
$person = [System.Security.Principal.NTAccount]"Administrator"
$access = [System.Security.AccessControl.FileSystemRights]"FullControl"
$inheritance = [System.Security.AccessControl.InheritanceFlags] "ObjectInherit,ContainerInherit"
$propagation = [System.Security.AccessControl.PropagationFlags]"None"
$type = [System.Security.AccessControl.AccessControlType]"Allow"
$rule = New-Object System.Security.AccessControl.FileSystemAccessRule( $person,$access,$inheritance,$propagation,$type)
$acl.AddAccessRule($rule)

# 添加第二个规则:
$person = [System.Security.Principal.NTAccount]"Everyone"
$access = [System.Security.AccessControl.FileSystemRights]"ReadAndExecute"
$inheritance = [System.Security.AccessControl.InheritanceFlags] "ObjectInherit,ContainerInherit"
$propagation = [System.Security.AccessControl.PropagationFlags]"None"
$type = [System.Security.AccessControl.AccessControlType]"Allow"
$rule = New-Object System.Security.AccessControl.FileSystemAccessRule( $person,$access,$inheritance,$propagation,$type)
$acl.AddAccessRule($rule)

# 保存权限更新:
Set-Acl Protected $acl
```

接下来, 让我们一起看看每个访问规则是怎么定义的. 每一个规则需要5个细节:

+ `Person`: 这是该规则应当适用的人或者组.
+ `Access`: 这里选择规则要控制的权限.
+ `Inheritance`: 这里选择规则要应用的对象. 这个规则能够, 并且一般是会授予它的子对象, 这样它就能自动适用于目录中的文件了.
+ `Propagation`: 决定权限是否要传递给子对象(比如子目录和文件), 通常情况下设置为`None`, 仅仅授予权限.
+ `Type`:它能让你设置权限或者限制, 如果限制, 指定的权限会明确不予批准.

接下来问题是这些规范允许那些值?
这个例子演示通过`.NET`对象(第六章)显示这些规范. 你可以使用下面的命令列出访问权限允许的值:

```powershell
[System.Enum]::GetNames([System.Security.AccessControl.FileSystemRights])
```

如果你想设置权限时, 实际上得结合上面列表中列出的相关值, 比如:

```powershell
$access = [System.Security.AccessControl.FileSystemRights]::Read `
-bor [System.Security.AccessControl.FileSystemRights]::Write
$access

131209
```

结果是一个数字, 读和写权限的位掩码.

在上面的例子中, 你可以非常简单地获取相同的结果, 因为允许你指定你想要的项目, 甚至把它们放在一个逗号分隔项中, 紧跟在中括号括起来的`.NET`枚举类型后面.

```powershell
$access = [System.Security.AccessControl.FileSystemRights]"Read,Write"
$access
out:
Write, Read

[int]$access
out:
131209
```

因为这里你没有指定二进制计算符`-bor`, 它的结果是可读的文本.
而此时需要位掩码来工作, 所以把它转换成`Integer`整形数据类型.
你可以像这样随时得出设置的相关值.

```powershell
[int][System.Security.AccessControl.InheritanceFlags] `
"ObjectInherit,ContainerInherit"

3
```

这样做的意义在于, 你现在可以测试其它`.NET`枚举类型的值, 把它们转换成整数.
虽然不能增强你的命令的可读性, 但是可以压缩脚本.
因为下面的脚本行和前面例子中的脚本行可以做同一件事.

```powershell
Del Protected
Md Protected
$acl = Get-Acl Protected
$rule = New-Object System.Security.AccessControl.FileSystemAccessRule( "Administrator",2032127,3,0,0)
$acl.AddAccessRule($rule)
$rule = New-Object System.Security.AccessControl.FileSystemAccessRule( "Everyone",131241,3,0,0)
$acl.AddAccessRule($rule)
# 保存更新的权限:
Set-Acl Protected $acl
```

最后, 我们看看`PowerShell`是怎么指定特定用户的权限的.
在上面的例子中, 你指定了用户或者组的名称,
但是权限不能识别用户名, 但能识别账号的唯一`SID`, 用户名在内部会被更改成`SID`,
你也可以在脚本中手动更改用户名, 看看指定的用户名是否存在.

```powershell
$Account = [System.Security.Principal.NTAccount]"Administrators"
$SID = $Account.translate([System.Security.Principal.Securityidentifier])
$SID
out:
BinaryLength   AccountDomainSid  Value
------------   ----------------  -----
16 S-1-5-32-544
```

一个`NTAccount`对象描述了一个权限可以分配的安全主体.
在实践中, 它是用户和组. `NTAccount`对象可以使用`Translate()`来输出它包含的与主体对应的`SID`.
而这只会在指定的账号确实存在的情况下有效.
否则, 你会得到一个错误. 因此你也可以使用`Translate()`来验证一个账号的存在性.

通过`Translate()`获取的`SID`非常有用.
如果你仔细看, 你会发现管理员组的`SID`和你自己当前账号的`SID`完全不同:

```powershell
([System.Security.Principal.NTAccount]"$env:userdomain\$env:username").`
Translate([System.Security.Principal.Securityidentifier]).Value

S-1-5-21-2146773085-903363285-719344707-1282827

([System.Security.Principal.NTAccount]"Administrators").`
Translate([System.Security.Principal.Securityidentifier]).Value

S-1-5-32-544
```

管理员组的`SID`不但很短, 而且是唯一的.
为了整合这个账号, `Windows`使用了所谓的众所周知的`SID`, 它在所有的`Windows`系统中都是相同的. 这一点很重要, 因为你在德文系统中运行上面的脚本会失败.
在德文系统上, `Administrators`组叫做"`Administratoren`", "`Everyone`"组叫做"`Jeder`".
但是这些账号的`SID`是相同的.
知道了这些组的`SID`号, 你就可以使用它们代替那些本地化的名称了.
下面是怎样将`SID`转换成用户账号的名称:

```powershell
$sid = [System.Security.Principal.SecurityIdentifier]"S-1-1-0"
$sid.Translate([System.Security.Principal.NTAccount])
out:
Value
-----
Everyone
```

下面的脚本可以在国际上各种本地化的机器上运行:

```powershell
Del Protected
Md Protected
$acl = Get-Acl Protected

# 管理员完全控制:
$sid = [System.Security.Principal.SecurityIdentifier]"S-1-5-32-544"
$access = [System.Security.AccessControl.FileSystemRights]"FullControl"
$rule = New-Object System.Security.AccessControl.FileSystemAccessRule( `
$sid,$access,3,0,0)
$acl.AddAccessRule($rule)

# 所有用户的只读权限:
$sid = [System.Security.Principal.SecurityIdentifier]"S-1-1-0"
$access = [System.Security.AccessControl.FileSystemRights]"ReadAndExecute"
$rule = New-Object System.Security.AccessControl.FileSystemAccessRule( `
$sid,$access,3,0,0)
$acl.AddAccessRule($rule)

# 保存权限更新:
Set-Acl Protected $acl
```
