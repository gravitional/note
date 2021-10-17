# learn.powershell.1.md

For myself and for you

[收集和分享 Windows PowerShell 相关教程,技术和最新动态](https://www.pstips.net/)
版权归原作者所有

## 使用对象

### 对象=属性+方法

在现实世界中, 你可能已经了解对象就是那些能够摸到的东西. `Powershell`中的对象和现实生活很相似.

例如要在现实生活中描述一把小刀. 我们可能会分两方面描述它.

+ 属性: 一把小刀拥有一些特殊的属性, 比如它的颜色, 制造商, 大小, 刀片数. 这个对象是红色的, 重55克, 有3个刀片, ABC公司生产的. 因此属性描述了一个对象是什么.
+ 方法: 可以使用这个对象做什么, 比如切东西, 当螺丝钉用, 开啤酒盖.  一个对象能干什么就属于这个对象的方法.

#### 创建对象

通过`New-Object`可以创建一个对象, 甚至可以创建一个虚拟的小刀, 但是第一步需要创建一个空对象.
空对象什么都没有, 如果调用它, 不会返回任何东西.

```powershell
PS C:Powershell> $pocketknife=New-Object object
PS C:Powershell> $pocketknife
System.Object
```

#### 增加属性

接下来描述这个对象是什么

```powershell
PS C:Powershell> Add-Member -InputObject $pocketknife -Name Color -Value "Red"-MemberType NoteProperty
PS C:Powershell> $pocketknife

Color
-----
Red

PS C:Powershell> Add-Member -InputObject $pocketknife -Name Weight -Value "55" -MemberType NoteProperty
PS C:Powershell> $pocketknife | Add-Member NoteProperty Blades 3
PS C:Powershell> $pocketknife | Add-Member NoteProperty Manufacturer ABC
PS C:Powershell> $pocketknife

Color Weight Blades Manufacturer
----- ------ ------ ------------
Red   55          3 ABC
```

#### 增加方法

给一个对象增加了属性后, 这个对象就有形状了, 但是它仍然不能做任何事, 要想它做事, 必须给它增加方法.
同样使用`Add-Member`, 不过`-memberType` 选项使用`ScriptMethod`.

```powershell
# 增加一个新方法:
Add-Member -memberType ScriptMethod -In $pocketknife ` -name cut -Value { "I'm whittling now" }
# 指定参数类型增加一个新方法:
Add-Member -in $pocketknife ScriptMethod screw { "Phew...it's in!" }
#直接通过管道增加一个新方法:
$pocketknife | Add-Member ScriptMethod corkscrew { "Pop! Cheers!" }
```

方法添加成功后就可以调用了

```powershell
PS C:Powershell> $pocketknife.cut()
I'm whittling now
PS C:Powershell> $pocketknife.screw()
Phew...it's in!
PS C:Powershell> $pocketknife.corkscrew()
Pop! Cheers!
```

在调用方法时如果没有使用圆括号, 方法不会执行, 但是可以返回方法的基本信息.

```powershell
PS C:Powershell> $pocketknife.corkscrew

Script                      :  "Pop! Cheers!"
OverloadDefinitions : {System.Object corkscrew();}
MemberType           : ScriptMethod
TypeNameOfValue  : System.Object
Value                      : System.Object corkscrew();
Name                     : corkscrew
IsInstance               : True
```

到目前为止一个虚拟的小刀对象就创建完成了, 一个对象包含数据(属性)和动作(方法).

### 属性: 描述对象是什么

属性可以描述一个对象, 对象的属性可以被`Powershell`自动转换成文本, 并且输出到控制台.
因此可以通过这种方法查看任何对象, 例如`$host`:

```powershell
PS C:Powershell> $host

Name             : ConsoleHost
Version          : 2.0
...
```

`InternalHost`对象存储在`$host`变量中, 包含`9`个属性.
输出的第一列为对象的属性, 第二列为文本形式的属性值.
例如要查看当前`Powershell`的版本号, 可以访问`$host`对象的`Version`属性:

```powershell
PS C:Powershell> $host.Version

Major  Minor  Build  Revision
-----  -----  -----  --------
2      0      -1     -1
```

由此可知, `Version`并不是以一串单独的数字存储的, 它本身也是一个对象, 包含 `Major, Minor, Build, Revision`四个属性, 可以查看`Version`的具体类型, 也可以访问它的每一个属性:

```powershell
PS C:Powershell> $Host.Version.GetType().FullName
System.Version
PS C:Powershell> $Host.Version.Build
-1
PS C:Powershell> $Host.Version.Major
2
PS C:Powershell> $Host.Version.MajorRevision
-1
PS C:Powershell> $Host.Version.Revision
-1
```

查看一个对象的类型很实用, 因为可以通过这个类型**构造新的对象**或者**进行类型转换**等等.

```powershell
PS C:Powershell> [System.Version]'2012.12.20.4444'

Major  Minor  Build  Revision
-----  -----  -----  --------
2012   12     20     4444
```

例如`CurrentCulture`属性,
可以通过`$host`的`CurrentCulture`访问当前系统的本地化信息和该信息的类型:

```powershell
PS C:Powershell> $Host.CurrentCulture

LCID             Name             DisplayName
----             ----             -----------
2052             zh-CN            中文(中华人民共和国)

PS C:Powershell> $Host.CurrentCulture.GetType().FullName
System.Globalization.CultureInfo
```

`CurrentCulture`包含3个属性, `LCID, Name, and DisplayName`.
通过MSDN查看`System.Globalization.CultureInfo`的构造函数可知,
可以将**国家代码**和**国家名称标志**字符串转换成一个新的`CultureInfo`对象.

```powershell
PS C:Powershell> [System.Globalization.CultureInfo]'zh-cn'

LCID             Name             DisplayName
----             ----             -----------
2052             zh-CN            中文(中华人民共和国)

PS C:Powershell> [System.Globalization.CultureInfo]'zh-tw'

LCID             Name             DisplayName
----             ----             -----------
1028             zh-TW            中文(台湾)

PS C:Powershell> [System.Globalization.CultureInfo]'en-us'

LCID             Name             DisplayName
----             ----             -----------
1033             en-US            英语(美国)

PS C:Powershell> [System.Globalization.CultureInfo] 55

LCID             Name             DisplayName
----             ----             -----------
55               ka               格鲁吉亚语
```

#### 属性中包含对象

一个对象的属性用来存储数据, 反过来这些数据又可以存储其它对象.
`$host`有两个比较特别的属性`UI`和`PrivateData`.
把`$host`对象输出到控制台上后, 除了`UI`和`PrivateData`所有的属性都会被转换成确定的文本:

```powershell
PS C:Powershell> $Host

Name    : ConsoleHost
Version             : 2.0
InstanceId          : 7fefa1fa-fb2e-47c7-a867-c13b123da5c2
UI                  : System.Management.Automation.Internal.Host.InternalHostUserInterface
CurrentCulture      : zh-CN
CurrentUICulture    : zh-CN
PrivateData         : Microsoft.PowerShell.ConsoleHost+ConsoleColorProxy
IsRunspacePushed    : False
Runspace            : System.Management.Automation.Runspaces.LocalRunspace
```

原因是这两个属性中又包含了一个对象:

```powershell
PS C:Powershell> $Host.UI

RawUI
-----
System.Management.Automation.Internal.Host.InternalHostRawUserInterface

PS C:Powershell> $Host.UI.RawUI

ForegroundColor : DarkYellow
BackgroundColor : DarkMagenta
CursorPosition: 0,23
WindowPosition : 0,0
CursorSize  : 25
BufferSize  : 100,200
WindowSize  : 100,61
MaxWindowSize  : 100,62
MaxPhysicalWindowSize : 160,62
KeyAvailable      : False
WindowTitle      : Windows PowerShell
```

"`RawUI`" 为 "`Raw User Interface`" 提供了配置`Powershell`控制台用户界面的接口.
上面的属性可以读取, 但是个别却不能更改.

#### 只读属性和读写属性

属性可以准确的描述对象, 一旦属性更改了, 这一更改也会体现在对象上.
如果不能更改, 属性就是"只读"属性.
通过简单地修改控制台的背景和前景的颜色, 可以发现属性更改可以直接反映到对象上.

```powershell
PS C:Powershell> $host.ui.rawui.BackgroundColor = "Green"
PS C:Powershell> $host.ui.rawui.ForegroundColor = "White"
PS C:Powershell> cls
```

有的属性不能更改, 如果尝试修改, 就会抛出异常.

```powershell
PS C:Powershell> $Host.UI.RawUI.KeyAvailable
False
PS C:Powershell> $Host.UI.RawUI.KeyAvailable=$false
"KeyAvailable"为 ReadOnly 属性.
```

控制台是否接收到了一个按键请求, 应当取决于用户的操作, 因此该属性拒绝被更改, 你只能读取它.

#### RawUI的属性

+ `ForegroundColor`: 前景色
+ `BackgroundColor`: 背景色
+ `CursorPosition`: 光标的位置
+ `WindowPosition`: 窗口的位置
+ `CursorSize`: 光标的大小
+ `BufferSize`: 缓冲区的大小
+ `WindowSize`: 窗口的大小
+ `MaxWindowSize`: 允许窗口的最大值
+ `MaxPhysicalWindowSize`:窗口可能的最大值
+ `KeyAvailable`: 是否存在按键
+ `WindowTitle`: 窗口的标题

#### 属性的类型

有些属性只接受整数值, 例如控制台光标的大小, 值域在`0-100`, 用来控制关闭大小的百分比.
可以将光标设置为`75%`, 但是不能超过`100%`, 否则就会产生错误.

```powershell
PS C:Powershell> $Host.UI.RawUI.CursorSize=75
PS C:Powershell> $Host.UI.RawUI.CursorSize=101
设置"CursorSize"时发生异常:"无法处理 CursorSize, 因为指定的光标大小无效.
```

另一个属性`ForegoundColor`的类型为`Color`枚举值.
因此给`ForegoundColor`所赋的值必须是已经在`System.ConsoleColor`中定义过的.
可以将"`Black`"但是不能使用"`Pink`"

```powershell
PS C:Powershell> $Host.UI.RawUI.ForegroundColor="Black"
PS C:Powershell> $Host.UI.RawUI.ForegroundColor="Pink"
设置"ForegroundColor"时发生异常:"由于枚举值无效, 无法将值"Pink"转换为类型
```

可以使用`[System.Enum]::GetNames`方法查看`ConsoleColor`定义的所有颜色.

```powershell
PS C:Powershell> [System.Enum]::GetNames([System.ConsoleColor])
Black
DarkBlue
DarkGreen
DarkCyan
DarkRed
DarkMagenta
DarkYellow
Gray
DarkGray
Blue
Green
Cyan
Red
Magenta
Yellow
White
```

有时一个属性期望的赋值必须是一个指定类型的对象.
例如`WindowSize`, 如果想改变`Powershell`的窗口大小, 可以设置`WindowSize`属性,
但是它是一个`System.Management.Automation.Host.Size`对象, 怎样获取这个对象呢?
*****
1.先读取属性, 保存为临时变量, 更改临时变量, 将临时变量赋给`WindowSize`
2.直接创建一个`System.Management.Automation.Host.Size`, 赋给`WindowSize`

```powershell
PS C:Powershell> $tmp=$Host.UI.RawUI.WindowSize
PS C:Powershell> $tmp

Width Height
----- ------
  100     60

PS C:Powershell> $tmp.Height=30
PS C:Powershell> $tmp.Width=60
PS C:Powershell> $Host.UI.RawUI.WindowSize=$tmp
Width Height
----- ------
  60     30

PS C:Powershell> $Host.UI.RawUI.WindowSize=New-Object System.Management.Automation.Host.Size(60,40)
PS C:Powershell> $Host.UI.RawUI.WindowSize

Width Height
----- ------
   60     40
```

#### 查看所有属性

因为属性和方法都是对象的成员, 可以使用`Get-Member`返回它们的详细信息,
如果只显示属性可以使用参数 `memberType` 为"`Property`"

```powershell
PS C:Powershell> $host | Get-Member -memberType property

   TypeName: System.Management.Automation.Internal.Host.InternalHost

Name             MemberType Definition
----             ---------- ----------
CurrentCulture      Property   System.Globalization.CultureInfo CurrentCulture {get;}
CurrentUICulture    Property   System.Globalization.CultureInfo CurrentUICulture {get;}
...
```

在`Name`列, 可以看到`$host`支持的所有属性.
`在Definition`列首先列出属性的具体类型, 然后列出构造器,
如果一个构造器中只有`Get`方法, 没有`Set`方法, 表示该属性为**只读**属性.

### 方法(对象能做什么)

方法定义了一个对象可以做什么事情.
当你把一个对象输出在控制台时, 它的属性可能会被转换成可视的文本.
但是它的方法却不可见.
列出一个对象的所有方法可以使用`Get-Member`命令, 给"`MemeberType`"参数 传入"`Method`":

```powershell
PS C:Powershell> $Host | Get-Member -MemberType Method

   TypeName: System.Management.Automation.Internal.Host.InternalHost

Name                   MemberType Definition
----                   ---------- ----------
EnterNestedPrompt     Method     System.Void EnterNestedPrompt()
Equals                 Method     bool Equals(System.Object obj)
...
```

#### 过滤内部方法

`Get-Memeber`列出了一个对象定义的所有方法, 但并不是所有的方法都有用, 有些方法的的用处非常有限.

`Get_` 和 `Set_` 方法

所有名称以"`get_`"打头的方法都是为了给对应的属性返回一个值.
例如"`get_someInfo()`"方法的作用就是返回属性`someInfo`的值, 因此可以直接通过属性调用.

```powershell
PS C:Powershell> $Host.Version

Major  Minor  Build  Revision
-----  -----  -----  --------
2      0      -1     -1

PS C:Powershell> $Host.get_Version()

Major  Minor  Build  Revision
-----  -----  -----  --------
2      0      -1     -1
```

类似的像"`set_someinfo`"一样, 该方法只是为了给属性`someinfo`赋值, 可以直接通过属性赋值调用.
如果一个对象中只有"`get_someinfo`", 没有对应的"`set_someinfo`", 说明`someinfo`这个属性为只读属性.

#### 标准方法

几乎每个对象都有一些继承自父类的方法, 这些方法并不是该对象所特有的方法, 而是所有对象共有的方法.

+ `Equals` 比较两个对象是否相同
+ `GetHashCode` 返回一个对象的数字格式的指纹
+ `GetType` 返回一个对象的数据类型
+ `ToString` 将一个对象转换成可读的字符串

剔除包含下划线的方法可以使用操作符 `-notlike` 和 通配符 `*`

```powershell
PS C:Powershell> $Host.UI.RawUI | Get-Member -me method | where {$_.Name -notlike '*_*'}

   TypeName: System.Management.Automation.Internal.Host.InternalHostRawUserInterface

Name                 MemberType Definition
----                 ---------- ----------
Equals               Method     bool Equals(System.Object obj)
FlushInputBuffer    Method     System.Void FlushInputBuffer()
...
```

#### 调用方法

一定要注意, 在调用一个方法前, 必须知道这个方法的功能.
因为有的命令可能比较危险, 例如错误地修改环境变量.
调用一个方法, 通过圆点加圆括号:

```powershell
$Host.GetType()
```

#### 调用带参数的方法

`UI`对象有很多实用的方法, 可以通过`get-member`预览

```powershell
PS C:Powershell> $Host.UI | Get-Member -MemberType method

   TypeName: System.Management.Automation.Internal.Host.InternalHostUserInterface

Name                   MemberType Definition
----                   ---------- ----------
Equals                 Method     bool Equals(System.Object obj)
GetHashCode            Method     int GetHashCode()
...
```

#### 哪一个参数是必须的

从列表中筛选出一个方法, 再通过`Get-Member`得到更多的信息.

```powershell
PS C:Powershell> $info=$Host.UI |  Get-Member WriteDebugLine
PS C:Powershell> $info

   TypeName: System.Management.Automation.Internal.Host.InternalHostUserInterface

Name           MemberType Definition
----           ---------- ----------
WriteDebugLine Method     System.Void WriteDebugLine(string message)

PS C:Powershell> $info.Definition
System.Void WriteDebugLine(string message)
```

`Definition`属性告诉你怎样调用一个方法, 每一个方法的定义都会返回一个Objec对象, System.Void 是一个特殊的类型, 代表什么都没有, 即返回值为空.
接下来就可以根据函数的定义, 给它传进合适的参数调用了.

```powershell
PS C:Powershell> $Host.UI.WriteDebugLine("Hello 2012 !")
调试: Hello 2012 !
```

#### 低级函数

上述的`WriteDebugLine()`函数并没有什么特别.
事实上所谓的`$Host`中的很多方法只不过是一些简单的`Cmdlets`命令.
例如使用如下`cmdlet`输出一条调试通知

```powershell
PS C:Powershell> Write-Debug "Hello 2012 !"
PS C:Powershell> Write-Debug -Message "Hello 2012 !"
```

上述的命令并没有输出黄色的调试信息, 这和`$DebugPreference`配置有关,
因为`$DebugPreference`的默认值为: `SilentlyContinue`.
当`$DebugPreference`为`Stop`, `Continue`, `Inquire`时就会输出调试消息:

```powershell
PS C:Powershell> [System.Enum]::GetNames([System.Management.Automation.ActionPreference])
SilentlyContinue
Stop
Continue
Inquire
PS C:Powershell> $DebugPreference="stop"
PS C:Powershell> Write-Debug "Hello 2012"
调试: Hello 2012
Write-Debug : 已停止执行命令, 因为首选项变量"DebugPreference"或通用参数被设置为 Stop.
...

PS C:Powershell> $DebugPreference="continue"
PS C:Powershell> Write-Debug "Hello 2012"
调试: Hello 2012
```

`WriteErrorLine`, `WriteVerboseLine`, `WriteWarningLine`的情况也类似.
如果你不想受$DebugPreference配置的依赖, 输出错误消息可以直接使用 `$host.UI.WriteDebugLine()`方法.

#### 多个方法的签名

有些方法名相同, 可以接受不同类型或者不同个数的参数,
如何查看一个方法支持的所有签名, 使用`Get-Member`获取方法对象, 然后查看`Definition`属性.

```powershell
PS C:Powershell> $method
PS C:Powershell> $method=$Host.UI | Get-Member WriteLine
PS C:Powershell> $method.Definition
System.Void WriteLine(), System.Void WriteLine(System.ConsoleColor foregroundColor, System.ConsoleColor backgroundColor
, string value), System.Void WriteLine(string value)
```

但是`Definition`的输出阅读不方便, 可以稍加润色.

```powershell
PS C:Powershell> $method.Definition.Replace("),",")`n")
System.Void WriteLine()
System.Void WriteLine(System.ConsoleColor foregroundColor, System.ConsoleColor backgroundColor, string value)
System.Void WriteLine(string value)
```

#### 创建选择菜单

这里需要使用`$host.UI.PromptForChoice()`方法, 先查看方法的定义:

```powershell
PS C:Powershell> $host.ui.PromptForChoice

MemberType          : Method
OverloadDefinitions : {int PromptForChoice(string caption, string message,
...
```

下面的脚本演示如何创建选择菜单:

```powershell
$SwitchUser = ([System.Management.Automation.Host.ChoiceDescription]"&Switchuser")
$LoginOff = ([System.Management.Automation.Host.ChoiceDescription]"&LoginOff")
$Lock= ([System.Management.Automation.Host.ChoiceDescription]"&Lock")
$Reboot= ([System.Management.Automation.Host.ChoiceDescription]"&Reboot")
$Sleep= ([System.Management.Automation.Host.ChoiceDescription]"&Sleep")

$selection = [System.Management.Automation.Host.ChoiceDescription[]]($SwitchUser,$LoginOff,$Lock,$Reboot,$Sleep)
$answer=$Host.UI.PromptForChoice('接下来做什么事呢? ','请选择:',$selection,1)
"您选择的是: "
switch($answer)
{
0 {"切换用户"}
1 {"注销"}
2 {"锁定"}
3 {"重启"}
4 {"休眠"}
}
```

```powershell
PS C:PowerShell> .\test.ps1

接下来做什么事呢?
请选择:
[S] Switchuser  [L] LoginOff  [L] Lock  [R] Reboot  [S] Sleep  [?] 帮助 (默认值为"L"): Reboot
您选择的是:
重启
```

### 使用真实的对象工作

每一个`Powershell`命令都会返回一个对象, 但是返回的对象不易操作,
因为控制台解释器会自动将对象转换成可视的文本, 这样就会丢失绝大多数对象的信息.

#### 在变量中存储结果

不要将结果在控制台输出可以防止对象转换成文本.
控制台是一个不安全的地方, 任何对象输出后都会自动转换成文本, 最安全的方式是将对象保存在变量中.
如果想将对象输出为文本, 可以在控制台输入变量名.

```powershell
PS C:Powershell> $FileList=dir
PS C:Powershell> $FileList

    目录: C:Powershell

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
-a---        2011/12/19     17:43       8956 a.ccs
-a---        2011/12/19     18:02      46411 a.csv
```

事实上述存储在`$FileList`变量中的并不是单个的对象, 而是一个对象数组, 数组可以通过索引访问得到真实的对象.

```powershell
PS C:Powershell> $obj=(dir)[0]
PS C:Powershell> $obj

    目录: C:Powershell

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
-a---        2011/12/19     17:43       8956 a.ccs
```

#### 使用对象的属性

可以使用`Get-Member`得到一个对象所有的属性:

```powershell
PS C:Powershell> $obj=(dir)[0]
PS C:Powershell> $obj | Get-Member -MemberType Property

   TypeName: System.IO.FileInfo

Name              MemberType Definition
----              ---------- ----------
Attributes        Property   System.IO.FileAttributes Attributes {get;set;}
CreationTime      Property   System.DateTime CreationTime {get;set;}
...
```

如果属性的定义列中包含`{get;set}`表明该属性可以被更新:

```powershell
PS C:Powershell> $obj.LastAccessTime

2011年12月19日 17:43:37

PS C:Powershell> $obj.LastAccessTime=Get-Date
PS C:Powershell> $obj.LastAccessTime

2012年1月11日 14:21:01
```

#### 特殊属性

`Powershell`中可以给一个对象增加属性, 增加的属性仍然可以通过`Get-Member`的标签辨别,
因为对象的正常属性标签名为: `Property`, 新增加的属性标签多了一个前缀, 如`ScriptProperty`和`NoteProperty`.

一个`NoteProperty`包含了**静态的数据**.
一个`ScriptProperty`中包含了一段脚本, 通过脚本计算出属性的值.

下面的例子新建一个对象`$obj`,
给`$obj`增加两个属性一个为`NoteProperty`, 一个为`ScriptProperty`,
输出`$obj`, `CurrentTime`属性会自动更新, `AddTime`则不会.

```powershell
PS C:Powershell> $obj=New-Object PSobject
PS C:Powershell> $obj | Add-Member -MemberType NoteProperty -Name AddTime -Value (get-date)
PS C:Powershell> $obj | Add-Member -MemberType ScriptProperty -Name CurrentTime -Value {get-date}
PS C:Powershell> $obj

AddTime                                                     CurrentTime
-------                                                     -----------
2012/1/11 14:35:38                                          2012/1/11 14:36:35

PS C:Powershell> $obj

AddTime                                                     CurrentTime
-------                                                     -----------
2012/1/11 14:35:38                                          2012/1/11 14:36:44
```

`MemberType`包括:

+ `AliasProperty` : 另外一个属性的别名
+ `CodeProperty` : 通过静态的`.Net`方法返回属性的内容
+ `Property` : 真正的属性
+ `NoteProperty` : 随后增加的属性
+ `ScriptProperty` : 通过脚本执行返回一个属性的值
+ `ParameterizedProperty` : 需要传递参数的属性

#### 调用对象的方法

同样可以通过`Get-Memeber`获得一个对象支持的所有方法:

```powershell
PS C:Powershell> $obj= (dir)[0]
PS C:Powershell> $obj | Get-Member -me method

   TypeName: System.IO.FileInfo

Name                      MemberType Definition
----                      ---------- ----------
AppendText                Method     System.IO.StreamWriter AppendText()
CopyTo                    Method     System.IO.FileInfo CopyTo(string destFileName), ...
```

调用一个对象的方法时, 省略括号可以获取一个方法的详细定义信息:

```powershell
PS C:Powershell> $obj.CreationTime

2011年12月19日 17:43:37

PS C:Powershell> $obj.MoveTo

MemberType          : Method
OverloadDefinitions : {System.Void MoveTo(string destFileName)}
TypeNameOfValue     : System.Management.Automation.PSMethod
Value               : System.Void MoveTo(string destFileName)
Name                : MoveTo
IsInstance          : True
```

#### 调用对象的Delete方法

```powershell
PS C:Powershell> Test-Path $obj
True
PS C:Powershell> $obj.Delete()
PS C:Powershell> Test-Path $obj
False
```

#### 不同的方法类型

类似于属性, `Powershell`对象也可以增加方法, 方法类型包括:

+ `CodeMethod`: 映射到静态的`.NET`方法
+ `Method`: 正常的方法
+ `ScriptMethod`: 一个执行`Powershell`脚本的方法

### 调用静态方法

`Powershell` 将信息存储在对象中, 每个对象都会有一个具体的类型,
简单的文本会以 `System.String` 类型存储, 日期会以`System.DateTime`类型存储.
任何`.NET`对象都可以通过`GetType()`方法返回它的类型,
该类型中有一个`FullName`属性, 可以查看类型的完整名称.

```powershell
PS C:Powershell> $date=get-date
PS C:Powershell> $date

2012年1月11日 15:19:49

PS C:Powershell> $date.GetType().FullName
System.DateTime
```

每一个类型都可以包含一些静态的方法, 可以通过方括号和类型名称得到**类型对象本身**,
然后通过`Get-Memeber`命令查看该类型支持的所有静态方法.

```powershell
PS C:Powershell> [System.DateTime] | Get-Member -static -memberType *Method

   TypeName: System.DateTime

Name           MemberType Definition
----           ---------- ----------
Compare        Method     static int Compare(System.DateTime t1, System.Dat...
DaysInMonth    Method     static int DaysInMonth(int year, int month)
...
```

`System.DateTime`类支持的静态方法非常实用

使用`Parse`方法将一个字符串转换成`DateTime`类:

```powershell
PS C:Powershell> [System.DateTime]::Parse("2012-10-13 23:42:55")

2012年10月13日 23:42:55
```

使用`isLeapYear`方法判断闰年

```powershell
#1988年是闰年吗?
[System.DateTime]::IsLeapYear(1988)
#打印1988到2000年的所有闰年
for($year=1988;$year -le 2000;$year++)
{
    if( [System.DateTime]::IsLeapYear($year) ){$year}
}

True
1988
1992
1996
2000
```

另一个常用的类为`Math`类, 在`Math`类中定义了很多实用的静态方法:

例如`求绝对值`, `三角函数`, `取整`:

```powershell
PS C:Powershell> [Math]::Abs(-10.89)
10.89
PS C:Powershell> [Math]::Sin([Math]::PI/2)
1
PS C:Powershell> [Math]::Truncate(2012.7765)
2012
```

#### 查看感兴趣的.NET类型

`.NET`支持成千上万的类型, 有了这些类型可以做许多事情, 幸运的是`Powershell`恰好支持这些类型.

##### 对象类型转换

例如使用`System.Net.IPAddress`类将字符串`IP`地址转换成一个`IPAddress`实例

PS C:Powershell> [Net.IPAddress]'10.3.129.71'

Address            : 1199637258
AddressFamily      : InterNetwork
ScopeId            :
IsIPv6Multicast    : False
IsIPv6LinkLocal    : False
IsIPv6SiteLocal    : False
IPAddressToString  : 10.3.129.71

###### 调用静态的方法

同样是`System.Net.IPAddress`类, 根据`IP`地址查看主机名, `8.8.8.8`是谷歌的免费`DNS`服务器

```powershell
PS C:Powershell> [system.Net.Dns]::GetHostByAddress('8.8.8.8') | fl

HostName    : google-public-dns-a.google.com
Aliases     : {}
AddressList : {8.8.8.8}
```

##### 根据类型创建实例

下面演示通过`$webClient`类的`DownloadFile`方法下载文件:

```powershell
PS C:Powershell> $localName="C:\Users\Tom\Desktop\new\Powershellindex.php"
PS C:Powershell> Test-Path $localName
False
PS C:Powershell> $add="http://www.mossfly.com/index.php"
PS C:Powershell> $webClient=New-Object Net.WebClient
PS C:Powershell> $webClient.DownloadFile($add,$localName)
PS C:Powershell> Test-Path $localName
True
```

#### 查看程序集

`.NET`中的类型定义在不同的程序集中, 首先得知道当前程序已经加载了那些程序集.
`AppDomain`类可以完成这个需求, 因为它有一个静态成员`CurrentDomain`, `CurrentDomain`中有一个`GetAssemblies()`方法.

```powershell
PS C:Powershell> [AppDomain]::CurrentDomain

FriendlyName           : DefaultDomain
Id                     : 1
...

PS C:Powershell> [AppDomain]::CurrentDomain.GetAssemblies()

GAC    Version        Location
---    -------        --------
True   v2.0.50727     C:WindowsMicrosoft.NETFrameworkv2.0.50727mscorlib...
True   v2.0.50727     C:WindowsassemblyGAC_MSILMicrosoft.PowerShell.Cons...
...
```

#### 搜索指定类型

查询每个程序集中的方法可使用 `GetExportedTypes()` 方法.
因为许多程序集中包含了大量的方法, 在搜索时最好指定关键字.
下面的代码演示如何查找包含"`environment`"关键字的类型.

```powershell
PS C:Powershell> [AppDomain]::CurrentDomain.GetAssemblies() | Where-Object { -not $_.IsDynamic } | ForEach-Object {$_.GetExportedTypes() } | Where-Object { $_ -like '*environment*' } | ForEach-Object { $_.FullName }

System.EnvironmentVariableTarget
System.Environment
...
```

上面搜索到的类型有一个为: `System.Environment`类,
`System.Environment`类可以做很多事情, 可以先查看以下`System.Environment`类的所有静态方法.

```powershell
PS C:Powershell> [Environment] | Get-Member -Static

   TypeName: System.Environment

Name                       MemberType Definition
----                       ---------- ----------
Equals                     Method     static bool Equals(System.Object objA,
Exit                       Method     static System.Void Exit(int exitCode)

```

例如`System.Environment`中的属性输出当前登录域, 用户名, 机器名:

```powershell
PS C:Powershell> [Environment]::UserDomainName
MyHome
PS C:Powershell> [Environment]::UserName
xiaoming
PS C:Powershell> [Environment]::MachineName
LocalHost
```

#### 搜索方法

下面的例子演示如何根据指定关键字"`Address`",搜索方法.

```powershell
[AppDomain]::CurrentDomain.GetAssemblies() | ForEach-Object { $_.GetExportedTypes() } | ForEach-Object { $_.getmembers() } | Where-Object { $_.isStatic} | Where-Object { $_ -like $searchtext } | ForEach-Object { "[{0}]::{1} --> {2}" -f  $_.declaringtype, $_.toString().SubString($_.toString().IndexOf(" ")+1), $_.ReturnType }

[System.Net.IPAddress]::Parse(System.String) --> System.Net.IPAddress
[System.Net.IPAddress]::IsLoopback(System.Net.IPAddress) --> System.Boolean
...
```

新版的 `.NET` 中有动态类型, 不能直接通过`GetExportedTypes()`调用了

### 创建对象

`.Net`类型中的方法功能很强大.
可以通过类型的构造函数创建新的对象, 也可以将已存在的对象转换成指定的类型.

### 通过 New-Object 创建新对象

如果使用构造函数创建一个指定类型的实例对象, 该类型必须至少包含一个签名相匹配的构造函数.
例如可以通过字符和数字创建一个包含指定个数字符的字符串:

```powershell
PS C:Powershell> New-Object String("*",100)
*******************************************************************************
*********************
```

为什么支持上面的方法, 原因是`String`类中包含一个`Void .ctor(Char, Int32)`构造函数

```powershell
PS C:Powershell> [String].GetConstructors() | foreach {$_.tostring()}
Void .ctor(Char*)
Void .ctor(Char*, Int32, Int32)
Void .ctor(SByte*)
Void .ctor(SByte*, Int32, Int32)
Void .ctor(SByte*, Int32, Int32, System.Text.Encoding)
Void .ctor(Char[], Int32, Int32)
Void .ctor(Char[])
Void .ctor(Char, Int32)
```

`.ctor` 是"构造函数"的缩写---`constructor`

#### 通过类型转换创建对象

通过类型转换可以替代`New-Object`

```powershell
PS C:Powershell> $date="1999-9-1 10:23:44"
PS C:Powershell> $date.GetType().fullName
System.String
PS C:Powershell> $date
1999-9-1 10:23:44
PS C:Powershell> [DateTime]$date="1999-9-1 10:23:44"
PS C:Powershell> $date.GetType().FullName
System.DateTime
PS C:Powershell> $date

1999年9月1日 10:23:44
```

如果条件允许, 也可以直接将对象转换成数组

```powershell
PS C:Powershell> [char[]]"mossfly.com"
m
o
s
s
f
l
y
.
c
o
m
PS C:Powershell> [int[]][char[]]"mossfly.com"
109
111
115
115
102
108
121
46
99
111
109
```

#### 加载程序集

自定义一个简单的`C#`类库编译为`Test.dll`:

```powershell
using System;
using System.Collections.Generic;
using System.Text;
using System.Net;

namespace Test
{
    public class Student
    {
        public string Name { set; get; }
        public int Age { set; get; }
        public Student(string name, int age)
        {
            this.Name = name;
            this.Age = age;
        }
        public override string  ToString()
        {
            return string.Format("Name={0};Age={1}", this.Name,this.Age);
        }
    }
}
```

在`Powershell`中加载这个`dll`并使用其中的`Student`类的构造函数生成一个实例,
最后调用`ToString()`方法.

```powershell
PS C:Powershell> ls .Test.dll

    目录: C:Powershell

Mode                LastWriteTime     Length Name
----                -------------     ------ ----
-a---         2012/1/13     10:49       4608 Test.dll

PS C:Powershell> $TestDLL=ls .Test.dll
PS C:Powershell> [reflection.assembly]::LoadFile($TestDLL.FullName)

GAC    Version        Location
---    -------        --------
False  v2.0.50727     C:PowershellTest.dll

PS C:Powershell> $stu=New-Object Test.Student('Mosser',22)
PS C:Powershell> $stu

Name                                                                        Age
----                                                                        ---
Mosser                                                                       22

PS C:Powershell> $stu.ToString()
Name=Mosser;Age=22
```

#### 使用`COM`对象

作为`.NET`的补充, `Powershell`可以加载和访问`COM`对象.

#### 查看可用的COM对象

每一个`COM`对象都有存储在注册表中的唯一标识符, 想遍历访问可用的`COM`对象, 可以直接访问注册表.

```powershell
Dir REGISTRY::HKEY_CLASSES_ROOT\CLSID  -include PROGID -recurse | foreach {$_.GetValue("")}
DAO.DBEngine.36
DAO.PrivateDBEngine.36
DAO.TableDef.36
......
```

#### 怎样使用COM对象

一旦得到了`COM`对象的`ProgID`, 就可以使用`New-Object`创建`COM`对象, 只需要指定参数为`-comObject`.

```powershell
PS C:Powershell> New-Object -ComObject ADODB.Command.6.0

Properties       : System.__ComObject
ActiveConnection :
CommandText      :
...
```

`COM`对象的和`.NET`对象相似, 仍然可使用`Get-Member` 得到该对象的所有属性和方法:

```powershell
PS C:Powershell> $DBEng=New-Object -ComObject ADODB.Command.6.0
PS C:Powershell> $DBEng | Get-Member -me *method

   TypeName: System.__ComObject#{00000021-0000-0010-8000-00aa006d2ea4}

Name                MemberType Definition
----                ---------- ----------
BeginTrans          Method     void BeginTrans ()
CommitTrans         Method     void CommitTrans (int)
...

PS C:Powershell> $DBEng | Get-Member -me *property

   TypeName: System.__ComObject#{00000021-0000-0010-8000-00aa006d2ea4}

Name            MemberType Definition
----            ---------- ----------
DefaultPassword Property   string DefaultPassword () {set}
DefaultType     Property   int DefaultType () {get} {set}
...
```

常用的`COM`对象中有

+ `WScript.Shell,`
+ `WScript.Network,`
+ `Scripting.FileSystemObject,`
+ `InternetExplorer.Application,`
+ `Word.Application,`
+ `Shell.Application`

下面的例子使用`WScript.shell` `COM`对象和它的方法`CreateShortcut()`在桌面上创建一个`Powershell`快捷方式:

```powershell
PS C:Powershell> $wshell=New-Object -ComObject WScript.shell
PS C:Powershell> $path=[environment]::GetFolderPath('Desktop')
PS C:Powershell> $link=$wshell.CreateShortcut("$path/Powershell.lnk")
PS C:Powershell> $link | Get-Member

   TypeName: System.__ComObject#{f935dc23-1cf0-11d0-adb9-00c04fd58a0b}

Name             MemberType Definition
----             ---------- ----------
Load             Method     void Load (string)
...

PS C:Powershell> $link.TargetPath='Powershell.exe'
PS C:Powershell> $link.Description="启动Powershell"
PS C:Powershell> $link.WorkingDirectory=$PROFILE
PS C:Powershell> $link.IconLocation='Powershell.exe'
PS C:Powershell> $link.Save()
```

## 条件判断

### 比较运算符

+ `-eq` : 等于
+ `-ne` : 不等于
+ `-gt` : 大于
+ `-ge` : 大于等于
+ `-lt` : 小于
+ `-le` : 小于等于
+ `-contains` : 包含
+ `-notcontains` :不包含

### 进行比较

可以将比较表达式直接输入进`Powershell`控制台, 然后回车, 会自动比较并把比较结果返回.

```powershell
PS C:Powershell> (3,4,5 ) -contains 2
False
PS C:Powershell> (3,4,5 ) -contains 5
True
PS C:Powershell> (3,4,5 ) -notcontains 6
True
PS C:Powershell> 2 -eq 10
False
PS C:Powershell> "A" -eq "a"
True
PS C:Powershell> "A" -ieq "a"
True
PS C:Powershell> "A" -ceq "a"
False
PS C:Powershell> 1gb -lt 1gb+1
True
PS C:Powershell> 1gb -lt 1gb-1
False
```

### 求反

求反运算符为`-not` 但是像高级语言一样"`! `" 也支持求反.

```powershell
PS C:Powershell> $a= 2 -eq 3
PS C:Powershell> $a
False
PS C:Powershell> -not $a
True
PS C:Powershell> !($a)
True
```

### 布尔运算

+ `-and` : 和
+ `-or`  : 或
+ `-xor` : 异或
+ `-not` : 逆

```powershell
PS C:Powershell> $true -and $true
True
PS C:Powershell> $true -and $false
False
PS C:Powershell> $true -or $true
True
PS C:Powershell> $true -or $false
True
PS C:Powershell> $true -xor $false
True
PS C:Powershell> $true -xor $true
False
PS C:Powershell>  -not  $true
False
```

### 比较数组和集合

#### 过滤数组中的元素

```powershell
PS C:Powershell> 1,2,3,4,3,2,1 -eq 3
3
3
PS C:Powershell> 1,2,3,4,3,2,1 -ne 3
1
2
4
2
1
```

### 验证一个数组是否存在特定元素

```powershell
PS C:Powershell> $help=(man ls)
PS C:Powershell> 1,9,4,5 -contains 9
True
PS C:Powershell> 1,9,4,5 -contains 10
False
PS C:Powershell> 1,9,4,5 -notcontains 10
True
```

`-contains` 大小写不敏感, `-ccontains`大小写敏感.

```powershell
PS> "a","b","c" -ccontains "a"
True
PS> "a","b","c" -contains "a"
True
PS> "a","b","c" -ccontains "A"
False
```

### 条件过滤Where-Object

本篇会对条件判断进行实际应用.
在管道中可以通过条件判断过滤管道结果, Where-Object会对集合逐个过滤, 将符合条件的结果保留.

#### 过滤管道结果

使用`Get-Process`返回所有的当前进程 , 但是你可能并不对所有的进程感兴趣,
然后通过每个`Process`对象的属性进行过滤. 首先得知道每个对象支持那些属性

```powershell
PS C:Powershell> Get-Process | select -First 1 | fl *
```

*****
根据进程名过滤所有`记事本`进程.

```powershell
PS C:Powershell> Get-Process | Where-Object {$_.Name -eq "notepad"}

Handles  NPM(K)    PM(K)      WS(K) VM(M)   CPU(s)     Id ProcessName
-------  ------    -----      ----- -----   ------     -- -----------
    158       7     8800      37264   114    18.41   6204 notepad
```

*****
根据进程名过滤所有`IE`进程.

```powershell
PS C:Powershell> Get-Process | Where-Object {$_.Name -eq "iexplore"}

Handles  NPM(K)    PM(K)      WS(K) VM(M)   CPU(s)     Id ProcessName
-------  ------    -----      ----- -----   ------     -- -----------
    710      23    12832      18160   175    10.51   4204 iexplore
    971      39    81000     107580   399    22.20   6764 iexplore
    336      13    28516      20096   187     0.34   6792 iexplore
    929      35    51020      46568   314    10.42   7192 iexplore
    835      26    49200      32360   308     7.82   7952 iexplore
```

*****
根据`company`过滤所有产品发布者以"`Microsoft`"打头的进程:

```powershell
PS C:Powershell> Get-Process | Where-Object {$_.company -like '*Microsoft*' }| select Name,Description,Company
msseces                    Microsoft Security Clie... Microsoft Corporation
notepad                    记事本                     Microsoft Corporation
ONENOTEM                   Microsoft OneNote Quick... Microsoft Corporation
OUTLOOK                    Microsoft Outlook          Microsoft Corporation
powershell                 Windows PowerShell         Microsoft Corporation
prevhost                   Preview Handler Surroga... Microsoft Corporation
RDCMan                     RDCMan                     Microsoft Corporation
SearchProtocolHost         Microsoft Windows Searc... Microsoft Corporation
taskhost                   Windows 任务的主机进程     Microsoft Corporation
```

#### 使用别名

因为`Where-Object`的使用概率比较高, 所以有一个很形象的别名`? `可以使用:

```powershell
PS C:Powershell> Get-Service | ? {$_.Name -like "B*"}

Status   Name               DisplayName
------   ----               -----------
Running  BDESVC             BitLocker Drive Encryption Service
Running  BFE                Base Filtering Engine
Running  BITS               Background Intelligent Transfer Ser...
Stopped  Browser            Computer Browser
Stopped  bthserv            Bluetooth Support Service
```

#### example

`Where-Object`

Selects objects from a collection based on their property values.

These commands get a list of all services that are currently stopped.
The `$_` automatic variable represents each object that is passed to the `Where-Object` cmdlet.
The first command uses the **script block** format, the second command uses the **comparison statement** format.

The commands are equivalent and can be used interchangeably.

```powershell
Get-Service | Where-Object {$_.Status -eq "Stopped"}
Get-Service | where Status -eq "Stopped"
```

*****
`-Like`

Indicates that this cmdlet gets objects if the property value matches a value that **includes wildcard characters**.

For example: `Get-Process | where ProcessName -Like "*host"`
*****

### Powershell IF-ELSEIF-ELSE 条件

`Where-Object` 进行条件判断很方便, 如果在判断后执行很多代码可以使用`IF-ELSEIF-ELSE`语句. 语句模板:

```powershell
If(条件满足){
如果条件满足就执行代码
}
elseif
{
如果条件满足
}
else
{还不满足}
```

条件判断必须放在圆括号中, 执行的代码必须紧跟在后面的花括号中.

```powershell
PS C:Powershell> $n=8
PS C:Powershell> if($n -gt 15) {"$n  大于 15 " }
PS C:Powershell> if($n -gt 5) {"$n  大于 5 " }
8  大于 5
PS C:Powershell> if($n -lt 0 ){"-1" } elseif($n -eq 0){"0"} else {"1"}
1
```

### Powershell Switch 条件

如果语句中有多路分支, 使用`IF-ELSEIF-ELSE`不友好, 可以使用`Switch`, 看起来比较清爽一点.
下面的例子将`If-ElseIF-Else`转换成`Switch`语句

```powershell
#使用 IF-ElseIF-Else
If( $value -eq 1 )
{
    "Beijing"
}
Elseif( $value -eq 2)
{
    "Shanghai"
}
Elseif( $value -eq 3 )
{
    "Tianjin"
}
Else
{
    "Chongqing"
}

#使用 Switch
switch($value)
{
    1 {"Beijing"}
    2 {"Shanghai"}
    3 {"Tianjin"}
    4 {"Chongqing"}
}
```

#### 测试取值范围

使用 `Switch`时缺省的比较运算符为 `-eq` 等于, 你也可以自己定制比较条件,
将条件放在花括号中, 必须保证条件表达式的返回值为布尔类型"`$True`"或"`$False`"

```powershell
$value=18
# 使用 Switch 测试取值范围
switch($value)
{
    {$_ -lt 10} {"小于10"}
    10  {"等于10"}
    {$_  -gt 10} {"大于10"}
}
#输出
#大于10
```

#### 没有匹配条件

在`IF-Else`语句中如果没有合适的条件匹配, 可以在`Else`中进行处理,
同样在`Switch`语句中如果`case`中没有条件匹配, 可以使用关键字`Default`进行处理.
同样是上面的例子, 稍加修改:

```powershell
$value=-7
# 使用 Switch 测试取值范围
switch($value)
{
    {($_ -lt 10) -and ( $_ -gt 0) }  {"小于10"}
    10  {"等于10"}
    {$_  -gt 10} {"大于10"}
    Default {"没有匹配条件"}
}
#Output:
#没有匹配条件
```

#### 多个条件匹配

如果`case`中有多个条件匹配, 那么每个匹配的条件都会进行处理, 例如:

```powershell
$value=2
# 使用 Switch 测试取值范围
switch($value)
{
    {$_ -lt 5 }  { "小于5" }
    {$_ -gt 0 }   { "大于0" }
    {$_ -lt 100}{ "小于100"}
    Default {"没有匹配条件"}
}

#小于5
#大于0
#小于100
```

如果碰到匹配条件时只处理一次, 可以使用`Break`关键字

```powershell
$value=99
# 使用 Switch 测试取值范围
switch($value)
{
    {$_ -lt 5 }   { "小于5"; break}
    {$_ -gt 0 }   { "大于0"; break}
    {$_ -lt 100}  { "小于100"; break}
    Default {"没有匹配条件"}
}

#大于0
```

#### 比较字符串

之前的条件比较的都是数字, 接下来比较字符串, 默认的条件判断为`-eq`,
我们知道在`Powershell`中字符串的使用`-eq`比较大小写不敏感, 所以才有下面的例子:

```powershell
$domain="www.mossfly.com"
switch($domain)
{
    "Www.moSSfly.com" {"Ok 1"}
    "www.MOSSFLY.com" {"Ok 2" }
    "WWW.mossfly.COM" {"Ok 3"}
}
Ok 1
Ok 2
Ok 3
```

大小写敏感

怎样在比较字符串时能够恢复为大小写敏感模式?
`Switch`有一个`-case`选项, 一旦指定了这个选项, 比较运算符就会从`-eq` 切换到`-ceq`, 即大小写敏感比较字符串:

```powershell
$domain="www.mossfly.com"
#大小写敏感
switch -case ($domain)
{
    "Www.moSSfly.com" {"Ok 1"}
    "www.MOSSFLY.com" {"Ok 2" }
    "www.mossfly.com" {"Ok 3"}
}
#Ok 3
```

#### switch使用通配符

字符串非常特殊, 需要使用通配符, 幸运的是`Powershell`也支持, 果然`Power`啊.
但是在Switch语句后要指定 `-wildcard` 选项

```powershell
$domain="www.mossfly.com"
#使用通配符
switch -wildcard($domain)
{
    "*"     {"匹配'*'"}
    "*.com" {"匹配*.com" }
    "*.*.*" {"匹配*.*.*"}
}
匹配'*'
匹配*.com
匹配*.*.*
```

在字符串匹配中, 比通配符功能更强大是正则表达式, `Powershell`的`Switch`语句也支持, 真是太棒了.
当然需要给`Switch`关键字指定选项`-regex`

```powershell
$mail="www@mossfly.com"
#使用通配符
switch -regex ($mail)
{
    "^www"     {"www打头"}
    "com$"     {"com结尾" }
    "d{1,3}.d{1,3}.d{1,3}.d{1,3}" {"IP地址"}
}

#www打头
#com结尾
```

#### 同时处理多个值

`Switch`支持对集合所有元素进行匹配,下面的例子使用`Powershell Switch`语句演示打印水仙花数:

```powershell
$value=100..999
switch($value)
{
{[Math]::Pow($_%10,3)+[Math]::Pow( [Math]::Truncate($_%100/10) ,3)+[Math]::Pow( [Math]::Truncate($_/100) , 3) -eq $_} {$_}
}

#153
#370
#371
#407
```

## 循环

### ForEach-Object 循环

`Powershell`管道就像流水线, 对于数据的处理是一个环节接着一个环节,
如果你想在某一环节对流进来的数据逐个细致化的处理, 可以使用`ForEach-Object`,
`$_`代表当前的数据.

#### 对管道对象逐个处理

如果使用`Get-WmiObject`获取系统中的服务, 为了排版可能会也会使用`Format-Table`对结果进行表格排版.

```powershell
PS C:Powershell> Get-WmiObject Win32_Service | Format-Table status,DisplayName -AutoSize

status DisplayName
------ -----------
OK     Adobe Acrobat Update Service
OK     Application Experience
...
```

但是如果想对每个服务进行更定制化的处理可以使用`ForEach-Object`

```powershell
PS C:Powershell> Get-WmiObject Win32_Service | ForEach-Object {"Name:"+ $_.Disp
layName, ", Is ProcessId more than 100:" + ($_.ProcessId -gt 100)}
Name:Adobe Acrobat Update Service , Is ProcessId more than 100:True
Name:Application Experience , Is ProcessId more than 100:False
Name:Application Layer Gateway Service , Is ProcessId more than 100:False
```

#### 结合条件处理

`ForEach-Object`的处理可以包含任意`Powershell`脚本, 当然也包括条件语句

```powershell
Get-WmiObject Win32_Service | ForEach-Object {
    if ($_.ProcessId -gt 3000)
    { "{0}({1})" -f $_.DisplayName,$_.ProcessID}
}

Windows Presentation Foundation Font Cache 3.0.0.0(5408)
Microsoft Network Inspection(5260)
BranchCache(4112)
Windows Modules Installer(7656)
```

#### 在ForEach中调用方法

在`ForEach-Object`中, `$_`代表当前对象, 当然也允许通过`$_`,调用该对象支持的方法.
下面的例子杀死所有IE浏览器进程:

```powershell
PS C:Powershell> Get-Process iexplore

Handles  NPM(K)    PM(K)      WS(K) VM(M)   CPU(s)     Id ProcessName
-------  ------    -----      ----- -----   ------     -- -----------
    883      29    14728      22432   181    34.26   4300 iexplore
    771      28    55552     129152   425     8.56   5732 iexplore
...

PS C:Powershell> Get-Process iexplore | ForEach-Object {$_.kill()}
PS C:Powershell> Get-Process iexplore
```

### Foreach 循环

`Foreach-object` 为`cmdlet`命令, 使用在管道中, 对管道结果逐个处理,

`foreach`为遍历集合的关键字.

下面举两个例子:

```powershell
$array=7..10
foreach ($n in $array)
{
    $n*$n
}

#49
#64
#81
#100

foreach($file in dir c:\windows)
{
    if($file.Length -gt 1mb)
    {
        $File.Name
    }
}

#explorer.exe
#WindowsUpdate.log
```

这里只为了演示foreach, 其实上面的第二个例子可以用`Foreach-Object`更简洁.

```powershell
PS C:\Powershell> dir C:\Windows | where {$_.length -gt 1mb} |foreach-object {$_.Name}
explorer.exe
WindowsUpdate.log
```

### Do While 循环

`Do`和`While`可能产生死循环, 为了防止死循环的发生, 你必须确切的指定循环终止的条件.
指定了循环终止的条件后, 一旦条件不满足就会退出循环.

#### 继续与终止循环的条件

`do-while()`会先执行再去判断, 能保证循环至少执行一次.

```powershell
PS C:Powershell> do { $n=Read-Host } while( $n -ne 0)
10
100
99
2012
世界末日
为什么不退出
因为条件不满足
怎样才能满足
请输入一个0, 试一试
0
PS C:Powershell>
```

#### 单独使用While

```powershell
$n=5
while($n -gt 0)
{
    $n
    $n=$n-1
}
5
4
3
2
1
```

#### 终止当前循环

使用`continue`关键字, 可以终止当前循环, 跳过`continue`后其它语句, 重新下一次循环.

```powershell
$n=1
while($n -lt 6)
{
    if($n -eq 4)
    {
        $n=$n+1
        continue

    }
    else
    {
        $n
    }
    $n=$n+1
}
1
2
3
5
```

#### 跳出循环语句

跳出循环语句使用`break`关键字

```powershell
$n=1
while($n -lt 6)
{
    if($n -eq 4)
    {
        break
    }
    $n
    $n++
}
```

### For 循环

如果你知道循环的确切次数可以使用`For`循环, `For`循环属于计数型循环,
一旦达到最大次数, 循环就会自动终止. 下面的例子通过循环求`1-100`的数列和.

```powershell
$sum=0
for($i=1;$i -le 100;$i++)
{
    $sum+=$i
}
$sum
```

#### For循环是特殊类型的While循环

在`For`循环开始的圆括号中, 由分号隔开的语句为循环的控制条件,
分别为: 初始化, 循环执行满足的条件, 增量.

For循环的控制语句第一个和第三个可以为空:

```powershell
$sum=0
$i=1
for(;$i -le 100;)
{
    $sum+=$i
    $i++
}
$sum
```

#### For循环的特殊应用

上面的`For`循环示例停留在数字层面上, 其实`While`循环能办到的事, `For`循环也可以,
只是可能有时不方便而已. 例如判断域名的例子:

```powershell
for($domain="";!($domain -like "www.*.*");$domain=Read-Host "Input domain")
{
    Write-Host -ForegroundColor "Green" "Please give a valid domain name."
}
Please give a valid domain name.
Input domain: www
Please give a valid domain name.
Input domain: mossfly.com
Please give a valid domain name.
```

#### 逐行读取文本文件

```powershell
for($file=[IO.File]::OpenText("c:autoexec.bat") ; !($file.EndOfStream);$line=$file.ReadLine() )
{
    $line;
}
$file.Close()
REM Dummy file for NTVDM
```

### Switch 循环

`Switch`本是多路分支的关键字, 但是在`Powershell`中由于`Switch`支持集合,
所以也可以使用它进行循环处理. 下面举两个例子.

第一个将`Foreach`循环转换成`Switch`循环:

```powershell
#使用Foreach循环
$nums=10..7
foreach($n in $nums)
{
    "n=$n"
}
n=10
n=9
n=8
n=7

#使用Switch循环
$nums = 10..7
Switch ($nums)
{
Default { "n= $_" }
}

n= 10
n= 9
n= 8
n= 7
```

有时对集合的处理, 在循环中还须条件判断, 使用`Switch`循环可以一步到位, 例如:

```powershell
$nums = 10..7
Switch ($nums)
{
    {($_ % 2) -eq 0} {"$_ 偶数"}
    {($_ % 2) -ne 0} {"$_ 奇数"}
}

10 偶数
9 奇数
8 偶数
7 奇数
```
