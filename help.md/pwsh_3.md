# pwsh_3

## 类与对象

使用`Get-Member -Static`来查看对象的静态属性和对象的类型.  使用`[类型名称]`, 如`[System.Enum]`来指定某个类对象, 可以调用静态方法.

### 一切皆对象

与大多数 `Shell`(它们接受和返回文本)不同,`Windows PowerShell` 是在 `.NET Framework` 公共语言运行时 (`CLR`) 和`.NET Framework` 的基础上生成的,
它将接受和返回 `.NET Framework` 对象. 每一个`Powershell`命令都会返回一个对象.包括 `comlet` `变量`,`函数`,`字符串`等等,都是对象.

属性可以描述一个对象,对象的属性可以被`Powershell`自动转换成文本,并且输出到控制台. 因此可以通过这种方法查看任何对象,例如`$host`:

```powershell
$ host
out:
Name    : ConsoleHost
Version : 2.0
```

但将对象的属性转换成文本并不安全,最安全的方式是将对象保存在变量中. 如果想将对象输出为文本,可以在控制台输入变量名.

```powershell
$FileList=dir
$FileList
```

### get-command

可以先查看存在哪些已经定义过的`命令`,`函数` etc. 别名是`gcm`, 也可以用来查看二进制程序的位置, 如`gcm unzip`.

比如,`Powershell`已经提供了许多用户能够使用的预定义函数, 这些函数可以通过`Function:PSDrive` **虚拟驱动器**查看.

```powershell
ls function:
```

其他的类似命令还有：

+ `Get-Command` 获取所有可用命令
+ `ls alias:` 获取所有别名
+ `ls function:` 获取所有已命名函数
+ `ls variable:` 查看所有已定义的变量
+ `env:windir` 驱动器变量

查看某一具体命令的信息,如 `ls` ：

```powershell
PS C:> Get-command ls
```

***
删除函数

```powershell
del Function:Get-Command
```

### 变量的作用域

+ `$global` 全局变量,在所有的作用域中有效,如果你在脚本或者函数中设置了全局变量,即使脚本和函数都运行结束,这个变量也任然有效.
+ `$script` 脚本变量,只会在脚本内部有效,包括脚本中的函数,一旦脚本运行结束,这个变量就会被回收.
+ `$private` 私有变量,只会在当前作用域有效,不能贯穿到其他作用域.
+ `$local` 默认变量,可以省略修饰符,在当前作用域有效,其它作用域只对它有只读权限.

打开`Powershell`控制台后,`Powershell`会自动生成一个新的**全局作用域**.
如果增加了函数和脚本,或者特殊的定义,才会生成其它作用域.

如果在当前控制台,只存在一个作用域,通过修饰符访问,其实访问的是同一个变量.

### 查看对象的所有成员

知道了都存在哪些对象之后,可以查看它们的具体成员.

对象的成员包括**属性**和**方法**,可以使用`Get-Member`返回它们的详细信息,

***
对象的属性

如果只显示属性可以使用参数 `memberType` 为“`Property`”

```powershell
$host | Get-Member -memberType property
```

***
对象的方法

方法定义了一个对象可以做什么事情.
当你把一个对象输出在控制台时,它的属性可能会被转换成可视的文本, 但是它的方法却不可见.
列出一个对象的所有方法可是使用`Get-Member`命令,给“`MemeberType`”参数传入“`Method`”:

```powershell
$Host | Get-Member -MemberType Method
```

***
筛选方法

`Get_` 和 `Set_` 方法

所有名称以”`get_`”打头的方法都是为了给对应的属性返回一个值.
例如”`get_someInfo()`”方法的作用就是返回属性`someInfo`的值,因此可以直接通过属性调用.
类似的像”`set_someinfo`”一样,该方法只是为了给属性`someinfo`赋值,可以直接通过属性赋值调用.

剔除包含下划线的方法可以使用操作符 `-notlike` 和 通配符 `*`

```powershell
$Host.UI.RawUI | Get-Member -me method | where {$_.Name -notlike '*_*'}
```

***
查看方法的详情

从列表中筛选出一个方法,再通过`Get-Member`得到更多的信息.

```powershell
$info=$Host.UI |  Get-Member WriteDebugLine
$info
$info.Definition
```

### 总结

`对象`=`属性`+`方法`

一个对象的属性用来存储数据,反过来这些数据又可以存储其它对象.
每一个类型都可以包含一些静态的方法,可以通过方括号和类型名称得到**类型对象本身**,如

```powershell
[System.Security.AccessControl.FileSystemRights]
```

### 对象的原型--类

上面查看的都是已经存在的`objetc`, 对象一般是由`class`生成的

#### 继承

面向对象程序设计中最重要的一个概念是继承. 继承允许我们依据另一个类来定义一个类,这使得创建和维护一个应用程序变得更容易.
这样做,也达到了重用代码功能和提高执行效率的效果.

当创建一个类时,您不需要重新编写新的数据成员和成员函数,只需指定新建的类继承了一个已有的类的成员即可.
这个已有的类称为`basetype`, 新建的类称为`派生类`. `继承`代表了`is a` 关系.例如,

    Mammal is a animal, dog is a mammal, so dog is a animal, etc.

#### 类型的简称

由类型下的某个具体对象(实例),查看类型的名称:

```powershell
(10).gettype().name
("aaa").gettype().name
("aaa","sdafsa").gettype().name
```

#### 类型的完整名称

`Powershell` 将信息存储在`对象`中,每个对象都会有一个具体的`type`, 任何`.NET`对象都可以通过`GetType()`方法返回它的类型,
该类型中有一个`FullName`属性,可以查看类型的`完整名称`.

```powershell
$Host.Version.GetType().FullName
System.Version
$Host.Version.Build
-1
```

```powershell
($Host.Version.GetType()).GetType().name
```

`($Host.Version.GetType())`的类型为`System.RuntimeType`

#### 类型的静态方法

每一个类型都可以包含一些`静态方法`,可以通过方括号和类型名称得到`类型对象本身`, 然后通过`Get-Memeber`命令查看该类型支持的所有静态方法.

```powershell
[System.DateTime] | Get-Member -static -memberType Method
```

注：
`C++`中,若类的方法前加了`static`关键字,则该方法称为`静态方法`,反之为`实例方法`.
静态方法为类所有,可以通过`对象`来使用,也可以通过`类`来使用.
但一般提倡通过`类`来使用,因为静态方法只要定义了`类`,不必建立类的`实例`就可使用.
静态方法只能调用`静态变量`.

#### 查看类的具体属性

```powershell
$Host.UI |  Get-Member WriteDebugLine
```

#### 查看基类

```powershell
[Microsoft.PowerShell.ExecutionPolicy].BaseType
```

### 创建新对象

通过 `New-Object` 创建某一类型的新对象.  查看`String`类的构造函数

```powershell
[String].GetConstructors() | foreach {$_.tostring()}
```

`| foreach {$_.tostring()}` 是为了格式化输出结果

```powershell
New-Object String('s',10)
out:
**********
```

为什么可以用这个方法? 原因是`String`类中包含一个`Void .ctor(Char, Int32)`构造函数, `.ctor` 是`构造函数(constructor)`的缩写.

#### 类型转换

通过类型转换可以替代 `New-Object`

```powershell
[System.Version]'2012.12.20.4444'

Major  Minor  Build  Revision
-----  -----  -----  --------
2012   12     20     4444
```

#### 枚举某个属性

脚本执行策略类为：`Microsoft.PowerShell.ExecutionPolicy`, 查看所有支持的执行策略：

```powershell
[System.Enum]::GetNames([Microsoft.PowerShell.ExecutionPolicy])
```

使用`[System.Enum]`类的`GetNames()`方法,查看`[Microsoft.PowerShell.ExecutionPolicy]`类的值域

#### 枚举类型

`Enum 类`:

+ 命名空间: `System`
+ 程序集: `System.Runtime.dll, mscorlib.dll, netstandard.dll`
+ 说明: 为枚举提供基类.
+ `GetNames(Type)`方法: 检索指定枚举中常数名称的数组.

[class Enum](https://docs.microsoft.com/zh-cn/dotnet/api/system.enum?redirectedfrom=MSDN&view=netframework-4.8#definition)

枚举类型是一组命名常量定义的值类型, 底层是整数类型. 要定义枚举类型, 请使用`enum`关键字并指定枚举成员的名称：

```C#
enum Season
{
    Spring,
    Summer,
    Autumn,
    Winter
}
```

默认情况下, 与枚举成员的相关的常数是`int`类型；它们从零开始, 并按照定义文本顺序增加一. 
您可以显式指定任何其他整数类型作为枚举类型的基础类型. 您还可以显式指定关联的常数值, 例如：

```C#
enum ErrorCode : ushort
{
    None = 0,
    Unknown = 1,
    ConnectionLost = 100,
    OutlierReading = 200
}
```

### .net 对象

`.NET`中的类型定义在不同的程序集中,首先得知道当前程序已经加载了那些程序集.
`AppDomain`类的静态成员`CurrentDomain`,有一个`GetAssemblies()`方法.

```powershell
[AppDomain]::CurrentDomain.GetAssemblies()
```

***
搜索指定类型

查询每个程序集中的方法可使用 `GetExportedTypes()` 方法. 因为许多程序集中包含了大量的方法,在搜索时最好指定关键字.

```powershell
[AppDomain]::CurrentDomain.GetAssemblies() | Where-Object { -not $_.IsDynamic } | ForEach-Object {$_.GetExportedTypes() } | Where-Object { $_ -like '*environment*' } | ForEach-Object { $_.FullName }

System.EnvironmentVariableTarget
System.Environment
...
```

### 自动变量

[about_Automatic_Variables](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_automatic_variables)

+ 简短说明; Automatic_Variables 是存储 `PowerShell` 的状态信息的变量.  这些变量由 `PowerShell` 创建和维护. 
+ 长说明 ; 从概念上讲, 这些变量被视为只读.  即使它们 `可以` 写入, 但为了向后兼容 , `不应写入` 它们. 

下面是 PowerShell 中的自动变量列表：

+ `$$` ; 包含会话收到的最后一行中的最后一个令牌. 
+ `$?`; 包含最后一个命令的执行状态.  如果最后一个命令成功, 它包含 `True`; 如果失败, 它包含 `False`. 

对于在管道中各个阶段运行的 cmdlet 和高级函数(例如在`process `和`end `块中), 
在任意位置调用`this.WriteError()` or `$PSCmdlet.WriteError()` 将设置`$?`为`False`,  
`this.ThrowTerminatingError()`和`$PSCmdlet.ThrowTerminatingError()` 类似.

`Write-Error` 在执行后总是立即将`$?`设置为 `False`, 但对于调用它的函数, 它不会将`$?`设置为`False`：

```powershell
function Test-WriteError
{
    Write-Error "Bad"
    $? # $false
}
```

对于后一种 用途, 应该使用`$PSCmdlet.WriteError()`.

对于本机命令 (二进制可执行程序) , 当`$LASTEXITCODE`为 `0`时, `$?`被设置为 `True`, 如果`$LASTEXITCODE` 为其他任意值, 则设置为`False`.

> 备注：
>在 `PowerShell 7`之前, 包含在括号内的语句`(...)`, 子表达式语法`$(...)`或数组表达式`@(...)`总是将`$?`重置为`True`, 因此`(Write-Error)`显示`$?`为`True`. 这一点在 `PowerShell 7` 中有所改变, `$?` 将总是反映表达式中运行的最后一条命令的实际结果. 

***

+ `$^`; 包含会话收到的最后一行中的第一个token(令牌). 
+ `$_`; 与`$PSItem`相同. 包含管道对象中的当前对象. 你可以使用这个变量, 对管道中的每个对象, 或选定的对象执行动作.
+ `$args`; 包含一个数组, 传递未声明的参数值给函数, 脚本或脚本块. 
当你创建一个函数时, 你可以通过使用`param`关键字来声明参数, 或者在函数名称后面的括号中添加一个逗号分隔的参数列表. 

在一个`event action`(事件动作)中, `$args` 变量包含代表正在处理的事件参数的对象. 这个变量只在事件注册命令的`Action`块中被填充. 
这个变量的值也可以在`Get-Event`返回的`PSEventArgs`对象的`SourceArgs`属性中找到. 
