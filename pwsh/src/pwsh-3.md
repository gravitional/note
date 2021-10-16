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

其他的类似命令还有:

+ `Get-Command` 获取所有可用命令
+ `ls alias:` 获取所有别名
+ `ls function:` 获取所有已命名函数
+ `ls variable:` 查看所有已定义的变量
+ `env:windir` 驱动器变量

查看某一具体命令的信息,如 `ls` :

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

如果只显示属性可以使用参数 `memberType` 为"`Property`"

```powershell
$host | Get-Member -memberType property
```

***
对象的方法

方法定义了一个对象可以做什么事情.
当你把一个对象输出在控制台时,它的属性可能会被转换成可视的文本, 但是它的方法却不可见.
列出一个对象的所有方法可是使用`Get-Member`命令,给"`MemeberType`"参数传入"`Method`":

```powershell
$Host | Get-Member -MemberType Method
```

***
筛选方法

`Get_` 和 `Set_` 方法

所有名称以"`get_`"打头的方法都是为了给对应的属性返回一个值.
例如"`get_someInfo()`"方法的作用就是返回属性`someInfo`的值,因此可以直接通过属性调用.
类似的像"`set_someinfo`"一样,该方法只是为了给属性`someinfo`赋值,可以直接通过属性赋值调用.

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

注:
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

脚本执行策略类为: `Microsoft.PowerShell.ExecutionPolicy`, 查看所有支持的执行策略:

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

枚举类型是一组命名常量定义的值类型, 底层是整数类型. 要定义枚举类型, 请使用`enum`关键字并指定枚举成员的名称:

```C#
enum Season
{
    Spring,
    Summer,
    Autumn,
    Winter
}
```

默认情况下, 与枚举成员的相关的常数是`int`类型; 它们从零开始, 并按照定义文本顺序增加一.
您可以显式指定任何其他整数类型作为枚举类型的基础类型. 您还可以显式指定关联的常数值, 例如:

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

### 搜索指定类型

查询每个程序集中的方法可使用 `GetExportedTypes()` 方法. 因为许多程序集中包含了大量的方法,在搜索时最好指定关键字.

```powershell
[AppDomain]::CurrentDomain.GetAssemblies() | Where-Object { -not $_.IsDynamic } | ForEach-Object {$_.GetExportedTypes() } | Where-Object { $_ -like '*environment*' } | ForEach-Object { $_.FullName }

System.EnvironmentVariableTarget
System.Environment
...
```

## 函数

[Chapter 9 - Functions](https://docs.microsoft.com/en-us/powershell/scripting/learn/ps101/09-functions?view=powershell-7.1#parameters)

## 函数的高级参数

[about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)

简要说明; 解释了如何为`高级函数`添加参数.

长描述

你可以向你编写的`高级函数`添加参数, 并使用参数 `attributes` and `arguments`, 来限制函数用户提交的参数值.

除了 PowerShell 自动添加到所有 cmdlet 和高级函数中的`常用参数`外, 你添加到函数中的参数也可以供用户使用.
关于 PowerShell 通用参数的更多信息, 请参阅about_CommonParameters.

从 PowerShell 3.0开始, 你可以使用 `@Args` 的 splitting, 来表示命令中的参数.
`Splatting` 对简单和高级函数都有效. 欲了解更多信息, 请参见 about_Functions 和 about_Splatting .

## about_Splatting

[about_Splatting](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_splatting?view=powershell-7.1)

简要说明 ; 描述了如何在 `PowerShell` 中使用 `splitting` 向命令传递参数.

### 长描述

`Splatting` 是一种将`参数值`的集合作为一个单元, 传递给命令的方法.
`PowerShell` 将集合中的每个`值`与一个`命令参数`相关联.
`拼合的参数值`存储在被命名的`拼合变量`中, 这些变量看起来像标准变量, 但以 `At` 符号(`@`)而不是美元符号(`$`)开头.
`At`符号告诉 `PowerShell`, 你传递的是`值的集合`, 而不是`单一的值`.

`拼接`(Splatting)使你的命令更短, 更容易阅读.
你可以在不同的命令调用中重复使用`拼接值`, 并使用`拼接`将参数值从 `$PSBoundParameters` 自动变量传递给其他脚本和函数.

从Windows PowerShell 3.0开始, 你也可以用`splatting`来表示一个命令的所有参数.

### 语法

```powershell
<CommandName> <optional parameters> @<HashTable> <optional parameters>
<CommandName> <optional parameters> @<Array> <optional parameters>
```

要为`位置参数`提供`参数值`, 其中不需要`参数名`, 使用`数组语法`.
要提供`参数名称`和`值`组成的对, 请使用`哈希表`语法. `拼接的值`可以出现在参数列表的任何地方(顺序无关).

当拼接时, 你不需要使用`哈希表`或`数组`来传递`所有参数`.
你可以通过使用`拼接`来传递一些参数, 并通过`位置`或 `参数名称` 来传递其他参数.
另外, 你可以在一个命令中拼接`多个对象`, 这样你就不用为每个`参数`传递一个以上的`值`.

从 PowerShell 7.1 开始, 你可以通过在命令中明确定义一个参数, 来`覆盖`一个`拼接`的参数.

### 用哈希表拼接

使用`哈希表`来拼接参数名称和值对.
你可以对所有参数类型使用这种格式, 包括`位置参数`和`switch 参数`. `位置参`数必须按`名称`分配.

下面的例子比较了两个`Copy-Item`命令, 将 `Test.txt` 文件复制到同一目录下的 `Test2.txt` 文件.

第一个例子使用传统的格式, 其中包括参数名称.

```PowerShell
Copy-Item -Path "test.txt" -Destination "test2.txt" -WhatIf
```

第二个例子使用`哈希表拼接`.
第一条命令创建了一个参数名和参数值对的`哈希表`, 并将其存储在 `$HashArguments` 变量中.
第二条命令在命令中使用 `$HashArguments` 变量 with `splatting`.
`At`符号(`@HashArguments`)取代了命令中的美元符号(`$HashArguments`).

要为 `WhatIf` 开关参数提供一个值, 使用 `$True` 或 `$False`.

```PowerShell
$HashArguments = @{
  Path = "test.txt"
  Destination = "test2.txt"
  WhatIf = $true
}
Copy-Item @HashArguments
```

> 注意;
>在第一条命令中, `At`符号(`@`)表示`哈希表`, 而不是一个`splatted`的值.
>PowerShell 中哈希表的语法是: `@{<name>=<value>; <name>=<value>; ...}`

## about_Functions_CmdletBindingAttribute

[about_Functions_CmdletBindingAttribute](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_cmdletbindingattribute?view=powershell-7.1)
