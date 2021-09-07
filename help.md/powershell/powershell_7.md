# powershell.7.md

ref: [Powershell快速入门-1][], [Powershell快速入门-2][]

[Powershell快速入门-1]: https://www.jianshu.com/p/c8f5c374466a

[Powershell快速入门-2]: https://www.jianshu.com/p/31010d0470d3

## 简介

### 启动Powershell ISE

在Windows 10下, 直接在开始菜单中输入`ISE`, 就可以打开Powershell ISE了. 

首先说明一下, 和 `Linux Shell` 不同, Powershell 的命令基本上都是`动词-名词`形式的. 这样做的好处是命令作用很容易就可以看出, 缺点就是输入稍微有些麻烦, 习惯了Linux 的简洁的同学可能会不太适应. 

Powershell 和Linux Shell 还有一个不同点在于Powershell 是基于`.NET`平台的, 它的命令叫做`cmdlet`. `cmdlet`功能比普通的`Linux`命令更强, 因为`cmdlet`接受的参数不是字符串, 而是`.NET`对象, 这使得Powershell的功能更加强大和灵活. 

### 获取命令

如果想要获取当前会话中所有可用的内置命令, 可以使用命令`Get-Command`, 它的别名是`gcm`. 

```powershell
PS C:\Users\asddf> Get-Command
```

如果希望列出指定名称的命令, 可以使用`Name`参数. 

```powershell
PS C:\Users\asddf> Get-Command -Name Get-Command
```

### 获取别名

有些命令比较常用, 除了`动词-名词`版本外, Powershell还提供了和Linux一样的别名来简化输入. 
我们可以使用`Get-Command -CommandType Alias`来显示所有的命令别名

### 获取动词/名词

当然, 如果想查找特定动词/名词的命令也是可以的. 比方说, 如果我想查找所有以`Get`开头的命令, 可以使用下面的命令. 

```powershell
PS C:\Users\asddf> Get-Command -Verb Get
```

相应的, 如果我想获取所有名词是`Help`的命令, 可以使用下面的命令. 

```powershell
PS C:\Users\asddf> Get-Command -Noun Help
```

## 变量

还有一个命令`Get-Member`, 别名是`gm`, 用于获取对象的属性. 

如果我们要获取对象的所有属性, 使用`MemberType`参数. 

```powershell
C:\Users\asddf> pwd|gm -MemberType Property
```

我们还可以在变量上调用方法,  比如说将路径转换为全小写. 

```powershell
C:\Users\asddf> $current.Path.ToLower()
```

最后, 如果不再需要一个变量, 可以使用`Remove-Variable`删除变量, 它的别名是`rv`.

## 操作符

来看看`Powershell`中支持的操作符. 

### 数学运算符

首先, 基本的数学运算符都是支持的. 

```powershell
 $i=5
 $sum=3+4*($i-3)/2
 $sum
```

前置后置自增自减运算符也是支持的. 

```powershell
 $i=0
 $i--
 $i++
 ++$i
 --$i
```

### 比较运算符

然后是比较运算符, 这些和Linux Shell中很相似, 
有大于(`-gt`), 大于等于(`-ge`), 小于(`-lt`), 
小于等于(`-le`), 等于(`-eq`), 不等于(`-ne`)几个. 

### 字符串匹配运算符

`-like`和`-notlike`用于`?*`这样的通配符. 

```powershell
 'hello' -like '?ello'
True
 'hello' -notlike '?ello'
False
 'hello' -like '*o'
True
```

`-match`和`-notmatch`用于正则表达式. 

```powershell
 'aabcc' -match 'a*b?c+'
True
 'aab' -match 'a*b?c+'
False
```

### 包含和替换运算符

`-contains`查找序列中是否包含某个元素. 

```powershell
 'hello','zhang3' -contains 'zhang3'
True
```

`-replace`用于替换字符串中某个部分, 当然正则表达式也是支持的. 

```powershell
 'hello zhang3' -replace 'zhang3','yitian'
hello yitian
```

### 分隔和连接运算符

`-split`和`-join`用于将一个字符串分为几个子部分, 或者将几个子部分组合为一个字符串. 

```powershell
 'A B C DE' -split ' '
```

```powershell
 'A','B','C' -join ','
```

上面这些运算符都是大小写不敏感的, 如果需要大小写敏感的功能, 可以在运算符前面添加`c`前缀. 

```powershell
 'yitian' -match 'Yitian'
True
 'yitian' -cmatch 'Yitian'
False
```

### 逻辑运算符

逻辑运算符有与(`-and`), 或(`-or`), 非(`-not`或`!`)以及异或(`xor`)几个, 
并且支持短路运算. 
如果需要使用真值和假值字面量, 可以使用`$true`和`$false`. 

ref: [短路运算](https://blog.csdn.net/bulebin/article/details/79345875)

1, `&&`和`||`属于逻辑运算符. 关于`&&`和`||`的表达式的运算规则是这样的：
表达式1 `||` 表达式2, 只要任意表达式为`true`, 则整个表达式的运算结果为`true`. 
表达式1 `&&` 表达式2, 只有所有表达式都为`true`, 只要任意表达式为`false`, 则整个表达式的运算结果为`false`. 
2, `&&`和`||`的短路运算: 如果在进行前面的表达式的运算过程, 
通过判断已经明确的知道整个表达式的结果, 那么就不会执行后面的表达式. 

### 类型运算符

`Powershell`和`.NET`平台绑定, 所以它是一门强类型的脚本. 
因此我们可以在脚本中判断数据的类型, 只要使用`-is`或`-isnot`运算符即可, 
类型需要写到方括号中. 这里的类型可以是所有合适的`.NET`类型. 

```powershell
 3.14 -is [Double]
True
 3.14 -isnot [Float]
True
```

### 重定向运算符

首先是`>`和`>>`运算符, 用于将标准输出流重定向到文件, 前者会覆盖已有文件, 
后者则是追加到已有文件末尾. 

然后我们来说说日志级别, 如果有使用过某些语言的日志框架的话, 就很好理解了. 
在这里, `2`代表错误, `3`代表警告, `4`代表信息, `5`代表调试信息. 
`n>`和`n>>`运算符就是用于将对应级别的输出重定向到文件的, 这两者的区别和前面相同. 
`n>&1`将对应级别的输出和标准输出一起重定向到文件. 

最后就是`*>`和`*>>`了, 这两者将所有输出信息重定向到文件. 

需要注意, Powershell使用`Unicode`编码来输出信息. 如果你需要使用其他类型的编码, 就不能使用重定向运算符了, 而应该使用`Out-File`命令. 

### 特殊运算符

`&`运算符将它后面的命令设置为后台运行, 当运行的命令需要阻塞当前终端的时候很有用. 

`.\`运算符用于执行一个脚本或命令. 如果执行的是Powershell脚本, 那么脚本会在自己的作用域中执行, 也就是说在当前环境下无法访问被执行的脚本中的变量. 

`[]`运算符用于转换变量的类型, 比如说下面的代码, 就将`pi`变量转换为了`Float`类型. 

```powershell
[Float]$pi = 3.14
$pi -is [Float]
```

`.`运算符用于调用`.NET`对象的成员, 它也可以用于执行脚本. 
当它用于执行脚本的时候, 脚本会在当前作用域中执行. 所以脚本结束之后, 我们可以访问脚本中的元素. 

`::`运算符用于调用类中的静态成员, 例如下面就会调用`.NET`平台中`DateTime`类的`Now`属性. 

```powershell
 [DateTime]::Now
```

`..`运算符用于创建一个范围闭区间, 例如下面这样. 

```powershell
 1..3
 3..1
```

`-f`运算符用于格式化数据, 例如下面这样. 
格式化方法和`C#`中的完全相同, 所以如果不熟悉的话直接看在`C#`中如何格式化数据就行了. 

```powershell
 'My name is {0},  I am {1} years old' -f 'yitian',24
My name is yitian,  I am 24 years old
```

`$`运算符可以将字符串内部的变量转换为实际的值, 例如下面这样. 
需要注意使用内插操作符的时候, 外部字符串需要使用双引号, 否则Powershell会直接输出字符串内容. 

```powershell
 $name='yitian'
 $age=24
 "My name is $name, I am $age years old."
My name is yitian, I am 24 years old.
```

`@()`运算符用于将一系列值转换为一个数组. 
假如在脚本中有一个函数可能返回0, 1或多个值, 就可以使用这个操作符, 将一系列值合并为一个数组, 方便后续处理. 

`,`逗号运算符如果放置在单个值前面, 就会创建一个包含这个值的单元素数组. 

## 循环语句

提醒一下, 不管是哪种循环语句, 在循环体内都可以使用`break`或`continue`中断/继续循环. 

### do循环

首先来看看`do-while`循环, 先执行循环体, 然后判断是否满足条件, 如果满足条件则继续执行. 

```powershell
$i = 0
do {
    $i++
    Write-Output $i
}while ($i -ne 3)
```

然后是`do-until`循环, 和`do-while`类似, 不过当条件不满足的时候才会继续循环, 如果满足条件则退出循环. 

```powershell
$i = 0
do {
    $i++
    Write-Output $i
}until ($i -eq 3)
```

### while循环

`while`循环是先判断循环条件, 满足条件时执行循环. 
$i = 0
while ($i -lt 3) {
    Write-Output $i
    $i++
}

### for循环

`for`循环可以看做是`while`循环的另一种形式, 常用于固定次数的循环. 

```powershell
for ($i = 0; $i -ne 3; $i++) {
    Write-Output $i
}
```

`foreach-object`循环

alias `foreach -> ForEach-Object`

`foreach-object`循环用于遍历一个集合中的所有元素. 

```powershell
$array = @(1, 2, 3, 4)
foreach ($i in $array) {
    Write-Output $i
}
```

值得一提的是, `foreach-object`语句用在管道上时, 还有以下一种用法. 

```powershell
<command> | foreach {<beginning command_block>}{<middle command_block>}{<ending command_block>}
```

使用这种方法时, `for-each`后面可以跟三个语句块, 第一个语句块是开始语句块, 
在循环前执行一次, 常用来初始化一些数据；
第三个是结束语句块, 在循环结束之后执行一次, 常用于统计一些循环数据；
第二个就是正常的循环语句块, 会循环多次. 

### 函数的参数

函数当然也可以带参数了, 参数列表有两种写法, 这两种方法是完全等价的:

+ 第一种是`C`风格的, 参数列表写在函数名后面, 使用小括号分隔开；
+ 第二种方式是在方法体中, 使用`param`关键字声明参数. 

`Powershell`是一种强类型的脚本语言, 所以可以在参数列表上添加参数类型, 
参数类型是可选的, 不过我还是推荐写的时候带上类型, 方便阅读和类型检查. 

```powershell
#style-1
function Say-Hello ([string] $name) {
    Write-Output "Hello, $name"
}
#style-2
function Say-Hello2 {
    param([string] $name)
    Write-Output "Hello, $name"
}
```

调用带参数的函数时, 需要向调用命令那样, 使用`-参数名`来传递参数, 例如下面这样. 

```powershell
Say-Hello -name 'yitian'
```

### 默认参数

Powershell支持默认参数, 直接用赋值号`=`在参数列表上指定参数默认值即可. 

```powershell
function Say-Hello3 {
    param([string] $name = 'zhang3')
    Write-Output "Hello, $name"
}
```

### 位置参数

`Powershell`也支持位置参数, 它会把所有参数包装到`$args`数组中, 
所以我们可以通过这个变量访问所有位置的参数. 例如下面, 将所有参数合并一个字符串, 然后打印出来. 

```powershell
function Say-Hellos {
    $names = $args -join ','
    Write-Output "Hello, $names"
}
```

这个函数调用时候需要指定多个参数, 注意不要在多个参数之间添加括号, 
否则会变成一个数组参数, 而不是多个参数. 

```powershell
Say-Hellos 'yitian' 'zhang3' 'li4'
```

### 开关参数

开关参数没有类型, 作用仅仅是标志是或者否. 
如果在使用函数的时候带上开关参数, 那么它就是开的状态, 否则就是关的状态. 
开关参数需要指定参数类型为`switch`. 

```powershell
function Answer-Hello ([switch] $yes) {
    if ($yes) {
        Write-Output "Hi"
    }
}
```

然后在调用时就可以看出区别了. 

```powershell
Answer-Hello -yes
Answer-Hello
```

### 函数返回值

最后来说说函数返回值. 这个其实也很简单, 只要使用`return`语句就可以了. 

```powershell
function Add ([double]$a, [double]$b) {
    $c = $a + $b
    return $c
}
```

然后我们调用函数, 就可以看到结果了. 

```powershell
Add -a 3 -b 5
```

关于Powershell编程的知识就介绍到这里, 其实如果看看官方文档的话, 
就知道这里介绍的也仅仅是一部分而已. 
不过这一部分对于我们日常使用和学习基本上也够用了. 

如果要查看详细帮助的话, 可以运行一下下面的命令, 这样会显示所有和Powershell相关的帮助文档. 

```powershell
Get-Help about*
```

然后, 就可以阅读自己感兴趣的部分了. 
比方说, 如果我们想了解用Powershell编写类, 就可以使用下面的命令. 

```powershell
Get-Help about_Classes
```

如果想在浏览器中浏览器在线版本, 加上`-online`参数即可. 

## 文件管理

ref: [Powershell快速入门(三) 实战应用][]

[Powershell快速入门(三) 实战应用]: https://www.jianshu.com/p/347ca5ddfc1b

### 常用命令

先来看看常用的文件管理命令. 

+ `Set-Location`命令用于切换工作目录, 它的别名是`cd`. 
+ `Get-Location`命令用于获取当前工作目录, 它的别名是`pwd`. 
+ `Get-ChildItem`命令用于获取当前目录下的所有文件. 
+ `Get-Item`命令用于获取给定文件的信息. 

还有文件移动, 删除, 复制, 粘贴, 重命名等命令, 
输入`Get-Command -Noun item`就可以看到这些命令, 这里就不做介绍了. 

### 获取文件信息

获取文件信息可以利用命令`Get-Item`. 
下面获取了我电脑上的`cmder.exe`可执行文件的信息. 

```powershell
Get-Item .\Cmder.exe
```

默认只列出三个属性, 当然其实文件属性远不止这些. 
我们可以通过管道, 将文件信息对象传递给命令`Select-Object`, 让它帮我们显示所有属性. 
这里只粘贴了一点点内容, 其实文件信息很长, 大家可以自行尝试. 

```powershell
Get-Item .\.vimrc | Select-Object *
```

### 过滤文件

用`Get-ChildItem`显示当前当前文件的时候, 会显示所有文件. 有时候我们可能仅仅需要搜索或者过滤部分文件. 

首先, 如果是比较简单的需求, 可以使用`?*`通配符来搞定, 问号用于匹配任意单个字符, 星号用于匹配任意多个字符. 
比方说, 我想要列出所有`.md`格式的文件, 就可以使用下面的命令. 

```powershell
Get-ChildItem *.md
```

有时候可能需要使用正则表达式来查找文件, 可以使用`Where-Object`命令来自定义查询. 
如果了解`C#`语言的`LINQ`的话, 应该可以猜到, 这个命令对应于`LINQ`的`where`语句. 

下面同样是查找所有`.md`格式的文件, 不过这次使用了`Where-Object`和正则表达式, 
其中`Where-Object`里面的`$_`是形式变量, 代表每次迭代的文件. 
如果了解过`C#`的`LINQ`, 或者`Java 8`的流类库, 应该对这种形式会比较熟悉. 

```powershell
Get-ChildItem | Where-Object {$_ -match '\w*.md$'}
```

如果仅仅为了搜索文件名的话, 这种方式好像一点优势都没有. 实际上`Where-Object`的功能非常强大. 
比方说, 我现在想查找大于`5kb`的所有`.md`格式文件, 那么就可以这么写. 
这里又用到了`Powershell`的一个方便的特性, 文件大小单位, `KB GB MB TB`等单位都支持. 
当然其实并不仅仅可以查询文件大小属性, 基本上所有文件信息都可以用来查询. 

```powershell
 Get-ChildItem | Where-Object {$_ -match '\w*.md$' -and $_.Length/1kb -gt 5}
```

最后, `Get-ChildItem` 不仅可以列出当前文件夹下的所有内容, 
还可以递归查询所有子文件夹. 
比方说, 我要查找一下迅雷文件夹下所有可执行文件, 
就可以使用下面的命令. 如果添加`-Depth`参数的话, 还可以指定递归深度. 

```powershell
Get-ChildItem -Recurse *.exe
```

### 修改hosts

访问谷歌的一种方式就是更改`hosts`文件. 这里就用Powershell做一个修改`hosts`的功能. 

首先先来介绍一个命令`Invoke-WebRequest`, 
利用它我们可以获取网页内容, 下载文件甚至是填写表单. 
这个命令的别名是`iwr`, `curl`和`wget`. 我们就使用它来下载网上的`hosts`文件. 

剩余就没有什么难度了, 无非就是读写文件, 追加文件, 复制和粘贴这种基本操作. 
最后写完这个功能发现有一百多行, 就不往这里复制粘贴了. 
如果有兴趣的话, 可以直接看[我的Github上面的脚本](https://github.com/techstay/powershell-study/blob/master/scripts/update-hosts.ps1). 

## 进程管理

### 查看进程

首先我们看看有多少和进程相关的命令, 这个很简单, 只要查看一下名词是`Process`的命令即可. 

```powershell
Get-Command -Noun process
```

使用这些命令, 我们就可以非常方便的管理进程了. 
比方说, 我想查询现在运行的所有进程, 就可以使用下面的命令, 
这样就会列出所有运行的进程, 就像任务管理器里显示的那样. 

```powershell
Get-Process
```

上面这个命令会显示所有进程. 
如果需要, 我们可以按照某个属性对进程进行排序显示, 这需要使用另外一个命令`Sort-Object`. 
另外, 如果只需要显示前几个进程, 可以使用命令`Select-Object`来选择显示多少数据. 
比方说, 如果我们要查看当前占用CPU前5的`firefox`进程, 就可以使用下面的命令. 

```powershell
Get-Process firefox | Sort-Object cpu -Descending | Select-Object -First 5
```

利用这几个命令, 我们可以按照任何想要的方式来查询进程. 

### 管理进程

先来看看`MSDN`上的一个官方例子. 
首先先打开三个记事本进程, 然后使用名称获取这些进程, 
然后调用进程的`Kill()`函数即可把这些进程全杀掉. 
中间调用了`Count`属性测试了一下总共获取到了几个进程. 

```powershell
notepad;notepad;notepad;
$notepads=Get-Process -Name notepad
$notepads.Count
3
$notepads.Kill()
```

再学习Powershell编程的时候, 我们常常会同时开几个Powershell窗口. 
不再使用的时候一个一个关闭它们也是一件麻烦事情, 
所以官方文档还为我们介绍了如何关闭除当前窗口外的所有Powershell进程. 

每个Powershell进程都有一个变量`$PID`, (当前powershell的`ID`)
用于标志当前进程的进程号, 
利用这一点我们就可以实现这个功能. 
这里的`-WhatIf`参数表示不真正关闭进程, 仅列出将要关闭的进程. 

```powershell
Get-Process powershell | Where-Object {$_.Id -ne $PID} | Stop-Process -WhatIf
WhatIf: 正在目标“powershell (2676)”上执行操作“Stop-Process”. 
```

如果既想要关闭进程, 还想知道关闭了哪些进程, 可以使用`-PassThru`参数. 

```powershell
Get-Process powershell | Where-Object {$_.Id -ne $PID} | Stop-Process -PassThru
```

### 轮询关闭进程

如果在死循环中不断查找任务管理器进程, 发现它在运行就把它关闭, 
就可以做一个小小的“病毒”. 代码很简单, 基本上一下子就能看懂. 
一开始我没有加`Sleep`, 然后CPU使用率飚的非常高, 加了之后基本上对电脑性能没有影响了. 

```powershell
$process_name = "taskmgr"
while ($true) {
    $processes = Get-Process
    if ($processes.Name -contains $process_name) {
        Get-Process $process_name|Stop-Process
    }
    else {
        Start-Sleep -Milliseconds 500
    }

}
```

如果把上面代码中的`taskmgr`换成英雄联盟的进程名字, 
我们就可以做一个简单的“熊孩子防火墙”, 防止熊孩子用电脑来玩游戏了. 

## 注册表操作

### 读取注册表

首先来介绍一下注册表根的简写, 
例如`HKEY_CURRENT_USER`的简写就是`HKCU`, 
`HKEY_LOCAL_MACHINE`的简写就是`HKLM`. 
知道了简写, 我们就可以将Powershell的工作目录切换到注册表内. 
例如, 如果我们想查看`HKEY_CURRENT_USER\Control Panel\Desktop\MuiCached`下的值, 
就可以先把工作目录切换到这个位置内部. 
这里需要将对应的注册表根修改为对应的简写加冒号的形式. 

```powershell
Set-Location 'HKCU:\Control Panel\Desktop\MuiCached'
PS HKCU:\Control Panel\Desktop\MuiCached>
```

切换到了注册表内部, 我们就可以利用`Get-Item`命令获取注册表的值了. 
比如说, 要获取这个注册表键的值, 就可以直接输入`Get-Item .`了. 
注意这个点不能省去, 它代表当前工作目录. 

```powershell
HKCU:\Control Panel\Desktop\MuiCached> Get-Item .
```

如果要获取当前注册表项的属性值, 可以利用`Get-ItemProperty`命令. 

```powershell
Get-ItemProperty .  MachinePreferredUILanguages
```

当然, 切换工作目录这件事情也可以不做. 
直接利用`Get-ItemProperty`命令通过路径参数来获取属性. 

```powershell
Get-ItemProperty -Path 'HKCU:\Control Panel\Desktop\MuiCached' -Name MachinePreferredUILanguages
```

### 编辑注册表项

下面这个路径是一个安全的注册表路径, 在这里修改注册表不会造成严重的系统问题. 
所以我们把它保存为一个变量. 

```powershell
$path = "HKCU:\Control Panel\Desktop"
```

如果要新建注册表项, 可以使用`New-Item`命令. 
我们可以使用注册表编辑器`regedit`来验证项是否创建成功. 

```powershell
New-Item –Path $path –Name HelloKey
```

如果要修改项的属性, 使用`Set-ItemProperty`命令. 

```powershell
Set-ItemProperty -path $path\hellokey -name Fake -Value fuck
```

最后, 如果要删除项的属性, 使用`Remove-ItemProperty`命令. 

```powershell
Remove-ItemProperty -path $path\hellokey -name Fake
```

如果要删除整个注册表项, 使用`Remove-Item`命令. 

```powershell
Remove-Item -path $path\hellokey -Recurse
```

### 获取当前.NET版本

下面的参考资料中列出了一个`MSDN`上的文档, 
告诉我们如何读取注册表的值来判断当前安装了`.NET Framework`的版本, 
并给出了相应的`C#`代码. 下面的代码做的就是将`C#`代码改写成`Powershell`脚本. 

```powershell
# 判断系统当前安装.NET框架版本的脚本

$path = 'HKLM:\SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full'
$not_found_msg = ".net framework 4.5 or later not installed on your system"

function CheckFor45PlusVersion([int] $releaseKey) {
    if ($releaseKey -ge 460798) {
        return "4.7 or later";
    }
    if ($releaseKey -ge 394802) {
        return "4.6.2";
    }
    if ($releaseKey -ge 394254) {
        return "4.6.1";
    }
    if ($releaseKey -ge 393295) {
        return "4.6";
    }
    if (($releaseKey -ge 379893)) {
        return "4.5.2";
    }
    if (($releaseKey -ge 378675)) {
        return "4.5.1";
    }
    if (($releaseKey -ge 378389)) {
        return "4.5";
    }
    return $not_found_msg;
}


try {
    $key = get-item $path
    $releaseKey = Get-ItemPropertyValue $path 'release'
    if ($releaseKey -is [int]) {
        $releaseKey = [int]$releaseKey
        $version = CheckFor45PlusVersion($releaseKey)
        Write-Host ".NET framework ${version}"
    }
    else {
        Write-Host $not_found_msg
    }
}catch {
    Write-Host $not_found_msg
}
```

## Office互操作

这里的操作好像过时了

### 操作Excel

虽然Powershell可以通过`COM`接口和`Office`程序交互, 
不过最常用的还是操作`Excel`, 所以我这里只介绍如何控制`Excel`表. 
参考的操作`Excel`的文章:[powershell-read-excel-file-using-com][],
可能需要梯子才能访问. 
需要注意一点, 既然是操作`Excel`, 当然首先电脑上需要先安装`Excel`才能正常使用. 

[powershell-read-excel-file-using-com]: http://www.lazywinadmin.com/2014/03/powershell-read-excel-file-using-com.html

### 打开和关闭

首先, 我们来创建一个Excel对象, 这样实际上会创建一个`Excel`应用程序. 

```powershell
$excel = New-Object -ComObject Excel.Application
```

执行了上面的命令, 什么事情都没有发生. 
这是因为默认启动的实例是隐藏的, 要显示`Excel`的窗口的话, 将它设置为可见即可. 

```powershell
$excel.Visible=$true
```

如果要打开一个现成的工作簿, 使用`Open`函数. 

```powershell
$workbook = $excel.Workbooks.Open("XXX.xlsx")
```

如果要创建一个新的工作簿, 使用`Add`函数. 

```powershell
$workbook = $excel.Workbooks.Add()
```

一个工作簿可以有多个工作表, 要选择某一个工作表, 
使用`Worksheets.Item`属性, 需要注意这里的下标从一开始. 

```powershell
$worksheet = $workbook.Worksheets.Item(1)
```

对数据完成操作之后, 需要保存的话, 使用`SaveAs`函数. 

```powershell
$workbook.SaveAs("D:\Desktop\hello.xlsx")
```

### 操作数据

前面只说了打开和关闭操作, 下面来看看如何具体读取和写入数据. 
首先回到上面那步工作表, 因为如果要操作数据, 需要在工作表对象上进行操作. 

```powershell
$worksheet = $workbook.Worksheets.Item(1)
```

要操作数据, 调用工作表对象的`Cells`属性即可. 
比方说我要打印九九乘法表, 那么就可以用类似下面的代码来迭代写入数据. 

```powershell
for ($i = 1; $i -le 9; ++$i) {
    # 第一行
    $worksheet.Cells(1, $i + 1) = $i
    # 第一列
    $worksheet.Cells($i + 1, 1) = $i
    # 它们的乘积
    for ($j = 1; $j -le 9; ++$j) {
        $worksheet.Cells($i + 1, $j + 1) = $i * $j
    }
}
```

操作之后, `Excel`表中应该存在数据. 

### 写入数据

类似的, 读取数据也很简单, 只要读取`Cells`属性即可. 

```powershell
for ($i = 1; $i -le 10; ++$i) {
    for ($j = 1; $j -le 10; ++$j) {
        Write-Host -NoNewline $worksheet.Cells($i, $j).Text "`t"
    }
    Write-Host
}
```

上面的代码获取了我们刚才写入`Excel`的数据, 然后将其转换为文本并输出, 
每个数据之间使用制表符`\t`分隔, 注意`Powershell`中的转义字符使用的这个特殊字符. 
结果应该类似如图所示. 

### 绘制图表

Excel很常用的一种操作就是绘制图表, 这里也简单说说. 
不过由于这种资料在网上面实在太少, 我就算用谷歌搜索英文网页也搜不出来多少资料, 
大部分都属于一点小脚本. 所以这里只能随便说说了. 

首先准备一下数据, 我准备了如下所示的数据. 

| 姓名 | 语文 | 数学 | 英语 |
| ----- | ----- | ----- | ----- |
| 小王 | 56 | 67 | 45 |
| 小明 | 12 | 89 | 98 |
| 小李 | 78 | 33 | 63 |
| 小黄 | 53 | 45 | 53 |

然后来创建一个图表对象. 如果使用交互式环境`Powershell ISE`的话, 
智能提示会显示这里有`AddChart`和`AddChart2`两个方法, 
不过我看了下文档, 前面那个过时了, 所以这里使用带`2`的那个版本. 

```powershell
$chart=$worksheet.Shapes.AddChart2().Chart
```

创建了图表对象之后, 我们为它指定数据源. 

```powershell
$chart.SetSourceData($worksheet.Range('a1', 'd5'))
```
