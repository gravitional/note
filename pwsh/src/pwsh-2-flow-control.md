# 创建脚本

### 传递参数

+ 传递给一个函数或者一个脚本的参数都保存在`$args`变量中. 默认情况下,传递给`Powershell`脚本的参数类型为`数组`,例如:

```powershell
PS E:> .MyScript.ps1 My Website      Is        www.mossfly.com
Hello,My Website Is www.mossfly.com
```

上面的文本中包含多个连续的空格,可是当脚本把参数输出时却不存在连续的空格了.那是因为脚本会把文本根据白空格截断并转换成数组.
如果不想文本被当成数组那就把它放在引号中.

```powershell
PS E:> ./MyScript.ps1 "My Website      Is        www.mossfly.com"
Hello,My Website      Is        www.mossfly.com
```

+ 因为`$args`是一个`数组`,自然可以通过索引访问数组的每一个元素. 可以将`MyScript.sp1`的内容改为:

```powershell
For($i=0;$i -lt $args.Count; $i++)
{
    Write-Host "parameter $i : $($args[$i])"
}
```

然后在控制台测试:

```powershell
PS E:> .\MyScript.ps1 www moss fly com
parameter 0 : www
...
```

+ 在脚本中使用参数名

通过`Powershell`传递参数固然方便,但是如果用户不知道参数的传递顺序,也是很郁闷的.
所以最好的方式给参数指定名称,输入以下的脚本:

```powershell
param($Directory,$FileName)

"Directory= $Directory"
"FileName=$FileName"
```

其中`param`给参数指定名称.

执行脚本:

```powershell
PS E:> .\MyScript.ps1 -Directory $env:windir -FileName config.xml
Directory= C:windows
FileName=config.xml
PS E:> .\MyScript.ps1 -FileName config.xml -Directory $env:windir
Directory= C:windows
FileName=config.xml
```

## 流程控制

### Throw异常

[about_Throw](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_throw)
[about_Try_Catch_Finally](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_try_catch_finally)

+ 长描述;  `Throw` 关键字会导致一个`terminating`错误. 你可以使用`Throw`关键字来停止一个命令, 函数或脚本的处理.

例如, 你可以在`If`语句的脚本块中使用`Throw`关键字来响应一个条件, 或者在`Try-Catch-Finally`语句的`Catch`块中使用.
你也可以在一个参数声明中使用`Throw`关键字, 使一个函数参数成为强制性的.
`Throw`关键字可以抛出任何对象, 如用户信息字符串或导致错误的对象.

Throw关键字的语法如下.

```powershell
Throw [<表达式>]
```

`Throw`语法中的表达式是可选的. 当`Throw`语句没有出现在`Catch`块中, 并且不包括表达式时, 它会产生一个`ScriptHalted`错误.

```powershell
C:\PS> throw
Exception: ScriptHalted
```

如果在`Catch`块中使用`Throw`关键字, 且没有表达式, 它会再次抛出当前的`RuntimeException`. 更多信息请参见`about_Try_Catch_Finally`.

+ `Throw` 语句中的可选表达式可以是一个字符串:

```powershell
C:\PS> throw "This is an error."
```

+ 表达式也可以抛出代表PowerShell进程的对象, 如下例所示,

```powershell
C:\PS> throw (get-process Pwsh)
```

### if 条件判断

`Where-Object` 进行条件判断很方便,如果在判断执行代码段,可以使用`IF-ELSEIF-ELSE`语句.语句模板:

```powershell
If (条件1){
如果条件1满足就执行代码
}
elseif (条件2)
{
如果条件2满足执行代码
}
else
{还不满足}
```

条件判断必须放在圆括号中,执行的代码必须紧跟在后面的花括号中.

```powershell
$n=8
if($n -gt 15) {"$n  大于 15 " }
if($n -gt 5) {"$n  大于 5 " }
8  大于 5
if($n -lt 0 ){"-1" } elseif($n -eq 0){"0"} else {"1"}
1
```

### Switch

[about_Switch](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_switch)
[about_Automatic_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables)

`If`语句可以检查多种类型的条件, 包括变量的值和对象的属性.
若要检查多个条件, 请使用 `Switch` 语句.  `Switch`语句等效于一系列语句 `If` , 但更简单.
语句 `Switch` 列出每个条件和可选操作.  如果某个条件满足, 则执行该操作. `Switch`语句可以使用`$_ `,  `$switch` 自动变量.

+ `$switch`; 包含`enumerator`而不是`Switch`语句的结果值.
`$switch`变量只在`Switch`语句运行时存在; 当`Switch`语句执行完毕时, 它将被删除. 欲了解更多信息, 请参见`about_Switch`.
枚举器包含属性和方法, 你可以用来检索循环值和改变当前循环迭代. 更多信息, 请参见`Using Enumerators`.

#### 基本用法

```powershell
Switch (<test-value>)
{
    <condition> {<action>}
    <condition> {<action>}
}
```

默认情况下, 所有的条件都会被检测,

```powershell
switch (3)
{
    1 {"It is one."}
    2 {"It is two."}
    3 {"It is three."}
    4 {"It is four."}
    3 {"Three again."}
}
```

如果不想检测所有条件, 使用`Break`

```powershell
switch (3)
{
    1 {"It is one."}
    2 {"It is two."}
    3 {"It is three."; Break}
    4 {"It is four."}
    3 {"Three again."}
}
```

如果输入是一个`collection`, 每个元素会按照顺序测试,

```powershell
switch (4, 2)
{
    1 {"It is one." }
    2 {"It is two." }
    3 {"It is three." }
    4 {"It is four." }
    3 {"Three again."}
}
```

`Break`对整个集合生效, 而不是对单个元素

```powershell
switch (4, 2)
{
    1 {"It is one."; Break}
    2 {"It is two." ; Break }
    3 {"It is three." ; Break }
    4 {"It is four." ; Break } # 输出 It is four. 跳出 switch
    3 {"Three again."}
}
```

#### 完全体switch

```powershell
switch [-regex|-wildcard|-exact][-casesensitive] (<value>)
{
    "string"|数字|变量| { 表达式 }    { 执行的操作 }
    default { 执行的操作 }
}
# 或者
switch [-regex|-wildcard|-exact][-casesensitive] -file filename
{
    "string"|数字|变量|{ 表达式 } { 执行的操作 }
    default { 执行的操作 }
}
```

如果没有使用参数, `Switch`的行为相当于使用`Exact`参数.  它对`<value>`进行不区分大小写的`match`.
如果值是一个集合, 按照出现每个元素都的顺序对其运算.

`Switch`语句必须包括至少一个条件语句. 当`<value>`不符合任何条件时, 会触发`Default`子句.
每个`Switch`语句中只允许有一个`Default`子句.

`Switch` 有以下参数;

+ `Wildcard` ; 表示条件是通配符字符串. 如果匹配子句不是字符串, 该参数将被忽略. 比较是不区分大小写的.
+ `Exact` ; 如果匹配子句是字符串, 则必须完全匹配. 如果匹配子句不是字符串, 这个参数将被忽略. 比较是不区分大小写的.
+ `CaseSensitive` ; 执行大小写敏感的匹配. 如果匹配子句不是字符串, 这个参数被忽略.
+ `File` ; 从文件, 而不是从值语句中获取. 如果包含多个`File`参数, 只使用最后参数. 文件的每一行都被读取并由`Switch`语句进行运算, 不分大小写的.
+ `Regex` ; 执行值与条件的正则表达式匹配. 如果匹配子句不是字符串, 这个参数将被忽略. 比较是不区分大小写的. `$matches`自动变量可在匹配语句块中使用.

注意事项 : 当指定冲突的值时, 如`Regex`和`Wildcard`, 最后指定的参数优先, 所有冲突的参数被忽略. 也允许参数的多个实例. 然而, 只有最后使用的参数才是有效的.

在这个例子中, 若传递不是字符串或数字对象给`Switch`. `Switch`对该对象进行强制字符串转换, 再对结果进行运算.

```powershell
$test = @{
    Test  = 'test'
    Test2 = 'test2'
}
$test.ToString() # 输出 System.Collections.Hashtable
switch -Exact ($test)
{
    'System.Collections.Hashtable' {'Hashtable string coercion'}
    'test' {'Hashtable value'}
} # 输出 Hashtable string coercion
```

使用正则表达式的例子, 检测网站协议.

```powershell
$target = 'https://bing.com'
switch -Regex ($target)
{
    '^ftp\://.*$' { "$_ is an ftp address"; Break }
    '^\w+@\w+\.com|edu|org$' { "$_ is an email address"; Break }
    '^(http[s]?)\://.*$' { "$_ is a web address that uses $($matches[1])"; Break }
}
```

`Switch`语句的`statement`条件可以是:

+ 表达式, 它的值将会与输入值进行比较
+ 脚本块, 如果条件得到满足, 它应该返回`$true`.

`$_`自动变量包含传递给`switch`语句的值, 可在条件语句的范围内进行运算和使用. 每个条件的动作都与其他条件的动作无关.
下面的例子演示了使用脚本块作为开关语句的条件.

```powershell
switch ("Test")
{
    {$_ -is [String]} {"Found a string"}
    "Test" {"This $_ executes as well" }
}
```

如果值符合多个条件, 则执行每个条件的动作. 要改变这种行为, 可以使用`Break`或`Continue`关键字.

+ `Break`关键字停止处理并退出`Switch`语句.
+ `Continue` 关键字停止处理当前值, 但继续处理任何后续值.

下面的例子是处理一个数组并显示它们是奇数还是偶数. 负数用`Continue`关键字跳过. 如果遇到一个非数字, 则用`Break`关键字终止执行. `[Int32]`表示`32`位整数类型.

```powershell
switch (1,4,-1,3,"Hello",2,1)
{
    {$_ -lt 0} { Continue }
    {$_ -isnot [Int32] } { Break }
    {$_ % 2} { "$_ is Odd" }
    {-not ($_ % 2)} {"$_ is Even"}
}
```

### for, foreach

+ `for`循环可以看做是`while`循环的另一种形式,常用于固定次数的循环.

```powershell
for ($i = 0; $i -ne 3; $i++) {
    Write-Output $i
}
```

+ `foreach`循环用于遍历一个集合中的所有元素.

[关于 ForEach](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_foreach)
[流控制](https://docs.microsoft.com/zh-cn/powershell/scripting/learn/ps101/06-flow-control)

```powershell
$array = @(1, 2, 3, 4)
foreach ($i in $array) {
    Write-Output $i
}
```

值得一提的是,`foreach-object`语句用在管道上时,还有以下一种用法.

```powershell
<command> | foreach {<beginning command_block>}{<middle command_block>}{<ending command_block>}
```

使用这种方法时,`for-each`后面可以跟三个语句块,第一个语句块是开始语句块,
在循环前执行一次,常用来初始化一些数据;
第三个是结束语句块,在循环结束之后执行一次,常用于统计一些循环数据;
第二个就是正常的循环语句块,会循环多次.

### while循环

`while`循环是先判断循环条件, 满足条件时执行循环.
$i = 0
while ($i -lt 3) {
    Write-Output $i
    $i++
}
