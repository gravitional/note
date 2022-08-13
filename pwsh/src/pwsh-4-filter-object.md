# 筛选对象

## 筛选管道中的对象

通过管道可以过滤某些对象和对象的属性, 这个功能很实用,
因为很多时候我们并不是对所有的结果感兴趣, 可能只会对某些结果感兴趣.

+ `Select-Object`: `select`, 选取前几个对象如`-First 3`, 或者选取对象的`属性`.
可以用它先查看对象都有什么属性,

+ `Where-Object`:  `where`, 根据对象的 `属性`, 从对象集合中挑选特定的几个.
例如, 选择在某个日期之后创建的文件, 具有特定ID的事件, 或者使用特定版本Windows的计算机.

+ `ForEach-Object`: `foreach`, 对输入对象集合中的每个项目执行操作.
输入对象可以通过管道进入 `cmdlet`, 也可以通过使用 `InputObject` 参数指定.

+ `Get-Uinque`: `gu`, 从排序过的列表中返回不重复的对象.
+ `Select-Object -Index`: 根据 `index` 从数组中选择对象.
在 `逗号分隔的列表` 中输入索引. 数组中的索引从`0`开始, 其中`0`代表第一个值, `n-1`代表最后一个值.

比如过滤正在运行的服务,可以通过每个服务的属性`Status`进行过滤.
首先我们看看服务的属性,可以通过`Format-List *`,也可以通过`Get-memeber`.

```powershell
Get-service | Select-Object -First 1 | Format-List * #Format-List 输出对象的属性, 每行一个
```

找出`Status`为`Running`的程序, 这里是 `where-object` 的脚本块用法

```powershell
get-service | Where-Object {$_.Status -eq "Running"}
```

## 比较操作符

+ `-Contains` 包含

```powershell
Get-Process | where ProcessName -Contains "Svchost"
```

+ `-GE` 大于;

    ```powershell
    Get-Process | Where-Object -Property Handles -GE -Value 1000
    Get-Process | where Handles -GE 1000
    ```

    第一条命令使用`comparison`语句格式. 没有使用别名, 并且所有参数前带上了参数名称.
    第二个命令是更常用的格式, 使用 `where` 代替了`Where-Object ` cmdlet, 并且省略了所有可选参数名称.

+ `-Like` 通配符; 如果`property`值与包含通配符的值匹配, 则此cmdlet将获取对象.

```powershell
Get-Process | where ProcessName -Like "*host"
```

```powershell
Get-Process | where ProcessName -Like "*.pdf"
```

+ `-match` 正则表达式;

用`Get-ChildItem`显示当前当前文件的时候,会显示所有文件.有时候我们可能仅仅需要搜索或者过滤部分文件.

首先,如果是比较简单的需求,可以使用`?*`通配符来搞定,`?`用于匹配任意单个字符,`*`用于匹配任意多个字符.
比方说,我想要列出所有`.md`格式的文件,就可以使用下面的命令.

```powershell
Get-ChildItem *.md
```

有时候可能需要使用正则表达式来查找文件,考虑使用 `Get-ChildItem`+`Where-Object`,比如查找所有`.md`格式的文件,
`Where-Object`里面的`$_`是形式变量,代表每次迭代的文件.
如果了解过`C#`的`LINQ`,或者`Java 8`的流类库,应该对这种形式会比较熟悉.

```powershell
Get-ChildItem | Where-Object {$_ -match '\w*.md$'}
```

+ 查找大于`5kb`的所有 `.md` 格式文件,

```powershell
 Get-ChildItem | Where-Object {$_ -match '\w*.md$' -and $_.Length/1kb -gt 5}
```

这里到了`Powershell`的一个方便的特性,文件大小单位,`KB GB MB TB`等单位都支持.
当然其实并不仅仅可以查询文件大小属性,基本上所有文件信息都可以用来查询.

+ `Get-ChildItem` 不仅可以列出当前文件夹下的内容,还可以递归查询子文件夹. 比如查找文件夹下所有可执行文件:

```powershell
Get-ChildItem -Recurse *.exe
```

通过添加`-Depth`参数, 还可以指定递归深度.

## 拷贝特定文件类型

+ 找出所有 `jpg` 和 `png`

```powershell
Get-ChildItem -Recurse | Where-Object {$_ -match '\w*.jpg$' -or $_ -match '\w*.png$'}
```

+ 找出当前文件夹下所有图片并复制到指定路径

```powershell
Get-ChildItem -Path . -Recurse | Where-Object {$_ -match '\w*.jpg$'} | ForEach-Object {Copy-Item $_.FullName -Destination (-Join("C:\Users\Tom\Desktop\test\",$_.Name)) }
```

+ 找出当前文件夹下所有图片并复制到指定路径,并以父文件夹命名. 若要重命名项目而不复制它, 请使用`Rename-Item`.

```powershell
Get-ChildItem -Path . -Recurse | Where-Object {$_ -match '\w*.jpg$'} |
ForEach-Object {Copy-Item $_.FullName -Destination (-Join("C:\Users\Tom\Desktop\test\",$_.Directory.Name)) }
```

其中`$a[0].Directory.Name` 会给出父文件夹的名字.
