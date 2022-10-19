# h5dump

[h5dump (1) - Linux Man Pages](https://www.systutorials.com/docs/linux/man/1-h5dump/)

`h5dump`: 显示HDF5文件的内容.
在Linux中显示h5dump手册的命令:  `man 1 h5dump`

```bash
h5dump [OPTIONS] file
```

描述

+ `h5dump` 使用户能够检查HDF5文件的内容, 并将这些内容以 human-readble 形式转储(dump)到 `ASCII` 文件.
+ `h5dump` 将 `HDF5` 文件的内容转储到标准输出.
它可以显示整个 `HDF5` 文件的内容或选定的对象, 这些对象可以是 `组`, `数据集`,
`数据集` 的一个 `子集`, `链接`, `属性` 或 `数据类型`.

+ `--header` 选项只显示对象的 `标题信息`.

+ `名称`是 对象的绝对名称.
`h5dump` 显示对象的顺序与命令的顺序相同.
如果一个名字不是以斜线开头, `h5dump` 会从 `root group` 开始搜索指定的对象.

+ 如果一个对象有多个名称, 且名称都是硬链接的,
`h5dump` 会在第一次出现时显示该对象的 `内容`. 在后来的出现中只显示 `链接信息`.

+ h5dump 为任何未命名的数据类型分配一个名字,
形式为 `#oid1:oid2`, 其中 `oid1` 和 `oid2` 是库分配的对象标识符.
未命名的类型(types)会显示在根组中.

+ 数据类型以标准类型名称显示. 例如, 如果一个数据集以 `H5T_NATIVE_INT` 类型创建, 而该机器上整数的标准类型名是 `H5T_STD_I32BE`,
`h5dump` 就会显示 `H5T_STD_I32BE` 作为数据集的类型.

+ `h5dump` 也可以转储一个数据集的子集.
这个功能的操作方式与 `HDF5` 中的 `hyperslab` 基本相同;
在 命令行中指定的参数被传递给函数 `H5Sselect_hyperslab`, 然后显示所选择的内容.

+ `h5dump` 的输出在 DDL for HDF5, 即 `数据描述语言`(Data Description Language)文件中有详细描述.

注意: 不允许用`一个标志`指定多个 `属性`, `数据集`, `数据类型`, `组` 或 `软链接`.
例如, 我们不能发出这样的命令

```bash
h5dump -a /attr1 /attr2 foo.h5 #错误
```

来同时显示 `/attr1` 和 `/attr2`. 我们必须发出以下命令.

```bash
h5dump -a /attr1 -a /attr2 foo.h5 #正确
```

可以通过使用 `--filedriver` (`-f`) 命令行选项来选择打开 `HDF5` 文件的 文件驱动.
`--filedriver` 选项的可接受值是:
`sec2`, `family`, `split`, `multi`, and `stream`.
如果没有指定文件驱动标志,
那么文件将按照上面指定的顺序依次用每个驱动打开, 直到一个驱动成功打开文件.

## XML OUTPUT

使用 `--xml` 选项, `h5dump` 会生成 `XML` 输出.
这个输出包含了文件的完整描述, 用XML标记.
该 `XML` 符合[HDF5文档类型定义(DTD)](http://hdf.ncsa.uiuc.edu/DTDs/HDF5-File.dtd.)

`XML` 输出适合与其他工具一起使用, 包括 `HDF5 Java` 工具.

## 选项

-h 或 --help 打印一个使用信息并退出.
-B 或 --bootblock 打印启动块的内容. (这个选项尚未实现).
-H 或 --header 只打印标题, 不显示数据.
-A 打印标题和属性值;不显示数据集的数据.
-i 或 --object-ids 打印对象的ids.
-r 或 --string 以ASCII格式打印1字节的整数数据集.
-V 或 --version 打印版本号并退出.
-a P 或 --attribute=P 打印指定的属性.
-d P 或 --dataset=P 打印指定的数据集.
-f D 或 --filedriver=D 指定用哪个驱动程序打开文件.
-g P 或 --group=P 打印指定的组和所有成员.
-l P 或 --soft-link=P 打印指定的软链接的值.
-o F 或 --output=F 将原始数据输出到F文件.
-t T 或 --datatype=T 打印指定的命名数据类型.
-w N 或 --width=N 设置输出的列数.
-x 或 --xml 使用XML模式(默认)而不是DDL输出XML.
-u 或 --use-dtd 使用XML DTD而不是DDL输出XML.
-D U 或 --xml-td=U 在XML输出中, 参考位于U的DTD或模式, 而不是默认的模式/DTD.
-X S 或 --xml-dns=S 在XML输出中, (XML模式)在XML中使用限定名称.
":": 没有命名空间, 默认: "hdf5:"

-s L 或 --start=L 子集选择的起始点的偏移. 默认值: 数据集的开头.
-S L 或 --stride=L Hyperslab stride. 默认值: 所有维度都是1.
-c L 或 --count=L 在选择中包括的块的数量.
-k L 或 --block=L 超文本中的块的大小. 默认值: 所有尺寸都是1.

-- 表示所有下面的参数都是非选项.
例如, 要转储一个名为 `-f` 的文件, 使用 `h5dump -- -f`.

`file`; 要检查的文件.

### 上面列出的选项参数定义如下

+ `D`;
在打开文件时使用哪个文件驱动.
可接受的值是 "sec2", "family", "split", "multi "和 "stream".
如果没有文件驱动标志, 文件将按照上面指定的顺序依次用每个驱动打开, 直到一个驱动成功打开文件.

+ `P`; 从根组到对象的完整路径
+ `T`; 数据类型的名称
+ `F`; 一个文件名
+ `N`; 一个大于1的整数
+ `L`; 一个整数的列表, 其数量等于被查询的数据空间的维数
+ `U`; 一个URI(定义于[IETF RFC 2396], 由[IETF RFC 2732]更新),
指的是用来验证XML的DTD.

子集(Subsetting )参数也可以用一种方便的紧凑形式来表达, 如下所示.

```bash
--dataset="/foo/mydataset[START;STRIDE;COUNT;BLOCK]"
```

所有的分号(`;`)都是必须的, 即使没有指定参数值. 当没有指定时, 将使用默认参数值.

### 举例说明

1. Dumping 文件 quux.h5 中的组 `/GroupFoo/GroupBar`.

```bash
h5dump -g /GroupFoo/GroupBar quux.h5
```

2. Dumping 文件 quux.h 5中 `/GroupFoo/GroupBar` 组中的数据集 `Fnord`.

```bash
h5dump -d /GroupFoo/GroupBar/Fnord quux.h5
```

3. Dumping 文件 quux.h5 中 `/GroupFoo/GroupBar` 组中的数据集 `Fnord` 的 `属性元数据`.

```bash
h5dump -a /GroupFoo/GroupBar/Fnord/metadata quux.h5
```

4. Dumping 文件 `quux.h5` 中属于 `根组属性` 的元数据.

```bash
h5dump -a /metadata quux.h5
```

5. 产生一个文件 `bobo.h5` 的 `XML` 列表.

```bash
h5dump --xml bobo.h5 > bobo.h5.xml
```

6. Dumping 文件 `quux.h5` 中的数据集 `/GroupFoo/databar/` 的一个子集

```bash
h5dump -d /GroupFoo/databar --start="1,1" --stride="2,3" --count="3,19" --block="1,1" quux.h5
```

7. 同样的例子, 使用简短的形式来指定子集的参数.

```bash
h5dump -d "/GroupFoo/databar[1,1;2,3;3,19;1,1]" quux.h5
```

## 当前状态

h5dump的当前版本显示以下信息:

+ 组
    + 组的属性(见Attribute)
    + 组成员

+ 数据集
    + 数据集属性(参见Attribute).
    + 数据集类型(见数据类型).
    + 数据集空间(见数据空间)
    + 数据集数据

+ 属性
    + 属性类型(见数据类型).
    + 属性空间(见数据空间).
    + 属性数据

+ 数据类型
    + 整数类型
        - H5T_STD_I8BE, H5T_STD_I8LE, H5T_STD_I16BE, ...
    + 浮点类型
        - H5T_IEEE_F32BE, H5T_IEEE_F32LE, H5T_IEEE_F64BE, ...
    + 字符串类型
    + 复合类型
        - 命名的, 未命名的和暂存(transient )的复合类型--整数, 浮点或字符串类型成员
    + 不透明(opaque)的类型
    + 引用类型
        + 对象引用
        + 数据区域
    + 枚举类型
    + 变长的数据类型
        - 仅限原子类型atomic types)
        - 支持标量或单维数组的可变长度类型

+ 数据空间
    + 标量和简单空间

+ 软链接
+ 硬链接
+ 循环检测

## SEE ALSO

h5ls(1), h5diff(1), h5repart(1), h5import(1), gif2h5(1), h52gif(1), h5perf(1)

+ [HDF5 Data Description Language syntax](file:///usr/share/doc/libhdf5-doc/html/ddl.html)
+ [HDF5 XML Schema](http://hdf.ncsa.uiuc.edu/DTDs/HDF5-File.xsd)
+ [HDF5 XML information ](http://hdf.ncsa.uiuc.edu/HDF5/XML/)

## Pages related to h5dump

+ h5diff (1) - Compares two HDF5 files and reports the differences.
+ h5c++ (1) - Helper script to compile HDF5 C++ applications.
+ h5cc (1) - Helper script to compile HDF5 applications.
+ h5copy (1) - Copies HDF5 objects from a file to a new file
+ h5fc (1) - Reports statistics regarding an HDF5 file and the objects in the file.
+ h5import (1) - Imports data into an existing or new HDF5 file.
+ h5jam (1) - Add a user block to a HDF5 file