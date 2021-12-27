# 字符串和数字

commandline chapter 35

在这一章中,我们将查看几个用来操作字符串和数字的 shell 功能.
shell 提供了各种执行字符串操作的参数展开功能.
除了算术展开(在第七章中接触过),还有一个常见的命令行程序叫做 `bc`,能执行更高级别的数学运算.

## 字符串总结

parameter 前面可能出现的保留字,`!`, `#`
parameter 后面可能接的保留字,`:` `#` `%` `/`

+ 返回变量名的参数展开
+ `${!prefix*}` :这种展开会返回以 `prefix` 开头的已有变量名
+ `${#parameter}` :展开成由 parameter 所包含的字符串的长度.
+ 空变量的展开
+ `${parameter:-word}` :若 parameter 没有设置(例如,不存在)或者为空,展开结果是 word 的值
+ `${parameter:=word}` :若 parameter 没有设置或为空,展开结果是 word 的值.另外,word 的值会赋值给 parameter.
+ `${parameter:?word}`:若 parameter 没有设置或为空,这种展开导致脚本带有错误退出,并且 word 的内容会发送到标准错误.
+ `${parameter:+word}`:若 parameter 为空,结果为空.若 parameter 不为空, word 的值; parameter 的值不改变.
+ 字符串展开
+ `${parameter:offset}` :从 `parameter` 所包含的字符串中提取一部分字符,到结尾
+ `${parameter:offset:length}` :从 `parameter` 所包含的字符串中提取一部分字符,`length`制定长度
+ 字符串修剪
+ `${parameter#pattern}` :从 `paramter` 所包含的字符串中清除开头的`pattern`
+ `${parameter##pattern}` :## 模式清除最长的匹配结果.
+ `${parameter%pattern}` :清除 `parameter` 末尾所包含的`pattern`
+ `${parameter%%pattern}` :%% 模式清除最长的匹配结果.
+ 字符串查找和替换操作 `parameter`必须是一个变量 `pattern` 和 `string` 可以不加引号
+ `${parameter/pattern/string}` :如果找到了匹配通配符 `pattern` 的文本, 则用 `string` 的内容替换它.
+ `${parameter//pattern/string}` : `//` 形式下,所有的匹配项都会被替换掉
+ `${parameter/#pattern/string}` :` /# `要求匹配项出现在字符串的开头,
+ `${parameter/%pattern/string}` :`/%` 要求匹配项出现在字符串的末尾

## 参数展开

尽管参数展开在第七章中出现过, 但我们并没有详尽地介绍它, 因为大多数的参数展开会用在脚本中, 而不是命令行中.
我们已经使用了一些形式的参数展开, 例如: `shell 变量`, shell 提供了更多方式.

## 基本参数展开

最简单的参数展开形式反映在平常使用的变量上.

在这个例子中,我们试图创建一个文件名,通过把字符串 `_file` 附加到变量 `a` 的值的后面.

```bash
[me@linuxbox ~]$ a="foo"
[me@linuxbox ~]$ echo "$a_file"
```

如果我们执行这个序列,没有任何输出结果,因为 `shell` 会试着展开一个称为 `a_file` 的变量,而不是 `a`.通过添加花括号可以解决这个问题:

```bash
[me@linuxbox ~]$ echo "${a}_file"
foo_file
```

我们已经知道通过把数字包裹在花括号中,可以访问大于`9`的位置参数. 例如,访问第十一个位置参数,我们可以这样做: `${11}`

## 管理空变量的展开

`null`
`undefined`
`defined`

几种用来处理 `不存在` 和 `空变量` 的参数展开形式.
这些展开形式对于解决丢失的 `位置参数` 和给参数指定 `默认值` 的情况很方便.

```bash
${parameter:-word}
```

若 `parameter` 没有设置(例如,不存在)或者为空,展开结果是 `word` 的值.
若 `parameter` 不为空,则展开结果是 `parameter` 的值.

```bash
foo=
echo ${foo:-"substitute value if unset"}
echo $foo
foo=bar
echo ${foo:-"substitute value if unset"}
echo $foo
```

```bash
${parameter:=word}
```

若 `parameter` 没有设置或为空,展开结果是 `word` 的值.另外,`word` 的值会赋值给 `parameter`.
若 `parameter` 不为空,展开结果是 `parameter` 的值.

```bash
foo=
echo ${foo:="default value if unset"}
unset
echo $foo
unset
foo=bar
echo ${foo:="default value if unset"}
echo $foo
```

注意: `位置参数` 或其它的特殊参数不能以这种方式赋值.

```bash
${parameter:?word}
```

若 `parameter` 没有设置或为空,这种展开导致脚本带有错误退出,并且 `word` 的内容会发送到标准错误.
若 `parameter` 不为空, 展开结果是 `parameter` 的值.

```bash
foo=
echo ${foo:?"parameter is empty"
echo $?
foo=bar
echo ${foo:?"parameter is empty"}
echo $?
```

```bash
${parameter:+word}
```

若 `parameter` 没有设置或为空,展开结果为空.
若 `parameter` 不为空, 展开结果是 `word` 的值会替换掉 `parameter` 的值;
然而, `parameter` 的值不会改变.

```bash
foo=
echo ${foo:+"substitute value if set"}
foo=bar
echo ${foo:+"substitute value if set"}
```

## 返回变量名的参数展开

`shell` 具有返回变量名的能力. 这会用在一些相当独特的情况下.

```bash
${!prefix*}
${!prefix@}
```

这种展开会返回以 `prefix` 开头的已有变量名. 根据 `bash` 文档,这两种展开形式的执行结果相同.
这里,我们列出了所有以 `BASH` 开头的环境变量名:

```bash
[me@linuxbox ~]$ echo ${!BASH*}
BASH BASH_ARGC BASH_ARGV BASH_COMMAND BASH_COMPLETION
...
```

## 字符串展开

有大量的展开形式可用于操作 `字符串`. 其中许多展开形式尤其适用于 `路径名` 的展开.

```bash
${#parameter}
```

展开成由 `parameter` 所包含的字符串的长度.

通常,`parameter` 是一个字符串;然而,如果 `parameter` 是 `@`或者是 `*` 的话, 则展开结果是位置参数的个数.

```bash
[me@linuxbox ~]$ foo="This string is long."
[me@linuxbox ~]$ echo "'$foo' is ${#foo} characters long."
'This string is long.' is 20 characters long.
```

***

```bash
${parameter:offset}
${parameter:offset:length}
```

这些展开用来从 `parameter` 所包含的字符串中提取一部分字符.
提取的字符始于第 `offset` 个字符(从字符串开头算起)直到字符串的末尾,除非指定提取的长度.

```bash
[me@linuxbox ~]$ foo="This string is long."
[me@linuxbox ~]$ echo ${foo:5}
string is long.
[me@linuxbox ~]$ echo ${foo:5:6}
string
```

若 `offset` 的值为负数,则认为 `offset` 值是从字符串的末尾开始算起,而不是从开头.
注意负数前面必须有一个空格, 为防止与 `${parameter:-word}` 展开形式混淆.`length`,若出现,则必须不能小于零.
如果 `parameter` 是 `@`,展开结果是 `length` 个位置参数,从第 offset 个位置参数开始.

```bash
[me@linuxbox ~]$ foo="This string is long."
[me@linuxbox ~]$ echo ${foo: -5}
long.
[me@linuxbox ~]$ echo ${foo: -5:2}
lo
```

***

```bash
${parameter#pattern}
${parameter##pattern}
```

这些展开会从 `paramter` 所包含的字符串中清除开头一部分文本,这些字符要匹配定义的 `patten` .
pattern 是通配符模式,就如那些用在路径名展开中的模式.
这两种形式的差异之处是该 `#` 形式清除最短的匹配结果, 而该`##` 模式清除最长的匹配结果.

```bash
[me@linuxbox ~]$ foo=file.txt.zip
[me@linuxbox ~]$ echo ${foo#*.}
txt.zip
[me@linuxbox ~]$ echo ${foo##*.}
zip
```

***

```bash
${parameter%pattern}
${parameter%%pattern}
```

这些展开和上面的 `#` 和 `##` 展开一样,除了它们清除的文本从 `parameter` 所包含字符串的末尾开始,而不是开头.

```bash
[me@linuxbox ~]$ foo=file.txt.zip
[me@linuxbox ~]$ echo ${foo%.*}
file.txt
[me@linuxbox ~]$ echo ${foo%%.*}
file
```

***

```bash
${parameter/pattern/string}
${parameter//pattern/string}
${parameter/#pattern/string}
${parameter/%pattern/string}
```

这种形式的展开对 `parameter` 的内容执行查找和替换操作.

如果找到了匹配通配符 `pattern` 的文本, 则用 `string` 的内容替换它.
在正常形式下,只有第一个匹配项会被替换掉.在 `//` 形式下,所有的匹配项都会被替换掉.
` /# `要求匹配项出现在字符串的开头,而 `/%` 要求匹配项出现在字符串的末尾.
`/string` 可能会省略掉,这样会导致删除匹配的文本.

```bash
[me@linuxbox~]$ foo=JPG.JPG
[me@linuxbox ~]$ echo ${foo/JPG/jpg}
jpg.JPG
[me@linuxbox~]$ echo ${foo//JPG/jpg}
jpg.jpg
[me@linuxbox~]$ echo ${foo/#JPG/jpg}
jpg.JPG
[me@linuxbox~]$ echo ${foo/%JPG/jpg}
JPG.jpg
```

知道 `参数展开` 是件很好的事情. `字符串展开` 操作可以用来替换其它常见命令比方说 `sed` 和 `cut`.
通过减少使用外部程序,展开提高了脚本的效率.

举例说明, 我们将修改在之前章节中讨论的 `longest-word` 程序,
用参数展开`${#j}` 取代命令 `$(echo $j | wc -c)` 及其`subshell` ,像这样:

`wc`- print newline, word, and byte counts for each file

```bash
#!/bin/bash
# longest-word3 : find longest string in a file
for i; do
   if [[ -r $i ]]; then
      max_word=
      max_len=
      for j in $(strings $i); do
  len=${#j}
  if (( len > max_len )); then
     max_len=$len
     max_word=$j
  fi
      done
      echo "$i: '$max_word' ($max_len characters)"
   fi
   shift
done
```

下一步,我们将使用 `time` 命令来比较这两个脚本版本的效率:

```bash
[me@linuxbox ~]$ time longest-word2 dirlist-usr-bin.txt
dirlist-usr-bin.txt: 'scrollkeeper-get-extended-content-list' (38 characters)
real 0m3.618s
user 0m1.544s
sys 0m1.768s
[me@linuxbox ~]$ time longest-word3 dirlist-usr-bin.txt
dirlist-usr-bin.txt: 'scrollkeeper-get-extended-content-list' (38 characters)
real 0m0.060s
user 0m0.056s
sys 0m0.008s
```

原来的脚本扫描整个文本文件需耗时`3.168`秒,
而使用参数展开的新版本, 仅仅花费了`0.06`秒  --  一个非常巨大的提高.

## 通配符/Wildcard/glob

[Shell中的通配符](https://www.jianshu.com/p/25f3d0cd5fdc)

`glob()`, glob: 一滴 一团

`glob()`函数根据`shell`使用的规则搜索所有与模式匹配的路径名 (请参阅`glob(7)`)
没有`tilde expansion`或`parameter substitution`;  如果需要这些,请使用`wordexp(3)`.

`globfree()`函数释放先前调用`glob()`时,动态分配的存储空间 .
`man 7 glob()` see glob(7)

在 `Shell` 中命令中,通常会使用通配符表达式来匹配一些文件,如以下命令可以查找当前目录下所有后缀为 `.xml` 的文件

```bash
find . -name "*.xml"
```

Shell 中可以使用的通配符如下:

| 通配符| 含义| 实例|
|--|--|--|
|`*`|  匹配 `0` 或多个字符   | `a*b`,`a`与`b`之间可以有任意长度的任意字符, 也可以一个也没有, 如 `aabcb`, `axyzb`, `a012b`, `ab`|
| `?`| 匹配任意单个字符   |`a?b`,`a`与`b`之间有且只有一个字符, 可以是任意字符,如 `aab`, `abb`, `acb`, `a0b`|
| `[list]`|  匹配 `list` 中的任意单个字符 | `a[xyz]b`,`a`与`b`之间必须也只能有一个字符, 但只能是 `x` 或 `y` 或 `z`, 如 `axb`, `ayb`, `azb`.|
|`[!list]`|  匹配除 `list` 中的任意单一字符 |  `a[!0-9]b`,`a`与`b`之间必须也只能有一个字符, 但不能是阿拉伯数字, 如 `axb,` `aab`, `a-b`.|
|`[c1-c2]`| 匹配 `c1-c2` 中的任意单一字符 | `a[0-9]b`,匹配`0`与`9`之间其中一个字符,如 `a0b`, `a1b`... `a9b`|
| `{s1,s2,...}` | 匹配 `s1` 或 `s2` (或更多)中的一个字符串 |`a{abc,xyz,123}b`,`a`与`b`之间只能是`abc`或`xyz`或`123`这三个字符串之一|
| `[[:class:]]`| 匹配任意一个属于指定字符类中的字符 | `*[[:lower:]123]`,以小写字母开头,或者以`1`,`2`,`3`结尾的文件 |

常用字符类

+ `[:alnum:]` : 匹配任意一个字母或数字
+ `[:alpha:]` :  匹配任意一个字母
+ `[:digit:]` : 匹配任意一个数字
+ `[:lower:]` : 匹配任意一个小写字母
+ `[:upper:]` : 匹配任意一个大写字母

## 转义字符

有的时候,我们匹配的内容里面会存在 `*`,`?`,`[`等通配符中的符号.
为了表示他们原来的意思,我们需要使用转义字符 `\`,如 `a\[ac\]c` 表示匹配 `a[a]c` 或 `a[c]c`.

`\ `本身用` \\` 表示.

## 字符切割

分字 word splitting

[Shell_Linux Shell 中实现字符串切割的几种方法](ttps://blog.csdn.net/u010003835/article/details/80750003)
[refs1](https://blog.csdn.net/u010003835/article/details/80749220)
[refs2](https://blog.csdn.net/whuslei/article/details/7187639)

***
`shell` 的 `for` 参数可以是一个连续的字符串,用`IFS`分割

```bash
#!/bin/bash
string="hello shell split test"  ; for var in ${string[@]}; do echo -e "$var EOF" ; done
###
echo test2
string="hello shell split test"
for var in ${string}
do
   echo -e "$var EOF"
done
```

***
我们在 `shell` 脚本编程中,经常需要用到字符串切割,即将字符串切割为一个数组,
类似 `java` 中的`split`函数,下面对几种常见的方式做一个总结.

+ 利用 `shell` 中 变量 的字符串替换
+ 设置分隔符,通过 `IFS` 变量
+ 利用`tr` 指令实现字符替换  (!只能针对单个分隔符)

***
方法一:利用 `shell` 中变量的字符串替换

示例:

```bash
#!/bin/bash
string="hello,shell,split,test"
array=(${string//,/ })

for var in ${array[@]}
do
   echo -e "$var \n"
done
```

***
方法二: 设置分隔符,通过 `IFS `变量

原理:自定义IFS变量, 改变分隔符, 对字符串进行切分

`IFS` 介绍

`Shell` 脚本中有个变量叫 `IFS`(Internal Field Seprator) ,**内部域分隔符**.

`Shell` 的环境变量分为 `set`, `env` 两种,其中 `set` 变量可以通过 `export` 工具导入到 `env` 变量中.
其中,`set` 是显示设置 `shell` 变量,仅在本 `shell` 中有效; `env` 是显示设置用户环境变量 ,仅在当前会话中有效.
换句话说,`set` 变量里包含了 `env` 变量,但 `set` 变量不一定都是 `env` 变量.
这两种变量不同之处在于变量的作用域不同.显然,`env` 变量的作用域要大些,它可以在 `subshell` 中使用.

而 `IFS` 是一种 `set` 变量,当 `shell` 处理"命令替换"和"参数替换"时, `shell` 根据 `IFS` 的值,默认是 `space`, `tab`, `newline` 来拆解读入的变量,然后对特殊字符进行处理,最后重新组合赋值给该变量.

***
`IFS` 简单实例

查看变量 `IFS` 的值.

```bash
$ echo $IFS

$ echo "$IFS" | od -b
0000000 040 011 012 012
0000004
```

直接输出IFS是看不到的,把它转化为二进制就可以看到了, `040`是空格,`011`是`Tab`,`012`是换行符`\n` .
最后一个 `012` 是 `echo`输出的(`echo` 默认会换行的).

```bash
SAVEIFS=$(echo -en "\0040\0011\0012"); # 默认的IFS
```

示例

```bash
#!/bin/bash

string="hello,shell,split,test"

#对IFS变量 进行替换处理
OLD_IFS="$IFS"
IFS=","
array=($string)
IFS="$OLD_IFS"

for var in ${array[@]}
do
   echo -e $var\n
done
```

***
方法三: 利用`tr`指令实现字符替换

原理: 由于只是对单个字符进行的替换,则可以用  `echo args |   tr "oldSpilt" "newSpilt"`  的方式实现.

`tr` 指令讲解: `tr`命令可以对来自标准输入的字符进行替换,压缩和删除.

语法:`tr(选项)(参数)`

选项

+ `-c`或`--complerment`:取代所有不属于第一字符集的字符;
+ `-d`或`--delete`:删除所有属于第一字符集的字符;
+ `-s`或`--squeeze-repeats`:把连续重复的字符以单独一个字符表示;
+ `-t`或`--truncate-set1`:先删除第一字符集较第二字符集多出的字符.

参数

+ `字符集1`:指定要转换或删除的原字符集.当执行转换操作时,必须使用参数`字符集2`指定转换的目标字符集.
但执行删除操作时,不需要参数`字符集2`;
+ `字符集2`:指定要转换成的目标字符集.

示例:

```bash
#!/bin/bash

string="hello,shell,split,test"
array=(`echo $string | tr ',' ' '` )

for var in ${array[@]}
do
   echo -e $var
done
```
