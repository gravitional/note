# 从shell眼中看世界

## 字符展开

传递到 `echo` 命令的任何参数都会在(屏幕上)显示出来.  让我们试一个例子:

```bash
echo *
Desktop Documents ls-output.txt Music Pictures Public Templates Videos
```

为什么 `echo` 不打印` * `呢? 答案就是在 `echo` 命令被执行前,
shell 把` * `展开成了另外的东西(在这种情况下,就是在当前工作目录下的文件名字).

当回车键被按下时,`shell` 在命令被执行前在命令行上自动展开任何符合条件的字符, 所以 `echo` 命令从不会发现` * `,只把它展开成结果.
知道了这个以后,我们能看到 `echo` 执行的结果和我们想象的一样.

## 路径名展开

这种通配符工作机制叫做 `路径名展开`. 我们能够执行以下参数展开模式:

```bash
echo D*
Desktop Documents
```

和:

```bash
echo *s
Documents Pictures Templates Videos
```

```bash
甚至是:
echo [[:upper:]]*
Desktop Documents Music Pictures Public Templates Videos
```

查看家目录之外的目录:

```bash
echo /usr/*/share
/usr/kerberos/share /usr/local/share
```

## 隐藏文件路径名展开

正如我们知道的,以圆点字符开头的文件名是 `隐藏文件`. 路径名展开也尊重这种 行为. 像这样的展开:

```bash
echo *
```

不会显示隐藏文件. 要是展开模式以圆点开头,我们就能够在展开模式中包含隐藏文件,
而且隐藏文件可能会出现在第一位置,就像这样:

```bash
echo .*
```

然而,如果我们仔细检查一下输出结果,我们会看到名字`.` 和`..`也出现在结果中.
因为这些名字是指当前工作目录和它的父目录,使用这种 模式可能会产生不正确的结果.
我们能看到这样的结果,如果我们试一下这个命令:
`ls -d .* | less`
为了在这种情况下正确地完成路径名展开,我们应该使用一个更精确些的模式.  这个模式会正确地工作:
`ls -d .[!.]?*`
这种模式展开成为文件名,每个文件名以圆点开头,第二个字符不包含圆点,再包含至少一个字符, 并且这
个字符之后紧接着任意多个字符.

这将列出大多数的隐藏文件 (但仍将不能包含以多个圆点开头的文件名)

这个带有 `-A` 选项(`几乎所有`)的`ls`命令能够提供一份正确的隐藏文件清单:

```bash
ls -A
```

## 波浪线展开

可能你从我们对 cd 命令的介绍中回想起来,波浪线字符(` ~ `)有特殊的意思.
当它用在 一个单词的开头时,它会展开成指定用户的家目录名,如果没有指定用户名,则是当前用户的家目录:

```bash
echo ~
/home/me
```

如果有用户`foo`这个帐号,然后:

```bash
echo ~foo
/home/foo
```

## 算术表达式展开

`shell` 允许算术表达式通过展开来执行. 这允许我们把 `shell` 提示当作计算器来使用:
算术表达式展开使用这种格式:`$((expression))`

```bash
echo $((2 + 2))
4
```

算术表达式只支持整数(全部是数字,不带小数点),但是能执行很多不同的操作.
这里是 一些它支持的操作符:

操作符 说明

+ `+` 加
+ `-` 减
+ `*` 乘
+ `/` 除(但是记住,因为展开只是支持整数除法,所以结果是整数. )
+ `%` 取余,只是简单的意味着,`余数`
+ `**` 取幂

在算术表达式中空格并不重要,并且表达式可以嵌套. 
例如, `5` 的平方乘以 `3`:

```bash
echo $(($((5**2)) * 3))
```

一对括号可以用来把多个子表达式括起来. 通过这个技术,我们可以重写上面的例子:

```bash
echo $(((5**2) * 3))
```

这是一个使用除法和取余操作符的例子. 注意整数除法的结果:

```bash
echo Five divided by two equals $((5/2))
Five divided by two equals 2
echo with $((5%2)) left over.
with 1 left over.
```

在35章会更深入的讨论算术表达式的内容.

## 花括号展开

可能最奇怪的展开是花括号展开. 
通过它, 你可以从一个包含花括号的模式中创建多个文本字符串. 例:

```bash
echo Front-{A,B,C}-Back
Front-A-Back Front-B-Back Front-C-Back
```

`花括号展开` 模式可能包含一个 开头部分叫做 `报头`,一个结尾部分叫做 `附言`.
花括号表达式本身可能包含由 `逗号` 分开的字符串列表,或者一系列整数,或者单个的字符串.
这种模式不能嵌入空白字符. 这个例题使用了一系列整数:

```bash
echo Number_{1..5}
```

一系列以倒序排列的字母:

```bash
echo {Z..A}
```

花括号展开可以嵌套(会自动展平):

```bash
echo a{A{1,2},B{3,4}}b
```

那么这对什么有好处呢?最普遍的应用是,创建一系列的文件或目录列表.

```bash
mkdir {2007..2009}-0{1..9} {2007..2009}-{10..12}
```

## 参数展开

在这一章我们将会简单地介绍参数展开,只是皮毛而已.
这个特性在 `shell` 脚本中比直接在命令行中更有用. 它的许多性能和系统存储小块数据,并给每块数据命名的能力有关系.

许多像这样的小块数据, 更适当些应叫做变量,可以方便地检查它们.
例如,叫做`USER`的变量包含你的用户名. 唤醒参数展开,揭示 `USER` 中的内容,可以这样做:

```bash
echo $USER
me
```

查看有效的变量列表,试试这个:

```bash
printenv | less
```

你可能注意到其它展开类型,如果你误输入一个模式,展开就不会发生.
这时 `echo` 命令只简单地显示误键入的模式. 通过参数展开,如果你拼写错了一个变量名, 展开仍然会进行,只是展成一个空字符串:

```bash
echo $SUER
```

## 命令替换

命令替换允许我们把一个命令的输出作为一个展开模式来使用:

```bash
echo $(ls)
```

我最喜欢用的一行命令是像这样的:

```bash
ls -l $(which cp)
-rwxr-xr-x 1 root root 71516 2007-12-05 08:58 /bin/cp
```

这里我们把 `which cp` 的执行结果作为一个参数传递给 `ls` 命令,因此要想得到 `cp` 程序的输出列表,
不必知道它完整的路径名.

我们不只限制于简单命令. 也可以使用整个管道线 (只展示部分输出):

```bash
file $(ls /usr/bin/* | grep zip)
....
```

在这个例子中,管道线的输出结果成为 `file` 命令的参数列表.

在旧版 shell 程序中,有另一种语法也支持命令替换,可与刚提到的语法换使用.
`bash` 也支持这种语法. 它使用 `倒引号` 来代替 `美元符号` 和 `括号`:

```bash
ls -l `which cp`
```

我们已经知道 shell 有许多方式可以完成展开,现在是时候学习怎样来控制展开了.

## 双引号

我们将要看一下引用的第一种类型, `双引号`. 如果你把文本放在双引号中, `shell` 使用的特殊字符,
除了`\`(反斜杠),`$` ,和 `` ` ``(倒引号)之外, 则失去它们的特殊含义, 被当作普通字符来看待.

这意味着下列展开被禁止:

+ 单词分割, (`空格`)
+ 路径名展开, (`*``?`)
+ 波浪线展开,(`~`)
+ 和花括号展开(`{}`)

而下列展开仍然执行:

+ 参数展开(`$USER`)
+ 算术展开(`$(())`)
+ 命令替换`$()`

使用 `双引号`,我们可以处理包含空格的文件名. 比方说我们是不幸的名为 `two words.txt` 文件的受害者.
如果我们试图在命令行中使用这个 文件,**单词分割机制**会导致这个文件名被看作两个独自的参数,而不是所期望的单个参数:

```bash
ls -l two words.txt
ls: cannot access two: No such file or directory
ls: cannot access words.txt: No such file or directory
```

使用双引号,我们可以阻止单词分割,得到期望的结果;进一步,我们甚至可以修复 破损的文件名.

```bash
ls -l `two words.txt`
mv `two words.txt` two_words.txt
```

记住,在双引号中,**参数展开**,**算术表达式展开**,和**命令替换**仍然有效:

```bash
echo `${USER} $((2+2)) $(cal)`
me 4
```

在默认情况下,单词分割机制会在单词中寻找**空格**,**制表符**,和**换行符**,并把它们看作单词之间的界定符. 它们只作为分隔符使用.

如果我们加上双引号,单词分割被禁止,内嵌的空格也不会被当作界定符,它们成为参数的一部分.
一旦加上双引号,我们的命令行就包含一个带有一个参数的命令.

考虑下面的例子:

```bash
echo $(cal)
echo `$(cal)`
```

在第一个实例中,没有引用的命令替换导致命令行包含`38`个参数.
在第二个例子中, 命令行只有一个参数,参数中包括嵌入的**空格**和**换行符**.

## 单引号

如果需要禁止所有的展开,我们使用单引号. 以下例子是无引用,双引号,和单引号的比较结果:

```bash
echo text ~/*.txt {a,b} $(echo foo) $((2+2)) $USER
echo `text ~/*.txt {a,b} $(echo foo) $((2+2)) $USER`
echo 'text ~/*.txt {a,b} $(echo foo) $((2+2)) $USER'
```

正如我们所看到的,随着引用程度加强,越来越多的展开被禁止.

## 转义字符

有时候我们只想引用单个字符. 我们可以在字符之前加上一个反斜杠,在这个上下文中叫做转义字符.
经常在双引号中使用转义字符,来有选择地阻止展开.

```bash
echo `The balance for user $USER is: \$5.00`
The balance for user me is: $5.00
```

使用转义字符来消除文件名中一个字符的特殊含义,是很普遍的. 例如,在文件名中可能使用一些对于 `shell` 来说,有特殊含义的字符.
这些字符包括`$`, `!`, `空格`等字符.
注意在单引号中,反斜杠失去它的特殊含义,它 被看作普通字符.

## 反斜杠转义字符序列

反斜杠除了作为转义字符外,反斜杠也是一种表示法的一部分,这种表示法代表某种特殊字符,叫做控制码.
`ASCII`编码表中前`32`个字符被用来把命令转输到像电报机一样的设备.

一些编码是众所周知的(制表符,退格符,换行符,和回车符),其它一些编码就不熟悉了(空值,传输结束码,和确认).

***
转义序列 含义

+ `\a` : 响铃(`警告`-导致计算机嘟嘟响)
+ `\b` : 退格符
+ `\n` : 新的一行. 在类 Unix 系统中,产生换行.
+ `\r` : 回车符
+ `\t` : 制表符

上表列出了一些常见的反斜杠转义字符.
反斜杠表示法背后的思想来源于 C 编程语言, 许多其它语言也采用了这种表示方法,包括 shell.

`echo` 命令带上 `-e` 选项,能够解释转义序列. 你可以把转义序列放在`$' '`里面.

以下例子,使用 `sleep` 命令,一个简单的程序,它会等待指定的秒数,然后退出.
我们可以创建一个简单的倒数计数器:

```bash
sleep 2; echo -e `Time's up\a`
```

我们也可以这样做:

```bash
sleep 2; echo `Time's up` /span>\a'
```

## shell 语法

### 空白字符

[对C标准中空白字符的理解](https://blog.csdn.net/boyinnju/article/details/6877087)
[Shell中去掉文件中的换行符简单方法](https://blog.csdn.net/Jerry_1126/java/article/details/85009615)

`C`标准库里`<ctype.h>`中声明了一个函数:

`int isspace(int c);`

该函数判断字符`c`是否为一个空白字符.

`C`标准中空白字符有六个:
空格(`' '`), 换页(`'\f'`), 换行(`'\n'`), 回车(`'\r'`), 水平制表符(`'\t'`), 垂直制表符(`'\v'`)

***
空格: ASCII码为`0x20`,而不是`0x00`.`0x00`代表空(`NULL`)

`0X00-0XFF` `16`进制一共`256`个,刚好是一个`bit`的范围.

***
回车('\r')效果是输出回到本行行首,结果可能会将这一行之前的输出覆盖掉,例如执行:

```bash
puts("hello world!\rxxx");
#在终端输出的是:
xxxlo world!
```

如果将上面的字符串写入文件中,例如执行:

```bash
char *s = "hello world!\rxxx";
FILE *str = fopen("t.txt","r");
fwrite(s, 16, 1, str);
```

用文本编辑器打开`t.txt`.显示的效果将由打开的编辑器所决定.
vi将`\r`用`^M`代替,而记事本就没有显示该字符.

***
换行('\n')
顾名思义,换行就是转到下一行输出.例如:

```bash
puts("hello\nworld!");
#在终端中将输出
hello
world!
```

但需要注意的是,终端输出要达到换行效果用``\n``就可以,但要在文本文件输出中达到换行效果在各个系统中有所区别.
在`*nix`系统中,每行的结尾是"`\n`",windows中则是"`\n\r`",mac则是"`\r`".

***
水平制表符('\t')

相信大家对'\t'还是比较熟悉的.一般来说,其在终端和文件中的输出显示相当于按下键盘`TAB`键效果.
一般系统中,显示水平制表符将占8列.同时水平制表符开始占据的初始位置是第`8*n`列(第一列的下标为0).例如:

```bash
puts("0123456\txx");
puts("0123456t\txx");
```

***
垂直制表符('\v')

垂直制表符不常用.它的作用是让`'\v'`后面的字符从下一行开始输出,且开始的列数为``\v``前一个字符所在列后面一列.例如:

```bash
puts("01\v2345");
```

***
换页('\f')

换页符的在终端的中的效果相当于`*nix`中`clear`命令.
终端在输出`'\f'`之后内容之前,会将整个终端屏幕清空空,然后在输出内容.给人的该觉是在`clear`命令后的输出字符串.

最后我想说明一点,`\t \r, \v \f`也是控制字符,它们会控制字符的输出方式.
它们在终端输出时会有上面的表现,但如果写入文本文件,一般文本编辑器(vi或记事本)对`\t \r, \v \f`的显示是没有控制效果的.

### shell 换行

把换行符注释掉,如果同时想插入注释,可以用`$()`或者两个`backtick`包裹注释

```bash
emcc -o ./dist/test.html `# 目标文件` \
--shell-file ./tmp.html `# 模板文件` \
--source-map-base dist `# source map 根路径` \
-O3 `# 优化级别` \
```

### 删除换行符

文件中每行都以`\n`结尾,如果要去掉换行符,使用`sed`命令

```bash
[root@host ~]# sed -i 's/\n//g' FileName
```

或者使用`tr`命令: tr - translate or delete characters

```bash
[root@host ~]# cat fileName | tr  -d '\n'
```

有一种简单的方法:

`xargs` - build and execute command lines from standard input

 ```bash
cat FileName | xargs | echo -n   # 连文件末尾换行符也去掉
# 或者
cat FileName | xargs           # 会保留文件末尾的换行符
 ```

### eval

[Shell 中eval的用法](https://blog.csdn.net/luliuliu1234/article/details/80994391)

```bash
eval command-line
```

其中`command-line`是在终端上键入的一条普通命令行.
然而当在它前面放上`eval`时,其结果是`shell`在执行命令行之前扫描它两次.如:

```bash
$ pipe="|"
$ eval ls $pipe wc -l
1
2
3
```

shell第1次扫描命令行时,它替换出`pipe`的值`|`,接着`eval`使它再次扫描命令行,这时shell把`|`作为管道符号了.

如果变量中包含任何需要`shell`直接在命令行中看到的字符,就可以使用eval.
命令行结束符(`;  |  &`),I/o重定向符(`< >`)和引号就属于对shell具有特殊意义的符号,必须直接出现在命令行中.

`eval echo \$$#`取得最后一个参数, 如:

```bash
$ cat last    #此处last是一个脚本文件,内容是下一行显示
$  eval echo \$$#
$ ./last one two three four

four
```

第一遍扫描后,shell把反斜杠去掉了.当shell再次扫描该行时,它替换了`$4`的值,并执行echo命令

***
以下示意如何用`eval`命令创建指向变量的`指针`:

```bash
x=100
ptrx=x
eval echo \$$ptrx  #指向 ptrx,用这里的方法可以理解上面的例子
eval $ptrx=50 #将 50 存到 ptrx 指向的变量中.
echo $x
```

```bash
# ptrx 指向x
echo $ptrx
x
# \$ 转义之后,再跟 x 连成一个字符串
echo \$$ptrx
$x
# eval 执行两次扫描,所以相当于 echo $x
eval echo \$$ptrx
```

### chmod

chmod - change file mode bits

SYNOPSIS

`chmod [OPTION]... MODE[,MODE]... FILE...`
`chmod [OPTION]... OCTAL-MODE FILE...`
`chmod [OPTION]... --reference=RFILE FILE...`

DESCRIPTION

chmod 后面可以接符号表示新的权限,也可以接八进制数字(octal) --表示新的 `模式位`(mode bits).

符号 mode 的格式一般是`[ugoa...][[-+=][perms...]...]`,`perms`一般是`0`,或者`rwxXst`中的多个字符,
或者`ugo`中的一个字符.多种符号mode可以给出,用逗号隔开.

`ugoa`表示控制特定用户访问权限:

+ u:拥有它的用户
+ g:其他在文件的组中的用户
+ o:不在文件的组中的其他用户
+ a:所有用户
如果没有给出,默认就是 a,but bits that are set in the umask are not affected.

operator `+`添加权限,`-`删除权限,`=`设置为`xxx`,except that a directory's unmentioned set user and group ID bits are not affected.

`rwxXst`表示mode bits,read (r), write (w), execute (or  search  for directories)  (x),
execute/search  only if the file is a directory or already has execute permission for some user (X),
set user or group ID on execution (s), restricted deletion flag or sticky bit (t)

或者指定`ugo`中的一个,
the permissions granted to the user who owns the file (u),
the permissions granted to other users who are members of the file's group (g),
and the permissions granted to users that are in neither of the two preceding categories (o).

+ 数字模式

数字 `mode` 是1到4个八进制数字(0-7), 通过把相应的值 `4`, `2`, `1`加起来, 可以推导出权限.
省略的数字被认为是前置的`0`.

第一位数字选择用户组
the set user ID (4) and
set group  ID(2)  and
restricted deletion or sticky (1) attributes.

第二位数字选择权限
read (4), write (2), and execute (1);

第三位数字设定组中其他用户的权限

第四位数字设定不在组中用户的权限
