# bash 脚本规范

## linux 内核编码风格

[Linux 内核代码风格](https://www.kernel.org/doc/html/latest/translations/zh_CN/process/coding-style.html)
[Linux 内核编码风格](https://zhuanlan.zhihu.com/p/330280764)

### 括号

左括号紧跟在语句的最后, 与语句在相同的一行. 而右括号要另起一行, 作为该行的第一个字符.
如果接下来的部分是相同语句的一部分, 那么右括号就不单独占一行.

```c
if ... {
              if ... {
              ...
              } else {
              ...
                     }
       }
return ... ;

do {
...
} while ( ...);
```

函数采用以下的书写方式:

```c
static inline int rt_policy(int policy)
{
              ...
}
```

最后不需要一定使用括号的语句可以忽略它:

```c
if (a==b)
              return 0;
return 1;
```

### 每行代码的长度

要尽可能地保证代码长度不超过80个字符, 如果代码行超过`80`应该折到下一行.
将参数分行输入, 在开头简单地加入两个标准tab:

```c
static int wait_noreap_copyout ( a, b, c, ...
              d,e,f )
{

}
```

### 命名规范

名称中不允许使用混合的大小写字符.
局部变量如果能够清楚地表明它的用途, 那么选取`idx`甚至是i这样的名称都是可行的.
而像`theLoopIndex`这样冗长反复的名字不在接受之列.  -- 匈牙利命名法(在变量名称中加入变量的类别)危害极大.

### 函数

根据经验函数的代码长度不应该超过两屏, 局部变量不应该超过十个.

+ 一个函数应该功能单一并且实现精准.
+ 将一个函数分解成一些更短小的函数的组合不会带来危害.  -- 如果你担心函数调用导致的开销, 可以使用inline关键字.

### 注释

一般情况下, 注释的目的是描述你的代码要做什么和为什么要做, 而不是具体通过什么方式实现的. 怎么实现应该由代码本身展现.
注释不应该包含谁写了那个函数, 修改日期和其他那些琐碎而无实际意义的内容. 这些信息应该集中在文件最开头地方.
重要信息常常以`XXX:`开头, 而`bug`通常以`FIXME`开头.

## 脚本参数处理

[解析命令行参数工具](https://cloud.tencent.com/developer/article/1043821)
[use getopts in bash](https://www.golinuxcloud.com/bash-getopts/)

`bash` 脚本中, 简单点的参数选项, 我们可以直接用位置参数 `$1`, `$2` 这样来获取处理了.

但是如果你的参数选项很多, 比如 `rsync`, `wget` 等动辄几十上百的参数选项, 那就必须用专业的工具来处理了,
在 `bash/shell` 中我们一般用: `getopts/getopt `

### bash 内置的 getopts

先看简单的例子:

```bash
#!/bin/bash
while getopts 'd:Dm:f:t:' OPT; do
    case $OPT in
        d)
            DEL_DAYS="$OPTARG";;
        D)
            DEL_ORIGINAL='yes';;
        f)
            DIR_FROM="$OPTARG";;
        m)
            MAILDIR_NAME="$OPTARG";;
        t)
            DIR_TO="$OPTARG";;
        ?)
            echo "Usage: `basename $0` [options] filename"
    esac
done
shift $(($OPTIND - 1))
```

`getopts` 后面的字符串就是可以使用的`选项列表`, 每个字母代表一个选项.
后面带`:`的意味着除了选项本身之外, 还会带上一个`参数`作为选项的值, 比如`d:`在实际的使用中就会对应`-d 30`, 选项的值就是`30`;

`getopts`字符串中没有跟随`:`的是`开关型`选项, 不需要再指定值, 相当于`true/false`, 只要带了这个参数就是`true`.
如果命令行中包含了没有在`getopts`列表中的选项, 会有警告信息, 如果在整个`getopts`字符串前面也加上个`:`, 就能消除警告信息了.

使用`getopts`识别出各个选项之后, 就可以配合`case`来进行相应的操作了.
操作中有两个相对固定的"常量", 一个是`OPTARG`, 用来取当前`选项的值`, 另外一个是`OPTIND`, 代表当前选项在参数列表中的位移.
注意`case`中的最后一个选择 --`?`, 代表这如果出现了不认识的选项,  所进行的操作.

`选项参数`识别完成之后, 如果要取剩余的其它命令行参数, 也就是位置参数 `#1,#2,...`, 可以使用`shift`把选项参数抹去.
就像例子里面的那样, 对整个参数列表进行`左移`操作, 最左边的参数就丢失了(已经用`case`判断并进行了处理, 不再需要了),
位移的长度正好是刚才`case`循环完毕之后的`OPTIND - 1`, 因为参数从`1`开始编号,
选项处理完毕之后, 正好指向剩余其它参数的第一个.
在这里还要知道, `getopts`在处理参数的时候, 处理一个`开关型`选项, `OPTIND`加`1`,
处理一个`带值`的选项参数, `OPTIND则会加`2`.

最后, 真正需要处理的参数就是`$1~$#`了, 可以用`for`循环依次处理.

使用`getopts`处理参数虽然是方便, 但仍然有局限:

1. 选项参数的格式必须是`-d val`, 而不能是中间没有空格的`-dval`.
2. 所有`选项参数`必须写在其它参数的前面, 因为`getopts`是从命令行前面开始处理,
遇到非`-`开头的参数, 或者选项参数结束标记`--`就中止了, 如果中间遇到非选项的命令行参数, 后面的`选项参数`就都取不到了.
3. 不支持长选项,  也就是`--debug`之类的选项

### 外部强大的参数解析工具: getopt

先来看下`getopt`/`getopts`的区别

1. `getopts`是`bash`内建命令的,  而`getopt`是外部命令
2. `getopts`不支持长选项,  比如: `--date`
3. 在使用`getopt`的时候,  每处理完一个位置参数后都需要自己`shift`来跳到下一个位置,
`getopts`只需要在最后使用`shift $(($OPTIND - 1))`来跳到`位置参数`的位置.
4. 使用`getopt`时,  在命令行输入的位置参数是什么,  在`getopt`中需要保持原样,
比如 `-t `,  在`getopt`的`case`语句中也要使用`-t`,   而`getopts`中不要前面的`-`.
5. `getopt`往往需要跟`set`配合使用
6. `getopt -o`的选项
7. `getopts` 使用语法简单, `getopt` 使用语法较复杂
8. `getopts` 不会重排所有参数的顺序, `getopt` 会重排参数顺序
9. `getopts` 出现的目的是为了代替 `getopt` 较快捷的执行参数分析工作

## shell 的自动变量

[Bash Shell $*, $@, $#,](https://zhuanlan.zhihu.com/p/57784678)

### $* 和 $@

+ 在 `Bash` 中, 不被双引号 `"` 包裹时, 它们两个展开的结果是相同的, 都是表示外部输入的`参数列表`.
+ 当被`双引号` 包裹时, 即 `"$*"`, `"$@"`. 这个时候, 前者表示的是用 `IFS` (Internal Field Separator) 连接起来的统一字符, 后者则表示的是输入的`每个参数`.
举例如下:

文档名字为 `test_1.sh`

```bash
#!/bin/bash

export IFS=%

cnt=1
for i in "$*"
do
    echo "Number of $cnt parameter is: $i"
    (( cnt++ ))
done

echo
echo

cnt=1
for i in "$@"
do
    echo "Number of $cnt parametre is: $i"
    (( cnt++ ))
done
```

执行这个文件:

    ./test_1.sh "Hello, how are you?" Second Third Fourth

输出的结果会是:

    Number of 1 parameter is: Hello, how are you?%Second%Third%Fourth

    Number of 1 parameter is: Hello, how are you?
    Number of 2 parameter is: Second
    Number of 3 parameter is: Third
    Number of 4 parameter is: Fourth

解释如下:

+ 被双括号后, `"$*"` 表示的是用`IFS`(内部分割符 ) 连接起来的`单个`字符串, 注意上面的打印输出只有`一个参数`.
+ 而 `"$@"` 仍然表示的是各个输入的参数. 所以这也就解释了, 除非特殊情况, 为什么推荐使用 `$@`, 而不是 `$*` 展开`参数列表`.
+ `$#` 获得`参数列表`的`个数`.

如果接着上面的 `test_1.sh` 文件, 在最后添加:

    echo Number of total parameters are $#

执行后获得另外新的结果将会是:

    Number of total parameteers are 4

### $$, $!, $? 获得进程 ID 信息

+ `$$`; 获得当前进程 ID
+ `$!`; 获得之前(上一个)进程 ID
+ `$?`; 获得之前(上一个)进程结束的状态码 (0 表示成功, 1 表示失败)

举例如下:
例如我们有一个文档, `test_2.sh`

```bash
#!/bin/bash

echo "Current process ID is: $$"

sleep 100 &
echo "The most recent process ID is: $!"
echo "The most recent process exit status is: $?"
```

执行`./test_2.sh`, 输出的结果:

    Current process ID is: 15599
    The most recent process ID is: 15600
    The most recent process ID exit status is: 0

然后接着执行`ps`, 输出的结果会是:

      PID   TTY       TIME      CMD
    14941   pts/0   00:00:00    bash
    15600   pts/0   00:00:00    sleep
    15601   pts/0   00:00:00    ps

解释如下:

注意: 不同的机器获得进程 `ID` 可能和上边的举例结果不一样.

当执行 `./test_2.sh` 之后, 我们得到了执行这个文件的进程 `ID: 15599` ,
后台执行 `sleep` 后, 再执行 `$!`, 我们可以获得这个后台进程的 `ID`, 结果为 `15600`;

由于这个进程是一个 `100` 秒的后台进程, 在后边的 `ps` 命令中, 我们很容易的就看到了这个 `ID` 为 `15600` 的后台进程.

紧接着我们执行 `$?,` 注意, 这个获得的进程状态码是上一个的进程结束码,
由于我们上一个命令是 `echo`, 而 `echo` 是顺利结束命令的, 所以我们获得了状态码 `0` , 表示 `echo` 命令执行成功.

### $- 和 $_

`$-` 是 `set` 命令的 `–h` 和 `–B` 的参数, 表示使用内置的 `set` 命令扩展解释之后的`参数行`,
具体分别表示为, 记住`工作路径`, 和允许使用 `!` 历史扩展, 详细请参阅 `set` 命令.

`$_ `(下划线) 表示的是打印上一个输入`参数行`, 在脚本开头使用这个命令, 打印出脚本的`路径名`.

举例如下: 例如我们有一个文档, `test_3.sh`

```bash
#!/bin/bash
echo "Current absolute file path name is: $_"
echo "$-"
echo "Second $_"

let cnt=1
echo "Third $_"
echo "$cnt"
echo "Fourth $_"
```

执行命令: `./test_3.sh`. 输出的结果是:

    Current absolute file path name is: ./test_3.sh
    hB
    Second hB
    Third cnt=1
    1
    Fourth 1

解释如下:

+ 由于我们是在`当前路径`下执行的`脚本`, 那么在`脚本`开始执行其他命令之前,
`$_` 获得就是`脚本`的`绝对路径`名称 -- `./test_3.sh`, 这里 `.` (点号) 表示当前路径.
+ 执行 `$-` 后, 表示使用 `set` 的 `–h` 和 `–B` 选项, 这时传入的参数是 `hB`;
+ 第二次执行 `$_` 后, 获得上次传入的参数, 表示为 `hB`;
+ 在第三次执行时, 由于上次对于 `let` 命令传入的参数是 `cnt=1`, 那么这时获得的参数是 `cnt=1`;
+ 第四次执行时, 对于 `echo` 传入的参数是扩展后的 `$cnt`, 也就是 `1`, 那么这时获得参数就是 `1`.