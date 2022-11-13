# bash 脚本参数处理 cmd args

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
