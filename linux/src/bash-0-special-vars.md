# bash  特殊变量 自动变量

[Bash Shell $*, $@, $#,](https://zhuanlan.zhihu.com/p/57784678)
[Bash 特殊变量的含义 ](https://www.cnblogs.com/kaituorensheng/p/4002697.html)

+ `$#`; 传给脚本的参数个数
+ `$0`; 脚本本身的名字
+ `$1`; 传递给该shell脚本的第一个参数
+ `$2`; 传递给该shell脚本的第二个参数
+ `$@`; 传给脚本的所有参数的列表
+ `$*`; 以一个单字符串显示所有向脚本传递的参数, 与位置变量不同, 参数可超过9个
+ `$$`; 脚本运行的当前进程ID号
+ `$?`; 显示最后命令的退出状态, 0表示没有错误, 其他表示有错误

`$@` 和 `$*` 的区别:

相同点: 都是引用所有参数
不同点: 只有在双引号中体现出来.
假设在脚本运行时写了三个参数(分别存储在 `1`, `2`, `3`)
则 `$*`  等价于  `1 2 3`(传递了一个参数);
而 `$@` 等价于 `1`,`2`,`3`(传递了三个参数)

## `$*` 和 `$@`

+ 在 `Bash` 中, 不被双引号 `"` 包裹时, 它们两个展开的结果是相同的, 都是表示外部输入的`参数列表`.
+ 当被`双引号` 包裹时, 即 `"$*"`, `"$@"`. 
这个时候, 前者表示的是用 `IFS` (Internal Field Separator) 连接起来的统一字符, 后者则表示的是输入的`每个参数`.

举例如下: 文档名字为 `test_1.sh`

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
具体分别表示为, 记住 `工作路径`, 和允许使用 `!` 历史扩展, 详细请参阅 `set` 命令.

`$_ `(下划线) 表示的是打印上一个输入的 `参数行`,
在脚本开头使用这个命令, 打印出脚本的`路径名`.

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
