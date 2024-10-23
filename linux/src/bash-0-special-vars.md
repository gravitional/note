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

## bash export

[export](https://www.gnu.org/software/bash/manual/bash.html#index-export)

```bash
export [-fn] [-p] [name[=value]]
```

标记环境中的每个`name`, 它们会传递给子进程.
如果提供了 `-f` 选项, 则名称指的是 `shell函数`; 否则, 名称指的是 `shell变量`.
`-n` 选项表示取消标记要导出的每个名称.
如果没有提供名称, 或者给出了 `-p` 选项, 则会显示所有导出变量的名称列表.

`-p` 选项 以可作为输入的形式 显示输出结果.
如果变量名后跟 `=value`, 变量值将被设置为 `value`.

除非提供的选项无效, 或其中一个名称不是有效的 shell 变量名,
或提供的 -f 名称不是 shell 函数, 否则返回状态为零.

## bash 操作符 |&

[What does "|&" mean in Bash?](https://stackoverflow.com/questions/35384999/what-does-mean-in-bash)

摘自 `man 1 bash`, Pipelines 一节:

[时间 [-p]] [ ! ] command [ [|⎪|&] command2 ... ]

如果使用了 `|&`, `command` 的 `stdout` 和 `stderr`,
会通过管道连接到 `command2` 的标准输入.
因此, 它就像管道操作符 `|`, 但同时对 `标准输出` 和 `标准错误` 进行 管道连接.

## bash 后台任务

[4 种简单方法, 在 Linux 后台运行命令](https://www.sysgeek.cn/run-linux-commands-in-background/)

### 什么是 Linux 后台任务

在 Linux 中, 后台任务实质上是从终端或会话中独立运行的进程. 
这些任务具有弹性, 即使终端被终止或用户退出登录, 它们仍然可以继续运行. 
Linux 后台任务对于耗时较长或无需用户持续干预的任务至关重要. 

在 Linux 后台运行命令有许多好处和优势, 包括: 

+ 不会占用当前终端会话: 在后台运行命令后, 您可以继续在当前终端中执行其他任务, 而不会被阻塞. 
+ 进程持续运行: 后台进程不会因关闭终端而停止, 可以一直运行到任务完成. 
+ 释放系统资源: 后台进程会释放终端的输入输出资源, 有利于其他前台进程使用. 
+ 提高生产力: 可以同时运行多个后台任务, 大大提高了系统利用率和用户生产力. 
+ 方便管理进程: 可以使用作业管理工具如 jobs, fg, bg 方便地管理后台进程. 
+ 避免进程阻塞: 对于需要长时间运行的进程, 在后台运行可以避免阻塞前台操作. 

### 方法 1: 使用 `&` 运算符在 Linux 后台运行命令

`&` 运算符是在 Linux 后台运行命令的基本工具. 
通过在任何命令的末尾添加 `&` 运算符, 可以让 Linux 在后台执行该命令. 

例如, 想要在后台运行一个名为 `example.sh` 的脚本, 命令如下: 

```bash
./example.sh &
```

执行该命令后, Linux 会立即返回后台任务的 PID, 以方便用户随后监视或管理该任务. 

### 方法 2: 使用 nohup 在 Linux 后台运行命令

如果要在终端会话终止后保持任务继续处于活动状态, `nohup` 就是首选工具. 
`nohup` 是「no hang up」的缩写, 它可以确保命令的不间断执行. 

例如, 使用 `nohup` 在后台运行 example.sh 脚本的命令如下: 

```bash
nohup ./example.sh &
```

默认情况下, 命令的输出将被重定向到名为 `nohup.out` 的文件. 
如果有需要, 可以灵活地将其重定向到其他文件. 

### 方法 3: 使用 screen 在后台运行 Linux 命令和管理会话

`screen` 是一个强大的 Linux 实用程序, 专为高级会话管理而设计. 
它能够创建多个终端会话, 并轻松地在会话之间切换, 还可以从当前会话中分离出来以便稍后重新连接. 

要创建一个新的 screen 会话, 可以使用以下命令: 

```bash
screen -S session_name
```

要从当前会话中分离(detach), 可以使用快捷键: 

```bash
Ctrl + a, 然后按 d
```

要重新连接到之前分离的会话, 可以运行: 

```bash
screen -r session_name
```

### 方法 4: 在后台运行 Linux 命令的高级工具 tmux

tmux 是另一个功能强大的工具, 提供类似于 screen 的功能, 并增加了一些额外的特性. 
它允许用户轻松地创建, 分离和重新连接会话. 

要使用 tmux 创建一个新的会话, 可以在终端中运行以下命令: 

```bash
tmux new -s session_name
```

要从活动会话中分离(detach), 可以使用快捷键: 

```bash
Ctrl + b, 然后按 d
```

要重新载入之前分离的会话, 可以运行: 

```bash
tmux attach -t session_name
```

### 监视和管理 Linux 后台命令

一旦任务被设置为在后台运行, 有效地监视和控制就变得至关重要.
jobs 命令可以提供所有后台任务的简洁列表: 

```bash
jobs
```

要将后台任务移到前台, 可以使用 fg 命令, 后面跟上作业编号: 

```bash
fg %job_number
```
