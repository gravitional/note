# ubuntu_2a

## UNIX进程

[UNIX进程](https://www.w3cschool.cn/xfgms6/5xsq8ozt.html)

### 进程列表

`PID`是每个进程唯一号码. 使用 `ps` 获取所有正在运行的进程列表. 

```bash
$ ps -auxefw # 所有正在运行进程的详尽列表
```

然而, 更典型的用法是使用管道或者`pgrep`:

```bash
$ ps axww | grep cron
  586  ??  Is     0:01.48 /usr/sbin/cron -s
$ ps aux | grep 'ss[h]' #找到所有所有 ssh 进程的 pid
$ pgrep -l sshd # 查找所有进程名中有sshd 的进程ID
$ echo $$   # 你的 shell 的PID
$ fuser -va 22/tcp # 列出使用端口22的进程,fuser:使用文件或者socket 确定进程
$ fuser -va /home   # 列出访问 /home 分区的进程
$ strace df # 跟踪系统调用和信号
$ truss df # 同上(FreeBSD/Solaris/类Unix)
$ history | tail -50 # 显示最后50个使用过的命令
```

### 优先级

用 `renice` 更改正在运行进程的优先级. 负值是更高的优先级, 最小为`-20`, 其正值与 `nice` 值的意义相同. 
 
```bash
# renice -5 586 # 更强的优先级586: old priority 0, new priority -5
```

使用 `nice` 命令启动一个已定义优先级的进程.  正值为低优先级, 负值为高优先级. 确定你知道`/usr/bin/nice` 或者使用 shell 内置命令(`which nice`). 

```bash
$ nice -n -5 top # 更高优先级(/usr/bin/nice)
$ nice -n 5 top # 更低优先级(/usr/bin/nice)
$ nice +5 top # tcsh 内置 nice 命令(同上)
```

`nice` 可以影响 `CPU` 的调度, 另一个实用命令 `ionice` 可以调度磁盘 `IO`, 这对于`IO`密集型应用程序非常有用.
此命令仅可在 `Linux` (AFAIK) 上使用. 你可以选择一个类型(`idle`,`best effort`,`real time`), 它的 man 页很短并有很好的解释. 

```bash
$ ionice c3 -p123 # 给 pid 123 设置为 idle 类型
$ ionice -c2 -n0 firefox # 用 best effort 类型运行 firefox 并且设为高优先级
$ ionice -c3 -p$$ # 将当前的进程(shell)的磁盘 IO 调度设置为 idle 类型
```

例中最后一条命令对于编译(或调试)一个大型项目会非常有用. 每一个运行于此 `shell` 的命令都会有一个较低的优先级, 但并不妨碍这个系统. `$$` 是你 `shell` 的 `pid` (试试 `echo $$`). 

### 前台/后台

当一个进程在 `shell` 中已运行, 可以使用 `[Ctrl]-[Z]` (`^Z`), `bg` 和 `fg` 来 调入调出前后台, 使用 `jobs` 列出后台列表.

`nohup` 英文全称 `no hang up`(不挂起), 用于在系统后台不挂断地运行命令, 退出终端不会影响程序的运行. 
`nohup` 命令, 在默认情况下(非重定向时), 会输出一个名叫 `nohup.out `的文件到当前目录下, 如果当前目录的 `nohup.out` 文件不可写, 输出重定向到 `$HOME/nohup.out` 文件中. 

语法格式

`nohup Command [ Arg … ] [&]`

参数说明：

+ `Command`：要执行的命令. 
+ `Arg`：一些参数, 可以指定输出文件. 
+ `&`：让命令在后台执行, 终端退出后命令仍旧执行. 

例如以下命令在后台执行 `root` 目录下的 `runoob.sh` 脚本：

```bash
nohup /root/runoob.sh &
```

如果要停止运行, 你需要使用以下命令查找到 `nohup` 运行脚本到 `PID`, 然后使用 `kill` 命令来删除：

```bash
ps -aux | grep "runoob.sh" 
```

以下命令在后台执行 root 目录下的 `runoob.sh` 脚本, 并重定向输入到 `runoob.log` 文件：

```bash
nohup /root/runoob.sh &> runoob.log  &
```

`&>` 解释：将标准输出`&1`以及标准错误`2`共同重定向到 `runoob.log` 文件中. 

### Top

`top` 程序用来实时显示系统中各个进程的运行信息.  当 top 在运行的时候, 按下 `h` 键会显示帮助画面. 常用键如下：

+ `u` [用户名] 只显示属于此用户的进程. 使用 `+` 或者空白可以查看所有用户
+ `k [PID]` 结束 `PID` 进程
+ `1` 显示所有进程状态信息(`Linux`才有)
+ `R` 将当前排序倒转

## Kill命令与信号

使用 kill 或 killall 终止或发送一个信号给进程. 

```bash
$ ping -i 60 cb.vu > ping.log & # [1] 4712
$ kill -s TERM 4712  # 同 kill -15 4712
$ killall -1 httpd # 发送 HUP 信号终止进程 httpd
$ pkill -9 http # 发送 KILL 信号终止包含 http 的进程
$ pkill -TERM -u www # 发送 TERM 信号终止 www 所有者进程
$ fuser -k -TERM -m /home # 终止所有访问 /home 的进程(卸载该分区前) -k Signal -m file
```

下面是一些重要的信号：

+ `1`:   `HUP` (挂起)
+ `2`:   `INT` (中断)
+ `3`:   `QUIT` (退出)
+ `9`:   `KILL` (`KILL` 信号不能被捕捉, 不能被忽略. )
+ `15`:   `TERM` (软件终止信号)
