# ubuntu_1b

## bash 快捷键

[Bash 快捷键大全 ](https://linux.cn/article-5660-1.html)
[vim ,vi总是卡死,终于找到原因了.](https://www.cnblogs.com/cocoliu/p/6369749.html)

+ `Alt+tab`:切换程序
+ `` Alt+` ``:切换程序的不同窗口

在`vim`下,有时候不小心按下了`CTRL-S`--`Suspend(XOFF)`,会冻结终端的输入,表现为按什么键都没有反应,这时候按下`CTRL-Q`即可恢复.

### 常用的

| 快捷键   | 快捷键说明|
| --------------- | ----------- |
| `CTRL-/` | 撤消操作,Undo.    |
| `ALT-B`  | 光标往回跳一个词,词以非字母为界(跳动到当前光标所在词的开头).   |
| `ALT-F`  | 光标往前跳一个词(移动到光标所在词的末尾).     |
| `ALT-D`  | 删除光标所在位置到光标所在词的结尾位置的所有内容      |
| `ALT-BASKSPACE` | 删除光标所在位置到词开头的所有内容.    |
| `ALT-数值`      | 这个数值可以是正或者是负,这个键单独没有作用,必须后面再接其他内容,如果后面是字符,则表示重复次数.如:`[ALT-9,k]`则光标位置会插入`9`个`k`字符(负值在这种情况下无效)；如果后面接的是命令,则数字会影响后面命令的执行结果,如:`[ALT-9,CTRL-D]`则向`CTRL-D`默认方向相反(负数)的方向执行`9`次操作. |
| `ALT-<`  | 移动到历史记录中的第一行命令.   |
| `ALT->`  | 移动到历史的最后一行,即当前正在输入的行(没有输入的情况下为空). |
| `ALT-P`  | 从当前行开始向前搜索,有必要则向"上"移动,移动时,使用非增量搜索查找用户提供的字符串.      |
| `ALT-N`  | 从当前行开始向后搜索,如果有必要向"下"移动,移动时,使用非增量搜索查找用户提供的字符串.    |
| `ALT-?`  | 列出能够补全标志点前的条目.     |
| `ALT-C`  | 将光标所在位置的字母转为大写     |
| `ALT-U`  | 将光标所在位置到词尾的所有字母转为大写.       |
| `ALT-L`  | 将光标位置到词尾的所有字母转为小写.    |
| `ALT-R`  | 取消所有变更,并将当前行恢复到在历史记录中的原始状态  |
| `ALT-T`  | 当光标两侧都存在词的时候,交换光标两侧词的位置.如:`abc <ALT-T>bcd -> bcd abc|`     |
| `ALT-.`  | 使用前一次命令的最后一个词(命令本身也是一个词,参见后一篇的Bang命令中的词指示符概念).      |
| `CTRL-A` | 将光标移到行首(在命令行下)     |
| `CTRL-C` | 中断,终结一个前台作业.         |
| `CTRL-E` | 将光标移动到行尾(在命令行下)   |
| `CTRL-K` | 在控制台或 xterm 窗口输入文本时,`CTRL-K`会删除从光标所在处到行尾的所有字符.        |
| `CTRL-U` | 擦除从光标位置开始到行首的所有字符内容.       |
| `CTRL-W` | `CTRL-W` 会删除从在光标处往回的第一个空白符之间的内容 |
| `CTRL-Y` | 将之前已经清除的文本粘贴回来(主要针对`CTRL-U`或`CTRL-W`).     |
| `CTRL-N` | 每按一次,是更接近的一条命令.   |
| `CTRL-P` | 此快捷键召回的顺序是由近及远的召回,    |
| `ALT-*`  | 把能够补全[`ALT-?`]命令能生成的所有文本条目插入到标志点前.      |
| `CTRL-Q` | `Resume (XON)`.恢复/解冻,这个命令是恢复终端的stdin用的,可参见`CTRL-S`.    |
| `CTRL-R` | 回溯搜索(Backwards search)history缓冲区内的文本(在命令行下).注意:按下之后,提示符会变成`(reverse-i-search)'':`输入的搜索内容出现在单引号内,同时冒号后面出现最近最匹配的历史命令.         |
| `CTRL-S` | `Suspend(XOFF)`,挂起.这个是冻结终端的`stdin`.要恢复可以按`CTRL-Q`.        |
| `CTRL-T` | 交换光标位置与光标的前一个位置的字符内容(在命令行下)|
| `CTRL-\` | 退出.和`CTRL-C`差不多,也可能dump一个"core"文件到你的工作目录下(这个文件可能对你没用).    |
***

terminal 快捷键

| 快捷键 | 快捷键说明      |
| ------------- | ---------------------- |
| `Ctrl+Shift+t`       | new tab  |
| `Ctrl+Shift+n`       | new window      |
| `Ctrl+Shift+w`       | close tab       |
| `Ctrl+Shift+q`       | close window    |
| `Ctrl+page up`   | switch to previous tab |
| `Ctrl+Shift+page up` | switch to the left     |

### 广泛的

| 快捷键  | 快捷键说明       |
| --------------- | ------ |
| `CTRL-A` | 将光标移到行首(在命令行下)          |
| `CTRL-B` | 退格 (非破坏性的),这个只是将光标位置往回移动一个位置.    |
| `CTRL-C` | 中断,终结一个前台作业. |
| `CTRL-D` | "EOF" (文件结尾:end of file).它用于表示标准输入(`stdin`)的结束.  |
| `CTRL-E` | 将光标移动到行尾(在命令行下)        |
| `CTRL-F` | 将光标向前移动一个字符(在命令行下)         |
| `CTRL-G` | `BEL`.在一些老式打印机终端上,这会引发一个响铃.在xterm终端上可能是哔的一声.     |
| `CTRL-H` | 擦除(Rubout)(破坏性的退格).在光标往回移动的时候,同时擦除光标前的一个字符.       |
| `CTRL-I` | 水平制表符.      |
| `CTRL-J` | 新行(`换行[line feed]`并到行首).在脚本中,也可能表示为八进制形式(`'\012'`)或十六进制形式(`'\x0a'`).   |
| `CTRL-K` | 垂直制表符(Vertical tab).在控制台或 xterm 窗口输入文本时,`CTRL-K`会删除从光标所在处到行尾的所有字符. |
| `CTRL-L` | 跳纸,换页(Formfeed),清屏.清空终端屏幕.在终端上,这个命令的作用和`clear`命令一样.但当这个命令发送到打印机时,`Ctrl-L`会直接跳到纸张(Paper sheet)的末尾.      |
| `CTRL-M` | 回车(Carriage return).  |
| `CTRL-N` | 擦除从history缓冲区召回的一行文本(在命令行下).如果当前输入是历史记录中选择的时候,这个是从这个历史记录开始,每按一次,是更接近的一条命令. |
| `CTRL-O` | 产生一个新行(在命令行下).          |
| `CTRL-P` | 从history缓冲区召回上一次的命令(在命令行下).此快捷键召回的顺序是由近及远的召回,即按一次,召回的是前一次的命令,再按一次,是召回上一次之前的命令,这和`CTRL-N`都是以当前的输入为起点,但是两个命令操作刚好相反,`CTRL-N`是从起点开始由远及近(如果起点是历史命令的话).        |
| `CTRL-Q` | `Resume (XON)`.恢复/解冻,这个命令是恢复终端的stdin用的,可参见`CTRL-S`.         |
| `CTRL-R` | 回溯搜索(Backwards search)history缓冲区内的文本(在命令行下).注意:按下之后,提示符会变成`(reverse-i-search)'':`输入的搜索内容出现在单引号内,同时冒号后面出现最近最匹配的历史命令. |
| `CTRL-S` | `Suspend(XOFF)`,挂起.这个是冻结终端的`stdin`.要恢复可以按`CTRL-Q`.|
| `CTRL-T` | 交换光标位置与光标的前一个位置的字符内容(在命令行下).比如:`echo $var;`,假设光标在`a`上,那么,按下`C-T`之后,`v`和`a`将会交换位置:`echo $avr;`.     |
| `CTRL-U` | 擦除从光标位置开始到行首的所有字符内容.在某些设置下,`CTRL-U`会不以光标位置为参考而删除整行的输入.    |
| `CTRL-V` | 在输入文本的时候,按下`C-V`之后,可以插入控制字符.比如:`echo -e '\x0a';`和`echo <CTRL-V><CTRL-J>;`这两种效果一样.这点功能在文本编辑器内非常有效. |
| `CTRL-W` | 当在控制台或一个xterm窗口敲入文本时, `CTRL-W` 会删除从在光标处往后(回)的第一个空白符之间的内容.在某些设置里, `CTRL-W` 删除光标往后(回)到第一个非文字和数字之间的字符.     |
| `CTRL-X` | 在某些文字处理程序中,这个控制字符将会剪切高亮的文本并且将它复制到剪贴板中.       |
| `CTRL-Y` | 将之前已经清除的文本粘贴回来(主要针对`CTRL-U`或`CTRL-W`).   |
| `CTRL-Z` | 暂停一个前台的作业；在某些文本处理程序中也作为替换操作；在MSDOS文件系统中作为EOF(End-of-file)字符.    |
| `CTRL-\` | 退出.和`CTRL-C`差不多,也可能dump一个"core"文件到你的工作目录下(这个文件可能对你没用).  |
| `CTRL-/` | 撤消操作,Undo.  |
| `CTRL-_` | 撤消操作.        |
| `CTRL-xx`       | 在行首和光标两个位置间进行切换,此处是两个`"x"`字符.      |
| `ALT-B`  | 光标往回跳一个词,词以非字母为界(跳动到当前光标所在词的开头). |
| `ALT-F`  | 光标往前跳一个词(移动到光标所在词的末尾).   |
| `ALT-D`  | 删除光标所在位置到光标所在词的结尾位置的所有内容(如果光标是在词开头,则删除整个词).      |
| `ALT-BASKSPACE` | 删除光标所在位置到词开头的所有内容.         |
| `ALT-C`  | 将光标所在位置的字母转为大写(如果光标在一个词的起始位置或之前,则词首字母大写).   |
| `ALT-U`  | 将光标所在位置到词尾的所有字母转为大写.     |
| `ALT-L`  | 将光标位置到词尾的所有字母转为小写.         |
| `ALT-R`  | 取消所有变更,并将当前行恢复到在历史记录中的原始状态(前提是当前命令是从历史记录中来的,如果是手动输入,则会清空行).        |
| `ALT-T`  | 当光标两侧都存在词的时候,交换光标两侧词的位置.如:`abc <ALT-T>bcd -> bcd abc|`   |
| `ALT-.`  | 使用前一次命令的最后一个词(命令本身也是一个词,参见后一篇的Bang命令中的词指示符概念).    |
| `ALT-_`  | 同`ALT-.`.       |
| `ALT-数值`      | 这个数值可以是正或者是负,这个键单独没有作用,必须后面再接其他内容,如果后面是字符,则表示重复次数.如:`[ALT-10,k]`则光标位置会插入`10`个`k`字符(负值在这种情况下无效)；如果后面接的是命令,则数字会影响后面命令的执行结果,如:`[ALT--10,CTRL-D]`则向`CTRL-D`默认方向相反(负数)的方向执行`10`次操作. |
| `ALT-<`  | 移动到历史记录中的第一行命令.        |
| `ALT->`  | 移动到历史的最后一行,即当前正在输入的行(没有输入的情况下为空).      |
| `ALT-P`  | 从当前行开始向前搜索,有必要则向"上"移动,移动时,使用非增量搜索查找用户提供的字符串.    |
| `ALT-N`  | 从当前行开始向后搜索,如果有必要向"下"移动,移动时,使用非增量搜索查找用户提供的字符串.  |
| `ALT-CTRL-Y`    | 在标志点上插入前一个命令的第一个参数(一般是前一行的第二个词).如果有参数`n`,则插入前一个命令的第`n`个词(前一行的词编号从`0`开始,见历史扩展).负的参数将插入冲前一个命令的结尾开始的第n个词.参数`n`通过`M-No.`的方式传递,如:`[ALT-0,ALT-CTRL-Y]`插入前一个命令的第`0`个词(命令本身).        |
| `ALT-Y`  | 轮询到删除环,并复制新的顶端文本.只能在`yank[CTRL-Y]`或者`yank-pop[M-Y]`之后使用这个命令.      |
| `ALT-?`  | 列出能够补全标志点前的条目.          |
| `ALT-*`  | 把能够补全[`ALT-?`]命令能生成的所有文本条目插入到标志点前.|
| `ALT-/`  | 试图对标志点前的文本进行文件名补全.`[CTRL-X,/]`把标志点前的文本当成文件名并列出可以补全的条目. |
| `ALT-~`  | 把标志点前的文本当成用户名并试图进行补全.`[CTRL-X,~]`列出可以作为用户名补全标志点前的条目.     |
| `ALT-$`  | 把标志点前的文本当成Shell变量并试图进行补全.`[CTRL-X,$]`列出可以作为变量补全标志点前的条目.    |
| `ALT-@`  | 把标志点前的文本当成主机名并试图进行补全.`[CTRL-X,@]`列出可以作为主机补全标志点前的条目.       |
| `ALT-!`  | 把标志点前的文本当成命令名并试图进行补全.进行命令名补全时会依次使用别名,保留字,Shell函数,shell内部命令,最后是可执行文件名.`[CTRL-X,!]`把标志点前的文本当成命令名并列出可补全的条目.          |
| `ALT-TAB`       | 把标志点前的文本与历史记录中的文本进行比较以寻找匹配的并试图进行补全.|
| `ALT-{`  | 进行文件名补全,把可以补全的条目列表放在大括号之间,让shell可以使用. |

***

在控制台或`xterm` 窗口输入文本时,``CTRL-D`` 删除在光标下的字符.从一个shell中退出 (类似于`exit`).如果没有字符存在,``CTRL-D`` 则会登出该会话.在一个xterm窗口中,则会产生关闭此窗口的效果.

`CTRL-K`
在脚本中,也可能表示为八进制形式(`'\013'`)或十六进制形式(`'\x0b'`).在脚本中,`CTRL-K`可能会有不一样的行为,下面的例子给出其不一样的行为:

```bash
#!/bin/bash
## 一个`CTRL-K`垂直制表符的例子
var=$'\x0aBottom Line\x0bTop line\x0a'
## 直接输出
echo "$var"
## 使用col来过滤控制字符
echo "$var" | col
## 上面的显示将会不一样
exit 0
```

### 系统快捷键

+ 不同`workspace`导航; ubuntu: `Super+PageUp/PageDown`; manjaro: `Ctrl+Super+Left/Right/Up/Down`
+ 将应用窗口移动到不同的`workspace`; ubuntu: `Shit+Super+ PageUp/PageDown`; manjaro: `Ctrl+Shift+Super+Left/Right/Up/Down`

## systemd

[Systemd 入门教程:命令篇](http://www.ruanyifeng.com/blog/2016/03/systemd-tutorial-commands.html)
[Systemd 入门教程:实战篇](http://www.ruanyifeng.com/blog/2016/03/systemd-tutorial-part-two.html)

历史上,Linux 的启动一直采用`init`进程.下面的命令用来启动服务.

```bash
$ sudo /etc/init.d/apache2 start # 或者
$ service apache2 start
```

这种方法有两个缺点.

+ 一是启动时间长.`init`进程是串行启动,只有前一个进程启动完,才会启动下一个进程.
+ 二是启动脚本复杂.`init`进程只是执行启动脚本,不管其他事情.脚本需要自己处理各种情况,这往往使得脚本变得很长.

`Systemd` 就是为了解决这些问题而诞生的.它的设计目标是,为系统的启动和管理提供一套完整的解决方案.
根据 `Linux` 惯例,字母`d`是守护进程(`daemon`)的缩写. `Systemd` 这个名字的含义,就是它要守护整个系统.

使用了 `Systemd`,就不需要再用init了.`Systemd` 取代了`initd`,成为系统的第一个进程(`PID `等于 `1`),其他进程都是它的子进程.

```bash
$ systemctl --version
```

上面的命令查看 `Systemd` 的版本.

### 系统管理

`Systemd` 并不是一个命令,而是一组命令,涉及到系统管理的方方面面.

#### systemctl

`systemctl` 是 `Systemd` 的主命令,用于管理系统.

```bash
$ sudo systemctl reboot # 重启系统
$ sudo systemctl poweroff # 关闭系统,切断电源
$ sudo systemctl halt # CPU停止工作
$ sudo systemctl suspend # 暂停系统
$ sudo systemctl hibernate # 让系统进入冬眠状态
$ sudo systemctl hybrid-sleep # 让系统进入交互式休眠状态
$ sudo systemctl rescue # 启动进入救援状态(单用户状态)
```

#### systemd-analyze

`systemd-analyze`命令用于查看启动耗时.

```bash
$ systemd-analyze # 查看启动耗时
$ systemd-analyze blame # 查看每个服务的启动耗时
$ systemd-analyze critical-chain # 显示瀑布状的启动过程流
$ systemd-analyze critical-chain atd.service # 显示指定服务的启动流
```

#### hostnamectl

`hostnamectl`命令用于查看当前主机的信息.

```bash
$ hostnamectl # 显示当前主机的信息
$ sudo hostnamectl set-hostname rhel7 # 设置主机名.
```

#### localectl

`localectl`命令用于查看本地化设置.

```bash

$ localectl # 查看本地化设置
$ sudo localectl set-locale LANG=en_GB.utf8 # 设置本地化参数.
$ sudo localectl set-keymap en_GB
```

#### timedatectl

`timedatectl` 命令用于查看当前时区设置.

```bash
$ timedatectl # 查看当前时区设置
$ timedatectl list-timezones # 显示所有可用的时区                                                         
$ sudo timedatectl set-timezone America/New_York # 设置当前时区               
$ sudo timedatectl set-time YYYY-MM-DD
$ sudo timedatectl set-time HH:MM:SS
```

#### loginctl

`loginctl`命令用于查看当前登录的用户.

```bash
$ loginctl list-sessions # 列出当前session
$ loginctl list-users # 列出当前登录用户
$ loginctl show-user ruanyf # 列出显示指定用户的信息
```

### Unit

#### 含义

`Systemd` 可以管理所有系统资源.不同的资源统称为 `Unit`(单位).

`Unit` 一共分成12种.

+ `Service unit`:系统服务
+ `Target unit`:多个 `Unit` 构成的一个组
+ `Device Unit`:硬件设备
+ `Mount Unit`:文件系统的挂载点
+ `Automount Unit`:自动挂载点
+ `Path Unit`:文件或路径
+ `Scope Unit`:不是由 `Systemd` 启动的外部进程
+ `Slice Unit`:进程组
+ `Snapshot Unit`:`Systemd` 快照,可以切回某个快照
+ `Socket Unit`:进程间通信的 `socket`
+ `Swap Unit`:`swap` 文件
+ `Timer Unit`:定时器

`systemctl list-units`命令可以查看当前系统的所有`Unit `.

```bash
$ systemctl list-units # 列出正在运行的 Unit
$ systemctl list-units --all # 列出所有Unit,包括没有找到配置文件的或者启动失败的
$ systemctl list-units --all --state=inactive # 列出所有没有运行的 Unit
$ systemctl list-units --failed # 列出所有加载失败的 Unit
$ systemctl list-units --type=service # 列出所有正在运行的, 类型为 service 的 Unit
```

#### Unit 的状态

`systemctl status`命令用于查看系统状态和单个 `Unit` 的状态.

```bash
$ systemctl status # 显示系统状态
$ sysystemctl status bluetooth.service # 显示单个 Unit 的状态
$ systemctl -H root@rhel7.example.com status httpd.service # 显示远程主机的某个 Unit 的状态
```

除了`status`命令,`systemctl`还提供了三个查询状态的简单方法,主要供脚本内部的判断语句使用.

```bash
$ systemctl is-active application.service # 显示某个 Unit 是否正在运行
$ systemctl is-failed application.service # 显示某个 Unit 是否处于启动失败状态
$ systemctl is-enabled application.service # 显示某个 Unit 服务是否建立了启动链接
```

#### Unit 管理

对于用户来说,最常用的是下面这些命令,用于启动和停止 `Unit`(主要是 `service`).

```bash
$ sudo systemctl start apache.service # 立即启动一个服务
$ sudo systemctl stop apache.service # 立即停止一个服务
$ sudo systemctl restart apache.service # 重启一个服务
$ sudo systemctl kill apache.service # 杀死一个服务的所有子进程
$ sudo systemctl reload apache.service # 重新加载一个服务的配置文件
$ sudo systemctl daemon-reload # 重载所有修改过的配置文件
$ systemctl show httpd.service # 显示某个 Unit 的所有底层参数
$ systemctl show -p CPUShares httpd.service # 显示某个 Unit 的指定属性的值
$ sudo systemctl set-property httpd.service CPUShares=500 # 设置某个 Unit 的指定属性
```

#### 依赖关系

`Unit` 之间存在依赖关系:`A` 依赖于 `B`,就意味着 `Systemd` 在启动 `A` 的时候,同时会去启动 `B`.

`systemctl list-dependencies`命令列出一个 `Unit` 的所有依赖.

```bash
$ systemctl list-dependencies nginx.service
```

上面命令的输出结果之中,有些依赖是 `Target` 类型(详见下文),默认不会展开显示.如果要展开 `Target`,就需要使用`--all`参数.

```bash
$ systemctl list-dependencies --all nginx.service
```

### Unit 的配置文件

#### 概述

每一个 `Unit` 都有一个配置文件,告诉 `Systemd` 怎么启动这个 `Unit` . `Systemd` 默认从目录`/etc/systemd/system/`读取配置文件.
但是,里面存放的大部分文件都是符号链接,指向目录`/usr/lib/systemd/system/`,真正的配置文件存放在那个目录, 也可能是`/lib/systemd/system/`.
`systemctl enable`命令用于在上面两个目录之间,建立符号链接关系.

```bash
$ sudo systemctl enable clamd@scan.service # 等同于
$ sudo ln -s '/usr/lib/systemd/system/clamd@scan.service' '/etc/systemd/system/multi-user.target.wants/clamd@scan.service'
```

如果配置文件里面设置了开机启动,`systemctl enable`命令相当于激活开机启动.
与之对应的,`systemctl disable`命令用于在两个目录之间,撤销符号链接关系,相当于撤销开机启动.

```bash
$ sudo systemctl disable clamd@scan.service
```

配置文件的后缀名,就是该 `Unit` 的种类,比如`sshd.socket`.如果省略,`Systemd` 默认后缀名为`.service`,所以`sshd`会被理解成`sshd.service`.

#### 配置文件的状态

`systemctl list-unit-files` 命令用于列出所有配置文件.

```bash
$ systemctl list-unit-files  # 列出所有配置文件
$ systemctl list-unit-files --type=service # 列出指定类型的配置文件
```

这个命令会输出一个列表.

```bash
$ systemctl list-unit-files
UNIT FILE              STATE
chronyd.service        enabled
clamd@.service         static
clamd@scan.service     disabled
```

这个列表显示每个配置文件的状态,一共有四种.

+ `enabled`:已建立启动链接
+ `disabled`:没建立启动链接
+ `static`:该配置文件没有`[Install]`部分(无法执行),只能作为其他配置文件的依赖
+ `masked`:该配置文件被禁止建立启动链接

注意,从配置文件的状态无法看出,该 `Unit` 是否正在运行.这必须执行前面提到的`systemctl status`命令.

```bash
$ systemctl status bluetooth.service
```

一旦修改配置文件,就要让 `SystemD` 重新加载配置文件,然后重新启动,否则修改不会生效.

```bash
$ sudo systemctl daemon-reload
$ sudo systemctl restart httpd.service
```

#### 配置文件的格式

配置文件就是普通的文本文件,可以用文本编辑器打开.

`systemctl cat`命令可以查看配置文件的内容.

```bash
$ systemctl cat atd.service
[Unit]
Description=ATD daemon
[Service]
Type=forking
ExecStart=/usr/bin/atd
[Install]
WantedBy=multi-user.target
```

从上面的输出可以看到,配置文件分成几个区块.每个区块的第一行,是用方括号表示的区别名,比如`[Unit]`.注意,配置文件的区块名和字段名,都是大小写敏感的.

每个区块内部是一些等号连接的键值对.

```bash
[Section]
Directive1=value
Directive2=value
. . .
```

注意,键值对的等号两侧不能有空格.

#### 配置文件的区块

`[Unit]`区块通常是配置文件的第一个区块,用来定义 `Unit` 的元数据,以及配置与其他 `Unit` 的关系.它的主要字段如下.

+ `Description`:简短描述
+ `Documentation`:文档地址
+ `Requires`:当前 `Unit` 依赖的其他 `Unit`,如果它们没有运行,当前 `Unit` 会启动失败
+ `Wants`:与当前 `Unit` 配合的其他 `Unit`,如果它们没有运行,当前 `Unit` 不会启动失败
+ `BindsTo`:与Requires类似,它指定的 `Unit` 如果退出,会导致当前 `Unit` 停止运行
+ `Before`:如果该字段指定的 `Unit` 也要启动,那么必须在当前 `Unit` 之后启动
+ `After`:如果该字段指定的 `Unit` 也要启动,那么必须在当前 `Unit` 之前启动
+ `Conflicts`:这里指定的 `Unit` 不能与当前 `Unit` 同时运行
+ `Condition`...:当前 `Unit` 运行必须满足的条件,否则不会运行
+ `Assert`...:当前 `Unit` 运行必须满足的条件,否则会报启动失败

`[Install]`通常是配置文件的最后一个区块,用来定义如何启动,以及是否开机启动.它的主要字段如下.

+ `WantedBy`:它的值是一个或多个 `Target`,当前 `Unit` 激活时(`enable`)符号链接会放入`/etc/systemd/system`目录下面以 `Target` 名 + `.wants`后缀构成的子目录中
+ `RequiredBy`:它的值是一个或多个 `Target`,当前 `Unit` 激活时,符号链接会放入`/etc/systemd/system`目录下面以 `Target` 名 + `.required`后缀构成的子目录中
+ `Alias`:当前 `Unit` 可用于启动的别名
+ `Also`:当前 `Unit` 激活(`enable`)时,会被同时激活的其他 `Unit`

`[Service]`区块用来 `Service` 的配置,只有 `Service` 类型的 `Unit` 才有这个区块.它的主要字段如下.

+ `Type`:定义启动时的进程行为.它有以下几种值.
+ `Type=simple`:默认值,执行 `ExecStart` 指定的命令,启动主进程
+ `Type=forking`:以 `fork` 方式从父进程创建子进程,创建后父进程会立即退出
+ `Type=oneshot`:一次性进程,`Systemd` 会等当前服务退出,再继续往下执行
+ `Type=dbus`:当前服务通过`D-Bus`启动
+ `Type=notify`:当前服务启动完毕,会通知`Systemd`,再继续往下执行
+ `Type=idle`:若有其他任务执行完毕,当前服务才会运行
+ `ExecStart`:启动当前服务的命令
+ `ExecStartPre`:启动当前服务之前执行的命令
+ `ExecStartPost`:启动当前服务之后执行的命令
+ `ExecReload`:重启当前服务时执行的命令
+ `ExecStop`:停止当前服务时执行的命令
+ `ExecStopPost`:停止当其服务之后执行的命令
+ `RestartSec`:自动重启当前服务间隔的秒数
+ `Restart`:定义何种情况 `Systemd` 会自动重启当前服务,可能的值包括`always`(总是重启), `on-success`, `on-failure`, `on-abnormal`, `on-abort`, `on-watchdog`
+ `TimeoutSec`:定义 `Systemd` 停止当前服务之前等待的秒数
+ `Environment`:指定环境变量

`Unit` 配置文件的完整字段清单,请参考官方文档.

### Target

启动计算机的时候,需要启动大量的 `Unit`.如果每一次启动,都要一一写明本次启动需要哪些 `Unit`,显然非常不方便.`Systemd` 的解决方案就是 `Target`.
简单说,`Target` 就是一个 `Unit` 组,包含许多相关的 `Unit` .启动某个 `Target` 的时候,`Systemd` 就会启动里面所有的 `Unit`.
从这个意义上说,`Target` 这个概念类似于"状态点",启动某个 `Target` 就好比启动到某种状态.

传统的`init`启动模式里面,有 `RunLevel` 的概念,跟 `Target` 的作用很类似.不同的是,`RunLevel` 是互斥的,不可能多个 `RunLevel` 同时启动,但是多个 `Target` 可以同时启动.

```bash
# 查看当前系统的所有 Target
$ systemctl list-unit-files --type=target
# 查看一个 Target 包含的所有 Unit
$ systemctl list-dependencies multi-user.target
# 查看启动时的默认 Target
$ systemctl get-default
# 设置启动时的默认 Target
$ sudo systemctl set-default multi-user.target
# 切换 Target 时,默认不关闭前一个 Target 启动的进程,
# systemctl isolate 命令改变这种行为,
# 关闭前一个 Target 里面所有不属于后一个 Target 的进程
$ sudo systemctl isolate multi-user.target
```

`Target` 与 传统 `RunLevel` 的对应关系如下.

Traditional runlevel      New target name     Symbolically linked to...

+ `Runlevel 0`  |  `runlevel0.target -> poweroff.target`
+ `Runlevel 1`  |  `runlevel1.target -> rescue.target`
+ `Runlevel 2`  |  `runlevel2.target -> multi-user.target`
+ `Runlevel 3`  |  `runlevel3.target -> multi-user.target`
+ `Runlevel 4`  |  `runlevel4.target -> multi-user.target`
+ `Runlevel 5`  |  `runlevel5.target -> graphical.target`
+ `Runlevel 6`  |  `runlevel6.target -> reboot.target`

它与`init`进程的主要差别如下.

+ 默认的 `RunLevel`(在`/etc/inittab`文件设置)现在被默认的 `Target` 取代,位置是`/etc/systemd/system/default.target`,通常符号链接到`graphical.target`(图形界面)或者`multi-user.target`(多用户命令行).
+ 启动脚本的位置,以前是`/etc/init.d`目录,符号链接到不同的 `RunLevel` 目录 (比如`/etc/rc3.d`, `/etc/rc5.d`等),现在则存放在`/lib/systemd/system`和`/etc/systemd/system`目录.
+ 配置文件的位置,以前`init`进程的配置文件是`/etc/inittab`,各种服务的配置文件存放在`/etc/sysconfig`目录.现在的配置文件主要存放在`/lib/systemd`目录,在`/etc/systemd`目录里面的修改可以覆盖原始设置.

### 日志管理

`Systemd` 统一管理所有 `Unit` 的启动日志.带来的好处就是,可以只用`journalctl`一个命令,查看所有日志(内核日志和应用日志).日志的配置文件是`/etc/systemd/journald.conf`.

`journalctl`功能强大,用法非常多.

```bash
$ sudo journalctl  # 查看所有日志(默认情况下 ,只保存本次启动的日志)
$ sudo journalctl -k # 查看内核日志(不显示应用日志)
# 查看系统本次启动的日志
$ sudo journalctl -b 
$ sudo journalctl -b -0
$ sudo journalctl -b -1 # 查看上一次启动的日志(需更改设置)
# 查看指定时间的日志
$ sudo journalctl --since="2012-10-30 18:17:16"
$ sudo journalctl --since "20 min ago"
$ sudo journalctl --since yesterday
$ sudo journalctl --since "2015-01-10" --until "2015-01-11 03:00"
$ sudo journalctl --since 09:00 --until "1 hour ago"

$ sudo journalctl -n # 显示尾部的最新10行日志
$ sudo journalctl -n 20 # 显示尾部指定行数的日志
$ sudo journalctl -f # 实时滚动显示最新日志
$ sudo journalctl /usr/lib/systemd/systemd # 查看指定服务的日志
$ sudo journalctl _PID=1 # 查看指定进程的日志
$ sudo journalctl /usr/bin/bash # 查看某个路径的脚本的日志

$ sudo journalctl _UID=33 --since today # 查看指定用户的日志
$ sudo journalctl -u nginx.service # 查看某个 Unit 的日志
$ sudo journalctl -u nginx.service --since today
$ sudo journalctl -u nginx.service -f # 实时滚动显示某个 Unit 的最新日志

$ journalctl -u nginx.service -u php-fpm.service --since today # 合并显示多个 Unit 的日志
# 查看指定优先级(及其以上级别)的日志,共有8级
# 0: emerg , 1: alert , 2: crit , 3: err, 4: warning
# 5: notice, 6: info, 7: debug
$ sudo journalctl -p err -b

$ sudo journalctl --no-pager # 日志默认分页输出,--no-pager 改为正常的标准输出
$ sudo journalctl -b -u nginx.service -o json # 以 JSON 格式(单行)输出
$ sudo journalctl -b -u nginx.serviceqq -o json-pretty # 以 JSON 格式(多行)输出,可读性更好

$ sudo journalctl --disk-usage # 显示日志占据的硬盘空间
$ sudo journalctl --vacuum-size=1G # 指定日志文件占据的最大空间
$ sudo journalctl --vacuum-time=1years # 指定日志文件保存多久
```

## Systemd 常见任务

### 开机启动

对于那些支持 `Systemd` 的软件,安装的时候,会自动在`/usr/lib/systemd/system`目录添加一个配置文件.
如果你想让该软件开机启动,就执行下面的命令(以`httpd.service`为例).

```bash
$ sudo systemctl enable httpd
```

上面的命令相当于在`/etc/systemd/system`目录添加一个符号链接,指向`/usr/lib/systemd/system`里面的`httpd.service`文件.
这是因为开机时,`Systemd`只执行`/etc/systemd/system`目录里面的配置文件.这也意味着,如果把修改后的配置文件放在该目录,就可以达到覆盖原始配置的效果.

### 启动服务

设置开机启动以后,软件并不会立即启动,必须等到下一次开机.如果想现在就运行该软件,那么要执行`systemctl start`命令.

```bash
$ sudo systemctl start httpd
```

执行上面的命令以后,有可能启动失败,因此要用`systemctl status`命令查看一下该服务的状态.

```bash
$ sudo systemctl status httpd
httpd.service - The Apache HTTP Server
Loaded: loaded (/usr/lib/systemd/system/httpd.service; enabled)
Active: active (running) since 金 2014-12-05 12:18:22 JST; 7min ago
Main PID: 4349 (httpd)
Status: "Total requests: 1; Current requests/sec: 0; Current traffic:   0 B/sec"
CGroup: /system.slice/httpd.service
├─4349 /usr/sbin/httpd -DFOREGROUND
...
12月 05 12:18:22 localhost.localdomain systemd[1]: Starting The Apache HTTP Server...
...
```

上面的输出结果含义如下.

+ `Loaded行`:配置文件的位置,是否设为开机启动
+ `Active行`:表示正在运行
+ `Main PID行`:主进程`ID`
+ `Status行`:由应用本身(这里是 `httpd` )提供的软件当前状态
+ `CGroup块`:应用的所有子进程
+ `日志块`:应用的日志

### 停止服务

终止正在运行的服务,需要执行`systemctl stop`命令.

```bash
$ sudo systemctl stop httpd.service
```

有时候,该命令可能没有响应,服务停不下来.这时候就不得不"杀进程"了,向正在运行的进程发出`kill`信号.

```bash
$ sudo systemctl kill httpd.service
```

此外,重启服务要执行`systemctl restart`命令.

```bash
$ sudo systemctl restart httpd.service
```

### 读懂配置文件

一个服务怎么启动,完全由它的配置文件决定.下面就来看,配置文件有些什么内容.
前面说过,配置文件主要放在`/usr/lib/systemd/system`目录,也可能在`/etc/systemd/system`目录.找到配置文件以后,使用文本编辑器打开即可.
`systemctl cat`命令可以用来查看配置文件,下面以`sshd.service`文件为例,它的作用是启动一个 `SSH` 服务器,供其他用户以 `SSH` 方式登录.

```bash
$ systemctl cat sshd.service
[Unit]
Description=OpenSSH server daemon
...
[Service]
EnvironmentFile=/etc/sysconfig/sshd
...
[Install]
WantedBy=multi-user.target
```

可以看到,配置文件分成几个区块,每个区块包含若干条键值对.

下面依次解释每个区块的内容.

#### [Unit] 区块:启动顺序与依赖关系.

`Unit`区块的`Description`字段给出当前服务的简单描述,`Documentation`字段给出文档位置.接下来的设置是启动顺序和依赖关系,这个比较重要.

`After`字段:表示如果`network.target`或`sshd-keygen.service`需要启动,那么`sshd.service`应该在它们之后启动.
相应地,还有一个`Before`字段,定义`sshd.service`应该在哪些服务之前启动.

注意,`After`和`Before`字段只涉及启动顺序,不涉及依赖关系.

举例来说,某 `Web` 应用需要 `postgresql` 数据库储存数据.在配置文件中,它只定义要在 `postgresql` 之后启动,而没有定义依赖 `postgresql` .
上线后,由于某种原因,`postgresql` 需要重新启动,在停止服务期间,该 `Web` 应用就会无法建立数据库连接.

设置依赖关系,需要使用`Wants`字段和`Requires`字段.
`Wants`字段:表示`sshd.service`与`sshd-keygen.service`之间存在"弱依赖"关系,即如果`sshd-keygen.service`启动失败或停止运行,不影响`sshd.service`继续执行.

`Requires`字段则表示"强依赖"关系,即如果该服务启动失败或异常退出,那么`sshd.service`也必须退出.
注意,`Wants`字段与`Requires`字段只涉及依赖关系,与启动顺序无关,默认情况下是同时启动的.

#### [Service] 区块:启动行为

`Service`区块定义如何启动当前服务.

#### 启动命令

许多软件都有自己的环境参数文件,该文件可以用 `EnvironmentFile` 字段读取.

`EnvironmentFile`字段:指定当前服务的环境参数文件.该文件内部的`key=value`键值对,可以用`$key`的形式,在当前配置文件中获取.

上面的例子中,`sshd` 的环境参数文件是`/etc/sysconfig/sshd`.
配置文件里面最重要的字段是`ExecStart`.
`ExecStart`字段:定义启动进程时执行的命令.

上面的例子中,启动`sshd`,执行的命令是`/usr/sbin/sshd -D $OPTIONS`,其中的变量`$OPTIONS`就来自`EnvironmentFile`字段指定的环境参数文件.

与之作用相似的,还有如下这些字段.

+ `ExecReload字段`:重启服务时执行的命令
+ `ExecStop字段`:停止服务时执行的命令
+ `ExecStartPre字段`:启动服务之前执行的命令
+ `ExecStartPost字段`:启动服务之后执行的命令
+ `ExecStopPost字段`:停止服务之后执行的命令

请看下面的例子.

```service
[Service]
ExecStart=/bin/echo execstart1
ExecStart=
ExecStart=/bin/echo execstart2
ExecStartPost=/bin/echo post1
ExecStartPost=/bin/echo post2
```

上面这个配置文件,第二行`ExecStart`设为空值,等于取消了第一行的设置,运行结果如下.

```bash
execstart2
post1
post2
```

所有的启动设置之前,都可以加上一个连词号(`-`),表示"抑制错误",即发生错误的时候,不影响其他命令的执行.
比如,`EnvironmentFile=-/etc/sysconfig/sshd`(注意等号后面的那个连词号),就表示即使`/etc/sysconfig/sshd`文件不存在,也不会抛出错误.

#### 启动类型

`Type`字段定义启动类型.它可以设置的值如下.

+ `simple(默认值)`: `ExecStart`字段启动的进程为主进程
+ `forking`:`ExecStart`字段将以`fork()`方式启动,此时父进程将会退出,子进程将成为主进程
+ `oneshot`:类似于`simple`,但只执行一次,`Systemd`会等它执行完,才启动其他服务
+ `dbus:`类似于`simple`,但会等待 `D-Bus` 信号后启动
+ `notify:`类似于`simple`,启动结束后会发出通知信号,然后 `Systemd` 再启动其他服务
+ `idle:`类似于`simple`,但是要等到其他任务都执行完,才会启动该服务.一种使用场合是为让该服务的输出,不与其他服务的输出相混合

下面是一个`oneshot`的例子,笔记本电脑启动时,要把触摸板关掉,配置文件可以这样写.

```service
[Unit]
Description=Switch-off Touchpad
[Service]
Type=oneshot
ExecStart=/usr/bin/touchpad-off
[Install]
WantedBy=multi-user.target
```

上面的配置文件,启动类型设为`oneshot`,就表明这个服务只要运行一次就够了,不需要长期运行.
如果关闭以后,将来某个时候还想打开,配置文件修改如下.

```service
[Unit]
Description=Switch-off Touchpad
[Service]
Type=oneshot
ExecStart=/usr/bin/touchpad-off start
ExecStop=/usr/bin/touchpad-off stop
RemainAfterExit=yes
[Install]
WantedBy=multi-user.target
```

上面配置文件中,`RemainAfterExit`字段设为`yes`,表示进程退出以后,服务仍然保持执行.
这样的话,一旦使用`systemctl stop`命令停止服务,`ExecStop`指定的命令就会执行,从而重新开启触摸板.

#### 重启行为

`Service`区块有一些字段,定义了重启行为.

`KillMode`字段:定义 `Systemd` 如何停止 `sshd` 服务.

上面这个例子中,将`KillMode`设为`process`,表示只停止主进程,不停止任何 `sshd` 子进程,即子进程打开的 `SSH session` 仍然保持连接.
这个设置不太常见,但对 `sshd` 很重要,否则你停止服务的时候,会连自己打开的 `SSH session` 一起杀掉.

`KillMode`字段可以设置的值如下.

+ `control-group`(默认值):当前控制组里面的所有子进程,都会被杀掉
+ `process`:只杀主进程
+ `mixed`:主进程将收到 `SIGTERM` 信号,子进程收到 `SIGKILL` 信号
+ `none`:没有进程会被杀掉,只是执行服务的 `stop` 命令.

接下来是`Restart`字段.`Restart`字段:定义了 `sshd` 退出后,`Systemd` 的重启方式.
上面的例子中,`Restart`设为`on-failure`,表示任何意外的失败,就将重启`sshd`.如果 `sshd` 正常停止(比如执行`systemctl stop`命令),它就不会重启.

Restart字段可以设置的值如下.

+ `no`(默认值):退出后不会重启
+ `on-success`:只有正常退出时(退出状态码为`0`),才会重启
+ `on-failure`:非正常退出时(退出状态码非`0`),包括被信号终止和超时,才会重启
+ `on-abnormal`:只有被信号终止和超时,才会重启
+ `on-abort`:只有在收到没有捕捉到的信号终止时,才会重启
+ `on-watchdog`:超时退出,才会重启
+ `always`:不管是什么退出原因,总是重启

对于守护进程,推荐设为`on-failure`.对于那些允许发生错误退出的服务,可以设为`on-abnormal`.
最后是`RestartSec`字段,表示 `Systemd` 重启服务之前,需要等待的秒数.上面的例子设为等待`42`秒.

#### [Install] 区块

`Install`区块,定义如何安装这个配置文件,即怎样做到开机启动.

+ `WantedBy字段`:表示该服务所在的 `Target`.

`Target`的含义是服务组,表示一组服务.`WantedBy=multi-user.target`指的是,`sshd`所在的 `Target` 是`multi-user.target`.
这个设置非常重要,因为执行`systemctl enable sshd.service`命令时,`sshd.service`的一个符号链接,就会放在`/etc/systemd/system`目录下面的`multi-user.target.wants`子目录之中. `Systemd` 有默认的启动 `Target`.

```bash
$ systemctl get-default
multi-user.target
```

上面的结果表示,默认的启动 `Target` 是 `multi-user.target`.在这个组里的所有服务,都将开机启动.这就是为什么`systemctl enable`命令能设置开机启动的原因.
使用 `Target` 的时候,`systemctl list-dependencies`命令和`systemctl isolate`命令也很有用.

```bash
# 查看 multi-user.target 包含的所有服务
$ systemctl list-dependencies multi-user.target
# 切换到另一个 target
# shutdown.target 就是关机状态
$ sudo systemctl isolate shutdown.target
```

一般来说,常用的 `Target` 有两个:一个是`multi-user.target`,表示多用户命令行状态；
另一个是`graphical.target`,表示图形用户状态,它依赖于`multi-user.target`.官方文档有一张非常清晰的 `Target` 依赖关系图.

### Target 的配置文件

`Target` 也有自己的配置文件.

```bash
$ systemctl cat multi-user.target
[Unit]
Description=Multi-User System
Documentation=man:systemd.special(7)
Requires=basic.target
Conflicts=rescue.service rescue.target
After=basic.target rescue.service rescue.target
AllowIsolate=yes
```

注意,`Target` 配置文件里面没有启动命令. 上面输出结果中,主要字段含义如下.

+ `Requires`字段:要求`basic.target`一起运行.
+ `Conflicts`字段:冲突字段.如果`rescue.service`或`rescue.target`正在运行,`multi-user.target`就不能运行,反之亦然.
+ `After`:表示`multi-user.target`在`basic.target` ,  `rescue.service`, `rescue.target`之后启动,如果它们有启动的话.
+ `AllowIsolate`:允许使用`systemctl isolate`命令切换到`multi-user.target`.

### 修改配置文件后重启

修改配置文件以后,需要重新加载配置文件,然后重新启动相关服务.

```bash
$ sudo systemctl daemon-reload  # 重新加载配置文件
$ sudo systemctl restart foobar # 重启相关服务
```

## cgroup

[Control groups](https://wiki.archlinux.org/title/Cgroups)
