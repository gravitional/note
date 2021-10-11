# ubuntu-1

***
`ls *.tex` and `"ls *.tex"`
前一种 bash 认为 `*.tex` 是参数,后一种 bash 认为 `"ls *.tex"` 是一个整体命令的名字.

bash 物理行上单个语句不用分号,两个语句并列时,采用分号.

***
如果用显式字符串作 `cd` 的参数,应该用绝对路径避免`~`的解析问题.如

```bash
cd  /home/tom/Downloads
```

***
`whoami`会输出用户的名称

***
bash -c `command` arg1 arg2 ...

这种调用形式,会读取 `-c`后面的`command_string`并执行命令,然后退出
如果在`command_string`之后有参数,则第一个参数分配给`$0`,其余所有参数分配给`位置参数`.
`$0`的赋值设置`shell`的名称,该名称用于`warning`和`error`消息.如

```bash
bash -c 'ls;echo;uptime'
```

## 日常

命令可以是下面四种形式之一:

1. 是一个可执行程序,就像我们所看到的位于目录`/usr/bin` 中的文件一样.
属于这一类的程序,可以编译成二进制文件,诸如用 `C` 和 `C++`语言写成的程序, 也可以是由脚本语言写成的程序,比如说 `shell`,`perl`,`python`,`ruby`,等等.
2. 是一个内建于 `shell` 自身的命令.bash 支持若干命令,内部叫做 shell 内部命令 `(builtins`).例如,`cd` 命
令,就是一个 `shell` 内部命令.
3. 是一个 `shell` 函数.这些是小规模的 `shell` 脚本,它们混合到环境变量中. 在后续的章节里,我们将讨论配
置环境变量以及书写 shell 函数.但是现在, 仅仅意识到它们的存在就可以了.
4. 是一个命令别名.我们可以定义自己的命令,建立在其它命令之上

***
[oh_my_zsh](https://ohmyz.sh/#install)

查看或设置主题

```zsh
_omz::theme list agnoster
_omz::theme set agnoster
```

更新 `omz`

```
_omz::update
```

***
powerline

[Powerline is a statusline plugin](https://github.com/powerline/powerline)
[Installation on Linux](https://powerline.readthedocs.io/en/latest/installation/linux.html)

get the latest release version

```powershell
pip install --user powerline-status
```

### 常用命令

+ `type` – 说明怎样解释一个命令名
+ `which` – 显示会执行哪个可执行程序
+ `man` – 许多希望被命令行使用的可执行程序,提供了一个正式的文档,叫做手册或手册页(man page).一个特殊的叫做`man` 的分页程序,可用来浏览他们
+ `apropos` - 显示适当的命令,基于某个关键字的匹配项.虽然很粗糙但有时很有用.
+ `info` – 项目提供了一个命令程序手册页的替代物,称为`info`.
+ `whatis` – 程序显示匹配特定关键字的手册页的名字和一行命令说明
+ `alias` – 创建命令别名
+ `type` 命令是 `shell` 内部命令,它会显示命令的类别
+ `which`这个命令只对可执行程序有效,不包括内部命令和命令别名,别名是真正的可执行程序的替代物
+ `bash` 有一个内建的帮助工具,可供每一个 `shell` 内部命令使用.输入`help`,接着是 `shell` 内部命令名.例如: `help cd`
+ ` --help` : 许多可执行程序支持这个选项, 显示命令所支持的语法和选项说明.
+ `less ` 浏览文件内容
+ `basename file suffix` 用来去掉文件后缀名
+ `/bin/kill -L` : 查看linux `kill` 的数字对应的短语
+ `ldd`查看依赖信息
+ `sha256sum`: 计算并检查 `SHA256` message digest (消息摘要)
+ `xdg-open`: 可以设置别名为`open`, 使用默认的程序打开文件或者`url`.
+ `lsusb`: 用于显示系统中的`USB`总线和连接到它们的设备信息的工具. 
+ `lspci`: 用于显示系统中的`PCI`总线和连接到它们的设备的信息的工具. Peripheral Component Interconnect, 外围组件互连标准. `pcie`--express:高速串行总线.
+ `ssh remote-sys 'ls * > dirlist.txt'` ; 远程连接到`remote-sys`, 并执行`ls * > dirlist.txt` 命令.

***
`echo`输出的时候,可以考虑改变颜色增加辨认度

```bash
echo  -e "\033[1;47m\033[1;32m Testing output... "
```

***

`ls --color=always | less -R`: 这个命令可以保持颜色控制字符的传递. 

`less -R` or `--RAW-CONTROL-CHARS`:

和`-r`一样, 但只把ANSI "color"转义序列以 "raw" 形式输出.  大多数情况下幕外观保持正确. 
ANSI "颜色"转义序列是以下形式的序列：`ESC [ ... m`. 其中`...`是`0`个或更多的颜色规范字符. 为了保持屏幕外观, `ANSI`颜色转义序列被认为不会移动光标.  
你可以设置`m`之外的结束字符, 方法是将环境变量`LESSANSIENDCHARS`设为结束颜色转义序列的字符列表. 
也可以设置环境变量`LESSANSIMIDCHARS`,使标准字符之外的字符出现在`ESC`和`m`之间

或者可以使用`git diff --word-diff --no-index file1 file2 `. `--word-diff`指定按单词模式显示差异, `--no-index`表明不是与`cached`作比较, 而是比较工作区中的文件. 

***

+ `date` :日期
+ `cal`: 日历
+ `df` :磁盘剩余空间
+ `free`: 空闲内存
+ ` file`  :确定文件类型
+ `which`: 确定命令的位置, `where`: `zsh`内置命令, `which`:内置命令, 也有二进制, `whereis`: 二进制程序.
+ `type`: 用来查看命令的类型
+ `cd -` :更改工作目录到先前的工作目录
+ `cd ~user_name` :切换到用户家目录
+ `cp -u *.html destination` :更新文件到destination
+ `ln file link` :创建硬链接
+ `ln -s item link` :创建符号链接, `item`可以是一个文件或目录,`gnome`中,按住`ctrl+shift`拖动会创建链接.
+ `df -hT`: `h`让你以 `MB` 或 `G` 为单位查看磁盘的空间, `T`打印设备类型

***
查看系统版本信息

[ubuntu：查看ubuntu系统的版本信息](https://blog.csdn.net/whbing1471/article/details/52074390)

```bash
cat /proc/version
uname -a
sb_release -a
```

*** 
查看主要存储设备的使用情况

```bash
df -h | sort -hr --key=2
```

### 环境变量

[/etc/environment 与 /etc/profile区别](https://blog.csdn.net/lijingshan34/article/details/86568596)

`/etc/environment`是设置整个系统的环境, 而`/etc/profile`是设置所有用户的环境, 前者与登录用户无关, 后者与登录用户有关. 

跟环境变量相关的参数：

`/etc/profile` ->`/etc/enviroment` -->`$HOME/.profile` -->`$HOME/.env`

如果你把文本放在双引号中, `shell` 使用的特殊字符, 除了`\`(反斜杠),`$` ,和 `` ` ``(倒引号)之外, 则失去它们的特殊含义,被当作普通字符来看待. 

这意味着

+ 单词分割, (`空格`)
+ 路径名展开, (`*``?`)
+ 波浪线展开,(`~`)
+ 和花括号展开(`{}`)

都被禁止,然而

+ 参数展开(`$USER`)
+ 算术展开(`$(())`)
+ 命令替换`$()`

仍被执行, 所以在`.zshrc` or `.bashrc` 中设置环境变量的时候, 如果需要用到`~`, 那么就不用加`"` or `'`,(不要加任何引号)

```bash
export PATH=/usr/local/opt/coreutils/libexec/gnubin:~/bin:$PATH
```

`$HOME`:用户目录

### shell 模式切换

1. 查看系统支持的shell模式及位置

`echo &SHELL`
`cat /etc/shells`

2. 切换shell为/bin/sh

`# chsh -s /bin/sh`

### 重启x-org

[xorg 重新启动X窗口服务器](https://www.kaifa99.com/ubuntu/article_156280)

在`systemd`系统上(`Ubuntu 15.04`和更新版本)

```bash
sudo systemctl restart display-manager
```

注：这将强制退出所有图形程序, 将丢失未保存的工作, 强制被注销. 非图形程序不会受到影响. 

对于其他`Ubuntu`版本, 首先使用以下命令找到`Ubuntu`的显示管理器：

```bash
cat /etc/X11/default-display-manager
```

根据显示管理器的不同, 可以使用以下命令之一：

```bash
sudo restart lightdm # 使用LightDM 
sudo restart gdm # Gnome (带GDM )
sudo restart kdm # KDE (带KDM )
sudo restart mdm # 对于MDM (例如对于Mint Cinnamon )
```

[Display manager](https://wiki.archlinux.org/title/Display_manager)

`显示管理器`(display manager), 或称`登录管理器`(login manager), 通常是一个图形用户界面, 在 boot 过程结束时显示, 以取代默认的shell. 
显示管理器有多种实现方式, 就像有各种类型的 window managers 和 desktop environments 一样. 通常每一种都有一定程度的定制和主题性可供选择. 

### 录制屏幕

如果是`Gnome3`系用户,可以按`ctrl + shift + alt + r`,屏幕右下角有红点出现,则开始录屏,
要结束的话再按一次`ctrl + shift + alt + r`,录好的视频在`~/video`下

### ls 选项

`ls -d */`

+ `-d`= 选项指定只列出目录,`glob`模式当前目录下`*/`表示所有的子目录
+ `-S` 按文件大小排序,大的优先
+ `--sort=WORD` =  按`WORD`排序,而不是`name`: none (-U), size (-S), time (-t), version (-v), extension (-X)
+ `--time=WORD`= 和 `-l`一起使用,使用`WORD`代替默认的修改时间:atime or access or use (-u); ctime or status (-c); also use specified time as sort key if  `--sort=time` (newest first)
+ `-X` = 按拓展名的字母顺序排列
+ `-m`用逗号分隔的条目列表填充宽度
+ `-x` 按行而不是按列输出条目
+ `-b, --escape`: 对非图形字符, 打印`C`式转义符
+ `-q, --hide-control-chars`: 对非图形字符, 打印`?`
+ `-1`: 每行打印一个文件. 可以使用`-q`或者`-b`避免`\n`
+ `--format=WORD` 横跨`-x`,逗号`-m`,水平`-x`,长`-l`,单列`-1`,verbose`-l`,垂直`-C`

### 别名(alias)

[Linux shell 脚本中使用 alias 定义的别名](https://www.cnblogs.com/chenjo/p/11145021.html)

可以把多个命令放在同一行上,命令之间 用`;`分开

```bash
command1; command2; command3...
```

我们会用到下面的例子:

```bash
$ cd /usr; ls; cd -
bin games
kerberos lib64
local
```

正如我们看到的,我们在一行上联合了三个命令.
首先更改目录到`/usr`,然后列出目录 内容,最后回到原始目录(用命令`cd -`),结束在开始的地方.
现在,通过 `alia` 命令 把这一串命令转变为一个命令.

为了查清此事,可以使用 type 命令:

```bash
$ type test
test is a shell builtin
```

哦!`test`名字已经被使用了.试一下`foo`:

```bash
$ type foo
bash: type: foo: not found
```

创建命令别名:

```bash
$ alias foo='cd /usr; ls; cd -'
```

注意命令结构:

```bash
alias name='string'
```

在命令`alias`之后,输入`name`,紧接着(没有空格)是一个等号,等号之后是 一串用引号引起的字符串,字符串的内容要赋值给 `name`.

删除别名,使用 unalias 命令,像这样:

```bash
$ unalias foo
$ type foo
bash: type: foo: not found
```

如果想要永久保存定义的`alias`,可以将其写入到 `/etc/profile` 或者 `~/.bash_rc` 中去,
两个的区别是影响的范围不一样而已

#### zsh 别名

+ `grep`='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'

### 文件管理 cp rm mv

复制移动的时候,可以加上 `-i` 参数,防止覆盖

`cp [OPTION]... SOURCE... DIRECTORY`

`...` 表示可以重复

`cp -i  ... ... `

`cp -irf  ... ... `

短命令可以堆叠, `-i -r -f`=`-irf`=`--interactive --force --recursive`

`-R` 如果`source_file`是目录, 则`cp`复制目录和整个子目录.  如果`source_file`以`/`结尾, 则复制目录内容, 而不是此目录本身. 
此选项还会导致复制符号链接, 而不是复制链接对应的文件, 并让`cp`创建特殊文件, 而不是普通文件. 创建的目录与相应的源目录具有相同的`mode`, 不会被`umask`修改. 
在`-R`模式下, 即使检测到错误, `cp`仍将继续复制. 
请注意, `cp`将硬链接的文件单独复制一份.  如果你需要保持硬链接, 考虑改用`tar(1)`, `cpio(1)`, or `pax(1)`. 

`-a`与`-pPR`选项相同.  保留文件的结构和属性, 但不保留目录结构. 
***
要删除名称以`-`开头的文件,例如` -foo`,请使用以下命令之一:

+ `rm -- -foo`
+ `rm ./-foo`

删除本层目录下除了源文件的`latex`辅助文件

```
temp_a=$(find . -mindepth 1 -maxdepth 1 -type f   \( -not -name  "*.pdf" \)  \( -not -name  "*.tex" \) \( -not -name  "*.bib" \) -print0); if [[ ${temp_a} != '' ]]; then  echo -n ${temp_a} |  xargs --null rm; fi
```

可以多用花括号展开,指定多个文件名,例如:

```bash
ls ~/draft/draft.2008{03,04,05}.nb
/home/tom/draft/draft.200803.nb  /home/tom/draft/draft.200804.nb  /home/tom/draft/draft.200805.nb
```

删除本目录下的子目录, 只保留文件:

```bash
find . -mindepth 1 -maxdepth 0 -type d # 先查看输出
find . -mindepth 1 -maxdepth 1 -type d -exec rm -rf '{}' + #使用 + 号将输出合并到 rm -rf 后面
```

其中`{}`是当前路径名的符号表示,`;`界定符表示命令结束, `+`界定符表示合并结果一起执行命令.

### 重命名 rename

重命名除了使用`mv`,也可以使用`rename`.  在`ubuntu`上, 有两个版本的`rename`程序.
`perl`版的`rename`程序就叫做`rename`, `util-linux`包中的版本叫做`rename.ul`.

以下是简单的说明:

***
rename - renames multiple files

SYNOPSIS

```bash
rename [ -h|-m|-V ] [ -v ] [ -n ] [ -f ] [ -e|-E perlexpr]*|perlexpr [ files ]
```

DESCRIPTION

`rename`根据指定为第一个参数的规则重命名提供的文件名.
`perlexpr`参数是一个`Perl`表达式,它修改`Perl`中的`$ _`字符串.
如果给定的文件名未被表达式修改,则不会重命名.
如果命令行中未提供文件名,则将通过标准输入读取文件名.

`perlepxr`三种模式,分别是:

+ 匹配; `m`
+ 替换; `s`
+ 转化; `tr` or `y` ： 相当于一个映射表格, 进行批量替换

`perlepxr`的后向引用

```perl
$` : 匹配部分的前一部分字符串
$& : 匹配的字符串
$'  : 还没有匹配的剩余字符串
$1 : 反向引用的第一个字符串
```

`/expr/sub/mod`中`mod`表示模式修饰符, 可以是

+ `i`    如果在修饰符中加上`$1`, 则正则将会取消大小写敏感性, 即`$1`和`$1` 是一样的. 
+ `m`    默认的正则开始`^`和结束`$`只是对于正则字符串. 
如果在修饰符中加上`m`, 那么开始和结束将会指字符串的每一行：每一行的开头就是`^`, 结尾就是`$`. 
+ `o`    表达式只执行一次. 
+ `s`    如果在修饰符中加入`s`, 那么默认的`.`代表除了换行符以外的任何字符将会变成任意字符, 也就是包括换行符！
+ `x`    如果加上该修饰符, 表达式中的空白字符将会被忽略, 除非它已经被转义. 
+ `g`    替换所有匹配的字符串. 
+ `e`    替换字符串作为表达式

例如,要重命名所有匹配`* .bak`的文件,以去除扩展名,可以用

```bash
rename 's/\w.bak$//' *.bak
```

要将大写名称转换为小写,可以使用

```bash
rename 'y/A-Z/a-z/' *
```

把文件名中的 中划线 改称 下划线

```bash
rename -n 'y/-/_/' */* # 先使用 -n 查看将被改名的文件, 但不执行操作
rename -v 'y/-/_/' */* # 去掉 -n 选项, 执行操作
```

***
参数

+ ` -v, --verbose`;  打印出重命名成功的文件.
+ `-0, --null`; 当从`STDIN`读取时, 使用`\0`作为分隔符. 
+ `--path, --fullpath`; Rename full path: 重命名任何路径元素, 默认行为
+ `-d, --filename, --nopath, --nofullpath`; 不重命名文件夹, 只重命名文件部分. 
+ `-n, --nono`;No action: 打印出要重命名的文件, 但不执行操作
+ `-e  Expression`; 作用到文件名上的代码. 可以重复使用`-e expr1 -e expr2 ...`来构建代码, (like `perl -e`). 如果没有`-e`, 第一个参数被当成`code`
+ `-E Statement`; 类似于`-e`, 但需要`;`结束

### 获取绝对路径 realpath

`realpath` - `print the resolved path`(打印已解析的路径)

SYNOPSIS
`realpath [OPTION]... FILE...`

DESCRIPTION
打印解析的绝对文件名； 除最后一个组件外的所有组件都必须存在

`-e`, `--canonicalize-existing`: 路径的所有组成部分必须存在
`-m`,`--canonicalize-missing`:路径组件不需要存在,也不必是目录
`-L`, `--logical`:解析符号链接前的`..`组件
`-P`, `--physical`:解析遇到的符号链接(默认)
`-q`, `--quiet`:禁止显示大多数错误消息
`--relative-to=DIR`:打印相对于`DIR`的解析路径
`--relative-base=DIR`:只打印`DIR`后面的绝对路径路径
`-s`, `--strip, --no-symlinks`:不扩展符号链接
`-z`,` --zero`:用NUL而不是换行符结束每个输出行

canonical order: 在排序中,指一种标准的顺序,比如字母顺序.

### tar unzip

***
创建压缩文件

+ `tar -cvf a.tar /etc`
+ `gzip foo.txt`
+ `gzip -fvr foo.txt `: force,verbose,recursive
+ `zip -r foo.zip a b c ...`

创建存档的同时用`gunzip`压缩: `tar -czvf a.tar.gz /etc`.  
`tar`默认把路径当成相对路径, 如果提供的路径为`/home/user/file`, `tar`在创建存档时依次创建这些目录层次, 并且去掉开头的`/`, 使用`-P`选项改变默认设置.
下面讨论一些`tar`的选项：

+ `-P, --absolute-names`:在创建存档时, 不去掉领头的`/`.
+ `--no-recursion`:避免自动递归子目录. 
+ `--recursion`:递归子目录, 默认. 
+ `-f, --file=ARCHIVE`: 设置存档用的文件或设备为`ARCHIVE`. 

如果未提供`--file=ARCHIVE`, 则`tar`将首先检查环境变量`TAPE`. 如果`TAPE`不为`null`, 其值将用作存档名称. 
否则, `tar`将采用编译的默认值. 默认值可以使用`--show-defaults`选项, 或在`tar --help`输出的末尾查看. 
用带有`:`的存档名称表示远程计算机上的文件或设备. 冒号之前的部分作为机器名称或`IP`地址, 其后的部分为文件或设备路径名, 例如：`--file=remotehost:/dev/sr0`.
也可以使用`user@host`, 即`用户名@主机名`的形式. 默认情况下, 通过`rsh(1)`命令访问远程主机. 如今通常使用`ssh(1)`代替, 可以通过以下选项指定：`--rsh-command=/usr/bin/ssh`. 
远程计算机应安装了`rmt(8)`命令. 如果远程机器上`rmt`的路径名与`tar`的默认不匹配, 可以通过`--rmt-command`选项来指定正确的路径. 
使用`--force-local`选项: 即使带有`:`号, 也认为文件存在于本地. 

***
解压缩`xxx.tar.gz`

+ `tar -xzvf  xxx.tar.gz`
+ `gzip foo.txt`
+ `gzip -tv foo.txt.gz` : test,检验压缩文件完整性
+ `unzip file[.zip] [file(s) ...]  [-x xfile(s) ...] [-d exdir]`  文件名中可以使用通配符,但要`quote`起来

查看压缩文件内容,以下命令都可以

+ `tar -tf  xxx.tar.gz`
+ `tar -tzf  xxx.tar.gz`
+ `gunzip -c foo.txt | less`
+ `zcat foo.txt.gz | less`
+ `unzip -l file[.zip] [file(s) ...]`

支持的解压过滤器如下

+ `-a, --auto-compress`  使用存档后缀来确定压缩程序. 
+ `-I, --use-compress-program=COMMAND`: 通过COMMAND过滤数据.  它必须接受`-d`选项以进行解压缩.  该参数可以包含命令行选项. 
+ `-j, --bzip2`: 通过`bzip2(1)`过滤存档. 
+ `-J, --xz`: 通过`xz(1)`过滤存档. 
+ `--lzip` :通过`lzip(1)`过滤存档. 
+ `--lzma` :通过`lzma(1)`过滤存档. 
+ `--lzop`:通过lzop(1)过滤存档. 
+ `--no-auto-compress`: 不要使用存档后缀来确定压缩程序. 
+ `-z, --gzip, --gunzip, --ungzip`: 通过`gzip(1)`过滤存档. 
+ `-Z, --compress, --uncompress`通过`compress(1)`过滤存档. 
+ `--zstd`: 通过`zstd(1)`过滤存档. 

`unzip` 解压特定文件合并到当前目录: 使用`unzip -j`选项: junk paths, 不会重新创建档案的目录结构； 所有文件都存放在提取目录中(默认为当前目录), 结合通配符.

```bash
unzip -j '*.zip'  '*.otf'
```

### 7z

支持的格式

`LZMA2`, `XZ`, `ZIP`, `Zip64`, `CAB`, `RAR` (如果安装了 non-free `p7zip-rar`包),
`ARJ`,  `GZIP`, `BZIP2`, `TAR`, `CPIO`, `RPM`, `ISO`

用法: `7z <command> [<switches>...] <archive_name> [<file_names>...] [<@listfiles...>]`

+ 解压缩,输入密码, `x`动作保持压缩档案的目录结构:

```bash
7z x -p1234 filename
```

+ 压缩单个文件, `a`动作即添加压缩档案

```bash
7z a -t7z archive_name filename
```

+ 压缩`txt`中的文件

```bash
7z a -t7z configrc.win.7z @tom.rc_list.win
7z a -t7z configrc.linux.7z @tom.rc_list.linux
```

+ 各种动作

+ `a` : 添加文件到归档中
+ `b` : Benchmark
+ `d` : 从归档中删除
+ `e` : 从归档中提取(不使用目录名)
+ `h` : 计算文件的 hash 值
+ `i` : 展示支持的格式
+ `l` : 列出归档的内容
+ `rn` : 重命名归档中的文件
+ `t` : 检查归档的完整性
+ `u` : 把文件更新到归档
+ `x` : 提取文件, 使用全路径(也就是保持文件结构)

+ 可以使用的开关. 开关后面跟的具体参数不需要空格隔开.

+ `--` : 停止解析开关
+ `-o{Directory}` : 设置输出目录
+ `-p{Password}` : 设置密码
+ `-r[-|0]` : 递归子目录,作者不推荐使用
+ `-y` : 对所有询问回答 yes
+ `-t{Type}`:设置归档的类型
+ `-ai[r[-|0]]@{listfile} | !{wildcard}`: 包括额外的压缩文件, 以及通配符. 支持使用多次.
+ `-an`: 禁止命令行自动解析压缩文档的名字,需要与`-ai` (Include archives)开关一起使用 
+ `-m`: 设置压缩方法开关
+ `-v{Size}[b | k | m | g]`: 创建 Volumes, 指定分卷体积. 

`-an`禁止解析命令行中的压缩文档名称. 此开关必须与 `-ai` (Include archives) 开关一起使用.
如果你的压缩文档在列表文件中给出, 你要用`-ai`开关指定它, 所以在命令行中你需要禁止解析`存档_名称`字段. 

***

+ `-m`: 设置压缩方法开关. 语法:  `-m<method_parameters>`(压缩方法).  
    例如`7z a -pa.7z-mx=0 a.txt` 创建加密文档`a.7z`, 只是简单复制.

    开关描述中的 "默认值 "是指如果没有指定开关, 将使用的值. 
    允许使用布尔型开关的简化形式：`sw+`或`sw`代替`sw=on`, `sw-`代替`sw=off`. 
    此开关的格式取决于存档类型: `Zip `, `GZip `, `BZip2 `, `7z `, `XZ `, `WIM `. `-m`开关还可以为`h`(Hash)命令指定散列方法. 

    对于`7z`格式,
    Parameter  Default  Description  

  + `x=[0 | 1 | 3 | 5 | 7 | 9 ]`; `5`;  例如: `x=9`.定压缩级别. 将选择不同的`Dictionary`, `FastBytes`,  `MatchFinder`, `Filter` .
  + `yx=[0 | 1 | 3 | 5 | 7 | 9 ]`; `5`; 文件分析的级别. 
  + `s=[off | on | [e] [{N}f] [{N}b | {N}k | {N}m | {N}g | {N}t]`; `on`;  默认模式是`s=on`. 在`solid`模式下, 文件被分组在一起, 通常可以提高压缩率. 
  + `qs=[off | on]`; `off`;  在固实档案中按类型对文件进行分类.  

***

+ `-v{Size}[b | k | m | g]`: 创建 Volumes, 指定分卷体积. 

以 `Bytes`,` Kilobytes` (1 `Kilobyte` = 1024 `bytes`), `Megabytes` (1 `Megabyte` = 1024 `Kilobytes`) 或 `Gigabytes` (1 `Gigabyte` = 1024 `Megabytes`) 为单位指定卷的大小. 
如果你只指定 `{Size}`, `7-zip` 会把它当作字节来处理. 可以指定多个 `-v` 开关. 
注意：在完成归档之前, 请不要使用卷(也不要复制卷). `7-Zip` 可能在归档操作结束时改变任何卷, 包括第一个卷. 

例子, 创建多卷`a.7z`档案. 第一个卷是`10KB`, 第二个是`15KB`, 其他都是`2MB`. :

```bash
7z a a.7z *.txt -v10k -v15k -v2m
```

+ `7z`解压多个`.zip`文件; 使用下列形式:
    
    ```
    7z -an -ai[r[-|0]]@{listfile} | !{wildcard}
    ```

`-an`表示禁止`bash`解析压缩文档名, `-ai`表示要包括的压缩文件名. 由于`!`在`bash`中是特殊符号, 需要转义或者用引号`'`裹起来：

```bash
7z x -an -'air!*.zip'
7z t -an -ai!*.7z -ax!a*.7z # 测试所有*.7z 归档, 但排除 a*.7z 归档
find . -mindepth 1 -maxdepth 1 -type f -iname *.zip -exec 7z x -o'{}' '{}' + #使用 + 号将输出合并到 rm -rf 后面
```

类似的, `-i` 开关表示添加文件, 在创建压缩文档时使用.
 
```bash
7z a -tzip src.zip *.txt -ir!DIR1\*.cpp
```

将当前目录下的所有`*.txt`文件, 以及`DIR1`目录和所有它的子目录中的`*.cpp`文件添加到`src.zip` 中.

+ 解压`zip`文件同时, 创建外层的同名文件夹. 处理带有空格的文件名参考[For Loop File Names With Spaces](https://www.cyberciti.biz/tips/handling-filenames-with-spaces-in-bash.html)

```bash
SAVEIFS=$IFS;IFS=$(echo -en "\n\b"); #定义分词关键字为 \n\b newline, backspace
for f in *;do  echo "$f";done
IFS=$SAVEIFS;
```

所以, 可处理含有空格文件的, 批量解压缩的脚本为:

```bash
SAVEIFS=$IFS;IFS=$(echo -en "\n\b");
declare -a archs=( $(find . -mindepth 1 -maxdepth 1 -type f -iname '*.zip' -print0 | xargs --null  basename -s '.zip' ) );declare -p archs;
for i in ${archs}; do  unzip $i -d $i; done; 
IFS=$SAVEIFS
```

#### 7z 的通配符

通配符或带空格的文件名必须加引号.

    "Dir\Program files\*"
    Dir\"Program files"\*

`开关选项`可以组合起来以节省命令行长度. 
然而, 一些开关选项具有`可选参数`, 因此, 这种选项必须在`开关组合`的末尾, 因为`7-Zip`把`开关组合`后面的参数看作开关选项的`可选参数`. 

`7-Zip`使用类似于`Windows 95`的通配符.

+ `*`表示一个任意字符的序列. 
+ `?`意味着任何字符. 

>`7-Zip`不使用系统的通配符解析器. 
`7-Zip`不遵循古老的规则, 也就是`*.*`表示任何文件. `7-Zip`将`*.*`视为任何有扩展名的文件名. 要处理所有的文件, 你必须使用单个`*`通配符.
举例来说:

`*.txt`表示所有扩展名为`.txt`的文件. 
`?a*`表示所有第二个字符为`a`的文件 
`*1*`表示所有包含字符`1`的名字 
`*.*.*`表示所有包含两个至少`.`字符的名称. 

如果命令行中没有`文件名`/`通配符`, 将使用默认通配符`*`.
路径末尾的斜杠`\`表示一个目录. 如果路径末尾没有斜杠`\`, 路径可以指一个文件或一个目录. 

+ 列出文件

你可以为特殊的列表文件(包含文件列表的文件)提供一个或多个文件名或通配符. 这种列表文件中的文件名必须用换行符号分开.
对于列表文件, `7-Zip`默认使用`UTF-8`编码. 你可以使用 `-scs` 开关改变编码. 支持多个列表文件. 
例如, 如果文件 `listfile.txt` 包含以下内容. 

    My programs\*.cpp
    Src\*.cpp

那么命令

```bash
7z a -tzip archive.zip @listfile.txt
```

将 `My programs` 和 `Src` 目录下的所有 `*.cpp` 文件添加到`archive.zip`中. 

+ 短和长的文件名; `7-Zip`在某些情况下支持短文件名(如`FILENA~1.TXT`). 然而, 我们强烈建议只使用真正的(长)文件名. 

### 查看和安装字体

`fc-list`: 列出系统中可用的字体

```bash
# 语法
fc-list [ -vVh ]  [ pattern  [ element... ]   ]
# 例如列出所有中文字体, :lang=zh  代表匹配模式
fc-list :lang=zh
```

#### 安装字体

[Ubuntu系统字体命令和字体的安装](https://www.jianshu.com/p/e7f12b8c8602)

字体有`.ttf格`式(truetype font)和`.otf`格式(opentype font)字体

如果系统中没有中文字体,需要先行安装中文字体,在`Ubuntu`和`Cent OS`中的安装步骤如下:

+ 从网络上下载字体或者直接从其他计算机(windows)上拷贝
+ 建立`/usr/share/fonts/myfonts` 目录
+ `cd /usr/share/fonts/`

在`/etc/fonts/conf.d`目录下,有字体配置文件的符号链接

如果`fonts/`目录不存在,则创建

```bash
mkdir fonts
mkdir myfonts
```

把下载好的字体拷贝到`/usr/share/fonts/myfonts`目录下:

```bash
sudo cp ~/myfonts/* /usr/share/fonts/myfonts/
# ~/myfonts/ 是保存字体的目录
```

+ 修改字体文件的权限,使root用户以外的用户也可以使用

```bash
cd /usr/share/fonts/
sudo chmod -R  755 myfonts/
```

(5) 建立字体缓存

```bash
cd /usr/share/fonts/myfonts
sudo mkfontscale && mkfontdir && fc-cache -fv
```

```bash
sudo mkfontscale
# 如果提示 mkfontscale: command not found
# 在Ubuntu下运行如下命令
# sudo apt-get install ttf-mscorefonts-installer
# 在cent os下运行如下命令
# yum install mkfontscale
sudo mkfontdir
sudo fc-cache -fv
# fc-cache - build font information cache files
# 如果提示 fc-cache: command not found
# 在Ubuntu下运行如下命令
# sudo apt-get install fontconfig
# 在cent os下运行如下命令
# yum install fontconfig
```

至此字体就安装成功了,如果需要安装其他字体,只需将字体拷贝到字体目录下,重新运行以上的命令即可.

### apt 与 apt-get

[Linux中apt与apt-get命令的区别与解释](https://www.sysgeek.cn/apt-vs-apt-get/)

如果你已阅读过我们的 `apt-get` 命令指南,可能已经遇到过许多类似的命令,如`apt-cache`,`apt-config` 等.如你所见,这些命令都比较低级又包含众多功能,普通的 Linux 用户也许永远都不会使用到.换种说法来说,就是最常用的 Linux 包管理命令都被分散在了 `apt-get`,`apt-cache` 和 `apt-config` 这三条命令当中.

`apt` 命令的引入就是为了解决命令过于分散的问题,它包括了 `apt-get` 命令出现以来使用最广泛的功能选项,以及 `apt-cache` 和 `apt-config` 命令中很少用到的功能.
在使用 apt 命令时,用户不必再由 `apt-get` 转到 `apt-cache` 或 `apt-config`,而且 apt 更加结构化,并为用户提供了管理软件包所需的必要选项.

> 简单来说就是:`apt = apt-get`,`apt-cache` 和 `apt-config` 中最常用命令选项的集合.

***
`apt`

`install, remove, purge (apt-get(8))`
`apt list`(半成品)
`apt list`类似于`dpkg-query --list`, 它可以显示满足某些条件的软件包列表. 

它支持用`glob(7)`匹配软件包名称, 以及列出已安装(`--installed`), 可升级(`--upgradeable`)或所有可用(`--all-versions`)版本的选项. 

另外也可以用`whereis`

`whereis` - 找到命令的二进制文件, 源文件和 man 文件

***
`apt-get --install-suggests`

将建议的软件包视为安装的依赖项. 配置项:`APT::Install-Suggests`.

```bash
apt-get -f install pkg
```

### dpkg 应用管理

+ `ldd /bin/ls` : `ldd`查看依赖信息
+ `dpkg -i pkg`: 安装`pkg.deb`
+ `dpkg -r pkg`: 删除已安装的程序包
+ `dpkg -P pkg`: 彻底清除已安装的程序包
+ `dpkg -l, --list package-name-pattern...`: 列出与给定模式匹配的软件包. 
+ `dpkg -s, --status package-name...`: 报告指定软件包的状态. 
+ `dpkg -L, --listfiles package-name...`: 从软件包名称列出安装到系统的文件. 
+ `dpkg -S, --search filename-search-pattern...` 从已安装的软件包中搜索文件名. 
+ `dpkg -p, --print-avail package-name...` 显示有关软件包名称的详细信息, 存放在`/var/lib/dpkg/available`,基于`APT`的前端的用户使用`apt-cache`

### grep 过滤输出

+ `-n` 行号
+ `-v`,`--invert-match` 匹配不符合
+ `--color` 染色
+ `-P` perl 拓展
+ `-B` before 前输出
+ `-A` after 后输出
+ `-o` only 仅输出匹配字符
+ `-i` `--ignore-case` 忽略大小写

***
`-m NUM`, `--max-count=NUM` 输出的最大行: 在`NUM`行匹配的行之后停止读取文件. 

如果输入来自于普通文件, 就输出`NUM`行匹配的结果. 之后`grep`将标准输入定位到最后一个匹配行后面, 不再处理后面的内容. 
这使调用`grep`的程序可以继续搜索, 当`grep`停止之后, 它可以继续输出后面的文本. 
当同时使用`-c`或`--count`选项时, `grep`不会输出大于`NUM`的计数. 
当同时使用`-v`或`--invert-match`选项时, `grep`在输出`NUM`不匹配的行后停止. 

example:

```bash
grep -n --color -P -B 1 -A 6 "(?:tex:\d+:|warning:)" ./temp $tex_file".log"
```

`-e PATTERNS`, `--regexp=PATTERNS`: 使用`PATTERNS`作为模式.
此选项可以多次使用或与`-f`(`--file`)选项结合使用,搜索给定的所有模式. 此选项可用于保护以`-`开头的模式.

`f FILE`, `--file=FILE`:从`FILE`中获取模式,每行一个.
如果此选项多次使用或与`-e`(`--regexp`)选项结合使用,则搜索给定的所有模式. 空文件包含零个模式,因此不匹配.

### 图片格式转换

`pdf`转成图片格式. 包名: `pdftoppm`. 语法是：`pdftoppm input.pdf outputname -png -f {page} -singlefile`

```bash
pdftoppm  -png -rx 300 -ry 300  input.pdf outputname
```

这个命令将会把`PDF`的每一页转换成`png`格式, 文件名为`outputname-01.png`,`outputname-02.png`等等. 
如果只想转换其中的特定一页, 使用`-f {page}`选项指定. 例如`-f 1`表示第一页. 

`gnome`默认的查看图片程序为`eog`: eye of gnome 

### ubuntu 自带截图

`ubuntu` 自带截图程序叫做`gnome-serceenshot`

[Ubuntu设置截图到剪贴板,像QQ一样截图](https://www.jianshu.com/p/7f453c144f9c). 可以定义一个快捷键,保存到桌面文件

```bash
gnome-screenshot -a --file=(~"/Desktop/$(date +%s).png")
```

`date +%s`给出 UTC 时间

在 Ubuntu(18.04,16.04)或 Debian(Jessie 和更新版本)中安装 `GPaste`

对于 Debian,GPaste 可用于 Jessie 和更新版本,而对于 Ubuntu,GPaste 在 16.04 及更新版本的仓库中(因此可在 Ubuntu 18.04 Bionic Beaver 中使用).

你可以使用以下命令在 Debian 或 Ubuntu 中安装 GPaste(守护程序和 Gnome Shell 扩展):

```bash
sudo apt install gnome-shell-extensions-gpaste gpaste
```

安装完成后,按下 `Alt + F2` 并输入 `r` 重新启动 Gnome Shell,然后按回车键.现在应该启用了 GPaste Gnome Shell 扩展,其图标应显示在顶部 Gnome Shell 面板上.
如果没有,请使用 Gnome Tweaks(Gnome Tweak Tool)启用扩展.

Debian 和 Ubuntu 的 GPaste 3.28.0 中有一个错误,如果启用了图像支持选项会导致它崩溃,所以现在不要启用此功能.
这在 GPaste 3.28.2 中被标记为已修复,但 Debian 和 Ubuntu 仓库中尚未提供此包.

### 文档格式转换

用 `pandoc`

`pandoc [options] [input-file]...`

`-f --from -t --to`

`--latex-engine=pdflatex|lualatex|xelatex`

把markdown转换成`pdf`

```bash
pandoc -f markdown --latex-engine=xelatex -o output.pdf input.md
```

### ubunut 安装 typora

[typora for linux](https://www.typora.io/#linux)

```bash
# or run:
# sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys BA300B7755AFCFAE
wget -qO - https://typora.io/linux/public-key.asc | sudo apt-key add -
# add Typora's repository
sudo add-apt-repository 'deb https://typora.io/linux ./'
sudo apt-get update
# install typora
sudo apt-get install typora
```

### 挂载命令 mount

[linux挂载命令mount及U盘,移动硬盘的挂载](https://www.cnblogs.com/sunshine-cat/p/7922193.html)
[gpt格式的移动硬盘在Linux系统下挂载方法](https://blog.csdn.net/zhang_can/article/details/79714012)

+ `mount -l -t type` : `-l` 选项可以显示`label`
+ `findmnt [options] device|mountpoint`: 可以更清晰的显示文件系统
+ `umount [-dflnrv] {directory|device}` : 卸载文件系统,应该通过给出文件目录来使用,`-l, --lazy`Lazy  unmount

***
`fdisk` 查看磁盘列表

```bash
sudo fdisk -l
```

显示某个特定设备

```bash
sudo fdisk -l /dev/sdb
```

首先查看所有已经 mount 的设备:

```bash
mount [-l] [-t type]
```

显示如下信息

```bash
root@kali:~# fdisk -l
...
Device     Boot     Start       End   Sectors   Size Id Type
/dev/sda1  *  2048 209719295 209717248   100G  7 HPFS/NTFS/exFAT
/dev/sda2       209719296 976773119 767053824 365.8G  f W95 Ext'd (LBA)
/dev/sda5       209721344 465575935 255854592   122G  7 HPFS/NTFS/exFAT
/dev/sda6       465577984 721432575 255854592   122G  7 HPFS/NTFS/exFAT
/dev/sda7       721434624 976773119 255338496 121.8G  7 HPFS/NTFS/exFAT
...
```

`parted /dev/sdb print`  显示 sdb 的分区表

可以知道sdb2(135M to 6001G)为基本数据分区,格式为`NTFS`

mount 命令的标准格式:

```bash
mount -t type device dir
```

告诉 kernel attach the filesystem found on `device` (which is of type `type`) at the directory `dir`.  The option `-t type` is optional.

挂载到指定目录即可:

```bash
sudo mount -t ntfs /dev/sda1 /home/6T
```

The option `-l` adds labels to this listing.

***
弹出设备

```bash
umount /dev/sda5
```

通过`df`可以查看设备挂载点

### U盘格式化 exFAT

[将 USB 盘格式化为 exFAT](https://linux.cn/article-12294-1.html)

从 `Linux kernel 5.4`开始, `Linux` 内核本身中启用了 `exFAT` 文件系统支持. 
检查正在运行的 `Linux` 内核版本: `uname -r`. 如果是内核 `5.4` 或更高版本, 那么应该没问题. 
不然, 你必须启用`exFAT`支持. 在基于 `Ubuntu` 的发行版中, 你可以安装以下软件包：

```bash
sudo apt install exfat-fuse exfat-utils
```

***
方法 1：使用 `GNOME 磁盘工具`将磁盘格式化为 `exFAT`.

使用`GNOME 磁盘` 格式化驱动器是一项简单的工作. 它预装在许多 Linux 发行版中. 
插入外部 `USB` 盘. 在菜单中查找 `Disk`, 然后打开`GNOME 磁盘` 应用. 第一步, 选择要格式化的驱动器, 要使用 `exFAT`, 请选择 `其它`, 然后单击`下一步`. 

***
方法 2：在 `Linux` 命令行中将磁盘格式化为 `exFAT`.

插入外部硬盘, 然后在终端中输入以下命令`sudo fdisk -l`.通过列出的磁盘大小信息找出`USB`的标记, 假设`/dev/sdc1`.
如果磁盘有多个分区, 想要管理, 可以使用`sudo fdisk /dev/sdc`进行分区的管理工作. 这里要输入`/dev/sdc`也就是整个`U`盘, 而不是`/dev/sdc1`.
进入交互式分区工具:

`m`提示可用的命令列表.
`o`:创建新的`dos`分区表, `n`添加一个新的分区.       
`t`:更改分区类型, `dos` 类型的分区表, 如果要和`windows`格式化一致, 为`HPFS/NTFS/exFAT`, 就输入`7`.
`w`:保存更改, `q`退出不保存更改.

在这里更改分区表保存后, 磁盘已经可用了. 如果保存原来的分区直接格式, 可以使用`mkfs.exfat`:

+ `mkfs.exfat`:格式化成`exfat`.
+ `mkfs.fat`:格式化成`fat32`.

`man mkfs.exfat`会发现, `mkfs.exfat`的同义词`mkexfatfs`. 如果分区表是`MBR`类型的, 需要将文件系统类型设置为`0X07`(`NTFS/exFAT`), 否则其他操作系统可能会拒绝挂载. 
仍然假设U盘分区为`/dev/sdc1`, 使用以下命令将它格式化为 `exfat`. 

```bash
sudo mkfs.exfat -i 0x07 -n udisk /dev/sdc1
```

将`/dev/sdc1` 替换为你的磁盘 `ID`. `udisk` 是你要为磁盘命名的名称. 可选地, 运行 `fsck` 检查`sudo fsck.exfat /dev/sdc1`, 以确保格式化正确, 享受 `exFAT` 盘吧. 

### 查看文档首行末行

文档尾巴, `tail -n, --lines=[+]NUM`, 从第`num`行开始. 
文档开头, `head -n, --lines=[-]NUM`, 减去最后`num`行.

### 查看使用的桌面环境 x11 wayland

[如何找出你所使用的桌面环境 ](https://linux.cn/article-12124-1.html)

***
检查你使用的是哪个桌面环境

你可以在`Linux`中使用 `echo` 命令在终端中显示 `XDG_CURRENT_DESKTOP` 变量的值.

```bash
echo $XDG_CURRENT_DESKTOP
echo $XDG_SESSION_TYPE
```

或者使用`loginctl`得到`<SESSION_ID>`, 然后使用下列命令查看会话的类型:

```bash
loginctl show-session <SESSION_ID> -p Type
```

***
如何获取桌面环境版本

与获取桌面环境的名称不同.获取其版本号的方法并不直接,因为它没有标准的命令或环境变量可以提供此信息.
在 `Linux` 中获取桌面环境信息的一种方法是使用 `screenfetch`,`neofetch`之类的工具.
此命令行工具以 `ascii` 格式显示 `Linux` 发行版的 logo 以及一些基本的系统信息.桌面环境版本就是其中之一.
安装:`sudo apt install screenfetch`.

对于其他 Linux 发行版,请使用系统的软件包管理器来安装此程序.
安装后,只需在终端中输入 `screenfetch` 即可,它应该显示桌面环境版本以及其他系统信息.

### 查看linux 系统信息

ref: [3 Ways to Check Linux Kernel Version in Command Line](https://itsfoss.com/find-which-kernel-version-is-running-in-ubuntu/)

***
uname

`inxi -S`: 命令行系统信息脚本 for 终端和 IRC(InternetRelayChat)
`uname` -打印系统信息
`uname -r`命令的输出为:`5.4.0-48-generic`

这意味着您正在运行Linux内核`5.4.0-48`,或更笼统地说,您正在运行Linux内核版本`5.4`.

但是其他数字在这里意味着什么？

+ `5` – 内核版本
+ `4` – 重大修订
+ `0` – 次要修订
+ `48`– Bug fix
+ `generic`–特定于发行版的字符串.对于Ubuntu,这表示我使用的是`desktop`版本.
对于Ubuntu服务器版本,它将是`server`

使用`-a`选项可以输出更多信息
`uname -a`

命令的输出应如下所示:
`Linux OP7050 5.4.0-48-generic #52-Ubuntu SMP Thu Sep 10 10:58:49 UTC 2020 x86_64 x86_64 x86_64 GNU/Linux`

让我解释一下输出及其含义:

+ `Linux` –内核名称. 如果在BSD或macOS上运行相同的命令,结果将有所不同.
+ `OP7050` –主机名.
+ `5.4.0-48-generic` –内核版本(如我们在上面看到的).
+ `52-Ubuntu SMP Thu Sep 10 10:58:49 UTC 2020` –这意味着Ubuntu已编译`5.4.0-48-generic` `52`次. 最后一次编译的时间戳也在那里.
+ `x86_64` –机器架构.
+ `x86_64` –处理器架构.
+ `x86_64` –操作系统体系结构(您可以在64位处理器上运行32位OS).
+ `GNU / Linux` –操作系统(它不会显示发行版名称).

让我们看看其他一些命令来查找您的Linux内核版本.

***
使用`/proc/version file`

在Linux中,您还可以在文件`/proc/version`中找到内核信息. 只需查看此文件的内容即可:

`cat /proc/version`

在命令行中检查Linux内核版本, 您会看到类似于uname的输出.

`Linux version 5.4.0-48-generic (buildd@lcy01-amd64-010) (gcc version 9.3.0 (Ubuntu 9.3.0-10ubuntu2)) #52-Ubuntu SMP Thu Sep 10 10:58:49 UTC 2020`

您可以在此处看到内核版本`5.4.0-48-generic`.

***
使用`dmesg`

[Linux dmesg命令](https://www.runoob.com/linux/linux-comm-dmesg.html)

dmesg是用于编写内核消息的强大命令.这对于获取系统信息也非常有用.`dmesg`命令用于显示开机信息.
`kernel`会将开机信息存储在`ring buffer`中.您若是开机时来不及查看信息,可利用dmesg来查看.开机信息亦保存在`/var/log`目录中,名称为`dmesg`的文件里.

由于`dmesg`提供了很多信息,使用`grep`挑选.

```bash
dmesg | grep Linux
```

输出将包含几行,但是您应该能够在其中轻松识别Linux内核版本.

```bash
[    0.000000] Linux version 5.4.0-48-generic (buildd@lcy01-amd64-010) (gcc version 9.3.0 (Ubuntu 9.3.0-10ubuntu2)) #52-Ubuntu SMP Thu Sep 10 10:58:49 UTC 2020 (Ubuntu 5.4.0-48.52-generic 5.4.60)
...
[   12.936690] Intel(R) Wireless WiFi driver for Linux
```

### curl wget

curl -fsSL https://www.preining.info/rsa.asc | tlmgr key add -

`-f, --fail`
(HTTP)服务器错误时`静默失败`(没有输出).  这样做主要是为了, 使脚本等更好地处理失败的尝试. 
在正常情况下, 当`HTTP`服务器无法交付文档时, 它将返回`HTML文档`, 说明(通常还会描述原因及更多).  
该`flag`将阻止`curl`输出该错误并返回`error 22`. 

此方法不是`fail-safe`的, 并且有时会漏入不成功的响应代码, 尤其是在涉及验证时(response codes 401 and 407). 

`-s, --silent`

静音或安静模式.  不显示进度表或错误消息.  使`Curl`静音.  仍会输出您要求的数据, 甚至到终端/标准输出, 除非您将其重定向. 
除此选项外, 还可以使用`-S`, `--show-error`禁用进度表, 但仍显示错误消息. 
另请参见`-v`, `--verbose` and `--stderr`.       

`-S, --show-error`
与`-s`, `--silent`一起使用时, 如果`curl`失败, 它将使`curl`显示一条错误消息. 

`-L, --location`

(HTTP)如果服务器报告请求的页面已移动到其他位置(由`Location:  header`  and  a `3XX`响应代码), 此选项将使`curl`在新位置上重做请求. 
如果与`-i`, `--include`或`-l`,`--head`一起使用, 将显示所有请求页面的`headers`. 使用身份验证时, curl仅将其凭据发送到初始主机.
如果重定向将curl转移到其他主机, 它无法截获`user+password`. 
另请参阅`--location-trusted`查看如何修改这项设置. 
您可以使用`--max-redirs`选项来限制要遵循的重定向数量. 

当curl跟随重定向并且请求不是简单的`GET`(例如`POST`或`PUT`)时, it will do the following request with a GET:
如果`HTTP`响应是`301`, `302`或`303`. 如果响应代码是任何其他`3xx`代码, curl will resend the following request using the same unmodified method.

您可以通过使用专用的选项`--post301,` `--post302` and `--post303`, 来告诉curl 对于`30x`response, 不要将 `non-GET` request method 更改为`GET`.

### 查看ip地址

使用`ip`命令

```bash
ip addr show
ip link show #查看 MAC 地址
```

### 安装额外解码器

如果你刚刚安装了 Ubuntu 或其他 Ubuntu 特色版本 如 Kubuntu,Lubuntu 等,你会注意到系统无法播放某些音频或视频文件.

对于视频文件,你可以在 Ubuntu 上安装 `VLC`.`VLC` 是 Linux 上的最佳视频播放器之一,它几乎可以播放任何视频文件格式.但你仍然会遇到无法播放音频和 `flash` 的麻烦.

好消息是 Ubuntu 提供了一个软件包来安装所有基本的媒体编解码器:`ubuntu-restricted-extras`.

ubuntu-restricted-extras 是一个包含各种基本软件,如 `Flash` 插件,`unrar` ,`gstreamer`,`mp4`,`Ubuntu` 中的 `Chromium` 浏览器的编解码器等的软件包.

由于这些软件不是开源软件,并且其中一些涉及软件专利,因此 Ubuntu 默认情况下不会安装它们.你必须使用 `multiverse` 仓库,它是 Ubuntu 专门为用户提供非开源软件而创建的仓库.

由于 `ubuntu-restrcited-extras` 软件包在 `multiverse` 仓库中,因此你应验证系统上已启用 `multiverse` 仓库:

```bash
sudo add-apt-repository multiverse
```

然后你可以使用以下命令安装:

```bash
sudo apt install ubuntu-restricted-extras
```

[What are Ubuntu Repositories](https://itsfoss.com/ubuntu-repositories/)
[一条命令在 Ubuntu 中安装所有基本的媒体编解码器 ](https://linux.cn/article-11906-1.html)

### source 命令

[Ubuntu如何使用source命令执行文件](http://www.xitongzhijia.net/xtjc/20150714/52870.html)

`Ubuntu source` 命令的作用就是将设置在文件中的配置信息马上生效,而不需要经过重启.

Ubuntu如何使用`source`命令执行文件

source命令用法:
`source filename` 或 `. filename`

在对编译系统核心时常常需要输入一长串的命令,如:

```bash
make mrproper
make menuconfig
make dep
make clean
make bzImage
......
```

如果把这些命令做成一个文件,让它自动顺序执行,对于需要多次反复编译系统核心的用户来说会很方便,
而用source命令就可以做到这一点,
它的作用就是把一个文件的内容当成shell来执行,先在linux的源代码目录下(如`/usr/src/linux-2.4.20`)建立一个文件,如`make_command`,在其中输入一下内容:

```bash
make mrproper &&
make menuconfig &&
make dep &&
make clean &&
...
```

文件建立好之后,每次编译核心的时候,只需要在`/usr/src/linux-2.4.20`下输入:`source make_command`即可

顺便补充一点,`&&`命令表示顺序执行由它连接的命令,但是只有它之前的命令成功执行完成了之后才可以继续执行它后面的命令.

另外执行source命令时如果提示command not found,是因为环境变量没配置好的原因,在终端运行如下命令即可修复:

`export PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin:/root/bin`

### 查看磁盘空间

`df`命令是linux系统以磁盘分区为单位查看文件系统,可以加上参数查看磁盘剩余空间信息,命令格式:

`df - report file system disk space usage`

***
SYNOPSIS

`df [OPTION]... [FILE]...`

+ `-a`, `--all` include pseudo, duplicate, inaccessible file systems
+ `-l`, `--local` limit listing to local file systems
+ `-h`, `--human-readable` print sizes in powers of 1024 (e.g., 1023M)
+ `-T`, `--print-type` print file system type

### find 过滤文件

删除日志文件

```bash
sudo /dev/null > /var/log/**.log
```

下面这个推荐使用,删除30天之前的旧文件

```bash
sudo find /var/log/ -type f -mtime +30 -exec rm -f {} \;
```

***
`find` - search for files in a directory hierarchy

`find [-H] [-L] [-P] [-D debugopts] [-Olevel] [starting-point...] [expression]`

***
`expression`

`starting points`列表之后的部分是`表达式`.  这是一种查询规范, 描述了我们如何匹配文件以及如何处理匹配的文件. 
表达式由一系列事物组成:`Test`, `Actions`,...

***
`-exec command ;`

执行命令； 如果返回`0`状态, 则为`true`.  之后传递给`find`的参数都将作为命令的参数, 直到遇到`;`为止. 
字符` {}`被替换为当前文件名命令参数中出现的任何地方的当前文件名, 而不仅仅是在单独存在的参数中. 
这两种构造都可能需要转义(以`\`表示)或加引号以保护它们, 避免 shell 展开. 
有关使用`-exec`选项的示例, 请参见示例部分. 对每个匹配的文件运行一次指定的命令.  
该命令在起始目录中执行.  与`-exec`有关的操作具有不可避免的安全问题； 你应该使用`-execdir`选项代替. 

***
`-exec command {} +`

在选定的文件上运行指定的命令, `-exec` action的变体. 但是通过在命令结尾附加上每个选中的文件名; 
该命令的调用总数将远远少于匹配文件的数目.  命令行的构建方式与`xargs`几乎相同.
命令中仅允许使用一个`{}`实例, 并且当从`shell`调用`find`时,应该用引号保护起来, 例如`'{}'`, 以防止其被`shell`解释.  
该命令在起始目录中执行.  如果有任何调用返回一个非零值作为退出状态, 则`find`返回一个非零退出状态.  
如果`find`遇到错误, 有时可能会导致立即退出, 因此一些待处理的命令可能根本不会运行. This variant of `-exec` always returns `true`.

***
`-mtime n`
文件数据的最后修改时间为 `n*24` 小时. 请参阅`-atime`的注释, 以了解舍入如何影响文件修改时间的解释

`-type c`
File is of type c:

+ `b`  block (buffered) special
+ `c`  character (unbuffered) special
+ `d`  directory
+ `p`  named pipe (FIFO)
+ `f`  regular file

### 查看文件大小

[Ubuntu下查看文件, 文件夹和磁盘空间的大小](https://blog.csdn.net/BigData_Mining/java/article/details/88998472)

在实际使用`ubuntu`时候,经常要碰到需要查看文件以及文件夹大小的情况.
有时候,自己创建压缩文件,可以使用 `ls -hl`查看文件大小.参数`-h` 表示`Human-Readable`,使用`GB`,`MB`等易读的格式方式显示.对于文件夹的大小,`ll -h` 显示只有`4k`.

***
那么如何来查看文件夹的大小呢？

使用`du`命令查看文件或文件夹的磁盘使用空间,`–max-depth` 用于指定深入目录的层数.

如要查看当前目录已经使用总大小及当前目录下一级文件或文件夹各自使用的总空间大小,
输入`du -h --max-depth=1`即可.

如要查看当前目录已使用总大小可输入:`du -h --max-depth=0`

***

```bash
du [OPTION]... [FILE]...
du [OPTION]... --files0-from=F
```

+ `-s,` `--summarize`: 对每个参数仅显示总计
+ ` -h`, `--human-readable`: 以人类可读的格式显示大小(例如`1K` `234M` `2G`)
+ `-d,` `--max-depth=N`: 指定目录递归的层数； `--max-depth = 0`与`--summaryize`相同
+ `--si`   like `-h`, 但是使用`1000`的幂而不是`1024`的幂
+ `-a,` `--all` :给出所有文件的统计, 而不仅仅是目录

### 创建链接

`ln` — 创建链接

`ln` 命令即可创建硬链接,也可以创建符号链接.

可以用其中一种方法来使用它:

+ `ln file link` 创建硬链接
+ `ln -s item link` 创建符号链接,`item` 可以是一个文件或是一个目录.

#### 硬链接

硬链接和符号链接比起来,硬链接是最初 `Unix` 创建链接的方式,而符号链接更加现代. 在默认情况下,每个文
件有一个硬链接,这个硬链接给文件起名字.
当我们创建一个 硬链接以后,就为文件创建了一个额外的目录条目.硬链接有两个重要局限性:

1. 一个硬链接不能关联它所在文件系统之外的文件.这是说一个链接不能关联 与链接本身不在同一个磁盘分区
上的文件.
2. 一个硬链接不能关联一个目录.

一个硬链接和文件本身没有什么区别.不像符号链接,当你列出一个包含硬链接的目录 内容时,你会看到没有特
殊的链接指示说明.当一个硬链接被删除时,这个链接 被删除,但是文件本身的内容仍然存在(这是说,它所占
的磁盘空间不会被重新分配), 直到所有关联这个文件的链接都删除掉.知道硬链接很重要,因为你可能有时
会遇到它们,但现在实际中更喜欢使用符号链接,下一步我们会讨论符号链接.

#### 符号链接

创建符号链接是为了克服硬链接的局限性.
符号链接生效,是通过创建一个特殊类型的文件,这个文件包含一个关联文件或目录的文本指针.
在这一方面, 它们和 Windows 的快捷方式差不多,当然,符号链接早于Windows 的快捷方式 很多年;-)

一个符号链接指向一个文件,而且这个符号链接本身与其它的符号链接几乎没有区别.
例如,如果你往一个符号链接里面写入东西,那么相关联的文件也被写入.

然而, 当你删除一个符号链接时,只有这个链接被删除,而不是文件自身.
如果先于符号链接删除文件,这个链接仍然存在,但是不指向任何东西.
在这种情况下,这个链接被称为坏链接.
在许多实现中,`ls` 命令会以不同的颜色展示坏链接,比如说红色,来显示它们的存在.

***
创建硬链接

现在,我们试着创建链接.首先是硬链接.我们创建一些关联我们 数据文件的链接:

```bash
[me@linuxbox playground]$ ln fun fun-hard
[me@linuxbox playground]$ ln fun dir1/fun-hard
[me@linuxbox playground]$ ln fun dir2/fun-hard
```

`ls` 命令有一种方法,来展示(文件索引节点)的信息.在命令中加上``-i``选项:

***
创建符号链接

建立符号链接的目的是为了克服硬链接的两个缺点:硬链接不能跨越物理设备, 硬链接不能关联目录,只能是文
件.符号链接是文件的特殊类型,它包含一个指向 目标文件或目录的文本指针.

符号链接的建立过程相似于创建硬链接:

```bash
[me@linuxbox playground]$ ln -s fun fun-sym
[me@linuxbox playground]$ ln -s ../fun dir1/fun-sym
[me@linuxbox playground]$ ln -s ../fun dir2/fun-sym
```

第一个实例相当直接,在 ln 命令中,简单地加上`-s`选项就可以创建一个符号链接, 而不是一个硬链接.

`fun-sym` 的列表说明了它是一个符号链接,通过在第一字段中的首字符`l` 可知,并且它还指向`../fun`,也是正确的.

当建立符号链接时,你即可以使用绝对路径名:

```bash
ln -s /home/me/playground/fun dir1/fun-sym
```

也可用相对路径名,正如前面例题所展示的.使用相对路径名更令人满意, 因为它允许一个包含符号链接的目录重命名或移动,而不会破坏链接.

### basename

***
截取文件名和后缀

编写Shell脚本的过程中,经常会和文件名和文件路径打交道.
如果用户输入了一个文件的全名(可能包含绝对路径和文件后缀),如何得到文件的路径名,文件名,文件后缀这些信息呢.
Shell脚本拥有强大的字符串处理能力,如果把文件名当做字符串,我们不难使用`cut`或`sed`这样的工具得到我们想要的结果.

```bash
$fullfile=/the/path/foo.txt
$fullname=$(basename $fullfile)
$dir=$(dirname $fullfile)
$filename=$(echo $fullname | cut -d . -f1)
$extension=$(echo $fullname | cut -d . -f2)
$ echo $dir , $fullname , $filename , $extension
/the/path , foo.txt , foo , txt
```

这里使用`basename`命令可以直接得到包含后缀的文件名,而`dirname`命令可以得到路径名,
然后就能简单的用`cut`截取文件名和后缀名.

***
更复杂的情况

如果对付简单应用场景,到这里已经可以打完收工了,但是有时候文件可能不止有一个后缀,比如`*.tar.gz`,怎样得到最后一个后缀呢？
再`cut`一回？当然可以,但是如果文件名是`mylib.1.0.1a.zip`这样的呢？呃......正则表达式肯定可以.

```bash
$ fullname=mylib.1.0.1a.zip
$ filename=$(echo $fullname | sed 's/\.[^.]*$//')
$ extension=$(echo $fullname | sed 's/^.*\.//')
$ echo $filename, $extension
mylib.1.0.1a, zip
```

这里面的逻辑是这样的:

文件名:把以`.`字符开头以后一直到行尾都是非`.`字符的子串替换为空.
后缀名:把从行首开始以`.`字符结尾的子串替换为空.

光用语言把这两个正则表达式描述出来脑细胞也要死不少.有没有像上面`cut`版本一样简单容易理解的方法呢？
由于`.`分隔符的个数不确定,正常使用`cut`来分割最后一个`.`字符是不太可能的.
但是我们可使用`rev`命令将字符串反转一下,区分后缀和文件名的`.`字符位置就确定了.
截取了想要的部分之后,再次反转就得到了我们想要的内容.

```bash
$ fullname=mylib.1.0.1a.zip
$ filename=$(rev <<< $fullname | cut -d . -f2- | rev)
$ extension=$(rev <<< $fullname | cut -d . -f1 | rev)
$ echo $filename, $extension
mylib.1.0.1a, zip
```

***
使用参数扩展

其实不借助复杂的正则表达式,甚至不调用`basename`, `dirname`, `cut`, `sed`命令,`shel`l脚本一样可以做到所有的操作.
看下面的实现:

```bash
$ fullfile=/the/path/mylib.1.0.1a.zip
$ fullname="${fullfile##*/}"
$ dir="${fullfile%/*}"
$ extension="${fullname##*.}"
$ filename="${fullname%.*}"
$ echo $dir , $fullname , $filename , $extension
/the/path , mylib.1.0.1a.zip , mylib.1.0.1a , zip
```

真是不能再简洁了,大括号之内变量名配合几个神奇的字符,就是Shell的参数扩展(Parameter Extension)功能.

+ `${fullfile##*/}`:从前面开始删除`fullfile`中最大匹配(longest matching pattern) `*/` 的字符串
+ `${fullfile%/*}`:从后面开始删除`fullfile`中最小匹配(shortest matching pattern) `/*` 的字符串
+ `${fullname##*.}`:从前面开始删除`fullname`中最大匹配(longest matching pattern) `*.` 的字符串
+ `${fullname%.*}`:从后面开始删除`fullname`中最小匹配(shortest matching pattern) `.*` 的字符串

参数扩展有多种形式,在shell编程中可以用作参数的拼接,字符串的替换,参数列表截取,变量初值等操作,
这里不再详述,请参考后面的功能列表和官方文档

***
使用`basename`命令输出所有`*.tex`的名字

```bash
basename -s '.tex' $(ls *.tex) | xargs echo
```

### inode

[Linux的inode的理解](https://www.cnblogs.com/itech/archive/2012/05/15/2502284.html)

***
inode是什么

理解`inode`,要从文件储存说起.
文件储存在硬盘上,硬盘的最小存储单位叫做`扇区`(Sector).每个扇区储存`512`Byte(相当于`0.5KB`).
操作系统读取硬盘的时候,不会一个个扇区地读取,这样效率太低,而是一次性连续读取多个扇区,即一次性读取一个"块"(`block`).
这种由多个扇区组成的`块`,是文件存取的最小单位.`块`的大小,最常见的是`4KB`,即连续八个 `sector`组成一个 `block`.

文件数据都储存在`块`中,那么很显然,我们还必须找到一个地方储存文件的元信息,比如文件的创建者, 文件的创建日期, 文件的大小等等.
这种储存文件元信息的区域就叫做`inode`,中文译名为`索引节点`, `index node`.

***
inode的内容

`inode`包含文件的元信息,具体来说有以下内容:

+ 文件的字节数
+ 文件拥有者的User ID
+ 文件的Group ID
+ 文件的读, 写, 执行权限
+ 文件的时间戳,共有三个:`ctime` 指 `inode` 上一次变动的时间, `mtime` 指文件内容上一次变动的时间, `atime` 指文件上一次打开的时间.
+ 链接数,即有多少文件名指向这个 `inode`
+ 文件数据`block`的位置

可以用`stat`命令,查看某个文件的`inode`信息:

```bash
stat example.txt
```

总之,除了文件名以外的所有文件信息,都存在`inode`之中.至于为什么没有文件名,下文会有详细解释.

***
inode的大小

`inode`也会消耗硬盘空间,所以硬盘格式化的时候,操作系统自动将硬盘分成两个区域.一个是数据区,存放文件数据；另一个是`inode`区(`inode table`),存放`inode`所包含的信息.
每个`inode`节点的大小,一般是`128`字节或`256`字节.
`inode`节点的总数,在格式化时就给定,一般是每1KB或每2KB就设置一个`inode`.
假定在一块1GB的硬盘中,每个`inode`节点的大小为128字节,每`1KB`就设置一个`inode`,那么`inode table`的大小就会达到`128MB`,占整块硬盘的`12.8%`.

查看每个硬盘分区的`inode`总数和已经使用的数量,可以使用`df`命令.

```bash
df -i
```

查看每个`inode`节点的大小,可以用如下命令:

```bash
sudo dumpe2fs -h /dev/hda | grep "Inode size"
```

由于每个文件都必须有一个`inode`,因此有可能发生`inode`已经用光,但是硬盘还未存满的情况.这时,就无法在硬盘上创建新文件.

***
inode号码

每个`inode`都有一个号码,操作系统用`inode`号码来识别不同的文件.

这里值得重复一遍,`Unix/Linux`系统内部不使用文件名,而使用`inode`号码来识别文件.对于系统来说,文件名只是`inode`号码便于识别的别称或者绰号.
表面上,用户通过文件名,打开文件.实际上,系统内部这个过程分成三步:首先,系统找到这个文件名对应的`inode`号码；其次,通过`inode`号码,获取`inode`信息；
最后,根据`inode`信息,找到文件数据所在的`block`,读出数据.

使用`ls -i`命令,可以看到文件名对应的`inode`号码:

```bash
ls -i example.txt
```

***
目录文件

`Unix/Linux`系统中,目录(directory)也是一种文件.打开目录,实际上就是打开目录文件.

目录文件的结构非常简单,就是一系列目录项(`dirent`)的列表.每个目录项,由两部分组成: 所包含文件的文件名,以及该文件名对应的 `inode` 号码.

`ls`命令只列出目录文件中的所有文件名:

```bash
ls /etc
```

ls -i命令列出整个目录文件,即文件名和`inode`号码:

```bash
ls -i /etc
```

如果要查看文件的详细信息,就必须根据`inode`号码,访问`inode`节点,读取信息.`ls -l`命令列出文件的详细信息.

```bash
ls -l /etc
```

***
硬链接

一般情况下,文件名和`inode`号码是"一一对应"关系,每个`inode`号码对应一个文件名.但是,Unix/Linux系统允许,多个文件名指向同一个`inode`号码.
这意味着,可以用不同的文件名访问同样的内容；对文件内容进行修改,会影响到所有文件名；但是,删除一个文件名,不影响另一个文件名的访问.这种情况就被称为"硬链接"(`hard link`).

`ln`命令可以创建硬链接:

```bash
ln 源文件 目标文件
```

运行上面这条命令以后,源文件与目标文件的inode号码相同,都指向同一个`inode`.`inode`信息中有一项叫做"链接数",记录指向该`inode`的文件名总数,这时就会增加1.反过来,删除一个文件名,就会使得`inode`节点中的"链接数"减1.当这个值减到0,表明没有文件名指向这个`inode`,系统就会回收这个`inode`号码,以及其所对应`block`区域.

这里顺便说一下目录文件的"链接数".创建目录时,默认会生成两个目录项:"."和"..".前者的`inode`号码就是当前目录的`inode`号码,等同于当前目录的"硬链接"；后者的`inode`号码就是当前目录的父目录的`inode`号码,等同于父目录的"硬链接".所以,任何一个目录的"硬链接"总数,总是等于`2`加上它的子目录总数(含隐藏目录),这里的`2`是父目录对其的"硬链接"和当前目录下的".硬链接".

***
软链接

除了硬链接以外,还有一种特殊情况.文件`A`和文件`B`的`inode`号码虽然不一样,但是文件`A`的内容是文件`B`的路径.
读取文件`A`时,系统会自动将访问者导向文件`B`.因此,无论打开哪一个文件,最终读取的都是文件`B`.这时,文件`A`就称为文件`B`的"软链接"(soft link)或者"符号链接(symbolic link).

这意味着,文件`A`依赖于文件`B`而存在,如果删除了文件`B`,打开文件`A`就会报错:"No such file or directory".
这是软链接与硬链接最大的不同:文件`A`指向文件`B`的文件名,而不是文件`B`的`inode`号码,文件`B`的`inode`"链接数"不会因此发生变化.

`ln -s`命令可以创建软链接.

```bash
ln -s 源文文件或目录 目标文件或目录
```

***
inode的特殊作用

由于`inode`号码与文件名分离,这种机制导致了一些`Unix/Linux`系统特有的现象.

+ 有时,文件名包含特殊字符,无法正常删除. 这时,直接删除`inode`节点,就能起到删除文件的作用.
+ 移动文件或重命名文件,只是改变文件名,不影响`inode`号码.
+ 打开一个文件以后,系统就以`inode`号码来识别这个文件,不再考虑文件名.因此,通常来说,系统无法从`inode`号码得知文件名.

第`3`点使得软件更新变得简单,可以在不关闭软件的情况下进行更新,不需要重启.
因为系统通过`inode`号码,识别运行中的文件,不通过文件名.更新的时候,新版文件以同样的文件名,生成一个新的`inode`,不会影响到运行中的文件.
等到下一次运行这个软件的时候,文件名就自动指向新版文件,旧版文件的`inode`则被回收.

### shebang 脚本开头

[Shebang](https://bash.cyberciti.biz/guide/Shebang)

大多数`Linux shell`和`perl`/`python`脚本以以下行开头：

```bash
#!/bin/bash
#!/usr/bin/perl
#!/usr/bin/python
#!/usr/bin/python3
#!/usr/bin/env bash
```

这称为`shebang`或`bang`行. 

`shebang`(意思为这一切)其实就是`Bash`解释器的绝对路径. 几乎所有的`bash`脚本通常都以`#!/bin/bash`开头(假设`Bash`已安装在`/bin`中).
这样可以确保即使在另一个`shell`下执行脚本, 也可以使用`Bash`来解释该脚本. 
`Shebang`是由Dennis Ritchie在第7版和8版`Unix`之间在`Bell`实验室推出的.  然后, 它也被添加到Berkeley的`BSD`中. 

`/usr/bin/env`在修改后的环境中运行`bash`之类的程序.  它使您的`bash`脚本具有可移植性.  
`#!/usr/bin/env bash`的优点是, 它将使用运行用户的`$PATH`变量中最先出现的`bash`可执行文件. 

### 日志文件

[linux系统日志在哪？](https://www.php.cn/linux-435716.html)
[linux日志介绍](https://zhuanlan.zhihu.com/p/26428150)

```bash
sudo tail -f /var/log/messages
# or in ubuantu
sudo tail -f /var/log/kern.log
```

`linux`日志大多是以明文存储, 一般存储在`/var/log`目录中, linux系统中主要有三个日志子系统：连接时间日志, 进程统计日志, 错误日志. 

+ `assess-log` 记录和`HTTP/web`的传输
+ `secure` 记录登录系统存取资料的消息
+ `btmp` 记录失败的消息
+ `lastlog` 记录最近几次成功登录的事件和最后一次不成功的登录
+ `messages`：包括整体系统信息, 其中也包含系统启动期间的日志. 此外, 还包括`mail`, `cron`, `daemon`, `kern`和`auth`等内容
+ `sudolog` 记录`sudo`发出的命令
+ `sulog` 记录使用`su`命令的使用
+ `utmp` 记录当前登录的每个用户
+ `wtmp` 一个用户每次登录进入和退出的的永久记录
+ `syslog`：它和`/etc/log/messages`日志文件不同, 它只记录警告信息, 常常是系统出问题的信息. 
+ `xferlog` 记录了FTP会话
+ `user.log`：记录所有等级用户信息的日志. 
+ `auth.log`：包含系统授权信息, 包括用户登录和使用的权限机制等. 
+ `daemon.log`：包含各种系统后台守护进程日志信息. 
+ `kern.log`：包含内核产生的日志, 有助于在定制内核时解决问题. 

连接时间日志是有多个程序执行的, 把日志记录到`/var/log/wtmp`, `/var/run/utmp`,`/var/log/lastlog` 三个文件中, 这三个文件记录了用户登录系统和退出的有关信息, 
`utmp`保存了当前用户的每个用户的信息, `wtmp`记录了每个用户登录注销和系统的启动, 关机的事件, `lastlog`记录了每个用户最后登录的信息记录. 

`wtmp`和`utmp`文件都是二进制, 不能使用`cat`和`tail`命令查看, 但是可以使用`who`, `w`,`users`,`last` 等命令查看着两个文件的信息

***
`who [参数]`

+ `-a` 显示全部信息
+ `-m` 只显示当前终端的登录用户信息
+ `-q` 只显示当前登录到系统中的用户名称和数量, 和其他参数共同使用的时候, 其他参数将被忽略

***
`Systemd`统一管理所有 `Unit` 的启动日志. 带来的好处就是可以只用`journalctl`一个命令, 查看所有日志(内核日志和 应用日志). 

语法格式： `journalctl [参数]`

+ `-k,--dmesg`: 查看内核日志
+ `-b`:  查看系统本次启动的日志
+ `-u`:  查看指定服务的日志
+ `-n`:  指定日志条数
+ `-f`:  追踪日志
+ `--disk-usage`:  查看当前日志占用磁盘的空间的总大小

`-S, -since =, -U, -until =`
显示比指定日期更早或更晚的日志. 日期格式为`"2012-10-30 18:17:16"`.如果省略时间部分, 则假定为`00:00:00`.如果只有秒部分是省略, 则假定为`:00`. 
如果省略日期部分, 则假定为当前日期.  也可以使用字符串`yesterday`, `"today"`, `"tomorrow"`.`now`是指当前时间. 
是指当日, 当日或当日后一天的00:00:00. 最后, 可以用`-`或`+`为前缀指定相对时间. 有关完整的时间和日期规范, 请参见`systemd.time(7).`. 
`--output=short-full`将完全按照此格式打印时间戳. 

`x, --catalog`:
用信息目录中的解释文本来增加日志行. 这将为输出中的日志信息添加解释性的帮助文本, 如果这是可用的. 
这些简短的帮助文本将解释错误或日志事件的背景, 可能的解决方案, 以及指向支持论坛, 开发人员文档和任何其他相关手册的指针. 
请注意, 帮助文本不是对所有的信息都可用, 而只是对选定的 的帮助文本. 关于消息目录的更多信息, 请参考消息目录开发者文档[5]. 

注意：当把 `journalctl` 输出附加到错误报告时, 请不要使用 `-x`. 

`-e, --pager-end`:

立即跳到默认的分页工具的日志末尾. 这意味着`-n1000`, 以保证`pager`不会缓冲无限制大小的日志. 
可以用一个明确的`-n数值`来覆盖, 而`-nall`将禁用这个上限. 注意, 这个选项只支持`less(1)`pager. 

参考实例

+ 查看所有日志： `journalctl` 
+ 查看httpd的日志： `journalctl -u httpd`
+ 查看最近发生的20条日志： `journalctl -n 20`
+ 追踪日志： `journalctl -f`

### Linux 安装时的分区 

[UEFI/GPT 示例](https://wiki.archlinux.org/title/Parted_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#UEFI/GPT_%E7%A4%BA%E4%BE%8B)
[manjaro_user_guide 也有分区的例子](https://manjaro.org/support/userguide/)
[Arch boot process](https://wiki.archlinux.org/title/Arch_boot_process_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#%E5%90%AF%E5%8A%A8%E5%8A%A0%E8%BD%BD%E5%99%A8)

以`UEFI`主板为例, 参考`manjaro_user_guide`. 主要设置三个分区:

文件系统格式, 挂载点, 建议大小, `flags`

+ `fat32`,`/boot/efi`,`512MiB`, `boot,esp`, `esp`即 `UEFI System Partition`, GPT 中它是`boot`的别名.
+ `linuxswap`,不需要挂载, $\sqrt{\text{内存大小}} \sim 2* \text{内存大小}$
+ `ext4`,`/`,最简单的, 可以把剩下空间全部分配给`/`,文件系统也可以选别的, 比如`Btrfs`

强迫症可以把分区调整成`EFI`,`root`,`swap`的顺序, 可能有玄学加成. 

### 交换分区 swap

[ubnuntu SwapFaq](https://help.ubuntu.com/community/SwapFaq)

**
如果想要确认是否有`swap`分区, 使用`parted`查看所有分区

```bash
sudo parted --list
```

如果有`swap`, 在输出中可以看到类似下面的内容

    5      236GB   256GB   20.0GB  linux-swap(v1)

***
增加交换分区大小并将其用于休眠

+ 创建交换分区
+ 激活交换分区
+ 使新的交换分区适用于休眠状态(可选)

***
创建交换分区

使用`Ubuntu`安装介质如U盘开机, 选择`立即运行Ubuntu`.
打开`GParted`分区编辑器：删除原先的`swap`, `resize`主分区大小, 留出合适的空白空间用作`swap`.可以直接在`"free space following"`一项中设置想要的`swap`分区大小.
在新的空白空间中, 选择`new`, 输入`linux-swap`, 如果喜欢,可以起个名`swap`到`Partition name`中, 然后点击`Apply`应用.  完成后, 重新启动原先硬盘上的`Ubuntu`系统. 

***
激活交换分区

如果交换位于主硬盘驱动器上, 则无需在此处做任何事情. 
现在, 您需要查找`swap`所在的`分区`及其`UUID`, Universally Unique IDentifier, 它是该分区的通用唯一身份, 即使由于添加磁盘, 重启之后分区挂载点改变, `UUID`也不会改变. 

切换到`root`用户, 运行`gparted` . 右键单击交换分区, 然后选择`Information`. 
应该可以看到`Path`和`UUID`. 保持打开状态以供进一步参考. 
运行 `gedit /etc/fstab`, 查找其中带有`swap`的行. 它应该在第三列, 用空格或制表符分隔. 您可以使用路径或`UUID`来告诉`Linux`在哪里找到交换分区. 建议使用`UUID`, 因为即使您移动分区或磁盘之后, `sdb`变成`sda`, `UUID`也将保持不变. 
如果您使用的是UUID, 则您的代码行应如下所示：

    UUID=41e86209-3802-424b-9a9d-d7683142dab7 none swap sw 0 0

或使用路径：

    /dev/sda2 none swap sw 0 0 

保存更改. 

使用此命令启用新的交换分区. 

```bash
sudo swapon --all # 或者 sudo swapon --all --verbose
```

确认交换分区存在. 

```bash
$ cat /proc/swaps
Filename                                Type            Size  Used    Priority
/dev/sda2                               partition      20971480       -1
```

接着可以重启查看交换分区能否被正确激活. 

***
使交换分区用于休眠(可选)

运行`cat /proc/swaps`, 可以看到`swap`分区的路径. 找到它的`UUID`. 使用

    sudo gedit /etc/default/grub

修改`grub`(启动引导加载程序)的配置. 如果怕出问题可以先备份

    sudo cp -a /etc/default/grub /etc/default/grub.bak 

查找`GRUB_CMDLINE_LINUX="xx"`行, 在后面添加上:

    GRUB_CMDLINE_LINUX="XXX resume=UUID=41e86209-3802-424b-9a9d-d7683142dab7"

并保存文件.`sudo update-grub`并等待其完成.

然后`sudo gedit /etc/initramfs-tools/conf.d/resume`, 确保内容类似下面这样：

    resume=UUID=41e86209-3802-424b-9a9d-d7683142dab7 
    
如果不使用`swap`, 这里的设置应该是`resume=none`,保存文件.
 然后运行`sudo update-initramfs -u`. 重启. 现在应该可以休眠了.

***
启用未生效的交换分区：如果你已经有交换分区, 则有几种启用它的方法. 首先查看`fstab`

    cat /etc/fstab

确保下面有`swap`的记录, 这样可以在启动时启用`swap`. 

    /dev/sdb5       none            swap    sw              0       0

然后禁用所有`swap`, 再重新创建它, 然后使用以下命令重新启用

```bash
sudo swapoff -a
sudo /sbin/mkswap /dev/sdb5
sudo swapon -a
```

***
什么是`swappiness`, 我该如何更改？

`swappiness`参数控制内核使用`swap`的倾向. 因为磁盘要比`RAM`慢得多, 所以如果进程经常主动地移出内存, 可能导致系统和应用程序的响应时间变慢. 

+ `swappiness` 的值可以在`0`到`100`之间
+ `swappiness=0`告诉内核尽可能避免将进程从物理内存中交换出来
+ `swappiness=100` 告诉内核积极地把进程从物理内存移动到`swap`缓存

`Ubuntu` 中的默认设置为`swappiness=60`. 降低 `swappiness` 的默认值可能会提高典型的`Ubuntu`桌面安装的整体性能. 
建议将 `swappiness` 的值设置为`10`, 你也可以自己尝试. `Ubuntu`服务器对桌面系统的性能要求不同, 默认值`60`可能更合适. 

检查`swappiness`值

    cat /proc/sys/vm/swappiness

临时更改 `swappiness` 值(重启之后丢失):

    sudo sysctl vm.swappiness=10

永久更改：

    sudo gedit /etc/sysctl.conf

搜索`vm.swappiness`根据需要更改其值. 如果`vm.swappiness`不存在则自己添加：

    vm.swappiness=10

保存文件并重新启动. 

### 编码,字符集

[File name is garbled](https://wiki.archlinux.org/title/Localization/Simplified_Chinese)

避免乱码基本原则：使用 `utf-8` 代替 `gbk/gb2312`.  

`convmv`: 将文件名从一种编码转换到另一种编码, 例如:

```bash
convmv -f GBK -t UTF-8 --notest --nosmart file
```

`-f`指定原始编码, `-t`指定输出编码. 使用 `convmv --list` 可查询所有支持的编码.  
`--notest` 表示进行实际操作, 而非测试, 如果不使用该参数只会打印出转换结果而不会实际转码. `--smart`表示如果已经是`UTF-8` 则忽略. 

#### 文件内容乱码

使用 `iconv` 命令转换格式. 示例：

```bash
iconv -f GBK -t UTF-8 -o new-file origin-file
```

`-f` 指定原始编码, `-t` 指定输出编码. 使用 `iconv -l` 可查询所有支持的编码. `-o` 指定输出文件. 

#### zip 压缩包乱码

避免方法：非 `utf8` 编码环境下(一般 `windows` 下的中文环境即是)不使用 `zip` 进行压缩(建议使用 `7z`). 
解决方案：安装使用 `unzip-iconv` 或者 `unzip-natspec`取代原版的 `unzip` 来解压缩, 示例：

```bash
unzip -O gbk file.zip
```

`file.zip` 是压缩文件, `gbk` 是该文件的编码格式, 以 `-O` 指定(原版 `unzip` 无 `-O` 选项). 

#### MP3 文件标签乱码

对于用 `GStreamer` 做后端的播放器, 如 `Rhythmbox``, totem`, 设置如下的环境变量后即可正确读取 `mp3` 中 `GBK` 编码的 `ID3 tag`：

```bash
export GST_ID3_TAG_ENCODING=GBK:UTF-8:GB18030
export GST_ID3V2_TAG_ENCODING=GBK:UTF-8:GB18030
```

### gnome 蓝牙传送文件

`bluetooth-sendto`; 用于通过蓝牙传输文件的GTK应用程序

+ 说明; `bluetooth-sendto [--device=XX:XX:XX:XX:XX:XX [--name=NAME]] [file...]`
+ 描述; `bluetooth-sendto` 将显示一个通过蓝牙传输文件的对话框. `bluetooth-sendto`是`gnome-bluetooth`的一部分, 参见[GnomeBluetooth](http://live.gnome.org/GnomeBluetooth)
+ 选项; 
  + `--device`;  定义要发送文件的设备地址.  如果省略, 将显示一个选择器. 
  + `--name`; 定义要发送文件的设备名称.  如果省略, 将被自动检测. 
  + `file` 要发送到设备的文件.  如果省略, 将显示一个选择器. 
