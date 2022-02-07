# linux-1-daily

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

## 命令类型

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

## 基本命令

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
ANSI "颜色"转义序列是以下形式的序列: `ESC [ ... m`. 其中`...`是`0`个或更多的颜色规范字符. 为了保持屏幕外观, `ANSI`颜色转义序列被认为不会移动光标.
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

[ubuntu: 查看ubuntu系统的版本信息](https://blog.csdn.net/whbing1471/article/details/52074390)

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

## 环境变量

[/etc/environment 与 /etc/profile区别](https://blog.csdn.net/lijingshan34/article/details/86568596)

`/etc/environment`是设置整个系统的环境, 而`/etc/profile`是设置所有用户的环境, 前者与登录用户无关, 后者与登录用户有关.

跟环境变量相关的参数:

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

## shell 模式切换

1. 查看系统支持的shell模式及位置

`echo &SHELL`
`cat /etc/shells`

2. 切换shell为/bin/sh

`# chsh -s /bin/sh`

## 重启x-org

[xorg 重新启动X窗口服务器](https://www.kaifa99.com/ubuntu/article_156280)

在`systemd`系统上(`Ubuntu 15.04`和更新版本)

```bash
sudo systemctl restart display-manager
```

注: 这将强制退出所有图形程序, 将丢失未保存的工作, 强制被注销. 非图形程序不会受到影响.

对于其他`Ubuntu`版本, 首先使用以下命令找到`Ubuntu`的显示管理器:

```bash
cat /etc/X11/default-display-manager
```

根据显示管理器的不同, 可以使用以下命令之一:

```bash
sudo restart lightdm # 使用LightDM
sudo restart gdm # Gnome (带GDM )
sudo restart kdm # KDE (带KDM )
sudo restart mdm # 对于MDM (例如对于Mint Cinnamon )
```

[Display manager](https://wiki.archlinux.org/title/Display_manager)

`显示管理器`(display manager), 或称`登录管理器`(login manager), 通常是一个图形用户界面, 在 boot 过程结束时显示, 以取代默认的shell.
显示管理器有多种实现方式, 就像有各种类型的 window managers 和 desktop environments 一样. 通常每一种都有一定程度的定制和主题性可供选择.

## ls 选项

`ls -d */`

+ `-d`; 选项指定只列出目录,`glob`模式当前目录下`*/`表示所有的子目录
+ `-S` ;  按文件大小排序,大的优先
+ `--sort=WORD` ;  按`WORD`排序,而不是`name`: none (-U), size (-S), time (-t), version (-v), extension (-X)
+ `--time=WORD`; 和 `-l`一起使用, 显示`WORD`时间, 而不是默认的修改时间, 可使用字段:
`atime` or `access` or `use` (`-u`); `ctime` or `status` (`-c`); 同时使用指定的 `time` 作为排序键, 如果使用了`--sort=time` (新的在前)
+ `-X` ; 按拓展名的字母顺序排列
+ `-m`; 用逗号分隔的条目列表填充宽度
+ `-x` ;    按行而不是按列输出条目
+ `-b, --escape`;    对非图形字符, 打印`C`式转义符
+ `-q, --hide-control-chars`;    对非图形字符, 打印`?`
+ `-1` ;    每行打印一个文件. 可以使用`-q`或者`-b`避免`\n`
+ `--format=WORD` ;  横跨`-x`,逗号`-m`,水平`-x`,长`-l`,单列`-1`,verbose`-l`,垂直`-C`

## 别名(alias)

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

## zsh 别名

+ `grep`='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'

## 文件管理 cp rm mv

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

## 重命名 rename

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
+ 转化; `tr` or `y` :  相当于一个映射表格, 进行批量替换

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
如果在修饰符中加上`m`, 那么开始和结束将会指字符串的每一行: 每一行的开头就是`^`, 结尾就是`$`.
+ `o`    表达式只执行一次.
+ `s`    如果在修饰符中加入`s`, 那么默认的`.`代表除了换行符以外的任何字符将会变成任意字符, 也就是包括换行符!
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

## 获取绝对路径 realpath

`realpath` - `print the resolved path`(打印已解析的路径)

SYNOPSIS
`realpath [OPTION]... FILE...`

DESCRIPTION
打印解析的绝对文件名;  除最后一个组件外的所有组件都必须存在

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

## tar unzip

***
创建压缩文件

+ `tar -cvf a.tar /etc`
+ `gzip foo.txt`
+ `gzip -fvr foo.txt `: force,verbose,recursive
+ `zip -r foo.zip a b c ...`

创建存档的同时用`gunzip`压缩: `tar -czvf a.tar.gz /etc`.
`tar`默认把路径当成相对路径, 如果提供的路径为`/home/user/file`, `tar`在创建存档时依次创建这些目录层次, 并且去掉开头的`/`, 使用`-P`选项改变默认设置.
下面讨论一些`tar`的选项:

+ `-P, --absolute-names`:在创建存档时, 不去掉领头的`/`.
+ `--no-recursion`:避免自动递归子目录.
+ `--recursion`:递归子目录, 默认.
+ `-f, --file=ARCHIVE`: 设置存档用的文件或设备为`ARCHIVE`.

如果未提供`--file=ARCHIVE`, 则`tar`将首先检查环境变量`TAPE`. 如果`TAPE`不为`null`, 其值将用作存档名称.
否则, `tar`将采用编译的默认值. 默认值可以使用`--show-defaults`选项, 或在`tar --help`输出的末尾查看.
用带有`:`的存档名称表示远程计算机上的文件或设备. 冒号之前的部分作为机器名称或`IP`地址, 其后的部分为文件或设备路径名, 例如: `--file=remotehost:/dev/sr0`.
也可以使用`user@host`, 即`用户名@主机名`的形式. 默认情况下, 通过`rsh(1)`命令访问远程主机. 如今通常使用`ssh(1)`代替, 可以通过以下选项指定: `--rsh-command=/usr/bin/ssh`.
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

`unzip` 解压特定文件合并到当前目录: 使用`unzip -j`选项: junk paths, 不会重新创建档案的目录结构;  所有文件都存放在提取目录中(默认为当前目录), 结合通配符.

```bash
unzip -j '*.zip'  '*.otf'
```

## 7z

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
    允许使用布尔型开关的简化形式: `sw+`或`sw`代替`sw=on`, `sw-`代替`sw=off`.
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
注意: 在完成归档之前, 请不要使用卷(也不要复制卷). `7-Zip` 可能在归档操作结束时改变任何卷, 包括第一个卷.

例子, 创建多卷`a.7z`档案. 第一个卷是`10KB`, 第二个是`15KB`, 其他都是`2MB`. :

```bash
7z a a.7z *.txt -v10k -v15k -v2m
```

+ `7z`解压多个`.zip`文件; 使用下列形式:

    ```
    7z -an -ai[r[-|0]]@{listfile} | !{wildcard}
    ```

`-an`表示禁止`bash`解析压缩文档名, `-ai`表示要包括的压缩文件名. 由于`!`在`bash`中是特殊符号, 需要转义或者用引号`'`裹起来:

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

### 7z 的通配符

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

## 查看和安装字体

`fc-list`: 列出系统中可用的字体

```bash
# 语法
fc-list [ -vVh ]  [ pattern  [ element... ]   ]
# 例如列出所有中文字体, :lang=zh  代表匹配模式
fc-list :lang=zh
```

### 安装字体

[x-org 系统字体命令和字体的安装](https://www.jianshu.com/p/e7f12b8c8602)

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

+ 建立字体缓存

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

## grep 过滤输出

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

## 文档格式转换

用 `pandoc`

`pandoc [options] [input-file]...`

`-f --from -t --to`

`--latex-engine=pdflatex|lualatex|xelatex`

把markdown转换成`pdf`

```bash
pandoc -f markdown --latex-engine=xelatex -o output.pdf input.md
```

## 挂载命令 mount

[linux挂载命令mount及U盘,移动硬盘的挂载](https://www.cnblogs.com/sunshine-cat/p/7922193.html)
[gpt格式的移动硬盘在Linux系统下挂载方法](https://blog.csdn.net/zhang_can/article/details/79714012)

+ `mount -l -t type` : `-l` 选项可以显示`label`
+ `findmnt [options] device|mountpoint`: 可以更清晰的显示文件系统
+ `umount [-dflnrv] {directory|device}` : 卸载文件系统,应该通过给出文件目录来使用,`-l, --lazy`Lazy  unmount

+ `fdisk` 查看磁盘列表

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

## U盘格式化 exFAT

[将 USB 盘格式化为 exFAT](https://linux.cn/article-12294-1.html)

从 `Linux kernel 5.4`开始, `Linux` 内核本身中启用了 `exFAT` 文件系统支持.
检查正在运行的 `Linux` 内核版本: `uname -r`. 如果是内核 `5.4` 或更高版本, 那么应该没问题.
不然, 你必须启用`exFAT`支持. 在基于 `Ubuntu` 的发行版中, 你可以安装以下软件包:

```bash
sudo apt install exfat-fuse exfat-utils
```

***
方法 1: 使用 `GNOME 磁盘工具`将磁盘格式化为 `exFAT`.

使用`GNOME 磁盘` 格式化驱动器是一项简单的工作. 它预装在许多 Linux 发行版中.
插入外部 `USB` 盘. 在菜单中查找 `Disk`, 然后打开`GNOME 磁盘` 应用. 第一步, 选择要格式化的驱动器, 要使用 `exFAT`, 请选择 `其它`, 然后单击`下一步`.

***
方法 2: 在 `Linux` 命令行中将磁盘格式化为 `exFAT`.

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

## 查看文档首行末行

文档尾巴, `tail -n, --lines=[+]NUM`, 从第`num`行开始.
文档开头, `head -n, --lines=[-]NUM`, 减去最后`num`行.

## 查看使用的桌面环境 x11 wayland

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

## 查看 linux 系统信息

ref: [3 Ways to Check Linux Kernel Version in Command Line](https://itsfoss.com/find-which-kernel-version-is-running-in-ubuntu/)

***
uname

`inxi -S`: 命令行系统信息脚本 for 终端和 IRC(InternetRelayChat)
`uname` -打印系统信息
`uname -r`命令的输出为:`5.4.0-48-generic`

这意味着您正在运行Linux内核`5.4.0-48`,或更笼统地说,您正在运行Linux内核版本`5.4`.

但是其他数字在这里意味着什么?

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
