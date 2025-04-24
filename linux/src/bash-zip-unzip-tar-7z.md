# bash 压缩解压缩

[runoob-linux-tutorial](https://www.runoob.com/linux/linux-tutorial.html)

## 查看帮助

1. `命令 -- help`
1. `man 命令`

后者更加详细

首先帮助中尖括号`<>`和方括号`[]`以及省略号`...`的含义,

在方括号内的表达式`[` 和 `]`之间的字符是可选的(要去掉括号).
在尖括号内的表达式`<`和`>`之间的字符, 是必须替换的表达式(要去掉括号).

省略号表示该选项可以单个或多个

以`7z`的参数为例:

```bash
7z <command> [<switches>...] <archive_name> [<file_names>...]  [<@listfiles...>]
```

```bash
7z <命令> [[选项]...] <档案名称(压缩包名称)> [<被压缩的文件名>...] [<@文件列表(如txt文件中的文件列表)...>]
```

实例分析

```bash
7z a -tzip -p111 archive.zip txt.txt  txt.txt文件压缩为archive.zip, 压缩方式为zip 密码为111
7z x -tzip -p111 archive.zip            解压 密码为111
```

`a`为命令, 命令至少要有一个

`-t /-p`为选项(`switch`), 可选`0`个或多个, `zip`和`111`分别为`-t`和`-p`的参数, 中间不需要空格
选项(如`-r`)放前放后都行

`7z`的命令不区分大小写, 有些命令如`cp`要区分

示例

```bash
7z x archive.zip
#从压缩档案 archive.zip 中释放所有文件到当前文件夹.
7z x archive.zip -oc:\soft *.cpp
#从压缩档案 archive.zip 中释放 *.cpp 文件到 c:\soft 文件夹.
```

[linux命令行括号帮助含义](https://blog.csdn.net/yandaonan/article/details/56489513)

7-Zip 不使用系统通配符解析器. 7-Zip 不遵循过时的规则, 即 `*.*` 表示任何文件.
7-Zip 将 *.* 解释为与 **具有扩展名** 的任何文件名匹配.
要处理所有文件, 必须使用 * 通配符.
示例:

+ `*.txt` 表示所有扩展名为 `.txt` 的文件
+ `?a*` 表示所有第二个字符为 `a` 的文件
+ `*1*` 表示所有包含字符 `1` 的文件名
+ `*.*.*` 表示所有包含至少两个 `.` 字符的文件名

如果命令行中没有提供*文件名/通配符*, 将使用默认通配符 `*`.
路径末尾带反斜杠 (`\`, 例如 `abc\`), 则它表示目录.
如果路径末尾没有反斜杠 (例如 `abc`), 则路径可以指代 `文件` 或 `目录`.

## alias

[Linux中使用 alias 来简化命令行输入](https://blog.csdn.net/kobejayandy/article/details/38710859)
[bash alias的使用](https://www.jianshu.com/p/63e91c67e39a)

`alias`(别名), 顾名思义, 其作用大概为命令的别名, 那么就可以它的好处就显而易见了. 可以用来缩短一些又长又难记的命令.

方法一:

+ `vim ~/.bashrc` ===> 打开bash的配置文件
+ 输入 `alias 新命令='旧命令 -选项/参数'` ===> 修改命令别名
+ `source ~/.bashrc` ===> 使得修改完的命令别名生效
+ 命令别名的增删改查都可以在`.bashrc`操作

方法二:

+ 增/改: 输入 `alias 新命令='旧命令 -选项/参数'`即完成了命令别名的添加.
+ 删: `unalias` 命令
+ 查: `alias` 命令

## 压缩/解压一览

+ 7z

```bash
7z a win.configrc.7z -pxxxxx `@tom.configrc
7z x win.configrc.7z -pxxxxx
```

+ tar

解包: `tar xvf FileName.tar`
打包: `tar cvf FileName.tar DirName`
(`tar` 仅仅是打包, 也有压缩的选项)

+ `.gz`

解压1: `gunzip FileName.gz`
解压2: `gzip -d FileName.gz`
压缩: `gzip FileName`

`.tar.gz` 和 `.tgz`
解压: `tar zxvf FileName.tar.gz`
压缩: `tar zcvf FileName.tar.gz DirName`

+ `.bz2`

解压1: `bzip2 -d FileName.bz2`
解压2: `bunzip2 FileName.bz2`
压缩:  `bzip2 -z FileName`

`.tar.bz2`
解压: `tar jxvf FileName.tar.bz2`
压缩: `tar jcvf FileName.tar.bz2 DirName`

+ `.bz`

解压1: `bzip2 -d FileName.bz`
解压2: `bunzip2 FileName.bz`
压缩: 未知

`.tar.bz`
解压: `tar jxvf FileName.tar.bz`
压缩: 未知

+ `.Z`

解压: `uncompress FileName.Z`
压缩: `compress FileName`
`.tar.Z`

解压: `tar Zxvf FileName.tar.Z`
压缩: `tar Zcvf FileName.tar.Z DirName`

+ `.zip`

解压: `unzip FileName.zip`
压缩: `zip FileName.zip DirName`

+ `.rar`

解压: `rar x FileName.rar`
压缩: `rar a FileName.rar DirName`

+ `.lha`

解压: `lha -e FileName.lha`
压缩: `lha -a FileName.lha FileName`

+ `.rpm`

解包: `rpm2cpio FileName.rpm | cpio -div`

+ `.deb`

解包: `ar p FileName.deb data.tar.gz | tar zxf -`

+ `sEx`

`.tar` `.tgz` `.tar.gz` `.tar.Z` `.tar.bz` `.tar.bz2` `.zip` `.cpio` `.rpm` `.deb` `.slp` `.arj` `.rar` `.ace` `.lha` `.lzh` `.lzx` `.lzs` `.arc` `.sda` `.sfx` `.lnx` `.zoo` `.cab` `.kar` `.cpt` `.pit` `.sit` `.sea`

解压: `sEx x FileName.*`
压缩: `sEx a FileName.* FileName`

`sEx`只是调用相关程序, 本身并无压缩, 解压功能, 请注意!

+ gzip 命令

减少文件大小有两个明显的好处, 一是可以减少存储空间, 二是通过网络传输文件时, 可以减少传输的时间.
`gzip` 是在 `Linux` 系统中经常使用的一个对文件进行压缩和解压缩的命令, 既方便又好用.

语法: `gzip [选项] <压缩(解压缩)的文件名>`

该命令的各选项含义如下:

+ `-c` 将输出写到标准输出上, 并保留原有文件.
+ `-d` 将压缩文件解压.
+ `-l` 对每个压缩文件, 显示下列字段: 压缩文件的大小; 未压缩文件的大小; 压缩比; 未压缩文件的名字
+ `-r` 递归式地查找指定目录并压缩其中的所有文件或者是解压缩.
+ `-t` 测试, 检查压缩文件是否完整.
+ `-v` 对每一个压缩和解压的文件, 显示文件名和压缩比.
+ `-num` 用指定的数字 `num` 调整压缩的速度, `-1` 或 `--fast` 表示最快压缩方法(低压缩比);`-9` 或`--best`表示最慢压缩方法(高压缩比).系统缺省值为`6`.

指令实例:

+ `gzip *%` 把当前目录下的每个文件压缩成`.gz`文件.
+ `gzip -dv *` 把当前目录下每个压缩的文件解压, 并列出详细的信息.
+ `gzip -l *` 详细显示例1中每个压缩的文件的信息, 并不解压.
+ `gzip usr.tar` 压缩`tar`备份文件`usr.tar`, 此时压缩文件的扩展名为`.tar.gz`.

[Linux/Ubuntu下解压命令](https://blog.csdn.net/u013063153/article/details/53894711)

## tar unzip

### 创建压缩文件

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

### 解压缩`xxx.tar.gz`

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

### 支持的格式

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

### 各种动作

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

+ `--`; 停止解析开关
+ `-o{Directory}`: 设置输出目录
+ `-p{Password}`: 设置密码
+ `-r[-|0]`: 递归子目录,作者不推荐使用
+ `-y` : 对所有询问回答 yes
+ `-t{Type}`: 设置归档的类型
+ `-ai[r[-|0]]@{listfile} | !{wildcard}`: 包括额外的压缩文件, 以及通配符. 支持使用多次.
+ `-an`: 禁止命令行自动解析压缩文档的名字,需要与`-ai` (Include archives)开关一起使用
+ `-m`: 设置压缩方法开关
+ `-v{Size}[b | k | m | g]`: 创建 Volumes, 指定分卷体积.

`-an`禁止解析命令行中的压缩文档名称. 此开关必须与 `-ai` (Include archives) 开关一起使用.
如果你的压缩文档在列表文件中给出, 你要用`-ai`开关指定它, 所以在命令行中你需要禁止解析`存档_名称`字段.

+ quiet 忽略输出, 7z 没有提供专门的 silence or quiet 开关, 但可以将输出重定向到 `/dev/null`

```bash
7z a file.7z ~/file > /dev/null # in bash
7z a file.7z ~/file | ignore # in nushell
```

### 方法详情

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

+ `-v`

+ `-v{Size}[b | k | m | g]`: 创建 Volumes, 指定分卷体积.

以 `Bytes`,` Kilobytes` (1 `Kilobyte` = 1024 `bytes`), `Megabytes` (1 `Megabyte` = 1024 `Kilobytes`) 或 `Gigabytes` (1 `Gigabyte` = 1024 `Megabytes`) 为单位指定卷的大小.
如果你只指定 `{Size}`, `7-zip` 会把它当作字节来处理. 可以指定多个 `-v` 开关.
注意: 在完成归档之前, 请不要使用卷(也不要复制卷). `7-Zip` 可能在归档操作结束时改变任何卷, 包括第一个卷.

例子, 创建多卷`a.7z`档案. 第一个卷是`10KB`, 第二个是`15KB`, 其他都是`2MB`. :

```bash
7z a a.7z *.txt -v10k -v15k -v2m
```

+ `7z`解压多个`.zip`文件; 使用下列形式:

```bash
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

### 例子

+ 解压`zip`文件同时, 创建外层的同名文件夹.

处理带有空格的文件名参考[For Loop File Names With Spaces](https://www.cyberciti.biz/tips/handling-filenames-with-spaces-in-bash.html)

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

## 安装 Windows 字体

或者, 将所有的 `Windows` 字体复制到 /usr/share/fonts 目录下并使用一下命令安装字体:

```bash
mkdir /usr/share/fonts/WindowsFonts
cp /Windowsdrive/Windows/Fonts/* /usr/share/fonts/WindowsFonts
chmod 755 /usr/share/fonts/WindowsFonts/*
```

最后, 使用命令行重新生成`fontconfig` 缓存:

```bash
fc-cache
```

### vscode 调整字体

"editor.fontFamily": "Fira Code, Source Code Pro, Noto Sans CJK SC, monospace"

这个是用的谷歌的开源字体思源黑体.

`makrdown` 的话, 还有个"`markdown.preview.fontFamily`"设置.

>但是也许是我的版本较高, 版本`1.23.1`, 我的里面设置是这样子的:
>`"editor.fontFamily": "Consolas, Dengxian"`,
>英文字体用了`Consolas`, 如果不适用的字体就用`Dengxian`

我的设置

```json
"editor.fontFamily": "Fira Code Retina, Microsoft YaHei",
"editor.fontLigatures": true,
```

`makrdown` 的话, 还有个`"markdown.preview.fontFamily"`设置.

[refer1](https://zhuanlan.zhihu.com/p/40434062)
[refer2](https://segmentfault.com/a/1190000004168301)
[refer3][(https://www.v2ex.com/t/453862)

### oh-my-zsh 中定义的别名

为了方便使用, oh-my-zsh 定义了许多别名, 下面是其中一部分.

```bashrc
-='cd -'
...=../..
....=../../..
.....=../../../..
......=../../../../..
1='cd -'
2='cd -2'
3='cd -3'
4='cd -4'
5='cd -5'
6='cd -6'
7='cd -7'
8='cd -8'
9='cd -9'
_='sudo '
afind='ack -il'
diff='diff --color'
egrep='egrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
fgrep='fgrep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
g=git
ga='git add'
gaa='git add --all'
gam='git am'
gama='git am --abort'
gamc='git am --continue'
gams='git am --skip'
gamscp='git am --show-current-patch'
gap='git apply'
gapa='git add --patch'
gapt='git apply --3way'
gau='git add --update'
gav='git add --verbose'
gb='git branch'
gbD='git branch -D'
gba='git branch -a'
gbd='git branch -d'
gbda='git branch --no-color --merged | command grep -vE "^(\+|\*|\s*($(git_main_branch)|development|develop|devel|dev)\s*$)" | command xargs -n 1 git branch -d'
gbl='git blame -b -w'
gbnm='git branch --no-merged'
gbr='git branch --remote'
gbs='git bisect'
gbsb='git bisect bad'
gbsg='git bisect good'
gbsr='git bisect reset'
gbss='git bisect start'
gc='git commit -v'
'gc!'='git commit -v --amend'
gca='git commit -v -a'
'gca!'='git commit -v -a --amend'
gcam='git commit -a -m'
'gcan!'='git commit -v -a --no-edit --amend'
'gcans!'='git commit -v -a -s --no-edit --amend'
gcb='git checkout -b'
gcd='git checkout develop'
gcf='git config --list'
gcl='git clone --recurse-submodules'
gclean='git clean -id'
gcm='git checkout $(git_main_branch)'
gcmsg='git commit -m'
'gcn!'='git commit -v --no-edit --amend'
gco='git checkout'
gcount='git shortlog -sn'
gcp='git cherry-pick'
gcpa='git cherry-pick --abort'
gcpc='git cherry-pick --continue'
gcs='git commit -S'
gcsm='git commit -s -m'
gd='git diff'
gdca='git diff --cached'
gdct='git describe --tags $(git rev-list --tags --max-count=1)'
gdcw='git diff --cached --word-diff'
gds='git diff --staged'
gdt='git diff-tree --no-commit-id --name-only -r'
gdw='git diff --word-diff'
gf='git fetch'
gfa='git fetch --all --prune --jobs=10'
gfg='git ls-files | grep'
gfo='git fetch origin'
gg='git gui citool'
gga='git gui citool --amend'
ggpull='git pull origin "$(git_current_branch)"'
ggpur=ggu
ggpush='git push origin "$(git_current_branch)"'
ggsup='git branch --set-upstream-to=origin/$(git_current_branch)'
ghh='git help'
gignore='git update-index --assume-unchanged'
gignored='git ls-files -v | grep "^[[:lower:]]"'
git-svn-dcommit-push='git svn dcommit && git push github $(git_main_branch):svntrunk'
gk='\gitk --all --branches'
gke='\gitk --all $(git log -g --pretty=%h)'
gl='git pull'
glg='git log --stat'
glgg='git log --graph'
glgga='git log --graph --decorate --all'
glgm='git log --graph --max-count=10'
glgp='git log --stat -p'
glo='git log --oneline --decorate'
globurl='noglob urlglobber '
glod='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'\'
glods='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%an>%Creset'\'' --date=short'
glog='git log --oneline --decorate --graph'
gloga='git log --oneline --decorate --graph --all'
glol='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'
glola='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'' --all'
glols='git log --graph --pretty='\''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'\'' --stat'
glp=_git_log_prettily
glum='git pull upstream $(git_main_branch)'
gm='git merge'
gma='git merge --abort'
gmom='git merge origin/$(git_main_branch)'
gmt='git mergetool --no-prompt'
gmtvim='git mergetool --no-prompt --tool=vimdiff'
gmum='git merge upstream/$(git_main_branch)'
gp='git push'
gpd='git push --dry-run'
gpf='git push --force-with-lease'
'gpf!'='git push --force'
gpoat='git push origin --all && git push origin --tags'
gpristine='git reset --hard && git clean -dffx'
gpsup='git push --set-upstream origin $(git_current_branch)'
gpu='git push upstream'
gpv='git push -v'
gr='git remote'
gra='git remote add'
grb='git rebase'
grba='git rebase --abort'
grbc='git rebase --continue'
grbd='git rebase develop'
grbi='git rebase -i'
grbm='git rebase $(git_main_branch)'
grbs='git rebase --skip'
grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox}'
grev='git revert'
grh='git reset'
grhh='git reset --hard'
grm='git rm'
grmc='git rm --cached'
grmv='git remote rename'
groh='git reset origin/$(git_current_branch) --hard'
grrm='git remote remove'
grs='git restore'
grset='git remote set-url'
grss='git restore --source'
grt='cd "$(git rev-parse --show-toplevel || echo .)"'
gru='git reset --'
grup='git remote update'
grv='git remote -v'
gsb='git status -sb'
gsd='git svn dcommit'
gsh='git show'
gsi='git submodule init'
gsps='git show --pretty=short --show-signature'
gsr='git svn rebase'
gss='git status -s'
gst='git status'
gsta='git stash push'
gstaa='git stash apply'
gstall='git stash --all'
gstc='git stash clear'
gstd='git stash drop'
gstl='git stash list'
gstp='git stash pop'
gsts='git stash show --text'
gstu='git stash --include-untracked'
gsu='git submodule update'
gsw='git switch'
gswc='git switch -c'
gtl='gtl(){ git tag --sort=-v:refname -n -l "${1}*" }; noglob gtl'
gts='git tag -s'
gtv='git tag | sort -V'
gunignore='git update-index --no-assume-unchanged'
gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
gup='git pull --rebase'
gupa='git pull --rebase --autostash'
gupav='git pull --rebase --autostash -v'
gupv='git pull --rebase -v'
gwch='git whatchanged -p --abbrev-commit --pretty=medium'
gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit --no-verify --no-gpg-sign -m "--wip-- [skip ci]"'
history=omz_history
l='ls -lah'
la='ls -lAh'
ll='ls -lh'
ls='ls --color=tty'
lsa='ls -lah'
md='mkdir -p'
mma='mathematica -singleLaunch'
rd=rmdir
which-command=whence
```
