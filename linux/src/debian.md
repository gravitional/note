# debian

## update-initramfs

`update-initramfs` - generate an `initramfs` image

SYNOPSIS

```bash
update-initramfs {-c|-d|-u} [-k version] [-v] [-b directory]
update-initramfs -h
```

DESCRIPTION

update-initramfs 脚本在`local box`中管理 `initramfs` 映像.
它跟踪`/boot`中现有的initramfs存档.  有创建, 更新或删除三种操作模式.  您必须至少指定这些模式之一.

`initramfs`是一个`gzipped cpio archive`.
在`boot time`, `kernel`将归档文件解压缩到`RAM disk`中, 进行`mounts`并将其用作`initial root file system`.
所有`root device `的发现都发生在这个早期的用户空间中.

+ `-c` This mode creates a new initramfs.
+ `-u` This mode updates an existing initramfs.
+ `-d`This mode deletes an existing initramfs.

## update-alternatives

[Debian之 update-alternatives命令配置软链接](https://blog.csdn.net/lzxomg/article/details/76353677)

可以将具有相同或相似功能的多个程序同时安装在单个系统上. 例如, 许多系统同时安装了多个文本编辑器.
这为系统的用户提供了选择, 允许每个用户使用不同的编辑器(如果需要), 但是如果用户未指定特定的首选项, 则程序很难为编辑器做出好的选择, 以调用该编辑器.

`Debian`的`alternatives`系统旨在解决这个问题. 文件系统中的`generic name`由提供类似功能的所有文件共享.
`alternatives`系统和系统管理员共同确定此`generic name`引用哪个实际文件.
例如, 如果文本编辑器`ed`  和`nvi`都安装在系统上,
`alternatives`系统将使`generic name`:`/usr/bin/editor`在默认情况下引用`/usr/bin/nvi`.
系统管理员可以覆盖此设置, 并使其改为引用`/usr/bin/ed`, 并且`alternatives`系统只有在明确要求这样做时, 才会更改此设置.

`generic name`不是指向所选替代项的直接符号链接.
相反, 它是指向`alternatives directory`中名称的符号链接, 而该目录又是指向所引用的实际文件的符号链接. 也就是二次软链接
这样做是为了将系统管理员的更改限制在`/etc`目录中: `FHS(q.v.)`给出了这种方式的优点.

***
由于更新替代方案的活动涉及很多, 因此某些特定术语将有助于解释其操作.

+ `generic name` (`link`): 一个名称, 例如`/usr/bin/editor`, 它通过候选系统指向多个功能相似的文件之一
+ `alternative name`(`name`) : 候选目录中的符号连接名称.
+ `alternative` (`path`) : 指向实际文件的路径, 可以通过`link`进行访问
+ `alternatives directory`(候选目录) :  缺省情况下, 目录`/etc /alternatives`包含符号链接.
+ `administrative`目录 :  默认情况下为`/var/lib/dpkg/alternatives`的目录, 其中包含`update-alternatives`的状态信息.
+ `link group` : 一组相关的符号链接, 作为一个组进行更新.
+ `master link` :  链接组中的候选链接, 用于确定组中其他链接的配置方式.
+ `slave link`:  链接组中的候选链接, 该链接由主链接的设置控制.
+ `automatic mode` : 当链接组处于自动模式时, 替代系统将确保组中的链接指向适合该组的最高优先级替代.
+ `manual mode`:  当链接组处于手动模式时, 替代系统将不会对系统管理员的设置进行任何更改.

### display选项

例如可以用来查看本机上所有可以用来被 `editor` 链接的命令.

`display`选项用来显示一个`generic name`的所有可选命令, 即查看一个命令链接组的所有信息, 包括链接的模式(自动还是手动), 链接`priority`值, 所有可用的链接命令等等.

例如: `update-alternatives --display java`

### install选项

`install`选项的功能是向系统添加一组`alternatives`.

使用语法为: `update-alternatives --install link name path priority [--slave link name path]...`

其中`link`为系统中功能相同软件的公共链接目录, 比如`/usr/bin/java`(需绝对目录);
`name`在`alternatives  directory`中, 是`link`的`symlink`,如`java`;
`path`为你所要使用新命令, 新软件的所在目录;
`priority`为优先级, 当命令链接已存在时, 需高于当前值, 因为当`alternative`为自动模式时,系统默认启用`priority`高的链接; `–slave`为从`alternative`,后面的参数是类似的.

### config选项

`config`选项用来显示和修改实际指向的候选命令, 在现有的命令链接选择一个作为系统默认.

首先, `update-alternatives` 在一般情况下是由 `postinst` 和 `prerm` 这样的安装脚本自动调用的, 所以一个 `alternative` 的状态有两种: 自动和手动.
每个 `alternative` 的初始状态都是自动. 如果系统发现管理员手动修改了一个 `alternative`, 它的状态就从自动变成了手动, 这样安装脚本就不会更新它了. 如果你希望将一个 `alternative` 变回自动, 只要执行代码:
`update-alternatives --auto java`

`-auto`, `--display` 和 `--config` 跟的都是 link.
我们要说的第三个概念是优先级. 这个比较简单, 当然优先级越高的程序越好啦.
最后一个概念是主和从的 `alternative` . 想想看, 你将 `/usr/bin/editor` 链接到了 `vim`, 可是当你执行 `man editor` 时看到的却是 `emacs` 的 `manpage`, 你会做何感想呢? 这就引出了主和从 `alternative` 的概念了: 当更新主的 `alternative` 时, 从的 `alternative` 也会被更新.

### remove选项

`remove` 选项的功能是删除一个命令的`link`值, 其附带的`slave`也将一起删除.

使用语法为:`update-alternatives --remove name path`.

其中`name` and `path`, 与`--install`中的一致, 如果所删除的链接组中还有其他链接的话, 系统将会自动从其他中选择一个`priority`高的链接作为默认为链接.  比如:

```bash
update-alternatives –remove java /usr/lib/jvm/jre1.6.0_20/bin/java
```

### 例子

```bash
## 如果Linux存在 其他已默认的JDK, 比如openjdk, 现在我要将自己安装的JDK设置为默认的JDK版本, 执行下面的代码:
sudo update-alternatives --install /usr/bin/java java /usr/java/bin/java 1000
sudo update-alternatives --install /usr/bin/javac javac /usr/java/bin/javac 1000
# 然后执行下面代码选择我们安装的JDK版本:
sudo update-alternatives --config java
# 然后选择java版本.
```

## build-dep

### lyx手动编译

***
如果是后安装的`texlive`, `lyx` 需要运行 `tools-reconfigure` 进行配置

***
[compiling-lyx-2-2-on-debian][]

[compiling-lyx-2-2-on-debian]: https://unix.stackexchange.com/questions/321039/compiling-lyx-2-2-on-debian

编译的时候报错,`cannot compile a simple Qt executable. Check you have the right $QTDIR`.

`QTDIR`并不是必要的, 但是尝试将其设置为`/usr/share/qt5`.

解决方法:

您可以尝试构建Debian源代码包:

```bash
sudo apt-get install devscripts dpkg-dev build-essential
sudo apt-get build-dep lyx
dget http://httpredir.debian.org/debian/pool/main/l/lyx/lyx_2.3.2-1.dsc
cd lyx-2.2.0
dpkg-buildpackage -us -uc
```

前两个命令安装了构建`lyx`所需的软件包.
然后`dget`下载并提取源程序包,
和`dpkg-buildpackage`构建它并产生一系列`.deb`软件包
您可以照常使用`dpkg`手动安装.

***
代码解释

`build-dep`导致`apt-get`安装/删除软件包, 以试图满足源软件包的构建依赖性.
默认情况下, 满足依赖关系才能本地构建程序包
下载源代码包需要在 `source.list` 中添加软件源代码的源,即以` deb src` 开头的行.

`gpg`

Your issue is you have not imported our public key into your keyring. You skipped a step.

***
`tar --one-top-level -xJvf  lyx`

`-J, --xz`

通过`xz(1)`过滤存档.

`--one-top-level[=DIR]`

将所有文件解压缩到`DIR`中, 如果没给出参数, 则使用存档的`basename`
(从名字中减去--auto-compress可以识别的标准压缩后缀).

***
`dpkg-buildpackage -us -uc`

`-us`, `--unsigned-source`

不要对源软件包进行签名(自`dpkg 1.18.8`起支持长选项).

`uc`, `--unsigned-changes`
不对`.buildinfo`和`.changes`文件签名(自`dpkg 1.18.8`起支持长选项).

***
Compiling and installing LyX

快速编译指南

这四个步骤将编译, 测试和安装LyX:

1. Linux用户请注意: 您需要使用相同版本的`qt4/5` and `qt4/5-devel` 软件包来编译 `LyX`.
通常, 还建议安装`pkg-config`(名称可能因您的发行版而异).

1.`./configure`根据您的系统配置LyX.
您可能必须设置`--with-qt-dir=<path-to-your-qt-installation>`(例如, `--with-qt-dir=/usr/share/qt4/`), 如果未设置环境变量`QTDIR`并且`pkg-config`不可用.
您需要`--enable-qt5`开关来选择`qt5`而不是`qt4`.
如果不存在`./configure`脚本, 请参见下面的注释.

`./configure --enable-qt5`

1. `make`编译程序.
1. `src/lyx`运行程序, 以便您可以检出它.
1. `make install`将安装它.  如果您想要较小的二进制文件, 则可以使用`make install-strip`代替.

## tex的TDS

latex 组织文件的规范叫做 TDS-compliant

a standard `TeX Directory Structure` (TDS): 宏,字体和其他与实现无关的TeX系统文件的目录层次结构.

### TDS 特征

TDS树中的共有属性

+ Subdirectory searching
+ Rooting the tree
+ Local additions
+ Duplicate filenames

***
子目录搜索

Technical Working Group (TWG) 要求,一个综合的TDS 需要支持 implicit subdirectory searching.

更精确地说,具体实现要满足,在寻找输入文件(TeX,Metafont and their companion utilities )的时候,能够指定具体路径,也能够递归地遍历所有的子文件夹.

***
tree的根目录

我们把`TDS`的根目录称为`texmf`(TeX and Metafont),意思是,这个目录包含了一个完整TeX 系统附属的文件(including Metafont, MetaPost, BibTeX, etc.),而不是只有单独的 TeX 自己.

***
Local additions

TDS 不能精确地指出,哪些包是"local addition".

One common case of local additions is dynamically generated files, e.g., PK fonts by the mktexpk script (which originated in Dvips as MakeTeXPK). A site may store the generated files directly in any of:

+ their standard location in the main TDS tree (if it can be made globally writable);
+ an alternative location in the main TDS tree (for example, under texmf/fonts/tmp);
+ a second complete TDS tree (as outlined above);
+ any other convenient directory (perhaps under /var, for example /var/spool/fonts).

***
重复文件名

TDS tree 中的文件可能有相同的文件名.默认并不进一步区分,但TDS要求满足以下例外:

Names of TeX input files must be unique within each first-level subdirectory of `texmf/tex` and `texmf/tex/generic`, but not within all of `texmf/tex`; 比如, different TeX formats may have files by the same name.

所以具体实现必须提供**格式依赖**的路径指定方式.

### TDS顶层目录

texmf root 下面包含了 TeX system 的主要成员
The top-level directories specified by the TDS are:

+ `tex` for TeX files (Section Macros).
+ `fonts` for font-related files (Section Fonts).
+ `metafont` for Metafont files which are not fonts (Section Non-font Metafont files).
+ `metapost` for MetaPost files (Section MetaPost).
+ `bibtex` for BibTeX files (Section BibTeX).
+ `scripts` for platform-independent executables (Section Scripts).
+ `doc` for user documentation (Section Documentation).
+ `source` for sources. This includes both traditional program sources (for example, Web2C sources go in texmf/source/web2c) and, e.g., LaTeX dtx sources (which go in texmf/source/latex). The TDS leaves unspecified any structure under source.
+ `implementation` for implementations (examples: `emtex`, `vtex`, `web2c`), to be used for whatever purpose deemed suitable by the implementor or TeX administrator.
That is, files that cannot be shared between implementations, such as pool files (tex.pool) and memory dump files (plain.fmt) go here, in addition to implementation-wide configuration files.
+ `program` for program-specific input and configuration files for any TeX-related programs (examples: `mft`, `dvips`). In fact, the `tex`, `metafont`, `metapost`, and `bibtex` items above may all be seen as instances of this case.

## CMake

[CMake is an open-source](https://cmake.org/)
[Ubuntu16.04下安装Cmake](https://blog.csdn.net/l1216766050/article/details/77513045)

CMake is an open-source, cross-platform family of tools designed to build, test and package software.

`CMake`用于使用简单平台和独立于编译器的配置文件来控制软件编译过程,并生成可在您选择的编译器环境中使用的本机`makefile`和工作区. `CMake`工具套件是由Kitware创建的,旨在满足ITK和VTK等开源项目对强大,跨平台构建环境的需求.

## linux 版向日葵

### 安装

下载linux版本,然后安装

安装命令:
Ubuntu/Deepin系统: `sudo dpkg -i 文件名.deb`

*卸载命令(2步):

```bash
sudo dpkg -l | grep sunlogin
sudo dpkg -r sunloginclient
```

PS: 安装包因版本不同,名字可能会有所出入,建议直接复制当前下载安装包名字进行安装.
如果您的系统高于16.04版本,遇到`lib`依赖问题的话,可按照如下操作:

安装报错:

```bash
尝试sudo /安装路径/sunloginclient启动程序
```

启动报错:

```bash
使用ldd /安装路径/sunloginclient 查看依赖缺失问题
```

发现`libncurses.so.5/libform.so.5/libtinfo.so.5`缺失,由于版本较高,
依赖包版本也可能是更高版本,因此需要创建软链接.

具体如下:

查找现有`lib`包

可以使用`find / -name libncurses*`查找
找到后使用`ln -s /usr/lib/源依赖包位置 /usr/lib/指向依赖包位置`

创建成功后,卸载`sunloginclient`,再次安装`deb`包.

安装成功.直接调用`sudo /usr/local/sunlogin/bin/sunloginclient`即可运行

### 登录

1. 通过命令行开启向日葵: `sudo /usr/local/sunlogin/bin/sunloginclient`启动(路径为向日葵默认安装路径)
2. 登录向日葵: 开启程序后的初始状态为未绑定,可见界面左上角的提示`Sunlogin (F12)`为进入菜单选项

## dpkg

### dpkg 应用管理

***
安装应用 deb pkg

```bash
dpkg -i pkg
```

`-i, --install`
`ldd /bin/ls` : `ldd`查看依赖信息
`-r, --remove package...|-a|--pending`

删除已安装的程序包.  这会删除除`conffiles`和其他由`postrm`脚本清除的数据以外的所有内容,
这可能避免在以后重新安装该软件包时重新配置该软件包(`conffiles`是`DEBIAN/conffiles control file`中列出的配置文件).
如果没有`DEBIAN/conffiles `或`DEBIAN/postrm`脚本, 则此命令等效于`--purge`.
如果给出`-a`或`--pending`而不是软件包名称, 则所有在`/var/lib/dpkg/status`标记为删除的, `unpacked`的软件包将被删除.
卸下软件包包括以下步骤:

1. 运行`prerm`脚本
1. 删除已安装的文件
1. 运行`postrm`脚本

`-P, --purge package...|-a|--pending`

清除已安装或已删除的软件包. 这将删除所有内容, 包括`conffiles`, 以及从`postrm`中清除的所有其他内容.
如果给出`-a`或`--pending`而不是软件包名称, 则所有在`/var/lib/dpkg/status`中标记为要清除的软件包, `unpacked` or `removed`, 都会被清除

注意: `dpkg`可能不了解某些配置文件, 因为它们是通过`configuration scripts`单独创建和处理的.
在这种情况下, `dpkg`不会自行删除它们, 但是软件包的`postrm script`(由`dpkg`调用)在清除过程中必须要小心.
当然, 这仅适用于系统目录中的文件, 不适用于个人用户主目录中的配置文件.

清除软件包包括以下步骤:

1. `Remove` the package(如果尚未卸载). 有关如何完成此操作的详细信息, 请参见`--remove`.
2. 运行 `postrm script` .

***
dpkg-query actions

有关以下操作的更多信息, 请参见`dpkg-query(1)`.

+ `-l, --list package-name-pattern...`: 列出与给定模式匹配的软件包.
+ `-s, --status package-name...`: 报告指定软件包的状态.
+ `-L, --listfiles package-name...`: 从软件包名称列出安装到系统的文件.
+ `-S, --search filename-search-pattern...` 从已安装的软件包中搜索文件名.
+ `-p, --print-avail package-name...` 显示有关软件包名称的详细信息, 存放在`/var/lib/dpkg/available`,基于`APT`的前端的用户应该改用`apt-cache` 显示这个软件的信息
