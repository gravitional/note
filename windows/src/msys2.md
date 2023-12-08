# msys2 使用

[想在Windows 下使用GCC 等工具, 应该选MinGW 还是MSYS2?](https://www.zhihu.com/question/47505053)

MSYS2 和 MinGW 都提供 gcc, 但是属于两个完全不同的工具链.
前者属于 msys2-devel , 后者属于 mingw-w64-$arch-toolchain.
使用 mingw-gcc 编译的目标文件是原生的,
而使用 msys2-gcc 编译的目标文件依赖于 msys-2.0.dll 提供的虚拟 POSIX 环境.
如果你要编译的东西强依赖于 POSIX syscall (比如 fork 等), 那么就要用 MSYS2 的 gcc.

而如果是要当做一个 release 拿去给别人用的, 用 MinGW 的 gcc 好些.
当然, 事情也不绝对, 对于前者你也可以给源文件 patch 一个 MinGW 移植,
对于后者也可以用 MSYS2 然后附带上 msys-2.0.dll.
根据我自己的经验, 如果你使用目标文件的环境就是 MSYS2 的话,
用 MSYS2 工具链编译的程序要比 MinGW 工具链的稳定一些(对 ruby, python 等工具而言).

请务必记住, MinGW 与 MSYS2 或 Cygwin 不同.
MSYS2 和 Cygwin 是完全的 POSIX 环境, 有很多神奇的功能, 如 `fork()` 和自己的 `malloc()`.
MinGW 基于普通的 Microsoft C Runtime, 没有这些功能.
请明确您要构建的是哪一种环境.

## msys2 环境

[Environments](https://www.msys2.org/docs/environments/)

MSYS2 自带不同的环境, 首先要决定使用哪种环境.
不同环境之间的差异主要在于 `环境变量`, 默认`编译器/链接器`,
architecture, 使用的 system libraries 等.
如果不确定, 请使用 `UCRT64`.

`MSYS` 环境包含基于 unix-like/cygwin 的工具,
位于 `/usr` 下, 其特殊之处在于它始终处于激活状态.
所有其他环境都继承自 `MSYS` 环境, 并在其基础上添加了各种功能.

例如, 在 `UCRT64` 环境中, `$PATH` 变量以`/ucrt64/bin:/usr/bin`开头,
因此你可以获得所有基于 ucrt64 的工具以及所有 MSYS 工具.

### overview

Name  Prefix  Toolchain  Architecture  C Library  C++ Library
msys  MSYS  /usr  gcc  x86_64  cygwin  libstdc++
ucrt64  UCRT64  /ucrt64  gcc  x86_64  ucrt  libstdc++
clang64  CLANG64  /clang64  llvm  x86_64  ucrt  libc++
clangarm64  CLANGARM64  /clangarm64  llvm  aarch64  ucrt  libc++
clang32  CLANG32  /clang32  llvm  i686  ucrt  libc++
mingw64  MINGW64  /mingw64  gcc  x86_64  msvcrt  libstdc++
mingw32  MINGW32  /mingw32  gcc  i686  msvcrt  libstdc++

活动环境通过 `MSYSTEM` 环境变量选择.
将 `MSYSTEM` 设置为 `UCRT64` 并启动登录 `shell` 将进入该环境.

不同启动方式的 区别主要在于编译环境软件包的不同, 如gcc, clang等版本不同.
通用的工具如: grep,git,vim,emacs等等在三种方式内都是一样的.

### GCC 与 LLVM/Clang

这些是用于构建各自软件源中所有软件包的默认 `编译器/工具链`.

基于 GCC 的环境:

+ 目前已广泛测试/使用
+ 支持 Fortran
+ 虽然在 MINGW 环境中也有一个 Clang 软件包, 但它仍然使用 GNU 链接器和 GNU C++ 库.
在某些情况下, Clang 也用于构建软件包, 以防上游更喜欢 Clang 而不是 GCC.

基于 LLVM/Clang 的环境:

+ 仅使用 LLVM 工具, `LLD` 作为链接器, `LIBC++` 作为 C++ 标准库
+ Clang 提供 ASAN 支持
+ 本地支持 TLS(线程本地存储)
+ LLD 比 LD 更快, 但不支持 LD 支持的所有功能
+ 某些工具 lack feature parity 与对应的 GNU 工具
+ 在 Microsoft Windows 10 上支持 ARM64/Arch64 架构

### MSVCRT 与 UCRT

这是 Microsoft Windows 上 C 标准库的两种变体.

`MSVCRT`(Microsoft Visual C++ Runtime)在所有 Microsoft Windows 版本上默认可用,
但由于向后兼容性问题, 它还停留在过去, 与 C99 不兼容, 而且缺少一些功能.

+ 它不兼容 C99, 例如 `printf()` 函数系列, 但...
+ mingw-w64 提供了替代函数, 在许多情况下都与 C99 兼容
+ 它不支持 UTF-8 本地语言
+ 与 MSVCRT 链接的二进制文件不应与 UCRT 的二进制文件混合, 因为它们的内部结构和数据类型不同.
(更严格地说, 为不同目标构建的对象文件或静态库不应混合使用.
为不同 CRTs 生成的 DLL 可以混合使用,
只要它们不跨 DLL 边界共享 CRT 对象, 如 `FILE*` 即可).
同样的规则也适用于 MSVC 编译的二进制文件, 因为 MSVC 默认使用 UCRT(如果未作更改).
+ 开箱即用, 适用于所有版本的 Microsoft Windows.

`UCRT`(Universal C Runtime, 通用 C 运行时)是一个更新的版本, Microsoft Visual Studio 默认也使用它.
它的工作和行为与使用 MSVC 编译的代码无异.

+ 无论是在编译时还是在运行时, 都能更好地兼容 MSVC.
+ 它仅在 Windows 10 中默认使用, 对于旧版本, 您必须自行提供或依赖用户安装.

## Windows terminal 设置登录 shell

[MSYS shell in Windows Terminal Preview?](https://superuser.com/questions/1505610/msys-shell-in-windows-terminal-preview)
[Terminals](https://www.msys2.org/docs/terminals/)

MSYS2 有 [自己的指南](https://www.msys2.org/docs/terminals/) 来将 MSYS2 添加到 Windows 终端.
它使用 `msys2_shell.cmd` 代替手动运行 shell.

```bash
使用方法
    msys2_shell.cmd [选项] [登录 shell 参数］

选项:
    -mingw32 | -mingw64 | -ucrt64 | -clang64 | -msys[2] 设置 shell 类型
    -defterm | -mintty | -conemu 设置终端类型
    -here   将当前目录作为工作目录
    -where DIRECTORY   将指定的 DIRECTORY 作为工作目录
    -[use-]full-path    使用完整的当前 PATH 变量而不是修剪到最小
    -no-start       不使用 "start" 命令, 并返回登录 shell 后产生的 错误代码为该批处理文件 结果错误代码
    -shell SHELL    设置登录 shell
    -help | --help | -? | /?         显示帮助并退出
```

任何不能被视为有效选项的参数和所有之后的参数都作为 login shell command 参数传递.

我想手册本身已经很清楚了.
这里重要的标志是 `-no-start` 和 `-defterm`.
但在这里, 我想添加更多的标志.
我添加的是指南推荐的 `-here` 和 `-shell`, 因为我想选择另一个shell.
你可以根据自己的需要自定义.

```bash
C:\\msys64\\msys2_shell.cmd -defterm -here -no-start -mingw64 -shell zsh
```

因此, JSON 配置文件将是

```json
{
    "guid": "{05351409-756a-416f-81c1-a97a1a2cdfe2}",
    "name": "MINGW64",
    "commandline": "C:\\msys64\\msys2_shell.cmd -defterm -here -no-start -mingw64 -shell zsh",
    "hidden": false
},
```

对于 GUID 字段, 您可以使用 PowerShell 中的 `[guid]::NewGuid().Guid` 生成自己的 `GUID`,
或者使用像这样的[在线服务](https://www.guidgenerator.com/online-guid-generator.aspx).
这只是标识此配置文件 与 其他可能具有相同名称的配置文件不同.

对于最新版本的 Windows 终端, 你无需手动编辑 JSON 文件, 它还会为你生成 GUID.

只需点击 "添加新配置文件 "并指定 "命令行 "字段即可.

`-defterm`; 这就好比你想使用哪个控制台主机/哪个终端模拟器.
在 Windows 10/11 中, 我们有传统的 `conhost.exe` 和 Windows终端.
设置为 `-defterm` 后, Msys2 将使用 Windows 默认的 Conhost(本例中为 Windows 终端).

## 登录shell

[改变MSYS2默认shell为zsh](https://zhuanlan.zhihu.com/p/555502485)

在windows上使用MSYS2时, 想把默认的bash换掉
查看安装的 `shell/etc/shells`

```bash
cat /etc/shells
#
# /etc/shells
#
/usr/bin/zsh    //有zsh
/bin/zsh
...

# End of file
```

在msys2安装目录 `D:\msys64下/etc/nsswitch.conf` 文件中添加这行代码:

```conf
db_shell: /bin/zsh
```

现在通过 `msys.exe` 启动就是默认 `zsh`.

启动后, 如果你的msys的shell依然没有改变,
那么你大概不是直接打开的 `msys.exe`, 而是通过msys提供的快捷方式!
这个msys提供的默认快捷方式是通过安装目录下的 `msys2_shell.cmd` 来启动msys的,
在 `msys2_shell.cmd` 中有这样一句:

```cmd
4 | ...
5 | set "LOGINSHELL=bash" //将bash替换为zsh即可改变快捷方式启动后的shell
6 | ...
```

将其中的 `bash` 替换为 `zsh` 即可改变通过快捷方式启动后的shell

### 命令行补全

[bash命令补全工具bash-completion](https://blog.csdn.net/ChaITSimpleLove/article/details/109440590)

```bash
pacman -S bash-completion
```

安装完成 /etc目录会出现一个bash_completion文件;

在用户的shell中运行;

```bash
source /etc/bash_completion
```

也可以将其加入配置文件中;

编辑文件, `vim /etc/bash.bashrc`

```bashrc
# enable bash completion in interactive shells
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
```

## 安装编译环境

[MSYS2安装gcc, make环境](https://www.jianshu.com/p/04636461341e)
[windows上msys2配置及填坑](https://hustlei.github.io/2018/11/msys2-for-win.html)

使用过archlinux的应该会知道, pacman在安装的时候, 如果源没有设置好, 下载是很慢的.

需要修改的文件是:

```conf
\etc\pacman.d\mirrorlist.mingw32
\etc\pacman.d\mirrorlist.mingw64
\etc\pacman.d\mirrorlist.msys
```

这三个文件
镜像源我推荐使用下面的这两个:

[清华大学](https://mirrors.tuna.tsinghua.edu.cn/)
[中国科学技术大学](http://mirrors.ustc.edu.cn/)

### pacman基本命令

```bash
pacman -Sy 更新软件包数据
pacman -Syu 更新所有
pacman -Ss xx 查询软件xx的信息
pacman -S xx 安装软件xx
pacman -R xx 删除软件xx
```

## 安装gcc, g++编译器

随便哪个msys2 子环境都可以, 只是安装好后, 只能在对应的环境下运行.

### 安装make编译器

```bash
#查询并找到msys/make
pacman -Ss make
#安装
pacman -S msys/make

# 安装 msys 版本
pacman -S msys/gcc
```

查看可用的安装包

```bash
pacman -Ssq gcc #或者pacman -Sl grep gcc
```

可以看到以下三个

mingw32 mingw-w64-i686-gcc
mingw64 mingw-w64-x86_64-gcc
msys gcc

分别对应于 msys 的三个环境.

### 安装 mingw-w64-ucrt 工具链

```bash
pacman -S mingw-w64-ucrt-x86_64-gcc mingw-w64-ucrt-x86_64-gcc-fortran mingw-w64-ucrt-x86_64-gcc-libgfortran
 mingw-w64-ucrt-x86_64-gcc-libs mingw-w64-ucrt-x86_64-perl
```

### 安装 mingw-w64-x86_64 工具链

不论你在哪个环境下安装,
MSYS2都会将 `mingw-w64-x86_64-gcc` 安装在 `msys64/mingw64` 下.
从之前的分析可知只有在 mingw64 环境下才能使用这个目录下的程序.
在其他两个环境下虽然能够安装mingw-w64-x86_64-gcc, 但是不能使用mingw-w64-x86_64-gcc.

### 安装编译工具链

```bash
pacman-key --init
# 安装 mingw64 版本
pacman -Syu
pacman -S mingw-w64-x86_64-gcc mingw-w64-x86_64-gdb
pacman -S mingw-w64-x86_64-cmake mingw-w64-x86_64-extra-cmake-modules
pacman -S mingw-w64-x86_64-make
pacman -S mingw-w64-x86_64-toolchain
```

### pacman 的配置

编辑 `/etc/pacman.d/mirrorlist.mingw32`, 在文件开头添加:

```conf
Server = https://mirrors.tuna.tsinghua.edu.cn/msys2/mingw/i686
```

编辑 `/etc/pacman.d/mirrorlist.mingw64`, 在文件开头添加:

```conf
Server = https://mirrors.tuna.tsinghua.edu.cn/msys2/mingw/x86_64
```

编辑 `/etc/pacman.d/mirrorlist.msys`, 在文件开头添加:

```conf
Server = https://mirrors.tuna.tsinghua.edu.cn/msys2/msys/$arch
```

然后执行 pacman -Sy 刷新软件包数据即可.

## MSYS2 目录映射问题

[windows上msys2配置及填坑](https://hustlei.github.io/2018/11/msys2-for-win.html)

### MSYS2 ln -s软连接会复制目录的问题

MSYS2在windows上用ln -s dir创建软连接时, 会复制所有文件到目标文件夹.

解决这个问题需要在/etc/profile文件里面加上一个关键变量:

```bash
export MSYS="winsymlinks:lnk"
```

添加后创建的目录软连接, 就和linux上很类似了.
直接cd就能进入被连接的目录文件夹, 非常方便.

### fastab配置文件目录的方法

在fstab中配置也可以映射目录, 个人更喜欢用ln -s软连接.

在/etc/fstab配置文件目录映射的方法:

直接在/etc/fstab后加入如下代码, 然后重启msys2就可以了

```bash
C:\Users\adminstrator\Desktop /desktop
#目录路径中不能有空格. 如果目录路径中有空格请使用转义字符"\040"代替
```

上述命令配置完成后, 在终端 `cd /desktop` 后可以直接切换到 `C:\Users\adminstrator\Desktop` 目录下.

## pacman 管理包

[pacman的包管理命令](https://blog.csdn.net/qq_41601836/article/details/106519865)

```bash
sudo pacman -Qdt #查询所有孤儿软件, 不再被需要的.
sudo pacman -Qdtq #查询所有不再被依赖的包名
# 查询孤儿软件并删除掉他们
sudo pacman -R $(sudo pacman -Qdtq)
```

```bash
pacman -Sw # 包名: 只下载包, 不安装.
pacman -Sc # 清理未安装的包文件, 包文件位于 /var/cache/pacman/pkg/ 目录
pacman -Scc # 清理所有的缓存文件
```

## perl cpan, cpanm, cpanp

[Error: sys/wait.h: No such file or directory](https://stackoverflow.com/questions/18013950/error-sys-wait-h-no-such-file-or-directory)

msys2 默认没有 `perldoc` 命令

```bash
pacman -S perl-doc
```

安装 cpan 的其他包管理器

```bash
cpan -i -T App::cpanminus
cpan -i -T App::pmuninstall
```

`-t` 表示运行测试. `-T` 表示不运行测试.

msys2 安装 [List::Util][] 模块需要在 msys2 环境下运行,
因为编译过程使用了 `<sys/wait.h>`.
这些是 OS specific 文件, 但不是 Windows 头文件.
Windows 并不完全支持 POSIX API.
您可能需要对代码进行一些移植, 才能使其在 Windows 上编译和运行.

`List::Util` 依赖于 `crypt.h`, 使用 pacman 安装

```bash
pacman -S msys/libcrypt-devel
```

[List::Util]: https://metacpan.org/pod/List::Util

## 各种环境的区别

[在 msys2 中的 mingw64 ,  ucrt64 ,  clang64 的区别与相同点有啥?](https://www.zhihu.com/question/463666011/answer/1927907983)

`clang` 和 `gcc` 是两个不同的 C/C++ 编译器, 而 `mingw-w64` 是一个 Windows 上的编译和运行时环境.
注意, `mingw-w64` 本身并不是一个编译器, 而是一组库, 头文件和实用工具.
gcc 需要 `mingw-w64` 环境才能在 Windows 上编译程序,
加上最初(现已过时)的 mingw 项目就是专为 gcc 设计的,
因此通常用 mingw64 代指 64 位的 gcc 和 mingw-w64 环境.

mingw64, ucrt64, clang64 都是 Windows 原生程序(不依赖 cygwin.dll),
不过 mingw64 是很早就有的, 后两者是最近(本回答最初写于2021年6月)才新加的,
~~所以只是选一个用的话就 mingw64 就没问题.~~(划掉)
2022年11月25日更新: 现在 msys2 官方推荐优先选择 ucrt64,
经过一年多时间, 现在 ucrt64 环境已经非常稳定了, 而且对 UTF-8 语言环境的支持更好.

后两者先后刚出来时我还查过, 简而言之, 这三者的区别是:
mingw64 与 ucrt64 都是用 gcc 编译器编译的 Windows 64 位程序,
只不过它们链接到的 crt(C runtime)不同,
mingw64 是链接到了 msvcrt ,
而 ucrt64 则是链接到了 Windows 上新的 ucrt 上.
而 clang64 很好理解, 就是用 clang 而非 gcc 来编译各种库.
另外它也是链接到了 ucrt 而非 msvcrt.
三者是共同点是, 它们都需要 mingw-w64 环境来进行编译.

以 zstd 为例:

![zstd](https://picx.zhimg.com/80/v2-2f6ed84eb78c2cfae0d64a30662bb152_720w.webp?source=1def8aca)

图中的 `/usr/bin/zstd` 就是依赖 cygwin 的非原生程序,
这里的 `msys-2.0.dll` 实际上就是 msys2 版的 `cygwin.dll`
如果你了解 `archlinux` 的打包的话,
如果不了解可以在 [archwiki](https://link.zhihu.com/?target=https%3A//wiki.archlinux.org/title/Creating_packages) 了解一下,
也可以从这几个包的打包脚本(PKGBUILD)来观察区别.
msys2 的打包脚本可以在 packages.msys2.org 在线查看.

在这个网站上搜索 zstd 得到这几个 zstd 包的构建脚本,
发现这几个 Windows 原生的包其实是共用同一个构建脚本的.
从 `mingw_arch` 变量来看, 还有几个32位的包和 arm64 的包也是用这个打包脚本.

只不过它会根据不同的 `MINGW_PACKAGE_PREFIX` 环境变量来执行不同的构建工具,
构建出不同的包.

```makefile
mingw_arch=('mingw32' 'mingw64' 'ucrt64' 'clang64' 'clang32' 'clangarm64')
makedepends=("${MINGW_PACKAGE_PREFIX}-gcc"
             "${MINGW_PACKAGE_PREFIX}-ninja"
             "${MINGW_PACKAGE_PREFIX}-cmake")
```

这里是 msys2 最初关于添加 ucrt64 与 clang64 构建的讨论: [issue 6901](https://link.zhihu.com/?target=https%3A//github.com/msys2/MINGW-packages/issues/6901)

## msys2 oh-my-zsh 卡顿 解决方法

[去掉oh_my_zsh主题自带的git检查](https://www.cnblogs.com/brady-wang/p/14356525.html)

修改 `.zshrc` 中的主题名称为 `ymy`,

```rc
ZSH_THEME="ymy"
```

然后在以下位置新建 `ymy` 主题

```bash
vim ~/.oh-my-zsh/themes/ymy.zsh-theme
```

内容为

```bash
PROMPT="%(?:%{$fg_bold[green]%}%1{➜%} :%{$fg_bold[red]%}%1{➜%} ) %{$fg[cyan]%}%c%{$reset_color%}"
PROMPT+=' '

#PROMPT="%(?:%{$fg_bold[green]%}%1{➜%} :%{$fg_bold[red]%}%1{➜%} ) %{$fg[cyan]%}%c%{$reset_color%}"
#PROMPT+=' $(git_prompt_info)'

#ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}git:(%{$fg[red]%}"
#ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
#ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}%1{✗%}"
#ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"
```

也就是把 `git` 检查相关的部分注释掉.
