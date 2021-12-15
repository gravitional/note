# mac os 软件安装和设置

## mac 快捷键设置

[在 Mac 上使用全局键盘快捷键](https://support.apple.com/zh-cn/guide/mac-help/mchlp2262/10.15/mac/10.15)
您只能为现有菜单命令创建键盘快捷键. 您不能为通用任务定义键盘快捷键, 例如: 打开一个 `App` 或在 `App` 之间切换.

妙控键盘的控制键顺序是 `contrl`,`option`,`command`, 把键位更改成这个顺序, 大部分使用默认的快捷键, 加上少部分修改:

+ 切换输入为`opt+space`
+ `spotlight`的快捷键为`opt+s`
+ `Show launchpad`的快捷键改成`F12`
+ `显示桌面`为`F11`,在`设置-调度中心`修改.
+ 切换全屏的快捷键为`^+cmd+F`: `Enter Full Screen`, `Exit Full Screen`
+ 锁定屏幕快捷键`^+cmd+Q`

***
`vscode`常用的快捷键,

+ 插入多个光标变成`cmd+option+up`, 可以在`Selection`里面选择鼠标插入的修饰键`alt`or`ctrl`
+ 把`markdown`切换代码环境--`toggle code block`的快捷键设置为`^+k ^+b`
+ 把补全提示--`trigger Suggest`的快捷键设置为`^+space`.
+ `go last`->`cmd+end`
+ 跳转到文档开头--`cmd+up`,跳转到文档末尾--`cmd+down`

[Keyboard shortcuts for macOS](https://code.visualstudio.com/shortcuts/keyboard-shortcuts-macos.pdf)

***
mathematica

查找帮助的快捷键: `Find Selected Function`--`F1`
清除所有输出的快捷键: `Delete All Output`--`^+L`

***
`终端` or `iterm2` 中大部分常用的快捷键,移动,清屏等等与`linux` 相同, 是在`readline`库中约定的.
[Bash 行操作](https://wangdoc.com/bash/readline.html)

***
Finder中显示隐藏文件`Cmd+Shift+.`

`mac` 中询问窗口, 焦点的移动方式仍然是按下`tab`, 但是取消操作需要按下`space`,不管在什么情况下, `enter`都是确定.

## homebrew,软件管理

`brew Shellenv`: 打印 export 设置. 在 `shell` 中运行时, 将 `Homebrew` 的安装路径添加到`PATH`, `MANPATH`和`INFOPATH`.
变量`HOMEBREW_PREFIX`, `HOMEBREW_CELLAR`和`HOMEBREW_REPOSITORY`也被 `export` 以避免多次查询.
考虑将该命令的输出添加配置文件中(例如`~/.profile, ~/.bash_profile, or ~/.zprofile`), 例如: `eval "$(/opt/homebrew/bin/brew shellenv)"`

***
uninstall/卸载brew: 运行  `git` 仓库中的`uninstall.sh`脚本即可, 但是其中有一个`raw github` 地址, 需要更改一下.

卸载之后打开终端时, 可能会报错:

```
zsh problem: compinit:503: no such file or directory: /usr/local/share/zsh/site-functions/_brew
```

进入`/usr/local/share/zsh/site-functions/`, `rm _brew _brew_cask`即可.

***
brew 命令补全: 在`.zshrc`文件附上

```bash
if type brew &>/dev/null; then
FPATH=$(brew --prefix)/share/zsh-completions:$(brew --prefix)/share/zsh/site-functions:$FPATH
autoload -Uz compinit
compinit
fi
```

其中`/share/zsh-completions`是插件`zsh-completions`补全函数的位置.

### brew 国内源

建议按照[Homebrew / Linuxbrew 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/homebrew/)中的教程安装
以及 [Homebrew-bottles 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/homebrew-bottles/)
或者参考使用中科大镜像的 `git` 备份, [ineo6/homebrew-install](https://github.com/ineo6/homebrew-install)

如果没有使用过`homebrew/cask/sublime-text`下面的软件,
可能没有`/opt/homebrew/Library/Taps/homebrew/homebrew-cask`这个目录, 随便安装一个`cask`软件, 再设置即可.

### Bottles

+ 地址: https://mirrors.ustc.edu.cn/homebrew-bottles/
+ 说明: Homebrew 预编译二进制软件包
+ 收录仓库: homebrew/homebrew-core
+ 使用说明: 请在运行 `brew` 前设置环境变量 `HOMEBREW_BOTTLE_DOMAIN`, 值为 `https://mirrors.ustc.edu.cn/homebrew-bottles`.
+ 临时替换:

    ```bash
    export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.ustc.edu.cn/homebrew-bottles"
    ```

+ 永久替换, 在shell 配置文件中添加:

    ```bash
    # 对于 bash 用户, ~/.bash_profile
    export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.ustc.edu.cn/homebrew-bottles"
    # 对于 zsh 用户, ~/.zshrc
    export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.ustc.edu.cn/homebrew-bottles"
    ```

>注解:
>Linuxbrew 核心仓库(`linuxbrew-core`)自 2021 年 10 月 25 日(brew 版本 3.3.0 起)被弃用,
>Linuxbrew 用户应迁移至 `homebrew-core`.  Linuxbrew 用户请依本镜像说明重新设置镜像.

### Core 源使用帮助

+ 地址: https://mirrors.ustc.edu.cn/homebrew-core.git/
+ 说明: Homebrew 核心软件仓库
+ 临时使用 USTC 镜像:

    ```bash
    export HOMEBREW_CORE_GIT_REMOTE="https://mirrors.ustc.edu.cn/homebrew-core.git"
    brew update
    ```

+ 永久替换, 在shell 配置文件中添加:

    ```bash
    # 对于 bash 用户, ~/.bash_profile
    export HOMEBREW_CORE_GIT_REMOTE="https://mirrors.ustc.edu.cn/homebrew-core.git"
    # 对于 zsh 用户, ~/.zshrc
    export HOMEBREW_CORE_GIT_REMOTE="https://mirrors.ustc.edu.cn/homebrew-core.git"
    ```

重置为官方地址:

```bash
unset HOMEBREW_CORE_GIT_REMOTE
brew tap --custom-remote homebrew/core https://github.com/Homebrew/homebrew-core
```

>注解:

若出现 `Error: invalid option: --custom-remote` 错误, 请先运行 `brew update` 将 brew 更新至 3.2.17 或以上版本.
重置回默认远程后, 用户应该删除 `shell` 的 `profile` 设置中的环境变量 `HOMEBREW_CORE_GIT_REMOTE` 以免运行 `brew update` 时远程再次被更换.

注解

Linuxbrew 核心仓库(linuxbrew-core)自 2021 年 10 月 25 日(brew 版本 3.3.0 起)被弃用, Linuxbrew 用户应迁移至 homebrew-core.
Linuxbrew 用户请依本镜像说明重新设置镜像. 注意迁移前请先运行 brew update 将 brew 更新至 3.3.0 或以上版本.
迁移过程中若出现任何问题, 可使用如下命令重新安装 homebrew-core:

```bash
export HOMEBREW_CORE_GIT_REMOTE="https://mirrors.ustc.edu.cn/homebrew-core.git"
rm -rf "$(brew --repo homebrew/core)"
brew tap --custom-remote --force-auto-update homebrew/core https://mirrors.ustc.edu.cn/homebrew-core.git
```

### Cask

+ 地址: https://mirrors.ustc.edu.cn/homebrew-cask.git/
+ 说明: Homebrew cask 软件仓库, 提供 macOS 应用和大型二进制文件
+ 使用说明: 使用 USTC 镜像安装, 或将已安装的仓库远程替换为 USTC 镜像:

    ```bash
    brew tap --custom-remote --force-auto-update homebrew/cask https://mirrors.ustc.edu.cn/homebrew-cask.git
    ```

    > 注解:若出现 `Error: invalid option: --custom-remote` 错误, 请先运行 `brew update` 将 `brew` 更新至 3.2.17 或以上版本.

+ 重置为官方地址:

    ```bash
    brew tap --custom-remote --force-auto-update homebrew/cask https://github.com/Homebrew/homebrew-cask
    ```

    >注解: Caskroom 的 `Git` 地址在 2018 年 5 月 25 日从 https://github.com/caskroom/homebrew-cask 迁移到了 https://github.com/Homebrew/homebrew-cask .

### Cask Versions 源使用帮助

+ 地址: https://mirrors.ustc.edu.cn/homebrew-cask-versions.git/
+ 说明: Homebrew cask 其他版本 (alternative versions) 软件仓库, 提供使用人数多的, 需要的版本不在 cask 仓库中的应用.
+ 使用说明: 使用 USTC 镜像安装, 或将已安装的仓库远程替换为 USTC 镜像:

    ```bash
    brew tap --custom-remote --force-auto-update homebrew/cask-versions https://mirrors.ustc.edu.cn/homebrew-cask-versions.git
    ```

>注解: 若出现 `Error: invalid option: --custom-remote` 错误, 请先运行 `brew update` 将 brew 更新至 3.2.17 或以上版本.

    重置为官方地址:

    ```bash
    brew tap --custom-remote --force-auto-update homebrew/cask-versions https://github.com/Homebrew/homebrew-cask-versions
    ```

### brew miss bottles 报错

[HOMEBREW_BOTTLE_DOMAIN需要指定到/bottles这一级路径](https://github.com/tuna/issues/issues/1224#issuecomment-818728498)

`brew Release 3.0.7`更新之后, 去除了对`/bottles`这级路径的拼接, 目前需要自己修改`HOMEBREW_BOTTLE_DOMAIN`的地址:

   HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles/bottles

等待后续完成迁移即可.

也可以先切换成`ustc`的源:

[Bottles 源使用帮助](http://mirrors.ustc.edu.cn/help/homebrew-bottles.html)

```bash
echo 'export HOMEBREW_BOTTLE_DOMAIN=https://mirrors.ustc.edu.cn/homebrew-bottles' >> ~/.zshrc
source ~/.zshrc
```

## brew,sspai

[在 M1 芯片 Mac 上使用 Homebrew](https://sspai.com/post/63935)

为什么 ARM 版 Mac 要使用 `/opt `路径?

根据<文件系统层次结构标准>(Filesystem Hierarchy Standard, 主要为 Linux 系统制定, 但对具有共同 UNIX 基因的 macOS 也有参考价值):
`/usr/local` 目录用于系统管理员在本地安装软件. 系统软件更新时, 该目录应免于被覆盖.
`/opt` 目录留作附加应用程序(add-on application)软件包的安装. 安装在该目录下的软件包必须将其静态文件放置在单独的 `/opt/<package>` 或` /opt/<provider>` 路径下.
历史上, `/usr/local` 主要用于放置在本地编译并另行安装的程序, 避免和` /usr `下的系统自带版本冲突;而 `/opt` 则用于安装非系统自带的, 第三方预先编译并发行的独立软件包.
显然, 在如今的 macOS 使用场景下, 用户很少会需要自行编译软件包, `/usr/local` 和 `/opt` 的区分一定程度上已经成为名义上的了.
Homebrew 启用 `/opt` 作为 ARM 版的安装路径, 可能更多是出于确保与 X86 版相互区隔的考虑.

***
`Homebrew` 是一款`Mac OS`平台下的软件包管理工具,拥有安装, 卸载, 更新, 查看, 搜索等很多实用的功能.`Homebrew` 的两个术语:

`Formulae` :软件包,包括了这个软件的依赖, 源码位置及编译方法等;
`Casks`:已经编译好的应用包,如图形界面程序等. 现在的版本中,`brew cask install` 已经过时了,建议使用`brew install [--cask]`.

***
`Homebrw`相关的几个文件夹用途:

`bin` :用于存放所安装程序的启动链接(相当于快捷方式)
`etc` :`brew` 安装程序的配置文件默认存放路径
`Library`:`Homebrew` 系统自身文件夹
`Cellar` :通过 `brew` 安装的程序将以 `[程序名/版本号]` 存放于本目录下

`X86` 版 `Homebrew` 无法在 `ARM` 环境下安装.为此,需要先启动一个 `X86` 环境的终端.
网络上传播较广的方法是创建一个 `Terminal.app` 的副本,然后令其在 `Rosetta` 兼容模式下运行,显得有些麻烦.

其实,注意到在任何命令前增加 `arch -x86_64`,就可以以 `x86` 模式运行该命令.因此,运行:

`arch -x86_64 $SHELL`, 就可以启动一个 `x86` 模式终端,使得之后运行的命令都在 X86 模式下运行.

此时,运行 `Homebrew` 的官方安装脚本

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

就可以完成 `X86` 版 `Homebrew` 的安装.

### 常用软件的安装

使用`brew info zsh-completions`, 可以查看安装完需要进行的操作,比如配置路径等. `gnu`的软件会多一个`g`开头.

```bash
brew install  gettext libunistring libidn2 openssl wget # 前面的是依赖
brew install zsh-completions zsh-autosuggestions brew-cask-completion
brew install coreutils rename pcre grep
brew info tcl-tk git-gui # gitk 命令
brew install ncurses; brew install htop # 更好用的 top
brew install libevent utf8proc ; brew install tmux # 终端复用器
brew install visual-studio-code lyx # 文字编辑器
brew install findutils # find locate xargs
```

为了使安装的软件能利用`zsh`的补全功能, 需要添加 `brew` 补全函数的路径:
[在 Mac OS X 系统下为 Brew 开启 Zsh 补全功能](https://tommy.net.cn/2015/02/24/enable-zsh-completion-of-brew-under-mac-os-x/). 在`.zshrc`中添加

```bash
# brew 的 zsh 补全, 依情况更改路径
fpath=(/opt/homebrew/share/zsh/site-functions $fpath)
compinit
```

### arhc 指定软件架构

`arch` -- 打印 `architecture` type 或者运行指定`architecture`的`universal binary`.

语法

```bash
arch
arch [-32] [-64] [[-arch_name | -arch arch_name]...] [-c] [-d envname]... [-e envname=value]... [-h]
   prog [args ...]
```

描述

不带参数使用的`arch`命令将显示计算机的`architecture`类型.

arch命令的另一个用途是运行通用二进制文件的选定体系结构.`universal binary`--通用二进制文件包含可以在不同体系结构上运行的代码.
默认情况下,操作系统将选择与处理器类型最匹配的架构. 如果处理器是`64`位,优先使用`64`位架构,而`32`位架构只能在在32位处理器上运行.

当最自然的架构不可用时,操作系统将尝试选择其他架构.在64位处理器上,尝试使用32位体系结构.否则,将不运行任何体系结构,并且报错.
`arch`命令可用于更改操作系统的正常选择顺序.最普遍的用来在64位处理器上选择32位架构,即使有现成的`64`位架构版本.

`arch_name`参数必须是当前受支持的架构之一:

+ `i386`     32-bit intel
+ `x86_64`   64-bit intel
+ `x86_64h`  64-bit intel (haswell)
+ `arm64`    64-bit arm
+ `arm64e`   64-bit arm (Apple Silicon)

### brew and brew cask 的区别

[brew install 和 brew cask install 的区别](https://zhuanlan.zhihu.com/p/138059447)

`Homebrew` 提供了两种安装软件的方式,`brew install` 和 `brew cask install`,下面对两种方式进行一些解释说明.

`brew` 是下载源码解压,然后 `./configure && make install` ,同时会包含相关依存库,并自动配置好各种环境变量.
对于对程序员只需通过简单的指令,就能快速安装和升级本地的各种开发环境,非常快捷方便.

`brew cask` 是针对已经编译好了的应用包(`.dmg/.pkg`)下载解压,然后放在统一的目录中(`Caskroom`),省掉了自己下载, 解压, 安装等步骤.
这个对一般用户来说会比较方便,包含很多在 `AppStore` 里没有的常用软件.

现在的版本中,`brew cask install` 已经过时了,建议使用`brew install [--cask]`.

简单来说,

`brew install` 用来安装一些不带界面的命令行工具和第三方库.
`brew install [--cask]` 用来安装一些带界面的应用软件.

### 常用的brew命令

[HomeBrew 管理第三方应用](https://sspai.com/post/43451)

+ 查看`brew`版本:`brew -v`
+ 更新`brew`版本:`brew update`
+ 本地软件库列表:`brew list`
+ 查看软件库版本:`brew list --versions`
+ 查找软件包:`brew search xxx` (xxx为要查找软件的关键词)
+ 安装软件包:`brew install xxx` (xxx为软件包名称)
+ `brew info `: 显示某个软件的信息, 不带参数时显示总的统计
+ 卸载软件包:`brew uninstall xxx`
+ 安装图形界面软件:`brew install [--cask] xxx`(xxx为软件名称)
+ 卸载图形界面:`brew uninstall [--cask] xxx`
+ 查找软件安装位置:`which xxx` (xxx为软件名称)
+ 更新应用和清理旧版: `brew outdated`
+ 更新所有应用: `brew upgrade`
+ 更新特定应用: `brew upgrade 应用名`
+ 删除应用的旧版本和缓存: `brew cleanup`,`brew cleanup -n` 不实际运行,`brew cleanup 应用名` 只清理特定应用的缓存
+ 访问应用官网: `brew home 应用名`

## zsh 配置

***
`zsh-syntax-highlighting`,语法高亮

[最漂亮( iTerm2+oh-my-zsh配色)](https://www.jianshu.com/p/246b844f4449)
[zsh-syntax-highlighting/INSTALL.md](https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/INSTALL.md)

需要使`zsh-syntax-highlighting`是最后被`sourced`的插件.

`Oh-my-zsh` 下的安装

克隆到`oh-my-zsh`的插件仓库:

```bash
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
```

在`~/.zshrc`配置文件中激活插件:

```bash
plugins=( [plugins...] zsh-syntax-highlighting)
```

重启`zsh`.

***
zsh-completions,代码补全插件
[zsh-users/zsh-completions](https://github.com/zsh-users/zsh-completions)

克隆到 Clone the repository inside your oh-my-zsh repo:

```bash
git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions
```

在 `.zshrc` 中开启:

  plugins=(...  zsh-completions)
  autoload -U compinit && compinit

### 终端配置

[Mac下Alt键配置](https://blog.csdn.net/lzghxjt/article/details/80957474)
[Bash 行操作](https://wangdoc.com/bash/readline.html)

打开`iTerm2`终端, 按快捷键`Command+,`, 打开配置界面.
选择`Profiles`,在左侧选择当前配置, 在右侧选择`Keys`选项卡下两个`Esc+`, 即可将`option`键作为`alt`键使用.

`Bash` 内置了 `Readline` 库, 具有这个库提供的很多`行操作`功能, 比如命令的自动补全, 可以大大加快操作速度.
这个库默认采用 `Emacs` 快捷键, 也可以改成 `Vi` 快捷键.

```bash
$ set -o vi
```

下面的命令可以改回 `Emacs` 快捷键.

```bash
$ set -o emacs
```

如果想永久性更改编辑模式(`Emacs/Vi`), 可以将命令写在`~/.inputrc`文件, 这个文件是 `Readline` 的配置文件.

```bash
set editing-mode vi
```

本章介绍的快捷键都属于 `Emacs` 模式.`Vi` 模式的快捷键, 读者可以参考 Vi 编辑器的教程.

`Bash` 默认开启这个库, 但是允许关闭.

```bash
$ bash --noediting
```

上面命令中, `--noediting` 参数关闭了 `Readline` 库, 启动的 `Bash` 就不带有行操作功能.

***
光标移动

`Readline` 提供快速移动光标的快捷键.

+ `Ctrl+a`:移到行首.
+ `Ctrl+b`:向行首移动一个字符, 与左箭头作用相同.
+ `Ctrl+e`:移到行尾.
+ `Ctrl+f`:向行尾移动一个字符, 与右箭头作用相同.
+ `Alt+f`:移动到当前单词的词尾.
+ `Alt+b`:移动到当前单词的词首.
上面快捷键的 `Alt` 键, 也可以用 `ESC` 键代替.

***
清除屏幕

`Ctrl+l`快捷键可以清除屏幕, 即将当前行移到屏幕的第一行, 与`clear`命令作用相同.

***
编辑操作

下面的快捷键可以编辑命令行内容.

`Ctrl+d`:删除光标位置的字符(delete).
`Ctrl+w`:删除光标前面的单词.
`Ctrl+t`:光标位置的字符与它前面一位的字符交换位置(transpose).
`Alt+t`:光标位置的词与它前面一位的词交换位置(transpose).
`Alt+l`:将光标位置至词尾转为小写(lowercase), `alt+l`可能被占用, 尝试使用`alt+shift+l`.
`Alt+u`:将光标位置至词尾转为大写(uppercase). `alt+u`可能被占用, 尝试使用`alt+shift+U`.
使用`Ctrl+d`的时候, 如果当前行没有任何字符, 会导致退出当前 `Shell` , 所以要小心.

剪切和粘贴快捷键如下.

`Ctrl+k`:剪切光标位置到行尾的文本.
`Ctrl+u`:剪切光标位置到行首的文本.
`Alt+d`:剪切光标位置到词尾的文本.
`Alt+Backspace`:剪切光标位置到词首的文本.
`Ctrl+y`:在光标位置粘贴文本.
同样地, `Alt`键可以用`Esc` 键代替.

***
其他快捷键

`Ctrl+j`: 等同于回车键(LINEFEED).
`Ctrl+m`: 等同于回车键(CARRIAGE RETURN).
`Ctrl+o`: 等同于回车键, 并展示操作历史的下一个命令.
`Ctrl+v`: 将下一个输入的特殊字符变成字面量, 比如回车变成`^M`.
`Ctrl+[`: 等同于 `ESC`.
`Alt+.`: 插入上一个命令的最后一个词.
`Alt+_`: 等同于`Alt+.`.
上面的`Alt+.`快捷键, 对于很长的文件路径, 有时会非常方便.因为 Unix 命令的最后一个参数通常是文件路径.

### 颜色配置

[彩色的Shell](https://www.cnblogs.com/bamanzi/p/colorful-shell.html)

`ls --color`

大多数情况下, 你执行 `ls --color`  就可以了, 它会用彩色来区分不同的文件类型．
你可以添加 `alias ls='ls --color=auto'`  到你的 `~/.zshrc` . `auto`这个值可以确保你将`ls`的输出重定向到一个文件时不会产生问题．

如果你想要详细了解这个功能, 可以阅读`coreutils`文档里面的相应章节:
[GNU Coreutils: General output formatting](http://www.gnu.org/software/coreutils/manual/html_node/General-output-formatting.html#General-output-formatting),
[GNU Coreutils: dircolors invocation](http://www.gnu.org/software/coreutils/manual/html_node/dircolors-invocation.html#dircolors-invocation). (注意配置文件的详细说明其实在 `dircolors --print-database`  这个命令的输出里面)．

这里还有一个在线配置工具: [LSCOLORS Generator](https://geoff.greer.fm/lscolors/) －－不过我从来没去配过细节．

## 命令行下的系统配置

[5 款小工具,轻松解锁 Mac 隐藏功能](https://sspai.com/post/45668)

TinkerTool 是一款专注于管理系统设置的免费应用.
借助它,我们可以对 macOS 的访达, 程序坞, Launchpad 等不同组件的设置进行更改.你可以在[官网](https://www.bresink.com/osx/TinkerTool.html) 下载 `TinkerTool`.

打开 `TinkerTool` 后可以看到,它分为`Finder`, `Dock`, `Launchpad`等不同的标签页.
在`Finder`标签页中,勾选`Finder options`中的最后一行,`Show selected path in window title`.
经过这样的设置,当我们在访达中打开文件时,标题栏会显示我们所处的路径.当文件夹层级较多时,开启这个功能可以帮你更方便地定位自己的文件.

### mas,批量更新macOS软件

[终端上的 Mac App Store mas](https://sspai.com/post/40382)
[mas-cli/mas](https://github.com/mas-cli/mas)

如果你希望安装 `Mac App Store` 上的应用,也可以绕过原生的商店应用,直接采用终端进行安装.
你可以先通过刚才安装的 `Homebrew` 安装一个我们需要的 `mas` ,即在终端输入:

```bash
brew install mas
```

### 查询与安装应用

Mac App Store 中每一个应用都有自己的应用识别码(Product Identifier),这可以在每个应用的链接中看到.
`mas` 就是根据 `Product Identifier `安装与更新应用,也提供了查询应用 `ID` 的命令.

由 `1Password` 的链接可知其识别码为 `443987910`

   https://itunes.apple.com/cn/app/1password/id443987910?mt=12

除了查看链接,有以下 `x` 种方法获取应用的识别码:

+ 用命令 `mas search` 关键词 查询应用.比如在终端中执行 `mas search xcode`;
+ 用命令 `mas list` 查询已安装应用及其识别码.

```bash
mas search Xcode & mas list
```

安装应用只需知道此应用的识别码就可以安装具体软件.比如安装 `Bear`,流程如下:
第一步: 由命令 `mas search bear` 得知应用 `Bear` 的识别码为 `1091189122`;
第二步: 使用命令 `mas install 1091189122` 安装.

>注意:
>应用必须在商店登陆账号的已购列表中,因为命令行无法完成`购买`这个操作;
>对于新上架的应用,可能无法查询到其`识别码`.因为 `mas` 的查询列表在缓存文件中,目前尚不清楚其列表更新周期,
但若由其他途径(如`应用链接`)>得知新上架`应用识别码`,仍可正常安装.

我们不仅可以使用命令行安装单个应用,还可以批量安装应用,只需在应用识别码之间加上空格:

```bash
mas install 甲应用识别码 乙应用识别码 丙应用识别码
```

### 更新应用

如果要更新所有 `Mac App Store` 应用,只需终端执行一句命令:

```bash
mas upgrade
```

如果更新特定应用,需要使用命令 `mas outdated` 先查询待更新列表以获取应用识别码,再更新一个或几个应用:

```bash
mas upgrade 甲应用识别码
mas upgrade 甲应用识别码 乙应用识别码 丙应用识别码
```

但要注意, `mas` 无法用于系统更新,即只能更新显示在 `Mac App Store` 中的应用.
但可以使用命令 `softwareupdate -l` 获取系统更新列表,然后使用 `sudo softwareupdate -iRa` 进行更新. `-R`表示重启.

### 切换 MacAppStore 账号

这是多区账号拥有者的福音,我们终于可以更方便地下载和更新其他区的应用了.
如果忘记了当前帐号,使用命令 `mas account` 查询.可用命令 `mas signout` 退出当前帐号,并按如下命令登陆新的账号:

```bash
mas signin Apple ID "密码" # 如: mas signin mas@example.com "mypassword"
```

也可以设置命令别名以得到更爽快的体验,在隐藏文件 `.bashrc` 中添加以下内容: 其路径为 `~/.bashrc`,同时按 `shift + command + .` 可显示隐藏文件.

```bash
alias masus='mas signout && mas signin myusappleid "mypassword"'
alias mascn='mas signout && mas signin mycnappleid "mypassword"'
alias mas?='mas account'
```

需重新打开终端以载入设置,那么在终端中执行 `masus` 即可切换到美区帐号,`mascn` 即切到中区, `mas?` 可查询目前登陆帐号.
但如果开启了双重认证,可能遇到错误信息:

   Error: Sign in failed: The operation couldn' t be completed. (mas.MASError error 1.)

关闭双重认证则一切正常,但并不建议这样做,可以考虑关闭非重要帐号的双重认证,但 iOS 10.3 或 macOS Sierra 10.12.4 及更高版本中创建的某些帐户,无法关闭双重认证.
`mas` 团队正在着手处理开启双重认证无法登陆的问题,可去[督促一番](https://github.com/mas-cli/mas/issues/56).

在问题解决之前该怎么办: 在 Mac App Store 中登陆帐号,然后重新打开终端(`Terminal`)即可.

## vs code

### mac设置vscode命令行启动

[mac设置vscode命令行启动](https://blog.csdn.net/flitrue/article/details/90906578)

***
方法一
在 `VS Code`中打开命令面板 (`shift+cmmand+P`),输入`shell command`,找到: `Install 'code' command in PATH`,点击就可以了.
以后在命令行中执行`code`就可以打开`VS Code`

***
方法二

```bash
vim ~/.zshrc
# 将下列内容添加到最后
# 设置vscode启动的简称
alias vscode="你的安装目录/vscode.app/Contents/Resources/app/bin/code"
## 然后 :wq 关闭 ~/.zshrc 文件
source ~/.bash_profile
```
