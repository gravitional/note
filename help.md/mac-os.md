# macos

## 日常

### 命令

[请问有哪些实用的Mac终端命令](https://www.zhihu.com/question/27864961)

***
`say` 让 mac 读出一段文字

`$ sleep 10 && say "hello"`

***
`pbcopy`, 与之对应的是`pbpaste`

把命令行的输出直接复制到粘贴板: 

```bash
$ sort -u | pbcopy
```

把粘贴板内容直接存入一个文件: 

```bash
$ pbpaste > out.txt
```

用 `Finder` 打开当前目录

```bash
$ open .
```

***
`cdf`
可以让 `shell` 跳转到`finder`打开的文件夹, 不过要提前在`.zshrc`中的`plugin`中加入`osx`

***
`ccat`
可以让你的cat文件时候高亮代码
`brew install ccat`

***
`archey`

这是一个查看当前`mac`概览的插件, 可以用`brew`安装. 好处就是, 查看自己`ip`的时候不用输入`ifconfig`然后一行一行去看, 看`mac`的`关于本机`不用再去点菜单栏了.

`brew install archey`

***
`mdfind` 命令查找文件, 类似 `Spotlight` 查询效果的命令行

### 中文字体

查找 `字体册`, `font Book.app`应用.

`macos` 的字体存放在`/System/Library/Fonts`目录下,`macos` 预装的中文字体为,

```bash
Apple LiGothic Medium
Apple LiSung Light
Apple SD Gothic Neo Regular
AppleGothic Regular
AppleMyungjo Regula
Baoli SC Regular
BiauKai Regular
GB18030 Bitmap Regular
Hannotate SC Regular
HanziPen SC Regular
Hei Regular
Heiti TC Light
Hiragino Sans CNS W3
Hiragino Sans GB W3
Kai Regular
Kaiti SC Regular
Lantinghei SC Extralight
Libian SC Regular
LiHei Pro Medium
LingWai SC Medium
LiSong Pro Light
Nanum Gothic Regular
PCMyungjo Regular
PingFang SC Regular
SimSong Regular
Songti SC Regular
STFangsong Regular
STHeiti Light
STKaiti Regular
STSong Regular
Wawati SC Regular
Weibei SC Bold
Xingkai SC Light
Yuanti SC Regular
Yuppy SC Regular
```

### 命令行安装字体

[Sarasa Mono SC](https://github.com/laishulu/Sarasa-Mono-SC-Nerd)

MacOS 用户可以直接通过cask安装: 

```bash
brew tap laishulu/cask-fonts
brew install font-sarasa-nerd
```

如果是安装本地字体文件, 参考[nstalling fonts from terminal](https://apple.stackexchange.com/questions/240381/installing-fonts-from-terminal-instead-of-font-book),
直接复制到对应的文件夹, `/Library/Fonts` (系统公用) or `~/Library/Fonts` (当前用户使用).

### XeLaTeX 使用系统字体

[修复 MacTeX 2015 无法按字体文件名调](https://liam.page/2015/07/11/mactex-2015-system-font/)

`XeLaTeX` 通过字体文件名调用字体需要 `kpathsea` 库的协助. 默认情况下, `kpathsea` 会搜索 `MacTeX` 自己的目录树(TEXMF).
如果希望 `kpathsea` 搜索系统字体目录的话, 还需要配置 `OSFONTDIR` 这个环境变量.
在 `MacTeX 2014` 中, 这个变量是默认配置好了的. 但是在 `MacTeX 2015` 中, 不知为何, 这个变量没有预先配置. 因此, 需要用户自行配置.

具体的配置方法如下: 
编辑文件`/usr/local/texlive/2015/texmf.cnf`, 在文件末尾加上一行: `OSFONTDIR = /Library/Fonts//:~/Library/Fonts//`
保存文件

`macTex`的字体文件在`/usr/local/texlive/2020/texmf-dist/fonts`, 使用`fontspec`和`xetex`时, 可能不会自动搜索`TeXLive` tree, 参考
[Missing Dejavu Font](https://tex.stackexchange.com/questions/314298/missing-dejavu-font)
最简单的方法是直接把需要的字体文件复制到`/Users/tom/Library/fonts`, 或者`/Library/Fonts`目录下, 前者是用户字体文件夹. 使用`字体册.app`安装也可以, 但是速度感人.

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

## homebrew 软件安装

`brew Shellenv`:
Print export statements. 在shell中运行时, 将Homebrew 的安装路径添加到`PATH`, `MANPATH`和`INFOPATH`.
变量`HOMEBREW_PREFIX`, `HOMEBREW_CELLAR`和`HOMEBREW_REPOSITORY`也被导出以避免多次查询.
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

### brew,tuna

建议按照[Homebrew / Linuxbrew 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/homebrew/)中的教程安装
以及 [Homebrew-bottles 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/homebrew-bottles/)

或者参考使用中科大镜像的git备份, [ineo6/homebrew-install](https://github.com/ineo6/homebrew-install)

如果没有使用过`homebrew/cask/sublime-text`下面的软件, 可能没有`/opt/homebrew/Library/Taps/homebrew/homebrew-cask`这个目录, 随便安装一个`cask`软件, 再设置即可.

### brew,sspai

[在 M1 芯片 Mac 上使用 Homebrew](https://sspai.com/post/63935)

为什么 ARM 版 Mac 要使用 `/opt `路径？

根据《文件系统层次结构标准》(Filesystem Hierarchy Standard, 主要为 Linux 系统制定, 但对具有共同 UNIX 基因的 macOS 也有参考价值): 
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

  plugins=(… zsh-completions)
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

### mac 命令行管理

[简单几条命令,轻松开启 macOS 系统隐藏功能 | 新手问号](https://sspai.com/post/41695)

***
配置 `Launchpad`

可以通过终端对 `Launchpad` 排列方式进行修改,复制以下代码至终端即可:

```bash
defaults write com.apple.dock springboard-columns -int 8;
defaults write com.apple.dock springboard-rows -int 7;
defaults write com.apple.dock ResetLaunchPad -bool TRUE;
killall Dock
```

命令中有两个数字 `8` 和 `7`,它们分别代表的是布局中的列数和行数,如果想更清除的了解该段命令,
可以参考[《通过终端命令改变 Launchpad 中应用图标的大小》](https://sspai.com/post/33299).

除了可以对 `Launchpad` 的布局进行更改,还可以根据自己的喜好对背景的模糊程度进行更改,复制以下代码至终端即可:

```bash
defaults write com.apple.dock springboard-blur-radius -int 100;
killall Dock
```

命令中有一个数字 `100` ,它代表的背景模糊的程度,你可以在 `0~255`的范围内选择.

***
修改截图属性

`Mac` 上自带的截图非常的还用,可以区域, 窗口, 延时截图.
但是截图会默认保存在你的桌面上,时间一长,你的桌面就会被五花八门的截图堆满.
对此,我们可以新建一个文件夹专门来存放截图,新建一个 `screenshot` 的文件夹在桌面或者任意一个你希望它待在的地方,将下述代码复制进终端即可:

```bash
mkdir -p ~/Pictures/screenshot
defaults write com.apple.screencapture location ~/Pictures/screenshot;
killall SystemUIServer
```

`~`之后填写你相应的文件夹路径即可.

除此之外,你也可以使用以下命令修改截图保存的类型,例如你想保存 `JPG` 格式的截图:

```bash
efaults write com.apple.screencapture type jpg && killall SystemUIServe
```

***
显示隐藏文件夹

在 `Windows` 上隐藏文件夹大家应该都是老手了, 转到 `Mac` 后,却发现隐藏文件夹和自己想象有那么一些不一样.
为了更好的把大家的`小秘密`藏到内心最深处的地方,也可以使用两段命令来完成操作.
跟前文一样,我们需要获取文件夹的路径,然后在终端中输入以下代码:

```bash
chflags hidden ~/Desktop/Hidden
```

你也可以使用`nohidden`重新让该文件夹显示.如果你要显示全部文件,推荐大家直接使用快捷键`Shift + Command + .`即可显示全部隐藏文件.

除此之外,如果你觉得自己桌面太乱了,但是这会又有人来看你的电脑,你可以使用一段命令行将桌面的文件全部隐藏起来,让桌面回归清爽,文件也依旧可以通过 Finder 中的桌面中找到:

```bash
defaults write com.apple.finder CreateDesktop -bool false; killall Finder
```

如果想重新看到桌面的图标,将 `false` 替换为 `true` 输入终端即可.

***
Dock 栏属性修改

Mac 中为了获得更大的可视空间,在不使用 `Dock` 时我们可以隐藏它.若要查看隐藏的 `Dock`,可以将指针移到 `Dock` 所在屏幕的边缘.
但是这个显示速度存在了一定的延迟,为了加速这个过程,我们可以使用一段命令行,让你的隐藏 `Dock` 弹出的时候更加的顺滑流畅:

```bash
defaults write com.apple.Dock autohide-delay -float 0 && killall Dock
```

使用后的效果,可以说是非常明显了,再也不会有在`挤牙膏`的感觉.

如果在你的使用下,`Dock` 栏上摆满了各类 App,却发现这不是自己想要的结果.你可以通过终端来重置你的 `Dock` 栏,让它回到最开始的状态:

```bash
defaults delete com.apple.dock; killall Dock
```

让屏幕亮的更久

Mac 在运行一段时间后,会自动进入睡眠.如果大家不想 Mac 那么快的进入书面,可以采用一些第三方软件来达到此目的.
其实与其下载一个软件占用 Mac 上精贵的储存,不如使用一段命令行就可以解决这些问题了.
下方命令行中的 `3600` 单位是秒,即你希望多长时间内你的 Mac 不会进入睡眠:

```bash
caffeinate -t 3600
```

***
应用安装与更新

`Mac Apple Store` 的连接情况大家也很清楚,况且还有很多应用并不在商店上架,或是非商店版本有更多的功能.
原来的时候,我们需要查找一个又一个的官网,然后下载安装,其实这么多繁琐操作,在终端里可以更快的完成.你只需要输入:

```bash
brew cask install App
```

将 `App` 替换为你需要安装的软件的名字即可.
但是使用前,需要你在电脑中安装 `Homebrew Cask` ,具体可以参考`再谈 Homebrew Cask 在 macOS 上安装应用的轻松感`.
大多数通过 `Cask` 安装的软件都自带更新选项,如果没有该选项,用户依旧可以通过终端进行更新,
在终端中输入`brew tap buo/cask-upgrade`,然后再输入下段命令即可更新全部应用:

```bash
brew cu
```

### mas,批量更新macOS软件

[终端上的 Mac App Store mas](https://sspai.com/post/40382)
[mas-cli/mas](https://github.com/mas-cli/mas)

如果你希望安装 `Mac App Store` 上的应用,也可以绕过原生的商店应用,直接采用终端进行安装.
你可以先通过刚才安装的 `Homebrew` 安装一个我们需要的 `mas` ,即在终端输入:

```bash
brew install mas
```

#### 查询与安装应用

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

#### 更新应用

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

#### 切换 MacAppStore 账号

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

## vs code 调整

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

## diskutil 磁盘管理

[MacOS 磁盘管理工具 diskutil 介绍](https://www.jianshu.com/p/6a1f365617ad)

电脑上的操作系统, 应用程序和应用数据一般都需要保存在永久存储器中(通常就是硬盘),这样电脑断电后应用数据等就不会丢失.
为了更有效地组织磁盘上的数据信息,通常将磁盘预先划分成一个或多个磁盘分区,创建对应的文件系统,以方便计算机对各分区分别进行管理.
`MacOS` 系统自带一个图形化的磁盘管理工具(`Disk Utility`),同时还有一个命令行版本的 `diskutil`.
通过该命令的使用,可以很快捷地对本地磁盘进行擦除数据, 调整分区大小, 格式化等操作.

`diskutil` 命令的格式为:`diskutil <verb> <options>`

### verb 动作

不带任何选项的 `diskutil` 命令会列出该命令支持的 `verb` 及其对应的介绍:

```bash
 diskutil
Disk Utility Tool
Utility to manage local disks and volumes
```

列出的 `verb` 主要分为以下几类:

+ 获取磁盘和分区信息:如 `list`, `info`, `activity` 等
+ 挂(卸)载磁盘或卷:如 `mount`, `eject`, `mountDisk` 等
+ 验证, 修复磁盘分区或文件系统:如 `verifyVolume`, `repairDisk` 等
+ 分区操作:如 `splitPartitions`, `mergePartitions` 等
+ 其他:如 `appleRAID`, `apfs` 等
+ `diskutil listFilesystems`: 列出支持的文件系统格式

如不清楚某个 `verb` 的具体命令格式,可以直接使用 `diskutil` 命令加上该 `verb` 并且不带任何其他选项,命令行即输出该 `verb` 的使用介绍.
如 `eraseDisk` 的使用介绍:

```bash
$   diskutil eraseDisk
用法:  diskutil eraseDisk format name [APM[Format]|MBR[Format]|GPT[Format]]
        MountPoint|DiskIdentifier|DeviceNode
```

完全擦除现有的整个磁盘. 清除磁盘上的所有内容, 需要磁盘的所有权.
`format`是你擦除之后,重新格式化要得到的格式(`HFS+`等).例如:

擦除整个设备或者磁盘
`diskutil eraseDisk JHFS+ Untitled disk3`

### 获取磁盘分区信息

***
list

可以使用 `list` 选项简要列出 MacOS 系统的磁盘及分区信息,包括分区类型(TYPE), 分区名(NAME), 容量大小(SIZE)和标志符(IDENTIFIER)等.
如此时系统挂载了 `dmg` 映像文件,其信息也会显示在列表中(下表中的 disk3 ).

```bash
$   diskutil list
/dev/disk0 (internal, physical):
   #:                       TYPE NAME                    SIZE       IDENTIFIER
   0:      GUID_partition_scheme                        *121.3 GB   disk0
   1:                        EFI EFI                     209.7 MB   disk0s1
   2:                 Apple_APFS Container disk1         121.1 GB   disk0s2
   ...
```

其中的 `/dev/disk0` 为内置磁盘,`/dev/disk2` 为外置磁盘(U 盘,已在 Windows 系统下格式化为 `FAT32` 格式),`/dev/disk3` 为 `DMG` 映像文件.
而 `/dev/disk1` 其实就是 `disk0s2` 作为 `APFS` 文件系统容器的具体信息.

***
info

`info` 选项可以列出指定磁盘或分区的详细信息.如查看 `disk2` (即 8 G 优盘)的信息:

```bash
 diskutil info disk2
   Device Identifier:        disk2
   Device Node:              /dev/disk2
   Whole:                    Yes
   Part of Whole:            disk2
   Device / Media Name:      DataTraveler 2.0
   ...
```

输出的信息包括设备标志符(Device Identifier), 设备节点(Device Node), 设备名(Device / Media Name), 容量大小(Disk Size), 块大小(Block Size)等.

也可以查看某个分区的详细信息:

```bash
 diskutil info disk1s1
   Device Identifier:        disk1s1
   Device Node:              /dev/disk1s1
   Whole:                    No
   Part of Whole:            disk1
   ...
```

### 擦除磁盘或分区

`eraseDisk` 选项用于擦除整个磁盘并重新格式化.该命令的格式为:

```bash
diskutil eraseDisk <format> <name> [APM|MBR|GPT] MountPoint|DiskIdentifier|DeviceNode
```

`format` 用于指定擦除数据后需要重新建立的文件系统类型.可以为 `%noformat%` 来跳过初始化文件系统的操作.其他支持的类型可以通过 `listFilesystems` 选项查看.

```bash
$   diskutil listFilesystems
Formattable file systems
...
```

可以使用 `diskutil eraseDisk ExFAT StarkyDisk GPT disk2` 命令将优盘数据擦除并格式化为 `ExFAT` 格式.

```bash
$   diskutil eraseDisk ExFAT StarkyDisk GPT disk2
Started erase on disk2
Unmounting disk
...
```

此时的优盘分区表变为 `GPT` 类型,且多了一个 `EFI` 分区.

也可以在擦除磁盘时指定分区表类型:

```bash
$   sudo diskutil eraseDisk ExFAT StarkyDisk MBR disk2
Password:
Started erase on disk2
```

此时的优盘分区表变为 `MBR` 类型:

其他擦除命令如 `eraseVolume` (完全擦除整个磁盘或某个磁盘分区,创建新的文件系统), `zeroDisk` (向整个磁盘或某个分区全部写入`'0'`)
使用 `zeroDisk` 命令擦除磁盘(该过程会花费很长的时间,我试了)后,该磁盘上的全部信息被抹除,同时也不再包含分区和文件系统信息.
则再次插入此优盘会提示你`初始化`或`格式化`该磁盘.

***
制作启动U盘

```bash
# 首先取消usb硬盘的挂载
diskutil unmountDisk /dev/disk6
# bs是读写快的大小, 太小会增大io, 降低效率, 一般1M~2M即可.
sudo dd if=/Users/tom/software/manjaro-kde-20.2.1-210103-linux59.iso of=/dev/disk6 bs=2M
```

### 创建磁盘分区

可以通过 `partionDisk` 选项完成对磁盘的分区操作.该命令的格式为:

```bash
diskutil partitionDisk MountPoint|DiskIdentifier|DeviceNode
        [numberOfPartitions] [APM|MBR|GPT]
        [part1Format part1Name part1Size part2Format part2Name part2Size
         part3Format part3Name part3Size ...]
```

命令选项中的 `Size` 用来指定分区的大小(以扇区数计量),合法的值包括带有指定后缀的浮点数.
其中的后缀有 `B(ytes)`, `S(512-byte-blocks)`, `K(ilobytes)`, `M(egabytes)`, `G(igabytes)`, `T(erabytes),` `P(etabytes)`,
也可以是 `%` 来表示对整个磁盘的占比.
最后一个分区会自动扩展到占用整个磁盘的剩余空间,如果想为最后一个分区指定固定的大小,可在其后再创建一个类型为`free space`的分区.

```bash
$   sudo diskutil partitionDisk disk2 3 MBR MS-DOS F01 3G JHFS+ F02 3G "Free Space" F03 0
Started partitioning on disk2
Unmounting disk
...
```

上面的命令在优盘(`disk2`)上创建了 3 个分区,第一个(F01)格式为 `FAT32`,大小是 `3 Gb`.第二个(F02)格式为 `JHFS+`,大小为 `3 Gb`.
最后一个是`自由空间`,大小为剩余的容量.所以实际上只是分了两个区,整体的分区表类型为 `MBR` .

### 分割/合并磁盘分区

`splitPartition` 选项可以用来将已存在的某个分区再分割成数个更小的分区,注意原分区上的所有数据都会丢失.
该选项的第一个参数为需要分割的分区的`挂载点`/`标志符`/`设备节点`,其余参数和使用 `partitionDisk` 时相同.

```bash
$   sudo diskutil list | grep disk2
/dev/disk2 (external, physical):
   0:      GUID_partition_scheme                        *7.8 GB     disk2
   1:                        EFI EFI                     209.7 MB   disk2s1
   2:                  Apple_HFS starky                  7.5 GB     disk2s2
$   sudo diskutil splitPartition disk2s2 2 MS-DOS F01 3g JHFS+ F02 3g
Started partitioning on disk2s2 starky
Splitting
```

上面的命令将优盘的第二个分区(`disk2s2`)又分割成了两个更小的分区,分别是 `FAT32` 格式的 `F01`(`disk2s2`),和 `JHFS+` 格式的 F02(`disk2s3`).
虽然命令中指定了 F02 的大小是 `3G`,因为是最后一个分区,所以自动扩展到占用剩余的磁盘空间.最后它的实际大小是 `4.5G`.

`mergePartitions` 选项用来将多个已存在的分区合并为一个大的分区.该选项的格式为:

```bash
diskutil mergePartitions [force] format name DiskIdentifier|DeviceNode DiskIdentifier|DeviceNode
```

第一个分区参数为起始分区,第二个分区参数为结束分区.这两个分区之间的所有分区都将被合并.
如果 `force` 选项没有被指定,且合并前的第一个分区是可调整大小的文件系统(如 `JHFS+`),则第一个分区上的数据会保留到合并后的分区.

```bash
$   sudo diskutil list | grep disk2
/dev/disk2 (external, physical):
   0:      GUID_partition_scheme                        *7.8 GB     disk2
   1:                        EFI EFI                     209.7 MB   disk2s1
   2:                  Apple_HFS F01                     2.9 GB     disk2s2
   3:       Microsoft Basic Data F02                     4.5 GB     disk2s4
$   sudo diskutil mergePartitions JHFS+ Starky disk2s2 disk2s4
Merging partitions into a new partition
...
```

### 调整分区大小(无损)

`resizeVolume` 选项可以无损调整(增加或缩减)分区大小.

将 `disk2s2` 分区缩减为 `4g` 大小,腾出的空间作为`free space`:

```bash
$   diskutil list | grep disk2
/dev/disk2 (external, physical):
   0:      GUID_partition_scheme                        *7.8 GB     disk2
   1:                        EFI EFI                     209.7 MB   disk2s1
   2:                  Apple_HFS F01                     7.5 GB     disk2s2
$   sudo diskutil resizeVolume disk2s2 4g
Resizing to 4000000000 bytes
```

将 `disk2s2` 分区扩展,并尽可能占用所有可用的自由空间.

```bash
$   sudo diskutil resizeVolume disk2s2 R
Resizing to full size (fit to fill)
...
/dev/disk2 (external, physical):
   #:                       TYPE NAME                    SIZE       IDENTIFIER
   0:      GUID_partition_scheme                        *7.8 GB     disk2
   1:                        EFI EFI                     209.7 MB   disk2s1
   2:                  Apple_HFS F01                     7.5 GB     disk2s2
```

## 挂载文件

### mounty for NTFS

[Mounty for NTFS](https://mounty.app/)

一个微型工具,用于以读写模式在macOS下重新 mount write-protected `NTFS` .

 相当于指令

```bash
$ sudo umount /Volumes/UNTITLED
$ sudo mount -t ntfs -o rw,auto,nobrowse /dev/disk3s1 ~/ntfs-volume
```

## 维护

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
