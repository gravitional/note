# homebrew,软件管理

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

## brew 国内源

建议按照[Homebrew / Linuxbrew 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/homebrew/)中的教程安装
以及 [Homebrew-bottles 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/homebrew-bottles/)
或者参考使用中科大镜像的 `git` 备份, [ineo6/homebrew-install](https://github.com/ineo6/homebrew-install)

如果没有使用过`homebrew/cask/sublime-text`下面的软件,
可能没有`/opt/homebrew/Library/Taps/homebrew/homebrew-cask`这个目录, 随便安装一个`cask`软件, 再设置即可.

## Bottles

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

## Core 源使用帮助

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

## Cask

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

## Cask Versions 源使用帮助

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

## brew miss bottles 报错

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

## 常用软件的安装

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

## arhc 指定软件架构

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

## brew and brew cask 的区别

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

## 常用的brew命令

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

## macOS 操作系统的备份与导入

[全平台的备份指南](https://sspai.com/post/56272)

### 导出 Homebrew 和 App Store 的软件列表

借助 `Homebrew Bundle`, 我们可以非常简洁地导出所安装的 Homebrew, Homebrew Cask 和 Mac App Store 应用.
为了处理 Mac App Store 应用, 首先需要安装一个依赖:

```bash
brew install mas
```

然后就可以执行:

```bash
brew bundle dump
```

这样, 在当前的目录下就自动生成了一个名为 `Brewfile` 的文本文件. 打开之后可以看到, 里面包含了:

```bash
tap
brew
cask
mas
```

对于 `brew`, 它导出的列表已经剔除了自动安装的依赖项, 而只留下了我们手动安装的部分.
对于 `mas`, 它导出的列表包含了程序名称, 以及用于后续安装的 `app ID`.

### 导出 Launchpad 中的软件列表

`Launchpad` 也就是 `启动台` , 其中的软件列表与 `应用程序` 文件夹内可以认为一致.
如果需要, 可以备份这个软件列表.

+ 打开 `应用程序` 文件夹
+ 全选, 复制
+ 打开 VSCode 并粘贴

这样, 软件列表就会以文本的形式得以保存了.

### 导入备份的软件

将需要安装的软件按格式放到 `Brewfile` 中, 并执行:

```bash
brew bundle
```

稍等之后, 列表中的软件就已经安装完成了.
