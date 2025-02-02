# mac os 软件安装和设置

## mac 快捷键设置

[在 Mac 上使用全局键盘快捷键](https://support.apple.com/zh-cn/guide/mac-help/mchlp2262/10.15/mac/10.15)
您只能为现有菜单命令创建键盘快捷键. 您不能为通用任务定义键盘快捷键, 例如: 打开一个 `App` 或在 `App` 之间切换.
[在macOS上微调音量](https://sspai.com/post/38531)

妙控键盘的控制键顺序是 `contrl`, `option`, `command`, 把键位更改成这个顺序, 大部分使用默认的快捷键, 加上少部分修改:

+ 切换输入为 `CapsLock`
+ `spotlight`的快捷键为`opt+s`
+ `Show launchpad`的快捷键改成`F12`
+ `显示桌面`为`F11`,在`设置-调度中心`修改.
+ 切换全屏的快捷键为`^+cmd+F`: `Enter Full Screen`, `Exit Full Screen`
+ 锁定屏幕快捷键`^+cmd+Q`

+ 在调整音量时，同时按住 `Shift + option` 可以做更细微的调整，
  + 调低 1/4 格音量：shift + option + F11
  + 调高 1/4 格音量：shift + option + F12

### vscode 常用的快捷键

+ 插入多个光标变成`cmd+option+up`, 可以在`Selection`里面选择鼠标插入的修饰键`alt` or `ctrl`
+ 把`markdown`切换代码环境--`toggle code block`的快捷键设置为`^+k ^+b`
+ 把补全提示--`trigger Suggest`的快捷键设置为`^+space`.
+ `go last`->`cmd+end`
+ 跳转到文档开头--`cmd+up`,跳转到文档末尾--`cmd+down`

[Keyboard shortcuts for macOS](https://code.visualstudio.com/shortcuts/keyboard-shortcuts-macos.pdf)

### mathematica

查找帮助的快捷键: `Find Selected Function`--`F1`
清除所有输出的快捷键: `Delete All Output`--`^+L`

### 终端

`终端` or `iterm2` 中大部分常用的快捷键,移动,清屏等等与`linux` 相同, 是在`readline`库中约定的.
[Bash 行操作](https://wangdoc.com/bash/readline.html)

### Finder

Finder中显示隐藏文件`Cmd+Shift+.`

`mac` 中询问窗口, 焦点的移动方式仍然是按下`tab`, 但是取消操作需要按下`space`,不管在什么情况下, `enter`都是确定.

## zsh 配置

### zsh-syntax-highlighting, 语法高亮

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
