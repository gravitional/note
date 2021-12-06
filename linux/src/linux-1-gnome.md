# gnome.md

## 录制屏幕

如果是`Gnome3`系用户,可以按`ctrl + shift + alt + r`,屏幕右下角有红点出现,则开始录屏,
要结束的话再按一次`ctrl + shift + alt + r`,录好的视频在`~/video`下

## 图片格式转换

`pdf`转成图片格式. 包名: `pdftoppm`. 语法是: `pdftoppm input.pdf outputname -png -f {page} -singlefile`

```bash
pdftoppm  -png -rx 300 -ry 300  input.pdf outputname
```

这个命令将会把`PDF`的每一页转换成`png`格式, 文件名为`outputname-01.png`,`outputname-02.png`等等.
如果只想转换其中的特定一页, 使用`-f {page}`选项指定. 例如`-f 1`表示第一页.

`gnome`默认的查看图片程序为`eog`: eye of gnome

## ubuntu 自带截图

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

#### MP3 文件标签乱码

对于用 `GStreamer` 做后端的播放器, 如 `Rhythmbox``, totem`, 设置如下的环境变量后即可正确读取 `mp3` 中 `GBK` 编码的 `ID3 tag`:

```bash
export GST_ID3_TAG_ENCODING=GBK:UTF-8:GB18030
export GST_ID3V2_TAG_ENCODING=GBK:UTF-8:GB18030
```

## gnome 蓝牙传送文件

`bluetooth-sendto`; 用于通过蓝牙传输文件的GTK应用程序

+ 说明; `bluetooth-sendto [--device=XX:XX:XX:XX:XX:XX [--name=NAME]] [file...]`
+ 描述; `bluetooth-sendto` 将显示一个通过蓝牙传输文件的对话框. `bluetooth-sendto`是`gnome-bluetooth`的一部分, 参见[GnomeBluetooth](http://live.gnome.org/GnomeBluetooth)
+ 选项;
    + `--device`;  定义要发送文件的设备地址.  如果省略, 将显示一个选择器.
    + `--name`; 定义要发送文件的设备名称.  如果省略, 将被自动检测.
    + `file` 要发送到设备的文件.  如果省略, 将显示一个选择器.

## gnome 快捷键

在设置--`Keyboard Shortcuts` 中, 我习惯的键位如下:

```yaml
Launchers:
    Home folder : Super+E # ymy
    Launch Terminal: Ctrl+Alt+T
    Move to workspace above: Super + Page Up
    Move to workspace down: Super + Page Down
    Move window one workspace up: Shift + Super + Page Up
    Move window one workspace down: Shift + Super + Page Down
    Switch to workspace 1: Super + Home
    Switch to last workspace: Super + End
    Switch Applications : Super + Tab # 在所有打开的应用间切换
    Switch windows : Alt + Tab # ymy,在同一个 workspace 内切换
    Switch window Directly: Alt + Escape
    Switch windows of an application: Alt+` or Super +`

Navigation:
    Hide all normal windows : Super+D # ymy

ScreenShots:
    Copy a screenshot of a windows to clipboard: Ctrl +Alt +Print
    Copy a screenshot of an area to clipboard: Shit + Super + S #ymy
    Save a screenshot of an area to Pictures: Shift + Print

System:
    Lock Screen: Super + L
    Log out: Ctrl + Alt +Delete
    Show all applications: Super+A
    Show the overview: Super +S
    Show the run command prompt: Alt + F2

Typing:
    Switch to next input source: Super + Space
    Switch to previous input source: Shift + Super + Space

Windows:
    Hide window: Super + H
    Maxmize window: Super + Up
    Restore window: Super + Down
    View split on left: Super + Left
    View split on right: Super + right
```

## ubuntu ibus 输入法 Ctrl+shift+e;emoji;

[What does Ctrl+ Shift + e do](https://askubuntu.com/questions/1083913/what-does-ctrl-shift-e-do-while-typing-text sdsdfasd)

+ 在 ibus 输入法中, 按下 `Ctrl+Shift+u`, 可以使用 `Unicode` 的十六进制表示法来输入字符:
出现一个带下划线的 `u`, 你输入代码并点击回车.
例如, `Ctrl+Shift+u;e;9;回车` 可以输入字符`é`.

+ 而 `Ctrl+Shift+e` 是表情符号(emoji)输入的`热键序列`.
它产生一个带下划线的 `e̲`, 如果你在它后面输入 `joy`(所以它看起来像 `ejoy`), 整个单词将被下划线.
按下一个空白输入键如`空格`, `ejoy`文字被改变为`😂`.

为了解决热键冲突, 在终端中运行:

```bash
ibus-setup
```

并在`Emoji`标签修改键位即可.

## GnomeShell

[GnomeShell/CheatSheet](https://wiki.gnome.org/Projects/GnomeShell/CheatSheet)

本页包含了许多 GNOME Shell 功能的简要描述, 比如键合, 拖放功能和特殊的实用工具.

许多功能都与启动和在应用程序之间切换有关. 其中有些在 GNOME 的当前版本中就有, 有些是新的.
其他的功能是内置的实用程序, 例如截屏记录工具和集成的检查器工具和 JavaScript 控制台.

### 在桌面上

`Alt+Tab` 在应用程序之间切换. 在你循环浏览时, 有多个窗口的应用程序的窗口预览是可用的.
`Alt+Shift+Tab` 以相反的方向循环浏览应用程序.

`Alt+[Tab上面的键]`(即美国键盘上的`` Alt+` ``)在一个应用程序中的窗口之间切换.
这可以在 `Alt+Tab` 切换器中使用, 也可以在它之外使用(打开切换器时已经选择了当前应用程序的窗口预览).

![Alt+Tab视图](https://wiki.gnome.org/Projects/GnomeShell/CheatSheet?action=AttachFile&do=get&target=shortcuts-Alt-Tab.png)

在其他工作区运行的应用程序会在一个垂直分隔符之后显示.

![带有多个工作区的Alt+Tab视图](https://wiki.gnome.org/Projects/GnomeShell/CheatSheet?action=AttachFile&do=get&target=shortcuts-Alt-Tab-multiple-workspaces.png)

`Alt+F2` 允许输入一个命令来启动一个应用程序.
如果你想在新的终端窗口中启动一个 `shell` 命令, 按 `Ctrl+Enter`.

![Alt+F2提示](https://wiki.gnome.org/Projects/GnomeShell/CheatSheet?action=AttachFile&do=get&target=shortcuts-Alt-F2-cut.png)

### 切换到概览中或从概览中切换

+ Hot corner - 将鼠标指针移到屏幕的左上角, 就可以进入概览或返回到桌面.
+ Activities button - 点击屏幕左上角的活动按钮, 就可以进入概览或回到桌面.
+ 系统(Windows)键 或 `Alt+F1` - 这些组合键将带你进入概览或回到桌面.

### 在概览中

+ 右键点击正在运行的应用程序的图标将显示一个带有窗口标题的菜单, 以选择其中一个窗口.
这个菜单还提供了为该应用程序打开一个新窗口的选项, 并根据该应用程序的当前状态, 将其删除或添加到收藏夹.
+ Ctrl+点击或中键点击正在运行的应用程序的图标, 将在当前工作区打开该应用程序的一个新窗口.

![概述中的窗口选择器](https://wiki.gnome.org/Projects/GnomeShell/CheatSheet?action=AttachFile&do=get&target=shortcuts-window-selector3.png)

在右键菜单选项的帮助下, 正在运行的应用程序可以被添加到收藏夹.
你也可以从应用程序浏览窗格中拖动一个应用程序到收藏夹行中, 使其成为收藏夹中的应用程序.

### Escape

按 `Esc` 键可以 退出:

+ Alt+Tab
+ Alt+F2
+ menus / calendar
+ overview
+ search in the overview
+ Looking Glass
+ Lock Screen

### 屏幕截图

+ Print Screen: 捕捉屏幕截图到文件(在 ~/Images 下).
+ Ctrl + Print Screen: 捕获屏幕截图到剪贴板
+ Alt + Print Screen: 仅捕获活动窗口的屏幕截图到文件中
+ Shift + Print Screen: 捕获屏幕区域到文件, 鼠标变成了区域选择器

### 录制屏幕

`Control+Shift+Alt+R` 键绑定开始和停止录制. (注意: 这个功能目前在一些发行包中没有. ) 在录制过程中, 屏幕的右下角会显示一个红圈.
录制完成后, 一个名为 `"shell-%d%u-%c.webm"` 的文件会被保存在你的 `Videos` 目录中.
在文件名中, `%d` 是日期, `%u` 是一个使文件名独一无二的字符串, `%c` 是计数器, 每次在 `gnome-shell` 会话中进行录制时都会递增.

### 放大器

[GNOME Shell 放大器]: https://wiki.gnome.org/Projects/GnomeShell/Magnification
[python脚本]: https://wiki.gnome.org/Projects/GnomeShell/Magnification#Magnifier_Preferences_Dialog

屏幕放大功能内置于 GNOME Shell 中, 提供各种形式的屏幕增强功能.
它可以使用 `DConf` 编辑器启动和配置, 通过修改 `org/gnome/a11y/magnifier` 设置.
还有一个 [python脚本][], 实现了一个改变缩放偏好的对话框.

更多的细节可以在 [GNOME Shell 放大器][] 页面上找到.

### 开发者工具

`Looking Glass` 是 GNOME Shell 的集成检查器工具和 JavaScript 控制台, 对调试非常有用.
它可以通过在 `Alt+F2` 提示下输入 `lg` 来运行, 然后可以通过按 `Esc` 退出.
更多细节在[这里](https://wiki.gnome.org/Projects/GnomeShell/LookingGlass).

![观察镜](https://wiki.gnome.org/Projects/GnomeShell/CheatSheet?action=AttachFile&do=get&target=shortcuts-looking-glass.png)

+ 在 `Alt+F2` 提示符下输入 `r` 或 `restart` 将重启GNOME Shell. 当你在 GNOME Shell 中工作时对 GNOME Shell 的代码进行修改时, 这很有用.
如果你只修改了 `JavaScript` 代码, 你不需要编译任何东西, 但在重启之前, 你需要像通常对 `C` 代码那样运行编译.
+ 在 `Alt+F2` 提示符下输入 `rt` 将重新加载 `GNOME Shell` 主题.
当你是一个主题设计者, 想要测试你的主题的变化而不需要重启整个 `shell` 时, 这很有用.
主题文件是 `share/gnome-shell/theme/gnome-shell.css`.
+ 在 `Alt+F2` 提示下输入 `debugexit` 将退出 `GNOME Shell`. 这通常只在你运行从命令行启动的 GNOME Shell 开发版本时有用;
在正常的 GNOME 3 会话中, 如果你退出 GNOME Shell, gnome-session 会直接重新启动它.

### Keybindings 列表

这是一个记录 GNOME Shell 拦截的所有按键绑定的地方.
开发者: 如果你增加或改变了一个按键绑定, 请更新这个列表.

+ 系统(Windows)键. 在概览和桌面之间切换
+ Alt+F1: 在概览和桌面之间切换
+ Alt+F2: 弹出命令对话框
+ Alt+Tab. 弹出应用程序切换器
+ Alt+Shift+Tab: 在应用程序切换器中进行反方向循环
+ `Alt+[Tab上面的键]`; 在Alt+Tab中切换同一应用程序的窗口
+ Ctrl+Alt+Tab: 弹出[accessibility 切换器](https://wiki.gnome.org/Accessibility)
+ Ctrl+Shift+Alt+R: 开始和结束屏幕广播录制
+ Ctrl+Alt+上/下箭头. 在工作空间之间切换
+ Ctrl+Alt+Shift+上/下箭头. 将当前窗口移动到不同的工作空间
+ 系统(Windows)键+右/左箭头: 调整窗口大小并移动到当前工作区的右/左半部分
+ 系统(Windows)键+Shift+右/左箭头: 将窗口移动到右/左显示器.

大多数键位可以在 `User Menu -> System Settings -> Keyboard -> Shortcuts ` 中查看.

## gsettings

[gsettings简介及常用操作](https://www.cnblogs.com/awakenedy/articles/10868034.html)

先尽力简单地说明一下几个名词之间的关系. `gsettings` 提供了对 `GSetings` 的命令行操作.
`GSetings` 实际上是一套高级 `API`, 用来操作 `dconf`. `dconf` 存储着 `GNOME3` 的配置, 是二进制格式.
它做为 `GSettings` 的后端系统存在, 暴露出低级 `API`. 在 `GNOME2` 时代, 类似的角色是 `gconf`, 但它是以 `XML` 文本形式存储.

更接地气的说法是, `dconf` 是 `GNOME3` 的注册表, `gsettings` 是一个查询, 读取, 设置注册表键值的命令行工具.

+ 列出所有 `schema`

      gsettings list-schemas

+ 查找某个schema

        gsettings list-schemas | grep "org.gnome.settings-daemon.plugins.keyboard"
        org.gnome.settings-daemon.plugins.keyboard

+ 查找某个 `schema下的所有` `key`

        gsettings list-keys "org.gnome.settings-daemon.plugins.keyboard"

+ 查看某个 `schema` 下某个 `key` 的值

        gsettings get "org.gnome.settings-daemon.plugins.keyboard" priority

+ 查看某个 `schema` 下某个 `key` 的值类型

    ```bash
    gsettings range "org.gnome.settings-daemon.plugins.keyboard" priority
    type i #这是一个整形

    gsettings range "org.gnome.settings-daemon.plugins.keyboard" active
    type b 这是一个boolean类型
    ```

+ 设置某个 `schema` 下某个 `key` 的值

        gsettings set "org.gnome.settings-daemon.plugins.keyboard" active false

+ 也可以一步到位, 同时搜索 `schema` 和 `keys`

        gsettings list-recursively | grep "org.gnome.settings-daemon.plugins.keyboard"

如果你觉得命令行操作还是不太直观, 可以使用 `GUI` 工具 `gconf-editor`
