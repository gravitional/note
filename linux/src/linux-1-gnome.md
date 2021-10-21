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
