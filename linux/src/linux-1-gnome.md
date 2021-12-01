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

## xterm 配置

[xterm](https://wiki.archlinux.org/title/Xterm#See_also)
[Xterm的配置](https://www.cnblogs.com/rookieagle/p/12310284.html)
[gotbletu/shownotes: XTerm Xresources ](https://github.com/gotbletu/shownotes/blob/master/xterm_xresources.md)

```Xresources
!-------- Xterm Terminal Settings {{{
!------------------------------------------------------
! https://wiki.archlinux.org/index.php/Xterm
! https://lukas.zapletalovi.com/2013/07/hidden-gems-of-xterm.html
! http://www.futurile.net/2016/06/14/xterm-setup-and-truetype-font-configuration/
! http://www.futurile.net/2016/06/15/xterm-256color-themes-molokai-terminal-theme/

! 允许xterm 正确报告 TERM变量.
! 不要从你的 ~/.bashrc 或 ~/.bash_profile 或类似文件中设置TERM变量.
! 终端本身应该向系统报告正确的 TERM, 这样才能使用正确的 terminfo 文件.
! 两个可用的 terminfo 名称是 xterm 和 xterm-256color.
XTerm.termName: xterm-256color

! Fonts ====================================================
! set 英文和中文字体, 和字体大小 https://github.com/laishulu/Sarasa-Mono-SC-Nerd
XTerm*faceName:Sarasa Mono SC Nerd:antialias=True
XTerm*faceNameDoubleSize:Sarasa Mono SC Nerd:antialias=True
! font size
XTerm*faceSize: 14

UXTerm*faceName:Sarasa Mono SC Nerd:antialias=True
UXTerm*faceNameDoubleSize:Sarasa Mono SC Nerd:antialias=True
UXTerm*faceSize: 14

! VT Font Menu: Unreadable
xterm*faceSize1: 8
! VT font menu: Tiny
xterm*faceSize2: 10
! VT font menu: Medium
xterm*faceSize3: 12
! VT font menu: Large
xterm*faceSize4: 16
! VT font menu: Huge
xterm*faceSize5: 22

! 确保你的locale设置为 UTF-8. 如果你不使用 UTF-8, 你可能需要强迫 xterm 更严格地遵循你的 locale, 通过设置
XTerm.vt100.locale: true

! Cursor ====================================================
! pointer and cursor (blinking and color)
XTerm*pointerColor: white
XTerm*pointerColorBackground: black
XTerm*cursorColor: white
XTerm*cursorBlink: true

!! Selecting Text ========================================================
! Only select text
XTerm*highlightSelection: true
! Remove trailing spaces
XTerm*trimSelection: true

!! Scrolling ========================================================
! Use: Shift-Pageup / Shift-Pagedown 来滚动 或 鼠标滚轮
! 你可以回滚的输出行
XTerm*saveLines: 16384

! 打开滚动条, 并把它放在右边
! XTerm.vt100.scrollBar: true
! XTerm.vt100.scrollbar.width: 8
! xterm*scrollBar: true
! xterm*rightScrollBar: true

! 当有新的输入时不要滚动, 例如 tail -f /var/syslog
XTerm*scrollTtyOutput: false

!! Keybinding ========================================================
! http://blog.rot13.org/2010/03/change-font-size-in-xterm-using-keyboard.html
! - change fontsize on the fly (ctrl+plus = increase ; ctrl+minus = decrease, ctrl+0 = default)
! - copy/paste hotkey (ctrl+shift+c = copy ; ctrl+shift+v = paste)
! - open url (clickable links)
!   1) double click to highlight the full url
!   2) Shift + click it to open it
XTerm.vt100.translations: #override \n\
  Ctrl <Key> minus: smaller-vt-font() \n\
  Ctrl <Key> plus: larger-vt-font() \n\
  Ctrl <Key> 0: set-vt-font(d) \n\
  Ctrl Shift <Key>C: copy-selection(CLIPBOARD) \n\
  Ctrl Shift <Key>V: insert-selection(CLIPBOARD) \n\
  Shift <Btn1Up>: exec-formatted("xdg-open '%t'", PRIMARY) \n\
  <Btn1Up>: select-end(PRIMARY, CLIPBOARD, CUT_BUFFER0) \n\
  <Btn2Up>: insert-selection(PRIMARY)

! enable copy/paste hotkey to work (shift+insert = paste ; mouse highlight = copy)
XTerm*selectToClipboard: true
! disable fullscreen hotkey alt+enter (hotkey conflicts with weechat, midnight commander ...etc)
XTerm*fullscreen: never
! enable alt key to work
XTerm*metaSendsEscape: true
! Fix the backspace key (for Emacs)
XTerm.vt100.backarrowKey: false
XTerm.ttyModes: erase ^?

! double-click to select whole URLs :D
! https://scarygliders.net/2011/12/01/customize-xterm-the-original-and-best-terminal/
XTerm*charClass: 33:48,36-47:48,58-59:48,61:48,63-64:48,95:48,126:48

! Tips: Left and right selection (text selection using left button for beginning and right button for end)
! Tips: Triple Click ( 1 click = nothing, 2 click = select word, 3 click = select line )
!
! XTerm*on3Clicks: regex [[:alpha:]]+://([[:alnum:]!#+,./=?@_~-]|(%[[:xdigit:]][[:xdigit:]]))+
XTerm*on3Clicks: regex ([[:alpha:]]+://)?([[:alnum:]!#+,./=?@_~-]|(%[[:xdigit:]][[:xdigit:]]))+

! <BtnUp>:select-end(SELECT, CUT_BUFFER0) \n\
! Ctrl <Btn4Down>:scroll-back(1,halfpage,m) \n\
! <Btn4Down>:scroll-back(5,line,m) \n\
! Ctrl <Btn5Down>:scroll-forw(1,halfpage,m) \n\
! <Btn5Down>:scroll-forw(5,line,m) \n\

!    Ctrl <Key>M: maximize() \n\
!    Ctrl <Key>R: restore() \n\
!                 Shift <KeyPress> Prior:scroll-back(1,halfpage) \n\
!                  Shift <KeyPress> Next:scroll-forw(1,halfpage) \n\
!                Shift <KeyPress> Select:select-cursor-start() \
!                                        select-cursor-end(SELECT, CUT_BUFFER0) \n\
!                Shift <KeyPress> Insert:insert-selection(SELECT, CUT_BUFFER0) \n\
!                        Alt <Key>Return:fullscreen() \n\
!               <KeyRelease> Scroll_Lock:scroll-lock() \n\
!           Shift~Ctrl <KeyPress> KP_Add:larger-vt-font() \n\
!           Shift Ctrl <KeyPress> KP_Add:smaller-vt-font() \n\
!           Shift <KeyPress> KP_Subtract:smaller-vt-font() \n\
!                       ~Meta <KeyPress>:insert-seven-bit() \n\
!                        Meta <KeyPress>:insert-eight-bit() \n\
!                       !Ctrl <Btn1Down>:popup-menu(mainMenu) \n\
!                       ~Meta <Btn1Down>:select-start() \n\
!                     ~Meta <Btn1Motion>:select-extend() \n\
!                       !Ctrl <Btn2Down>:popup-menu(vtMenu) \n\
!                 ~Ctrl ~Meta <Btn2Down>:ignore() \n\
!                        Meta <Btn2Down>:clear-saved-lines() \n\
!                   ~Ctrl ~Meta <Btn2Up>:insert-selection(SELECT, CUT_BUFFER0) \n\
!                       !Ctrl <Btn3Down>:popup-menu(fontMenu) \n\
!                 ~Ctrl ~Meta <Btn3Down>:start-extend() \n\
!                     ~Meta <Btn3Motion>:select-extend() \n\
!                        Ctrl <Btn4Down>:scroll-back(1,halfpage,m) \n\
!                             <Btn4Down>:scroll-back(5,line,m)     \n\
!                        Ctrl <Btn5Down>:scroll-forw(1,halfpage,m) \n\
!                                <BtnUp>:select-end(SELECT, CUT_BUFFER0) \n\
!                              <BtnDown>:ignore()

! http://forums.fedoraforum.org/showpost.php?p=1538211&postcount=3
! https://stackoverflow.com/a/29551654
! XTerm*title: xterm
! XTerm*background: #011622
! XTerm*foreground: WhiteSmoke
! XTerm*pointerColor: white
! XTerm*pointerColorBackground: #011622
! XTerm*cursorColor: #EBD27D
! XTerm*internalBorder: 3
! XTerm*loginShell: true
! XTerm*scrollBar: false
! XTerm*scrollKey: true
! XTerm*saveLines: 1250
! XTerm*multiClickTime: 250
! XTerm*Geometry: 140x50+110+60
! XTerm*renderFont: true

! set fontsize
! xterm*font:     *-fixed-*-*-*-24-*
! set font
! xterm*faceName: Monospace
! }}}

!####################
!###### Themes ######
!####################
! http://web.archive.org/web/20090130061234/http://phraktured.net/terminal-colors/

!-------- Theme: Solarized {{{
!------------------------------------------------------

!!Source http://github.com/altercation/solarized

*background: #002b36
*foreground: #657b83
!!*fading: 40
*fadeColor: #002b36
*cursorColor: #93a1a1
*pointerColorBackground: #586e75
*pointerColorForeground: #93a1a1

!! black dark/light
*color0: #073642
*color8: #002b36

!! red dark/light
*color1: #dc322f
*color9: #cb4b16

!! green dark/light
*color2: #859900
*color10: #586e75

!! yellow dark/light
*color3: #b58900
*color11: #657b83

!! blue dark/light
*color4: #268bd2
*color12: #839496

!! magenta dark/light
*color5: #d33682
*color13: #6c71c4

!! cyan dark/light
*color6: #2aa198
*color14: #93a1a1

!! white dark/light
*color7: #eee8d5
*color15: #fdf6e3

! }}}
! !-------- Theme: Rezza {{{
! !------------------------------------------------------
! !!colors ripped from rezza: http://metawire.org/~rezza/index.php
! *foreground:     rgb:dd/dd/dd
! *background:     rgb:22/22/22
! !black
! *color0:         rgb:19/19/19
! *color8:         rgb:25/25/25
! !red
! *color1:         rgb:80/32/32
! *color9:         rgb:98/2b/2b
! !green
! *color2:         rgb:5b/76/2f
! *color10:        rgb:89/b8/3f
! !brown/yellow
! *color3:         rgb:aa/99/43
! *color11:        rgb:ef/ef/60
! !blue
! *color4:         rgb:32/4c/80
! *color12:        rgb:2b/4f/98
! !magenta
! *color5:         rgb:70/6c/9a
! *color13:        rgb:82/6a/b1
! !cyan
! *color6:         rgb:92/b1/9e
! *color14:        rgb:a1/cd/cd
! !white
! *color7:         rgb:ff/ff/ff
! *color15:        rgb:dd/dd/dd
!
!
! ! }}}
! !-------- Theme: One {{{
! !------------------------------------------------------
! !Theme One
! *background: rgb:00/00/00
! *foreground: rgb:a8/a8/a8
! *color0:     rgb:00/00/00
! *color1:     rgb:a8/00/00
! *color2:     rgb:00/a8/00
! *color3:     rgb:a8/54/00
! *color4:     rgb:00/00/a8
! *color5:     rgb:a8/00/a8
! *color6:     rgb:00/a8/a8
! *color7:     rgb:a8/a8/a8
! *color8:     rgb:54/50/54
! *color9:     rgb:f8/54/50
! *color10:    rgb:50/fc/50
! *color11:    rgb:f8/fc/50
! *color12:    rgb:50/54/f8
! *color13:    rgb:f8/54/f8
! *color14:    rgb:50/fc/f8
! *color15:    rgb:f8/fc/f8
! ! }}}
! !-------- Theme: Two {{{
! !------------------------------------------------------
! !Theme Two
! *background: rgb:00/00/00
! *foreground: rgb:7f/7f/7f
! *color0:     rgb:00/00/00
! *color1:     rgb:9e/18/28
! *color2:     rgb:ae/ce/92
! *color3:     rgb:96/8a/38
! *color4:     rgb:41/41/71
! *color5:     rgb:96/3c/59
! *color6:     rgb:41/81/79
! *color7:     rgb:be/be/be
! *color8:     rgb:66/66/66
! *color9:     rgb:cf/61/71
! *color10:    rgb:c5/f7/79
! *color11:    rgb:ff/f7/96
! *color12:    rgb:41/86/be
! *color13:    rgb:cf/9e/be
! *color14:    rgb:71/be/be
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Three {{{
! !------------------------------------------------------
! !Theme Three
! *background: rgb:00/00/00
! *foreground: rgb:cf/cf/cf
! *color0:     rgb:00/00/00
! *color1:     rgb:e0/10/10
! *color2:     rgb:20/ad/20
! *color3:     rgb:d4/c2/4f
! *color4:     rgb:23/1b/b8
! *color5:     rgb:9c/38/85
! *color6:     rgb:1d/bd/b8
! *color7:     rgb:fe/fe/fe
! *color8:     rgb:6a/6a/6a
! *color9:     rgb:e8/3a/3d
! *color10:    rgb:35/e9/56
! *color11:    rgb:ff/ff/2f
! *color12:    rgb:3a/53/f0
! *color13:    rgb:e6/28/ba
! *color14:    rgb:1c/f5/f5
! *color15:    rgb:ff/ff/ff
! ! }}}
! !-------- Theme: Four {{{
! !------------------------------------------------------
! !Theme Four
! *background: rgb:00/00/00
! *foreground: rgb:ff/ff/ff
! *color0:     rgb:00/00/00
! *color1:     rgb:d3/62/65
! *color2:     rgb:ae/ce/91
! *color3:     rgb:e7/e1/8c
! *color4:     rgb:7a/7a/b0
! *color5:     rgb:96/3c/59
! *color6:     rgb:41/81/79
! *color7:     rgb:be/be/be
! *color8:     rgb:66/66/66
! *color9:     rgb:ef/81/71
! *color10:    rgb:e5/f7/79
! *color11:    rgb:ff/f7/96
! *color12:    rgb:41/86/be
! *color13:    rgb:ef/9e/be
! *color14:    rgb:71/be/be
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Five {{{
! !------------------------------------------------------
! !Theme Five
! *background: rgb:ad/aa/ad
! *foreground: rgb:00/00/00
! *color0:     rgb:00/00/00
! *color1:     rgb:64/0f/19
! *color2:     rgb:63/79/6b
! *color3:     rgb:ad/71/42
! *color4:     rgb:4f/4f/89
! *color5:     rgb:b2/5c/7c
! *color6:     rgb:52/75/6b
! *color7:     rgb:ad/aa/ad
! *color8:     rgb:52/55/52
! *color9:     rgb:a5/61/63
! *color10:    rgb:ce/c2/63
! *color11:    rgb:73/ae/70
! *color12:    rgb:36/70/9f
! *color13:    rgb:aa/82/9c
! *color14:    rgb:51/89/89
! *color15:    rgb:ff/ff/ef
! ! }}}
! !-------- Theme: Six {{{
! !------------------------------------------------------
! !Theme Six
! *background: rgb:be/be/be
! *foreground: rgb:21/21/21
! *color0:     rgb:00/00/00
! *color1:     rgb:bf/72/76
! *color2:     rgb:86/af/80
! *color3:     rgb:96/8a/38
! *color4:     rgb:36/73/b5
! *color5:     rgb:9a/70/b2
! *color6:     rgb:7a/be/cc
! *color7:     rgb:db/db/db
! *color8:     rgb:66/92/af
! *color9:     rgb:e5/50/5f
! *color10:    rgb:87/bc/87
! *color11:    rgb:e0/d9/5c
! *color12:    rgb:1b/85/d6
! *color13:    rgb:ad/73/ba
! *color14:    rgb:33/8e/aa
! *color15:    rgb:f4/f4/f4
! ! }}}
! !-------- Theme: Seven {{{
! !------------------------------------------------------
! !Theme Seven
! *background: rgb:67/67/67
! *foreground: rgb:ff/ff/ff
! *color0:     rgb:00/00/00
! *color1:     rgb:bf/46/46
! *color2:     rgb:67/b2/5f
! *color3:     rgb:cf/c4/4e
! *color4:     rgb:51/60/83
! *color5:     rgb:ca/6e/ff
! *color6:     rgb:92/b2/f8
! *color7:     rgb:d5/d5/d5
! *color8:     rgb:00/00/00
! *color9:     rgb:f4/8a/8a
! *color10:    rgb:a5/d7/9f
! *color11:    rgb:e1/da/84
! *color12:    rgb:a2/bb/ff
! *color13:    rgb:e2/b0/ff
! *color14:    rgb:ba/cd/f8
! *color15:    rgb:d5/d5/d5
! ! }}}
! !-------- Theme: Eight {{{
! !------------------------------------------------------
! !Theme Eight
! *background: rgb:10/10/10
! *foreground: rgb:d3/d3/d3
! *color0:     rgb:10/10/10
! *color1:     rgb:cd/5c/5c
! *color2:     rgb:2e/8b/57
! *color3:     rgb:f0/e6/8c
! *color4:     rgb:b0/c4/de
! *color5:     rgb:ba/55/d3
! *color6:     rgb:46/82/b4
! *color7:     rgb:d3/d3/d3
! *color8:     rgb:4d/4d/4d
! *color9:     rgb:ff/6a/6a
! *color10:    rgb:8f/bc/8f
! *color11:    rgb:ff/fa/cd
! *color12:    rgb:1e/90/ff
! *color13:    rgb:db/70/93
! *color14:    rgb:5f/9e/a0
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Nine {{{
! !------------------------------------------------------
! !Theme Nine
! *background: rgb:1a/1a/1a
! *foreground: rgb:d6/d6/d6
! *color0:     rgb:00/00/00
! *color1:     rgb:9e/18/28
! *color2:     rgb:00/88/00
! *color3:     rgb:96/8a/38
! *color4:     rgb:41/41/71
! *color5:     rgb:96/3c/59
! *color6:     rgb:41/81/79
! *color7:     rgb:be/be/be
! *color8:     rgb:66/66/66
! *color9:     rgb:cf/61/71
! *color10     rgb:7c/bc/8c
! *color11     rgb:ff/f7/96
! *color12     rgb:41/86/be
! *color13     rgb:cf/9e/be
! *color14     rgb:71/be/be
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Ten {{{
! !------------------------------------------------------
! !Theme Ten
! *background: rgb:1a/1a/1a
! *foreground: rgb:d6/d6/d6
! *color0:     rgb:00/00/00
! *color1:     rgb:98/56/5e
! *color2:     rgb:66/82/5d
! *color3:     rgb:96/91/76
! *color4:     rgb:4d/65/85
! *color5:     rgb:96/73/95
! *color6:     rgb:5f/7f/7b
! *color7:     rgb:b3/b3/b3
! *color8:     rgb:73/73/73
! *color9:     rgb:cf/a3/a9
! *color10:    rgb:ca/f7/bb
! *color11:    rgb:ff/f8/bc
! *color12:    rgb:83/a3/be
! *color13:    rgb:bb/a9/cf
! *color14:    rgb:96/cc/cc
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Eleven {{{
! !------------------------------------------------------
! !Theme Eleven
! *background: rgb:33/33/33
! *foreground: rgb:ff/ff/ff
! *color0:     rgb:33/33/33
! *color8:     rgb:33/33/33
! *color1:     rgb:ff/a0/a0
! *color9:     rgb:ff/a0/a0
! *color2:     rgb:98/fb/98
! *color10:    rgb:9a/cd/32
! *color3:     rgb:f0/e6/8c
! *color11:    rgb:f0/e6/8c
! *color4:     rgb:87/ce/eb
! *color12:    rgb:87/ce/eb
! *color5:     rgb:ff/a0/a0
! *color13:    rgb:ff/a0/a0
! *color6:     rgb:87/ce/eb
! *color14:    rgb:87/ce/eb
! *color7:     rgb:ff/ff/ff
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Twelve {{{
! !------------------------------------------------------
! !Theme Twelve
! *foreground: rgb:ff/ff/ff
! *background: rgb:00/00/00
! *color0:     rgb:00/00/00
! *color1:     rgb:bf/72/76
! *color2:     rgb:86/af/80
! *color3:     rgb:96/8a/38
! *color4:     rgb:36/73/b5
! *color5:     rgb:9a/70/b2
! *color6:     rgb:7a/be/cc
! *color7:     rgb:db/db/db
! *color8:     rgb:66/92/af
! *color9:     rgb:e5/50/5f
! *color10:    rgb:87/bc/87
! *color11:    rgb:e0/d9/5c
! *color12:    rgb:1b/85/d6
! *color13:    rgb:ad/73/ba
! *color14:    rgb:33/8e/aa
! *color15:    rgb:f4/f4/f4
!
!
! ! }}}
! !-------- Theme: Thirteen {{{
! !------------------------------------------------------
! !Theme Thirteen
! *background: rgb:00/00/00
! *foreground: rgb:aa/aa/aa
! *color0:     rgb:00/00/00
! *color1:     rgb:9e/18/28
! *color2:     rgb:ae/ce/92
! *color3:     rgb:96/8a/38
! *color4:     rgb:41/41/71
! *color5:     rgb:96/3c/59
! *color6:     rgb:7f/9f/7f
! *color7:     rgb:be/be/be
! *color8:     rgb:66/66/66
! *color9:     rgb:cf/61/71
! *color10:    rgb:af/c5/af
! *color11:    rgb:f0/df/af
! *color12:    rgb:8e/9f/bc
! *color13:    rgb:dc/a3/a3
! *color14:    rgb:95/c1/c5
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Fourteen {{{
! !------------------------------------------------------
! !Theme Fourteen
! *background: rgb:95/95/95
! *foreground: rgb:00/00/00
! *color0:     rgb:7f/7f/7f
! *color1:     rgb:cd/00/00
! *color2:     rgb:00/8b/00
! *color3:     rgb:ee/ee/00
! *color4:     rgb:00/00/cd
! *color5:     rgb:cd/00/cd
! *color6:     rgb:00/ee/ee
! *color7:     rgb:fa/eb/d7
! *color8:     rgb:e5/e5/e5
! *color9:     rgb:80/00/00
! *color10:    rgb:00/50/20
! *color11:    rgb:99/55/00
! *color12:    rgb:00/40/80
! *color13:    rgb:44/33/00
! *color14:    rgb:30/60/80
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Fifteen {{{
! !------------------------------------------------------
! !Theme Fifteen
! *background: rgb:1d/2b/3a
! *foreground: rgb:be/be/be
! *color0:     rgb:00/00/00
! *color1:     rgb:d3/62/65
! *color2:     rgb:ae/ce/91
! *color3:     rgb:e7/e1/8c
! *color4:     rgb:7a/7a/b0
! *color5:     rgb:96/3c/59
! *color6:     rgb:41/81/79
! *color7:     rgb:be/be/be
! *color8:     rgb:66/66/66
! *color9:     rgb:ef/81/71
! *color10:    rgb:e5/f7/79
! *color11:    rgb:ff/f7/99
! *color12:    rgb:41/86/be
! *color13:    rgb:ef/9e/be
! *color14:    rgb:71/be/be
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Sixteen {{{
! !------------------------------------------------------
! !Theme Sixteen
! *background: rgb:00/00/00
! *foreground: rgb:be/be/be
! *color0:     rgb:00/00/00
! *color1:     rgb:9e/18/28
! *color2:     rgb:ae/ce/92
! *color3:     rgb:96/8a/38
! *color4:     rgb:41/41/71
! *color5:     rgb:96/3c/59
! *color6:     rgb:41/81/79
! *color7:     rgb:be/be/be
! *color8:     rgb:66/66/66
! *color9:     rgb:cf/61/71
! *color10:    rgb:c5/f7/79
! *color11:    rgb:ff/f7/96
! *color12:    rgb:41/86/be
! *color13:    rgb:cf/9e/be
! *color14:    rgb:71/be/be
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Seventeen {{{
! !------------------------------------------------------
! !Theme Seventeen
! *background: rgb:00/00/00
! *foreground: rgb:e5/e5/e5
! *color0:     rgb:00/00/00
! *color1:     rgb:ff/00/00
! *color2:     rgb:00/ff/00
! *color3:     rgb:ff/ff/00
! *color4:     rgb:00/00/ff
! *color5:     rgb:ff/00/ff
! *color6:     rgb:00/ff/ff
! *color7:     rgb:ff/ff/ff
! *color8:     rgb:ff/d3/9b
! *color9:     rgb:ff/82/47
! *color10:    rgb:ff/82/ab
! *color11:    rgb:87/ce/fa
! *color12:    rgb:ff/ff/ff
! *color13:    rgb:ff/ff/ff
! *color14:    rgb:ff/ff/ff
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Eighteen {{{
! !------------------------------------------------------
! !Theme Eighteen
! *color0:  rgb:00/00/00
! *color1:  rgb:9e/18/28
! *color2:  rgb:5c/b2/47
! *color3:  rgb:96/8a/38
! *color4:  rgb:41/61/a0
! *color5:  rgb:9b/76/8e
! *color6:  rgb:41/91/89
! *color7:  rgb:be/be/be
! *color8:  rgb:66/66/66
! *color9:  rgb:cf/61/71
! *color10: rgb:c5/f7/79
! *color11: rgb:ff/f7/96
! *color12: rgb:41/86/be
! *color13: rgb:cf/9e/be
! *color14: rgb:71/be/be
! *color15: rgb:dd/dd/dd
!
!
! ! }}}
! !-------- Theme: Nineteen {{{
! !------------------------------------------------------
! !Theme Nineteen
! *color0:  rgb:00/00/00
! *color1:  rgb:b0/70/50
! *color2:  rgb:12/91/4e
! *color3:  rgb:a0/a0/70
! *color4:  rgb:3e/45/81
! *color5:  rgb:a0/70/a0
! *color6:  rgb:70/a0/a0
! *color7:  rgb:a0/a0/a0
! *color8:  rgb:60/60/60
! *color9:  rgb:b0/70/50
! *color10: rgb:12/91/4e
! *color11: rgb:c0/c0/90
! *color12: rgb:3e/45/81
! *color13: rgb:c0/90/c0
! *color14: rgb:90/c0/c0
! *color15: rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Twenty {{{
! !------------------------------------------------------
! !Theme Twenty
! *foreground: rgb:aa/aa/aa
! *background: rgb:00/00/00
! *color0:     rgb:30/34/30
! *color1:     rgb:bf/79/79
! *color2:     rgb:97/b2/6b
! *color3:     rgb:cd/cd/c1
! *color4:     rgb:86/a2/be
! *color5:     rgb:d9/b7/98
! *color6:     rgb:a1/b5/cd
! *color7:     rgb:ff/ff/ff
! *color8:     rgb:cd/b5/cd
! *color9:     rgb:f4/a4/5f
! *color10:    rgb:c5/f7/79
! *color11:    rgb:ff/ff/ef
! *color12:    rgb:98/af/d9
! *color13:    rgb:d7/d9/98
! *color14:    rgb:a1/b5/cd
! *color15:    rgb:de/de/de
!
!
! ! }}}
! !-------- Theme: Twenty-One {{{
! !------------------------------------------------------
! !Theme Twenty-One
! *background: rgb:1a/1a/1a
! *foreground: rgb:aa/aa/aa
! *color0:     rgb:00/00/00
! *color8:     rgb:66/66/66
! *color1:     rgb:9e/18/28
! *color9:     rgb:bc/57/66
! *color2:     rgb:00/88/00
! *color10:    rgb:61/a1/71
! *color3:     rgb:d2/bb/4b
! *color11:    rgb:e7/db/52
! *color4:     rgb:41/41/71
! *color12:    rgb:50/85/af
! *color5:     rgb:96/3c/59
! *color13:    rgb:a9/7a/99
! *color6:     rgb:41/81/79
! *color14:    rgb:6b/a4/a4
! *color7:     rgb:be/be/be
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Twenty-Two {{{
! !------------------------------------------------------
! !Theme Twenty-Two
! *background: rgb:00/00/00
! *foreground: rgb:be/be/be
! *color0:     rgb:00/00/00
! *color1:     rgb:d3/62/65
! *color2:     rgb:ae/ce/91
! *color3:     rgb:e7/e1/8c
! *color4:     rgb:7a/7a/b0
! *color5:     rgb:96/3c/59
! *color6:     rgb:7f/9f/7f
! *color7:     rgb:be/be/be
! *color8:     rgb:66/66/66
! *color9:     rgb:ef/81/71
! *color10:    rgb:e5/f7/79
! *color11:    rgb:f0/df/af
! *color12:    rgb:8e/9f/bc
! *color13:    rgb:ef/9e/be
! *color14:    rgb:71/be/be
! *color15:    rgb:ff/ff/ff
!
!
! ! }}}
! !-------- Theme: Twenty-Three {{{
! !------------------------------------------------------
! !Theme Twenty-Three
! *background:   rgb:0e/0e/0e
! *foreground:   rgb:4a/d5/e1
! *color0:       rgb:00/00/00
! *color1:       rgb:dc/74/d1
! *color2:       rgb:0e/b8/c7
! *color3:       rgb:df/e3/7e
! !*color4: ???
! *color5:       rgb:9e/88/f0
! *color6:       rgb:73/f7/ff
! *color7:       rgb:e1/dd/dd
! *color8:       rgb:8b/8f/93
! *color9:       rgb:dc/74/d1
! *color10:      rgb:0e/b8/c7
! *color11:      rgb:df/e3/7e
! *color13:      rgb:9e/88/f0
! *color14:      rgb:73/f7/ff
! *color15:      rgb:e1/dd/dd
!
!
! ! }}}
! !-------- Theme: Twenty-Four {{{
! !------------------------------------------------------
! !Theme Twenty-Four
! *color0:  rgb:00/00/00
! *color1:  rgb:cd/5c/5c
! *color2:  rgb:8e/ae/71
! *color3:  rgb:d2/b4/8c
! *color4:  rgb:5f/7b/8a
! *color5:  rgb:cd/cd/b4
! *color6:  rgb:68/68/68
! *color7:  rgb:ff/ff/ff
! *color8:  rgb:00/00/00
! *color9:  rgb:ee/63/63
! *color10: rgb:95/c7/49
! *color11: rgb:cd/cd/c1
! *color12: rgb:6b/7b/8a
! *color13: rgb:cd/cd/b4
! *color14: rgb:77/87/98
! *color15: rgb:ca/ca/ca
!
!
! ! }}}
! !-------- Theme: Twenty-Five {{{
! !------------------------------------------------------
! ! Theme Twenty-Five
! *color0:  rgb:00/00/00
! *color1:  rgb:80/00/00
! *color2:  rgb:00/80/00
! *color3:  rgb:d0/d0/90
! *color4:  rgb:00/00/80
! *color5:  rgb:80/00/80
! *color6:  rgb:a6/ca/f0
! *color7:  rgb:d0/d0/d0
! *color8:  rgb:b0/b0/b0
! *color9:  rgb:f0/80/60
! *color10: rgb:60/f0/80
! *color11: rgb:e0/c0/60
! *color12: rgb:80/c0/e0
! *color13: rgb:f0/c0/f0
! *color14: rgb:c0/d8/f8
! *color15: rgb:e0/e0/e0
!
!
! ! }}}
! !-------- Theme: Tango {{{
! !------------------------------------------------------
! !tango color scheme
! ! source: https://github.com/lenage/dotfiles/blob/master/.Xresources
! xterm*color0: #1e1e1e
! xterm*color1: #cc0000
! xterm*color2: #4e9a06
! xterm*color3: #c4a000
! xterm*color4: #3465a4
! xterm*color5: #75507b
! xterm*color6: #0b939b
! xterm*color7: #d3d7cf
! xterm*color8: #555753
! xterm*color9: #ef2929
! xterm*color10: #8ae234
! xterm*color11: #fce94f
! xterm*color12: #729fcf
! xterm*color13: #ad7fa8
! xterm*color14: #00f5e9
! xterm*color15: #eeeeec
!
!
! ! }}}
! !-------- Theme: Snazzy Color {{{
! !------------------------------------------------------
! ! https://github.com/olstenlarck/urxvt-xterm-snazzy/blob/7c34009a19ade65271e26ef065c678b0fa2abd0d/.Xdefaults
!
! *background: #282a36
! *foreground: #eff0eb
!
! ! original #86a2b0
! *colorUL: #eff0eb
! *underlineColor: #eff0eb
!
! ! black
! *color0  : #2E3436
! *color8  : #686868
!
! ! red
! *color1  : #ff5c57
! *color9  : #ff5c57
!
! ! green
! *color2  : #5af78e
! *color10 : #5af78e
!
! ! yellow
! *color3  : #f3f99d
! *color11 : #f3f99d
!
! ! blue
! *color4  : #57c7ff
! *color12 : #57c7ff
!
! ! magenta
! *color5  : #ff6ac1
! *color13 : #ff6ac1
!
! ! cyan
! *color6  : #9aedfe
! *color14 : #9aedfe
!
! ! white
! *color7  : #f1f1f0
! *color15 : #eff0eb
!
!
! ! }}}
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
