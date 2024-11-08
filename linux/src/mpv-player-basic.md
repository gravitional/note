# mpv播放器

[mpv播放器默认快捷键设置](https://www.bilibili.com/read/cv11615972)

mpv是一个自由开源的跨平台媒体播放器, 具有轻量, 兼容性好, 播放性能优秀, 简洁无广告等特点.
[mpv官网](https://mpv.io):

## mpv默认快捷键方案

![img](https://i0.hdslb.com/bfs/article/33d07afd7cd75dc52f4c2c03e3d895314da2b05f.jpg@942w_606h_progressive.webp)

`m` 静音

## 配置文件

以下是 `mpv.conf` 和 `input.conf` 的简化版, 只写了最基础常用的快捷键功能.
更丰富的配置见 [自用MPV播放器input.conf, mpv.conf(修订版)](https://www.bilibili.com/read/cv13479755)

使用方法:

+ windows端, 在mpv解压所得文件夹中新建 `portable_config` 文件夹,
复制以下代码块内容到txt文本文件中, 分别改名称为 `mpv.conf`, `input.conf`,
放置于 `mpv\portable_config\`中;
+ 类Unix(Linux/macOS等)则放在 `/usr/local/etc/mpv/` 或 `~/.config/mpv/`
(注意, .开头的文件夹是隐藏的).

`#` 为注释符, 表示之后内容为注释, 不会生效.
读者可按需自行修改. 不同系统的文件目录写法不同需要注意.

![mpv解压所得文件夹](https://i0.hdslb.com/bfs/article/eac14495617f8002ee8ac6c369f9a1b95d79e6c1.jpg@360w_359h_progressive.webp)

![portable_config文件夹](https://i0.hdslb.com/bfs/article/04d0628df883c84481b29fd6b10f4d6d98379f46.jpg@554w_300h_progressive.webp)

mpv.conf

```conf
hwdec=yes      #硬解, 改成no为软解
save-position-on-quit   #退出时记住播放状态
#screenshot-directory=D:\image #截屏文件保存路径
no-input-builtin-bindings  #禁用内建快捷键方案
sub-auto=fuzzy      #自动加载包含视频文件名的字幕
alang=en,eng,zh,chi,zho,chs,sc  #指定优先使用音轨
slang=zh,chi,zho,chs,sc,en,eng  #指定优先使用字幕轨
```

input.conf

```conf
MBTN_LEFT_DBL cycle fullscreen  #左键双击 全屏/退出全屏
MBTN_RIGHT    cycle pause   #右键 暂停/继续
WHEEL_UP      add volume 5   #滚轮向上 音量+5
WHEEL_DOWN    add volume -5   #滚轮向下 音量-5

ESC set fullscreen no    #ESC 退出全屏
SPACE cycle pause     #空格 暂停/继续
ENTER cycle fullscreen    #回车 全屏/退出全屏

UP  add volume 5     #方向键上 音量+5
DOWN  add volume -5     #方向键下 音量-5
LEFT  seek -5      #方向键左 后退5秒
RIGHT seek  5      #方向键右 前进5秒
Shift+RIGHT seek 87 exact   #前进87秒
Ctrl+UP add audio-delay -0.1  #音频延迟-0.1
Ctrl+DOWN add audio-delay +0.1  #音频延迟+0.1
Ctrl+LEFT add sub-delay -0.1  #字幕延迟-0.1
Ctrl+RIGHT  add sub-delay 0.1  #字幕延迟+0.1

PGUP playlist-prev     #播放列表上一个
PGDWN playlist-next     #播放列表下一个
HOME add chapter -1     #视频上一章节
END add chapter 1     #视频下一章节

t cycle ontop      #设置窗口最前
= screenshot video     #视频截图
z set speed 1.0      #播放速度设为1
c add speed 0.1      #播放速度+0.1
x add speed -0.1     #播放速度-0.1
v frame-back-step     #前一帧
b frame-step      #后一帧
n add sub-pos -1     #字幕上移1单位
m add sub-pos +1     #字幕下移1单位
, add sub-scale -0.05    #字幕缩小5%
. add sub-scale +0.05    #字幕放大5%
d cycle sub-visibility    #隐藏字幕/显示字幕
f cycle mute      #静音/取消静音
TAB script-binding stats/display-stats-toggle #打开/关闭播放信息
```

## OSC用法

On Screen Controller(OSC)是屏幕底部的简易控制面板,
如下图黄框区域, 能实现很多常用功能

![OSC位置](https://i0.hdslb.com/bfs/article/6456518a04eb571154173c459cd130cf29779c94.jpg@942w_531h_progressive.webp)

![OSC功能列表](https://i0.hdslb.com/bfs/article/20f661a44a33c8a005a4939824b1741d3668a745.png@698w_1277h_progressive.webp)
