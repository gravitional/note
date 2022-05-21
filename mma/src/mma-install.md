# mathematica 安装,维护

## Mathematica 的安装、激活、入门、常见问题

[tieba mma 激活指南](https://tiebamma.github.io/InstallTutorial/)
[Mathematica Key Generator Online ](https://ibug.io/blog/2019/05/mathematica-keygen/)

## linux mathematica 只打开单个前端

定义一个别名,用`singleLaunch`选项

`alias mma='mathematica -singleLaunch'`

`-singleLaunch [file]`

Allows only one copy of the front end to exist per DISPLAY setting and directs the first instance of the front end to open file.

## Linux, 卸载 Mathematica

如要卸载 `Mathematica`,需删除下列目录.请备份这些目录下任何需要保存的文件:

+ `/usr/local/Wolfram/Mathematica/`
+ `/usr/share/Mathematica/`
+ `~/.Mathematica/`
+ `~/.Wolfram/`
+ `~/.cache/Wolfram/`

命令行下运行 `wolframscript` 脚本出错,是因为

`~/.config/Wolfram/WolframScript/WolframScript.conf `中的`wolfram`环境变量影响了 `wolframscript` 的运行,清除失效的路径就可以了

### 高分辨率下字体太小

[How to increase the font size in Context-Sensitive Autocompletion?](https://mathematica.stackexchange.com/questions/216602/how-to-increase-the-font-size-in-context-sensitive-autocompletion)

创建文件`~/.Mathematica/FrontEnd/frontend.css`,在其中追加配置

```css
* { font-size:14pt; font-family:"Griffy"; }
```

### 没有启动图标

[Ubuntu下为安装的软件创建启动图标](https://blog.csdn.net/baidu_41704597/article/details/95043430)

装好后, 可能没有启动图标, 这时需要自己创建一个,放在`/usr/share/applications`中, 例如名称为`wolfram-mathematica12.desktop`, 内容参考:

```conf
[Desktop Entry]
Version=2.0
Type=Application
Name=Mathematica 12
Comment=Technical Computing System
TryExec=/usr/local/Wolfram/Mathematica/12.2/Executables/Mathematica
Exec=/usr/local/Wolfram/Mathematica/12.2/Executables/Mathematica %F
Icon=wolfram-mathematica
MimeType=application/mathematica;application/x-mathematica;application/vnd.wolfram.nb;application/vnd.wolfram.cdf;application/vnd.wolfram.player;application/vnd.wolfram.mathematica.package;application/vnd.wolfram.wl;x-scheme-handler/wolfram+cloudobject;x-scheme-handler/wolframmathematica+cloudobject;
```

主要修改`Exec,Icon`的内容, 替换成程序的位置, 在笔记本中运行`$InstallationDirectory`即可得到.
其中`MimeType`(Multipurpose Internet Mail Extensions) 是描述消息内容类型的因特网标准.
MIME 消息能包含文本, 图像, 音频, 视频以及其他应用程序专用的数据.
[MIME 参考手册](https://www.w3school.com.cn/media/media_mimeref.asp)

12.3.1 版本的例子

```conf
[Desktop Entry]
Version=1.0
Type=Application
Name=Mathematica 12
Comment=Technical Computing System
TryExec=/usr/local/Wolfram/Mathematica/12.3/Executables/Mathematica
Exec=/usr/local/Wolfram/Mathematica/12.3/Executables/Mathematica --name M-12.3 %F
Icon=wolfram-mathematica
StartupWMClass=M-12.3
MimeType=application/mathematica;application/x-mathematica;application/vnd.wolfram.nb;application/vnd.wolfram.cdf;application/vnd.wolfram.player;application/vnd.wolfram.mathematica.package;application/vnd.wolfram.wl;x-scheme-handler/wolfram+cloudobject;x-scheme-handler/wolframmathematica+cloudobject;
```
