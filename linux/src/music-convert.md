# 音乐格式转换

## avconv

[使用avconv录制视频和音频](https://linux.cn/article-4323-1.html)

`Libav`是一款跨平台的工具库, 能够用来处理多媒体文件, 流和协议. 它最初是源自`ffmpeg`. `Libav`带有一些工具, 比如:

+ `Avplay`: 一款视频音频播放器.
+ `Avconv`: 能够记录多个设备输入源的一个多媒体转换器和视频音频录制器.
+ `Avprobe`: 一个连接多媒体文件流并且返回关于这个文件流的统计信息的工具.
+ `Libavfilter`: 一个`Libav`工具的过滤器(`filtering`)API.

列出所有的音频输入源:

```bash
arecord -l
```

## shntool

[Ubuntu下用cue文件对ape和wav文件自动分轨](https://www.cnblogs.com/pandachen/p/4557573.html)
[shntool download](http://shnutils.freeshell.org/shntool/)

+ `iconv`: 将文本从一种字符编码转换成另一种. 语法:
    iconv [options] [-f from-encoding] [-t to-encoding] [inputfile]...

```bash
sudo apt-get install flac shntool ffmpeg
```

需要 `shntool` 以分割音频文件.  对于`ISO`内的`CD`镜像或其它原始数据则需要 `bchunk` .
`shntool`原生支持`WAV`格式的输入与输出. 对于其它格式则需要对应的编解码器如 `flac`,`mac` 或`wavpack`.
标记音频文件则需要其它软件 `cuetools`, `mp3info` 或 `vorbis-tools`.

+ `shntool`:`shntool`是一个命令行工具, 用于查看和/或修改`WAVE`数据和属性.  它在几种不同的操作模式下运行, 并支持各种无损音频格式.
语法:

```bash
shntool mode ...
shntool [CORE OPTION]
```

`shntool` 由三部分组成--它的`core`, `mode`模块和`format`模块.  这有助于使代码更容易维护, 以及帮助其他程序员开发新功能.
发行版中包含一个名为`modules.howto`的文件, 描述了如何创建一个新的模式或格式模块, 供有兴趣的人使用.

`Mode`模块: `shntool`通过使用模式模块对`WAVE`数据执行各种功能.  `shntool`的核心只是一个围绕着模式模块的包装器.
事实上, 当`shntool`运行时, 它的第一个参数是有效的模式. 它对于特定的模式运行主程序然后退出. `shntool`有几个内置的模式, 描述如下:

+ `len`: 显示`PCM WAVE`数据的长度, 大小和属性
+ `fix`: 修复CD质量的`PCM WAVE`数据的扇形边界问题
+ `hash`: 计算`PCM WAVE`数据的MD5或SHA1指纹.
+ `pad`: 将CD(hyquality)文件在扇区边界上不对齐的地方用静音垫起来.
+ `join`: 将多个文件的`PCM WAVE`数据连接成一个文件
+ `split`: 将`PCM WAVE`数据从一个文件分割成多个文件
+ `cat`: 将一个或多个文件的`PCM WAVE`数据写到终端.
+ `cmp`: 对比两个文件中的`PCM WAVE`数据
+ `cue`: 从一组文件中生成一个`CUE`表或分割点
+ `conv`: 将文件从一种格式转换为另一种格式
+ `info`: 显示`PCM WAVE`数据的详细信息
+ `strip`: 剥离额外的`RIFF`块和/或写出规范的文件头
+ `gen`: 生成含有静音的CD质量的`PCM WAVE`数据文件
+ `trim`: 将`PCM WAVE`的静音从文件的末端剪掉

### 无模式

在没有模式的情况下运行时, `shntool` 接受下列选项.

+ `-m`: 显示详细的`mode`模块信息
+ `-f`: 显示详细的`format`模块信息
+ `-a`: 显示默认的格式模块参数
+ `-v`: 显示版本信息
+ `-h`: 显示帮助屏幕

### 所有模式

所有模式都支持以下选项:

+ `-D` :打印调试信息
+ `-F file`:指定一个包含要处理的文件名列表的文件.  这优先于在命令行或终端上指定的任何文件.
注意: 大多数模式将接受来自单一来源的输入文件名, 根据以下优先顺序: 由`-F`选项指定的文件, 然后是命令行上的文件名, 然后是从终端输入的文件名

+ `-H`: 以`h:mm:ss.{ff,nnn}`格式打印时间, 而不是`m:ss.{ff,nnn}`.
+ `-P type`: 指定进度指示器类型, 是下列之一,  `{pct, dot, spin, face, none}`.
`pct`显示每个操作的完成百分比, `face`显示每个操作的进度, 显示六个表情符号, 随着操作接近完成, 表情符号会变得越来越高兴.
`none`不显示任何进度完成信息.  默认是`pct`.

+ `-h`: 显示此`mode`的帮助屏幕.
+ `-i fmt`: 指定输入文件`格式解码器/参数`.  其格式为 `fmt decoder [arg1 ... argN]`, 并且必须用引号包围.
如果给出了参数, 那么其中一个参数必须包含`%f`, 它将被在输入文件名代替. 例如

    -i 'shn shorten-2.3b'(使用官方的 shorten-2.3b, 而不是后来的版本; 不修改默认参数
    -i 'shn shorten -x -d 2048 %f -' (强迫 shorten 跳过每个文件的前 2048 字节)

`-q`: 抑制非关键性输出(安静模式).  除了错误或调试信息(如果指定的话), 通常进入`stderr`的输出将不被显示.
`-r val`: 重新排序输入文件. `val` 是以下之一, `{ask, ascii, natural, none}`, 默认为`natural`.
`-v`: 显示版本信息
`-w`: 抑制警告
`--`: 表示它后面的所有内容都是文件名

### 输出模式

任何创建输出文件的模式都支持以下选项.

+ `-O val`: 覆盖现有文件? `val`是以下选项之一,`{ask, always, never}`.  默认是询问.
+ `-a str`: 在文件名的基础部分(不包含拓展名)之前添加`str`.
+ `-z str`: 在基础部分(不包含拓展名)之后添加`str`
+ `-d dir`: 指定输出目录
+ `-o str`: 指定输出文件格式扩展名, 编码器/参数.
格式是: `fmt [ext=abc] [encoder [arg1 ... argN (%f = filename)]]`, 并且必须用引号包围.  如果给出了参数, 那么其中一个参数必须包含`%f`, 它将被替换为输出文件名.  例:

    -o 'shn shorten -v2 - %f' (创建没有寻址表的简短文件)
    -o 'flac flake - %f' (使用替代的flac编码器)
    -o 'aiff ext=aif' (用'aif'覆盖'aiff'的默认aiff扩展)
    -o 'cust ext=mp3 lame --quiet - %f' (使用lame创建mp3文件)

### conv mode

`conv` 模式,  用法:

  shntool conv [OPTIONS] [files]

模式的特定选项:

+ `-h`: 显示帮助屏幕
+ `-t`: 从终端读取`WAVE`数据

## CUE 分割

[CUE 分割](https://wiki.archlinux.org/title/CUE_Splitting)

### 分割

使用 `shnsplit` 命令分割 `.wav` 文件:

```bash
shnsplit -f file.cue file.wav
```

使用 `bchunk` 命令分割 `.bin` 文件并转换为 `.wav` 格式:

```bash
bchunk -v -w file.bin file.cue out
```

输出文件名可利用 `-t` 进行格式化 (`%p` 艺术家, `%a` 专辑, `%t` 标题, 以及 `%n` 轨数):

```bash
shnsplit -f file.cue -t "%n %t" file.wav
```

`shnsplit` 支持许多无损格式(参见 `shntool`).  以 `.flac `格式为例:

```bash
shnsplit -f file.cue -o flac file.flac
```

输出格式, 包括编码器, 可用 `-o` 命令指定:

```bash
shnsplit -f file.cue -o "flac flac -s -8 -o %f -" file.flac
```

可用 `shntool -a` 命令查看 `shntool` 原生支持的格式和编码器. 如果没有原生支持, 可以手动指定. 例如输出`ogg`格式:

```bash
shnsplit -f file.cue -o "cust ext=ogg oggenc -b 192 -o %f -" file.ape
```

***

```bash
shntool.exe split -f 'Rebirth Story4_Ruby.cue' -t "%t" 'Rebirth Story4_Ruby.wav'
shntool.exe split -f 'Rebirth Story4_Sapphire.cue' -t "%t" 'Rebirth Story4_Sapphire.wav'
```

### 标记

需要 `cuetools` 来运行 `cuetag.sh`.

可以用以下命令复制 `.cue` 的元数据至 `.mp3` 文件.

```bash
cuetag.sh file.cue *.mp3
```

或指定某些文件:

```bash
cuetag.sh file.cue track01.mp3 track02.mp3 track03.mp3 track04.mp3
```

`cuetag.sh` 支持 `.mp3` 文件的 `id3` 标签和 `.ogg` ,  `.flac` 文件的 `vorbis` 标签.

对于有损格式, `mp3splt-gtk` 或 `mp3splt` 可用于直接切割`mp3`文件以避免重编码带来的音质下降.

## netease 网易云音乐转码 ncm to mp3

网易云音乐格式转换
[anonymous5l/ncmdumpPublic](https://github.com/anonymous5l/ncmdump)
[ncmdump-gui](https://github.com/anonymous5l/ncmdump-gui)

命令行下使用, 先安装依赖库 `sudo apt install libtag1-dev`,  `make` 编译即可.

## pydub 模块

[pydub的中文文档](https://blog.csdn.net/baidu_29198395/article/details/86694365)
[ jiaaro/pydub ](https://github.com/jiaaro/pydub)

使用`pydub`调用`ffmpeg`库进行转码.

```python
from pydub import AudioSegment
song = AudioSegment.from_wav("/home/tom/test/fhcq01.wav")
song.export("pytrans.aac",format="adts", parameters=["-codec:a","aac","-ar","44100","-ac","2","-b:a","320k","-map_metadata","0","-id3v2_version","3"])
song.export("pytrans.mp3", format="mp3", parameters=["-codec:a","mp3","-ar","44100","-ac","2","-b:a","320k","-map_metadata","0","-id3v2_version","3"])
# 使用预设MP3质量0(相当于lame -V0), lame是个MP3编码器, -V设置的是VBR压缩级别,品质从0到9依次递减(译者注)
song.export("pytrans.mp3", format="mp3", parameters=["-q:a", "0"])
```

从`mp3`或者`wav`转换成`aac`的时候, 可能会报错:

    Requested output format 'aac' is not a suitable output format

参考[Converting MP3 to AAC](https://superuser.com/questions/554962/converting-mp3-to-aac-and-outputting-to-stdout), ffmpeg 的输出中有提示

    Output #0, adts, to 'o.aac':

这里, `ffmpeg`告诉我们, 它在使用名为`adts`的格式来生成一个`.aac`文件. 所以应该使用`-f adts`而不是`-f aac`, 例如

```bash
ffmpeg.exe -i s.mp3 -f adts -
```

***
[ADTS头之于AAC](https://www.jianshu.com/p/b5ca697535bd)

AAC音频文件的每一帧都由一个`ADTS`头(Audio Data Transport Stream)和`AAC ES`(AAC音频数据)组成.

`ADTS`头包含了`AAC`文件的采样率, 通道数, 帧数据长度等信息.
`ADTS`头分为固定头信息和可变头信息两个部分, 固定头信息在每个帧中的是一样的, 可变头信息在各个帧中并不是固定值.
`ADTS`头一般是`7`个字节(`(28+28)/8`)长度, 如果需要对数据进行`CRC`校验, 则会有`2`个`Byte`的校验码, 所以`ADTS`头的实际长度是`7`个字节或`9`个字节.
