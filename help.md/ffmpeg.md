# ffmpeg

[ FFmpeg 命令示例](https://zhuanlan.zhihu.com/p/67878761)
[在 Linux 中安装 FFmpeg](https://ostechnix.com/install-ffmpeg-linux/)

## 安装

[构建 FFMpeg (Windows MSYS2/MinGW-w64)](https://blog.csdn.net/luansxx/article/details/89632916)

+ 安装`MSYS2`: [MSYS2-installation](https://github.com/msys2/msys2/wiki/MSYS2-installation)
+ 安装 `MinGW-w64`: 打开`MSYS2 MinGW 64bit`(只是一个独立环境), 用  `pacman` 安装 `MinGW-w64`

```bash
pacman -S mingw-w64-x86_64-gcc
pacman -S mingw-w64-x86_64-libtool
```

安装其他工具

```bash
pacman -S yasm
pacman -S make
pacman -S autoconf automake
pacman -S pkg-config
pacman -S git
```

检查安装结果

```bash
$ which make gcc git
```

+ 下载`FFMpeg`源码

```bash
git clone https://github.com/FFmpeg/FFmpeg.git
```

+ 构建FFMpeg

```bash
./configure --prefix=/mingw64/usr/local
make && make install
```

## 基本使用

`FFmpeg` 命令的典型语法是：

```bash
ffmpeg [全局选项] {[输入文件选项] -i 输入_url_地址} ...  {[输出文件选项] 输出_url_地址} ...
```

现在我们将查看一些重要的和有用的 `FFmpeg` 命令. 

### 获取音频/视频文件信息

为显示你的媒体文件细节, 运行：

```bash
ffmpeg -i video.mp4
```

`FFmpeg` 将显示该媒体文件信息, 以及 `FFmpeg` 细节, 例如版本, 配置细节, 版权标记, 构建参数和库选项等等. 
如果你不想看 `FFmpeg` 标语和其它细节, 而仅仅想看媒体文件信息, 使用 `-hide_banner 标志`, 像下面. 

```bash
ffmpeg -i video.mp4 -hide_banner
```

### 视频转换格式

`FFmpeg` 是强有力的音频和视频转换器, 因此, 它能在不同格式之间转换媒体文件. 举个例子, 要转换 `mp4` 文件到 `avi` 文件, 运行：

```bash
ffmpeg -i video.mp4 video.avi
```

类似地, 你可以转换媒体文件到你选择的任何格式. 例如, 为转换 YouTube `flv` 格式视频为 `mpeg` 格式, 运行：

```bash
ffmpeg -i video.flv video.mpeg
```

如果你想维持你的源视频文件的质量, 使用 `-qscale 0` 参数：

```bash
ffmpeg -i input.webm -qscale 0 output.mp4
```

为检查 `FFmpeg` 的支持格式的列表, 运行：

```bash
ffmpeg -formats
```

### 音频转换格式

[ffmpeg 音频转码](https://www.cnblogs.com/tangchun/p/9013622.html)
[视频提取mp3和flac转mp3](https://www.cnblogs.com/dylanchu/p/14531207.html)

可以先查看支持的编解码格式, 以及它们的名称

```bash
ffmpeg -hide_banner -codecs | grep DEA
```

假如想转换成`.aac`格式

```bash
### 转换一个文件
ffmpeg -hide_banner -i xx.flac -codec:a aac -ar 44100 -ac 2 -b:a 320k -map_metadata 0 -id3v2_version 3 xx.aac

### 批量转换, 为了处理带有空格的文件名, 定义 shell 内域分隔符为 \n\b
SAVEIFS=$(echo -en "\0040\0011\0012"); # 默认的IFS
IFS=$(echo -en "\n\b");  #定义分词关键字为 \n\b newline, backspace
ffmt='wav'; #源文件的格式 
tfmt='aac'; # 目标文件的格式
declare -a flist=( $(find . -mindepth 1 -maxdepth 10 -type f -iname '*.'"$ffmt" ) )
rename.ul --verbose --no-act ".$ffmt" ".$tfmt" "${flist[@]}"  # -n 不执行操作, 只打印更改
## 先运行以上程序, 检查一下输出文件列表, 再执行转换
for file in ${flist[@]} ; do 
ffmpeg -hide_banner -i $file -codec:a $tfmt -ar 44100 -ac 2 -b:a 320k -map_metadata 0 -id3v2_version 3  ${file/%".$ffmt"/".$tfmt"}; done
IFS=$SAVEIFS;
```

***
将`flac`文件转换为`mp3`文件使用以下命令即可, 同时会将`flac`文件的`Vorbis`注释转换为`mp3`的`ID3v2`元数据

```bash
ffmpeg -hide_banner -i input.flac -codec:a mp3 -ar 44100 -ac 2 -b:a 320k -map_metadata 0 -id3v2_version 3 output.mp3
```

### 提取视频中的音频

转换一个视频文件到音频文件, 只需具体指明输出格式, 像 `.mp3`, 或 `.ogg`, 或其它任意音频格式. 
下面的命令将转换 `input.mp4` 视频文件到 `output.mp3` 音频文件. 

```bash
ffmpeg -i input.mp4 -vn output.mp3
```

此外, 也可以对输出文件使用各种各样的音频转换编码选项:

```bash
ffmpeg -i input.mp4 -vn -ar 44100 -ac 2 -b:a 320k -f mp3 output.mp3
```

### 更改视频文件的分辨率

如果你想设置一个视频文件为指定的分辨率, 你可以使用下面的命令：

```bash
ffmpeg -i input.mp4 -filter:v scale=1280:720 -c:a copy output.mp4
#或着
ffmpeg -i input.mp4 -s 1280x720 -c:a copy output.mp4
```

上面的命令将设置所给定视频文件的分辨率到 `1280×720`. 用`libx264`编码所有视频流并复制所有音频流. 

对于每个流, 最后一个匹配的`c`选项被使用, 所以

```bash
ffmpeg -i INPUT -map 0 -c copy -c:v:1 libx264 -c:a:137 libvorbis OUTPUT
```

将复制所有的数据流, 除了第二个视频, 它将用libx264编码, 以及第138个音频, 它将用`libvorbis`编码. 

### 流指定符

Stream specifiers

有些选项是按`流`(`stream`)应用的, 例如`比特率`或`编解码器`. 流指定符用于精确指定某个选项属于哪一个(或哪几个)流. 

流指定符是一个字符串, 一般附加在选项名称上, 用冒号隔开. 

+ 例如, `-codec:a:1 ac3`包含 `a:1`流指定符, 它与第`2`个音频流匹配. 因此, 它将为第`2`个音频流选择`ac3`编解码器. (编号从`0`开始)
+ 一个流指定符可以匹配几个流, 这样选项就会应用于所有的流. 例如, `-b:a 128k`中的流指定符匹配所有音频流. 
+ 一个空的流指定符匹配所有的流. 例如, `-codec copy`或`-codec: copy`会复制所有的流而不进行重新编码. 

流指定器的可能形式是:

+ `stream_index`: 匹配具有此索引的流. 例如, `-threads:1 4`将设置第二个流的线程数为`4`. 
如果`stream_index`被用作额外的流指定符(见下文), 那么它将从匹配的流中, 再次选择编号为`stream_index`的流. 
流的编号是基于`libavformat`检测到的流的顺序, 除非还指定了一个程序`ID`. 在这种情况下, 它是基于程序中的流在程序中的顺序. 

+ `stream_type[:additional_stream_specifier]`:`stream_type`是以下的一种. 
`v`或`V`代表视频, `a`代表音频, `s`代表字幕, `d`代表数据, `t`代表附件. `v`匹配所有的视频流, `V`只匹配没有附带`图片`, `视频缩略图`或`封面`的视频. 
如果使用了`additional_stream_specifier`, 那么它将匹配既具有这种`类型`又符合`additional_stream_specifier`的流. 
否则, 它将匹配所有属于指定的类型的流. 

+ `p:program_id[:additional_stream_specifier]`:匹配在程序中, `ID`为`program_id`的流. 
如果使用了`additional_stream_specifier`, 那么它匹配既是程序的一部分又符合`additional_stream_specifier`的流. 

+ `#stream_id`或`i:stream_id`: 通过流`ID`(例如`MPEG-TS`容器中的`PID`)匹配流. 

+ `m:key[:value]`:匹配具有指定`metadata tag key`的流. 如果没有给定值, 则匹配包含给定标签的任何值的流. 
+ `u`: 匹配具有可用配置的流, 编解码器必须被定义, 而且视频尺寸或音频采样率等基本信息必须存在. 

注意, 在`ffmpeg`中, 通过`metadata`进行匹配只能对输入文件正常工作. 

## ffmpeg 手册

[什么是muxer, demuxer](https://www.cnblogs.com/general001/articles/2517568.html)

`ffmpeg`是一个非常快速的视频和音频转换器, 也可以从现场的音频/视频源抓取. 它还可以在任意采样率之间进行转换, 并通过一个高质量的多相滤波器在运行中调整视频的大小. 

`ffmpeg`从读取任意数量的`输入文件`,可以是普通文件, 管道, 网络流, 抓取设备等, 由`-i`选项指定, 并写到任意数量的`输出文件`. 
这些文件是由`plain output url`指定. 在命令行中发现的任何不能被解释为选项的东西都被认为是一个`output url`. 

原则上, 每个输入或输出网址可以包含任何数量的不同类型的流(视频/音频/字幕/附加内容/数据). 允许的流的数量和/或类型可能受到容器格式的限制. 
选择哪些输入映射到哪些输出, 可以自动完成, 也可以使用`-map`选项(见`Stream selection`章节). 

要在选项中指定具体输入文件, 你必须使用它们的索引, 从`0`开始. 例如, 第一个输入文件是`0`, 第二个是`1`, 等等. 
同样, 一个文件中的流也用它们的索引来指代. 例如, `2:3`指的是第三个输入文件中的第四个流. 也可参见`Stream specifiers`一章. 

一般来说, 选项会应用于下一个指定的文件. 因此, 顺序很重要, 你可以在命令行上多次出现同一个选项. 
每次出现的选项都会应用到下一个输入或输出文件.  这个规则的例外情况是全局选项(例如：`verbosity level`), 应该先指定. 

不要混合输入和输出文件 -- 首先指定所有的输入文件, 然后是所有的输出文件. 也不要混合属于不同文件的选项. 
所有的选项只适用于下一个输入或输出文件, 并且在不同的文件之间被重置.  

`muxer`将视频文件, 音频文件和字幕文件合并为某一个视频格式. 如, 可将`a.avi`, `a.mp3`, `a.srt`用`muxer`合并为`mkv`格式的视频文件. 
`demuxer`是拆分这些文件的. 

+ 将输出文件的视频比特率设置为`64 kbit/s`. 

```bash
ffmpeg -i input.avi -b:v 64k -bufsize 64k output.avi
```

+ 要强制输出文件的帧率为`24 fps`. 

```bash
ffmpeg -i input.avi -r 24 output.avi
```

+ 强制输入文件的帧率(仅对`raw`格式有效)为`1 fps`, 输出文件的帧率为`24 fps`. 

```bash
ffmpeg -r 1 -i input.m2v -r 24 output.avi
```

对于`raw`输入文件可能需要格式选项, `-f`. 

***

+ `-f`:  输出文件格式. 输入文件一般会自动检测, 输出文件会从后缀名猜测, 所以一般不需要. 在我们的实例中, 它是 `mp3` 格式. 
+ `-y (global)`: 覆盖输出文件而不询问.
+ `-vn`:表明我们已经在输出文件中禁用视频录制. 
+ `-b:v`: 视频比特率.
+ `-ar[:流指定符] 频率 (输入/输出,per-stream)`: 设置输出文件的音频频率, 默认和输入相同. 通常使用的值是`22050 Hz`, `44100 Hz`, `48000 Hz`. 
+ `-ac[:流指定符] 通道数目 (输入/输出,per-stream)`:  设置音频通道的数目, 默认和输入相同. 
+ `-b:a`:  表明音频比特率(bitrate). 
+ `-acodec codec (输入/输出)`: 设置音频编码. 它是`-codec:a`的别名.
+ `-atag fourcc/tag (output)`: 强制设定音频`tag/fourcc`. 它是`-tag:a`的别名.
+ `-c[:流指定符] codec (输入/输出,per-stream)` or  `-codec[:stream_specifier] codec (input/output,per-stream)`:
当在输入文件前使用时, 为一个或多个数据流选择`decoder`. 当在输出文件前使用时, 选择`编码器`. `codec`是一个`解码器`/`编码器`的名称, 
或者取特殊值 `copy`来表示该流仅仅输, 不被重新编码. 

例如:

```bash
ffmpeg -i INPUT -map 0 -c:v libx264 -c:a copy OUTPUT
```

***
通用选项: 这些选项在ff*工具中是共享的. 

+ `-L`:显示许可证. 
+ `-h, -?, -help, --help [arg]`: 显示帮助. 可以指定一个可选的参数来打印关于一个特定项目的帮助. 如果没有指定参数, 只显示基本(非高级)工具选项. arg的可能值是:
  + `long`: 除了基本工具选项外, 打印高级工具选项. 
  + `full`:打印完整的选项列表, 包括编码器, 解码器, 解复用器, 复用器, 过滤器等的共享和私有选项. 
  + `decoder=decoder_name`:打印关于名为`decoder_name`的解码器的详细信息. 使用`-decoders`选项可以获得所有解码器的列表. 
  + `encoder=encoder_name`:打印关于名为`encoder_name`的编码器的详细信息. 使用`-encoders`选项可以获得所有编码器的列表. 
  + `demuxer=demuxer_name`:打印关于名为`demuxer_name`的解复用器的详细信息. 使用`-formats`选项可以得到所有`demuxer`和`muxers`的列表. 
  + `muxer=muxer_name`:打印关于名为`muxer_name`的多路复用器的详细信息. 使用`-formats`选项获得所有复用器和解复用器的列表. 
  + `filter=filter_name`:打印关于名为`filter_name`的过滤器的详细信息. 使用`-filters`选项可以得到所有过滤器的列表. 
  + `bsf=bitstream_filter_name`:打印关于比特流过滤器名称`bitstream_filter_name`的详细信息.  使用`-bsfs`选项可以得到所有比特流过滤器的列表. 
  + `decoder=decoder_name`:打印名为`decoder_name`的解码器的详细信息. 使用`-decoders`选项可以获得所有解码器的列表. 
  + `encoder=encoder_name`:打印关于名为`encoder_name`的编码器的详细信息. 使用`-encoders`选项可以获得所有编码器的列表. 
  + `demuxer=demuxer_name`:打印关于名为`demuxer_name`的解复用器的详细信息. 使用`-formats`选项可以得到所有`demuxer`和`muxers`的列表. 
  + `muxer=muxer_name`:打印关于名为`muxer_name`的多路复用器的详细信息. 使用`-formats`选项获得所有复用器和解复用器的列表. 
  + `filter=filter_name`:打印关于名为`filter_name`的过滤器的详细信息. 使用`-filters`选项可以得到所有过滤器的列表. 
  + `bsf=bitstream_filter_name`:打印关于比特流过滤器名称`bitstream_filter_name`的详细信息.  使用`-bsfs`选项可以得到所有比特流过滤器的列表. 

***

+ `-devices`: 显示可用的设备. 
+ `-codecs`: 显示`libavcodec`已知的所有编解码器. 请注意, 在本文档中使用的`codec`, 更正式的名称为`media bitstream format`, `媒体比特流格式`. 
+ `-protocols`:显示可用的协议. 
+ `-pix_fmts`:显示可用的像素格式. 
+ `-sample_fmts`:显示可用的样本格式. 
+ `-layouts`:显示通道名称和标准通道布局. 
+ `-colors`:显示公认的颜色名称. 
+ `-sources device[,opt1=val1[,opt2=val2]...] `:显示输入设备的自动检测的源. 有些设备可能提供与系统相关的源名称, 无法自动检测. 不能假设返回的列表总是完整的. 
ffmpeg -sources pulse,server=192.168.0.4
+ `-sinks device[,opt1=val1[,opt2=val2] ...]`:显示自动检测到的输出设备的汇.  一些设备可能会提供与系统相关的汇流排名称, 不能被自动检测到.  不能假设返回的列表总是完整的.    
ffmpeg -sinks pulse,server=192.168.0.4
+ `-loglevel [flags+]loglevel | -v [flags+]loglevel`:设置`library`所使用的日志级别和标志. 

***
编解码器样式说明:

+ `D.....` = 支持解码
+ `.E....` = 支持编码
+ `..V...` = 视频 codec, `..A...` = 音频 codec, ` ..S...` = 字幕 codec
+ `...I..` = Intra frame-only codec, 仅限帧内的编解码器
+ `....L.` = 有损压缩
+ `.....S` = 无损压缩

如查看是否支持`mp3`编解码:

```bash
ffmpeg -hide_banner -codecs | grep -i mp3
```

***

+ `-map_metadata[:metadata_spec_out] infile[:metadata_spec_in] (output,per-metadata)`

使用`infile`的元数据信息设置下一个输出文件中的元数据. 注意, 这些是按文件索引(基于0), 而不是按文件名. \
可选的`metadata_spec_in/out`参数指定要拷贝的元数据.  一个元数据可以有以下几种形式. 

+ `g`: 全局元数据, 即适用于整个文件的元数据
+ `s[:stream_spec]`:`stream_spec`是一个流指定器, 在 Stream specifiers 一章中有描述. 
在 input metadata specifier 中, 第一个匹配的流将被复制过来. 在一个out metadata specifier中, 所有匹配的流都被复制. 
+ `c:chapter_index`: `chapter_index`是基于零的章节索引. 
+ `p:program_index`: 每个程序的元数据, `program_index`是基于零的程序索引. 

如果省略了元数据指定符, 则默认为全局. 默认情况下, 全局元数据从第一个输入文件中复制, 每个流和每个章节的元数据与流/章节一起被复制. 
这些默认的映射可以通过创建任何相关类型的映射来禁用. 一个负的文件索引可以用来创建一个假的映射(dummy mapping), 可以用来禁止自动复制. 

例如, 从输入文件的第一个流复制元数据到输出文件的全局元数据. 

```bash
ffmpeg -i in.ogg -map_metadata 0:s:0 out.mp3
```

相反的, 复制全局元数据到所有的音频流. 

```bash
ffmpeg -i in.mkv -map_metadata:s:a 0:g out.mkv
```

注意, 在这个例子中, 简单的`0`也可以工作, 因为全局元数据是默认的. 

***

+ `AVOptions`: 这些选项是由`libavformat`, `libavdevice`和`libavcodec`库直接提供的. 要查看可用的`AVOptions`的列表, 请使用`-help`选项. 它们被分成两类. 
  + generic: 这些选项可以为任何容器, 编解码器或设备设置. 通用选项被列在`containers/devices`的`AVFormatContext`选项下, 以及`codecs`的`AVCodecContext`选项下. 
  + private: 这些选项是特定于给定的容器, 设备或编解码器的. 私有选项被列在它们相应的`containers/devices/codecs`下面. 

例如, 要给一个`MP3`文件写一个`ID3v2.3`头而不是默认的`ID3v2.4`, 使用`MP3`复用器(muxer)的`id3v2_version`私有选项. 

```bash
ffmpeg -i input.flac -id3v2_version 3 out.mp3
```

所有的编解码器`AVOptions`都是按流计算的, 因此应在其上附加一个流指定符. 

```bash
ffmpeg -i multichannel.mxf -map 0:v:0 -map 0:a:0 -map 0:a:0 -c:a:0 ac3 -b:a:0 640k -ac:a:1 2 -c:a:1 aac -b:2 128k out.mp4
```

***

+ `-vsync parameter`: 视频同步方法.  由于兼容性的原因, 旧的值可以被指定为数字.  新增加的值必须始终指定为字符串. 
  + `0`,`passthrough`:每一帧的时间戳都从`demuxer`传到`muxer`中. 
  + `1`,`cfr`: 帧将被复制和丢弃, 以准确实现所要求的恒定帧率. 
  + `2`,`vfr`: 帧将带着它们的时间戳通过或丢弃, 以防止`2`个帧具有相同的时间戳. 
  + `drop`: 与`passthrough`一样, 但破坏所有的时间戳, 使`muxer`根据帧速率生成新的时间戳. 
  + `-1`,`auto`:根据`muxer`的能力在`1`和`2`之间进行选择. 这是默认的方法. 

注意, 在这之后, 时间戳可能会被`muxer`进一步修改.  例如, 在格式选项`avoid_negative_ts`被启用的情况下. 
通过`-map`, 你可以选择从哪个流中获取时间戳. 你可以不改变视频或音频, 并将其余的流同步到不改变的流. 

***

+ `-pix_fmt[:stream_specifier] format (输入/输出,per-stream)`: 设置`pixel`格式. 使用`-pix_fmts`来显示所有支持的像素格式.  

如果不能选择所选的像素格式,`ffmpeg`将打印一个警告, 并选择编码器支持的最佳像素格式. 
如果`pix_fmt`的前缀是 `+`, 如果不能选择所要求的像素格式, `ffmpeg`将以错误退出, 并禁用滤波图内部的自动转换. 
如果`pix_fmt`是一个单独的 `+`, `ffmpeg`选择与输入(或图形输出)相同的像素格式, 自动转换功能被禁用. 

### avconv

[使用avconv录制视频和音频](https://linux.cn/article-4323-1.html)

`Libav`是一款跨平台的工具库, 能够用来处理多媒体文件, 流和协议. 它最初是源自`ffmpeg`. `Libav`带有一些工具, 比如：

+ `Avplay`: 一款视频音频播放器. 
+ `Avconv`: 能够记录多个设备输入源的一个多媒体转换器和视频音频录制器. 
+ `Avprobe`: 一个连接多媒体文件流并且返回关于这个文件流的统计信息的工具. 
+ `Libavfilter`: 一个`Libav`工具的过滤器(`filtering`)API. 

列出所有的音频输入源：

```bash
arecord -l
```

## 音乐格式转换

### shntool

[Ubuntu下用cue文件对ape和wav文件自动分轨](https://www.cnblogs.com/pandachen/p/4557573.html)
[shntool download](http://shnutils.freeshell.org/shntool/)

+ `iconv`: 将文本从一种字符编码转换成另一种. 语法：
    iconv [options] [-f from-encoding] [-t to-encoding] [inputfile]...

```bash
sudo apt-get install flac shntool ffmpeg
```

需要 `shntool` 以分割音频文件.  对于`ISO`内的`CD`镜像或其它原始数据则需要 `bchunk` . 
`shntool`原生支持`WAV`格式的输入与输出. 对于其它格式则需要对应的编解码器如 `flac`,`mac` 或`wavpack`. 
标记音频文件则需要其它软件 `cuetools`, `mp3info` 或 `vorbis-tools`. 

+ `shntool`:`shntool`是一个命令行工具, 用于查看和/或修改`WAVE`数据和属性.  它在几种不同的操作模式下运行, 并支持各种无损音频格式. 

SYNOPSIS:

    shntool mode ...
    shntool [CORE OPTION]

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

#### 无模式

在没有模式的情况下运行时, `shntool` 接受下列选项. 

+ `-m`: 显示详细的`mode`模块信息
+ `-f`: 显示详细的`format`模块信息
+ `-a`: 显示默认的格式模块参数
+ `-v`: 显示版本信息
+ `-h`: 显示帮助屏幕

#### 所有模式

所有模式都支持以下选项:

+ `-D` :打印调试信息
+ `-F file`:指定一个包含要处理的文件名列表的文件.  这优先于在命令行或终端上指定的任何文件. 
注意：大多数模式将接受来自单一来源的输入文件名, 根据以下优先顺序：由`-F`选项指定的文件, 然后是命令行上的文件名, 然后是从终端输入的文件名

+ `-H`: 以`h:mm:ss.{ff,nnn}`格式打印时间, 而不是`m:ss.{ff,nnn}`. 
+ `-P type`: 指定进度指示器类型, 是下列之一,  `{pct, dot, spin, face, none}`.
`pct`显示每个操作的完成百分比, `face`显示每个操作的进度, 显示六个表情符号, 随着操作接近完成, 表情符号会变得越来越高兴. 
`none`不显示任何进度完成信息.  默认是`pct`. 

+ `-h`: 显示此`mode`的帮助屏幕.
+ `-i fmt`: 指定输入文件`格式解码器/参数`.  其格式为 `fmt decoder [arg1 ... argN]`, 并且必须用引号包围.  
如果给出了参数, 那么其中一个参数必须包含`%f`, 它将被在输入文件名代替. 例如

    -i 'shn shorten-2.3b'(使用官方的 shorten-2.3b, 而不是后来的版本；不修改默认参数
    -i 'shn shorten -x -d 2048 %f -' (强迫 shorten 跳过每个文件的前 2048 字节)

`-q`: 抑制非关键性输出(安静模式).  除了错误或调试信息(如果指定的话), 通常进入`stderr`的输出将不被显示. 
`-r val`: 重新排序输入文件. `val` 是以下之一, `{ask, ascii, natural, none}`, 默认为`natural`. 
`-v`: 显示版本信息
`-w`: 抑制警告
`--`: 表示它后面的所有内容都是文件名

#### 输出模式

任何创建输出文件的模式都支持以下选项. 

+ `-O val`: 覆盖现有文件？`val`是以下选项之一,`{ask, always, never}`.  默认是询问. 
+ `-a str`: 在文件名的基础部分(不包含拓展名)之前添加`str`.
+ `-z str`: 在基础部分(不包含拓展名)之后添加`str`
+ `-d dir`: 指定输出目录
+ `-o str`: 指定输出文件格式扩展名, 编码器/参数.  
格式是：`fmt [ext=abc] [encoder [arg1 ... argN (%f = filename)]]`, 并且必须用引号包围.  如果给出了参数, 那么其中一个参数必须包含`%f`, 它将被替换为输出文件名.  例:

    -o 'shn shorten -v2 - %f' (创建没有寻址表的简短文件)
    -o 'flac flake - %f' (使用替代的flac编码器)
    -o 'aiff ext=aif' (用'aif'覆盖'aiff'的默认aiff扩展)
    -o 'cust ext=mp3 lame --quiet - %f' (使用lame创建mp3文件)

#### conv mode

`conv` 模式,  用法:

  shntool conv [OPTIONS] [files]

模式的特定选项:

+ `-h`: 显示帮助屏幕
+ `-t`: 从终端读取`WAVE`数据

### CUE 分割

[CUE 分割](https://wiki.archlinux.org/title/CUE_Splitting)

#### 分割

使用 `shnsplit` 命令分割 `.wav` 文件：

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

`shnsplit` 支持许多无损格式(参见 `shntool`).  以 `.flac `格式为例：

```bash
shnsplit -f file.cue -o flac file.flac
```

输出格式, 包括编码器, 可用 `-o` 命令指定:

```bash
shnsplit -f file.cue -o "flac flac -s -8 -o %f -" file.flac
```

可用 `shntool -a` 命令查看 `shntool` 原生支持的格式和编码器. 如果没有原生支持, 可以手动指定. 例如输出`ogg`格式：

```bash
shnsplit -f file.cue -o "cust ext=ogg oggenc -b 192 -o %f -" file.ape
```

***

```bash
shntool.exe split -f 'Rebirth Story4_Ruby.cue' -t "%t" 'Rebirth Story4_Ruby.wav'
shntool.exe split -f 'Rebirth Story4_Sapphire.cue' -t "%t" 'Rebirth Story4_Sapphire.wav'
```

#### 标记

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

### netease 网易云音乐转码 ncm to mp3

网易云音乐格式转换
[anonymous5l/ncmdumpPublic](https://github.com/anonymous5l/ncmdump)
[ncmdump-gui](https://github.com/anonymous5l/ncmdump-gui)

命令行下使用, 先安装依赖库 `sudo apt install libtag1-dev`,  `make` 编译即可.

### pydub 模块

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
