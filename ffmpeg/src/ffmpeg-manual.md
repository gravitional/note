# ffmpeg 手册

[什么是muxer, demuxer](https://www.cnblogs.com/general001/articles/2517568.html)

`ffmpeg`是一个非常快速的视频和音频转换器, 也可以从现场的音频/视频源抓取.
它还可以在任意采样率之间进行转换, 并通过一个高质量的多相滤波器在运行中调整视频的大小.

`ffmpeg`从读取任意数量的`输入文件`,
可以是普通文件, 管道, 网络流, 抓取设备等, 由`-i`选项指定, 并写到任意数量的`输出文件`.
这些文件是由`plain output url`指定.
在命令行中发现的任何不能被解释为选项的东西都被认为是一个`output url`.

原则上, 每个输入或输出网址可以包含任何数量的不同类型的流(视频/音频/字幕/附加内容/数据).
允许的流的数量和/或类型可能受到容器格式的限制.
选择哪些输入映射到哪些输出, 可以自动完成, 也可以使用`-map`选项(见`Stream selection`章节).

要在选项中指定具体输入文件, 你必须使用它们的索引, 从`0`开始.
例如, 第一个输入文件是`0`, 第二个是`1`, 等等.
同样, 一个文件中的流也用它们的索引来指代.
例如, `2:3`指的是第三个输入文件中的第四个流.
也可参见`Stream specifiers`一章.

一般来说, 选项会应用于下一个指定的文件.
因此, 顺序很重要, 你可以在命令行上多次出现同一个选项.
每次出现的选项都会应用到下一个输入或输出文件.
这个规则的例外情况是全局选项(例如: `verbosity level`), 应该先指定.

不要混合输入和输出文件 -- 首先指定所有的输入文件, 然后是所有的输出文件. 也不要混合属于不同文件的选项.
所有的选项只适用于下一个输入或输出文件, 并且在不同的文件之间被重置.

`muxer`将视频文件, 音频文件和字幕文件合并为某一个视频格式.
如, 可将`a.avi`, `a.mp3`, `a.srt`用`muxer`合并为`mkv`格式的视频文件.
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

## 命令行选项

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

## 通用选项

这些选项在 `ff*` 工具中是共享的.

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

## devices

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

## 编解码器样式说明

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

## map_metadata

+ `-map_metadata[:metadata_spec_out] infile[:metadata_spec_in] (output,per-metadata)`

使用`infile`的元数据信息设置下一个输出文件中的元数据. 注意, 这些是按文件索引(基于0), 而不是按文件名. \
可选的`metadata_spec_in/out`参数指定要拷贝的元数据.  一个元数据可以有以下几种形式.

+ `g`: 全局元数据, 即适用于整个文件的元数据
+ `s[:stream_spec]`:`stream_spec`是一个流指定器, 在 Stream specifiers 一章中有描述.
在 input metadata specifier 中, 第一个匹配的流将被复制过来. 在一个out metadata specifier中, 所有匹配的流都被复制.
+ `c:chapter_index`: `chapter_index`是基于零的章节索引.
+ `p:program_index`: 每个程序的元数据, `program_index`是基于零的程序索引.

如果省略了元数据指定符, 则默认为全局.
默认情况下, 全局元数据从第一个输入文件中复制, 每个流和每个章节的元数据与流/章节一起被复制.
这些默认的映射可以通过创建任何相关类型的映射来禁用.
`负数文件索引` 可以用来创建假的映射(dummy mapping), 可以用来禁止自动复制.

例如, 从输入文件的第一个流复制元数据到输出文件的全局元数据.

```bash
ffmpeg -i in.ogg -map_metadata 0:s:0 out.mp3
```

相反的, 复制全局元数据到所有的音频流.

```bash
ffmpeg -i in.mkv -map_metadata:s:a 0:g out.mkv
```

注意, 在这个例子中, 简单的`0`也可以工作, 因为全局元数据是默认的.

## AVOptions

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

## vsync

+ `-vsync parameter`: 视频同步方法.  由于兼容性的原因, 旧的值可以被指定为数字.  新增加的值必须始终指定为字符串.
    + `0`,`passthrough`:每一帧的时间戳都从`demuxer`传到`muxer`中.
    + `1`,`cfr`: 帧将被复制和丢弃, 以准确实现所要求的恒定帧率.
    + `2`,`vfr`: 帧将带着它们的时间戳通过或丢弃, 以防止`2`个帧具有相同的时间戳.
    + `drop`: 与`passthrough`一样, 但破坏所有的时间戳, 使`muxer`根据帧速率生成新的时间戳.
    + `-1`,`auto`:根据`muxer`的能力在`1`和`2`之间进行选择. 这是默认的方法.

注意, 在这之后, 时间戳可能会被`muxer`进一步修改.  例如, 在格式选项`avoid_negative_ts`被启用的情况下.
通过`-map`, 你可以选择从哪个流中获取时间戳. 你可以不改变视频或音频, 并将其余的流同步到不改变的流.

## pix_fmt

+ `-pix_fmt[:stream_specifier] format (输入/输出,per-stream)`: 设置`pixel`格式. 使用`-pix_fmts`来显示所有支持的像素格式.

如果不能选择所选的像素格式,`ffmpeg`将打印一个警告, 并选择编码器支持的最佳像素格式.
如果`pix_fmt`的前缀是 `+`, 如果不能选择所要求的像素格式, `ffmpeg`将以错误退出, 并禁用滤波图内部的自动转换.
如果`pix_fmt`是一个单独的 `+`, `ffmpeg`选择与输入(或图形输出)相同的像素格式, 自动转换功能被禁用.
