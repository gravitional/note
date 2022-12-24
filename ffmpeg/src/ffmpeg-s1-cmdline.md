# ffmpeg 命令行

[FFMPEG详解(完整版)](https://blog.csdn.net/davidullua/article/details/120562737)

## 认识FFMPEG

FFMPEG堪称自由软件中最完备的一套多媒体支持库, 它几乎实现了所有当下常见的数据封装格式,
多媒体传输协议以及音视频编解码器, 堪称多媒体业界的瑞士军刀.
因此, 对于从事多媒体技术开发的工程师来说, 深入研究FFMPEG成为一门必不可少的工作,
可以这样说, FFMPEG之于多媒体开发工程师的重要性正如kernel之于嵌入式系统工程师一般.

几个小知识:

+ FFMPEG项目是由法国人Fabrice Bellard发起的,
此人也是著名的CPU模拟器项目QEMU的发起者, 同时还是圆周率算法纪录的保持者.
+ FF是Fast Forward的意思, 翻译成中文是"快进".

+ FFMPEG的LOGO是一个"Z字扫描"示意图,
Z字扫描用于将图像的二维频域数据一维化,
同时保证了一维化的数据具备良好的统计特性, 从而提高其后要进行的一维熵编码的效率.

### 关于耻辱厅(Hall of Shame)

FFMPEG大部分代码遵循LGPL许可证,
如果使用者对FFMpeg进行了修改,要求公布修改的源代码;
有少部分代码遵循GPL许可证, 要求使用者同时公开使用FFMpeg的软件的源代码.
实际上, 除去部分具备系统软件开发能力的大型公司(Microsoft, Apple等)
以及某些著名的音视频技术提供商(Divx, Real等)提供的自有播放器之外,
绝大部分第三方开发的播放器都离不开FFMpeg的支持,
像Linux桌面环境中的开源播放器VLC, MPlayer, Windows下的KMPlayer,
暴风影音以及Android下几乎全部第三方播放器都是基于FFMPEG的.
也有许多看似具备自主技术的播放器, 其实也都不声不响地使用了FFMPEG,
这种行为被称为"盗窃", 参与"盗窃"的公司则被请入耻辱厅,
如于2009年上榜的国产播放器暴风影音, QQ影音.

关于FFMPEG的商业应用: 与其他开源软件不同的是,
FFMPEG所触及的多媒体编解码算法中有相当一部分处于大量的专利涵盖范围之内,
因此, 在商业软件中使用FFMPEG必须考虑可能造成的对专利所有者的权利侵犯,
这一点在FFMPEG的官方网站也有所提及, 所涉及的风险需使用者自行计算应对.

FFMPEG从功能上划分为几个模块, 分别为核心工具(libutils), 媒体格式(libavformat),
编解码(libavcodec), 设备(libavdevice)和后处理(libavfilter, libswscale, libpostproc),
分别负责提供公用的功能函数, 实现多媒体文件的读包和写包, 完成音视频的编解码,
管理音视频设备的操作以及进行音视频后处理.

## 使用FFMPEG

这里指FFMPEG提供的命令行(CLI)工具ffmpeg,
其使用方法如下(方括号表示可选项, 花括号表示必选项目):

```bash
ffmpeg [global options] {[infile options]['-i' 'infile'] ...} {[outfile options] 'outfile'...}
```

参数选项由三部分组成: 可选的一组全局参数,
一组或多组输入文件参数, 一组或多组输出文件参数,
其中, 每组输入文件参数以'-i'为结束标记;每组输出文件参数以输出文件名为结束标记.

## 基本选项

能力集列表

+ -formats: 列出支持的文件格式.
+ -codecs: 列出支持的编解码器.
+ -decoders: 列出支持的解码器.
+ -encoders: 列出支持的编码器.
+ -protocols: 列出支持的协议.
+ -bsfs: 列出支持的比特流过滤器.
+ -filters: 列出支持的滤镜.
+ -pix_fmts: 列出支持的图像采样格式.
+ -sample_fmts: 列出支持的声音采样格式.

常用输入选项

+ `-y`; overwrite output files
+ -i filename: 指定输入文件名.
+ -f fmt: 强制设定文件格式, 需使用能力集列表中的名称(缺省是根据扩展名选择的).
+ -ss hh:mm:ss[.xxx]: 设定输入文件的起始时间点, 启动后将跳转到此时间点然后开始读取数据.

对于输入, 以下选项通常是自动识别的, 但也可以强制设定.

+ -c codec: 指定解码器, 需使用能力集列表中的名称.
+ -acodec codec: 指定声音的解码器, 需使用能力集列表中的名称.
+ -vcodec codec: 指定视频的解码器, 需使用能力集列表中的名称.
+ -b:v bitrate: 设定视频流的比特率, 整数, 单位bps.
+ -r fps: 设定视频流的帧率, 整数, 单位fps.
+ -s WxH : 设定视频的画面大小. 也可以通过挂载画面缩放滤镜实现.
+ -pix_fmt format: 设定视频流的图像格式(如RGB还是YUV).
+ -ar sample rate: 设定音频流的采样率, 整数, 单位Hz.
+ -ab bitrate: 设定音频流的比特率, 整数, 单位bps.
+ -ac channels: 设置音频流的声道数目.

### 常用输出选项

+ -f fmt: 强制设定文件格式, 需使用能力集列表中的名称(缺省是根据扩展名选择的).
+ -c codec: 指定编码器, 需使用能力集列表中的名称(编码器设定为"copy"表示不进行编解码).
+ -acodec codec: 指定声音的编码器, 需使用能力集列表中的名称(编码器设定为"copy"表示不进行编解码).
+ -vcodec codec: 指定视频的编码器, 需使用能力集列表中的名称(编解码器设定为"copy"表示不进行编解码).
+ -r fps: 设定视频编码器的帧率, 整数, 单位fps.
+ -pix_fmt format: 设置视频编码器使用的图像格式(如RGB还是YUV).
+ -ar sample rate: 设定音频编码器的采样率, 整数, 单位Hz.
+ -b bitrate: 设定音视频编码器输出的比特率, 整数, 单位bps.
+ -ab bitrate: 设定音频编码器输出的比特率, 整数, 单位bps.
+ -ac channels: 设置音频编码器的声道数目.
+ -an 忽略任何音频流.
+ -vn 忽略任何视频流.
+ -t hh:mm:ss[.xxx]: 设定输出文件的时间长度.
+ -to hh:mm:ss[.xxx]: 如果没有设定输出文件的时间长度的画可以设定终止时间点.

### 流标识

FFMPEG的某些选项可以对一个特定的媒体流起作用, 这种情况下需要在选项后面增加一个流标识. 流标识允许以下几种格式:

+ 流序号. 譬如":1"表示第二个流.
+ 流类型. 譬如":a"表示音频流, 流类型可以和流序号合并使用, 譬如":a:1"表示第二个音频流.
+ 节目. 节目和流序号可以合并使用.
+ 流标识. 流标识是一个内部标识号.

假如要设定第二个音频流为copy, 则需要指定-codec:a:1 copy

## 音频选项

+ -aframes: 等价于frames:a, 输出选项, 用于指定输出的音频帧数目.
+ -aq: 等价于q:a, 老版本为qscale:a, 用于设定音频质量.
+ -atag: 等价于tag:a, 用于设定音频流的标签.
+ -af: 等价于filter:a, 用于设定一个声音的后处理过滤链, 其参数为一个描述声音后处理链的字符串.

## 视频选项

+ -vframes: 等价于frames:v, 输出选项, 用于指定输出的视频帧数目.
+ -aspect: 设置宽高比, 如4:3, 16:9, 1.3333, 1.7777等.
+ -bits_per_raw_sample: 设置每个像素点的比特数.
+ -vstats: 产生video统计信息.
+ -vf: 等价于filter:v, 用于设定一个图像的后处理过滤链, 其参数为一个描述图像后处理链的字符串.
+ -vtag: 等价于tag:v, 用于设定视频流的标签.
+ -force_fps: 强制设定视频帧率.
+ -force_key_frames: 显式控制关键帧的插入, 参数为字符串, 可以是一个时间戳, 也可以是一个"expr:"前缀的表达式.
如"-force_key_frames 0:05:00", "-force_key_frames expr:gte(t,n_forced*5)"

## 滤镜选项

### 高级选项

+ -re: 要求按照既定速率处理输入数据, 这个速率即是输入文件的帧率.
+ -map: 指定输出文件的流映射关系.
例如 `-map 1:0 -map 1:1` 要求将第二个输入文件的第一个流和第二个流写入到输出文件.
如果没有-map选项, ffmpeg采用缺省的映射关系.

## 用例

1. 将一个老式的avi文件转成mp4

ffmpeg -i final.avi -acodec copy -vcodec copy final.mp4

2. 从一个视频文件中抽取一帧图像:

ffmpeg -y -i test.mp4 -ss 00:03:22.000 -vframes 1 -an test.jpg

3. a

```bash
ffmpeg -i final.avi -vf scale=640:640 square.avi
```

4. 使用alsa接口录制一段音频存放到某个wav文件中

```bash
ffmpeg -f alsa -i hw:0 -t 100 alsaout.wav
```

5. 使用alsa接口搭建一个个人网络电台

```bash
ffmpeg -f alsa -i default -acodec aac -strict -2 -b:a 128k -r 44100 /var/www/data/main.m3u8
```

6. 将一个mp4文件的音视频流实时转码之后发送给某个远程设备,
远程设备可以通过http获取的sdp文件来接收rtp媒体数据.

```bash
ffmpeg -re -i example.mp4 -acodec copy -vcodec libx264 -s 480x270 \
-map 0:0 -f rtp rtp://10.131.202.62:1234 -map 0:1 \
-f rtp rtp://10.131.202.62:1238 > /var/www/live.sdp
```
