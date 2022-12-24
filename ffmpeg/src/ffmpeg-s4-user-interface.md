# 用户接口

## ffmpeg 数据结构

### 基本概念

编解码器, 数据帧, 媒体流和容器是数字媒体处理系统的四个基本概念.

首先需要统一术语:

+ 容器／文件(Conainer/File): 即特定格式的多媒体文件.
+ 媒体流(Stream): 指时间轴上的一段连续数据, 如一段声音数据,
一段视频数据或一段字幕数据, 可以是压缩的, 也可以是非压缩的, 压缩的数据需要关联特定的编解码器.
+ 数据帧／数据包(Frame/Packet): 通常, 一个媒体流由大量的数据帧组成,
对于压缩数据, 帧对应着编解码器的最小处理单元. 通常, 分属于不同媒体流的数据帧交错复用于容器之中, 参见交错.
+ 编解码器: 编解码器以帧为单位实现压缩数据和原始数据之间的相互转换.

在FFMPEG中, 使用AVFormatContext, AVStream, AVCodecContext,
AVCodec及AVPacket等结构来抽象这些基本要素, 它们的关系如下图所示:  

![img](https://img-blog.csdnimg.cn/img_convert/1af4a94c823a461d95e77ef91e1d4ff2.png)

## AVCodecContext

这是一个描述编解码器上下文的数据结构, 包含了众多编解码器需要的参数信息, 如下列出了部分比较重要的域:

如果是单纯使用libavcodec, 这部分信息需要调用者进行初始化;
如果是使用整个FFMPEG库, 这部分信息在调用avformat_open_input和avformat_find_stream_info
的过程中根据文件的头信息及媒体流内的头部信息完成初始化. 其中几个主要域的释义如下:

+ extradata/extradata_size: 这个buffer中存放了解码器可能会用到的额外信息, 在av_read_frame中填充.
一般来说, 首先, 某种具体格式的demuxer在读取格式头信息的时候会填充extradata,
其次, 如果demuxer没有做这个事情, 比如可能在头部压根儿就没有相关的编解码信息,
则相应的parser会继续从已经解复用出来的媒体流中继续寻找. 在没有找到任何额外信息的情况下, 这个buffer指针为空.

+ time_base: 编解码器的时间基准, 实际上就是视频的帧率(或场率).
+ width/height: 视频的宽和高.
+ sample_rate/channels: 音频的采样率和信道数目.
+ sample_fmt:  音频的原始采样格式.
+ codec_name/codec_type/codec_id/codec_tag: 编解码器的信息.

## AVStream

该结构体描述一个媒体流,

主要域的释义如下,
其中大部分域的值可以由avformat_open_input根据文件头的信息确定,
缺少的信息需要通过调用avformat_find_stream_info读帧及软解码进一步获取:

+ index/id: index对应流的索引, 这个数字是自动生成的,
根据index可以从AVFormatContext::streams表中索引到该流;
而id则是流的标识, 依赖于具体的容器格式. 比如对于MPEG TS格式, id就是pid.

+ time_base: 流的时间基准, 是一个实数,
该流中媒体数据的pts和dts都将以这个时间基准为粒度.
通常, 使用av_rescale/av_rescale_q可以实现不同时间基准的转换.

+ start_time: 流的起始时间, 以流的时间基准为单位, 通常是该流中第一个帧的pts.
+ duration: 流的总时间, 以流的时间基准为单位.
+ need_parsing: 对该流parsing过程的控制域.
+ nb_frames: 流内的帧数目.
+ r_frame_rate/framerate/avg_frame_rate: 帧率相关.
+ codec: 指向该流对应的AVCodecContext结构, 调用avformat_open_input时生成.
+ parser: 指向该流对应的AVCodecParserContext结构, 调用avformat_find_stream_info时生成. .

### AVFormatContext

这个结构体描述了一个媒体文件或媒体流的构成和基本信息,

这是FFMpeg中最为基本的一个结构, 是其他所有结构的根,
是一个多媒体文件或流的根本抽象. 其中:

nb_streams和streams所表示的AVStream结构指针数组包含了所有内嵌媒体流的描述;
iformat和oformat指向对应的demuxer和muxer指针;
pb则指向一个控制底层数据读写的ByteIOContext结构.
start_time和duration是从streams数组的各个AVStream中推断出的多媒体文件的起始时间和长度, 以微妙为单位.

通常, 这个结构由avformat_open_input在内部创建并以缺省值初始化部分成员.
但是, 如果调用者希望自己创建该结构,
则需要显式为该结构的一些成员置缺省值——如果没有缺省值的话, 会导致之后的动作产生异常. 以下成员需要被关注:

+ probesize
+ mux_rate
+ packet_size
+ flags
+ max_analyze_duration
+ key
+ max_index_size
+ max_picture_buffer
+ max_delay

## AVPacket

AVPacket定义在avcodec.h中, 如下:
FFMPEG使用AVPacket来暂存媒体数据包及附加信息(解码时间戳, 显示时间戳, 时长等),
这样的媒体数据包所承载的往往不是原始格式的音视频数据, 而是以某种方式编码后的数据,
编码信息由对应的媒体流结构AVStream给出. AVPacket包含以下数据域:

+ dts表示解码时间戳, pts表示显示时间戳, 它们的单位是所属媒体流的时间基准.
+ stream_index给出所属媒体流的索引;
+ data为数据缓冲区指针, size为长度;
+ duration为数据的时长, 也是以所属媒体流的时间基准为单位;
+ pos表示该数据在媒体流中的字节偏移量;
+ destruct为用于释放数据缓冲区的函数指针;
+ flags为标志域, 其中, 最低为置1表示该数据是一个关键帧.

AVPacket结构本身只是个容器, 它使用data成员引用实际的数据缓冲区.
这个缓冲区的管理方式有两种, 其一是通过调用av_new_packet直接创建缓冲区, 其二是引用已经存在的缓冲区.
缓冲区的释放通过调用av_free_packet实现, 其内部实现也采用了两种不同的释放方式,
第一种方式是调用AVPacket的destruct函数, 这个destruct函数可能是缺省的av_destruct_packet,
对应av_new_packet或av_dup_packet创建的缓冲区, 也可能是某个自定义的释放函数,
表示缓冲区的提供者希望使用者在结束缓冲区的时候按照提供者期望的方式将其释放,
第二种方式是仅仅将data和size的值清0, 这种情况下往往是引用了一个已经存在的缓冲区, AVPacket的destruct指针为空.

在使用AVPacket时, 对于缓冲区的提供者, 必须注意通过设置destruct函数指针指定正确的释放方式,
如果缓冲区提供者打算自己释放缓冲区, 则切记将destruct置空;而对于缓冲区的使用者,
务必在使用结束时调用av_free_packet释放缓冲区(虽然释放操作可能只是一个假动作).
如果某个使用者打算较长时间内占用一个AVPacket——比如不打算在函数返回之前释放它——
最好调用av_dup_packet进行缓冲区的克隆, 将其转化为自有分配的缓冲区, 以免对缓冲区的不当占用造成异常错误.
av_dup_packet会为destruct指针为空的AVPacket新建一个缓冲区,
然后将原缓冲区的数据拷贝至新缓冲区, 置data的值为新缓冲区的地址, 同时设destruct指针为av_destruct_packet.

上述媒体结构可以通过FFMPEG提供的av_dump_format方法直观展示出来, 以下例子以一个MPEG-TS文件为输入, 其展示结果为:

```conf
Input #0, mpegts, from '/Videos/suite/ts/H.264_High_L3.1_720x480_23.976fps_AAC-LC.ts':
  Duration: 00:01:43.29, start: 599.958300, bitrate: 20934 kb/s
  Program 1
    Stream #0.0[0x1011]: Video: h264 (High), yuv420p, 720x480 [PAR 32:27 DAR 16:9], 23.98 fps, 23.98 tbr, 90k tbn, 47.95 tbc
    Stream #0.1[0x1100]: Audio: aac, 48000 Hz, stereo, s16, 159 kb/s
```

从中可以找到媒体的格式, 路径, 时长, 开始时间, 全局比特率.
此外, 列出了媒体包含的一个节目, 由两个媒体流组成,
第一个媒体流是视频流, id为0x1011; 第二个为音频流, id为0x1100.
视频流是h264的High Profile编码, 色彩空间为420p, 大小为720×480, 平均帧率23.98,
参考帧率23.98, 流时间基准是90000, 编码的时间基准为47.95;
音频流是aac编码, 48kHz的采样, 立体声, 每个样点16比特, 流的比特率是159kbps.

## 时间信息

时间信息用于实现多媒体同步.

同步的目的在于展示多媒体信息时, 能够保持媒体对象之间固有的时间关系.
同步有两类, 一类是流内同步, 其主要任务是保证单个媒体流内的时间关系, 以满足感知要求,
如按照规定的帧率播放一段视频;另一类是流间同步,
主要任务是保证不同媒体流之间的时间关系, 如音频和视频之间的关系(lipsync).

对于固定速率的媒体, 如固定帧率的视频或固定比特率的音频,
可以将时间信息(帧率或比特率)置于文件首部(header), 如AVI的hdrl List, MP4的moov box,
还有一种相对复杂的方案是将时间信息嵌入媒体流的内部,
如MPEG TS和Real video, 这种方案可以处理变速率的媒体, 亦可有效避免同步过程中的时间漂移.

FFMPEG会为每一个数据包打上时间标签, 以更有效地支持上层应用的同步机制.
时间标签有两种, 一种是DTS, 称为解码时间标签, 另一种是PTS, 称为显示时间标签.
对于声音来说 , 这两个时间标签是相同的, 但对于某些视频编码格式, 由于采用了双向预测技术, 会造成DTS和PTS的不一致.

无双向预测帧的情况:

图像类型: I   P   P   P   P   P   P ...  I   P   P
DTS:     0   1   2   3   4   5   6...  100 101 102
PTS:     0   1   2   3   4   5   6...  100 101 102

有双向预测帧的情况:

图像类型: I   P   B   B   P   B   B ...  I   P   B
DTS:     0   1   2   3   4   5   6 ...  100 101 102
PTS:     0   3   1   2   6   4   5 ...  100 104 102

对于存在双向预测帧的情况, 通常要求解码器对图像重排序, 以保证输出的图像顺序为显示顺序:

解码器输入: I   P   B   B   P   B   B
 (DTS)     0   1   2   3   4   5   6
 (PTS)     0   3   1   2   6   4   5
解码器输出: X   I   B   B   P   B   B   P
 (PTS)     X   0   1   2   3   4   5   6

时间信息的获取:

通过调用avformat_find_stream_info,
多媒体应用可以从 `AVFormatContext` 对象中拿到媒体文件的时间信息:
主要是总时间长度和开始时间, 此外还有与时间信息相关的比特率和文件大小.
其中时间信息的单位是AV_TIME_BASE: 微秒.

```cpp
typedef struct AVFormatContext {
 
    ......
 
    /** Decoding: position of the first frame of the component, in
       AV_TIME_BASE fractional seconds. NEVER set this value directly:
       It is deduced from the AVStream values.  */
    int64_t start_time;
    /** Decoding: duration of the stream, in AV_TIME_BASE fractional
       seconds. Only set this value if you know none of the individual stream
       durations and also dont set any of them. This is deduced from the
       AVStream values if not set.  */
    int64_t duration;
    /** decoding: total file size, 0 if unknown */
    int64_t file_size;
    /** Decoding: total stream bitrate in bit/s, 0 if not
       available. Never set it directly if the file_size and the
       duration are known as FFmpeg can compute it automatically. */
    int bit_rate;
 
    ......
 
} AVFormatContext;
```

以上4个成员变量都是只读的, 基于FFMpeg的中间件需要将其封装到某个接口中, 如:

```cpp
LONG GetDuratioin(IntfX*);
LONG GetStartTime(IntfX*);
LONG GetFileSize(IntfX*);
LONG GetBitRate(IntfX*);
```
