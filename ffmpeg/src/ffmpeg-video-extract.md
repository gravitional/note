# ffmpeg视频精准剪切

[ffmpeg视频精准剪切](https://zhuanlan.zhihu.com/p/97914917)

## 导言

`ffmepg` 剪切视频, 很方便, 但是也有很大缺陷:

+ 剪切时间点不精确
+ 有时剪切的视频开头有黑屏

造成这些问题的原因是 `ffmpeg` 无法 `seek` 到非关键帧上.
一下本文通过一些参数配置尽可能地减轻以上问题

## 基本剪切方法

```bash
ffmpeg -i test.mp4 -ss 10 -t 15 -codec copy cut.mp4
```

参数说明:

+ `-i` : source
+ `-ss`: 开始时间
+ `-t` : 持续时间
+ `-c` : 视频, 音频编码 video,audio codec

时间格式:

+ `x秒`
+ `HOURS:MM:SS.MICROSECONDS`

可以设置输出视频的编码格式

```bash
-vcodec xxx
-acodec xxx
```

把 `-ss` , `-t` 参数放在 `-i` 参数之后, 是对 `输出文件` 执行的 `seek` 操作
`输入文件` 会逐帧解码, 直到 `-ss` 设置的时间点为止, 这么操作会很慢,
虽然时间点是准确的, 但是很容易出现黑屏问题.

## 根据开始时间结束时间切割视频

[ffmpeg时间有关的操作](https://www.cnblogs.com/yongfengnice/p/7156952.html)
[ffmpeg根据开始时间结束时间切割视频](https://blog.csdn.net/fjh1997/article/details/106037265)

根据开始时间结束时间, `-to 结束时间`.
注意这几个参数的顺序不能错误, `-i` 必须要第一个

```bash
ffmpeg  -i input.mp4 -ss 00:01:00 -to 00:02:00 -codec copy output1.mp4
```

根据时长切割视频

```bash
# 使用 hour:minute:seconds
ffmpeg  -ss 00:01:00  -t 00:02:00 -i input.mp4  -codec copy -avoid_negative_ts 1 out.mp4
# 使用 seconds
ffmpeg -ss 10 -t 15 -i input.mp4 -codec copy -avoid_negative_ts 1 out.mp4
```

可以写个` 输入文件列表.txt` 来合并视频, 例如名字为`file.txt`

```txt
file 'output1.mp4'
file 'output2.mp4'
file 'output3.mp4'
```

然后:

```bash
ffmpeg -f concat -i file.txt -codec copy output.mp4
```

windows 环境下使用:

```powershell
ffmpeg  -ss 01:26:03  -t 00:33:49  -i  `
'ipnut.mp4'  `
-codec copy -avoid_negative_ts 1 `
'output.mp4'
```

### 视频合并Unsafe file name

[ffmpeg视频合并Unsafe file name问题解决](https://blog.csdn.net/mp624183768/article/details/81603602)

使用ffmpeg进行多个视频合并过程中出现了 `unsafe file name` 错误, 解决的办法是加个 `-safe 0` 参数:

```bash
ffmpeg.exe -f concat -safe 0 -i fileToMerge.txt -codec copy -y  out.mp4
```

## 参数优化

+ 将 `-ss`,  `-t` 参数放在 `-i` 参数之前

```bash
ffmpeg -ss 10 -t 15 -i test.mp4 -codec copy -avoid_negative_ts 1 cut.mp4
```

对输入文件执行 `seek` 操作, 会 `seek` 到 `-ss` 设置的时间点前面的关键帧上.
时间不精确, 但是不会出现黑屏

+ accurate_seek
剪切时间更加精确

```bash
ffmpeg -ss 10 -t 15 -accurate_seek -i test.mp4 -codec copy cut.mp4
```

PS: `accurate_seek` 必须放在 `-i` 参数之前

+ `avoid_negative_ts`

如果编码格式采用的 `copy` 最好加上 `-avoid_negative_ts 1` 参数

```bash
ffmpeg -ss 10 -t 15 -accurate_seek -i test.mp4 -codec copy -avoid_negative_ts 1 cut.mp4
```

也可以尝试下面的命令

```bash
ffmpeg -ss 10 -t 15 -i test.mp4 -c:v libx264 -c:a aac -strict experimental -b:a 98k cut.mp4 -y
```

## 使用FFmpeg精确剪辑视频

[如何使用FFmpeg精确剪辑视频](https://zhuanlan.zhihu.com/p/423444166)

### 问题描述

之前基于ffmpeg做二次开发, 完成常见的视频处理功能, 并用ffmpeg命令行做兜底.
在此基础上, 还做一个转码接入和调度系统对外提供服务. 有个功能需要是这样的:
快速从指定的视频中裁剪某一时间范围的子视频,  两个要求:

1. 要快, 不能像转码一样耗时;
2. 要精确, 剪辑的时候能指定从哪一秒开始, 到哪一秒结束.

### 难点

用 `ffmpeg` 很容易从一个长视频剪辑出一段小视频. 比如命令

```bash
ffmpeg -i input.mp4 -ss 00:10:03 -t 00:03:00 -vcodec copy -acodec copy output.mp4
```

就是从input.mp4的第10分钟03秒开始剪辑出一个3分钟的视频并且保存为output.mp4文件.
参数-vcodec copy -acodec copy就是直接拷贝原始视频的音视频流, 不进行编解码.
虽然上面的方法很方便, 但有一个致命的缺陷:
画面在一开始会卡住(但声音一直是正常的), 几秒后画面才正常滚动.

### 原因分析

GOP: group of pictures

究其原因, 剪辑的开始时间落在视频 `GOP` 的中间位置而不是第一个 `I` 帧.
稍微了解过视频编码的同学应该都听过I, B, P帧.
简单来说, I帧是一张完整的图像, P帧则根据I帧做差分编码, B帧根据前后的I, P, B帧作差分编码.
也就是说I帧具有完整的内容, 而P和B帧不具有, 所以如果缺少I帧, 那么P和B帧是不能正常解码的.
通常来说, 一个GOP里面第一帧是I帧, 后面是若干个P和B帧.
一个GOP长达10秒都是有可能的.
下图是一个真实视频的I, B, P帧信息图, 红色的表示I帧, 可以看到两个I帧相隔深远(实际是隔了10秒).

![img](https://pic4.zhimg.com/80/v2-ad051046e8090fcdb2fa0e6d8e3d8677_720w.jpg)

从上面分析可知: 剪辑的开始时间很大可能不是落在I帧, 由于缺少I帧会使得后面的P和B帧无法解码导致画面卡住.
上面的分析都是基于不编解码的直接拷贝视频内容的, 如果考虑先解码成一张张的图像,
然后再对符合时间要求的图像编码, 那么剪辑时间可以做到非常精准.
但这样做的就是耗时过长: 需求花费大量的CPU完成编解码操作.

### 解决方案

解决的办法还是有的:
对前面第一个符合时间要求的GOP编解码, 而之后的GOP内容则直接拷贝到目标视频.
一来, 第一个GOP的帧由于是重新编码所以会重新分配I帧从而能播放,
二来, 之后的GOP内容是直接拷贝的所以基本不消耗CPU, 性能杠杆的.
如下图所示:

![img](https://pic2.zhimg.com/80/v2-638331017db1b821734c1ff410e863e1_720w.webp)

当然这里面还是有一些坑的, 下面开始填坑.

### 拼接

源视频可能会惊讶: 我凭本事编的码, 为什么你直接拷贝就能解码?
一般来说解码依赖于SPS和PPS, 而源视频与目标视频的SPS和PPS会有所不同, 因此直接拷贝是不能正确解码的.
对于mp4文件, SPS和PPS 一般是放到文件头.
一个文件只能有一个文件头, 也就不能存放两个不同的SPS和PPS.
为了能正确解码目标视频必须得有源视频的SPS和PPS.
不能放文件头的话, 那能放哪里?能不能放到拷贝的帧的前面呢?如何放?
一筹莫展, 无处下手, 直到有一天突然想起之前为了填一个坑,
追踪到 h264_mp4toannexb 的实现, 它的作用就是将SPS和PPS拷贝到帧(准确来说应该是AVPacket)的前面.
来!温习一下 `h264_mp4toannexb` 的具体实现:
在所有AVPacket前面增加0x000001或者0x00000001, 在I帧的前面插入SPS和PPS.
也就是通过 `h264_mp4toexannb` 就能把解码所需的SPS和PPS正确插入到视频中.
`h264_mp4toannexb` 使用起来也比较简单, 代码如下:

```cpp
AVBSFContext* initBSF(const std::string &filter_name, const AVCodecParameters *codec_par, AVRational tb)
{
    const AVBitStreamFilter *filter = av_bsf_get_by_name(m_filter_name.c_str());
​
    AVBSFContext *bsf_ctx = nullptr;
    av_bsf_alloc(filter, &bsf_ctx);
​
    avcodec_parameters_copy(bsf_ctx->par_in, codec_par);
    bsf_ctx->time_base_in = tb;
​
    av_bsf_init(bsf_ctx);
    return bsf_ctx;
}
​
AVPacket* feedPacket(AVBSFContext *bsf_ctx, AVPacket &packet)
{
    av_bsf_send_packet(bsf_ctx, packet);
​
    AVPacket *dst_packet = av_packet_alloc();
    av_bsf_receive_packet(bsf_ctx, dst_packet);
​
    return dst_packet;
}
​
void test()
{
    AVBSFContext *bsf_ctx = initBSF("h264_mp4toannexb", video_stream->codecpar, video_stream->time_base);
    AVPacket *packet = readVideoPacket();
    AVPacket *dst_packet = feedPacket(bsf_ctx, packet);
}
```

注意: 编解码第一个GOP和原始视频后续GOP拼接时的时间戳要小心处理,
不然视频播放时可能会出现抖动现象.

### 花屏

以为就完了吗?没有!!你会发现有些视频会在最后一秒出现花屏. . . .

出现花屏的原因其实也不难猜到: 最后一帧是B帧.
由于不是所有剪辑的视频最后一帧都是B帧, 所以花屏也不是必现的.
知道是B帧引起的, 那解决方案也就明确了:
保证最后一帧是P帧. 即使时间上稍微超一点(音频流也应该跟着视频流稍微超一下时间).
不过呢, 由于不能直接从 `AVPacket` 判断一个帧是否为P帧, 所以最后一个 `GOP` 也得解码(无需编码).
记录超出时间范围后的第一个P帧的pts, 后面拷贝GOP的时候, 拷贝到这个pts就可以停止了.

万变不离其宗, 从问题的原因出发, 一步步寻找解决方案, 并将一路上碰到的问题逐一击破.
记住, 明白原理才能解决问题.

## SPS, PPS

[iVikings-SPS, PPS](https://www.jianshu.com/p/9196326876ce)

在 H.264 流中, 有两种 NALU 极其重要

+ 序列参数集 (Sequence Paramater Set, SPS)
    + SPS 记录了编码的 Profile, level, 图像宽高等
    + 如果其中的数据丢失或出现错误, 那么解码过程很可能会失败

+ 图像参数集 (Picture Paramater Set, PPS)
每一帧编码后数据所依赖的参数保存于 PPS 中

一般情况 SPS 和 PPS 的 NAL Unit 通常位于整个码流的起始位置.
封装文件一般进保存一次, 位于文件头部, SPS/PPS 在整个解码过程中复用, 不发生变化.
然而对于实时流, 通常是从流中间开始解码, 因此需要在每个I帧前添加SPS和PPS;
如果编码器在编码过程中改变了码流参数(如分辨率), 需要重新调整SPS和PPS数据.

### 什么是 NAL

NAL (Network Abstract Layer), 即网络抽象层
在 H.264/AVC 视频编码标准中, 整个系统框架被分为了两个层面:

视频编码层面 (Video Coding Layer, 即VCL)

+ 负责有效表示视频数据的内容
+ VCL 是 H.264/AVC 的规格, 意思是压缩后, 去冗余(Visual Redundancy)的影像资料,
其技术核心包括动作估计, 转换编码, 预测编码, 去区块效应滤波及熵编码等

+ 网络抽象层面 (NAL)
负责格式化数据并提供头信息, 以保证数据适合各种信道和存储介质上的传播

视频编码层往往与网络抽象层(NAL)相互配合, 标准的 NAL-unit 总共规范(Profile)有12种,
这12种型式可粗分成 VCL NAL-unit及 non-VCL NAL-unit,
其中 VCL NAL-unit 是指 NAL-unit 中存放的完全是VCL的影像资料.

现实中的传输系统是多样化的, 其可靠性, 服务质量, 封装方式等特征各不相同,
NAL这一概念的提出提供了一个视频编码器和传输系统的友好接口,
使得编码后的视频数据能够有效的在各种不同的网络环境中传输.
