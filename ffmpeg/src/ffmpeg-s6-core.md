# 核心架构和机制

## 过滤链

FFMPEG支持由多个过滤器结构组成的过滤链,
以实现对视频帧和音频采样数据的后续处理, 如图像缩放, 图像增强, 声音的重采样等.

## 数据结构

![img](https://img-blog.csdnimg.cn/img_convert/b0cdd4a59596fb36ce25d344cf290b65.png)

FFMPEG使用了的四个主要的数据结构来构造过滤链, 如上图所示.
其中, AVFilter是一个过滤器的抽象,
它拥有若干AVFilterPad, 分别实现过滤器数据的输入和输出.
而AVFilterLink表示各个过滤器之间的连接, 这种连接的实现是基于AVFilterPad的,
每个连接都有一个实现输入的AVFilterPad和
一个实现输出的AVFilterPad以及对应的源过滤器和目的过滤器.

此外, 还有两个数据结构AVFilterBuffer和AVFilterBufferRef来表示过滤器的输入输出数据,
其中AVFilterBufferRef是AVFilterBuffer的引用, 这种设计的目的是避免不必要的数据拷贝.
通过avfilter_get_buffer_ref_from_frame可以获得一个AVFrame结构的引用,
从而实现过滤器对来自AVFrame的图像原数据或声音采样序列的处理.

## APIs

avfilter_open
avfilter_free
avfilter_init_filter
avfilter_link
avfilter_link_free
avfilter_ref_buffer
