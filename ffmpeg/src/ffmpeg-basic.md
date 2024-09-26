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

`FFmpeg` 命令的典型语法是:

```bash
ffmpeg [全局选项] {[输入文件选项] -i 输入_url_地址} ...  {[输出文件选项] 输出_url_地址} ...
```

现在我们将查看一些重要的和有用的 `FFmpeg` 命令.

## 获取音频/视频文件信息

为显示你的媒体文件细节, 运行:

```bash
ffmpeg -i video.mp4
```

`FFmpeg` 将显示该媒体文件信息, 以及 `FFmpeg` 细节, 例如版本, 配置细节, 版权标记, 构建参数和库选项等等.
如果你不想看 `FFmpeg` 标语和其它细节, 而仅仅想看媒体文件信息, 使用 `-hide_banner 标志`, 像下面.

```bash
ffmpeg -i video.mp4 -hide_banner
```

## 视频转换格式

`FFmpeg` 是强有力的音频和视频转换器, 因此, 它能在不同格式之间转换媒体文件. 举个例子, 要转换 `mp4` 文件到 `avi` 文件, 运行:

```bash
ffmpeg -i video.mp4 video.avi
```

类似地, 你可以转换媒体文件到你选择的任何格式. 例如, 为转换 YouTube `flv` 格式视频为 `mpeg` 格式, 运行:

```bash
ffmpeg -i video.flv video.mpeg
```

如果你想维持你的源视频文件的质量, 使用 `-qscale 0` 参数:

```bash
ffmpeg -i input.webm -qscale 0 output.mp4
```

为检查 `FFmpeg` 的支持格式的列表, 运行:

```bash
ffmpeg -formats
```

## 音频转换格式

[ffmpeg 音频转码](https://www.cnblogs.com/tangchun/p/9013622.html)
[视频提取mp3和flac转mp3](https://www.cnblogs.com/dylanchu/p/14531207.html)

可以先查看支持的编解码格式, 以及它们的名称

```bash
ffmpeg -hide_banner -codecs | grep DEA
```

假如想转换成`.aac`格式

```bash
## 转换一个文件
ffmpeg -hide_banner -i xx.flac -codec:a aac -ar 44100 -ac 2 -b:a 320k -map_metadata 0 -id3v2_version 3 xx.aac

## 批量转换, 为了处理带有空格的文件名, 定义 shell 内域分隔符为 \n\b
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

## 音频截取, 制作铃声

时间指定是 `hour:min:sec:milli`

```bash
ffmpeg  -hide_banner -ss 00:00:13.000  -t 00:01:33.000  -i  `
'Cyua - Sternengesang.mp3' `
-codec copy -map 0:a`
'Cyua - Sternengesang--rt.mp3'
```

`-map 0:a` 表示只转换 audio 部分, copy 表示编码直接复制

## flac转换成mp3

将`flac`文件转换为`mp3`文件使用以下命令即可,
同时会将`flac`文件的`Vorbis`注释转换为`mp3`的`ID3v2`元数据

```bash
ffmpeg -hide_banner -i input.flac -codec:a mp3 -ar 44100 -ac 2 -b:a 320k -map_metadata 0 -id3v2_version 3 output.mp3
```

## 提取视频中的音频

转换一个视频文件到音频文件, 只需具体指明输出格式, 像 `.mp3`, 或 `.ogg`, 或其它任意音频格式.
下面的命令将转换 `input.mp4` 视频文件到 `output.mp3` 音频文件.

```bash
ffmpeg -i input.mp4 -vn output.mp3
```

此外, 也可以对输出文件使用各种各样的音频转换编码选项:

```bash
ffmpeg -i input.mp4 -vn -ar 44100 -ac 2 -b:a 320k -f mp3 output.mp3
```

## 更改视频文件的分辨率

如果你想设置一个视频文件为指定的分辨率, 你可以使用下面的命令:

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

## 流指定符

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
