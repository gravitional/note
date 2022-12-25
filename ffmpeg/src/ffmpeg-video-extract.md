# ffmpeg视频精准剪切

[ffmpeg视频精准剪切](https://zhuanlan.zhihu.com/p/97914917)

## 导言

`ffmepg` 剪切视频, 很方便, 但是也有很大缺陷:

+ 剪切时间点不精确
+ 有时剪切的视频开头有黑屏

造成这些问题的原因是 `ffmpeg` 无法 `seek` 到非关键帧上.
一下本文通过一些参数配置尽可能地减轻以上问题

## 基本剪切方法

### 根据 开始时间, 持续时长 切割视频

```bash
# 使用 hour:minute:seconds
ffmpeg  -ss 00:01:00  -t 00:02:00 -i input.mp4  -codec copy -avoid_negative_ts 1 out.mp4
# 使用 seconds
ffmpeg -ss 10 -t 15 -i input.mp4 -codec copy -avoid_negative_ts 1 out.mp4
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

### 根据 开始时间, 结束时间 切割视频

[ffmpeg时间有关的操作](https://www.cnblogs.com/yongfengnice/p/7156952.html)
[ffmpeg根据开始时间结束时间切割视频](https://blog.csdn.net/fjh1997/article/details/106037265)

根据开始时间结束时间, `-to 结束时间`.
注意这几个参数的顺序不能错误, `-i` 必须要第一个

```bash
ffmpeg  -i input.mp4 -ss 00:01:00 -to 00:02:00 -codec copy output1.mp4
```

### 参数优化

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
ffmpeg -ss 10 -t 15 -accurate_seek -i test.mp4 -codec copy -avoid_negative_ts 1 out.mp4
```

也可以尝试下面的命令

```bash
ffmpeg  -y -ss 10 -t 15 -i test.mp4 -c:v libx264 -c:a aac -strict experimental -b:a 98k out.mp4
```

## 合并多个视频

[使用ffmpeg合并多个视频文件](https://blog.csdn.net/winniezhang/article/details/89260841)

可以将 `输入文件列表` 写到 `txt` 中, 然后合并视频. 例如`file.txt`的内容如下:

```txt
file 'output1.mp4'
file 'output2.mp4'
file 'output3.mp4'
```

然后:

```bash
ffmpeg -f concat -i file.txt -codec copy output.mp4
```

+ windows 环境下使用例子:

```powershell
ffmpeg  -ss 01:26:03  -t 00:33:49  -i  `
'ipnut.mp4'  `
-codec copy -avoid_negative_ts 1 `
'output.mp4'
```

也可以直接使用命令合并

```bash
ffmpeg -y -safe 0 -i "concat:i1.mp4 | i2.mp4 | i3.mp4" -codec copy output.mp4
```

## 视频合并Unsafe file name

[ffmpeg视频合并Unsafe file name问题解决](https://blog.csdn.net/mp624183768/article/details/81603602)

使用ffmpeg进行多个视频合并过程中出现了 `unsafe file name` 错误, 解决的办法是加个 `-safe 0` 参数:

```bash
ffmpeg.exe -y -f concat -safe 0 -i files.txt -codec copy  out.mp4
```
