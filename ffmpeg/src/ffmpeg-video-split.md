# [使用FFMPEG将一个长视频分割为多个短视频](https://fishc.com.cn/thread-246020-1-1.html)

有时候，我们可能需要将一个比较长的视频，裁剪成多个短视频。

比如将一个时长为 1 小时的视频，拆分为两个 0.5 小时的视频，或者 10 个 6 分钟的视频。

虽然有些视频工具支持这个功能，但咱们主打一个杀鸡焉用牛刀，况且有时候还不顺手……

## 拆分成 2 个视频

这里，二师兄教大家使用 FFMEPG 来实现，只需要几个简单的指令即可。

假设输入视频文件名为 input.mp4：

```bash
# 提取前半小时
ffmpeg -i input.mp4 -t 1800 -c copy first_half.mp4

# 提取后半小时
ffmpeg -i input.mp4 -ss 1800 -c copy second_half.mp4
```

前半小时：
-t 1800：指定持续时间为1800秒（30分钟）。
-c copy：直接复制音视频流，不重新编码。

后半小时：
-ss 1800：从1800秒（30分钟）处开始。
-c copy：直接复制音视频流，不重新编码。

搞定~

## 拆分成多个短视频

要使用 FFMPEG 将一个长视频分割成多个短视频，可以使用 -segment_time 和 -f segment 参数。

假设你想将视频拆分为 6 分钟时长的小段，可以使用以下命令：

```bash
ffmpeg -i input.mp4 -c copy -map 0 -f segment -segment_time 360 -reset_timestamps 1 output%03d.mp4
```

-i input.mp4：指定输入视频文件。
-c copy：直接复制音视频流，不重新编码。
-map 0：确保所有流都被复制。
-f segment：使用分段模式。
-segment_time 360：每个段的时长为 `360` 秒（6 分钟）。
-reset_timestamps 1：重置每个分段的时间戳。
output%03d.mp4：生成的输出文件名格式（如 output001.mp4、output002.mp4 等）。

搞定~

当然，ffmpeg 还提供了许多额外参数供你使用，这里是一些常用的参数：

-i：指定输入文件。
-t：指定输出的持续时间。
-ss：指定开始时间。
-c:v 和 -c:a：指定视频和音频的编解码器。
-b:v 和 -b:a：指定视频和音频的比特率。
-vf 和 -af：应用视频和音频过滤器。
-r：设置帧率。
-s：设置视频分辨率。
-an：移除音频。
-vn：移除视频。