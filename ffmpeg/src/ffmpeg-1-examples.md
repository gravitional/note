# ffmpeg 例子

水平翻转视频

```powershell
ffmpeg -i `
input.mp4 `
 -vf 'hflip' -avoid_negative_ts 1 `
output.mp4
```

截取视频片段

```powershell
ffmpeg  -ss 01:26:03  -t 00:33:49  -i  `
'ipnut.mp4'  `
-codec copy -avoid_negative_ts 1 `
'output.mp4'
```

截取音频片段

```powershell
ffmpeg  -hide_banner -ss 00:00:19.500  -to 00:01:50.000  -i  `
'input.mp3' `
-codec copy -map 0:a -y `
'output.mp3'
```

如果输入是 flac, 那么截取时长可能会有问题, 使用下面的命令, 即转码成 mp3 格式

```powershell
ffmpeg  -hide_banner -ss 00:00:19.500  -to 00:01:50.000  -i  `
'input.mp3' `
-map 0:a -y `
'output.mp3'
```
