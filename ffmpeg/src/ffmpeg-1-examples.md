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
