# ffmpeg将图片合成gif

[ffmpeg将图片合成gif](https://zhuanlan.zhihu.com/p/585480111)
[ffmpeg实例, 比特率码率](https://blog.csdn.net/yu540135101/article/details/84346146)

## 制作GIF

```bash
ffmpeg -r 2 -i image%4d.png out.gif
```

此命令可以制作一个 `2fps` 的动图,
其中 `image%4d.png` 是图片文件名称的模式, `%4d` 表示名称包含 `4`个数字,

```bash
image0000.png
image0001.png
...
```

但是在制作的时候会遇到了失色的情况(在图像周围有诡异的黄色出现).
为了解决此问题, 可以先为每一张图片生成一个调试板, 然后根据调色板制作gif图片.

```bash
ffmpeg -r 12 -i image.%4d.png -vf palettegen tmp.png
ffmpeg -r 12 -i image.%4d.png -i tmp.png -lavfi paletteuse out.gif
```

## 将gif转换成MP4

```bash
ffmpeg -f gif -i input.gif output.mp4
```

mp4生成Gif

```bash
ffmpeg -i input.mp4 name.gif
```

不清晰, 改用以下命令

```bash
ffmpeg -i input.mp4 -vf palettegen palette.png
ffmpeg -i input.mp4 -i palette.png -lavfi paletteuse out.gif
```

也可以简化为一条命令:

```bash
ffmpeg -i input.mp4 -vf "split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" out.gif
```

还可以增加分辨率:

```bash
ffmpeg -i input.mp4 -vf "split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -s 480*320 -r 10 out.gif
```

-s 表示分辨率改为多少
-r 多少fps
-vf 表示生产画板palettegen
-lavfi 表示使用画板paletteuse
