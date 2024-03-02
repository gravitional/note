# B站视频下载工具, bilibili

[ivenus/BBDown](https://gitee.com/venusboot/BBDown)
[nilaoda/BBDown](https://github.com/nilaoda/BBDown)
[FFmpeg 二进制Builds](https://www.gyan.dev/ffmpeg/builds/)

本软件已经以 [Dotnet Tool](https://www.nuget.org/packages/BBDown/) 形式发布

如果你本地有dotnet环境, 使用如下命令即可安装使用

```bash
dotnet tool install --global BBDown
```

如果需要更新bbdown, 使用如下命令

```bash
dotnet tool update --global BBDown
```

## 运行示例

```bash
./bbdown -p 'ALL' -e "hevc,av1,avc" -q "1080P 高帧率, 1080P 高清, 720P 高清"   -mt --work-dir 'C:/Users/qingz/Downloads' 'https://www.bilibili.com/xxxxx'
```
