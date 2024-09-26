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

## 登录

为了下载高清晰视频, 可以登录,
最好使用 TV 账号登陆

```bash
bbdown logintv
```

使用手机客户端扫描 二维码 登录.

## 运行

### general 示例

```bash
# `--only-show-info` ; 仅解析而不进行下载, 先查看视频流信息
bbdown -tv -info  https://www.bilibili.com/xxxx
# 下载所有 分p
bbdown -tv -p 'ALL' -e "hevc,av1,avc" -q "1080P 高帧率, 1080P 高清, 720P 高清"   -mt --work-dir 'C:/Users/qingz/Downloads' 'https://www.bilibili.com/xxxxx'
# 下载 p1
bbdown -tv -p 1 -e "hevc,av1,avc" -q "4K 超清, 1080P 高帧率, 1080P 高清, 720P 高清"  -mt --work-dir 'C:/Users/qingz/Downloads' 'https://www.bilibili.com/xxxxx'
```

### example 1

```bash
bbdown -tv -info  https://www.bilibili.com/bangumi/play/ss38384?spm_id_from=333.337.0.0
bbdown -tv -p ALL -e "hevc,av1,avc" -q "4K 超清, 1080P 高帧率, 1080P 高清, 720P 高清"  -mt --work-dir 'C:\Users\qingz\Downloads\digaAutoMan' https://www.bilibili.com/bangumi/play/ss38384?spm_id_from=333.337.0.0
```
