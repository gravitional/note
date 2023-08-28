# windows小工具

[搜狗截图工具](https://www.52pojie.cn/thread-1523757-1-1.html)

首先到[搜狗输入法官网下载](https://shurufa.sogou.com/)输入法安装包,
以 压缩包方式打开, 解压缩,
在一串数字的文件夹里, 例如 `13.1.0.6834` 下面单拎出以下文件

```powershell
debug.log
screencapture.cupf
screencapture.exe
screencaptureconfig.ini
SGCurlHelper.dll
SGSmartAssistant.exe
SmartAssistant.log
```

点击 `screencapture.exe` 即可使用.

## Dependencies, 查看dll 依赖

[lucasg/Dependencies](https://github.com/lucasg/Dependencies)

A rewrite of the old legacy software "depends.exe" in C# for Windows devs,
to troubleshoot dll load dependencies issues.
