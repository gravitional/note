# dotnet baisc

## 安装, 配置

[在 Windows 上安装 .NET](https://learn.microsoft.com/zh-cn/dotnet/core/install/windows?tabs=net80)
[dotnet nuget add source](https://learn.microsoft.com/zh-cn/dotnet/core/tools/dotnet-nuget-add-source)

首先安装 `.net SDK`, 打开 pwsh, 使用 `winget` 安装

```bash
winget install Microsoft.DotNet.SDK.8
```

```bash
dotnet --help #查看帮助
```

默认好像没有配置在线 package source, 使用 dotnet nuget add source 命令添加

```bash
# 列出当前 source 列表
dotnet nuget list source
# 将 nuget.org 添加为源：
dotnet nuget add source https://api.nuget.org/v3/index.json -n nuget.org
```

## build 项目

```bash
# 还原项目 package 依赖
dotnet restore

# 构建项目的 release 版本
dotnet build -c Release
# 构建 publish 版本
dotnet publish -c Release

# 指定 编译结果的 输出位置
dotnet build -c Release -o ~/Downloads/build-test
```
