# VS 命令行编译

[VS 2017 命令行编译 sln](https://blog.csdn.net/guo_lei_lamant/article/details/108713255)
[Devenv 命令行开关](https://learn.microsoft.com/zh-cn/visualstudio/ide/reference/devenv-command-line-switches?view=vs-2022)
[/Build (devenv.exe)](https://learn.microsoft.com/zh-cn/visualstudio/ide/reference/build-devenv-exe?source=recommendations&view=vs-2022)

## 工具

用到的命令行工具是devenv.com, 帮助手册如下:

```powershell
&'C:\Program Files\Microsoft Visual Studio\2022\Community\Common7\IDE\devenv.com' /?
```

用法:
devenv [解决方案文件 | 项目文件 | 任意文件.扩展名] [开关]

devenv 的第一个参数通常是一个解决方案文件或项目文件.
如果您希望在编辑器中自动打开文件,
也可以使用任何其他文件作为第一个参数.
当您输入项目文件时,
IDE 会在项目文件的父目录中查找与该项目文件具有相同基名称的 .sln 文件.
如果不存在这样的 .sln 文件,
IDE 将查找引用该项目的单个 .sln 文件.
如果不存在这样的单个.sln 文件,
则 IDE 将创建一个具有默认 .sln 文件名的未保存的解决方案,
其基名称与项目文件的基名称相同.

命令行生成:

devenv 解决方案文件.sln /build [ 解决方案配置 ] [ /project 项目名称或文件 [ /projectconfig 名称 ] ]
可用的命令行开关:

`/Build`          使用指定的解决方案配置生成解决方案或项目.
例如 `Debug`. 如果可能存在多个平台, 则配置名称必须用引号括起来, 并且包含平台名称. 例如 `Debug|Win32`, `Debug|x64`
`/Clean`          删除生成结果.
`/Command`        启动 IDE 并执行该命令.
`/Deploy`         生成并部署指定的生成配置.
`/Edit`           在此应用程序的运行实例中打开指定文件. 如果没有正在运行的实例, 则启动一个具有简化窗口布局的新实例.
`/LCID`           设置 IDE 中用于用户界面的默认语言.
`/Log`            将 IDE 活动记录到指定的文件以用于疑难解答.
`/NoVSIP`         禁用用于 VSIP 测试的 VSIP 开发人员许可证密钥.
`/Out`            将生成日志追加到指定的文件中.
`/Project`        指定生成, 清理或部署的项目. 必须和 /Build, /Rebuild, /Clean 或 /Deploy 一起使用.

/ProjectConfig  重写解决方案 配置中指定的项目配置. 例如"Debug".
如果可能存在多个平台, 则配置名称必须用引号括起来并包含平台名称.
例如"Debug|Win32". 必须和 /Project 一起使用.

`/Rebuild`        先清理, 然后使用指定配置生成解决方案或项目.
`/ResetSettings`  恢复 IDE 的默认设置, 还可以重置为指定的 VSSettings 文件.
`/ResetSkipPkgs`  清除所有添加到 VSPackages 的 SkipLoading 标记.
`/Run`            编译并运行指定的解决方案.
`/RunExit`        编译并运行指定的解决方案然后关闭 IDE.
`/SafeMode`       以安全模式启动 IDE, 加载最少数量的窗口.

`/Upgrade`        升级项目或解决方案以及其中的所有项目. 并相应地创建这些文件的备份.
有关备份过程的详细信息, 请参见 "Visual Studio 转换向导"上的帮助.

产品特定的开关:

`/debugexe`       打开要调试的指定可执行文件. 将命令行的剩余部分作为参数传递给此可执行文件.
`/diff`           比较两个文件.  采用四个参数:SourceFile, TargetFile, SourceDisplayName (可选),
                TargetDisplayName (可选)
`/TfsLink`        打开团队资源管理器并为提供的项目 URI 启动查看器(如果注册了项目 URI).
`/useenv`        使用 PATH, INCLUDE, LIBPATH 和 LIB 环境变量而不是使用 VC++ 生成的 IDE 路径.

要从命令行附加调试器, 请使用:
VsJITDebugger.exe -p <pid>

## 编译

```powershell
# release x64(默认)
.\devenv.com F:\00Projects\test.sln /Build

# debug x64
.\devenv.com F:\00Projects\test.sln /Build "Debug|x64"

# debug win32
.\devenv.com F:\00Projects\test.sln /Build "Debug|Win32"
```
