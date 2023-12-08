# visual studio 设置环境 vcvarsall.bat

## 在现有命令窗口中使用开发人员工具

在现有命令窗口中指定特定生成体系结构的最简单方法是使用 `vcvarsall.bat` 文件.
使用 vcvarsall.bat 设置环境变量以配置本机 32 位或 64 位编译的命令行.
参数可用于指定到 x86, x64, ARM 或 ARM64 处理器的交叉编译.
可以将 Microsoft Store, 通用 Windows 平台或 Windows 桌面平台作为目标.
甚至可以指定要使用的 Windows SDK, 并选择平台工具集版本.

在没有参数的情况下使用时, `vcvarsall.bat` 将配置环境变量,
使其使用当前面向 32 位 Windows 桌面版的 x86 位本机编译器.
可以添加参数来配置环境以使用任何本机或跨编译器工具.
如果你指定的配置未安装或在计算机上不可用, 则 `vcvarsall.bat` 显示错误消息.

### vcvarsall 语法

```cmd
vcvarsall.bat [architecture] [platform_type] [winsdk_version] [-vcvars_ver=vcversion] [spectre_mode]
```

### architecture

此可选参数指定要使用的主机和目标体系结构.
如果未指定体系结构, 则使用默认生成环境.
支持以下参数:

architecture  编译器  主机计算机体系结构  生成输出(目标)体系结构
x86  x86 32 位本机  x86, x64  x86
x86_amd64 或 x86_x64  x86 跨平台上的 x64  x86, x64  x64
x86_arm  x86 跨平台上的 ARM  x86, x64  ARM
x86_arm64  x86 跨平台上的 ARM64  x86, x64  ARM64
amd64 或 x64  x64 64 位本机编译器  X64  X64
amd64_x86 或 x64_x86  x64 跨平台上的 x86  x64  x86
amd64_arm 或 x64_arm  x64 跨平台上的 ARM  X64  ARM
amd64_arm64 或 x64_arm64  x64 跨平台上的 ARM64  x64  ARM64

### platform_type

可选择使用此自变量指定 store 或 uwp 作为平台类型.
默认情况下, 环境设置为生成桌面或控制台应用.

### winsdk_version

(可选)指定要使用的 Windows SDK 的版本.
默认情况下, 使用最新安装的 Windows SDK.
若要指定 Windows SDK 版本, 可使用完整的 Windows SDK 编号,
例如 10.0.10240.0, 或指定 8.1 以使用 Windows 8.1 SDK.

### vcversion

(可选)指定要使用的 Visual Studio 编译器工具集.
 默认情况下, 环境设置为使用当前的 Visual Studio 编译器工具集.

使用"-vcvars_ver=14.2x.yyyyy"指定 Visual Studio 2019 编译器工具集的特定版本.

使用"-vcvars_ver=14.29"指定 Visual Studio 2019 编译器工具集的最新版本.

使用"-vcvars ver=14.0"指定 Visual Studio 2015 编译器工具集.

### spectre_mode

请保留此参数, 以使用没有 Spectre 缓解措施的库.
使用 spectre 值以使用带有 Spectre 缓解措施的库.

### 在现有命令提示符窗口中设置生成环境

VS 2022 安装中的 `vcvarsall` 位于

```cmd
C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat
```

例如, 若要使用最新的 Windows SDK 和 Visual Studio 编译器工具集
在 64 位平台上为 UWP 生成 ARM 代码, 请使用以下命令行:

```cmd
xxx/vcvarsall.bat amd64_arm uwp
```

## pwsh 中运行 cmd 脚本设置环境变量

see repo [WintellectPowerShell](https://github.com/Wintellect/WintellectPowerShell/tree/master)

see pwsh 函数 [Invoke-CmdScript](./vs-env-vcvarsall-pwsh.ps1)
