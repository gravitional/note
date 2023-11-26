# vs 调试技巧 natvis

[使用 Natvis 框架在调试器中创建 C++ 对象的自定义视图](https://learn.microsoft.com/zh-cn/visualstudio/debugger/create-custom-views-of-native-objects?view=vs-2022)
[你很可能需要知道这个调试小技巧](https://bianchengnan.gitee.io/articles/view-variables-in-your-own-way-using-natvis-in-visual-studio/)
[Windows下VSCode+CMake搭建开发环境](https://zhuanlan.zhihu.com/p/370211322)

## vs 创建离线安装包

[创建和维护 Visual Studio 的网络安装](https://learn.microsoft.com/en-us/visualstudio/install/create-a-network-installation-of-visual-studio)
[创建 Visual Studio 的脱机安装包以进行本地安装](https://learn.microsoft.com/zh-cn/visualstudio/install/create-an-offline-installation-of-visual-studio)
[VS2022离线安装包--下载流程](https://blog.csdn.net/Somnr_m/article/details/112780221)

### 下载 Visual Studio 引导程序以创建布局

VS 不同的安装形态称为 layout.

为所需的 Visual Studio 版本下载正确的引导程序(bootstrapper),
并将其复制到要作为 layout's repository 的目录中.
创建布局后, 就可以用它将 Visual Studio 安装到任何客户端机器上.

bootstrapper 是用于创建, 更新 和 执行 其他布局操作的 executable.
您必须有互联网连接才能完成此步骤.

以下引导程序无论何时运行,
都将在 `Current channel` 上安装最新, 最安全的 Visual Studio 2022 版本.
另外, 如果您想创建或更新布局到 Visual Studio 2022 的特定版本或特定通道,
请访问 [Visual Studio 2022 发布历史页面](https://learn.microsoft.com/en-us/visualstudio/releases/2022/release-history#release-dates-and-build-numbers),
该页面上有每个通道上每个服务版本的 evergreen 和 fixed version 引导程序的链接,
然后下载您想要的引导程序, 将其复制到要作为 layout source 的目录中.

如果您之前下载了引导程序文件, 并想验证它将安装的版本, 方法如下.
在 Windows 中, 打开文件资源管理器, 右键单击引导程序文件,
选择 `属性`, 然后选择 `详细信息` 选项卡.
`产品版本` 字段将描述引导程序将安装的 通道 和 版本.
版本号应始终理解为 `指定的最新服务版本`,
除非明确指定, 否则通道应假定为 `Current` 通道.
因此, 产品版本为 LTSC 17.0 的引导程序将安装 17.0 LTSC 频道上的最新 `17.0.x` 服务版本.
产品版本为 `Visual Studio 2022` 的引导程序将安装 `Current ` 通道上的最新服务版本 `Visual Studio 2022`.

### 下载 Visual Studio 软件包

必须有互联网连接才能完成此步骤.

打开 elevated cmd, 导航到下载有 bootstrapper 的目录,
然后 [使用命令行参数安装 Visual Studio 页面][]
中定义的 bootstrapper 参数来创建和维护网络布局.
以下示例说明了创建初始布局的常用方法.
更多示例请参见 [Visual Studio安装命令行参数示例][] 页面.

对于 Visual Studio Community,
一个完整的单语言本地化初始布局需要约 40 GB 的磁盘空间,
对于 Visual Studio Enterprise, 则需要约 50 GB 的磁盘空间.
额外的语言本地化每个需要大约 半GB 的空间.

建议的方法是创建包含所有 工作负载 和 适当语言的 Visual Studio 初始布局,
并将软件包存储到网络服务器上的 laryout目录 中.
这样, 任何客户端安装都可以访问整个 Visual Studio 产品, 并能安装任何子集.
要创建完整的 Visual Studio 布局, 请在计划托管网络layout的目录下运行以下命令:

`--layout` 用于指定 VS2022 相关组件的下载目录;

```powershell
vs_enterprise.exe --layout c:\VSLayout
```

类似地有:

```powershell
# 创建包含所有功能和所有语言的完整 本地layout
vs_enterprise.exe --layout c:\localVSlayout

# 创建仅支持一种语言的 C++ 桌面开发(包括所有推荐组件和可选组件)
vs_enterprise.exe --layout c:\localVSlayout --add Microsoft.VisualStudio.Workload.NativeDesktop --includeRecommended --includeOptional --lang en-US
```

VS 各种版本的可用组件 see [Visual Studio工作负荷和组件ID](https://learn.microsoft.com/zh-cn/visualstudio/install/workload-and-component-ids?view=vs-2022)
例如 VS 生成工具 see [Visual Studio 生成工具组件目录](https://learn.microsoft.com/zh-cn/visualstudio/install/workload-component-id-vs-build-tools?view=vs-2022)

例如下载 VS生成工具组件: MSBuild 工具, 通用 Windows 平台生成工具, 使用 C++ 的桌面开发 到当前目录

```powershell
.\vs_BuildTools.exe --layout .\localVSlayout --add 'Microsoft.VisualStudio.Workload.MSBuildTools' 'Microsoft.VisualStudio.Workload.VCTools' 'Microsoft.VisualStudio.Workload.UniversalBuildTools' --includeRecommended --includeOptional --lang Zh-cn
```

[使用命令行参数安装 Visual Studio 页面]: https://learn.microsoft.com/en-us/visualstudio/install/create-a-network-installation-of-visual-studio?view=vs-2022
[Visual Studio安装命令行参数示例]: https://learn.microsoft.com/en-us/visualstudio/install/command-line-parameter-examples?view=vs-2022

### 从本地布局安装 Visual Studio

当你从本地布局安装 Visual Studio 时,
Visual Studio 安装程序会使用这些文件的本地版本.
不过, 如果你在安装过程中选择的组件不在布局中,
则 Visual Studio 安装程序将尝试从 Internet 下载.
若要确保仅安装先前下载的文件, 请使用在创建本地布局时所用的相同命令行选项.
若要确保安装程序安装产品时不会尝试访问 Internet, 请使用 `--noweb` 开关.

例如, 如果使用以下命令创建了本地安装布局:

```powershell
vs_enterprise.exe --layout c:\localVSlayout --add Microsoft.VisualStudio.Workload.ManagedDesktop --add Microsoft.VisualStudio.Workload.NetWeb --includeOptional --lang en-US
```

然后使用此命令运行安装, 并阻止客户端计算机访问 Internet:

```powershell
c:\localVSlayout\vs_enterprise.exe --noWeb --add Microsoft.VisualStudio.Workload.ManagedDesktop --add Microsoft.VisualStudio.Workload.NetWeb --includeOptional
```

>重要
如果使用 Visual Studio Community,
系统可能会提示你要在安装后的 30 天内登录, 但这不会影响你使用该产品的功能.

>备注
如果你遇到签名无效的错误, 则必须安装更新的证书.
在本地布局中打开证书文件夹.
双击每个证书文件, 然后单击完成证书管理器向导.
如果要求输入密码, 请将密码留空.

## 使用 winget

使用 Windows 程序包管理器 `winget` 工具,
以编程方式在计算机上安装或更新 Visual Studio, 还可同时安装或更新 winget 管理的其他包.
要自定义安装并指定其他工作负载和组件, 可将 winget 的 `--override` 开关,
与 winget 的 `install` 命令一起使用, 并传入导出的 `vsconfig` 文件, 如下所示:

```powershell
winget install --id Microsoft.VisualStudio.2022.Community --override "--passive --config C:\my.vsconfig"
```

还可使用 [winget configure][] 并传入 `.yaml` 文件来修改现有的 Visual Studio 安装.
此方法使用此处所述的[Visual Studio PowerShell DSC 提供程序](https://www.powershellgallery.com/packages/Microsoft.VisualStudio.DSC).

[winget configure]: https://learn.microsoft.com/zh-cn/windows/package-manager/configuration/