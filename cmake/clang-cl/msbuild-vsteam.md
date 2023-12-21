## VCBuild vs. C++ MSBuild on the Command Line

在 Visual Studio 2010 中,
命令行工具 vcbuild.exe 将被 msbuild.exe 取代.
可执行文件的改变意味着开关也将改变.
为了让迁移更容易, 我创建了这个表格, 作为新开关的快速指南,
并强调了工具之间的一些差异.
下表并不是两个工具中提供的所有开关的完整表格.

迁移到 MSBuild 需要使用不同扩展名(`vcxproj`)的 new project type.
Visual Studio 提供了两种转换现有项目和解决方案的工具.

+ 在处理单个项目时, `vcupgrade.exe <filename>.vcproj` 工具可提供快速转换.
+ 在处理解决方案中的多个项目时, 可使用 devenv 转换整个解决方案(`.sln`)和其中的所有项目.
一旦项目或解决方案转换无误, 就可以使用 `MSBuild`.

在没有明确 project configuration 的情况下调用时,
VCBuild 会默认构建所有 Configuration and Platform matrix,
而 MSBuild 则只构建默认的 `Debug | Win32`.

在 MSBuild 中, 通过 `/p[roperty]` 开关启用的任何功能 也可以通过设置
相应名称的 环境变量 来启用.

例如, 在命令行中设置 `set Configuration=Debug` 与
在所有 `MSBuild` 执行中传递 `/p:Configuration=Debug` 是一样的.

`[]`中的文字 = 用于帮助记忆开关的可选字符

+ Build Project
`VCBuild.exe <projectname.vcproj>`
`MSBuild.exe <projectname.vcxproj>`

+ Build Solution
VCBuild.exe <solutionname.sln>
MSBuild.exe <solutionname.sln>

+ Rebuild
`/rebuild`
`/t[arget]:rebuild`

+ Clean
`/clean`
`/t[arget]:clean`

+ 为 `INCLUDE` 和 `LIB` 使用环境变量
`/useenv`
`/p[roperty]:useenv=true`

+ 多处理器编译(见下文)
`/m#`
`/m:#`

+ 平台
`/platform:<platform>`
`/p:Platform=<platform>`

+ 配置
`<configuration>`
`/p:Configuration=<configuration>`

+ 强制链接(将始终执行link)
`/forcelink`
`/t:BuildLink`

+ Passes
`/pass0`
`/t:BuildGenerateSources`

`/pass1`
`/t:BuildCompile`

`/pass2`
`/t:BuildLink /p:BuildType=Build`
(BuildType 属性启用增量编译跟踪功能)

命令行编译默认使用单个节点(`/m:1`).
我们鼓励使用 n 个节点, 其中 n 等于机器上的cores数.

## MSBuild 特定开关

+ 单文件编译(选定文件编译)
Specify the files and tool's target name
that would execute upon the files
`/t:<toolname> /p:SelectedFiles="<files>"`

例如
`/t:ClCompile`
`/p: SelectedFiles="StdAfx.cpp;main.cpp"`

目前, 我无法提供工具列表, 但 MSDN 上会有一份完整的可能目标列表.

+ 预处理项目文件
通过内联(inline)所有将在build过程中导入的文件来聚合(Aggregate )项目文件.
这实际上并不执行build.
`/PreProcess<:file>` or `/pp<:file>`
例如
`/pp:outfile.txt`

+ File logging 文件记录
将编译过程记录到 msbuild.log 中.
有关更多信息和选项, 请参阅 msbuild 帮助.
`/FileLogger` or `/fl`

+ Verbosity
忽略或增加有关编译的详细信息.
Quiet and would barely show

`/V[erbosity]:(q[uiet], m[inimal], n[ormal], d[etailed], or diag[nostic])`

+ 详细摘要
在构建结束时提供统计数据和摘要.
`/DetailedSummary` or `/ds`

你也可以使用以下 PowerShell 模块来定位 [MSBuild: vssetup.powershell](https://github.com/Microsoft/vssetup.powershell).

为了强调常用的开关, 我遗漏了许多可能的开关.
如需更多参考资料, 请参考以下链接:

[MSBuild 命令行参考](https://learn.microsoft.com/zh-cn/visualstudio/msbuild/msbuild-command-line-reference?view=vs-2022)
[命令行上的 VC++构建](https://learn.microsoft.com/zh-cn/cpp/build/building-on-the-command-line?view=msvc-170)

[新的 VC++项目/构建系统](https://learn.microsoft.com/zh-cn/visualstudio/msbuild/whats-new-msbuild-17-0?view=vs-2022)
