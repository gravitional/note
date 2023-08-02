# msvc 平台 msbuild 的使用

[MSBuild 命令行参考](https://learn.microsoft.com/zh-cn/visualstudio/msbuild/msbuild-command-line-reference?view=vs-2022)

使用 `MSBuild.exe` 生成项目或解决方案文件时,
可以包含几个开关来指定过程的各个方面.

每个开关都有两种形式: `-switch` 和 `/switch`.
本文档仅介绍 -switch 形式. 开关不区分大小写.
如果从 Windows 命令提示符之外的 shell 运行 MSBuild,
则开关的参数列表(用分号或逗号分隔)可能需要单引号或双引号,
以确保将列表传递到 MSBuild, 而不是由 shell 解释.

```cmd
MSBuild.exe [Switches] [ProjectFile]
```

## 示例

例如编译求解器程序:

```cmd
MSBuild.exe /p:Platform=x64 /p:Configuration=Debug /v:n C:\solver\src\build\solver.sln
```

下面的示例生成 `MyProject.proj` 项目的 rebuild 目标.

```cmd
MSBuild.exe MyProject.proj -t:rebuild
```

可以使用 `MSBuild.exe` 执行更复杂的生成.
例如, 可以使用它在解决方案中生成 特定项目 的特定目标.
下面的示例 重新生成 项目 `NotInSolutionFolder`,
并清理项目 `InSolutionFolder` (位于 `NewFolder` 解决方案文件夹中).

```cmd
msbuild SlnFolders.sln -t:NotInSolutionfolder:Rebuild;NewFolder\InSolutionFolder:Clean
```

## Arguments

参数 描述
ProjectFile 在指定项目文件中生成目标.
如果不指定项目文件,
则 `MSBuild` 会在当前工作目录中搜索以 `proj` 结尾的文件扩展名并使用该文件.
还可以为此参数指定 `Visual Studio 解决方案文件`(.sln).

## 开关

开关    缩写形式    描述

`-detailedSummary[:True 或 False]`; `-ds[:True 或 False]`;
如果为 True, 则在生成日志末尾显示有关生成的配置,
以及如何将它们安排到节点中的详细信息.

`-graphBuild[:True or False]`; `-graph[:True or False]`;
使 MSBuild 构造和生成项目图.
构图涉及标识对窗体依赖项的项目引用.
生成该图涉及到尝试在引用它们的项目之前生成项目引用, 这不同于传统的 MSBuild 计划.
需要 MSBuild 16 或更高版本.

`-help`; `/?` 或 `-h`; 显示用法信息.
以下命令是一个示例:

```cmd
msbuild.exe -?
```

+ `-ignoreProjectExtensions: extensions`; `-ignore: extensions`;
确定要生成的项目文件时忽略指定扩展名.
使用分号或逗号分隔多个扩展名, 如以下示例所示:

```cmd
-ignoreprojectextensions:.vcproj,.sln
```

+ `-inputResultsCaches[:cacheFile[;cacheFile2]`; `-irc[:cacheFile[;cacheFile2]`;
MSBuild 将从中读取生成结果的输入缓存文件的以分号分隔的列表.
如果 `-isolateProjects` 设置为 `False`, 这会将其设置为 `True`.

`-interactive[:True or False]`; `-`;
指示允许生成中的操作与用户交互.
不要在不期望交互的自动方案中使用此参数.
指定 -interactive 等效于指定 -interactive:true.
使用此参数重写来自响应文件的值.

+ `-isolateProjects[:True 或 MessageUponIsolationViolation 或 False]`;
`-isolate[:True 或 MessageUponIsolationViolation 或 False]`;
使 MSBuild 单独生成每个项目.
当设置为 `MessageUponIsolationViolation`(或其简短形式 Message)时,
如果提供了 `-outputResultsCache` 开关, 则仅序列化顶级目标中的结果.
这是为了减少依赖项项目中违反隔离规定的目标, 由于依赖于缓存目标而使用不正确的状态的机会,
因为缓存目标的副作用不会被考虑在内.(例如, 属性的定义.)
这是 MSBuild 更严格的模式, 因为它要求在计算时可以静态发现项目图,
但生成大量项目时可以改进计划并减少内存开销.

`-lowPriority[:True 或 False]`; `-low[:True 或 False]`;
导致 MSBuild 以较低的进程优先级运行.
指定 `-lowPriority` 与指定 `-lowPriority:True` 相同.

+ `-maxCpuCount[:number]`; `-m[:number]`;
指定生成时要使用的 最大并发进程数.
如果不包含此开关, 则默认值为 `1`.
如果包含此开关而 没有指定值, MSBuild 将使用计算机中的处理器总数作为其值.
有关详细信息, 请参阅并行生成多个项目.

下面的示例指示 `MSBuild` 使用三个 `MSBuild` 进程进行生成,
这允许同时生成三个项目:

```cmd
msbuild myproject.proj -maxcpucount:3
```

+ `-noAutoResponse`; `-noautorsp`;
不自动包含任何 `MSBuild.rsp` 或 `Directory.Build.rsp` 文件.

`-nodeReuse:value` `-nr:value` 启用或禁用 MSBuild 节点的重复使用.
 你可以指定以下值:
`True`.
节点在生成完成之后保留, 以便后续生成可以使用它们(默认值).
`False`.
节点在生成完成之后不保留.

节点对应于正在执行的项目.
 如果你添加 -maxcpucount 开关, 多个节点可并发执行.

`-nologo`; 不显示启动版权标志或版权消息.

`-preprocess[:filepath]` `-pp[:filepath]`
通过内联会在生成期间导入的所有文件(标记其边界)创建单一的聚合项目文件.
可以使用此开关更轻松地确定所导入的文件, 从中导入文件的位置以及参与生成的文件.
使用此开关时, 不生成项目.

如果指定 filepath, 则会将聚合项目文件输出到文件.
否则, 输出将显示在控制台窗口中.

有关如何使用 Import 元素将项目文件插入到其他项目文件的详细信息,
请参阅 [Import 元素 (MSBuild)](https://learn.microsoft.com/zh-cn/visualstudio/msbuild/import-element-msbuild)
和[如何: 在多个项目文件中使用同一目标](https://learn.microsoft.com/zh-cn/visualstudio/msbuild/how-to-use-the-same-target-in-multiple-project-files).

`-outputResultsCache[:cacheFile]` `-orc[:cacheFile]`
MSBuild 将在生成结束时将生成结果缓存的内容写入 Output 缓存文件.
如果 -isolateProjects 设置为 False, 这会将其设置为 True.

`-profileEvaluation:<file>`; `-`
分析 MSBuild 计算, 并将结果写入指定的文件.
如果指定的文件的扩展名为".md", 则以 Markdown 格式生成结果.
否则, 将生成制表符分隔文件.

`-property:name=value` `-p:name=value`
设置或重写指定项目级属性, 其中 name 是属性名称, value 是属性值.
单独指定每个属性, 或使用分号或逗号分隔多个属性, 如以下示例所示:

```cmd
-property:WarningLevel=2;OutDir=bin\Debug
```

`-restore` `-r` 在生成实际目标之前运行 `Restore` 目标.

`-restoreProperty:name=value` `-rp:name=value`
仅在还原期间设置或重写这些项目级别的属性, 不要使用通过 -property 参数指定的属性.
name 是属性名称, value 是属性值.
使用分号或逗号分隔多个属性, 或单独指定每个属性.

+ `-target:targets` `-t:targets`
在项目中生成指定目标.
单独指定每个目标, 或使用分号或逗号分隔多个目标, 如以下示例所示:

```cmd
-target:PrepareResources;Compile
```

如果使用此开关指定任何目标, 则它们会代替项目文件中的 DefaultTargets 特性中的任何目标来运行.
有关详细信息, 请参阅目标生成顺序和如何: 指定首先生成的目标.

目标是一组任务. 有关详细信息, 请参阅目标.

`-targets[:file]` `-ts[:file]`
将可用目标的列表写入指定的文件(如果未指定任何文件, 则写入输出设备), 而无需实际执行生成过程.

`-toolsVersion:version` `-tv:version`
指定自定义工具集.
包含用于生成应用程序的任务, 目标和工具的工具集.
请参阅工具集 (ToolsVersion) 和标准和自定义工具集配置.

`-validate:[schema]` `-val[schema]`
验证项目文件, 如果验证成功, 则生成项目.
如果没有指定 schema, 则针对默认架构验证项目.
如果指定 schema, 则针对指定的架构验证项目.
下面的设置是一个示例:

```cmd
-validate:MyExtendedBuildSchema.xsd
```

+ `-verbosity:level` `-v:level` 指定要在生成日志中显示的信息量.
每个记录器基于为该记录器设置的详细级别显示事件.

可以指定以下详细级别: `q[uiet]`, `m[inimal]`, `n[ormal]`(默认), `d[etailed]` 和 `diag[nostic]`.

下面的设置是一个示例:

```cmd
-verbosity:quiet
```

+ `-version` `-ver` 仅显示版本信息.  不生成项目.

+ `@file`
从文本文件插入命令行开关.
 如果具有多个文件, 可单独指定它们.
 有关详细信息, 请参阅响应文件.

+ `-warnAsError[:code[;code2]`; `-err[:code[;code2]`
要视为错误的警告代码的列表.
使用分号或逗号分隔多个警告代码.
若要将所有警告视为错误, 请使用不带任何值的开关.
如果警告被视为错误, 则目标将像对待警告一样继续执行, 但总体生成会失败.
示例: `-err:MSB4130`

+ `-warnNotAsError[:code[;code2]` `-noerr[:code[;code2]` 不应提升为错误的警告代码列表.
 具体而言, 如果 warnAsError 开关设置为将所有警告提升为错误, 则不会提升使用 warnNotAsError 指定的错误代码.
 如果 warnAsError 未设置为将所有警告提升为错误, 则这不起作用.
 使用分号或逗号分隔多个警告代码.
示例: `-noerr:MSB4130`

+ `-warnAsMessage[:code[;code2]`; `-noWarn[:code[;code2]`
要视为低重要性消息的警告代码的列表.
使用分号或逗号分隔多个警告代码.
示例: `-noWarn:MSB3026`

## 记录器的开关

开关 缩写形式 描述

### `-binaryLogger[:[LogFile=]output.binlog [;ProjectImports=[None,Embed,ZipFile]]]`; `-bl`

将所有生成事件串行化为压缩的二进制文件.
默认情况下, 该文件位于当前目录中, 名称为 msbuild.binlog.
二进制日志是对生成过程的详细描述,
可在将来用于重建文本日志, 或由其它分析工具使用.
二进制日志的大小通常只有最详细文本诊断级日志的 1/10 到 1/20,
但却能包含更多信息.

二进制记录器默认收集项目文件的源文本, 包括所有导入的项目,
以及在生成期间所遇到的目标文件.
可选的 `ProjectImports` 开关控制此行为:
    + ProjectImports=None.
    不收集项目导入.
    + ProjectImports=Embed.
    在日志文件中嵌入项目导入(默认).
    + ProjectImports=ZipFile.
    将项目文件保存至 <output>.projectimports.zip,
    其中 <output> 的名称与二进制日志文件的名称相同.

`ProjectImports` 的默认设置为"嵌入".

请注意: 记录器不收集 `.cs`, `.cpp` 等非 MSBuild 源文件 .

将 `.binlog` 文件作为参数而非项目/解决方案传递给 msbuild.exe 可"播放"该文件 .
其他记录器将接收日志文件中包含的信息, 就像原始生成发生时那样.
若要详细了解二进制日志及其使用情况,
可访问 https://github.com/dotnet/msbuild/blob/main/documentation/wiki/Binary-Log.md

示例:

```cmd
-bl
-bl:output.binlog
-bl:output.binlog;ProjectImports=None
-bl:output.binlog;ProjectImports=ZipFile
-bl:..\..\custom.binlog
-binaryLogger
```

### `-consoleLoggerParameters:parameters`; `-clp:parameters`;

将指定的参数传递到控制台记录器, 后者会在控制台窗口中显示生成信息.
可以指定以下参数:

- PerformanceSummary.
显示在任务, 目标和项目中所花费的时间.
- Summary.
在末尾显示错误和警告摘要.
- NoSummary.
不在末尾显示错误和警告摘要.
- ErrorsOnly.
仅显示错误.
- WarningsOnly.
仅显示警告.
- NoItemAndPropertyList.
如果详细级别设置为 diagnostic, 则不在每个项目生成开头显示项和属性的列表.
- ShowCommandLine.
显示 TaskCommandLineEvent 消息.
- ShowProjectFile.
在诊断消息中显示项目文件的路径.
此设置默认启用.
- ShowTimestamp.
将时间戳显示为任何消息的前缀.
- ShowEventId.
显示每个已启动事件, 已完成事件和消息的事件 ID.
- ForceNoAlign.
不将文本与控制台缓冲区大小对齐.
- DisableConsoleColor.
将默认控制台颜色用于所有日志记录消息.
- DisableMPLogging.
在非多处理器模式下运行时, 禁用输出的多处理器日志记录样式.
- EnableMPLogging.
启用多处理器日志记录样式(即使在非多处理器模式下运行).
默认情况下, 此日志记录样式处于启用状态.
- ForceConsoleColor.
使用 ANSI 控制台颜色(即使控制台不支持该参数).
- Verbosity.
重写此记录器的 -verbosity 设置.

使用分号分隔多个参数, 如以下示例所示:

```cmd
-consoleloggerparameters:PerformanceSummary;NoSummary -verbosity:minimal
```

默认控制台记录器的详细级别为 `normal`, 并包括 `Summary`.

### `-distributedFileLogger` `-dfl`

将每个 MSBuild 节点的生成输出记录到其自己的文件.
这些文件的初始位置是当前目录.
默认情况下, 这些文件命名为 `MSBuild<NodeId>.log`.
可使用 `-fileLoggerParameters` 开关指定文件位置和 fileLogger 的其他参数.

如果你使用 `-fileLoggerParameters` 开关命名日志文件,
分布式记录器会在为每个节点创建日志文件时,
将相应名称用作模板, 并将节点 ID 追加到相应名称中.

### `-distributedLogger: central logger forwarding logger `

`-dl:central loggerforwarding logger `

记录 MSBuild 中的事件, 将不同记录器实例附加到每个节点.
若要指定多个记录器, 请分别指定每个记录器.

使用记录器语法指定记录器.
有关记录器语法, 请参阅下面的 logger 开关.

下面的示例演示如何使用此开关:

```cmd
-dl:XMLLogger,MyLogger,Version=1.0.2,Culture=neutral
-dl:MyLogger,C:\My.dll*ForwardingLogger,C:\Logger.dll
```

### `-fileLogger [number]`; `-fl[number] `

将生成输出记录到当前目录中的单个文件.
如果没有指定 `number`, 输出文件名为 `msbuild.log`.
如果指定 `number`, 输出文件名为 `msbuild<n>.log`, 其中 `<n>` 为 number.
`Number` 可以是 `1` 到 `9` 的数字.

可使用 `-fileLoggerParameters` 开关指定文件位置和 fileLogger 的其他参数.

### `-fileLoggerParameters[number]: parameters -flp[ number]: parameters`

为文件记录器和分布式文件记录器指定任何额外参数.
若有此开关, 表明存在对应的 `-filelogger[number]` 开关.
`Number` 可以是 `1` 到 `9` 的数字.

可使用针对 `-consoleloggerparameters` 列出的所有参数.
还可以使用以下一个或多个参数:

- LogFile.
写入生成日志的日志文件的路径.
分布式文件记录器将此路径用作其日志文件的名称的前缀.

- Append.
确定是将生成日志追加到日志文件还是覆盖它.
设置该开关时, 生成日志将追加到日志文件.
此开关不存在时, 将覆盖现有日志文件的内容.

示例:

```cmd
msbuild myfile.proj -flp:FileLogger,Microsoft.Build;logfile=MyLog.log;append
```

如果包含显式 `true` 或 `false` 设置, 则无论设置如何, 都将追加日志.
如果不包含追加开关, 则会覆盖日志.

在此例中会覆盖文件:

    msbuild myfile.proj -flp:FileLogger,Microsoft.Build;logfile=MyLog.log

在此例中会追加文件:

    msbuild myfile.proj -flp:FileLogger,Microsoft.Build;logfile=MyLog.log;append=true

在此例中会追加文件:

    msbuild myfile.proj -flp:FileLogger,Microsoft.Build;logfile=MyLog.log;append=false

- Encoding.
 指定文件的编码(例如, UTF-8, Unicode 或 ASCII).

下面的示例为警告和错误生成单独的日志文件:

    -flp1:logfile=errors.txt;errorsonly -flp2:logfile=warnings.txt;warningsonly

下面的示例演示其他可能性:

```cmd
-fileLoggerParameters:LogFile=MyLog.log;Append; Verbosity=diagnostic;Encoding=UTF-8

-flp:Summary;Verbosity=minimal;LogFile=msbuild.sum

-flp1:warningsonly;logfile=msbuild.wrn

-flp2:errorsonly;logfile=msbuild.err
```

`-logger: logger` `-l:logger`;
指定要用于记录 MSBuild 中的事件的记录器.
若要指定多个记录器, 请分别指定每个记录器.

将以下语法用于 `logger`:

    [``LoggerClass``,]``LoggerAssembly``[;``LoggerParameters``]

将以下语法用于 `LoggerClass`:

    [``PartialOrFullNamespace``.]``LoggerClassName

如果程序集恰好包含一个记录器, 则不必指定记录器类.
将以下语法用于 `LoggerAssembly`:

    {``AssemblyName``[,``StrongName``] &#124;AssemblyFile``}

记录器参数是可选的, 传递给记录器时与输入时完全一致.
下面的示例使用 `-logger` 开关.

```cmd
-logger:XMLLogger,MyLogger,Version=1.0.2,Culture=neutral

-logger:XMLLogger,C:\Loggers\MyLogger.dll;OutputAsHTML
```

`-noConsoleLogger`; `-noconlog`
禁用默认控制台记录器, 不将事件记录到控制台.

请参阅
[MSBuild 参考](https://learn.microsoft.com/zh-cn/visualstudio/msbuild/msbuild-reference?view=vs-2022)
[常用的 MSBuild 项目属性](https://learn.microsoft.com/zh-cn/visualstudio/msbuild/common-msbuild-project-properties?view=vs-2022)
