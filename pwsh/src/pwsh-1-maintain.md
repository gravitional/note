# 日常维护

[Update-Module](https://docs.microsoft.com/en-us/powershell/module/powershellget/update-module)

## PSReadLine

### 升级

当运行以下建议的命令时, 请确保退出所有 `powershell.exe`, `pwsh.exe` 或 `pwsh` 的实例, 包括在 `VSCode` 终端打开的实例.

然后, 要确保 `PSReadLine` 没有被加载.

+ 如果你在 `Windows` 上, 从 `cmd.exe`, `powershell_ise.exe` 或通过 `Win+R` 的快捷方式运行下面的建议命令.
+ 如果你在 `Linux/MacOS` 上, 从默认终端(如 `bash` 或 `zsh` )运行下面的建议命令.

如果你使用的是 `Windows PowerShell` 中的 `PSReadLine` 版本, 你需要运行:

```powershell
powershell -noprofile -command "Install-Module PSReadLine -Force -SkipPublisherCheck -AllowPrerelease"
```

注意: 在运行这个命令之前, 你需要确保 `PowershellGet` 已经更新.

如果你使用的是 `PowerShell 6`以上版本的 `PSReadLine`, 你需要运行.

```powershell
<pwsh 程序的路径> -noprofile -command "Install-Module PSReadLine -Force -SkipPublisherCheck -AllowPrerelease"
```

如果你是自己从 `PowerShell Gallery` 安装的 `PSReadLine`, 取决于你的`PowerShell`版本, 你可以简单地运行:

```powershell
powershell -noprofile -command "Update-Module PSReadLine -AllowPrerelease"
# 或者
<pwsh 程序的路径>  -noprofile -command "Update-Module PSReadLine -AllowPrerelease"
```

如果你得到一个错误, 如.

```log
Remove-Item : Cannot remove item
C:\Users\{yourName}\Documents\WindowsPowerShell\Modules\PSReadLine\Microsoft.PowerShell.PSReadLine.dll: Access to the path
'C:\Users\{yourName}\Documents\WindowsPowerShell\Modules\PSReadLine\Microsoft.PowerShell.PSReadLine.dll' is denied.
```

或一个警告, 如:

```log
WARNING: The version '2.0.0' of module 'PSReadLine' is currently in use. Retry the operation after closing the applications.
```

那么说明, 你没有杀死加载 `PSReadLine` 的所有进程.

## PowerShellGet

[PowerShellGet系列](https://blog.csdn.net/itanders/article/details/74444945)
[PowerShellGet](https://docs.microsoft.com/en-us/powershell/module/powershellget/?view=powershell-7.2)

### 介绍

什么是 `PowerShellGet` ?

`PowerShellGet` 是一个`模块`(module),
包含`发现`, `安装`, `更新`和`发布` PowerShell 作品(artifacts)的命令, 如`Modules`, `DSC Resources`, `Role Capabilities`和`Scripts`.

`PowerShellGet` 里内置了一系列用于管理PowerShell模块与脚本的相关命令.
用户可以使用 PowerShellGet 内的相关命令获取在线的 PowerShell 模块和脚本.

`PowerShellGet` 默认会连接 [PowerShell Gallery 站点](https://www.powershellgallery.com/)并从那儿获取在线资源.

让我们来看看[上一篇关于OneGet的文章](https://blog.csdn.net/itanders/article/details/74278801)中提到的架构图,
`PowerShellGet` 作为 `PackageManagementProviders` 提供给用户各类相关功能命令使用,
而他的 `Package Source` 数据源来自于 `PowerShell Gallery`.

![OneGet 架构图](https://img-blog.csdn.net/20170705145017126?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvaXRhbmRlcnM=/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast)

我们知道 `OneGet` 是用来统一管理 `Windows` 平台上各种各样的套件包的组件,
而 `OneGet` 还支持各种 `Provider扩展`, 他把各种套件提供者(Provider)组合在一起供用户使用.
而 `PowerShellGet` 作为一个Provider扩展套件被集成在 `OneGet` 中, 可以被用于管理 `PowerShell`.

我们用命令查看下 `PackageManagement` 的 `Provider`, 发现是 `PowerShellGet`.
其他系统查看, 例如 : PSReadLine

```powershell
Get-Package -Name PSReadLine
```

这下你应该明白了, `PowerShellGet` 作为一个 `Provider` 扩展套件被集成在 `OneGet` 中.

### 常用命令

[用 PowerShellGet 管理你的模块包](https://blog.csdn.net/itanders/article/details/75305163)

在命令行中输入下面的指令, 来查看 `PowerShellGet` 中包含的命令:

```powershell
Get-Command -Module PowerShellGet
CommandType     Name                                               Version    Source
-----------     ----                                               -------    ------
Function        Find-Command                                       2.2.5      PowerShellGet
Function        Find-DscResource                                   2.2.5      PowerShellGet
Function        Find-Module                                        2.2.5      PowerShellGet
...
```

我们获得了各类用于管理的相关命令, 为了方便查看, 可以用下面的命令进行排版:

```powershell
Get-Command -Module PowerShellGet | Group-Object -Property Verb
Count Name                      Group
----- ----                      -----
    5 Find                      {Find-Command, Find-DscResource, Find-Module, Find-RoleCapability…}
...
```

可以看到, 有:

+ Find(查找), Get(获取), Install(安装), Uninstall(卸载),Update(更新)
+ Save(保存), Set(设置), Test(检查),
+ New(新建), Publish(发布), Register(注册), Unregister(注销),

共12大类的功能管理,  你几乎可以用PowerShellGet来管理整个PowerShell.
因为里面涵盖的内容非常多, 所以我打算抽其中部分方面来介绍下:

+ 用 `Find-Module` 查看远程仓库中, 收录的某个软件的版本号, 例如 PowerShellGet 本身.
PowerShellGet的数据源是PSGallery在线站点,
所以它会连接PSGallery在线站点去获取我们的数据包, 下面列出了目前站点上收录的可用版本号.

```powershell
Find-Module -Name PowerShellGet -AllVersions
```

+ 用 `Get-Module` 查看下本地软件的版本号. PowerShellGet版本号确认下吧.

```powershell
Get-Module -Name PowerShellGet
```

+ 用 `Update-Module` 命令升级本地软件包, 将`x.x.x.x` 替换成你需要的版本, 也可以省略`-RequiredVersion x.x.x.x`.

```powershell
Update-Module -Name PowerShellGet -RequiredVersion x.x.x.x
```

注意到, 升级`PowerShellGet` 可能会报错:

```log
Update-Module: Module 'PowerShellGet' was not installed by using Install-Module, so it cannot be updated.
```

这段错误提示告诉我, 已存在的 `PowerShellGet模块` 不是通过 `Install-Module` 安装的, 所以不能被升级.
比如说, 它是在安装 `PowerShell` 时的自带模块.
也即只有通过 `Install-Module` 方式来安装的模块才可以配合 `Update-Module` 更新.
为验证, 使用 `Get-InstalledModule` 命令查看已安装模块:

```powershell
Get-InstalledModule

Version              Name                                Repository           Description
-------              ----                                ----------           -----------
2.2.0-beta4          PSReadLine                          PSGallery            Great command line editing in the PowerShell console host
...
```

既然知道原因了那就简单了, 可以用 `Install-Module` 安装另一个版本, 两个版本可以共存:

```powershell
Install-Module -Name PowerShellGet -RequiredVersion 2.2.5

Untrusted repository
You are installing the modules from an untrusted repository. If you trust this repository, change its
...
```

它可能提醒你, 正安装的模块包来自于一个未被信任的仓库来源. 我们可以输入 `Y` 参数来继续执行,
因为我们知道[狗微软的 Gallery ](https://www.powershellgallery.com/)是安全的.
我们使用 `Set-PSRepository` 将 `PSGallery` 设置为可信任源, 这样就不必每次都手动确定.
首先使用 `Get-PSRepository` 查看已有的仓库,
然后使用下面的命令, 将 `PSGallery` 设置为信任的仓库, 其他仓库同理.

```powershell
Set-PSRepository -Name PSGallery -InstallationPolicy Trusted
Get-PSRepository
```

再此使用 `Get-PSRepository`, 将会看到 `InstallationPolicy` 确实已设置为 `Trusted`.

+ 对于已安装的模块, 可以使用 `Update-Module` 来升级到指定的最新版本:

```powershell
Update-Module -Name PowerShellGet -RequiredVersion x.x.x.x
```

+ 使用下面的命令列出安装的所有版本:

```powershell
Get-Module -ListAvailable -Name PowerShellGet
```

可能看到好几个版本. 新版本的 PowerShell 考虑到兼容性, 支持了安装同一模块的多个版本.

+ 卸载模块使用 `Uninstall-Module` 如下, 可以卸载特定的版本

```powershell
Uninstall-Module -Name PSReadLine -RequiredVersion 2.1.0
```
