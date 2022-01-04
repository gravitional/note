# Windows SubLinux2

[ Win10 Terminal + WSL 2 安装配置指南](https://www.cnblogs.com/willick/p/13924325.html)
[设置从Windows Terminal打开wsl时进入Linux用户主目录](https://blog.csdn.net/baidu_33340703/article/details/106949948)
[适用于 Linux 的 Windows 子系统安装指南 (Windows 10)](https://docs.microsoft.com/zh-cn/windows/wsl/install-win10)
[如何在windows的资源管理器下访问WSL2?](https://www.zhihu.com/question/398307967)

`wsl`目录: 在资源管理器输入:`\\wsl$`

安装适用于 `Linux` 的 `Windows` 子系统 (`WSL`) 时有两个选项:

***
简化安装 (预览版) : `wsl --install`. 要使用 `wsl --install` 简化安装命令,必须先完成以下操作:

+ 加入 Windows 预览体验计划
+ 安装 Windows 10 的预览版(OS 版本 20262 或更高版本).
+ 使用管理员特权打开命令行窗口

满足这些要求后,可通过以下方式安装 `WSL`:

在管理员模式下打开命令行,并输入以下命令: `wsl.exe --install`
重启计算机

首次启动新安装的 `Linux` 分发版时,将打开一个控制台窗口,要求你等待将文件解压缩并存储到电脑上. 未来的所有启动时间应不到一秒.
然后,需要为新的 `Linux` 分发版创建用户帐户和密码.

祝贺你!现已成功安装并设置了与 Windows 操作系统完全集成的 `Linux` 分发!

`--install` 命令执行以下操作:

+ 启用可选的 WSL 和虚拟机平台组件
+ 下载并安装最新 Linux 内核
+ 将 WSL 2 设置为默认值
+ 下载并安装 Linux 分发版(可能需要重启)

默认情况下,安装的 `Linux` 分发版为 `Ubuntu`. 可以使用 `wsl --install -d <Distribution Name>` 进行更改.
(将 `<Distribution Name>` 替换为所需分发版的名称.)初始安装后,可以使用 `wsl --install -d <Distribution Name>` 命令将其他 Linux 分发版添加到计算机.

若要查看可用 `Linux` 分发版的列表,请输入 `wsl --list --online`.

***
手动安装步骤

1. 先启用`适用于 Linux 的 Windows 子系统`可选功能,然后才能在 `Windows` 上安装 `Linux` 分发.

以管理员身份打开 PowerShell 并运行:

```powershell
dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
```

建议现在转到步骤 #2,更新到 WSL 2,但如果只想安装 WSL 1,现在可以重新启动计算机,然后继续执行步骤 6 - 安装所选的 Linux 发行版.
若要更新到 WSL 2,请等待重新启动计算机,然后继续执行下一步.

2. 更新到 WSL 2

对于 x64 系统: 版本 1903 或更高版本,采用 内部版本 18362 或更高版本.
对于 ARM64 系统: 版本 2004 或更高版本,采用 内部版本 19041 或更高版本.
低于 18362 的版本不支持 WSL 2. 使用 Windows Update 助手更新 Windows 版本.

若要检查 Windows 版本及内部版本号,选择 `Windows 徽标键 + R`,然后键入`winver`,选择`确定`. (或者在 `Windows` 命令提示符下输入 `ver` 命令).
更新到"设置"菜单中的最新 Windows 版本.

3. 启用虚拟机功能

安装 WSL 2 之前,必须启用`虚拟机平台`可选功能.

以管理员身份打开 PowerShell 并运行:

```powershell
dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart
```

重新启动 计算机,以完成 `WSL` 安装并更新到 `WSL 2`.

4. 下载 Linux 内核更新包

下载最新包:
[适用于 x64 计算机的 WSL2 Linux 内核更新包](https://wslstorestorage.blob.core.windows.net/wslblob/wsl_update_x64.msi)

备注
如果使用的是 ARM64 计算机,请下载 ARM64 包. 如果不确定自己计算机的类型,请打开命令提示符或 PowerShell,并输入: `systeminfo | find "System Type"`.
运行上一步中下载的更新包. (双击以运行 - 系统将提示你提供提升的权限,选择`是`以批准此安装.)
安装完成后,请继续执行下一步 - 在安装新的 Linux 分发时,将 WSL 2 设置为默认版本. (如果希望将新的 Linux 安装设置为 WSL 1,请跳过此步骤.)

5. 将 WSL 2 设置为默认版本

打开 PowerShell,然后在安装新的 Linux 发行版时运行以下命令,将 `WSL 2` 设置为默认版本:

```powershell
wsl --set-default-version 2
```

备注

从 WSL 1 更新到 WSL 2 可能需要几分钟才能完成,具体取决于目标分发版的大小.
 如果从 Windows 10 周年更新或创意者更新运行 WSL 1 的旧(历史)安装,可能会遇到更新错误. 按照这些说明卸载并删除任何旧分发.

如果 `wsl --set-default-version` 结果为无效命令,请输入 `wsl --help`.
如果 `--set-default-version` 未列出,则表示你的 OS 不支持它,你需要更新到版本 `1903`(内部版本 18362)或更高版本.

运行命令后如果看到此消息: `WSL 2 requires an update to its kernel component. For information please visit https://aka.ms/wsl2kernel`.
仍需要安装 MSI Linux 内核更新包.

6. 安装所选的 `Linux` 分发

打开 `Microsoft Store`,并选择你偏好的 `Linux` 分发版.在分发版的页面中,选择"获取".
首次启动新安装的 `Linux` 分发版时,将打开一个控制台窗口,系统会要求你等待一分钟或两分钟,以便文件解压缩并存储到电脑上. 未来的所有启动时间应不到一秒.
然后,需要为新的 `Linux` 分发版创建用户帐户和密码.

7. 安装 Windows 终端(可选)

Windows 终端可启用多个选项卡(在多个 `Linux` 命令行, Windows 命令提示符, PowerShell 和 Azure CLI 等之间快速切换),
创建键绑定(用于打开或关闭选项卡, 复制粘贴等的快捷方式键), 使用搜索功能,以及使用自定义主题(配色方案, 字体样式和大小, 背景图像/模糊/透明度).

8. 将分发版版本设置为 WSL 1 或 WSL 2

可打开 `PowerShell` 命令行并输入以下命令(仅在 Windows 内部版本 18362 或更高版本中可用),检查分配给每个已安装的 `Linux` 分发版的 WSL 版本:

```powershell
wsl --list --verbose
```

若要将分发版设置为受某一 WSL 版本支持,请运行:

```powershell
wsl --set-version <distribution name> <versionNumber>
wsl --set-version Ubuntu20.04 2 # 把 ubuntu 20.04 设置为 wsl2
```

请确保将 `<distribution name>` 替换为你的分发版的实际名称,并将 `<versionNumber>` 替换为数字`1`或`2`.
 可以随时更改回 WSL 1,方法是运行与上面相同的命令,但将`2`替换为`1`.

此外,如果要使 WSL 2 成为你的默认体系结构,可以通过此命令执行该操作:

```powershell
wsl --set-default-version 2
```

这会将安装的任何新分发版的版本设置为 `WSL 2`.

## WSL2 参考的对象类型不支持尝试的操作

使用 `VPN` 造成的 WSL2 启动 crash

[failing to startup with code 4294967295](https://github.com/microsoft/WSL/issues/5092#:~:text=Solve%20%22process%20exited%20with%20code%204294967295%22%20%2C%20run,complete%20the%20reset.%20Does%20not%20resolve%20the%20issue.)
[关于使用WSL2出现'参考的对象类型不支持尝试的操作'的解决方法](https://zhuanlan.zhihu.com/p/151392411)

长期解决的方案(推荐),下载此软件: [http://www.proxifier.com/tmp/Test20200228/NoLsp.exe](http://www.proxifier.com/tmp/Test20200228/NoLsp.exe)

因需要梯子访问下载,有些朋友不方便,所以我上传到百度云分享在这里: [https://pan.baidu.com/s/1bVZ0OXZPxEt8l1IHYaFK3A](https://pan.baidu.com/s/1bVZ0OXZPxEt8l1IHYaFK3A) ,提取码: `vjge`

然后在管理员身份运行`CMD`输入:

```pwsh
NoLsp.exe C:\windows\system32\wsl.exe
```

请自行注意`NoLsp.exe`程序的位置,以及你的`wsl.exe`位置.

***
产生原因和解决方法分析:

代理软件和`wsl2`的`sock`端口冲突,使用`netsh winsock reset`重置修复. `Proxifer` 开发人员解释如下:

如果`Winsock LSP DLL`被加载到其进程中,则`wsl.exe`将显示此错误.
最简单的解决方案是对`wsl.exe`使用`WSCSetApplicationCategory WinAPI`调用来防止这种情况.
在后台,该调用在`HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\WinSock2\Parameters\AppId_Catalog`中为`wsl.exe`创建一个条目.
这将告诉`Windows`不要将`LSP DLL`加载到`wsl.exe`进程中.
