# windows dism 工具

https://www.tenforums.com/tutorials/165836-how-add-remove-optional-features-windows-install-media.html
https://learn.microsoft.com/en-us/windows-hardware/manufacture/desktop/dism-image-management-command-line-options-s14
https://learn.microsoft.com/en-us/windows-hardware/manufacture/desktop/features-on-demand-v2--capabilities
https://learn.microsoft.com/en-us/windows-hardware/manufacture/desktop/create-a-data-image-using-dism

dism 工具需要在 管理员powershell 中运行.

`/Online` 表示 Targets the running operating system, 也就是作用到当前的操作系统上.

+ 各种路径的变量

```powershell
$iso_path='C:\Users\qingz\Downloads\win-iso' # windows iso 解压后的目录
$wim_path='C:\Users\qingz\Downloads\win-0813.wim' # wim 文件路径
$wim_mount='C:\mount' # 挂载点目录, 尽量使用 C:\mount 这样的根目录
```

+ 制作 wim

```powershell
Dism /Capture-Image /ImageFile:C:\Users\qingz\Downloads\win-0813.wim /CaptureDir:C:\Users\qingz\Downloads\win-iso
```

+ 获取 wim 中的 windows 版本信息 和 index, 后续操作需要 index

```pwershell
Dism /Get-ImageInfo /ImageFile:C:\Users\qingz\Downloads\win-0813.wim
```

+ sdf

```powershell
dism /mount-image /index:1 /imagefile:C:\Users\qingz\Downloads\win-0813.wim /mountdir:C:\mount
Dism /Remount-Image /mountdir:C:\mount
```

+ 查看需要的 可选功能信息

```powershell
Dism /Online /Get-CapabilityInfo /CapabilityName:Tools.Graphics.DirectX
```

得到 确切版本
>功能标识:Tools.Graphics.DirectX~~~~0.0.1.0

+ 添加可选功能

```powershell
# 添加到镜像文件
Dism /Image:C:\mount /Add-Capability /CapabilityName:Tools.Graphics.DirectX~~~~0.0.1.0

# 添加到 local PC
Dism /online /Add-Capability /CapabilityName:Tools.Graphics.DirectX~~~~0.0.1.0 /Source:C:\mount
```

```powershell
# 查看 mounted 的镜像
Dism /Get-MountedImageInfo
#
dism /Get-ImageInfo /ImageFile:C:\Users\qingz\Downloads\win-0813.wim

# 查看 windows系统 镜像的 功能
DISM /image:C:\mount /Get-Capabilities
# 卸载镜像
Dism /Unmount-Image /MountDir:C:\mount /Discard
```

## 离线安装图形工具

https://download.csdn.net/download/eamon100/88489075

Win10系统的可选功能图形工具, 需要在联网的情况下安装. 在离线模式下需要下载这个压缩包.
适用于windows_10_business_editions_version_22h2版本, 其他版本没有测试过.
离线安装命令:
Dism /Online /Add-Capability /CapabilityName:Tools.Graphics.DirectX~~~~0.0.1.0 /LimitAccess /Source:C:\XpsMount
在线安装命令:
Dism /Online /Add-Capability /CapabilityName:Tools.Graphics.DirectX~~~~0.0.1.0
删除命令:
Dism /Online /Remove-Capability /CapabilityName:Tools.Graphics.DirectX~~~~0.0.1.0