# windows 修改系统镜像

[How to Add or Remove Optional Features on Windows Install Media](https://www.tenforums.com/tutorials/165836-how-add-remove-optional-features-windows-install-media.html)

## 挂载(Mount)挂载 Windows 映像用于离线服务

1. 插入Windows 安装U盘.
或者, 将 Windows ISO 映像加载为虚拟 DVD 驱动器(右键单击 ISO 文件, 选择加载),
然后将其所有文件和文件夹复制到硬盘上的文件夹中.
在本教程中, 我将以文件夹 `D:\ISO_Files` 为例.

2. 打开 elevated PowerShell(以管理员身份运行). 输入以下命令来检查安装介质中的 Windows 版本:

```powershell
Get-WindowsImage -ImagePath D:\ISO_Files\Sources\install.wim | Format-Table ImageIndex, ImageName
```

检查要编辑的版本(离线服务)的 image index(序号).
请注意, 这些功能只针对您选择的版本启用或禁用, 安装其他版本不会受到影响.

如果要从 U盘 而不是包含 ISO文件夹 挂载 `install.wim`,
请修改为 `-ImagePath X:\Sources\install.wim`, 其中 `X:` 是 U盘符.

>如果你的 ISO 文件或 USB介质是 双架构的(dual architecture),
>包含 32 位和 64 位 Windows 安装文件, 请相应更改路径.
>请使用 `\x32\Sources\install.wim` 或 `\x64\Sources\install.wim` 代替 `\Sources\install.wim`.

3. 创建 挂载点(mount point)文件夹. 在本教程中, 我将使用文件夹 `C:\Mount`.

4. 选中某个 windows 版本, 然后挂载, 其中 `-index X` 中的 `X` 是所选版本的映像索引号:

```powershell
Mount-WindowsImage -ImagePath D:\ISO_Files\Sources\install.wim -Index X -Path C:\Mount
```

根据实际情况更改 `-ImagePath`(`install.wim` 文件路径)和 `-Path`(挂载点文件夹).

## 添加可选功能

1. 使用以下命令在 `Grid View` 中打开 Windows 镜像中 默认已禁用的可选功能列表:

```powershell
Get-WindowsOptionalFeature -Path C:\Mount | Where-Object {$_.State –eq "Disabled"} | Out-GridView -PassThru | Enable-WindowsOptionalFeature
```

+ 根据实际挂载点更改 `-Path C:\Mount` 中实际 `挂载点文件夹` 的路径.
+ 如有需要, 请展开网格视图表中的 `FeatureName` 列:

2. 按住CTRL键多选, 选择要添加到 Windows镜像的可选功能, 然后单击确定.
在本例中, 我选择了 `Virtual Machine Platform` 和 `Windows Subsystem for Linux` 功能:
如何在 Windows 安装媒体上添加或删除可选功能-select-features.jpg

3. 选定的 Windows 可选功能将添加到 offline image 中.

## 删除可选功能

删除可选功能 与 添加可选功能是类似的.

1. 使用以下命令在 `Grid view` 中打开 Windows镜像 中 `已启用的可选功能` 列表:

```bash
Get-WindowsOptionalFeature -Path C:\Mount | Where-Object {$_.State -eq "Enabled"} | Out-GridView -PassThru | Disable-WindowsOptionalFeature
```

2. 按住 CTRL 键, 选择要从 Windows 映像中删除的可选功能, 然后单击 `确定`.

## 保存对 Windows镜像 的更改

1. 输入以下命令保存对 Windows 安装介质的更改:

```bash
Dismount-WindowsImage -Path C:\Mount -Save
```

根据实际情况更改挂载点的路径.

2. 所选 Windows 版本中的可选 Windows 功能已按您的选择添加或删除.
从安装介质安装所选版本的 Windows 10 时, 默认情况下将启用添加的所有选定功能.

如果你是在 `U盘` 中添加或删除了 `可选功能`, 那么它现在已经准备就绪.
如果你是将ISO文件复制到 某文件夹 `dir` 中进行操作, 则需要再次从 `dir` 创建新的ISO.
您可以使用自己喜欢的 ISO 创建方法.
有关创建新 ISO 的一种方法,请参阅[5.3至5.5: 从现有安装创建Windows 10 ISO 映像](https://www.tenforums.com/tutorials/72031-create-windows-10-iso-image-existing-installation.html)
