# pwsh update module

[update-module](https://learn.microsoft.com/en-us/powershell/module/powershellget/update-module)

## 参考

模块: [PowerShellGet](https://learn.microsoft.com/en-us/powershell/module/powershellget/?view=powershellget-3.x)
将指定模块的最新版本从 在线gallery 下载并安装到本地计算机.

## 语法

```PowerShell
Update-Module
      [[-Name] <String[]>]
      [-RequiredVersion <String>]
      [-MaximumVersion <String>]
      [-Credential <PSCredential>]
      [-Scope <String>]
      [-Proxy <Uri>]
      [-ProxyCredential <PSCredential>]
      [-Force]
      [-AllowPrerelease]
      [-AcceptLicense]
      [-PassThru]
      [-WhatIf]
      [-Confirm]
      [<CommonParameters>]
```

## 描述

Update-Module cmdlet 从 online gallery 中安装模块的最新版本.
安装前会提示您确认更新.
更新仅适用于使用 `Install-Module` 安装在本地计算机上的模块.
Update-Module 会搜索 `$env:PSModulePath` 以查找已安装的模块.

它是 `Microsoft.PowerShell.PSResourceGet` 中 `Update-PSResource` cmdlet 的代理 cmdlet.
有关详细信息, 请参阅 Update-PSResource.

## 示例

示例 1: 更新所有模块
此示例将所有已安装的模块更新为 online gallery 中的最新版本.

```PowerShell
Update-Module
```

### 示例 2: 按名称更新模块

此示例将在线图库中的特定模块更新为最新版本.

```电源
Update-Module -Name SpeculationControl
```

### 示例 3: 查看 Update-Module 运行的假设情况

此示例通过假设情景来显示 Update-Module 运行后的情况. 命令未运行.

```PowerShell
Update-Module -WhatIf

What if: Performing the operation "Update-Module" on target "Version '2.8.0' of module
  'Carbon', updating to version '2.8.1'".
What if: Performing the operation "Update-Module" on target "Version '1.0.10' of module
  'SpeculationControl', updating to version '1.0.14'".
```

Update-Module 使用 WhatIf 参数显示运行 Update-Module 时会发生的情况.

### 例 4: 将模块更新到指定版本

在本例中, 一个模块被更新为一个指定版本.
该版本必须存在于 online gallery 中, 否则会显示错误.

```PowerShell
Update-Module -Name SpeculationControl -RequiredVersion 1.0.14
```

RequiredVersion 参数指定了版本 1.0.14.

### 例 5: 无需确认即可更新模块

本例无需确认即可从在线图库将模块更新到最新版本. 如果模块已安装, Force 参数会重新安装模块.

```PowerShell
Update-Module -Name SpeculationControl -Force
```

Force 参数无需请求用户确认即可更新模块.
