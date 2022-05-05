# windows ssh

[Windows 中的 OpenSSH](https://docs.microsoft.com/zh-cn/windows-server/administration/openssh/openssh_overview)

## 安装配置

### 安装 OpenSSH

[安装OpenSSH](https://docs.microsoft.com/zh-cn/windows-server/administration/openssh/openssh_install_firstuse)

使用 PowerShell 安装 OpenSSH: 请先以管理员身份运行 PowerShell.
为了确保 OpenSSH 可用, 请运行以下 cmdlet:

```PowerShell
Get-WindowsCapability -Online | Where-Object Name -like 'OpenSSH*'
```

如果两者均尚未安装, 则此操作应返回以下输出:

```PowerShell
Name  : OpenSSH.Client~~~~0.0.1.0
State : NotPresent

Name  : OpenSSH.Server~~~~0.0.1.0
State : NotPresent
```

然后, 根据需要安装服务器或客户端组件:

```PowerShell
# Install the OpenSSH Client 客户端程序
Add-WindowsCapability -Online -Name OpenSSH.Client~~~~0.0.1.0

# Install the OpenSSH Server 服务器程序
Add-WindowsCapability -Online -Name OpenSSH.Server~~~~0.0.1.0
```

这两者应该都会返回以下输出:

```PowerShell
Path          :
Online        : True
RestartNeeded : False
```

### 启动并配置 OpenSSH 服务器

若要启动并配置 `OpenSSH` 服务器来开启使用,
请以管理员身份打开 `PowerShell`, 然后运行以下命令来启动 `sshd` service:

```PowerShell
# Start the sshd service
Start-Service sshd

# OPTIONAL but recommended:
Set-Service -Name sshd -StartupType 'Automatic'

# Confirm the Firewall rule is configured. It should be created automatically by setup. Run the following to verify
if (!(Get-NetFirewallRule -Name "OpenSSH-Server-In-TCP" -ErrorAction SilentlyContinue | Select-Object Name, Enabled)) {
    Write-Output "Firewall Rule 'OpenSSH-Server-In-TCP' does not exist, creating it..."
    New-NetFirewallRule -Name 'OpenSSH-Server-In-TCP' -DisplayName 'OpenSSH Server (sshd)' -Enabled True -Direction Inbound -Protocol TCP -Action Allow -LocalPort 22
} else {
    Write-Output "Firewall rule 'OpenSSH-Server-In-TCP' has been created and exists."
}
```

### 连接到 OpenSSH 服务器

安装后, 可从使用安装了 OpenSSH 客户端的设备连接到 OpenSSH 服务器, 如下所示.
请务必以管理员身份运行 `PowerShell`:

```PowerShell
ssh username@servername
```

### 使用 PowerShell 卸载 OpenSSH

卸载 `OpenSSH` 组件, 请使用以下命令:

```PowerShell
# Uninstall the OpenSSH Client
Remove-WindowsCapability -Online -Name OpenSSH.Client~~~~0.0.1.0

# Uninstall the OpenSSH Server
Remove-WindowsCapability -Online -Name OpenSSH.Server~~~~0.0.1.0
```

如果在卸载时服务正在使用中, 稍后可能需要重启 `Windows`.

## 为 Windows 中的 OpenSSH 配置默认 shell

默认命令 `shell` 提供用户使用 SSH 连接到服务器时看到的体验.
初始默认 `Windows` 是 Windows Command shell (cmd.exe).
Windows 还包括了 PowerShell 和 Bash, 第三方命令 shell 也可用于 Windows, 并可配置为服务器的默认 shell.

若要设置默认 `command shell`, 请首先确认 `OpenSSH` 安装文件夹是否位于 system path 中.
对于 `Windows`, 默认安装文件夹为 `%systemdrive%\WindowsDirectory\System32\openssh`.
以下命令显示当前 path 设置, 并向其中添加默认的 `OpenSSH` 安装文件夹.

命令 shell     要使用的命令

Command  path
PowerShell  $env:path

在 `Windows` 注册表中配置默认 `ssh shell`:
通过将 `shell executable` 的 `完整路径` 添加到
`Computer\HKEY_LOCAL_MACHINE\SOFTWARE\OpenSSH` 的字符串值 `DefaultShell`.

例如, 以下命令将默认 shell 设置为 powershell.exe:

```PowerShell
New-ItemProperty -Path "HKLM:\SOFTWARE\OpenSSH" -Name DefaultShell `
-Value "C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe" -PropertyType String -Force
```

## Sshd_config 中的 Windows 配置

[在线 Win32 OpenSSH 文档]:https://github.com/powershell/win32-openssh/wiki

在 `Windows` 中, `sshd` 默认情况下从 `%programdata%\ssh\sshd_config` 中读取配置数据,
也可以通过使用 `-f` 参数启动 `sshd` 来指定不同的配置文件.
如果该文件不存在, 则在启动该服务时, `sshd` 将使用默认配置生成一个文件.

下面列出的元素, 可以通过在 `sshd_config` 列出相应条目, 来实现 Windows 特定的配置.
可以在其中实现的其他一些配置设置在此处没有列出, 因为 [在线 Win32 OpenSSH 文档][] 中详细介绍了它们.

### AllowGroups, AllowUsers, DenyGroups, DenyUsers

使用 AllowGroups, AllowUsers, DenyGroups 和 DenyUsers 指令控制哪些用户和组可以连接到服务器.
allow/deny 指令按以下顺序处理: DenyUsers, AllowUsers, DenyGroups, 最后是 AllowGroups.
必须以小写形式指定所有帐户名称.  有关通配符模式的详细信息, 请参阅 ssh_config 中的 PATTERNS.

当使用域用户或组配置基于用户/组的规则时, 请使用以下格式: `user?domain*`.
Windows 允许使用多种格式来指定域主体, 但许多格式与标准 Linux 模式冲突.
因此, 添加了 * 来涵盖 FQDN.
此外, 此方法使用了 "?"(而非 @)来避免与 username@host 格式发生冲突.

工作组用户/组和连接到 internet 的帐户始终解析为其本地帐户名称(不包括域部分, 类似于标准 Unix 名称).
域用户和组严格解析为 [NameSamCompatible][] 格式 - domain_short_name\user_name.
所有基于用户/组的配置规则都需要遵循此格式.

[NameSamCompatible]: https://docs.microsoft.com/en-us/windows/desktop/api/secext/ne-secext-extended_name_format

#### 域用户和组的示例

```ssh
DenyUsers contoso\admin@192.168.2.23 : blocks contoso\admin from 192.168.2.23
DenyUsers contoso\* : blocks all users from contoso domain
AllowGroups contoso\sshusers : only allow users from contoso\sshusers group
```

#### 本地用户和组的示例

```ssh
AllowUsers localuser@192.168.2.23
AllowGroups sshusers
```

### AuthenticationMethods

对于 Windows OpenSSH, 唯一可用的身份验证方法是"password"和"publickey".

### AuthorizedKeysFile

默认值为 `".ssh/authorized_keys .ssh/authorized_keys2"`.
如果路径不是绝对路径, 则它相对于用户的主目录(或  profile image  路径).  示例:  `c:\users\user`.
请注意, 如果用户属于管理员组, 则改为使用 `%programdata%/ssh/administrators_authorized_keys`.

### ChrootDirectory(在 v7.7.0.0 中添加的支持)

此指令仅在 sftp 会话中受支持.  到 cmd.exe 的远程会话不遵循此规则.
若要设置仅限 sftp 的 chroot 服务器, 请将 ForceCommand 设置为 internal-sftp.
还可以通过实现仅允许 scp 和 sftp 的自定义 shell, 来通过 chroot 设置 scp.

## 主机密钥生成

公钥具有特定的 `ACL` 要求, 在 `Windows` 上,
这些要求等同于仅允许管理员和 `System` 进行访问.  首次使用 `sshd` 时, 将自动生成主机的密钥对.

>重要: 首先需要安装 OpenSSH 服务器.
请参阅 [OpenSSH 入门](https://docs.microsoft.com/zh-cn/windows-server/administration/openssh/openssh_install_firstuse).

默认情况下, `sshd` 服务设置为手动启动.
若要在每次重新启动服务器时启动它, 请从服务器上的 `管理员PowerShell 提示符` 运行以下命令:

```PowerShell
# Set the sshd service to be started automatically
Get-Service -Name sshd | Set-Service -StartupType Automatic
# Now start the sshd service
Start-Service sshd
```

由于没有与 `sshd` 服务关联的用户, 因此主机密钥存储在 `C:\ProgramData\ssh` 下.

### 用户密钥生成

若要使用基于 `密钥` 的身份验证, 首先需要为客户端生成 `公钥/私钥对`.
`ssh-keygen.exe` 用于生成密钥文件, 并且可以指定算法 `DSA`, `RSA`, `ECDSA` 或 `Ed25519`.
如果未指定算法, 则使用 `RSA`.
应使用强算法和密钥长度, 例如此示例中的 `Ed25519`.

若要使用 Ed25519 算法生成密钥文件, 请从客户端上的 PowerShell 或 cmd 提示符运行以下命令:

```PowerShell
ssh-keygen -t ed25519
```

这应当会显示以下内容(其中, `username` 将替代为你的用户名):

```PowerShell
Generating public/private ed25519 key pair.
Enter file in which to save the key (C:\Users\username\.ssh\id_ed25519):
```

你可以按 `Enter` 来接受默认值, 或指定要在其中生成密钥的路径和/或文件名.
此时, 系统会提示你使用密码来加密你的私钥文件.
这可为空, 但不建议这样做.
将密码与密钥文件一起使用来提供双因素身份验证.  在此示例中, 我们将密码留空.

```log
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in C:\Users\username\.ssh\id_ed25519.
Your public key has been saved in C:\Users\username\.ssh\id_ed25519.pub.
The key fingerprint is:
SHA256:OIzc1yE7joL2Bzy8!gS0j8eGK7bYaH1FmF3sDuMeSj8 username@server@LOCAL-HOSTNAME
...
```

现在, 指定位置已有 `公共/专用 Ed25519 密钥对`.
`.pub` 文件是公钥, 没有扩展名的文件是 `私钥`:

```PowerShell
Mode                LastWriteTime         Length Name
----                -------------         ------ ----
-a----         6/3/2021   2:55 PM            464 ed25519
-a----         6/3/2021   2:55 PM            103 ed25519.pub
```

请记住, 私钥文件等效于密码, 应当采用与保护密码相同的方式来保护它.
为了实现此目的, 请使用 `ssh-agent` 来将私钥安全地存储在与你的 `Windows` 登录关联的 `Windows` 安全上下文中.
为执行该操作, 请以管理员身份启动 ssh-agent 服务并使用 ssh-add 来存储私钥.

```PowerShell
# By default the ssh-agent service is disabled. Allow it to be manually started for the next step to work.
# Make sure you're running as an Administrator.
Get-Service ssh-agent | Set-Service -StartupType Manual
# Start the service
Start-Service ssh-agent
# This should return a status of Running
Get-Service ssh-agent
# Now load your key files into ssh-agent
ssh-add ~\.ssh\id_ed25519
```

完成这些步骤后, 每当从此客户端进行身份验证需要使用私钥时,
`ssh-agent` 都会自动检索本地私钥, 并将其传递到你的 `SSH` 客户端.

>重要

强烈建议你将 `私钥` 备份到一个安全位置, 将其添加到 `ssh-agent`, 然后将其从本地系统中删除.
如果使用了强算法(例如此示例中的 Ed25519), 则无法从代理中检索私钥.
如果你失去了对私钥的访问权限, 则必须在你与之交互的所有系统上创建新的密钥对并更新公钥.

### 部署公钥

若要使用上面创建的用户密钥,
需要将公钥 (`~\.ssh\id_ed25519.pub`) 的内容放置在服务器上的一个文本文件中,
其名称和位置取决于用户帐户是 `本地管理员` 组的成员还是 `标准用户帐户`.

### 标准用户

公钥 (`~\.ssh\id_ed25519.pub`) 的内容需放置在服务器上的名为 `authorized_keys` 的文本文件中,
该文件位于 `C:\Users\username\.ssh\`.
`OpenSSH` 客户端包括了 `scp` 来帮助实现此目的, 它是安全的文件传输实用工具.

以下示例将公钥复制到服务器(其中"username"替换为你的用户名).
对于首次登陆服务器, 需要使用 `用户帐户` 的 `密码`.

```PowerShell
# Make sure that the .ssh directory exists in your server's user account home folder
ssh username@domain1@contoso.com mkdir C:\Users\username\.ssh\

# Use scp to copy the public key file generated previously on your client to the authorized_keys file on your server
scp C:\Users\username\.ssh\id_ed25519.pub user1@domain1@contoso.com:C:\Users\username\.ssh\authorized_keys
```

### 管理用户

公钥 (`~\.ssh\id_ed25519.pub`) 的内容, 需放置在服务器上的名为 `administrators_authorized_keys` 的文本文件中,
该文件位于 `C:\ProgramData\ssh\`.
`OpenSSH` 客户端包括了 `scp` 来帮助实现此目的, 它是安全的文件传输实用工具.
此文件上的 `ACL` 需要配置为仅允许访问管理员和系统.

以下示例将 `公钥` 复制到 `服务器` 并配置 `ACL`(其中"username"替换为你的用户名).
对于首次登陆服务器, 需要使用 `用户帐户` 的 `密码`.

>备注
>
>此示例演示了创建 `administrators_authorized_keys file` 的步骤.
>如果多次运行, 则每次都会覆盖此文件.
>若要为多个管理用户添加公钥, 需将每个公钥 追加 到此文件中.

```PowerShell
# Make sure that the .ssh directory exists in your server's user account home folder
ssh user1@domain1@contoso.com mkdir C:\ProgramData\ssh\
# Use scp to copy the public key file generated previously on your client to the authorized_keys file on your server
scp C:\Users\username\.ssh\id_ed25519.pub user1@domain1@contoso.com:C:\ProgramData\ssh\administrators_authorized_keys
# Appropriately ACL the authorized_keys file on your server
ssh --% user1@domain1@contoso.com icacls.exe "C:\ProgramData\ssh\administrators_authorized_keys" /inheritance:r /grant "Administrators:F" /grant "SYSTEM:F"
```

这些步骤完成了对 `Windows` 上的 `OpenSSH` 使用基于密钥的身份验证所需的配置.
完成此项后, 用户可以从 `具有私钥` 的任何客户端连接到 `sshd` 主机.
