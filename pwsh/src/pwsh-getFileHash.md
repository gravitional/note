# Get-FileHash

[Get-FileHash](https://learn.microsoft.com/zh-cn/powershell/module/microsoft.powershell.utility/get-filehash?view=powershell-7.2)

模块: Microsoft.PowerShell.Utility
通过使用指定的哈希算法, 计算文件的哈希值.

```PowerShell
Get-FileHash
  [-Path] <String[]>
  [[-Algorithm] <String>]
  [<CommonParameters>]

Get-FileHash
  [-LiteralPath] <String[]>
  [[-Algorithm] <String>]
  [<CommonParameters>]

Get-FileHash
  [-InputStream] <Stream>
  [[-Algorithm] <String>]
  [<CommonParameters>]
```

## 说明

cmdlet `Get-FileHash` 使用指定的哈希算法计算文件的哈希值. 对于特定文件, 哈希值是唯一的.
哈希取决于文件内容, 而不是其文件名, 扩展名或其他指定标识文件的内容.
可以更改文件名和扩展名, 而无需更改文件的内容, 而且无需更改哈希值.
同样, 无需更改名称或扩展名即可更改文件的内容.
但是, 即使更改文件内容中的单个字符也会更改该文件的哈希值.

哈希值的用途是提供加密型安全的方式, 以验证尚未更改文件的内容.
虽然某些哈希算法(包括 MD5 和 SHA1)不再被视为安全免受攻击,
但安全哈希算法的目标是使无法更改文件的内容
(无论是意外还是恶意或未经授权的尝试), 并维护相同的哈希值.
你还可以使用哈希值来确定两个不同的文件是否具有完全相同的内容.
如果两个文件的哈希值相同, 则文件的内容也相同.

默认情况下, `Get-FileHash` cmdlet 使用 SHA256 算法, 但可以使用目标操作系统支持的任何哈希算法.

## 示例

### 示例 1: 计算文件的哈希值

此示例使用 Get-FileHash cmdlet 计算文件的哈希值 /etc/apt/sources.list.
使用的哈希算法是默认的 SHA256.
输出通过管道传递给 Format-List cmdlet, 以将输出格式设置为列表.

```PowerShell
Get-FileHash /etc/apt/sources.list | Format-List
```

### 示例 2: 计算 ISO 文件的哈希值

此示例使用 Get-FileHash cmdlet 和 SHA384
算法计算管理员从 Internet 下载的 ISO 文件的哈希值.
输出通过管道传递给 Format-List cmdlet, 以将输出格式设置为列表.

```PowerShell
Get-FileHash C:\Users\user1\Downloads\Contoso8_1_ENT.iso -Algorithm SHA384 | Format-List
```

### 示例 3: 计算流的哈希值

对于此示例, 我们将使用 System.Net.WebClient 从 Powershell 发布页下载包.
发布页还记录每个包文件的 SHA256 哈希.  我们可以将已发布的哈希值与使用 计算的 Get-FileHash哈希值进行比较.

```PowerShell
$wc = [System.Net.WebClient]::new()
$pkgurl = 'https://github.com/PowerShell/PowerShell/releases/download/v6.2.4/powershell_6.2.4-1.debian.9_amd64.deb'
$publishedHash = '8E28E54D601F0751922DE24632C1E716B4684876255CF82304A9B19E89A9CCAC'
$FileHash = Get-FileHash -InputStream ($wc.OpenRead($pkgurl))
$FileHash.Hash -eq $publishedHash

True
```

### 示例 4: 计算字符串的哈希

PowerShell 不提供 cmdlet 来计算字符串的哈希.
但是, 可以将字符串写入流, 并使用 的 Get-FileHashInputStream 参数获取哈希值.

```PowerShell
$stringAsStream = [System.IO.MemoryStream]::new()
$writer = [System.IO.StreamWriter]::new($stringAsStream)
$writer.write("Hello world")
$writer.Flush()
$stringAsStream.Position = 0
Get-FileHash -InputStream $stringAsStream | Select-Object Hash

Hash
----
64EC88CA00B268E5BA1A35678A1B5316D212F4F366B2477232534A8AECA37F3C
```
