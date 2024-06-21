# windows 命令行 cmd

[Windows控制台cmd默认代码页修改的办法[GBK, UTF-8]](https://blog.csdn.net/tanmx219/article/details/123723771)

## cmd 常用命令

```cmd
cd c:Users/xxx # 切换目录
cd # 显示当前目录
echo %cd% #显示当前目录
dir #相当于 ls, 显示文件和目录
```

## 修改CMD窗口编码

### 直接输入指令

```cmd
chcp 65001
```

常用的编码及对应的码值(10进制):
十进制码值    对应编码名称

+ 950    繁体中文
+ 65001    UTF-8代码页
+ 936    简体中文默认的GBK
+ 437    MS-DOS 美国英语

详情请参考这里,
[chcp|Microsoft Docs](https://docs.microsoft.com/zh-cn/windows-server/administration/windows-commands/chcp)

### 那么, 怎么才能永久性修改这个呢

有人想了些千奇百怪的办法,
[Change CodePage in CMD permanently?-Stack Overflow](https://stackoverflow.com/questions/7432545/change-codepage-in-cmd-permanently)
我认为没有一个靠谱的, 修改注册表可能会好一点(启动时也会引起其他问题),
因为只影响到cmd控制台, 例如,

+ Start -> Run -> regedit
+ Go to `[HKEY_LOCAL_MACHINE\Software\Microsoft\Command Processor]`
+ Add new String Value named: `Autorun`
+ Change the value to `chcp 437`

不推荐采用 `系统语言页面` 的修改办法, 这会影响到整个系统和你的大量程序.

### 我的办法

感觉上面的办法都有不靠谱的地方, 所以我自己写了启动脚本, 命名为 `D:\cmd.bat`, 内容如下(因为我常用ROS2, 这里加上)

```bat
chcp  65001
call  C:\dev\ros2_galactic\local_setup.bat
```

每次启动cmd窗口, 我只需要在D盘符下输入cmd, 或者直接输入 `d:/cmd.bat` 就可以了, 如下

"E:\IBECAE\\"%2"\bin\\"%3"\solver.exe" %1 -threads=1 %5 %6 %7 %8 %9
pause

## powershell 禁用 `关闭设备以节约电源`, 禁止关闭 wifi

[Disable turn off this device to save power for NIC](https://stackoverflow.com/questions/46145449/disable-turn-off-this-device-to-save-power-for-nic)

`win+x` 打开 管理员模式下的 powershell
设置所有的网络适配器 关闭 省电模式

```powershell
 $adapters = Get-NetAdapter -Physical | Get-NetAdapterPowerManagement
    foreach ($adapter in $adapters)
        {
        $adapter.AllowComputerToTurnOffDevice = 'Disabled'
        $adapter | Set-NetAdapterPowerManagement
        }
```

或者只设置 第一个, 使用 `select -Index 0`

```powershell
$adapter = Get-NetAdapter -Physical | select -Index 0 | Get-NetAdapterPowerManagement
$adapter.AllowComputerToTurnOffDevice = 'Disabled'
$adapter | Set-NetAdapterPowerManagement
```
