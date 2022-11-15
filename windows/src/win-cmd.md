# windows 命令行 cmd

[Windows控制台cmd默认代码页修改的办法[GBK, UTF-8]](https://blog.csdn.net/tanmx219/article/details/123723771)

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
