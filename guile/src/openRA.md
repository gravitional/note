# 编译 openRA

[OpenRA/INSTALL.md](https://github.com/OpenRA/OpenRA/blob/bleed/INSTALL.md)

## Windows

+ [Windows PowerShell >= 4.0](https://microsoft.com/powershell)(默认包含在最近的 Windows 10 版本中)
+ [.NET 6 SDK](https://dotnet.microsoft.com/download/dotnet/6.0)(或通过 Visual Studio)
要编译 OpenRA, 请打开主文件夹中的 `OpenRA.sln` 解决方案,
使用 `dotnet` 命令行构建它, 或使用 `Makefile` 类似命令 `make`,
所有脚本都使用 PowerShell 语法.

使用 `launch-game.cmd` 运行游戏.
例如, 运行 `launch-game.cmd Game.Mod=ra` 可启动 红色警戒(Red Alert),
运行 `launch-game.cmd Game.Mod=cnc` 可启动 泰伯利亚黎明(Tiberian dawn),
或运行 `launch-game.cmd Game.Mod=d2k` 可启动 沙丘 2000(Dune 2000).

## 安装 openRA 的 mod, ra2

+ 从 [ra2/archive/master](https://github.com/OpenRA/ra2/archive/master.zip.) 下载最新版本的 RA2 repository.
+ 解压缩并在 Windows 系统上运行 `make all`(命令行), 在 `Unix` 系统上运行 `make`.

```powershell
./make.cmd all # 为了跟系统的 make 区分开, 建议使用 ./make.cmd
```

+ 在 Windows 系统上使用 `launch-game.cmd`, 在 Unix 系统上使用 `launch-game.sh`, 即可运行 红色警戒 2. 