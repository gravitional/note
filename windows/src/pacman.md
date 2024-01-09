# msys2 pacman 包管理器

[pacman的包管理命令](https://blog.csdn.net/qq_41601836/article/details/106519865)
[msys2软件包管理工具pacman常用命令](https://blog.csdn.net/hustlei/article/details/86687621)

pacman是arclinux中的软件管理工具, 也是widows上msys2默认的软件管理工具.
pacman可以直接从网络上的软件仓库下载安装及删除软件, 自动处理依赖关系, 类似ubuntu中的apt-get.

## 安装软件

`pacman -S 软件名`: 安装软件.
也可以同时安装多个包, 只需以空格分隔包名即可.
`pacman -S --needed 软件名1 软件名2`: 安装软件, 但不重新安装已经是最新的软件.
`pacman -Sy 软件名`: 安装软件前, 先从远程仓库下载软件包数据库(数据库即所有软件列表).

`pacman -Sv 软件名`: 在显示一些操作信息后执行安装.
`pacman -Sw 软件名`: 只下载软件包, 不安装.
`pacman -U 软件名.pkg.tar.gz`: 安装本地软件包.
`pacman -U http://www.example.com/repo/example.pkg.tar.xz`: 
安装一个远程包(不在 pacman 配置的源里面).

## 更新系统

`pacman -Sy`: 从服务器下载新的软件包数据库(实际上就是下载远程仓库最新软件列表到本地).
`pacman -Su`: 升级所有已安装的软件包.

pacman 可以用一个命令就可以升级整个系统.
花费的时间取决于系统有多老.
这个命令会同步非本地(local)软件仓库并升级系统的软件包:

```bash
pacman -Syu
```

在msys2中 pacman -Syu后需要重启一下msys2(关掉shell重新打开即可).

在Arch linux中, 只支持系统完整升级, 不支持部分升级.
所以即使在msys2中, pacman -Syu也会升级整个系统.
可以观察一下, 即使新安装的msys2, pacman -Syu后, msys2安装目录占用空间立马变大很多.

如果升级时, 网络比较慢, 觉得既浪费时间又浪费硬盘, 实在不想升级那么多东西, 可以逐个软件包升级.
用下面命令可以升级核心包:

```bash
pacman -S --needed filesystem msys2-runtime bash libreadline libiconv libarchive libgpgme libcurl pacman ncurses libintl
```

## 卸载软件

`pacman -R 软件名`: 该命令将只删除包, 保留其全部已经安装的依赖关系
`pacman -Rv 软件名`: 删除软件, 并显示详细的信息
`pacman -Rs 软件名`: 删除软件, 同时删除本机上只有该软件依赖的软件.

`pacman -Rsc 软件名`: 删除软件, 并删除所有依赖这个软件的程序, 慎用
`pacman -Ru 软件名`: 删除软件,同时删除不再被任何软件所需要的依赖

## 搜索软件

pacman -Ss 关键字: 在仓库中搜索含关键字的软件包(本地已安装的会标记)
`pacman -Sl <repo>`:
显示软件仓库中所有软件的列表
可以省略, 通常这样用:pacman -Sl | grep 关键字

`pacman -Qs 关键字`: 搜索已安装的软件包
pacman -Qu: 列出所有可升级的软件包
pacman -Qt: 列出不被任何软件要求的软件包
参数加q可以简洁方式显示结果, 比如pacman -Ssq gcc会比pacman -Ss gcc显示的好看一些.

```bash
pacman -Sl | grep gcc # 跟pacman -Ssq gcc很接近, 但是会少一些和gcc有关但软件名不包含gcc的包.
```

查询软件信息

pacman -Q 软件名: 查看软件包是否已安装, 已安装则显示软件包名称和版本
pacman -Qi 软件名: 查看某个软件包信息, 显示较为详细的信息, 包括描述, 构架, 依赖, 大小等等
pacman -Ql 软件名: 列出软件包内所有文件, 包括软件安装的每个文件, 文件夹的名称和路径

## 软件包组

pacman -Sg: 列出软件仓库上所有的软件包组
pacman -Qg: 列出本地已经安装的软件包组和子包
pacman -Sg 软件包组: 查看某软件包组所包含的所有软件包
pacman -Qg 软件包组: 和pacman -Sg 软件包组完全一样

很多人建议通过安装软件组来安装工具链, 例如:

pacman -S mingw-w64-x86_64-toolchain
pacman -S mingw-w64-i686-toolchain
pacman -S mingw-w64-x86_64-qt5
pacman -S base-devel

但是这样比较浪费空间.

实际上如果把gcc, qt, clang等安装上, msys2就要占掉超过10G的硬盘空间, 所以个人很少直接安装软件组.

## 清理缓存

pacman -Sc: 清理未安装的包文件, 包文件位于 /var/cache/pacman/pkg/ 目录.
pacman -Scc: 清理所有的缓存文件.

pacman -Qeq #查询所有自己安装的软件，只显示包名，不显示版本号等
pacman -Qs <pkg_name> #查询本地安装的所有带<pkg_name>的软件
pacman -Qdt #查询所有孤儿软件，不再被需要的。
pacman -Qdtq #查询所有不再被依赖的包名

查询孤儿软件并删除掉他们
pacman -R $(pacman -Qdtq)

## 最常用的pacman命令小结

pacman命令较多, 作为新手, 将个人最常用的命令总结如下:

pacman -Syu: 升级系统及所有已经安装的软件.
pacman -S 软件名: 安装软件.

也可以同时安装多个包, 只需以空格分隔包名即可.

pacman -Rs 软件名: 删除软件, 同时删除本机上只有该软件依赖的软件.
pacman -Ru 软件名: 删除软件, 同时删除不再被任何软件所需要的依赖.
pacman -Ssq 关键字: 在仓库中搜索含关键字的软件包, 并用简洁方式显示.
pacman -Qs 关键字: 搜索已安装的软件包.
pacman -Qi 软件名: 查看某个软件包信息, 显示软件简介,构架,依赖,大小等详细信息.
pacman -Sg: 列出软件仓库上所有的软件包组.
pacman -Sg 软件包组: 查看某软件包组所包含的所有软件包.
pacman -Sc: 清理未安装的包文件, 包文件位于 /var/cache/pacman/pkg/ 目录.
pacman -Scc: 清理所有的缓存文件.

