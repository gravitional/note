# msys2 使用

## 登录shell

[改变MSYS2默认shell为zsh](https://zhuanlan.zhihu.com/p/555502485)

在windows上使用MSYS2时, 想把默认的bash换掉
查看安装的 `shell/etc/shells`

```bash
cat /etc/shells
#
# /etc/shells
#
/usr/bin/zsh    //有zsh
/bin/zsh
...

# End of file
```

在msys2安装目录 `D:\msys64下/etc/nsswitch.conf` 文件中添加这行代码:

```conf
db_shell: /bin/zsh
```

现在通过 `msys.exe` 启动就是默认 `zsh`.

启动后, 如果你的msys的shell依然没有改变, 那么你大概不是直接打开的 `msys.exe`,而是通过msys提供的快捷方式!
这个msys提供的默认快捷方式是通过安装目录下的 `msys2_shell.cmd` 来启动msys的, 在 `sys2_shell.cmd` 中有这样一句:

```cmd
4 | ...
5 | set "LOGINSHELL=bash" //将bash替换为zsh即可改变快捷方式启动后的shell
6 | ...
```

将其中的 `bash` 替换为 `zsh` 即可改变通过快捷方式启动后的shell

## 安装编译环境

[MSYS2安装gcc, make环境](https://www.jianshu.com/p/04636461341e)

使用过archlinux的应该会知道, pacman在安装的时候, 如果源没有设置好, 下载是很慢的.

需要修改的文件是:

```conf
\etc\pacman.d\mirrorlist.mingw32
\etc\pacman.d\mirrorlist.mingw64
\etc\pacman.d\mirrorlist.msys
```

这三个文件
镜像源我推荐使用下面的这两个:

清华大学 https://mirrors.tuna.tsinghua.edu.cn/
中国科学技术大学 http://mirrors.ustc.edu.cn/

### pacman基本命令

```bash
pacman -Sy 更新软件包数据
pacman -Syu 更新所有
pacman -Ss xx 查询软件xx的信息
pacman -S xx 安装软件xx
pacman -R xx 删除软件xx
```

### 安装gcc, g++编译器

```bash
#查询并找到msys/gcc
pacman -Ss gcc
#安装
pacman -S msys/gcc
```

### 安装make编译器

```bash
#查询并找到msys/make
pacman -Ss make
#安装
pacman -S msys/make
```

### 安装Clion编译工具链

```bash
pacman-key --init
pacman -Syu
pacman -S mingw-w64-x86_64-cmake mingw-w64-x86_64-extra-cmake-modules
pacman -S mingw-w64-x86_64-make
pacman -S mingw-w64-x86_64-gdb
pacman -S mingw-w64-x86_64-toolchain
```

### pacman 的配置

编辑 `/etc/pacman.d/mirrorlist.mingw32`, 在文件开头添加:

```conf
Server = https://mirrors.tuna.tsinghua.edu.cn/msys2/mingw/i686
```

编辑 `/etc/pacman.d/mirrorlist.mingw64`, 在文件开头添加:

```conf
Server = https://mirrors.tuna.tsinghua.edu.cn/msys2/mingw/x86_64
```

编辑 `/etc/pacman.d/mirrorlist.msys`, 在文件开头添加:

```conf
Server = https://mirrors.tuna.tsinghua.edu.cn/msys2/msys/$arch
```

然后执行 pacman -Sy 刷新软件包数据即可.
