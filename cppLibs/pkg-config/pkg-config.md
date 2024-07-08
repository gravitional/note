# pkg-config

[pkg-config原理及用法](https://www.cnblogs.com/sddai/p/10266624.html)
[https://linux.die.net/man/1/pkg-config](https://linux.die.net/man/1/pkg-config)

 大家应该都知道用第三方库, 就少不了要使用到第三方的头文件和库文件.
我们在编译, 链接的时候, 必须要指定这些头文件和库文件的位置.

对于一个比较大第三方库, 其头文件和库文件的数量是比较多的.
如果我们一个个手动地写, 那将是相当麻烦的.
所以, `pkg-config` 就应运而生了.
`pkg-config` 能够把这些头文件和库文件的位置指出来, 给编译器使用.
如果你的系统装有 `gtk`, 可以尝试下面的命令

```bash
pkg-config --cflags gtk+-2.0.
```

可以看到其输出是 `gtk` 的头文件的路径.

我们平常都是这样用pkg-config的.

```bash
$gcc main.c `pkg−config --cflags --libs>k+-2.0` -o main
```

上面的编译命令中, `` `pkg−config --cflags --libs>k+-2.0` ``
的作用就如前面所说的, 把 `gtk` 的头文件路径和库文件列出来, 让编译去获取.
`--cflags`和`--libs`分别指定头文件和库文件.

Ps:命令中的`` ` ``不是引号, 而是数字`1`左边那个键位的那个符号.
其实, pkg-config同其他命令一样, 有很多选项, 不过我们一般只会用到`--libs`和`--cflags`选项.
更多的选项可以在这里查看.

you can list the directories pkg-config looks in by default using:

```bash
pkg-config --variable pc_path pkg-config
# 输出../lib/pkgconfig;../share/pkgconfig
echo $PKG_CONFIG_PATH
export PKG_CONFIG_PATH=/ucrt64/lib
export PKG_CONFIG_PATH=/ucrt64/lib/pkgconfig
export PKG_CONFIG_PATH=''

pkg-config.exe --cflags hdf5
pkg-config --exists --print-errors hdf5
```

`PKG_CONFIG_PATH` needs the full `/usr/local/lib/pkgconfig` pathname appended to the variable.
