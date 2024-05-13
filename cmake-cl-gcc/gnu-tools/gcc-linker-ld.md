# linker 选项

[Options for Linking](https://gcc.gnu.org/onlinedocs/gcc/Link-Options.html)

## `-Wl,option`

将 `option ` 作为选项传递给 linker.
如果 `option` 包含逗号, 则会在逗号处将其分割为多个选项.
您可以使用该语法向选项传递一个参数.
例如, `-Wl,-Map,output.map` 将 `-Map output.map` 传递给链接器.
在使用 GNU 链接器时, 也可以用 `-Wl,-Map=output.map` 得到同样的效果.

+ `-R filename`
+ `--just-symbols=filename`
从filename中读取 symbol names及其地址,
但不对其重新定位, 也不将其包含在输出中.
这样, 输出文件就能以符号方式引用其他程序中定义的内存绝对位置.
您可以多次使用该选项.

为了与其他 ELF 链接器兼容, 如果 `-R` 选项后面跟的是目录名而不是文件名,
则会被视为 `-rpath` 选项.

## `-Wl,-rpath=dir`

[binutils ld Command-line Options](https://sourceware.org/binutils/docs-2.41/ld/Options.html)

在 `runtime library search path` 中添加一个目录.
这在连接包含 `shared objects` 的 ELF可执行文件 时使用.
所有 `-rpath` 参数都会被连接并传递给运行时链接器,
后者会在运行时使用这些参数来定位 `shared objects`.

在定位链接 中明确包含的共享对象所需的共享对象时, 也会使用 `-rpath` 选项
请参阅 `-rpath-link` 选项的说明.
只有使用 `--with-sysroot` 选项配置的
本地链接器 和 交叉链接器才支持以这种方式搜索 -rpath.

如果在链接 ELF 可执行文件时未使用 `-rpath` 选项, 则将使用已定义的环境变量 `LD_RUN_PATH` 的内容.

在 SunOS 上也可以使用 -rpath 选项.
默认情况下, 在 SunOS 上, 链接器将从给出的所有 -L 选项中生成一个运行时搜索路径.
如果使用了 -rpath 选项, 运行时搜索路径将完全由 `-rpath` 选项构成,
而忽略 `-L` 选项.
这在使用 gcc 时很有用, 因为 gcc 会添加许多 -L 选项,
而这些选项可能位于 NFS 挂载的文件系统上.

为了与其他 ELF 链接器兼容,
如果 `-R` 选项后面跟的是`目录名`而不是文件名, 则会被视为 `-rpath` 选项.

## `-Wl,-rpath-link=dir`

-rpath-link=dir
使用 ELF 或 SunOS 时, 一个共享库可能需要另一个共享库.
当 `ld -shared` 链接将 shared library 作为输入文件之一时, 就会出现这种情况.

当链接程序在进行非共享, 非可重定位链接时遇到这种依赖关系时, 如果没有明确将其包含在内,
它会自动尝试找到所需的共享库并将其包含在链接中.
在这种情况下, `-rpath-link` 选项指定了要搜索的第一组目录.
-rpath-link选项可以指定一系列目录名,
可以是用 `冒号分隔的目录名列表`,
也可以是多次使用 `-rpath-link=dir` 选项.

这些搜索目录中可以出现 `$ORIGIN` 和 `$LIB` token.
在 `$ORIGIN` 的情况下, 它们将被包含程序或共享对象的目录的完 整路径替换;
在 $LIB 的情况下, 它们将被 `lib`(32 位二进制文件)或 `lib64`(64 位二进制文件)替换.

也可以使用这些标记的另一种形式--`${ORIGIN}`和`${LIB}`.
不支持 `$PLATFORM` 标记.

使用该选项时应谨慎, 因为它可能会覆盖硬编译到共享库中的搜索路径.
在这种情况下, 可能会无意中使用与运行时链接器不同的搜索路径.
链接器使用以下搜索路径查找所需的共享库:

+ 由 -rpath-link 选项指定的任何目录.
+ 由 -rpath 选项指定的任何目录.
-rpath 和 -rpath-link 的区别在于,
-rpath 选项指定的目录会包含在可执行文件中, 并在运行时使用, 而 `-rpath-link` 选项只在链接时有效.
只有使用 `--with-sysroot` 选项配置的本地链接器和交叉链接器, 才支持以这种方式搜索 `-rpath`.
+ 在 ELF 系统上, 对于本地链接器, 如果未使用 -rpath 和 -rpath-link 选项,
则搜索环境变量 `LD_RUN_PATH` 的内容.

+ 在 SunOS 上, 如果未使用 -rpath 选项, 则搜索使用 -L 选项指定的任何目录.
+ 对于本地链接器, 搜索环境变量 `LD_LIBRARY_PATH` 的内容.
+ 对于本地 ELF 连接器, 会搜索共享库的 DT_RUNPATH 或 DT_RPATH 目录, 以查找所需的共享库.
如果存在 DT_RUNPATH 条目, 则 DT_RPATH 条目将被忽略.
+ 对于 Linux 系统的链接器, 如果存在文件 `/etc/ld.so.conf`, 则在该文件中找到目录列表.
注意: 如果定义了 `sysroot` 值, 该文件的路径将以 `sysroot` 值为前缀,
如果使用 `--prefix=<path>` 选项配置了链接器, 则以任何 `prefix` 字符串为前缀.

+ 对于 FreeBSD 系统上的本地链接器, `elf-hints.h` 头文件中定义的 `_PATH_ELF_HINTS` 宏所指定的任何目录.
+ 在命令行中给出的链接器脚本中, 由 `SEARCH_DIR` 命令指定的任何目录, 包括由 `-T` 指定的脚本(但不包括 -dT).
+ 默认目录, 通常是 `/lib` 和 `/usr/lib`.
+ 由插件 `LDPT_SET_EXTRA_LIBRARY_PATH` 指定的任何目录.
+ 默认链接器脚本中 SEARCH_DIR 命令指定的任何目录.

但请注意, 在基于 Linux 的系统中还有一个额外的注意事项:
如果启用了 --as-needed 选项, 并且找到了一个通常可以满足搜索要求的共享库,
但该库没有 libc.so 的 DT_NEEDED 标记, 而在搜索目录集的后面还有一个共享库也可以满足搜索要求,
并且第二个共享库确实有 libc.so 的 DT_NEEDED 标记, 那么第二个库将被选中, 而不是第一个库.

如果找不到所需的共享库, 链接器将发出警告并继续链接.

## gcc 指定链接方式, static, dynamic

[在Linux下, 如何强制让GCC静态链接?](https://www.zhihu.com/question/22940048/answer/222625910)

gcc使用 `-Wl` 传递连接器参数,
ld使用 `-Bdynamic` 强制连接动态库, `-Bstatic` 强制连接静态库.

所以部分静态, 部分动态连接这么写:

```bash
gcc ... -Wl,-Bstatic -l<your-static-lib> -Wl,-Bdynamic -l<your-dynamic-lib> ...
```

举个例子, 你想静态连接 `libA.a` 同时动态连接 `libB.so`,
(先保证你的连接路径-L里面能找到对应的静态或者动态库), 这么写:

```bash
gcc ... -Wl,-Bstatic -lA -Wl,-Bdynamic -lB ...
```

这里需要注意, 强制静态或者动态连接标记之后的链接库,
都将按照前面最近的一个标记进行链接,
所以如果后面出现了一个libC, 没有指定连接标记, 那么libC将会被动态连接:

```bash
gcc ... -Wl,-Bstatic -lA -Wl,-Bdynamic -lB ... -lC
```

如果参数里面没指定强制的连接方式标记,
那么gcc将按照默认的优先级去链接, 优先动态链接,
所以如果你这么写,
且同时存在 `libC.so` 和 `libC.a` 那么libC将被动态链接:

```bash
gcc ... -lC
```

由于-B连接标记会改变默认连接方式, 所以在Makefile里面如果有人这么干:

```makefile
LIBS += -Wl,-Bstatic -lC
```

那么他后面的LIBS+=的库就都只能以静态方式连接了, 有时候这是不行的,
因为没有静态库, 所以会有人这么应对:

```makefile
LIBS += -Wl,-Bdynamic -lD
```

这样就改回来了.
但是这种胡乱改的行为是非常不好的, 比较好的行为应该这样:

```makefile
LIBS += -l<auto-link-lib>
STATIC_LIBS += -l<static-lib>
DYN_LIBS += -l<dynamic-lib>
LDFLAGS := ${LIBS} -Wl,-Bstatic ${STATIC_LIBS} -Wl,-Bdynamic ${DYN_LIBS}
```

这样当你不关心怎么连接的时候用 `LIBS`,
当你想静态连接的时候用 `STATIC_LIBS`, 当你想动态连接的时候用 `DYN_LIBS`.

## gcc 链接顺序

[GCC linking libc static and some other library dynamically, revisited?](https://stackoverflow.com/questions/26277283/gcc-linking-libc-static-and-some-other-library-dynamically-revisited)

基本上, 你的第一种方法是正确的:

```bash
gcc test.c libc.a -lm
```

在 `gcc` 添加 implicit libraries 后, 它看起来(概念上)是这样的

```bash
gcc crt1.o test.c libc.a -lm -lc -lgcc -lc
```

这意味着, `crt1.o` 或 `test.c` 调用的 libc 函数将
从 `libc.a` 导入并静态链接, 而仅从 `libm` 或 `libgcc`
调用的函数将动态链接
(但如果 `libm` 调用了已经 pulled in 的函数, 则会重用静态函数).

链接器总是从最左侧的 file/library 开始向右链接, 不会折返向左.
`.c` 和 `.o` 文件是无条件链接的,
但 `.a` 文件和 `-l` 选项仅用于查找已被引用但尚未定义的函数.
因此, 左边的库是没有意义的
(而且 `-lc` 必须出现两次, 因为 `-lc` 依赖于 `-lgcc`,
而 `-lgcc` 依赖于 `-lc`).
**链接顺序很重要!**

不幸的是, 你似乎被 `strcmp`
(或者说是包含 `strcmp` 的 `libc`)中的一个 bug 所挫败:
`STT_GNU_IFUNC` 是一个聪明的功能,
它允许包含一个函数的多个版本, 并在运行时根据可用的硬件选择最合适的版本.
我不太确定, 但看起来这个功能只有在 `PIE`
(Position Independent Executable)或共享库构建中可用.

为什么要在静态 `libc.a` 中使用这个功能对我来说是个谜,
但有一个简单的解决方法: 实现你自己的 `strcmp`
(一个基本的, 慢速的实现只需几行 C 语言), 并在 `libc.a` 之前将其链接进去.

```bash
gcc test.c mystrcmp.c libc.a -lm
```

或者, 你也可以从 `libc.a` 中提取你真正需要的函数, 然后只静态链接这些函数:

```bash
ar x libc.a
gcc test.c somefile.o -lm
```

`ar` 与 `.a` 文件的关系就像 `tar` 与 `.tar` 文件的关系一样,
但命令用法略有不同, 因此本例从 `.a` 文件中提取 `.o` 文件, 然后明确链接它们.

## 找出标准库的位置

[Location of C standard library](https://stackoverflow.com/questions/5925678/location-of-c-standard-library)

使用以下命令找出 `libc.a` 的路径:

```bash
gcc --print-file-name=libc.a
/usr/lib/gcc/x86_64-linux-gnu/4.8/../../../x86_64-linux-gnu/libc.a
```
