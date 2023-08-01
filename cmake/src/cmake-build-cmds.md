# cmake 查看生成的编译指令

[How can I see the exact commands?](https://stackoverflow.com/questions/2670121/using-cmake-with-gnu-make-how-can-i-see-the-exact-commands)

运行 `make` 时, 添加 `VERBOSE=1` 以查看完整的命令输出. 例如

```bash
cmake .
make VERBOSE=1
```

或者在 `cmake` 命令中添加 `-DCMAKE_VERBOSE_MAKEFILE:BOOL=ON`,
以持续性地从生成的 `Makefile` 中获得 详细命令输出.

```bash
cmake -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON .
make
```

为了减少一些可能不那么有趣的输出, 您可能需要使用以下选项.
`CMAKE_RULE_MESSAGES=OFF` 可以删除类似

    [ 33%] Building C object...

这样的行,

而 `--no-print-directory` 则告诉 `make` 不打印当前目录,
过滤掉类似

    make[1]: Entering directory and make[1]: Leaving directory

综合起来就是

```bash
cmake -DCMAKE_RULE_MESSAGES:BOOL=OFF -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON .
make --no-print-directory
```

也可以在 `CMakeLists.txt` 设置 Builtin 变量

```cmake
set(CMAKE_VERBOSE_MAKEFILE ON)
```

## --verbose

```bash
cmake --build . --verbose
```

在 Linux 和生成 Makefile 的情况下,
底层很可能只是调用 `make VERBOSE=1`,
但 `cmake --build` 可以为你的build系统提供更多的可移植性,
例如跨操作系统工作, 或者如果你决定以后进行 Ninja build 等:

```bash
mkdir build
cd build
cmake ..
cmake --build . --verbose
```

它的文档也表明它等同于 `VERBOSE=1`:

>`--verbose, -v`
>启用详细输出(如果支持), 包括要执行的编译命令.
>如果设置了 `VERBOSE` 环境变量或 `CMAKE_VERBOSE_MAKEFILE` 缓存变量,
>则可以省略此选项.
