# cmake-buffer

[CMake Reference Documentation](https://cmake.org/cmake/help/latest/index.html#)
[CMake Tutorial](https://cmake.org/cmake/help/latest/guide/tutorial/index.html#guide:CMake%20Tutorial)

[Visual Studio 中的 CMake 项目](https://learn.microsoft.com/zh-cn/cpp/build/cmake-projects-in-visual-studio?view=msvc-160)
[在 Visual Studio 中创建 C++ 跨平台项目](https://learn.microsoft.com/zh-cn/cpp/build/get-started-linux-cmake?source=recommendations&view=msvc-170)

[Modern CMake 简体中文版](https://modern-cmake-cn.github.io/Modern-CMake-zh_CN/)
[Cmake 实践](http://file.ncnynl.com/ros/CMake%20Practice.pdf)
[CMake Primer](https://llvm.org/docs/CMakePrimer.html)
[Effective Modern CMake 实践](https://zhjwpku.com/category/2020/04/04/effective-modern-cmake-practice.html)
[ttroy50/cmake-examples](https://github.com/ttroy50/cmake-examples)
[cmake-examples-Chinese](https://sfumecjf.github.io/cmake-examples-Chinese/)

[cmake(13): 构建时设置预处理宏定义以及add_compile_definitions命令详解](https://blog.csdn.net/rangfei/article/details/125651845)

## cmake 列出项目中的所有 target, 所有编译目标, 所有 targets

[How do I list the defined make targets from the command line?](https://stackoverflow.com/questions/30793804/how-do-i-list-the-defined-make-targets-from-the-command-line)

对于 `Makefile generator` 的构建环境, 您可以使用

```bash
cmake --build . --target help
```

还有图形输出解决方案(此处有示例):

```bash
cmake --graphviz=test.graph
dotty test.graph
```

另请参阅 [使用 CMake 生成依赖关系图](https://gitlab.kitware.com/cmake/community/-/wikis/doc/cmake/Graphviz)
和 [CMake Graphviz Output Cleaner](https://www.semipol.de/software/cmake-graphviz-output-cleaner).

如果没有安装 `dotty`, 仍可在 `CMakeLists.txt` 中启用 [GLOBAL_DEPENDS_DEBUG_MODE](http://www.cmake.org/cmake/help/v3.2/prop_gbl/GLOBAL_DEPENDS_DEBUG_MODE.html),
使目标依赖关系可见:

```cmake
set_property(GLOBAL PROPERTY GLOBAL_DEPENDS_DEBUG_MODE 1)
```

这里的缺点是无法从命令行触发.
在 generate `make` 环境时, 它将始终显示在 `stderr` 中.

参考资料

[如何获取 cmake 目标的依赖关系列表?](https://stackoverflow.com/questions/22021312/how-can-i-get-the-list-of-dependencies-of-cmake-target)
[Internet Archive:  "CMake: 目标列表](https://web.archive.org/web/20160405081525/https://root.cern.ch/blog/cmake-list-targets)

### make help

[How do you get the list of targets in a makefile?](https://stackoverflow.com/questions/4219255/how-do-you-get-the-list-of-targets-in-a-makefile)

如果您的 Makefile 是由 CMake 创建的, 您可以运行 `make help`.

```bash
$ make help
The following are some of the valid targets for this Makefile:
... all (the default if no target is provided)
... clean
... depend
... install
etc
```

如果没有, 我写了一个补丁, 为 Make 添加对这一明显有用的功能的适当支持.
这比其他所有的答案都要好得多,
因为其他的答案都是用可怕的黑客手段来对 Makefile 进行搜索.
如果你包含其他 Makefile, 使用 计算得到的target name等, 这显然是行不通的.

补丁尚未合并, 所以你必须从源代码构建.
这并不难, 但你确实需要一些与 autoconf 相关的老旧构建工具:

git clone https://github.com/Timmmm/make
cd make
./bootstrap
./configure
make -j4
在 Linux 上, 你可以使用[我已经编译好的二进制文件](https://github.com/Timmmm/make/releases/download/0.0/make).

然后使用 `-l` 标志列出目标:

```bash
./make -C /path/to/your/project -l
```

## cmake 编译项目 Linux

+ 重新构建 makefile

```bash
cmake -B . -S .. -G 'Unix Makefiles' --fresh
```

+ 生成 makefile; 优化配置为 Release; 指定安装路径前缀;

```bash
cmake -B . -S .. -G 'Unix Makefiles' -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=C:/cppLibs/deal.ii
```

+ 调用编译器生成项目; compile Release 版本; -j 指定线程; -t <tgt> 指定编译目标, 如 INSTALL, 多个目标使用空格分隔

```bash
cmake --build . --config Debug -j 8 #-- 调用 gcc 编译源代码
```

## cmake 编译项目 Windows

```bash
#--- 获取 Generator 列表
cmake --help
#-- 生成 .sln; MSVC 不需要指定 CMAKE_BUILD_TYPE;
cmake -B . -S ..  -G 'Visual Studio 17 2022' -DCMAKE_INSTALL_PREFIX=C:/cppLibs/deal.ii

# 编译源代码; Debug 版本; 8 threads
cmake --build . --config Debug -j 8
#----- 或者生成 .sln 后, 使用 msvc msbuild 编译
msbuild /m /v:n /p:Platform=x64 /p:Configuration=Debug Test.sln

#-- 安装编译得到的二进制文件
cmake --install . --config Release --prefix C:/cppLibs/xxx
```

## cmake 编译项目 MSYS

### 生成编译规则

在 MSYS2 clang64 环境中编译,
确保 `clang.exe`, `clang++.exe` 已经添加到环境变量.

```bash
#  使用 ninja native tool;
# 使用 ninja 可能没有颜色; 即便已经 add_compile_options("-fcolor-diagnostics") 并且设置环境变量 CLICOLOR_FORCE=1
cmake -G 'Ninja Multi-Config' -B . -S .. -DCMAKE_C_COMPILER=clang.exe -DCMAKE_CXX_COMPILER=clang++.exe --fresh

# 指定 make 程序路径, 指定构建类型为 debug
cmake -G 'MSYS Makefiles' -B . -S .. -DCMAKE_C_COMPILER=clang.exe -DCMAKE_CXX_COMPILER=clang++.exe -DCMAKE_MAKE_PROGRAM='c:/msys64/usr/bin/make.exe' -DCMAKE_BUILD_TYPE=Debug --fresh

# 使用 gcc 进行编译
cmake -G 'MSYS Makefiles' -B . -S .. -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ -DCMAKE_MAKE_PROGRAM='c:/msys64/usr/bin/make.exe' -DCMAKE_BUILD_TYPE=Debug --fresh
```

### 编译目标

```bash
# -j 指定并线编译; -v 开启 verbose 输出
cmake --build . -j 10 -v

# 直接使用 make 编译; VERBOSE 是 CMake 添加的 VERBOSE 选项
make VERBOSE=1 -j 10

make -j10 debug
```

## ss

cmake -B . -S .. -G 'Unix Makefiles' \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/home/tom/cpplibs/suitesparse \
    -DCMAKE_Fortran_COMPILER=gfortran \
    -DBUILD_SHARED_LIBS=ON \
    -DSUITESPARSE_USE_OPENMP=ON \
    -DSUITESPARSE_USE_FORTRAN=ON