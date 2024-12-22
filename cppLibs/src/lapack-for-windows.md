# intel oneAPI fortran 编译器 ifx, c++ 编译器 icx 编译 LAPACK

## 使用 cmake + intel oneAPI, ifx 编译器编译

cmake 3.29 之后支持 `Visual Studio 17 2022` 选择 fortran 编译器,
指定cmake 命令行选项 `-T "fortran=ifx"`;
相关的 CMAKE 变量是 `CMAKE_GENERATOR_TOOLSET`,

LAPACK 官方提供的 `CMakeLists.txt` 中, 如果开启 `BLAS++`, `LAPACK++`,
需要在线下载包, 可能失败, 这里暂且选择关闭;
开启 `BLAS++`, `LAPACK++` 在命令行中加上选项 `-DBLAS++:BOOL=ON -DLAPACK++:BOOL=ON`.

### 设置环境变量

安装好 intel fortran 和 c++ 编译器之后,
打开 `intel oneAPI command prompt for Intel 64 for Visual Studio 2022` 窗口,
它会自动设置**环境变量**, 参考 [scripts-to-set-environment-variables](https://www.intel.com/content/www/us/en/docs/onemkl/developer-guide-linux/2025-0/scripts-to-set-environment-variables.html).

可以在 pwsh 下运行下面的命令, 是等价的(用 nushell 运行, 因为 quoting rule 不同, 好像会有问题)

```powershell
cmd.exe /k '"C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022 && pwsh'
```

>顺带一提, 上面的 `intel oneAPI command prompt` 快捷方式执行的命令是(可以在属性页查看, cmd 的引号规则非常奇葩)

```batch
C:\Windows\System32\cmd.exe /E:ON /K ""C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022"
```

执行成功之后, 可以用以下命令验证是否能找到 intel 编译器

```pwsh
gcm icx,ifx
# 输出
icx.exe  2025.0.4.0 C:\Program Files (x86)\Intel\oneAPI\compiler\latest\bin\icx.exe
ifx.exe  2025.0.4.0 C:\Program Files (x86)\Intel\oneAPI\compiler\latest\bin\ifx.exe
```

### 设置 CMake 编译选项

**在设置好环境变量的 pwsh 中运行下列的命令**, 设置 cmake 项目属性,

+ 注意适当修改 `-DCMAKE_INSTALL_PREFIX` 后面的位置,

+ 这里指定了 c 编译器为 `icx-cl.exe`, 与微软的 MSVC 编译器具有兼容的选项,
默认在 `C:\Program Files (x86)\Intel\oneAPI\compiler\latest\bin` 目录.
关于编译器 `icx`, `icx-cc`, `icx-cl` 的区别, 可以参考 [Get Started on Windows*](https://www.intel.com/content/www/us/en/docs/dpcpp-cpp-compiler/get-started-guide/2025-0/get-started-on-windows.html)

+ 生成系统必须使用 `Ninja`, 参考 [Use CMake with the Compiler](https://www.intel.com/content/www/us/en/docs/dpcpp-cpp-compiler/developer-guide-reference/2025-0/use-cmake-with-the-compiler.html), 以及 [如何让cmake在windows上编译时使用Intel编译器icx?](https://www.zhihu.com/question/605196623)
**MSVC 不支持 fortran 和 c 项目混合编译**, 编译 CBLAS 的时候会失败.

我开启的选项如下

```bash
cmake -G Ninja -B . -S ..  -DCMAKE_LINKER_TYPE=MSVC -DCMAKE_C_COMPILER=icx-cl -DCMAKE_CXX_COMPILER=icx-cl -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_INSTALL_PREFIX="C:/Users/qingz/Downloads/lapack-install"  -DCBLAS:BOOL=ON -DLAPACKE:BOOL=ON  -DBUILD_COMPLEX=ON -DBUILD_COMPLEX16=ON -DBUILD_DOUBLE:BOOL=ON -DBUILD_INDEX64_EXT_API=ON  -DBUILD_SHARED_LIBS:BOOL=ON  -DBUILD_SINGLE:BOOL=ON   -DLAPACKE_BUILD_SINGLE=ON -DUSE_OPTIMIZED_LAPACK:BOOL=OFF -DUSE_OPTIMIZED_BLAS:BOOL=OFF -DBLAS++:BOOL=OFF -DLAPACK++:BOOL=OFF  -DUSE_XBLAS=OFF
```

使用下面的命令可以查看 全部的构建目标, 但是太多了, 肉眼看没啥意义

```pwsh
ninja -t targets all
```

### 编译项目

使用 `cmake --build` 或者 `ninja` build 项目,
**但是先不要急, 在链接 `liblapacke.dll` 的时候会报错, 所以先手动修改 `CMakelistx.txt` 脚本**

具体表现是, 编译到最后执行 链接命令时,
报一个 [链接器工具错误 LNK1170](https://learn.microsoft.com/zh-cn/cpp/error-messages/tool-errors/linker-tools-error-lnk1170?view=msvc-170),
`windows` + `cmake` + `msvc` 纯沙壁.
原因是要连接的 `.obj` 文件太多, 超过了 `link.exe` 单行命令的长度限制 131071 个字符.
~~之前尝试修改 `build/CMakeFiles/rules.ninja` 文件, 不过效果好像不稳定.~~

[根据 intel 的 编译器选项说明](https://www.intel.com/content/www/us/en/docs/dpcpp-cpp-compiler/developer-guide-reference/2025-0/fuse-ld.html)
可以指定使用 llvm 的 `lld` 链接器, 开关是 `-fuse-ld=lld`,

修改 `LAPACKE\CMakeLists.txt`, 找到

```cmake
add_library(${LAPACKELIB} ${SOURCES})
set_target_properties(
    ${LAPACKELIB} PROPERTIES
    LINKER_LANGUAGE C
    VERSION ${LAPACK_VERSION}
    SOVERSION ${LAPACK_MAJOR_VERSION}
)
```

在下面添加 linker 选项

```cmake
target_link_options(${LAPACKELIB}
    BEFORE PRIVATE "-fuse-ld=lld"
)
```

在 `build\build.ninja` 文件中, 可以搜索 `build bin\liblapacke.dll lib\lapacke.lib`,
可以看到 `LINK_FLAGS` 中确实已添加了 `-fuse-ld=lld` 选项,

```ninja
LANGUAGE_COMPILE_FLAGS = /DWIN32 /D_WINDOWS /W3 /MDd /Zi /Ob0 /Od /RTC1
LINK_FLAGS = /Qoption,link,/machine:x64 /Qoption,link,/debug /Qoption,link,/INCREMENTAL  -fuse-ld=lld /Qoption,link,/DEF:LAPACKE\CMakeFiles\lapacke.dir\.\exports.def
```

`LINK_FLAGS` 在 `build\CMakeFiles\rules.ninja` 中被使用,
用于链接制作 `lapacke.dll`,

```ninja
#############################################
# Rule for linking C shared library.

rule C_SHARED_LIBRARY_LINKER__lapacke_Debug
  command = C:\WINDOWS\system32\cmd.exe /C "$PRE_LINK && "C:\Program Files\CMake\bin\cmake.exe" -E vs_link_dll --msvc-ver=1941 --intdir=$OBJECT_DIR --rc=C:\PROGRA~2\WI3CF2~1\10\bin\100226~1.0\x64\rc.exe --mt=C:\PROGRA~2\WI3CF2~1\10\bin\100226~1.0\x64\mt.exe --manifests $MANIFESTS -- C:\PROGRA~2\Intel\oneAPI\compiler\latest\bin\icx-cl.exe /nologo @$RSP_FILE  -LD $LINK_FLAGS -link /out:$TARGET_FILE /implib:$TARGET_IMPLIB /pdb:$TARGET_PDB /version:3.12 && $POST_BUILD"
  description = Linking C shared library $TARGET_FILE
  rspfile = $RSP_FILE
  rspfile_content = $in_newline $LINK_PATH $LINK_LIBRARIES
  restat = $RESTAT
```

然后运行 `ninja` build 项目, 即可顺利编译成功.
构建要花很久, **直接喝茶**.

```bash
ninja
# 或者
cmake --build . --config release -j
```

### 安装

安装到预定的位置, 可以使用

```bash
ninja install
# 或者
cmake --install . --config Release
```

## intel 链接器选项: fuse-ld

告诉编译器使用不同的链接器, 而不是默认链接器,
默认链接器在 Linux 上是 `ld`, 在 Windows 上是 `link.exe`.

### 语法

Linux 和 windows 相同

```bash
-fuse-ld=keyword
```

keyword: 告诉编译器使用哪个非默认链接器. 可能的值有

+ `bfd`; 告诉编译器使用 bfd 连接器. 此设置仅适用于 Linux.
+ `gold`; 告诉编译器使用 gold 链接器. 此设置仅适用于 Linux.
+ `lld`; 告诉编译器使用 lld 链接器.
+ `llvm-lib`; 告诉编译器使用 LLVM 库. 此设置仅适用于 Windows.

在 Linux 上, 提供此选项是为了与 gcc 兼容.

### 注意

在 Windows 中, 选项 `/Qipo` 会自动设置选项 `-fuse-ld=lld`.

注意
该选项仅适用于 host compilation. 启用 offloading 后, 它不会影响 device-specific compilation.

# lapack for windows, 官网的帖子

[LAPACK for Windows](https://icl.utk.edu/lapack-for-windows/lapack/)

## 您需要什么?

## 在Windows上运行LAPACK

如果您的计算机上安装了 INTEL 编译器,
请下载使用 [INTEL编译器库的预编译静态库](https://icl.utk.edu/lapack-for-windows/lapack/index.html#libraries_intel).

如果您的计算机上 **没有安装** INTEL 编译器, 则需要安装 [MinGW 32 bits](http://www.mingw.org/)
或 [MinGW-w64](http://mingw-w64.sourceforge.net/),
然后下载 [用Mingw预编译的动态链接库](https://icl.utk.edu/lapack-for-windows/lapack/index.html#libraries_mingw).

使用 `LAPACKE C接口` 直接从 C 语言调用 LAPACK.
您需要安装 [MinGW32位](http://www.mingw.org/),
然后下载 [使用Mingw预编译的动态链接库](https://icl.utk.edu/lapack-for-windows/lapack/index.html#libraries_mingw),
或者下载 [VS Studio 解决方案](https://icl.utk.edu/lapack-for-windows/lapack/index.html#lapacke)
(其中包含所有组件(BLAS, LAPACK 和 LAPACKE lib 和 dll)以及两个简单的LAPACKE示例), 您只需解压缩并编译即可.
请不要忘记参阅 [LAPACKE 用户指南](http://netlib.org/lapack/lapacke.html).

## Windows下运行调用LAPACK例程的程序

请参考我们的一位用户提供的 [详尽指南](https://icl.utk.edu/lapack-for-windows/lapack/index.html#running). 见下文.

## 在Windows 下构建LAPACK库

您需要在计算机上安装 [CMAKE](http://www.cmake.org/), 并请参阅 [构建部分](https://icl.utk.edu/lapack-for-windows/lapack/index.html#build).

## 用于 Microsoft Visual Studio 项目的预编译库

### 使用 Mingw 预编译动态链接库

要求 Mingw 32 位或 64 位
信息:  这些库使用 CMAKE for Visual Studio 2015 和 Mingw 编译器构建,
对应 LAPACK 3.7.0 + Bug Fix.

说明:

+ 下载符合您需求的 `BLAS` 和 `LAPACK` dll 和 lib. 请参见下表
+ 将使用 MSVC 构建的 C应用程序与刚刚下载的 BLAS 和 LAPACK 库(lib 文件)连接起来.
在项目属性中, 更改 `Linker > General > Additional Library Directory`属性, 告诉 Visual Studio 库的位置,
并在 `Linker > Input > Additional Dependencies` 中添加 `BLAS` 和 `LAPACK` 库的名称,
只需输入 `liblapack.lib;libblas.lib` 即可.

+ 正确编译应用程序后, 不要忘记将 `liblapack.dll` 和 `libblas.dll` 复制到可执行文件所在的位置,
或者确保 `dll` 位于系统路径上, 或者将它们放在 WINDOWS\system32 文件夹中, 否则二进制文件将无法运行.

您的应用程序还需要 MinGW 中的 GNU运行时 DLL(需要 `libgfortran-3.dll` 和 `libgcc_s_dw2-1.dll`).
只需将 GNU runtime 目录(例如 32 位为 `C:\MinGW\bin`)放入 `PATH`, 就可以使用了.

LAPACKE 的说明:

+ 下载 `BLAS`, `LAPACK` 和 `LAPACKE` dll.
目前只有 Win32 版本可用(但你可以用 CMAKE 构建自己的版本) 见下表
将使用 MSVC 构建的 C 应用程序, 与刚刚下载的 BLAS, LAPACK 和 LAPACKE 库(lib 文件)连接起来.
在项目属性中, 更改 `Linker > General > Additional Library Directory`属性, 告诉 Visual Studio 库的位置,
并在 `Linker > Input > Additional Dependencies` 中添加 BLAS, LAPACK 和 LAPACKE 库的名称,
只需输入 `liblapacke.lib;liblapack.lib;libblas.lib` 即可.
+ 具体对于 LAPACKE, 您需要在 `C/C++ > Preprocessor > Preprocessor Definitions` 中添加
`ADD_;HAVE_LAPACK_CONFIG_H;LAPACK_COMPLEX_STRUCTURE;`.

+ 一旦您的应用程序编译正确,
不要忘记将 `liblapacke.dll`, `liblapack.dll` 和 `libblas.dll` 复制到您的可执行文件所在的位置,
或者确保这些 dll 位于您的系统路径上, 或者将它们放在 `WINDOWS\system32` 文件夹中, 否则二进制文件将无法运行.
+ 您的应用程序还需要 MinGW 中的 GNU 运行时 DLL(需要 `libgfortran-3.dll` 和 `libgcc_s_dw2-1.dll`).
只需将 GNU 运行时目录(例如, 32 位为 C:\MinGW\bin)放入 `PATH`, 就可以使用了

+ 请不要忘记参阅 [LAPACKE 用户指南](http://netlib.org/lapack/lapacke.html).

CORREPOND TO LAPACK 3.7.0 WITH ADDITIONAL FIXES TO LAPACKE BUILD (JAN 13TH 2016)

#### Release

Ref BLAS

[x64 dll](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win64/libblas.dll)
[x64 lib](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win64/libblas.lib)
[win32 dll](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win32/libblas.dll)
[win32 lib](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win32/libblas.lib)

LAPACK

[x64 dll](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win64/liblapack.dll)
[x64 lib](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win64/liblapack.lib)
[win32 dll](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win32/liblapack.dll)
[win32 lib](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win32/liblapack.lib)

LAPACKE

[x64 dll](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win64/liblapacke.dll)
[x64 lib](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win64/liblapacke.lib)
[win32 dll](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win32/liblapacke.dll)
[win32 lib](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.7.0/Dynamic-MINGW/Win32/liblapacke.lib)

[LAPACKE header file](https://icl.utk.edu/lapack-for-windows/include/lapacke.h)
[LAPACKE mangling header file](https://icl.utk.edu/lapack-for-windows/include/lapacke_mangling.h)
[LAPACKE config header file](https://icl.utk.edu/lapack-for-windows/include/lapacke_config.h)
[LAPACKE utils header file](https://icl.utk.edu/lapack-for-windows/include/lapacke_utils.h)

### 使用 INTEL 编译器预制静态库

要求 适用于 Windows 的英特尔编译器
信息:  这些库是使用 CMAKE for Visual Studio 2010 和 INTEL 编译器构建的, 与 LAPACK 3.5.0 对应.

#### Release

[Ref BLAS x64](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win64/BLAS.lib)
[Ref BLAS win32](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win32/BLAS.lib)

[LAPACK x64](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win64/LAPACK.lib)
[LAPACK win32](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win32/LAPACK.lib)

[LAPACKE x64](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win64/LAPACKE.lib)
[LAPACKE win32](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win32/LAPACKE.lib)

#### Debug

[BLASx64](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win64/BLASd.lib)
[BLAS win32](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win32/BLASd.lib)

[LAPACK x64](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win64/LAPACKd.lib)
[LAPACK win32](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win32/LAPACKd.lib)

[LAPACKE x64](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win64/LAPACKEd.lib)
[LAPACKE win32](https://icl.utk.edu/lapack-for-windows/libraries/VisualStudio/3.5.0/Static-INTEL/Win32/LAPACKEd.lib)

## 在 Windows 下运行 LAPACK

感谢[Olumide, Evgenii Rudnyi 和 Mark Hoemmen](http://groups.google.com/group/matrixprogramming)
的早期建议和对本 HOWTO 草案的阅读.
本文档中的任何不准确之处均由我本人负责.
请谨慎使用.

### 1:获取 BLAS 库

#### 前言

LAPACK 设计为双层(two-tiered)Fortran 库,
包括高层子程序和 `lower-level Basic Linear Algebra Subprograms (BLAS)`,
以便有效利用现代基于高速缓存的体系结构上的高速缓存, see [wiki lapack](http://en.wikipedia.org/wiki/LAPACK).
为便于参考, LAPACK 安装程序提供了一个未经调整的 BLAS 版本, 该版本未针对任何体系结构进行优化.
对于矩阵因式分解和其他计算密集型矩阵运算, 该 reference BLAS 实现可能比优化实现慢几个数量级.
许多供应商和项目都提供经过优化的 BLAS 实现, 例如 Intel (商用), AMD, ATLAS 和 GotoBLAS.

##### 1a:使用参考 BLAS

可在此处下载 [Windows 的 Reference BLAS](https://icl.utk.edu/lapack-for-windows/lapack/index.html#libraries).

#### 1b:使用 MKL BLAS

[MKL](http://software.intel.com/en-us/intel-mkl/) 这当然与 INTEL Fortran 编译器相匹配.

#### 1b:使用 ACML BLAS

[AMD (AMCL)](http://developer.amd.com/cpu/Libraries/)

#### 1c:使用 ATLAS BLAS(需要编译)

需要 [Cygwin](http://cygwin.org/) 或 [MinGW](http://www.mingw.org/)

[ATLAS](http://math-atlas.sourceforge.net/) 的目的是通过 self-discovery 自动生成优化的 BLAS 库.
如需一步步了解操作步骤, 请参阅 [ALTAS 论坛](http://sourceforge.net/projects/math-atlas/forums/forum/1026734/topic/3434546)

#### 1c:使用 GotoBLAS(需要编译)

需要 [MinGW](http://www.mingw.org/) 和 MSYS(必须先安装 MinGW)

GotoBLAS 源代码可从[此处](http://www.tacc.utexas.edu/tacc-projects/)获取(需要填写简短的注册表),
并可通过 MinGW 编译为 Windows 版本.
GotoBLAS 的配置文件 Makefile.rule 无需更改, 除非偏好特定的编译器.
令人欣慰的是, 如果有多个处理器可用, 配置文件会自动启用多线程.

1. 下载并解压 GotoBLAS 源程序到任意选择的目录,
然后对配置文件进行所需的修改(默认选项也可以很好地解决这个问题).
2. `cd` 到包含源代码的顶层目录, 然后键入 `make`.

这一过程的结果应该是 `libgoto_-r.a` 文件和指向该文件的(符号)链接 `libgoto.a`.
(例如, libgoto_banias-r1.26.a),
但默认情况下也会生成 Windows 库 (`*.lib`) 和 `dll`.
.

#### 最后说明

+ Windows 需要被告知在哪里可以找到这个 dll, 否则当你尝试运行程序时就会出现严重错误.
有几种方法可以做到这一点.
一种是将 dll 的位置添加到 PATH 环境变量中.
另一种方法是将 dll 复制到 `Windows/system32` 文件夹.
我采用了后一种方法.
如需了解更多信息, 请参阅微软关于 Windows 查找 DLL 所用搜索路径的指南.

### 2:在 VS INTEL FORTRAN 项目中使用 LAPACK 子程序

1. 下载 [LAPACK预编译二进制文件](https://icl.utk.edu/lapack-for-windows/lapack/index.html#libraries).
预编译调试库的文件名以字母 `d` 结尾,
例如 `BLASd.lib` 和 `LAPACKd.lib`(与发行版 BLAS.lib 和 LAPACK.lib 相比).

2. 找到适合您机器的 BLAS 库.
(如果选择 `GOTOBLAS`, 可能需要选择调试配置)
3. 下载 [LAPACK-VS-Example Visual Studio项目](https://icl.utk.edu/lapack-for-windows/lapack/LAPACK-VS-Example.zip)并解压
4. 将 步骤1 中的库移动或复制到 LAPACK-VS-Example 文件夹中.
5. 如果不使用参考 BLAS, 则需要修改 `Linker > General > Additional Library Directory` 属性, 告诉 Visual Studio 库的位置,
并在 `Linker > Input > Additional Dependencies` 中添加 BLAS 库的名称.
6. 编译项目并运行生成的可执行文件. 你应该会得到以下输出

```bash
Hello World
INFO =           0
  3.00000000000000       0.333333333333333        4.00000000000000
 0.666666666666667
 -4.00000000000000        4.50000000000000
END OF PROGRAM...
```

### 2:在没有FORTRAN编译器的 VS C/C++ 项目中使用 LAPACK 子程序

1. 下载 [MinGW的LAPACK 预编译二进制文件](https://icl.utk.edu/lapack-for-windows/lapack/index.html#libraries_mingw).
您应该有两个文件: `liblapack.lib` 和 `liblapack.dll`(如果您还需要参考 BLAS, 则需要 `libblas.lib`,`libblas.dll` ).

2. 可选项: 为您的机器获取经过调整的 BLAS 版本(请参阅 `编译 GotoBLAS`).

3. 用以下[示例C程序](http://www.cs.rochester.edu/~bh/cs400/using_lapack.html)创建 Visual Studio 项目:
3. 对于 C++ 程序, 将上述程序中的原型重命名为

```c
void dgesv_( )
void dgels_( )
```

改为

```c++
extern "C" void dgesv_( )
extern "C" void dgels_( )
```

5. 在 Visual Studio 项目设置中添加 BLAS 和 LAPACK 库,
在 `Linker -> General -> Additional Library Directories` 中,
添加 `liblapack.lib` 所在的目录.

在 `Linker -> Input -> Additional Dependencies` 中,
添加 `libblas.lib;liblapack.lib`
(例如, 在我的机器上, 我使用的是 Reference BLAS).

**注意**: 由于 BLAS 库通常提供某些 LAPACK subroutines 的更快版本,
因此 BLAS 库必须列在 LAPACK 库之前.

**注意**: 确保所有 dll(BLAS, LAPACK, MinGW dll)都在系统路径上,
或者将它们复制到 WINDOWS\system32 文件夹中, 否则二进制文件将无法运行.

6. 编译项目并运行生成的可执行文件.
你应该会得到输出结果:  解是

```bash
-0.661082 9.456125 -16.014625
```

序言
本 HOWTO 第三部分将简要解释 `dgesv` 的含义,
以及如何使用适当的参数调用它和其他 LAPACK 子程序.

## 3:关于 LAPACK 子程序的更多信息(示例: dgesv)

在上一节中, 我解释了如何从 C 或 C++ 程序中调用 LAPACK 子程序,
例如 `dgesv_`, 但没有解释 `dgesv` 的含义及其参数.
这就是本 HOWTO 这一部分的目的.
在此过程中我将参考 LAPACK 文档,
并希望向大家展示如何轻松找到合适的 LAPACK 子程序,
并为其创建相应的 C/C++ 函数原型.

### 了解 dgesv

前缀 -- `dge`
从 [LAPACK 命名方案](http://www.netlib.org/lapack/lug/node24.html)中可以清楚地看出:

+ `dgesv` 中的 `d` 表示: double precsion数据
+ `dgesv` 中的 `ge` 表示: 一般非对称矩阵
因此, 我们可以推断出, `dge` 指的是我们拥有的矩阵类型--
包含双精度数据的普通/非对称矩阵

后缀--`sv`
这指的是用于求解线性系统的 driver routine(通俗地说就是求解器)的类型.
驱动程序有两种: 简单驱动程序(后缀为 `sv`)和 *专家*驱动程序(后缀为 `svx`).
请参阅 http://www.netlib.org/lapack/lug/node26.html .

因此 dgesv 是用于包含双精度数据的非对称矩阵的简单 driver routine.

### 子程序参数

从 [dgesv.f](http://www.netlib.org/lapack/double/dgesv.f) 网页上, 我们可以看到 dgesv 子例程有 8 个参数.

+ 第一个参数是 `N`, 整数.
在文档中, 它被标记为输入参数(意思是参数不会被修改,
与 input argument 或 input/output argument 不同).
因此, 在 C/C++ 语言中, 我们可以将参数 1 称作常量整数, 即 `const int`.
但是, 由于在 Fortran 中, 所有参数(*无一例外*)都是通过地址传递的,
因此 `N` 在 C/C++ 中的类型是: `const *int` (参数 2 也是如此).

+ 参数 3 在文档中标记为 input/output double precision array.
在 C/C++ 中, `input/output` 意味着 NOT-constant.
因此, 由于参数是通过引用传递的, 参数 3 的类型是: `double *`.
+ 参数 5 在文档中标记为 `output integer array`.
在 C/C++ 术语中, 这意味着参数不是常量.
因此参数 5 的类型是 `int*`.
参数 8 也是如此, 尽管它不是一个数组
(请记住, 所有 Fortran 参数都是通过地址传递的).

现在应该明白为什么 `dgesv` 的 C/C++ 原型码作:

```c++
extern "C" void dgesv_( const int * , const int * , double * , const int * , int * , double * , const int * , int * );
```

序言:
直接使用 LAPACK 子程序的模式现在应该清楚了.

+ 首先从 [可用驱动程序列表](http://www.netlib.org/lapack/lug/node25.html) 中找到 合适的子程序.
+ 在 [例程索引](http://www.netlib.org/lapack/explore-html/) 中查找 driver.
+ 为驱动程序创建相应的 C/C++ 原型.

## 在 VS C/C++ 项目中使用 LAPACKE 子程序

1. 下载 Visual Studio 解决方案 [LAPACKE示例](https://icl.utk.edu/lapack-for-windows/lapack/LAPACKE_examples.zip)并解压缩.
该解决方案包含您需要的所有 `includes`, `libraries ` 和 `dlls`.
2. 构建解决方案(仅适用于 Win32/Release版本)
3. 打开 cmd 提示符(单击 `运行...` 然后输入 cmd)
4. 使用 `cd` 命令进入 `LAPACKE_examples/Release` 文件夹
5. 您可以运行两个示例: `example_DGESV_rowmajor.exe`
和 `example_ZGESV_rowmajor.exe`(解 写在源文件中)
6. 可选项: 将您的反馈发送至 lapack 团队: lapack@eecs.utk.edu

## 简易 Windows 构建

### 使用 VS 的 Windows 版 LAPACK 3.5.0 的构建说明

要求 Visual Studio, Intel C 和 Fortran 编译器, CMAKE 2.8.12

1. 从 netlib 网站下载 [lapack.tgz](http://netlib.org/lapack/lapack.tgz), 并解压缩.
2. 下载 [CMAKE](http://www.cmake.org/) 并安装到计算机上.
3. 打开 CMAKE
   + 将 lapack-3.5.0 文件夹填入 `source code folder`
   + 在 build 一栏中, 填入一个新的文件夹(最好不要是相同的文件夹)
   + 点击 `configure`, 如果你想在特定位置安装库和包含文件, 请检查 `install` 路径.
   + 选择 Visual Studio 解决方案.
   + 点击 `Specify native compilers`, 并指明 `ifort` 编译器的路径.
    在我的机器上, 路径是 `C:/Program Files (x86)/Intel/Compiler/11.1/048/bin/ia32/icl.exe`.
   + 您可能需要再次点击配置, 直到所有项目都变成白色.
   + 点击 `生成`, 创建 Visual Studio 解决方案, 然后就完成了.
   + 关闭 CMAKE
4. 在 `build` 文件夹中找到 LAPACK Visual Studio 解决方案, 打开它即可.
5. 构建 `ALL_BUILD` 项目, 它将构建解决方案并创建库.
6. 构建 `INSTALL`. 这将把库和 include 放入安装文件夹.
7. 构建 `RUN_TESTS`. 将运行 BLAS 和 LAPACK 测试.

### 用 MinGW 为 Windows 创建 LAPACK 和 LAPACKE 3.5.0 dlls

要求:  MinGW, CMAKE MinGW, CMAKE 2.8.12, VS IDE

1. 从 netlib 网站下载 [lapack.tgz](http://netlib.org/lapack/lapack.tgz), 并解压缩.
2. 下载 CMAKE 并将其安装到您的计算机上.
3. 下载 MinGW 32 bits 或 MinGW-w64, 并将其安装到您的计算机上.
4. 将 GNU runtime 目录放入 PATH, 我在 PATH 中添加了 `C:\MinGW\bin` (MinGW 32 bits)
右键单击计算机图标, 进入属性, 高级系统设置, 环境变量, 查找 PATH 变量并在其当前值前面添加 `C:\MinGW\bin;`.

5. 打开 CMAKE
   + 将 lapack-3.5.0 文件夹填入 `source code folder`
   + 在 build 一栏中, 填入一个新的文件夹(最好不要是相同的文件夹)
   + 点击 `configure`, 如果你想在特定位置安装库和包含文件, 请检查 `install` 路径.
   + 选择 `MinGW Makefiles`.
   + 单击 `Specify native compilers`, 并标明 Mingw 编译器的路径.
    对于 ucrt64, 在我的计算机上, Fortran 编译器是 `C:/MyProgram/msys2/ucrt64/bin/gfortran.exe`,
    C 编译器是 `C:/MyProgram/msys2/ucrt64/bin/gcc.exe`,
    c++ 编译器是 `C:/MyProgram/msys2/ucrt64/bin/g++.exe`.

    对于 x64, 在我的机器上是 `C:/mingw64/bin/x86_64-w64-mingw32-gfortran.exe`,
    C 编译器是 `C:/mingw64/bin/x86_64-w64-mingw32-gcc.exe`.

   + 仅对于 x64编译版本, 添加变量 `CMAKE_SIZEOF_VOID_P`, 并将其设置为 `8`(字符串),
   这将强制 CMAKE 创建 `VCVARSAMD64` 变量, 请参阅[论坛上的帖子](https://icl.utk.edu/lapack-forum/viewtopic.php?f=12&t=4260&p=10623#p10623)
   注意: CMAKE 团队修正了该问题, 因此如果您使用的是 CMAKE 2.8.13 或更高版本, 则不需要此变通方法.
   + 将 `BUILD_SHARED_LIBS` 选项设置为 `ON`.
   + 将 `CMAKE_GNUtoMS` 选项设置为 `ON`.
   + **如果要编译LAPACKE库**, 则将 `LAPACKE` 选项设置为 `ON`.
   + 再次点击 `configure`, 直到一切变为白色
   + 点击 `generate`, 创建 mingw build.
   + 关闭 CMAKE

6. 打开 cmd 提示符(点击 `运行...` 然后输入 cmd)
7. 使用 cd 命令进入编译文件夹
8. 键入 `/ucrt64/bin/mingw32-make.exe`
9. 如果要运行 LAPACK 测试以确保一切正常, 请键入 `/ucrt64/bin/mingw32-make.exe test`
10. 库在 `lib` 文件夹中, dll 在 `bin` 文件夹中.
编译后将为 DLLs 提供 `GNU` 格式和 `MS` 格式的 import libraries.
11. 现在, 您应该可以创建一个使用 MSVC 构建的 C 应用程序, 并直接链接到 MinGW 构建的 LAPACK 动态链接库.
12. 注意: 使用 Microsoft Visual Studio 生成并链接到 MinGW构建 的 LAPACK 动态链接库的 C 应用程序可以运行,
但需要 MinGW 提供的 GNU 运行时动态链接库(需要 `libgfortran-3.dll` 和 `libgcc_s_dw2-1.dll`).
如果您的 PATH 目录中有 GNU runtime 目录, 那么您就可以使用了.
13. 请不要忘记参阅 [LAPACKE 用户指南](http://netlib.org/lapack/lapacke.html).

感谢 CMAKE 伙计们提供此构建版.

可以在 `msys2 ucrt64` 环境中使用下列命令行构建,
并指定安装路径为 `c:/cppLibs/LAPACK-openBLAS-ucrt`

```bash
# 创建 makefile
cmake -B . -S .. -G 'MinGW Makefiles' -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DCMAKE_GNUtoMS=ON -DCMAKE_INSTALL_PREFIX=/c/cppLibs/LAPACK-openBLAS-ucrt --fresh
# make; 构建
/ucrt64/bin/mingw32-make.exe -j

# install; 改变安装路径
# ./configure --prefix=/c/cppLibs/LAPACK-openBLAS-ucrt
# 使用 make DESTDIR=/c/cppLibs/LAPACK-openBLAS-ucrt install 指定安装路径
/ucrt64/bin/mingw32-make.exe install
```

或者直接使用 cmake 构建并安装

```bash
cmake --build . --config Release -j
cmake --install . --config Release --prefix '/c/cppLibs/LAPACK-openBLAS-ucrt'
```

## 支持与反馈

我们正在努力改进 LAPACK Windows 支持, 但用户似乎仍有问题.
我们希望了解我们的工作情况, 以及如何进一步帮助您.
欢迎您在论坛上发帖.

+ 论坛: [/lapack-forum/index.php](/lapack-forum/index.php)
+ 邮件列表: lapack@cs.utk.edu