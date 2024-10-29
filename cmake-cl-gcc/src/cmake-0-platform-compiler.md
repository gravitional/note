# cmake 判断平台 和 编译器 种类

[variables-that-describe-the-system](https://cmake.org/cmake/help/latest/manual/cmake-variables.7.html#variables-that-describe-the-system)

## [CMAKE_SYSTEM_NAME](https://cmake.org/cmake/help/latest/variable/CMAKE_SYSTEM_NAME.html)

CMake build 所在的操作系统名称.
操作系统版本请参见 `CMAKE_SYSTEM_VERSION` 变量.
请注意, 在脚本模式下运行时, `CMAKE_SYSTEM_NAME` 默认不设置为任何值, 因为它不会 build 任何内容.

## [LINUX](https://cmake.org/cmake/help/latest/variable/LINUX.html)

New in version 3.25.
当 target 系统是 `Linux` 时设置为 `true`

## [MSVC](https://cmake.org/cmake/help/latest/variable/MSVC.html)

当 编译器 是某个版本的 Microsoft Visual C++ 或
其他模拟 Visual C++ cl 命令行语法的编译器时, 设为 `true`.
另请参阅 `MSVC_VERSION` 变量.

## [MSVC_IDE](https://cmake.org/cmake/help/latest/variable/MSVC_IDE.html)

使用 Microsoft Visual C++ IDE 时为 `true`.
如果目标平台是 Microsoft Visual C++ IDE, 而不是 **命令行编译器**, 则设为 `true`.
注意 该变量仅在 执行编译器检测后可用, 因此它不适用于 toolchain files,
也不适用于 使用 **类 MSVC 编译器** 时的首次 `project()` 或 `enable_language()` 调用之前.

## [MSVC_VERSION](https://cmake.org/cmake/help/latest/variable/MSVC_VERSION.html)

使用的 Microsoft Visual C/C++ 版本(如果有).
如果使用的是模拟 Visual C++ 的编译器,
则该变量将设置为 `_MSC_VER` 预处理器定义 中给出的模拟工具集版本.
已知的版本号有:

1200      = VS  6.0
1300      = VS  7.0
1310      = VS  7.1
1400      = VS  8.0 (v80 toolset)
1500      = VS  9.0 (v90 toolset)
1600      = VS 10.0 (v100 toolset)
1700      = VS 11.0 (v110 toolset)
1800      = VS 12.0 (v120 toolset)
1900      = VS 14.0 (v140 toolset)
1910-1919 = VS 15.0 (v141 toolset)
1920-1929 = VS 16.0 (v142 toolset)
1930-1949 = VS 17.0 (v143 toolset)

另请参见 `CMAKE_<LANG>_COMPILER_VERSION` 和 `MSVC_TOOLSET_VERSION` 变量.

## [MSVC_TOOLSET_VERSION](https://cmake.org/cmake/help/latest/variable/MSVC_TOOLSET_VERSION.html)

3.12 版的新功能.
使用的 Microsoft Visual C/C++ 工具集版本(如果有).
如果使用 MSVC-like, 则根据 `MSVC_VERSION` 变量给出的 编译器版本 设置该变量.

已知的工具集版本号有

80        = VS 2005 (8.0)
90        = VS 2008 (9.0)
100       = VS 2010 (10.0)
110       = VS 2012 (11.0)
120       = VS 2013 (12.0)
140       = VS 2015 (14.0)
141       = VS 2017 (15.0)
142       = VS 2019 (16.0)
143       = VS 2022 (17.0)

比 CMake 所知编译器版本 更新的版本, 只会报CMake已知的最新版本.
另请参阅 `MSVC_VERSION` 变量.

## [MSYS](https://cmake.org/cmake/help/latest/variable/MSYS.html)

New in version 3.14.

True when using the `MSYS Makefiles` generator.

## [UNIX](https://cmake.org/cmake/help/latest/variable/UNIX.html)

当目标系统为 `UNIX` 或类 `UNIX` 系统(如 `APPLE` 和 `CYGWIN`)时, 设为 `True`.
如果需要更具体地了解目标系统, 则应查询 `CMAKE_SYSTEM_NAME` 变量.

## [XCODE](https://cmake.org/cmake/help/latest/variable/XCODE.html)

New in version 3.7.
True when using Xcode generator.

## [APPLE](https://cmake.org/cmake/help/latest/variable/APPLE.html)

当目标系统为 Apple 平台(macOS, iOS, tvOS, visionOS 或 watchOS)时, 设置为 `True`.

## [MINGW](https://cmake.org/cmake/help/latest/variable/MINGW.html)

New in version 3.2.

当至少有 one language 的编译器启用了
`GNU ABI on Windows` (MinGW), 则设置为 `true`.
否则, CMake 不会设置此变量.

## [IOS](https://cmake.org/cmake/help/latest/variable/IOS.html)

New in version 3.14.
Set to `1` when the target system (`CMAKE_SYSTEM_NAME`) is `iOS`.

## [CMAKE_<LANG>_COMPILER_ID](https://cmake.org/cmake/help/latest/variable/CMAKE_LANG_COMPILER_ID.html)

编译器标识字符串.
编译器供应商 独有的 短字符串. 可能的值包括

+ `GNU` GNU Compiler Collection
+ `AppleClang`; Apple Clang
+ `MSVC`    Microsoft Visual Studio

+ `ARMClang`;   ARM Compiler based on Clang
+ `ARMCC`;  ARM Compiler

+ `Clang`   LLVM Clang
+ `Flang`   Classic Flang Fortran Compiler
+ `LLVMFlang`   LLVM Flang Fortran Compiler

+ `G95` G95 Fortran

+ `NVHPC`   NVIDIA HPC Compiler
+ `NVIDIA`  NVIDIA CUDA Compiler

***

+ `Absoft`; Absoft Fortran
+ `ADSP`;   Analog VisualDSP++

+ `Bruce`;  Bruce C Compiler
+ `CCur`    Concurrent Fortran

+ `Cray`    Cray Compiler
+ `CrayClang`   Cray Clang-based Compiler
+ `Embarcadero, Borland`;   Embarcadero

+ `Fujitsu` Fujitsu HPC compiler (Trad mode)
+ `FujitsuClang`    Fujitsu HPC compiler (Clang mode)

+ `GHS` Green Hills Software
+ `HP`  Hewlett-Packard Compiler
+ `IAR` IAR Systems
+ `Intel`   Intel Classic Compiler
+ `IntelLLVM`   Intel LLVM-Based Compiler
+ `LCC` MCST Elbrus C/C++/Fortran Compiler
+ `LFortran`    LFortran Fortran Compiler

+ `OrangeC` OrangeC Compiler
+ `OpenWatcom`  Open Watcom
+ `PGI`; The Portland Group

+ `PathScale`;  PathScale
+ `SDCC`;   Small Device C Compiler
+ `SunPro`; Oracle Developer Studio
+ `Tasking`;    Tasking Compiler Toolsets
+ `TI`; Texas Instruments
+ `TIClang`;    Texas Instruments Clang-based Compilers
+ `TinyCC`; Tiny C Compiler

+ `XL, VisualAge, zOS`  IBM XL
+ `XLClang`     IBM Clang-based XL
+ `IBMClang`    IBM LLVM-based Compiler

不保证对所有 compilers 或 languages 都定义了该变量.
