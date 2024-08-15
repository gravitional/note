# cmake 指定 Visual Studio Toolset Selection

[cmake cmdline options](https://cmake.org/cmake/help/latest/manual/cmake.1.html#options)
[CMAKE_GENERATOR_TOOLSET](https://cmake.org/cmake/help/latest/variable/CMAKE_GENERATOR_TOOLSET.html#visual-studio-toolset-selection)

## `CMAKE_GENERATOR_TOOLSET`

用户提供的 原生编译系统工具集规范(Native build system toolset specification).

有些 CMake generators 支持工具集规范, 以告诉原生编译系统如何选择编译器.
如果用户指定了工具集(例如通过 `cmake -T` 选项或通过 `CMAKE_GENERATOR_TOOLSET` 环境变量), 该值就会出现在此变量中.

项目代码不得修改此变量的值.
由 `CMAKE_TOOLCHAIN_FILE` 变量指定的 `工具链文件` 可以初始化 `CMAKE_GENERATOR_TOOLSET`.
一旦某个 `构建树`(build tree)被初始化为该变量的某个特定值,
更改该值将产生未定义的行为.

只有特定的生成器才支持工具集规范:

+ VS 2010 及以上版本的 Visual Studio 生成器
+ Xcode 3.0 及以上版本的 Xcode 生成器
+ The Green Hills MULTI generator

有关允许的 toolset 名称, 请参见 native build system 文档.

## `cmake -T xxx`

+ `-T <toolset-spec>`
generator 的工具集规格(如果支持).

某些 CMake generators 支持工具集规范,
以告诉本地编译系统如何选择编译器.
详情请参阅 `CMAKE_GENERATOR_TOOLSET` 变量.

### Visual Studio 工具集选择

Visual Studio 生成器支持使用以下形式之一指定工具集:

```bash
toolset
toolset[,key=value]*
key=value[,key=value]*
```

+ `toolset`指定工具集名称.
选定的工具集名称在 `CMAKE_VS_PLATFORM_TOOLSET` 变量中提供.
+ `key=value` 对组成一个以逗号分隔的选项列表,
用于指定生成器选择工具集的特定细节. 支持的选项对有

+ `cuda=<version>|<path>`
指定要使用的 CUDA 工具包版本或独立 CUDA 工具包目录的路径.
VS 2010 及以上版本支持.
该版本只能在 **全局安装** 了 `CUDA toolkit VS integration` 的情况下使用.
请参阅 [CMAKE_VS_PLATFORM_TOOLSET_CUDA](https://cmake.org/cmake/help/latest/variable/CMAKE_VS_PLATFORM_TOOLSET_CUDA.html#variable:CMAKE_VS_PLATFORM_TOOLSET_CUDA)
和 [CMAKE_VS_PLATFORM_TOOLSET_CUDA_CUSTOM_DIR](https://cmake.org/cmake/help/latest/variable/CMAKE_VS_PLATFORM_TOOLSET_CUDA_CUSTOM_DIR.html#variable:CMAKE_VS_PLATFORM_TOOLSET_CUDA_CUSTOM_DIR) 变量.

+ `fortran=<compiler>`
3.29 版新增.
在已安装所需的 Visual Studio 集成功能 的编译器中指定要使用的 Fortran 编译器. 该值可以是

+ `ifort`; 英特尔经典 Fortran 编译器.
+ `ifx`; Intel oneAPI Fortran 编译器.

请参阅 [CMAKE_VS_PLATFORM_TOOLSET_FORTRAN](https://cmake.org/cmake/help/latest/variable/CMAKE_VS_PLATFORM_TOOLSET_FORTRAN.html#variable:CMAKE_VS_PLATFORM_TOOLSET_FORTRAN) 变量.

+ `host=<arch>`
将主机工具架构指定为 `x64` 或 `x86`.
VS 2013 及以上版本支持. 请参阅 [CMAKE_VS_PLATFORM_TOOLSET_HOST_ARCHITECTURE](https://cmake.org/cmake/help/latest/variable/CMAKE_VS_PLATFORM_TOOLSET_HOST_ARCHITECTURE.html#variable:CMAKE_VS_PLATFORM_TOOLSET_HOST_ARCHITECTURE) 变量.

+ `version=<version>`
指定要使用的 toolset 版本.
已安装指定工具集的 VS 2017 及以上版本支持.
请参见 [CMAKE_VS_PLATFORM_TOOLSET_VERSION](https://cmake.org/cmake/help/latest/variable/CMAKE_VS_PLATFORM_TOOLSET_VERSION.html#variable:CMAKE_VS_PLATFORM_TOOLSET_VERSION) 变量.

+ `VCTargetsPath=<path>`
为 Visual Studio 项目文件指定替代 `VCTargetsPath` 值.
这样就可以使用 VS 平台扩展配置文件(`.props` 和 `.targets`),
that are not installed with VS.
