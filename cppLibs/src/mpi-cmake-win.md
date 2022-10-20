# Windows MS-MPI Cmake

[FindMPI.cmake and MS-MPI](https://cmake.org/pipermail/cmake/2015-February/059978.html)

我从以下网站 [下载并安装了MS-MPI](http://www.microsoft.com/en-us/download/details.aspx?id=44990)
这个实现不能与 `FindMPI.cmake` 一起工作.

`includes` 和 `libs` 是在以下地方找到的:

    C:/Program Files (x86)/Microsoft SDKs/MPI/

`mpiexec.exe` 在

     C:/Program Files/Microsoft MPI/Bin/

我不得不做了以下改动, 以使其正常工作. 也许你想纳入这些修改到 `FIndMPI.cmake` 中.

```cmake
# 这个项目需要MPI
if(WIN32)
    # 这是为寻找MS-MPI
    # set(_MPI_PREFIX_PATH)
    # list(APPEND _MPI_PREFIX_PATH "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\MPI;Path]/..")
    # 用于运行 MPI程序 的可执行程序
    set(MPIEXEC_EXECUTABLE "C:/Program Files/Microsoft MPI/Bin/mpiexec.exe")

    # 对于构建MPI程序, 使用选定的Visual Studio编译器, 即cl.exe
    # 所以不需要设置特定的MPI编译器
    # set(MPI_CXX_COMPILER "${CMAKE_CXX_COMPILER}")
    # MPI c++ 头文件目录
    set(MPI_CXX_INCLUDE_DIRS "C:/Program Files (x86)/Microsoft SDKs/MPI/Include")

    # 确保选择了正确的库(64位或32位)
    # 在微软MPI 的32位 和64位 库之间做出决定
    if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
        set(MS_MPI_ARCH_DIR x64)
    else()
        set(MS_MPI_ARCH_DIR x86)
    endif()

    # MPI c++ 库文件目录
    set(MPI_CXX_LIBRARIES "C:/Program Files (x86)/Microsoft SDKs/MPI/Lib/${MS_MPI_ARCH_DIR}/msmpi.lib")
    set(MPI_C_INCLUDE_DIRS "${MPI_CXX_INCLUDE_DIRS}")
    set(MPI_C_LIBRARIES "{${MPI_CXX_LIBRARIES}")
    # 在给 `mpiexec`提供运行的处理器数量之前传递给它的标志
    set(MPIEXEC_NUMPROC_FLAG "-np" CACHE STRING
        "Flag used by MPI to specify the number of processes for MPIEXEC; \
        the next option will be the number of processes.")
else()
    find_package(MPI REQUIRED)
endif()
```

其中一些内容取自 `FindMPI.cmake`.

另外在FindMPI.cmake中, 你需要修复以下警告信息:

```bash
CMake Warning (dev) at C:/Program Files(x86)/CMake/share/cmake-3.1/Modules/FindMPI.cmake:163 (if):
    Policy CMP0054 is not set:
    Only interpret if() arguments as variables or keywords when unquoted.
Run "cmake --help-policy CMP0054" for policy details.
Use the cmake_policy command to set the policy and suppress this warning.

Quoted variables like "MSVC" will no longer be dereferenced when the policy is set to NEW.
Since the policy is not set the OLD behavior will be used.
Call Stack (most recent call first):
CMakeLists.txt:5 (find_package)

This warning is for project developers. Use -Wno-dev to suppress it.
```
