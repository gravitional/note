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
