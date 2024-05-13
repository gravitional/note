# 在源代码目录中新建构建目录: mkdir build; cd build;
# 调用 CMake, 根据此CMakeLists.txt 作 configure/generate, 生成本地编译命令:
# cmake ..; cmake --build .
# 也可以调用图形界面: cmake-gui ..
# 打开 VS 项目, 双击 build 文件夹下生成的 xxxx.sln

cmake_minimum_required(VERSION 3.10.2)

# 项目名称, 会根据名称自动设置相关变量的值
project(h5test CXX)

# 设置变量值为 "CXX shared"
set(FIND_HDF_COMPONENTS CXX shared)

# find package, hdf5安装时加入了 PATH 环境变量, 会被 Cmake 定位到
# find_package(HDF5 NAMES "hdf5" COMPONENTS ${FIND_HDF_COMPONENTS} HINTS "C:/Program Files/HDF_Group/HDF5/1.12.2")
find_package(HDF5 NAMES "hdf5" COMPONENTS ${FIND_HDF_COMPONENTS})

# 可以用 file 命令转换 windows 平台的路径分隔符, 但还是要先转义 \, 替换成 \\
# file(TO_CMAKE_PATH "C:\\Program Files\\HDF_Group\\HDF5\\1.12.2" myhdfpath)
# message("<< The converted path: " ${myhdfpath})

# 如果找到了 HDF5 库
if(HDF5_FOUND)
    message(" HDF5 find")

    if(HDF5_shared_CXX_FOUND)
        # 就把 HDF5 lib 目录存入变量 LINK_LIBS
        set(LINK_LIBS ${LINK_LIBS} ${HDF5_CXX_SHARED_LIBRARY})
    endif()
else()
    message(FATAL_ERROR "Can't find  HDF5")
endif()

# 输出 cmake 调试信息
message("<< include: " ${HDF5_INCLUDE_DIR})
message("<< hdf5 include directory: " ${HDF5_INCLUDE_DIR})
message("<< hdf5 c++ shared library dir: " ${HDF5_CXX_SHARED_LIBRARY})
message("<< FIND_HDF_COMPONENTS: " ${FIND_HDF_COMPONENTS})

# 添加要编译的 exe, 名字为 h5test1, 源文件是 Possion1.cpp
add_executable(h5test1 "Possion1.cpp")

# 为编译目标 h5test1 添加 include 目录
# 对应 VS 中项目右键: 属性页 > C/C++ > 所有选项 > 附加选项
# 对应微软 cl 编译器选项 /external:I <path>; 指定外部头文件的位置
target_include_directories(h5test1 PRIVATE ${HDF5_INCLUDE_DIR})

# 为编译目标 h5test1 添加 include 目录,
# 对应 VS 中项目右键: 属性页 > C/C++ > 常规 > 附加包含目录
# 对应微软 cl 编译器选项 /I<dir>; 在<dir> 中搜索 include 文件.
# 如果有多个 include 目录,可以使用
# target_compile_options(h5test1 PRIVATE "/I${HDF5_INCLUDE_DIR}" "/I C:/dir1//include" "/I C:/dir2//include" ...)
target_compile_options(h5test1 PRIVATE "/I${HDF5_INCLUDE_DIR}")

# 添加 链接库 Library 的位置, PUBLIC, PRIVATE 是属性声明
# 见https://sfumecjf.github.io/cmake-examples-Chinese/01-basic/1.3%20%20Static%20Library.html
target_link_libraries(h5test1 PUBLIC ${LINK_LIBS})

# 添加宏
macro(SetCurrentIncludeDirs DIRS)
    set(${DIRS} ${CMAKE_SOURCE_DIR})
    file(GLOB_RECURSE PATHS LIST_DIRECTORIES true *)

    foreach(PATH ${PATHS})
        if(IS_DIRECTORY ${PATH})
            list(APPEND ${DIRS} ${PATH})
        endif()
    endforeach()

    set(${DIRS} ${${DIRS}} CACHE INTERNAL "")
endmacro()
