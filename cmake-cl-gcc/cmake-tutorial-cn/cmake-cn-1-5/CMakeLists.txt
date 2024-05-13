# https://sfumecjf.github.io/cmake-examples-Chinese/01-basic/1.5%20%20build-type.html
cmake_minimum_required(VERSION 3.5)

# 如果没有指定则设置默认编译方式
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    # 在命令行中输出message里的信息
    message("Setting build type to 'RelWithDebInfo' as none was specified.")

    # 不管CACHE里有没有设置过CMAKE_BUILD_TYPE这个变量, 都强制赋值这个值为RelWithDebInfo
    set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose the type of build." FORCE)

    # 当使用cmake-gui的时候, 设置构建级别的四个可选项
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

project(build_type)
add_executable(cmake_examples_build_type main.cpp)

# 命令的具体解释在二  CMake解析中, 这里的注释只说明注释后每一句的作用
set(ENV{var} "sdfa" "extra" "extra2")