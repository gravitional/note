cmake_minimum_required(VERSION 3.5)
project(compile_flag)

# 检查编译器是否支持编译标志
include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)
CHECK_CXX_COMPILER_FLAG("/std:c++14" COMPILER_SUPPORTS_WIN_CXX14)

if(COMPILER_SUPPORTS_CXX11) # 如果支持 c++11
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
    message(">>>The compiler support `-std=c++11'")
elseif(COMPILER_SUPPORTS_CXX0X) # 如果支持 c++0x
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
    message(">>>The compiler support `-std=c++0x'")
elseif(COMPILER_SUPPORTS_WIN_CXX14) # windows cl.exe 编译器
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++14")
    message(">>>The Windows cl.exe support `/std:c++14'")
else() # 如果都不支持
    message(STATUS ">>>The compiler ${CMAKE_CXX_COMPILER} has no C++11 support. Please use a different C++ compiler.")
endif()

# ###########################################################
# Create an executable
# ###########################################################
# Add an executable with the above sources
# 指定用哪个源文件生成可执行文件
add_executable(compile_flag
    src/main.cpp
)
