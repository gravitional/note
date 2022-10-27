# CMake 最低版本号要求
cmake_minimum_required(VERSION 3.24)

# 项目信息
project(crtp)
add_compile_options("/std:c++latest")

# 指定生成目标
add_executable(crtp crtp.cpp)