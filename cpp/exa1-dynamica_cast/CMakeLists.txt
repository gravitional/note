# CMake 最低版本号要求
cmake_minimum_required(VERSION 3.24)

# 项目信息
project(dy-cast)
add_compile_options("/std:c++latest")

# 指定生成目标
add_executable(auto_cast auto_cast.cpp)

# 指定生成目标
add_executable(dy_cast dy_cast.cpp)