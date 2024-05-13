cmake_minimum_required(VERSION 3.5)

# 项目名称
project(subprojects)

# 添加子目录
add_subdirectory(sublibrary1)
add_subdirectory(sublibrary2)
add_subdirectory(subbinary)
message("//: " ${CMAKE_CURRENT_SOURCE_DIR})