cmake_minimum_required(VERSION 3.5)
project(hello_library)

# ###########################################################
# Create a library
# ###########################################################
# 库的源文件Hello.cpp生成静态库hello_library
add_library(hello_library STATIC
    src/Hello.cpp
)
target_include_directories(hello_library
    PUBLIC
    ${PROJECT_SOURCE_DIR}/include
)

# target_include_directories为一个目标
# (可能是一个库library也可能是可执行文件)添加头文件路径.
# ###########################################################
# Create an executable
# ###########################################################
# Add an executable with the above sources
# 指定用哪个源文件生成可执行文件
add_executable(hello_binary
    src/main.cpp
)

# 链接可执行文件和静态库
target_link_libraries(hello_binary
    PRIVATE
    hello_library
)

# 链接库和包含头文件都有关于scope这三个关键字的用法. 