cmake_minimum_required(VERSION 3.5)
project(hello_library)

# ###########################################################
# Create a library
# ###########################################################
# 根据Hello.cpp生成动态库
add_library(hello_library SHARED
    src/Hello.cpp
)

# 给动态库 hello_library 起一个别的名字hello::library
add_library(hello::library ALIAS hello_library)

# 为这个库目标, 添加头文件路径, PUBLIC表示包含了这个库的目标也会包含这个路径
target_include_directories(hello_library
    PUBLIC
    ${PROJECT_SOURCE_DIR}/include
)

# ! 添加编译属性, 在 windows 上, 需要指定 EXPORTS 导出标志
# add_compile_definitions(Hello_EXPORTS)

# 如果没有指定则设置默认编译方式
if(NOT Hello_EXPORTS)
    # 在命令行中输出message里的信息
    message("Setting  Export Flag Hello_EXPORTS.")

    # 不管CACHE里有没有设置过 Hello_EXPORTS 这个变量, 都强制赋值这个值为RelWithDebInfo
    set(Hello_EXPORTS True CACHE STRING "Set Hello_EXPORTS to True" FORCE)
endif()

# ###########################################################
# Create an executable
# ###########################################################
# 根据main.cpp生成可执行文件
add_executable(hello_binary
    src/main.cpp
)

# 链接库和可执行文件, 使用的是这个库的别名. PRIVATE 表示
target_link_libraries(hello_binary
    PRIVATE
    hello::library
)