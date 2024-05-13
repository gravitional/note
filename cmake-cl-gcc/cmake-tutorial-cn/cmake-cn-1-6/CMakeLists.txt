cmake_minimum_required(VERSION 3.5)

# 强制设置默认C++编译标志变量为缓存变量,
# 如CMake(五) build type所说, 该缓存变量被定义在文件中,
# 相当于全局变量, 源文件中也可以使用这个变量
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DEX2 -EHsc" CACHE STRING "Set C++ Compiler Flags" FORCE)

# 以上还指定了 C++ 异常处理程序try()catch的展开语义
# https://blog.csdn.net/wu10188/article/details/124706627
# 也可以使用
# set_target_properties(${PROJECT_NAME} PROPERTIES COMPILE_FLAGS "/EHsc")
project(compile_flags)

add_executable(cmake_examples_compile_flags main.cpp)

# 为可执行文件添加私有编译定义
target_compile_definitions(cmake_examples_compile_flags
    PRIVATE EX3
)

# 命令的具体解释在二  CMake解析中, 这里的注释只说明注释后每一句的作用