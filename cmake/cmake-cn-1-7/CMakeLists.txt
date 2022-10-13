cmake_minimum_required(VERSION 3.5)

# Set the project name
project(third_party_include)

# 输出模块路径变量
message("${CMAKE_MODULE_PATH}")

# find a boost install with the libraries filesystem and system
# 使用库文件系统和系统查找boost install
find_package(Boost 1.46.1 REQUIRED COMPONENTS filesystem system PATHS "C:/cppLibs/Boosts")

# 这是第三方库, 而不是自己生成的静态动态库
# check if boost was found
if(Boost_FOUND)
    message("boost found")
    message("The boost include directory is: " ${Boost_INCLUDE_DIRS})
    message("The boost System directory is: " ${Boost_SYSTEM_LIBRARY})
    message("The boost FileSystem directory is: " ${Boost_FILESYSTEM_LIBRARY})
else()
    message(FATAL_ERROR "Cannot find Boost")
endif()

# Add an executable
add_executable(third_party_include main.cpp)

# link against the boost libraries
target_link_libraries(third_party_include
    PRIVATE
    Boost::filesystem
)