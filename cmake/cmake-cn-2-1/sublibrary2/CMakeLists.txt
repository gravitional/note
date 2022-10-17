# Set the project name
project(sublibrary2)
# 添加编译目标:库
add_library(${PROJECT_NAME} INTERFACE)
# 添加别名
add_library(sub::lib2 ALIAS ${PROJECT_NAME})
# 目标包含目录
target_include_directories(${PROJECT_NAME}
    INTERFACE
    ${PROJECT_SOURCE_DIR}/include
)