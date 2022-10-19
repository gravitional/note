# 设置变量名称, 当前 LIB 名称
set(LIB_NAME "Electrics")

# 添加 preprocessor 定义
add_compile_definitions()

# 自定义 macro
SetCurrentIncludeDirs(ELECTRICS_INCLUDE_DIR)

# 添加到本 txt 所有 target 的 INCLUDE_DIRECTORIES 属性, 用于编译器搜索 include 文件
include_directories(
    ${ELECTRICS_INCLUDE_DIR}
    ${EMBASE_INCLUDE_DIR}
    ${FINITEELEMENT_INCLUDE_DIR}
    ${COMMON_INCLUDE_DIR}
)

# linker 链接库的目录
link_directories()

# 递归glob 匹配文件夹下的 头文件 源文件, Cmake 文件
file(GLOB_RECURSE SRC *.h *.cpp CMakeLists.txt)

# 在为 VS 生成的项目文件中, 为源文件定义 group
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${SRC})

# 添加项目要构建的库, SHARED 库即 .dll
add_library(${LIB_NAME} SHARED ${SRC})

# 添加链接库
target_link_libraries(${LIB_NAME} PRIVATE ${COMMON_LIBS} finiteelement embase)

# 设置目标属性, 输出名称
set_target_properties(${LIB_NAME} PROPERTIES OUTPUT_NAME "electrics")

# 在 顶层 targets 之间添加 依赖关系; add_dependencies(T  T的依赖项...)
add_dependencies(${LIB_NAME} EMBase)

# 在编译源文件时添加 `-D` define flags, deprecated
add_definitions(Solver ${LIB_NAME})
