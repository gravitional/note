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

file(GLOB_RECURSE SRC *.h *.cpp CMakeLists.txt)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${SRC})

add_library(${LIB_NAME} SHARED ${SRC})

target_link_libraries(${LIB_NAME} PRIVATE ${COMMON_LIBS} finiteelement embase)

set_target_properties(${LIB_NAME} PROPERTIES OUTPUT_NAME "electrics")

add_dependencies(${LIB_NAME} EMBase)

add_definitions(Solver ${LIB_NAME})
