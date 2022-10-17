# 
set(LIB_NAME "Electrics")
add_compile_definitions()

SetCurrentIncludeDirs(ELECTRICS_INCLUDE_DIR)

include_directories(
    ${ELECTRICS_INCLUDE_DIR}
    ${EMBASE_INCLUDE_DIR}
    ${FINITEELEMENT_INCLUDE_DIR}
    ${COMMON_INCLUDE_DIR}
)

link_directories()

file(GLOB_RECURSE SRC *.h *.cpp CMakeLists.txt)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${SRC})

add_library(${LIB_NAME} SHARED ${SRC})

target_link_libraries(${LIB_NAME} PRIVATE ${COMMON_LIBS} finiteelement embase)

set_target_properties(${LIB_NAME} PROPERTIES OUTPUT_NAME "electrics")

add_dependencies(${LIB_NAME} EMBase)

add_definitions(Solver ${LIB_NAME})
