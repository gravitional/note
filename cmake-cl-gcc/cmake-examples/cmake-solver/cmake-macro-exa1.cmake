# 收集项目 Top根目录 和 子目录
macro(SetCurrentIncludeDirs DIRS)
    # macro和函数不同, 此处 DIRS 存储变量名称, 例如: ${DIR} -> indir
    set(${DIRS} ${CMAKE_SOURCE_DIR})

    # PATHS 是变量名
    file(GLOB_RECURSE PATHS LIST_DIRECTORIES true *)

    foreach(PATH ${PATHS})
        if(IS_DIRECTORY ${PATH})
            list(APPEND ${DIRS} ${PATH})
        endif()
    endforeach()

    # 此处需要两次解引用才能给出 value
    # ${${DIRS}}->${indir}-> "C:/dir1;C:/dir2;C:/dir2;"
    set(${DIRS} ${${DIRS}} CACHE INTERNAL "")
endmacro()

# 如果存在子项目 CMakeLists.txt，则添加子项目
macro(AddSubmodule MOD_NAME)
    if(EXISTS "{CMAKE_CURRENT_SOURCE_DIR}/${MOD_NAME})/CMakeLists.txt")
        add_subdirectory(${MOD_NAME})
    endif()
endmacro()
