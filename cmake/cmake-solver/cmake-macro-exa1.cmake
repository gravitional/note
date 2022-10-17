# 添加宏
macro(SetCurrentIncludeDirs DIRS)
    set(${DIRS} ${CMAKE_SOURCE_DIR})
    file(GLOB_RECURSE PATHS LIST_DIRECTORIES true *)

    foreach(PATH ${PATHS})
        if(IS_DIRECTORY ${PATH})
            list(APPEND ${DIRS} ${PATH})
        endif()
    endforeach()

    set(${DIRS} ${${DIRS}} CACHE INTERNAL "")
endmacro()

# 添加宏
macro(AddSubmodule MOD_NAME)
    if(EXISTS "{CMAKE_CURRENT_SOURCE_DIR}/${MOD_NAME})/CMakeLists.txt")
        add_subdirectory(${MOD_NAME})
    endif()
endmacro()
