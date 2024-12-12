# --- https://stackoverflow.com/questions/60211516/programmatically-get-all-targets-in-a-cmake-project

#
# 获取 root 下的所有目录
# _result     : The variable in which to store the resulting directory list
# _root       : The root directory, from which to start.
#
macro(get_directories _result _root)
    file(GLOB_RECURSE dirs RELATIVE ${_root} LIST_DIRECTORIES ON ${_root}/*)

    foreach(dir ${dirs})
        if(IS_DIRECTORY ${dir})
            list(APPEND ${_result} ${dir})
        endif()
    endforeach()
endmacro()

#
# 获取指定目录下的所有 targets
# _result     : The variable in which to store the resulting list of targets.
# _dir        : The directory to query for targets.
#
macro(get_targets_by_directory _result _dir)
    get_property(_target DIRECTORY ${_dir} PROPERTY BUILDSYSTEM_TARGETS)
    set(_result ${_target})
endmacro()

# 获取给定目录下的所有 include path
macro(get_include_dirs_by_directory _result _dir)
    get_property(_target DIRECTORY ${_dir} PROPERTY INCLUDE_DIRECTORIES)
    set(_result ${_target})
endmacro()

#
# 获取 指定 root 目录下的所有 targets
# _result     : The variable in which to store the resulting list of targets.
# _root_dir   : The root project root directory
#
macro(get_all_targets _result _root_dir)
    get_directories(_all_directories ${_root_dir})

    foreach(_dir ${_all_directories})
        get_targets_by_directory(_target ${_dir})

        if(_target)
            list(APPEND ${_result} ${_target})
        endif()
    endforeach()
endmacro()

#*********************** 获取指定 root 目录下的所有 include path
macro(get_all_includes _result _root_dir)
    get_directories(_all_directories ${_root_dir})

    foreach(_dir ${_all_directories})
        get_targets_by_directory(_target ${_dir})

        if(_target)
            list(APPEND ${_result} ${_target})
        endif()
    endforeach()
endmacro()

get_all_includes(my_ALL_INCLUDES ${CMAKE_CURRENT_LIST_DIR})
foreach(path ${my_ALL_INCLUDES})
    message("${path}\n")
endforeach()
