# cmake 属性 property

## 列出 INCLUDE_DIRECTORIES

[listing-include-directories-in-cmake](https://stackoverflow.com/questions/6902149/listing-include-directories-in-cmake)

您可以使用 `get_property` 命令获取目录属性 `INCLUDE_DIRECTORIES` 的值

类似下面这样

```cmake
get_property(dirs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY INCLUDE_DIRECTORIES)
foreach(dir ${dirs})
  message(STATUS "dir='${dir}'")
endforeach()
```

此*目录属性*的值只跟踪同一 `CMakeLists` 文件中之前
出现过的 `include_directories` 命令,
或继承自父 `CMakeLists` 文件中之前出现过的 `include_directories` 命令.
如果你的 `find_package` 和 `include_directories`
命令分散在许多子目录中, 这将成为一个具有挑战性的问题.

如果到了这种地步, 你可以考虑用自己的函数或宏覆盖 `include_directories` 命令, 并自己跟踪传递给它的值.
或者, 只需在每次调用 `include_directories` 命令时,
将它们累加到全局属性或内部缓存变量中即可.

请参阅此处的文档:

[get_property](https://cmake.org/cmake/help/latest/command/get_property.html)
[INCLUDE_DIRECTORIES](https://cmake.org/cmake/help/latest/prop_dir/INCLUDE_DIRECTORIES.html)

## 获取项目中的所有 target

您没有提到 CMake 的版本, 所以我假定是 3.8 或更高版本, 这个解决方案已经过测试.

一种可能的解决方案是遍历项目中的所有子目录,
然后对每个子目录应用 `BUILDSYSTEM_TARGETS`.
为简单易读起见, 我将其分为三个不同的宏.

首先, 我们需要一种递归获取项目中所有子目录的方法.
为此, 我们可以使用 `file(GLOB_RECURSE ...)` 并将 `LIST_DIRECTORIES` 设置为 `ON`:

```cmake
# Get all directories below the specified root directory.
#   _result     : The variable in which to store the resulting directory list
#   _root       : The root directory, from which to start.
#
macro(get_directories _result _root)
    file(GLOB_RECURSE dirs RELATIVE ${_root} LIST_DIRECTORIES ON ${_root}/*)
    foreach(dir ${dirs})
        if(IS_DIRECTORY ${dir})
            list(APPEND ${_result} ${dir})
        endif()
    endforeach()
endmacro()
```

其次, 我们需要一种方法来获取特定目录级别的所有目标.
`DIRECTORY` 包含一个可选参数, 即您希望查询的目录, 这也是运行的关键所在:

```cmake
# Get all targets defined at the specified directory (level).
#   _result     : The variable in which to store the resulting list of targets.
#   _dir        : The directory to query for targets.
#
macro(get_targets_by_directory _result _dir)
    get_property(_target DIRECTORY ${_dir} PROPERTY BUILDSYSTEM_TARGETS)
    set(_result ${_target})
endmacro()
```

第三, 我们需要另一个宏将所有这些连接起来:

```cmake
#
# Get all targets defined below the specified root directory.
#   _result     : The variable in which to store the resulting list of targets.
#   _root_dir   : The root project root directory
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
```

最后, 下面是使用方法:

```cmake
get_all_targets(ALL_TARGETS ${CMAKE_CURRENT_LIST_DIR})
```

ALL_TARGETS 现在应该是一个列表, 其中包含调用者目录级别以下创建的每个目标的名称.
请注意, 它不包括在当前 `CMakeLists.txt` 中创建的任何目标.
为此, 您可以额外调用 `get_targets_by_directory(ALL_TARGETS ${CMAKE_CURRENT_LIST_DIR})`.
