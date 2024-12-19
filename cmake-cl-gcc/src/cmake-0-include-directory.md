# cmake 包含目录, include directories, 自动包含依赖目录

[CMake doesn't include header directory of submodule A within submodule B](https://stackoverflow.com/questions/38022700/cmake-doesnt-include-header-directory-of-submodule-a-within-submodule-b)

如果要表示 `include`目录 `subprojectA/include` 是 library `subprojectA` 的接口(interface),
请使用 `target_include_directories` 命令将此 property 附加到 target:

**subprojectA/CMakeLists.txt**:

```cmake
project(SubProjectA)
add_library(subprojectA STATIC src/libraryA.cpp)
# PUBLIC 将添加到两中属性:
#     1) 用于 compile 本lib 的 include 目录,
#     2) 用于 本 lib's interface 的 include 目录
target_include_directories(subprojectA PUBLIC include)
```

因此, 任何与 `subprojectA` 链接的 executable(或其他library)都将自动包含此 `include` 目录:

**subprojectB/CMakeLists.txt**:

```cmake
project(SubProjectB)
add_executable(mainBinary src/mainB.cpp)
target_link_libraries(mainBinary subprojectA)
```

当然, 要正确使用最后一条命令, 需要先处理包含library的目录, 然后再处理包含 executable 的目录:

**CMakeLists.txt**:

```cmake
project(Project)
add_subdirectory(subprojectA)
add_subdirectory(subprojectB)
```