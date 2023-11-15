# cmake public private interface

[Header Only Library](https://zhuanlan.zhihu.com/p/88166936)
[cmake: target_** 中的 PUBLIC, PRIVATE, INTERFACE](https://zhuanlan.zhihu.com/p/82244559)

## 指令说明

`target_include_directories()`: 指定 `目标` 包含的头文件路径.
[官方文档](https://link.zhihu.com/?target=https%3A//cmake.org/cmake/help/v3.15/command/target_include_directories.html%3Fhighlight%3Dtarget_include_directories)

`target_link_libraries()`: 指定 `目标` 链接的库.
[官方文档](https://link.zhihu.com/?target=https%3A//cmake.org/cmake/help/v3.15/command/target_link_libraries.html%3Fhighlight%3Dtarget_link_libraries)

`target_compile_options()`: 指定 `目标` 的编译选项.
[官方文档](https://link.zhihu.com/?target=https%3A//cmake.org/cmake/help/v3.15/command/target_compile_options.html%23command%3Atarget_compile_options)

`目标` 由 `add_library()` 或 `add_executable()` 生成.

这三个指令类似, 这里以 `target_include_directories()` 为例进行讲解.

## 指令讲解

测试工程目录结构:

```bash
cmake-test/                 工程主目录, main.c 调用 libhello-world.so
├── CMakeLists.txt
├── hello-world             生成 libhello-world.so, 调用 libhello.so 和 libworld.so
│   ├── CMakeLists.txt
│   ├── hello               生成 libhello.so
│   │   ├── CMakeLists.txt
│   │   ├── hello.c
│   │   └── hello.h         libhello.so 对外的头文件
│   ├── hello_world.c
│   ├── hello_world.h       libhello-world.so 对外的头文件
│   └── world               生成 libworld.so
│       ├── CMakeLists.txt
│       ├── world.c
│       └── world.h         libworld.so 对外的头文件
└── main.c
```

调用关系:

```bash
                                 ├────libhello.so
可执行文件────libhello-world.so
                                 ├────libworld.so
```

关键字用法说明:

### PRIVATE: 私有的

生成 libhello-world.so时, 只在 hello_world.c 中包含了 hello.h,
libhello-world.so `对外` 的头文件——hello_world.h 中不包含 hello.h.
而且 main.c 不会调用 hello.c 中的函数, 或者说 main.c 不知道 hello.c 的存在,
那么在 hello-world/CMakeLists.txt 中应该写入:

```cmake
target_link_libraries(hello-world PRIVATE hello)
target_include_directories(hello-world PRIVATE hello)
```

### INTERFACE: 接口

生成 `libhello-world.so` 时,
只在 `libhello-world.so` 对外的头文件 -- `hello_world.h` 中包含 了 `hello.h`,
`hello_world.c` 中不包含 `hello.h`,
即 `libhello-world.so` 不使用 `libhello.so` 提供的功能,
只使用 `hello.h` 中的某些信息, 比如结构体.
但是 `main.c` 需要使用 `libhello.so` 中的功能.
那么在 `hello-world/CMakeLists.txt` 中应该写入:

```cmake
target_link_libraries(hello-world INTERFACE hello)
target_include_directories(hello-world INTERFACE hello)
```

### PUBLIC: 公开的

`PUBLIC = PRIVATE + INTERFACE`.

生成 libhello-world.so 时, 在 hello_world.c 和 hello_world.h 中都包含了 hello.h.
并且 main.c 中也需要使用 libhello.so 提供的功能.
那么在 hello-world/CMakeLists.txt 中应该写入:

```cmake
target_link_libraries(hello-world PUBLIC hello)
```

target_include_directories(hello-world PUBLIC hello)
实际上, 这三个关键字指定的是, `目标文件` 依赖项的使用 `范围`(scope)或者一种 `传递`(propagate).
[官方说明](https://link.zhihu.com/?target=https%3A//cmake.org/cmake/help/v3.15/manual/cmake-buildsystem.7.html%23transitive-usage-requirements)

可执行文件 `xx.exe` 依赖 `libhello-world.so`,
`libhello-world.so` 依赖 `libhello.so` 和 `libworld.so`.

若 `main.c` 不使用 `libhello.so` 的任何功能,
则 `libhello-world.so` 无需将自己的依赖 `libhello.so` 传递给 main.c,
`hello-world/CMakeLists.txt` 中使用 `PRIVATE` 关键字;

若 `main.c` 使用 `libhello.so` 的功能, 但是 `libhello-world.so` 不使用,
则 `hello-world/CMakeLists.txt` 中使用 `INTERFACE` 关键字;

若 `main.c` 和 `libhello-world.so` 都使用 `libhello.so` 的功能,
`hello-world/CMakeLists.txt` 中使用 `PUBLIC` 关键字;

### sss

官网的解释:

假如你要写一个库:

如果你只是在实现(implementation)中 使用依赖`A`(dependency A),
而没有在头文件(header files) 中使用 依赖A
则 结合 `PRIVATE` 关键字使用 `target_link_libraries()` 来声明依赖A.

如果 依赖A 还在 `头文件` 中使用(例如 类继承), 则应该使用 `PUBLIC` 依赖,
如果 依赖A 只在头文件中使用, 而无需在实现中使用, 则使用 `INTERFACE` 依赖.

对于通常的 `.cpp / .h` 组织形式,
即查看当前文件夹下的 `.cpp` 文件和 `.h` 文件 如何使用 `子目录` 编译的链接库:

+ 仅 `.cpp`: `PRIVATE`
+ 仅 `.h` : `INTERFACE`
+ 两者都有 : `PUBLIC`

## include_directories(dir)

`target_include_directories()` 的功能完全可以使用 include_directories() 实现.
但是我还是建议使用 `target_include_directories()`.
为什么?保持清晰!

`include_directories(header-dir)` 是一个全局包含, 向下传递.
什么意思呢?就是说如果某个目录的 CMakeLists.txt 中使用了该指令, 其下所有的子目录默认也包含了header-dir 目录.

上述例子中, 如果在顶层的 cmake-test/CMakeLists.txt 中加入:

```cmake
include_directories(hello-world)
include_directories(hello-world/hello)
include_directories(hello-world/world)
```

那么整个工程的源文件在编译时都会增加:

```bash
-I hello-world -I hello-world/hello -I hello-world/world ...
```

各级子目录中无需使用 `target_include_directories()` 或者 `include_directories()`了.

如果此时查看详细的编译过程(`make VERBOSE=1`)就会发现编译过程是一大坨, 很不舒服.

当然了, 在最终子目录的 `CMakeLists.txt` 文件中, 使用 `include_directories()` 和 `target_include_directories()` 的效果是相同的.

## 目录划分

每一个目录都是一个模块, 目录内部应将对外和对内的头文件进行区分,
由模块的调用者决定模块是否被传递(PRIVATE, INTERFACE, PUBLIC).

## 参考:

https://cmake.org/pipermail/cmake/2016-May/063400.html
https://schneide.blog/2016/04/08/modern-cmake-with-target_link_libraries/
https://stackoverflow.com/questions/26037954/cmake-target-link-libraries-interface-dependencies
https://stackoverflow.com/questions/26243169/cmake-target-include-directories-meaning-of-scope

## add_subdirectory

[add_subdirectory](https://cmake.org/cmake/help/latest/command/add_subdirectory.html#add-subdirectory)

如果提供 `EXCLUDE_FROM_ALL` 参数,
则子目录中的目标默认情况下不会包含在父目录的 `ALL` 目标中,
也不会包含在 IDE 项目文件中.
用户必须明确在子目录中构建目标.

这适用于子目录包含项目中有用但不必要的单独部分(如examples)的情况.
通常情况下, 子目录应包含自己的 `project()` 命令调用,
以便在子目录中生成完整的构建系统(如 Visual Studio IDE 解决方案文件).
请注意, 目标之间的依赖关系会取代这种 exclusion.
如果 parent project 构建的目标依赖于子目录中的目标,
则 dependee 目标将包含在 parent project 构建系统中, 以满足依赖关系.

## add_library

[add_library](https://cmake.org/cmake/help/latest/command/add_library.html#command:add_library)

```cmake
add_library(<name> [STATIC | SHARED | MODULE]
    [EXCLUDE_FROM_ALL]
    [<source>...])
```

添加名为 `<name>` 的目标库, 该目标库将根据 命令调用 中列出的 `source` 构建.
`<name>` 对应 logical target name, 在项目中必须是全局唯一的.
构建的库的实际文件名根据本地平台的约定(如 `lib<name>.a` 或 `<name>.lib`)构建.

3.1 版新增: `add_library` 的源参数可使用 "生成器表达式", 语法为 `$<...>`.
有关可用的表达式, 请参阅 cmake-generator-expressions(7) 手册.
3.11 版新增功能: 如果以后使用 `target_sources()` 添加源文件, 则可省略源文件.
