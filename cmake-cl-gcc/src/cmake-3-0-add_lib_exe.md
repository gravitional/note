# cmake, add_compile_definitions, add_definitions, add_library, add_executable, add_dependencies,

## add_compile_definitions()

在 3.12 版本中新增.

在编译源文件(source)时添加 预处理程序(preprocessor) 定义.

```cmake
add_compile_definitions(<definition> ...)
```

将 `预处理器定义` 添加到 `编译器命令行` 中.

`预处理器定义` 被添加到当前 `CMakeLists` 文件的 [COMPILE_DEFINITIONS][] 目录属性中.
它们也被添加到当前 `CMakeLists` 文件中每个 `target` 的 [COMPILE_DEFINITIONS][] 目标属性中.

定义用 `VAR` 或 `VAR=value` 语法指定.不支持函数式定义.
`CMake` 会自动为本地构建系统正确转义 `value`
(注意CMake语言语法可能需要转义来指定某些值).

`add_compile_definitions` 的参数可以使用语法为 `$<...>` 的 [生成器表达式][].
参见 [cmake-buildsystem(7)][] 手册以了解更多关于定义 buildsystem属性 的信息.

[COMPILE_DEFINITIONS]: https://cmake.org/cmake/help/latest/prop_dir/COMPILE_DEFINITIONS.html#prop_dir:COMPILE_DEFINITIONS
[生成器表达式]: https://cmake.org/cmake/help/latest/manual/cmake-generator-expressions.7.html#manual:cmake-generator-expressions(7)
[cmake-buildsystem(7)]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#manual:cmake-buildsystem(7)

## add_definitions()

在编译源文件时添加 `-D` define flags.

```cmake
add_definitions(-DFOO -DBAR ...)
```

在 `编译器命令行` 中为 `当前目录` 中的 `target` 添加 definitions ,
不管是在 调用此命令之前 还是之后添加的, 以及之后添加的 子目录 中的.
这条命令可以用来添加 任何flags, 但它被设计用来添加预处理器定义(preprocessor definitions).

>注意; 这条命令已经被其他命令所取代.
>
>+ 使用 `add_compile_definitions()` 来添加 预处理程序定义(preprocessor definitions).
>+ 使用 `include_directories()` 来添加 `include目录`.
>+ 使用 `add_compile_options()` 来添加其他选项.

以 `-D` 或 `/D` 开头的标志, 看起来像 `预处理器定义`,
会被自动添加到当前目录的 `COMPILE_DEFINITIONS` directory property 中.
出于向后兼容的考虑, 具有 non-trivial值 的定义可能被留在 flags set 中, 而不是被转换.

参见 [directory][], [target][], [source file][] 的 `COMPILE_DEFINITIONS` 属性的文档,
以了解向 特定范围 和 配置 添加 `预处理程序定义` 的细节.

参见 cmake-buildsystem(7) 手册, 了解更多关于定义构建系统属性的信息.

[directory]: https://cmake.org/cmake/help/latest/prop_dir/COMPILE_DEFINITIONS.html#prop_dir:COMPILE_DEFINITIONS
[target]: https://cmake.org/cmake/help/latest/prop_tgt/COMPILE_DEFINITIONS.html#prop_tgt:COMPILE_DEFINITIONS
[source file]: https://cmake.org/cmake/help/latest/prop_sf/COMPILE_DEFINITIONS.html#prop_sf:COMPILE_DEFINITIONS

## [add_library][def2]

### 普通 library

```cmake
add_library(<name> [<type>] [EXCLUDE_FROM_ALL] <sources>...)
```

添加一个名为 `<name>` 的 library target, 该库将由 命令调用中列出的 源文件 构建. 

可选的 `<type>` 指定要创建的库类型: 

`STATIC`
对象文件的归档文件, 用于链接其他目标库. 

`SHARED`
动态链接库, 可被其他目标程序链接并在运行时加载. 

`MODULE`
一种plugin, 不能被其他目标机链接, 但可在运行时使用类似 `dlopen` 的功能动态加载. 

如果没有给出 `<type>`, 则默认为 `STATIC` 或 `SHARED`, 具体取决于 `BUILD_SHARED_LIBS` 变量的值. 

The options are:

EXCLUDE_FROM_ALL
Set the EXCLUDE_FROM_ALL target property automatically. See documentation of that target property for details.

The <name> corresponds to the logical target name and must be globally unique within a project. The actual file name of the library built is constructed based on conventions of the native platform (such as lib<name>.a or <name>.lib).

Added in version 3.1: Source arguments to add_library may use "generator expressions" with the syntax $<...>. See the cmake-generator-expressions(7) manual for available expressions.

Added in version 3.11: The source files can be omitted if they are added later using target_sources().

For SHARED and MODULE libraries the POSITION_INDEPENDENT_CODE target property is set to ON automatically. A SHARED library may be marked with the FRAMEWORK target property to create an macOS Framework.

Added in version 3.8: A STATIC library may be marked with the FRAMEWORK target property to create a static Framework.

If a library does not export any symbols, it must not be declared as a SHARED library. For example, a Windows resource DLL or a managed C++/CLI DLL that exports no unmanaged symbols would need to be a MODULE library. This is because CMake expects a SHARED library to always have an associated import library on Windows.

By default the library file will be created in the build tree directory corresponding to the source tree directory in which the command was invoked. See documentation of the ARCHIVE_OUTPUT_DIRECTORY, LIBRARY_OUTPUT_DIRECTORY, and RUNTIME_OUTPUT_DIRECTORY target properties to change this location. See documentation of the OUTPUT_NAME target property to change the <name> part of the final file name.

See the cmake-buildsystem(7) manual for more on defining buildsystem properties.

See also HEADER_FILE_ONLY on what to do if some sources are pre-processed, and you want to have the original sources reachable from within IDE.

Changed in version 3.30: On platforms that do not support shared libraries, add_library now fails on calls creating SHARED libraries instead of automatically converting them to STATIC libraries as before. See policy CMP0164.

### [BUILD_SHARED_LIBS][def]

Tell add_library() to default to SHARED libraries, instead of STATIC libraries, when called with no explicit library type.

Calls to add_library() without any explicit library type check the current BUILD_SHARED_LIBS variable value. If it is true, then the default library type is SHARED. Otherwise, the default is STATIC.

For example, the code:

add_library(example ${sources})
behaves as if written

if(BUILD_SHARED_LIBS)
  add_library(example SHARED ${sources})
else()
  add_library(example STATIC ${sources})
endif()
CMake does not define BUILD_SHARED_LIBS by default, but projects often create a cache entry for it using the option() command:

option(BUILD_SHARED_LIBS "Build using shared libraries" ON)
This provides a switch that users can control, e.g., with cmake -D. If adding such an option to the project, do so in the top level CMakeLists.txt file, before any add_library() calls. Note that if bringing external dependencies directly into the build, such as with FetchContent or a direct call to add_subdirectory(), and one of those dependencies has such a call to option(BUILD_SHARED_LIBS ...), the top level project must also call option(BUILD_SHARED_LIBS ...) before bringing in its dependencies. Failure to do so can lead to different behavior between the first and subsequent CMake runs.

## [add_executable][def3]

### 普通可执行文件

```cmake
add_executable(<name> <options>... <sources>...)
```

Add an executable target called <name> to be built from the source files listed in the command invocation.

The options are:

WIN32
Set the WIN32_EXECUTABLE target property automatically. See documentation of that target property for details.

MACOSX_BUNDLE
Set the MACOSX_BUNDLE target property automatically. See documentation of that target property for details.

EXCLUDE_FROM_ALL
Set the EXCLUDE_FROM_ALL target property automatically. See documentation of that target property for details.

The <name> corresponds to the logical target name and must be globally unique within a project. The actual file name of the executable built is constructed based on conventions of the native platform (such as <name>.exe or just <name>).

Added in version 3.1: Source arguments to add_executable may use "generator expressions" with the syntax $<...>. See the cmake-generator-expressions(7) manual for available expressions.

Added in version 3.11: The source files can be omitted if they are added later using target_sources().

By default the executable file will be created in the build tree directory corresponding to the source tree directory in which the command was invoked. See documentation of the RUNTIME_OUTPUT_DIRECTORY target property to change this location. See documentation of the OUTPUT_NAME target property to change the <name> part of the final file name.

See the cmake-buildsystem(7) manual for more on defining buildsystem properties.

See also HEADER_FILE_ONLY on what to do if some sources are pre-processed, and you want to have the original sources reachable from within IDE.

## add_dependencies

在 `顶层目标`(top-level targets)之间添加 依赖关系(dependency ).

```cmake
add_dependencies(<target> [<target-dependency>]...)
```

使一个顶级的 `<target>` 依赖于其他顶级目标, 以确保它们在 `<target>` 之前构建.
顶层目标是由 `add_executable()`, `add_library()` 或 `add_custom_target()` 命令创建的目标
(但不是由 `CMake` 生成的目标, 如 `install`).

添加到 [imported target][] 或 [interface library][] 中的 `依赖关系`
会在其位置上进行转接(followed transitively ), 因为目标本身并不构建.

*3.3版中的新内容*: 允许向 `接口库` 添加依赖关系.

+ 参见 [add_custom_target()][] 和 [add_custom_command()][] 命令的 `DEPENDS` 选项,
用于在 自定义规则 中添加 文件级依赖(file-level dependencies).
+ 参见 [OBJECT_DEPENDS][] 源文件属性, 为 `object文件` 添加文件级依赖关系.

## reference

[imported target]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#imported-targets
[interface library]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#interface-libraries
[add_custom_target()]: https://cmake.org/cmake/help/latest/command/add_custom_target.html#command:add_custom_target
[add_custom_command()]: https://cmake.org/cmake/help/latest/command/add_custom_command.html#command:add_custom_command
[OBJECT_DEPENDS]: https://cmake.org/cmake/help/latest/prop_sf/OBJECT_DEPENDS.html#prop_sf:OBJECT_DEPENDS
[def]: https://cmake.org/cmake/help/latest/variable/BUILD_SHARED_LIBS.html#variable:BUILD_SHARED_LIBS
[def2]: https://cmake.org/cmake/help/latest/command/add_library.html
[def3]: https://cmake.org/cmake/help/latest/command/add_executable.html#command:add_executable