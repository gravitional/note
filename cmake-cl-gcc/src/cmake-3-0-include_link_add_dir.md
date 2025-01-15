# cmake, add_subdirectory, include_directories, link_directories

## add_subdirectory()

[add_subdirectory](https://cmake.org/cmake/help/latest/command/add_subdirectory.html)

在 `build` 中添加一个 `子目录`.

```cmake
add_subdirectory(source_dir [binary_dir] [EXCLUDE_FROM_ALL] [SYSTEM])
```

在构建中添加 `子目录`.
`source_dir` 指定了源 `CMakeLists.txt` 和 `代码文件` 所在的目录.
如果它是 `相对路径`, 它将相对于 `当前目录` 计算(典型用法), 但它也可以是 `绝对路径`.

`binary_dir` 指定了放置 `输出文件` 的目录. 
如果它是相对路径, 它将相对于 `当前目录` 计算, 但它也可以是绝对路径.

如果没有指定 `binary_dir`, 在扩展任何相对路径之前, 将使用 `source_dir` 的值(典型用法).
在当前 `输入文件`(父目录 CMakeList.txt) 的处理进行到 `add_subdirectory`命令时,
CMake 将立即处理指定目录中的 `CMakeLists.txt` 文件,
接着再处理父目录 CMakeList.txt 中`add_subdirectory` 后面的语句.

如果提供了 `EXCLUDE_FROM_ALL` 参数,
那么子目录中的目标将默认不包括在父目录的 `ALL目标` 中,
并且将被排除在 `IDE工程文件` 之外. 用户必须明确地在子目录中构建目标.

这是用来, 当子目录包含项目中 `有用` 但 `不必要` 的独立部分时使用, 如一组 `用例`.
通常情况下, 子目录应该包含它自己的 `project()` 命令调用,
这样将在子目录中生成 `完整的构建系统`(比如VS IDE的解决方案文件).

请注意, `目标间的依赖关系`(inter-target dependencies)可以超越这种排除指定.
如果父项目构建的目标依赖于子目录中的目标,
被依赖的目标将被包含在父项目的 `构建系统` 中以满足 `依赖关系`.

>New in version 3.25:
如果提供了 `SYSTEM` 参数, 子目录的 `SYSTEM 目录属性` 将被设置为真.
这个属性被用来初始化, 在该 `子目录下`创建的每个 target 的 `SYSTEM属性`.
在编译 consumers 时, `SYSTEM` 设置为 `true` 的 target 的 `include目录` 将被视为 `SYSTEM`.

## include()

从 `文件` 或 `模块` 中加载和运行CMake代码.

```cmake
include(<file|module> [OPTIONAL] [RESULT_VARIABLE <var>]
                      [NO_POLICY_SCOPE])
```

从给定的文件中加载并运行 `CMake` 代码.

变量的读写遵从调用者的 scope(dynamic scoping).
如果存在 `OPTIONAL`, 找不到文件不会报错.

如果给出 `RESULT_VARIABLE` 选项,
`<var>`将被设置为 被引入文件的绝对路径, 如果没有找到, 则设置为 `NOTFOUND`.

如果指定了 `模块` 而不是 `文件`,
首先在 `CMAKE_MODULE_PATH` 中搜索名称为 `<modulename>.cmake` 的文件, 然后在 `CMake` 模块目录中搜索.

这有一个例外: 如果调用 `include()` 的文件本身位于CMake内置模块目录中,
那么首先搜索CMake内置模块目录, 然后搜索 `CMAKE_MODULE_PATH`.
另请参见策略 [CMP0017][].

[CMP0017]: https://cmake.org/cmake/help/latest/policy/CMP0017.html#policy:CMP0017

## INCLUDE_DIRECTORIES

`preprocessor` include文件搜索目录的列表.

这个属性指定了, 到目前为止传给 `target_include_directories()` 命令的目录列表.
除了那个命令接收的值之外, 还可以使用 `set_property()` 命令直接在任何目标上设置值.
`target `属性的初始值, 从 `INCLUDE_DIRECTORIES` directory属性值 获得.
`directory ` 和 `target ` 的属性值 都会被 `include_directories()` 命令调整.

这个属性的值被 `生成器` 用来为 `编译器` 设置包含路径.

`相对路径` 不应该被直接添加到这个属性中. 请使用上面的某个命令来处理相对路径.

`INCLUDE_DIRECTORIES` 的内容可以使用 cmake-generator-expressions(7) 的语法 `$<...>`.

## include_directories()

将 `include目录` 添加到 构建 中.

```cmake
include_directories([AFTER|BEFORE] [SYSTEM] dir1 [dir2 ...])
```

将给定的 `目录` 添加到编译器用来搜索 `include文件` 的目录中.
相对路径被解释为相对于 `当前的源代码目录`.

这些 `include文件` 被添加到当前 `CMakeLists` 文件所在directory 的 `INCLUDE_DIRECTORIES` 属性中.
它们也被添加到当前 `CMakeLists` 文件中所有 `target` 的 `INCLUDE_DIRECTORIES` 属性中.
生成器所使用的是 `target` 属性的值.

默认情况下, 指定的目录被 `附加` 到当前的目录列表中.
这个默认行为可以通过设置 `CMAKE_INCLUDE_DIRECTORIES_BEFORE` 为 `ON` 来改变.
通过明确使用 `AFTER` 或 `BEFORE`,
你可以在 `附加` 和 `前缀` 之间进行选择, 与默认值无关.

如果给出 `SYSTEM` 选项, 在某些平台上,
编译器将被告知这些目录作为 `系统包含目录`(system include).
告知这一设置可能会达到一些效果, 比如编译器会跳过警告,
或者这些固定安装的 系统文件 在依赖性计算中不被考虑--见编译器文档.

`include_directories` 的参数可以使用语法为 `$<...>`的 `生成器表达式`.

>注意: 首选 [target_include_directories()][] 命令来为单个目标添加包含目录,
>并可以选择将它们 传播/导出 到依赖者(dependents)

## link_directories()

添加目录, `链接器`(linker) 将在其中寻找 `库`.

```cmake
link_directories([AFTER|BEFORE] directory1 [directory2 ...])
```

这个命令的相对路径被解释为相对于当前源码目录, 见 [CMP0015](https://cmake.org/cmake/help/latest/policy/CMP0015.html#policy:CMP0015).

该命令只适用于调用该命令后创建的 targets.

>3.13版中的新内容:
>这些目录被添加到当前 `CMakeLists.txt` 文件所在目录 `LINK_DIRECTORIES` 属性中,
>根据需要将相对路径转换为绝对路径.
>参见 cmake-buildsystem(7) 手册 以了解更多关于定义构建系统属性的信息.
>
>3.13 版中的新内容:
>默认情况下, 指定的目录被 `附加` 到当前的目录列表中.
>这个默认行为可以通过设置 `CMAKE_LINK_DIRECTORIES_BEFORE` 为 `ON` 来改变.
>通过明确使用 `AFTER` 或 `BEFORE`, 你可以在 `追加` 和 `前缀` 之间进行选择, 与默认值无关.
>
>3.13版新增: link_directories的参数可以使用语法为 `$<...>` 的 `生成器表达式`.
>参见 cmake-generator-expressions(7) 手册中的可用表达式.

+ 注意 这个命令很少需要, 在有其他选择的情况下应该避免使用.
在可能的情况下, 倾向于传递 `库` 的 完整绝对路径, 因为这可以确保正确的库总是被链接.
  + `find_library()` 命令提供了完整的路径, 通常可以直接用于调用 `target_link_libraries()`.
  + 可能需要 `库搜索路径` 的情况包括:
    像Xcode这样的项目生成器, 用户可以在 `build` 时切换 target architecture,
    但不能使用一个库的完整路径, 因为它只提供一个 `architecture`(即它不是一个通用的二进制文件).
  + 库本身可能有其他私有库的依赖关系,
    期望通过 `RPATH` 机制找到, 但一些链接器不能完全解码这些路径
    (例如, 由于存在像 `$ORIGIN` 这样的东西).

+ 如果必须提供一个库的搜索路径, 最好用 `target_link_directories()` 命令,
而不是 `link_directories()` 来实现本地化的效果(localize the effect).
target-specific 的命令还可以控制 `搜索目录` 如何传播到其他 `依赖目标`.

## reference

[target_include_directories()]: https://cmake.org/cmake/help/latest/command/target_include_directories.html#command:target_include_directories