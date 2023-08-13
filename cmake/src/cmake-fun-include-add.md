# cmake add include 类指令

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

[target_include_directories()]: https://cmake.org/cmake/help/latest/command/target_include_directories.html#command:target_include_directories

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

## source_group

在为 `IDE` 生成的项目文件中, 为源文件定义`group`.
有两种不同的签名来创建 `源文件组`.

```cmake
source_group(<name> [FILES <src>...] [REGULAR_EXPRESSION <regex>])
source_group(TREE <root> [PREFIX <prefix>] [FILES <src>...])
```

定义了一个组, 在项目文件中, 源文件将被放置到其中.
这是为了在 `Visual Studio` 中设置 file tabs.
`group` 被 scoped 于此命令被调用的目录, 并应用到该目录的 targets 的源文件.

选项如下:

+ `TREE`
*3.8版的新功能.*
CMake 将自动检测 ` <src>`中的文件路径中, 创建需要的 源文件组,
以保持 源文件组 的结构与项目中的 实际文件 和 目录结构 相似.
`<src>` 文件的路径将被剪切为 相对于 `<root>`.
如果 `src` 中的路径不是以 `root` 开头, 则该命令会失败.

+ `PREFIX`
*3.8版中的新内容.*
直接位于 `<root>` 路径中的源组和文件, 将被放在 `<prefix>` 源文件组 中.

+ `FILES`
任何明确指定的 `源文件` 将被放置在 `<name>` 组中.
`相对路径` 的解析相对于 `当前源目录`.

+ `REGULAR_EXPRESSION`
任何名称与 `正则表达式` 相匹配的源文件将被放置在 `<name>` 组中.
    + 如果一个 `源文件` 与 `多个组` 相匹配,
    `最后一个` 用 `FILES` 明确列出该文件的组将被优先考虑, 如果有的话.
    + 如果没有组明确列出该文件,
    `最后一个` 与该文件的 `正则表达式` 相匹配的组将被优先考虑.
    + 组的 `<name>` 和 `<prefix>` 参数可以包含 正斜线 或 反斜线 来指定子组.
    反斜线需要被适当地转义.

```cmake
source_group(base/subdir ...)
source_group(outer\\inner ...)
source_group(TREE <root> PREFIX sources\\inc ...)
```

*3.18版新增*: 允许使用 `正斜线`(`/`)来指定子组.

+ 为了向后兼容, 简写的签名

```cmake
source_group(<name> <regex>)
```

相当于

```cmake
source_group(<name> REGULAR_EXPRESSION <regex>)
```

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

[imported target]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#imported-targets
[interface library]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#interface-libraries
[add_custom_target()]: https://cmake.org/cmake/help/latest/command/add_custom_target.html#command:add_custom_target
[add_custom_command()]: https://cmake.org/cmake/help/latest/command/add_custom_command.html#command:add_custom_command
[OBJECT_DEPENDS]: https://cmake.org/cmake/help/latest/prop_sf/OBJECT_DEPENDS.html#prop_sf:OBJECT_DEPENDS

## add_definitions

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
