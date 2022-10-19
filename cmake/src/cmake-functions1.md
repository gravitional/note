# CMake 函数

## INCLUDE

## set()

[分号分隔的列表]: https://cmake.org/cmake/help/latest/manual/cmake-language.7.html#cmake-language-lists
[unset()]: https://cmake.org/cmake/help/latest/command/unset.html#command:unset
[cmake-language(7)]: https://cmake.org/cmake/help/latest/manual/cmake-language.7.html#cmake-language-variables

该命令可以为 `普通变量`, `缓存变量`, `环境变量` 赋值.

将一个普通变量, 缓存变量或环境变量设置为一个给定的值.
参见 [cmake-language(7)] 变量文档, 了解 `普通变量` 和 `缓存项` 的 `作用范围` 和 `相互关系`.

这个命令的签名中, `<value>...` 占位符期望 `零个或多个` 参数.
多个参数将以 [分号分隔的列表][] 形式组合, 以形成要设置的实际变量值.
零参数将导致`普通变量` 回归未赋值 , 见 [unset()][] 命令显式 擦除变量.

所以此处学习SET命令需要分为设置普通变量, 缓存变量以及环境变量三种类别来学习.

### 普通变量(Normal Variable)

```camke
set(<variable> <value>... [PARENT_SCOPE])
```

在 `当前函数` 或 `目录范围` 内设置给定的 `<variable>`,
也可能是当前的 `CMakeLists.txt` 文件,
一个工程可能有多个 `CMakeLists.txt`.

当这个语句中加入 `PARENT_SCOPE` 后,
表示要设置的变量是父目录中的 `CMakeLists.txt` 设置的变量.

比如有如下目录树:

```cpp
├── CMakeLists.txt
└── src
    └── CMakeLists.txt
```

并且在 顶层的 C`MakeLists.txt` 中包含了 `src` 目录: `add_subdirectory(src)`
那么, 顶层的 `CMakeLists.txt` 就是父目录,
如果父目录中有变量 `Bang`,在子目录中可以直接使用(比如用message输出 `Bang`,
值是父目录中设置的值)并且利用set()修改该变量Bang的值,

但是如果希望在出去该子 `CMakeLists.txt` 对该变量做出的修改能够得到保留, 即影响上层目录.
那么就需要在 `set()` 命令中加入 `PARENT_SCOPE` 这个选项.
当然, 如果父目录中本身没有这个变量,
子目录中仍然使用了 `PARENT_SCOPE`, 那么出了这个作用域后, 该变量仍然不会存在.

这里举一个实际的例子:

```cpp
test:
    build
    sub:
        build
        CmakeLists.txt
    CmakeLists.txt
```

我们建立一个项目结构如上:

```cmake
# test/sub/CMakeLists.txt
cmake_minimum_required (VERSION 3.5)
project (subtest)

set (val sub_hello)
set (val par_hello PARENT_SCOPE)

message (">>>>>> in sub level, value = ${val}")
```

```cmake
# test/CMakeLists.txt
cmake_minimum_required (VERSION 3.5)
project (partest)

add_subdirectory (sub)

message (">>> in parent , value = ${val}")
```

执行如下:

```cmake
#在项目test/build下执行cmake ..
>>>>>> in sub level, value = sub_hello
>>> in parent , value = par_hello
#在项目test/sub/build下执行cmake ..
>>>>>> in sub level, value = sub_hello
```

### CACHE变量,Cache Entry

完整语句如下:

```cmake
set(<variable> <value>... CACHE <type> <docstring> [FORCE])
```

+ 首先什么是CACHE变量, 就是在运行cmake的时候,
变量的值可能会被缓存到一份文件里即build命令下的CMakeCache.txt,
当你重新运行cmake的时候, 那些变量会默认使用这个缓存里的值.
这个变量是全局变量, 整个CMake工程都可以使用该变量.

+ 在这个文件里, 只要运行 `cmake ..` 命令, 自动会出现一些值, 比如 CMAKE_INSTALL_PREFIX ,
如果设置set(CMAKE_INSTALL_PREFIX "/usr") ,
虽然CACHE缓存文件里还有这个CMAKE_INSTALL_PREFIX 变量,
但是因为我们显示得设置了一个名为CMAKE_INSTALL_PREFIX 的 `正常变量, `
所以之后使用CMAKE_INSTALL_PREFIX , 值是我们设置的 `正常变量` 的值.

+ 如果加上CACHE关键字, 则设置的这个变量会被写入缓存文件中
(但如果本身缓存文件中有这个变量, 则不会覆盖缓存中的变量).
只有加上FORCE关键字, 这个被写入文件的值会覆盖之前文件中存在的同名变量.

>[Set Cache Entry](https://cmake.org/cmake/help/latest/command/set.html)
>设置给定的缓存<变量>(缓存条目).
>由于缓存条目是为了提供用户可设置的值, 所以默认情况下不会覆盖现有的缓存条目. 使用FORCE选项来覆盖现有条目.

+ 加上 `CACHE` 关键字时, `<type>` 和 `<docstring>` 是必需的.

`<type>` 被 `cmake-gui.exe` 用来选择一个窗口, 让用户设置值. 可以有5种选项.
其中一个是STRING , 弹出提示消息

+ 为 `BOOL`, 则为 `布尔ON/OFF值`.  [cmake-gui(1)][] 提供一个复选框.
+ 为 `FILEPATH`, 则为磁盘上文件的 `路径`.  [cmake-gui(1)][] 提供一个文件对话框.
+ 为 `PATH` , 则为磁盘上目录的路径.  [cmake-gui(1)][] 提供一个文件对话框.
+ 为 `STRING` , 则为一行文字.  [cmake-gui(1)][] 提供文本字段或下拉选择
(如果 STRINGS 设置了缓存条目属性. )
+ 为 `INTERNAL` , 则为一行文字.  [cmake-gui(1)][] 不显示内部条目.
它们可用于在运行之间持久存储变量. 使用此类型暗含FORCE.

比如

```cmake
set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose the type of build." FORCE)
```

+ 这句话, 就是强制在缓存文件中覆盖 `CMAKE_BUILD_TYPE` 这个变量,
将这个变量设置为 `RelWithDebInfo`.
+ 而 `STRING "Choose the type of build."` 参数在使用 `cmake-gui.exe ..` 的时候起作用,
作为鼠标悬停时侯的提示文字
+ 在gui 中的 Value 列会出现下拉框, 供用户选择 `CMAKE_BUILD_TYPE` 变量的值.
`<docstring>`里的一行文字作为提示.

但是这个下拉框里的内容, 需要使用随后的

    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS"Debug" "Release" "MinSizeRel" "RelWithDebInfo")

这个命令来设置. 也就是所谓的设置 `string缓存条目属性`.
界面显示如本节2.2.1

[官方文档](https://cmake.org/cmake/help/latest/command/set.html)
[参考博客](https://blog.csdn.net/Zhanganliu/article/details/99851352)
[ncuneugcj](https://www.cnblogs.com/ncuneugcj/p/9756324.html)
[cmake-gui(1)]: https://cmake.org/cmake/help/latest/manual/cmake-gui.1.html#manual:cmake-gui(1)

## add_compile_definitions

在 3.12 版本中新增.

在编译源文件(source)时添加 预处理程序(preprocessor) 定义.

```cmake
add_compile_definitions(<definition> ...)
```

将预处理器定义添加到 `编译器命令行` 中.

预处理器定义被添加到当前 `CMakeLists` 文件的 [COMPILE_DEFINITIONS][] 目录属性中.
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

## link_directories

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
