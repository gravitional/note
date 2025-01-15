# CMake 函数

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

```cmake
set(<variable> <value>... [PARENT_SCOPE])
# set 的时候变量不需要加 `$`
set(var 'temp string')
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

set(val sub_hello)
set(val par_hello PARENT_SCOPE)

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

+ 首先什么是 CACHE 变量, 就是在运行cmake的时候,
变量的值可能会被缓存到一份文件里即build命令下的CMakeCache.txt,
当你重新运行cmake的时候, 那些变量会默认使用这个缓存里的值.
这个变量是 `全局变量`, 整个 CMake 工程都可以使用该变量.

+ 在这个文件里, 只要运行 `cmake ..` 命令, 会自动出现一些值,
比如 `CMAKE_INSTALL_PREFIX` , 如果设置

    set(CMAKE_INSTALL_PREFIX "/usr")

虽然 `CACHE缓存` 文件里还有这个 `CMAKE_INSTALL_PREFIX` 变量,
但是因为我们显式设置了名为 `CMAKE_INSTALL_PREFIX` 的 `正常变量`,
所以之后引用 `CMAKE_INSTALL_PREFIX`, 指向我们设置的 `正常变量` 的值.

+ 如果加上 `CACHE` 关键字, 则设置的这个变量会被写入 `缓存文件` 中
(但如果本身缓存文件中有这个变量, 则不会覆盖缓存中的变量).
只有加上 `FORCE` 关键字, `缓存文件` 中的值才会在每次运行中被更新.

>[Set Cache Entry](https://cmake.org/cmake/help/latest/command/set.html)
>设置给定的缓存<变量>(缓存条目).
>由于缓存条目是为了提供用户可设置的值, 所以默认情况下不会覆盖现有的缓存条目.
>使用 `FORCE` 选项来覆盖现有条目.

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

## set_target_propertie()

`targets` 可以有`属性`(properties ), 这将影响它们如何被构建(build).

```cmake
set_target_properties(target1 target2 ...
                      PROPERTIES prop1 value1
                      prop2 value2 ...)
```

设置 targets 的属性.
该命令的语法是列出所有你想改变的 targets, 然后提供你接下来想设置的值.
你可以使用任何你想要的 `prop value 对`,
并在以后用 [get_property()][] 或 [get_target_property()][] 命令提取它.

+ 也请参见 [set_property(TARGET)][] 命令.
+ 参见 [Properties on Targets][], 以了解CMake已知的属性列表.

[get_property()]: https://cmake.org/cmake/help/latest/command/get_property.html#command:get_property
[get_target_property()]: https://cmake.org/cmake/help/latest/command/get_target_property.html#command:get_target_property
[set_property(TARGET)]: https://cmake.org/cmake/help/latest/command/set_property.html#command:set_property
[Properties on Targets]: https://cmake.org/cmake/help/latest/manual/cmake-properties.7.html#target-properties
