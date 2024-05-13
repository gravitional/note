# cmake lib interface public private

## target_include_directories时, 添加头文件所在的目录

使用 `target_include_directories()` 添加了一个 `目录`,
这个目录是 `库` 所包含的 `头文件` 的目录, 并设置 `库属性` 为 `PUBLIC`.

```cmake
target_include_directories(hello_library PUBLIC
    ${PROJECT_SOURCE_DIR}/include)
```

This will cause the included directory used in the following places:

使用这个函数后, 这个目录会在以下情况被调用:

+ 编译这个库的时候
  因为这个库 `hello_library` 由 `Hello.cpp` 生成, `Hello.cpp` 中函数的定义在`Hello.h` 中,
  `Hello.h` 在这个 `include` 目录下, 所以显然编译这个库的时候, 这个目录会用到

+ 欲编译其他目标 `A`(`库`或者`exe`), 而 `A` 链接到(依赖于)库 `hello_library`.

### private pubic interface的范围详解

[transitive-usage-requirements](https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#transitive-usage-requirements)

大家如果去搜索, 会发现解释杂乱无章. 大部分解释是这样:

>下面的 `目标`(target), 指当前的 `编译目标`, 在这里就是 `hello_library`.
>`依赖` 指的是编译 `当前目标` 需要的其他 `库`, `文件` 等等, 在这里是 `include/Hello.h`.
>为什么要显式声明 `hello_library` 依赖于 `include/Hello.h` 呢, 因为它们被放在不同文件夹了.
>
>如果 `目标` 的 `头文件`中 包含了 `依赖` 的 `头文件`(`源文件`间接包含), 那么这里就是 `PUBLIC`
>如果 `目标` 仅 `源文件`中 包含了 `依赖` 的 `头文件`, 那么这里就是 `PRIVATE`
>如果 `目标` 的 `头文件` 包含依赖, 但 `源文件` 未包含, 那么这里就是 `INTERFACE`
>
>或者是这样: 当创建动态库时,
>
>如果 `源文件`(例如 .CPP)中包含第三方头文件, 但是 `头文件`(例如 .hpp)中不包含该第三方文件头, 采用 `PRIVATE`.
>如果 `源文件` 和 `头文件` 中都包含该第三方文件头, 采用 `PUBLIC`.
>如果 `头文件` 中包含该第三方文件头, 但是 `源文件`(例如CPP)中不包含, 采用 `INTERFACE`.

我个人认为上面的说法是错误的. 正确理解:
称当前 `编译目标(target)` 为 `目标(库)`,
链接了(依赖于) `目标` 的 `其他目标`(库或者可执行程序)称为 `user`

+ `PRIVATE`; `目录` 将被添加到 `目标(库)` 的 include路径 中.
+ `INTERFACE`; `目录` 不会被添加到 `目标(库)` 的 include路径 中, 而是 `user` 的 include路径 中
+ `PUBLIC`; `目录` 既被添加到 `目标(库)` 的 include路径 中, 同时添加到 `user` 的 include路径 中

也就是说, 根据 `目标(库)` 是否包含这个路径, 以及 `user` 是否包含这个路径, 可以分为三种scope.
有点像 C++ 类继承关系中的 private, protect.

### ​建议!

对于公共的头文件, 最好在 `include` 文件夹下建立子目录.
传递给函数 `target_include_directories()` 的目录,
应该是所有包含目录的根目录, 然后在这个根目录下建立不同的文件夹, 分别写头文件.

这样使用的时候, 不需要写 `${PROJECT_SOURCE_DIR}/include`, 而是直接选择对应的文件夹里对应头文件.
下面是例子: `#include "static/Hello.h"` 而不是 `#include "Hello.h"`
使用此方法意味着在项目中使用多个库时, 头文件名冲突的可能性较小.

## Header only Libraries 只有头文件的库

如果您有一个库被创建为 `仅头文件` 的库,
则 `cmake` 支持 `INTERFACE` 目标, 以允许创建没有 `任何build输出` 的目标.
可以[从here找到更多详细信息](https://cmake.org/cmake/help/v3.4/command/add_library.html#interface-libraries)

```cmake
add_library(${PROJECT_NAME} INTERFACE)
```

创建目标时, 您还可以使用 `INTERFACE` 范围包含该目标的 `目录`.
`INTERFACE` 范围用于指定, 链接到 `此目标` 的其他目标需要依赖 `此目录`,
但编译 `此目标` 本身不需要显式指定此目录(可能本身就在一起).

```cmake
target_include_directories(${PROJECT_NAME}
    INTERFACE ${PROJECT_SOURCE_DIR}/include)
```
