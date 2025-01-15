# cmake 导入库, imported lib, 导入第三方库, windows "导入库" import lib, 预先编译的库

## 概念

+ `x` 的 `denpendency` A B C: `x` 依赖 denpendency A B C 来编译.
+ `A` 的 `dependent` b c d... : b c d 要依赖 `A` 来编译.

+ `imported lib`: 导入预先编译好的库, 通常来自于软件开发的上游, 例如使用别人提供的 `xxx.dll`.
+ `import lib` or `implLib`: windows, AIX or Apple 特有的,
  伴随 `.dll` 的 `.lib` 文件, 相当于 `.dll` 的接口说明, 好处是 link 时可以随便指定 dll 的次序(顺序无关).

+ `A` 的 `usage requirements`; 别人使用 `A` 时, 需要设置的一些项目属性, 例如 编译选项 和 宏定义

## [imported libraries][def11], 导入预先编译的库

```bash
add_library(<name> <type> IMPORTED [GLOBAL])
```

添加名为 `<name>` 的 [IMPORTED library target][].
该 target name 可以像项目中构建的任何目标一样被引用,
但默认情况下只能在创建该目标的目录及其下级目录中可见.
添加 Global 以使全局可见.

`<type>` 必须是以下类型之一

### `STATIC`, `SHARED`, `MODULE`, `UNKNOWN`

引用位于 项目外部 的库文件.
`IMPORTED_LOCATION` 目标属性,
或其 per-configuration 的变体 `IMPORTED_LOCATION_<CONFIG>`
指定了主库文件在磁盘上的位置:

+ 对于大多数非 `Windows` 平台上的 `SHARED`,
  main library file 是 linkers 和 dynamic loaders 使用的 `.so` 或 `.dylib` 文件.
  可以是用 `;` 分隔的字符串(cmake 列表), 表示多个文件.

  + 如果引用的库文件有 `SONAME`(或在 macOS 上有以 `@rpath/` 开头的 `LC_ID_DYLIB`),
    则应在 `IMPORTED_SONAME` 目标属性中设置该字段的值.
  + 如果引用的库文件没有 `SONAME`, 但平台支持 `SONAME`, 则应设置 `IMPORTED_NO_SONAME` 目标属性.

+ 对于 Windows 上的共享库, `IMPORTED_IMPLIB` 目标属性,
  或其按配置的变体 `IMPORTED_IMPLIB_<CONFIG>`,
  指定 DLL 导入库文件(`.lib` 或 `.dll.a`)在磁盘上的位置;
  而 `IMPORTED_LOCATION` 是 `.dll` 运行时库的位置,
  可选, 但 `TARGET_RUNTIME_DLLS` 生成器表达式需要.

其它 usage requirements 可在 `INTERFACE_*` 属性中指定.

`UNKNOWN` 库类型通常只用于 [Find Modules][def8] 的实现.
它允许使用 imported library 的路径(通常使用 `find_library()`命令查找),
而无需知道它是什么类型的库.
在 Windows 系统中, `static library` 和 DLL 的 接口库(`import library`)
具有相同的文件扩展名, 这一点尤其有用.

### `OBJECT`

引用位于项目外部的一组 `object files`.
`IMPORTED_OBJECTS` 目标属性(或其按配置的变体 `IMPORTED_OBJECTS_<CONFIG>`)指定了磁盘上对象文件的位置.
其他使用要求可在 `INTERFACE_*` 属性中指定.

### `INTERFACE`

不引用磁盘上的任何库或对象文件, 但可在 `INTERFACE_*` 属性中指定使用要求.
可用于 header-only 的外部依赖,
也就是使用的时候只需 `#include` 头文件的那种 模板库.

### 选项包括

`GLOBAL` 使目标名称全局可见.

对于 `imported targets`, 不会生成构建规则, 且目标属性 `IMPORTED` 为 `True`.
导入的库可以方便地从 `target_link_libraries()` 等命令中引用.

有关 imported library 的详细信息,
可通过设置 名称以 `IMPORTED_` 和 `INTERFACE_` 开头的属性来指定.
有关详细信息, 请参阅此类属性的文档.

[IMPORTED library target]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#imported-targets

## [Imported Targets][], 预先编译的dll

[IMPORTED][]目标 表示**预先存在**的依赖.
通常这类目标是由上游软件包定义的, 应视为不可变的.
在声明 `IMPORTED` 目标后, 可以使用
`target_compile_definitions()`, `target_include_directories()`,
`target_compile_options()` 或 `target_link_libraries()`
等常用命令来调整其目标属性, 就像使用其他普通目标一样.

IMPORTED 目标与二进制目标一样, 也有 usage requirement 属性,
如 `INTERFACE_INCLUDE_DIRECTORIES`, `INTERFACE_COMPILE_DEFINITIONS`,
`INTERFACE_COMPILE_OPTIONS`, `INTERFACE_LINK_LIBRARIES` 和
`INTERFACE_POSITION_INDEPENDENT_CODE`.
>也就是暴露给 使用方 的接口, 别人链接这个库的时候, 需要包含这些 include目录 或者 链接这些libraries.

可以从 IMPORTED 目标中读取 `LOCATION`, 但很少有这样做的理由.
`add_custom_command()` 等命令可以透明地使用 `IMPORTED EXECUTABLE`目标 作为 `COMMAND` 可执行文件.

+ `IMPORTED` 目标的定义范围是 它被定义的目录.
它可以从子目录访问和使用, 但不能从父目录或同级目录访问和使用.
其作用域类似于 cmake变量 的作用域.
+ 也可以定义一个 `GLOBAL` IMPORTED 目标, 在 build系统 中可以全局访问.

请参阅 [cmake-packages(7)][def2] 手册, 了解使用 IMPORTED 目标创建软件包的更多信息.

### cmake属性[IMPORTED](https://cmake.org/cmake/help/latest/prop_tgt/IMPORTED.html#prop_tgt:IMPORTED)

以只读方式显示目标是否为 `IMPORTED`.
如果在使用 `add_executable()` 或 `add_library()` 时指定了选项 `IMPORTED`, 则该属性的布尔值为 `True`.
对于**在项目内部**构建的目标, 该值为 `False`.

### [IMPORTED_LOCATION][def3], 导入位置

磁盘上 `IMPORTED` target 的 main file 的完整路径.

将其设置为 IMPORTED target file(而非所在目录) 在磁盘上的位置.

+ 对于 可执行文件, 这是 可执行文件 的位置.
+ 对于 静态库和模块, 这是 library 或 module 的位置.
+ 对于 非DLL平台上的共享库, 这是 shared library 的位置.
+ 对于 macOS上的 application bundles, 这是捆绑包文件夹内 `Contents/MacOS` 中可执行文件的位置.
+ 对于 macOS上的 frameworks, 这是框架文件夹内 library file symlink 的位置.
+ 对于 DLL, 这是 `.dll`文件 的位置.
+ 对于未知库, 这是 需要链接的文件的位置.
+ 对于非导入目标将忽略.

在 3.28 版中添加:  对于 Apple 平台上的普通frameworks,
这可能是 `.framework`文件夹 本身的位置.
对于 XCFrameworks, 它可以是 `.xcframework` 文件夹的位置,
在这种情况下, 任何与之链接的目标都将获得所选库的 `Headers` 目录作为 usage requirement.

对于 特定的配置 `<CONFIG>`,(Debug, Release),
可以用 `IMPORTED_LOCATION_<CONFIG>` 目标属性 覆盖一般属性 `IMPORTED_LOCATION`.
此外, [MAP_IMPORTED_CONFIG_<CONFIG>][def4]
目标属性可用于在 project配置 和 imported目标配置之间进行映射.

如果没有设置这些属性, 则可以选择 [IMPORTED_CONFIGURATIONS][def5]
目标属性中列出的任何其他 config 的名称,
并使用其 [IMPORTED_LOCATION_<CONFIG>][def6] 值.

要获取 imported target 的位置, 请读取 [LOCATION][def] 或 [LOCATION_<CONFIG>][def] 属性之一.

对于带 import libraries 的平台(如 Windows, AIX 或 Apple), 另请参阅 [IMPORTED_IMPLIB][def9].

### [LOCATION][def], 目标位置

目标在磁盘上的只读位置.
对于 imported target, 此只读属性返回目标提供的,
未指定配置 `<CONFIG>` 的 `LOCATION_<CONFIG>` 属性的值.

对于非导入目标, 提供此属性是为了与 CMake 2.4 及以下版本兼容.
它旨在获取可执行目标的输出文件的位置, 以便在 `add_custom_command()` 中使用.
路径可能包含特定于build系统的部分, 该部分在build时被替换为build的配置(如 VS 中的 `$(ConfigurationName)`).
在 CMake 2.6 及以上版本中,
`add_custom_command()` 会自动识别 `COMMAND` 和 `DEPENDS` 选项中的 target name 并计算目标位置.
在 CMake 2.8.4 及以上版本中, `add_custom_command()` 可识别 [生成器表达式][def10],
可以在 自定义命令的任意位置 引用目标位置. 因此, 创建自定义命令时不需要此属性.

读取此属性后, 请勿设置会影响 target位置的属性.
这些属性包括名称匹配 `(RUNTIME|LIBRARY|ARCHIVE)_OUTPUT_(NAME|DIRECTORY)(_<CONFIG>)?`,
`(IMPLIB_)?(PREFIX|SUFFIX)` 或 "LINKER_LANGUAGE "的属性.
Failure to follow this rule is not diagnosed and leaves the location of the target undefined.

### [IMPORTED_IMPLIB][def9], win接口库位置

`IMPORTED` target 的 `import library`(implLib) 的 完整路径.

该属性可设置为

+ 在 DLL 平台上, 可设置为 DLL 的 `.lib` 部分的位置.
+ 已在 3.16 版中添加: 在 `AIX` 上, 为 可执行文件创建的,
用于 export symbols 的 import file(如 `.imp`),
请参阅 [ENABLE_EXPORTS][def12] 目标属性.

+ 在 3.27 版中添加: 在 Apple 平台上,
为 shared libraries 或frameworks 创建的 import file(如 `.tbd`),
请参阅 [ENABLE_EXPORTS][def12] 目标属性.
对于框架而言, 这是框架文件夹内 `.tbd`文件符号链接 的位置.

+ 已在 3.28 版中添加:  在非 DLL 平台上, shared library 的位置.
其位置将不会作为运行时搜索路径添加到链接该库的依赖库中.
如果设置时未同时指定 [IMPORTED_LOCATION][def3], 则该库将被视为 stub,
它的 location 将不会作为 runtime search path 添加到 主链接它的 目标中.

+ 3.28 版中的修改: 如果导入的目标是 Apple framework 或 XCFramework,
首选的做法是将 `IMPORTED_LOCATION` 设为 `.framework` 或 `.xcframework` 目录.
CMake 将自动在该框架或 `XCFramework` 中找到相关的 `.tbd` 文件, 而无需设置 `IMPORTED_IMPLIB`.

对于给定的配置 `<CONFIG>`,
可以使用 `IMPORTED_IMPLIB_<CONFIG>` 目标属性 `IMPORTED_IMPLIB `.
此外, `MAP_IMPORTED_CONFIG_<CONFIG>` 目标属性可用于在 项目配置 和 imported target 配置之间进行映射.

如果没有设置这些属性, 将会选择 `IMPORTED_CONFIGURATIONS` 目标属性 中列出的任何其他 配置的名称,
并使用其 `IMPORTED_IMPLIB_<CONFIG>` 值.

对于 non-imported targets, 该属性将被忽略.

### [INTERFACE_INCLUDE_DIRECTORIES][def13]

使用 library 所需的 public include 目录列表.

Targets 可填充此属性, 以发布头文件目录,
当用户方面向此 target 的头文件编译时, 需要包含这些 include 目录.
`target_include_directories()` 命令使用
`PUBLIC` 和 `INTERFACE` 关键字 对应的值 填充该属性.
项目也可以直接获取和设置 该属性.

当使用 `target_link_libraries()` 指定 target dependencies 时,
CMake 将从所有 dependencies 中读取此属性, 以确定 客户方 的 build属性.

`INTERFACE_INCLUDE_DIRECTORIES` 的内容可以使用 "generator expressions", 语法为 `$<...>`.
有关可用的表达式, 请参见 [cmake-generator-expressions(7)][def10] 手册.
有关定义 buildsystem 属性的更多信息, 请参阅 cmake-buildsystem(7) 手册.

Include directories usage requirements 通常在 build-tree 和 install-tree 之间有所不同.
`BUILD_INTERFACE` 和 `INSTALL_INTERFACE` 生成器表达式 可用于描述基于使用位置的不同使用要求.
在 `INSTALL_INTERFACE` 表达式中, 允许使用相对路径, 并且相对于**安装前缀**进行解释. 例如

```cmake
target_include_directories(mylib INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/mylib>
  $<INSTALL_INTERFACE:include/mylib>  # <prefix>/include/mylib
)
```

### [INTERFACE_LINK_LIBRARIES][def14],

List **public interface libraries** for a library.
库的 公共接口库 列表,
**也就是伴随 `.dll` 的 `.lib` 文件**

该属性包含 transitive link dependencies 列表.
当使用 `target_link_libraries()` 命令,
把 target `A` 作为依赖, 链进 target `B` 时,
`A` 的 `INTERFACE_LINK_LIBRARIES`中列出的库(以及递归的 link interface libraries) 将提供给 target `B`.
如果策略 [CMP0022][def16] 为 `OLD` 或未设置,
该属性将被 `LINK_INTERFACE_LIBRARIES` 或 `LINK_INTERFACE_LIBRARIES_<CONFIG>` 属性覆盖.

generators 在为 dependent target(用户方)构建 链接规则 时, 会使用该属性的值.

+ dependent target 的直接 link dependencies 首先被链接,
在其 [LINK_LIBRARIES][def15] 目标属性中指定,
+ 然后是直接dependencies 的 `INTERFACE_LINK_LIBRARIES` 属性的 transitive closure 中的间接依赖关系.
参见规范 [CMP0022][def16].

`INTERFACE_LINK_LIBRARIES` 的内容可以使用 "生成器表达式", 语法为 `$<...>`.
有关可用的表达式, 请参见 cmake-generator-expressions(7) 手册.
有关如何定义 buildsystem 属性的更多信息, 请参阅 cmake-buildsystem(7) 手册.

>注意: 对 `target_link_libraries(<target> ...)` 的调用可能会更新 `<target>` 上的此属性.
>如果 `<target>`创建在 `A` 目录, 而调用`target_link_libraries`在 `B`目录(也就是不在同一目录),
>那么 `target_link_libraries()` 将以 `::@(directory-id);...;::@` 的形式包装每个条目,
>其中 `::@` 是字面值, `(directory-id)` 是未定的.
>
>这就告诉 generators, 涉及的库 必须在 caller 的作用域中查找,
>而不是在创建 `<target>` 的作用域中查找.
>`install(EXPORT)` 和 `export()` 命令在导出时会 stripped Valid directory ids.

`INTERFACE_LINK_LIBRARIES` 为 target 的 dependents 添加了 transitive link dependencies.
在高级用例中, 我们可以使用 `INTERFACE_LINK_LIBRARIES_DIRECT` 和
`INTERFACE_LINK_LIBRARIES_DIRECT_EXCLUDE` 目标属性,
更新 target's dependents 的 direct link dependencies.

## 创建可重新定位的软件包

请注意, 不宜在目标的 `INTERFACE_LINK_LIBRARIES` 中填入 dependencies 的绝对路径.
这将在已安装的软件包中 硬编码 dependencies 的文件路径,
也就是 编译平台(电脑) 上的文件路径.

请参阅 cmake-packages(7) 手册的 [Creating Relocatable Packages][def17] 部分,
了解在创建用于 redistribution 的软件包时,
在指定 usage requirements 时必须注意的其他事项.

## [$<TARGET_RUNTIME_DLLS:tgt>][def7], 依赖的dll

在 3.21 版中添加.

target runtime 依赖的 DLL 列表.
这是由 target 的 transitive dependencies 中所有 SHARED targets 的位置决定的.
如果只需要 DLL 的目录,
请参阅 `TARGET_RUNTIME_DLL_DIRS` 生成器表达式.
在 可执行文件, 共享库 和 MODULE库 以外的目标上使用此 生成器表达式 会导致错误.
在 非DLL平台上, 此表达式的值总是 空字符串.

在 `POST_BUILD` 自定义命令中, 可以使用 `cmake -E copy -t` 命令
将目标所依赖的所有 DLL 复制到其输出目录中. 例如

```cmake
find_package(foo CONFIG REQUIRED) # package generated by install(EXPORT)

add_executable(exe main.c)
target_link_libraries(exe PRIVATE foo::foo foo::bar)
add_custom_command(TARGET exe POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy -t $<TARGET_FILE_DIR:exe> $<TARGET_RUNTIME_DLLS:exe>
  COMMAND_EXPAND_LISTS
)
```

注意 只有当目标知道其 `.dll` 文件的位置时, 才支持 Imported Targets.
imported SHARED library 必须将 `IMPORTED_LOCATION` 设置为其 `.dll` 文件的位置.
有关详情, 请参阅 `add_library` imported libraries 部分.
许多 Find 模块会生成 `UNKNOWN` 类型的导入目标, 因此会被忽略.
在支持运行时路径 (`RPATH`) 的平台上, 请参考 `INSTALL_RPATH` 目标属性.
在 Apple 平台上, 请参考 `INSTALL_NAME_DIR` 目标属性.

## reference

[def]: https://cmake.org/cmake/help/latest/prop_tgt/LOCATION.html#prop_tgt:LOCATION
[def2]: https://cmake.org/cmake/help/latest/manual/cmake-packages.7.html#manual:cmake-packages(7)
[IMPORTED]: https://cmake.org/cmake/help/latest/prop_tgt/IMPORTED.html#prop_tgt:IMPORTED
[Imported Targets]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#imported-targets
[def3]: https://cmake.org/cmake/help/latest/prop_tgt/IMPORTED_LOCATION.html#prop_tgt:IMPORTED_LOCATION
[def4]: https://cmake.org/cmake/help/latest/prop_tgt/MAP_IMPORTED_CONFIG_CONFIG.html#prop_tgt:MAP_IMPORTED_CONFIG_%3CCONFIG%3E
[def5]: https://cmake.org/cmake/help/latest/prop_tgt/IMPORTED_CONFIGURATIONS.html#prop_tgt:IMPORTED_CONFIGURATIONS
[def6]: https://cmake.org/cmake/help/latest/prop_tgt/IMPORTED_LOCATION_CONFIG.html#prop_tgt:IMPORTED_LOCATION_%3CCONFIG%3E
[def9]: https://cmake.org/cmake/help/latest/prop_tgt/IMPORTED_IMPLIB.html#prop_tgt:IMPORTED_IMPLIB
[def10]: https://cmake.org/cmake/help/latest/manual/cmake-generator-expressions.7.html#manual:cmake-generator-expressions(7)
[def11]: https://cmake.org/cmake/help/latest/command/add_library.html#imported-libraries
[def12]: https://cmake.org/cmake/help/latest/prop_tgt/ENABLE_EXPORTS.html#prop_tgt:ENABLE_EXPORTS
[def13]: https://cmake.org/cmake/help/latest/prop_tgt/INTERFACE_INCLUDE_DIRECTORIES.html#prop_tgt:INTERFACE_INCLUDE_DIRECTORIES
[def7]: https://cmake.org/cmake/help/latest/manual/cmake-generator-expressions.7.html#genex:TARGET_RUNTIME_DLLS
[def8]: https://cmake.org/cmake/help/latest/manual/cmake-developer.7.html#find-modules
[def14]: https://cmake.org/cmake/help/latest/prop_tgt/INTERFACE_LINK_LIBRARIES.html#prop_tgt:INTERFACE_LINK_LIBRARIES
[def15]: https://cmake.org/cmake/help/latest/prop_tgt/LINK_LIBRARIES.html#prop_tgt:LINK_LIBRARIES
[def16]: https://cmake.org/cmake/help/latest/policy/CMP0022.html#policy:CMP0022
[def17]: https://cmake.org/cmake/help/latest/manual/cmake-packages.7.html#creating-relocatable-packages