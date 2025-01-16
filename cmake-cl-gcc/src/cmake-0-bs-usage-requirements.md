# [Target Usage Requirements][def]

target 的 usage requirements 是传播给 consumers(用户方)的设置,
用户方 通过 `target_link_libraries()` 链接到 target,
以便正确 compile 和 link with target.
它们由 transitive compile and link properties.

需要注意的是, usage requirements 并不是为了方便下游使用特定的
[COMPILE_OPTIONS][def2], [COMPILE_DEFINITIONS][def3] 等而设计的.
属性的内容必须是 **要求**, 而不仅仅是建议.

请参阅 cmake-packages(7) 手册的 "创建可重置的软件包" 部分,
了解在创建用于再分发的软件包时, 在指定使用要求时必须注意的其他事项.

target 的使用要求可以传递给 dependents.
`target_link_libraries()` 命令有 `PRIVATE`, `INTERFACE` 和 `PUBLIC` 关键字来控制传播.

```c
add_library(archive archive.cpp)
target_compile_definitions(archive INTERFACE USING_ARCHIVE_LIB)

add_library(serialization serialization.cpp)
target_compile_definitions(serialization INTERFACE USING_SERIALIZATION_LIB)

add_library(archiveExtras extras.cpp)
target_link_libraries(archiveExtras PUBLIC archive)
target_link_libraries(archiveExtras PRIVATE serialization)
# archiveExtras is compiled with -DUSING_ARCHIVE_LIB
# and -DUSING_SERIALIZATION_LIB

add_executable(consumer consumer.cpp)
# consumer is compiled with -DUSING_ARCHIVE_LIB
target_link_libraries(consumer archiveExtras)
```

由于 `archive` 是 `archiveExtras` 的 `PUBLIC` dependency,
其使用要求也会传播给 `consumer`.

因为 `serialization` 是 `archiveExtras` 的 `PRIVATE` dependency,
所以它的使用要求不会传播给 `consumer`.

+ 一般来说, 如果 dependency 只在库的实现中使用, 而不在头文件中使用,
则应在使用 `target_link_libraries()` 时指定 `PRIVATE` 关键字.
+ 如果 dependency 在库的头文件中被额外使用(例如用于类继承), 则应将其指定为 `PUBLIC` 依赖关系.
+ 如果 dependency 不被库的实现使用, 而仅被其头文件使用, 则应指定为 `INTERFACE` 依赖关系.

在调用 `target_link_libraries()` 命令时, 可以多次使用每个关键字:

```c
target_link_libraries(archiveExtras
  PUBLIC archive
  PRIVATE serialization
)
```

通过读取 dependencies 的目标属性的 `INTERFACE_` 变体,
并将 得到的值 附加到 用户方(例如上面的archiveExtras) 的非 `INTERFACE_` 变体, 来传播使用要求.

例如, 读取 dependencies 项的 `INTERFACE_INCLUDE_DIRECTORIES`
并将其附加到 用户方 的 `INCLUDE_DIRECTORIES` 中.
如果顺序很重要, 需要小心维护, 而 `target_link_libraries()` 调用产生的顺序不允许正确编译,
可以使用适当的命令直接设置属性以更新顺序.

例如, 如果 target 需要的链接库必须按 `lib1 lib2 lib3` 的顺序指定,
但 头文件include目录 必须按 `lib3 lib1 lib2` 的顺序指定:

```c
target_link_libraries(myExe lib1 lib2 lib3)
target_include_directories(myExe
  PRIVATE $<TARGET_PROPERTY:lib3,INTERFACE_INCLUDE_DIRECTORIES>)
```

请注意, 如果 targets 之后要使用 `install(EXPORT)` 命令 导出用以安装,
则指定目标的 使用要求 时必须谨慎. 更多信息请参阅[创建软件包][def4].

## ref

[def]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#target-usage-requirements
[def2]: https://cmake.org/cmake/help/latest/prop_tgt/COMPILE_OPTIONS.html#prop_tgt:COMPILE_OPTIONS
[def3]: https://cmake.org/cmake/help/latest/prop_tgt/COMPILE_DEFINITIONS.html#prop_tgt:COMPILE_DEFINITIONS
[def4]: https://cmake.org/cmake/help/latest/manual/cmake-packages.7.html#creating-packages