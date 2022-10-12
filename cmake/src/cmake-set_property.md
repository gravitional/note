# set_property

在给定的 scope 内设置一个命名的属性(property).

```cmake
set_property(<GLOBAL                      |
              DIRECTORY [<dir>]           |
              TARGET    [<target1> ...]   |
              SOURCE    [<src1> ...]
                        [DIRECTORY <dirs> ...]
                        [TARGET_DIRECTORY <targets> ...] |
              INSTALL   [<file1> ...]     |
              TEST      [<test1> ...]     |
              CACHE     [<entry1> ...]    >
             [APPEND] [APPEND_STRING]
             PROPERTY <name> [<value1> ...])
```

在某个 `scope` 的 零个或多个 `对象`(objects)上设置一个`属性`(property).

第一个参数决定了设置该属性的 `作用域`. 它必须是下列之一.

+ `GLOBAL`; 作用域是唯一的, 不接受名称.

+ `DIRECTORY`
Scope  默认为 `当前目录`,
但其他目录(已被CMake处理)可以用 `全路径` 或 `相对路径` 命名.
`相对路径` 被认为是相对于当前源目录的. 参见 [set_directory_properties()][]命令.

>3.19版中的新内容: `<dir>`可以引用一个二进制目录.

+ `TARGET`(目标)
Scope  可以命名零个或多个现有目标. 请参见 [set_target_properties()命令][].

+ `SOURCE`
Scope 可以引用零个或多个 `源文件`.
默认情况下, `源文件属性` 只对添加在同一目录下的 targets(`CMakeLists.txt`)可见.

>3.18版中的新内容: 可以使用 以下子选项 在其他目录范围内设置可见性.

+ `DIRECTORY <dirs>...`
源文件属性将被设置在每个 `<dirs>` 目录的作用域中.
CMake必须已经知道这些目录中的每一个,
要么是通过调用[add_subdirectory()][]来添加它们, 要么是它是最高级别的源目录.
相对路径被视为相对于当前源目录.

>3.19版中的新内容: `<dirs>`可以引用一个二进制目录.

+ `TARGET_DIRECTORY <targets>...`
源文件属性将在创建任何指定<targets>的目录范围内被设置
(因此<targets>必须已经存在).

也请参见 [set_source_files_properties()][] 命令.

+ `INSTALL`
    >3.1版中的新内容.

Scope 可以引用零或更多的 `安装文件路径`. 这些将被提供给 `CPack` 以影响部署.

属性的`键`和`值`都可以使用 `生成器表达式`(generator expressions).
特定的属性可能适用于 `已安装的文件和/或目录`.

`路径元素` 必须用正斜杠隔开, 必须规范化, 并且区分大小写.

要用相对路径引用 `安装前缀本身`, 请使用 `.`.

目前 `已安装文件属性` 只为 `WIX生成器`定义,
其中给出的路径是相对于 `安装前缀` 的.

+ `TEST`
Scope 可以引用零个或多个现有的测试.
也请参见 [set_tests_properties()][] 命令.

对于由 [add_test(NAME)][] 签名创建的测试,
测试属性值可以使用 [生成器表达式][] 来指定,

+ `CACHE`;
Scope 必须引用零个或更多的 缓冲区现有条目.

required  `PROPERTY` 选项, 后面紧接着要设置的 `属性名称`,
其余的参数用于组成以 `分号分隔` 的列表形式 的 `属性值`.

如果给定了 `APPEND` 选项, 该列表 将被附加到任何现有的 `属性值上`(除了空值被忽略, 且不被附加).
如果给定 `APPEND_STRING` 选项, 则将给出的字符串, 附加到任何现有的 `属性值`上,
也就是说, 它的结果是被延长的 `字符串`, 而不是 `字符串` 组成的列表.

当一个属性 定义时支持继承行为, 在它身上使用 `APPEND` 或 `APPEND_STRING` 时,
见 [define_property()](https://cmake.org/cmake/help/latest/command/define_property.html#command:define_property).
在寻找要被附加的 初始值 时, 不会发生继承行为.
如果该 `属性` 在指定的范围内没有被 `directly set`,
该命令的行为就像没有给出 `APPEND` 或 `APPEND_STRING` 一样.

请参阅 [cmake-properties(7)][] 手册以获得每个作用域中的 `属性列表`.

注意 [GENERATED][] 源文件属性可能是全局可见的. 详情请参见其文档.

[set_directory_properties()]: https://cmake.org/cmake/help/latest/command/set_directory_properties.html#command:set_directory_properties
[set_target_properties()命令]: https://cmake.org/cmake/help/latest/command/set_target_properties.html#command:set_target_properties
[add_subdirectory()]: https://cmake.org/cmake/help/latest/command/add_subdirectory.html#command:add_subdirectory
[set_source_files_properties()]: https://cmake.org/cmake/help/latest/command/set_source_files_properties.html#command:set_source_files_properties
[set_tests_properties()]: https://cmake.org/cmake/help/latest/command/set_tests_properties.html#command:set_tests_properties
[生成器表达式]: https://cmake.org/cmake/help/latest/manual/cmake-generator-expressions.7.html#manual:cmake-generator-expressions(7)
[add_test(NAME)]: https://cmake.org/cmake/help/latest/command/add_test.html#command:add_test
[cmake-properties(7)]: https://cmake.org/cmake/help/latest/manual/cmake-properties.7.html#manual:cmake-properties(7)
[GENERATED]: https://cmake.org/cmake/help/latest/prop_sf/GENERATED.html#prop_sf:GENERATED
