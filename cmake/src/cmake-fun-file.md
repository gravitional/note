# cmake file() 命令

[cmake_path()]: https://cmake.org/cmake/help/latest/command/cmake_path.html#command:cmake_path

+ 文件操作命令.
+ 该命令专门用于需要访问文件系统的, `文件` 和 `路径` 操作.

对于其他 `路径操作`, 只处理 `语法方面` 的问题, 请看 [cmake_path()][] 命令.

注意子命令 `RELATIVE_PATH`, `TO_CMAKE_PATH` 和 `TO_NATIVE_PATH`
已经分别被 `cmake_path()` 命令的子命令 `RELATIVE_PATH`,
`CONVERT ... TO_CMAKE_PATH_LIST` 和 `CONVERT ... TO_NATIVE_PATH_LIST` 取代.

## Synopsis

+ 读取,Reading

```cmake
file(READ <filename> <out-var> [...])
file(STRINGS <filename> <out-var> [...])
file(<HASH> <filename> <out-var>)
file(TIMESTAMP <filename> <out-var> [...])
file(GET_RUNTIME_DEPENDENCIES [...])
```

+ 写入,Writing

```cmake
file({WRITE | APPEND} <filename> <content>...)
file({TOUCH | TOUCH_NOCREATE} [<file>...])
file(GENERATE OUTPUT <output-file> [...])
file(CONFIGURE OUTPUT <output-file> CONTENT <content> [...])
```

+ 文件系统,Filesystem

```cmake
file({GLOB | GLOB_RECURSE} <out-var> [...] [<globbing-expr>...])
file(MAKE_DIRECTORY [<dir>...])
file({REMOVE | REMOVE_RECURSE } [<files>...])
file(RENAME <oldname> <newname> [...])
file(COPY_FILE <oldname> <newname> [...])
file({COPY | INSTALL} <file>... DESTINATION <dir> [...])
file(SIZE <filename> <out-var>)
file(READ_SYMLINK <linkname> <out-var>)
file(CREATE_LINK <original> <linkname> [...])
file(CHMOD <files>... <directories>... PERMISSIONS <permissions>... [...])
file(CHMOD_RECURSE <files>... <directories>... PERMISSIONS <permissions>... [...])
```

+ 路径转换,Path Conversion

```cmake
file(REAL_PATH <path> <out-var> [BASE_DIRECTORY <dir>] [EXPAND_TILDE])
file(RELATIVE_PATH <out-var> <directory> <file>)
file({TO_CMAKE_PATH | TO_NATIVE_PATH} <path> <out-var>)
```

+ 传输,Transfer

```cmake
file(DOWNLOAD <url> [<file>] [...])
file(UPLOAD <file> <url> [...])
```

+ Locking

```cmake
file(LOCK <path> [...])
```

+ 归档,Archiving

```cmake
file(ARCHIVE_CREATE OUTPUT <archive> PATHS <paths>... [...])
file(ARCHIVE_EXTRACT INPUT <archive> [...])
```

## Filesystem

### glob匹配

```cmake
file(GLOB <variable>
     [LIST_DIRECTORIES true|false] [RELATIVE <path>] [CONFIGURE_DEPENDS]
     [<globbing-expressions>...])
file(GLOB_RECURSE <variable> [FOLLOW_SYMLINKS]
     [LIST_DIRECTORIES true|false] [RELATIVE <path>] [CONFIGURE_DEPENDS]
     [<globbing-expressions>...])
```

生成一个符合 `<globbing-expressions>` 的文件列表, 并将其存入 `<variable>` 中.
Globbing 表达式类似于正则表达式, 但要简单得多.
如果指定了 `RELATIVE` 标志, 结果将作为 `给定路径` 的 `相对路径` 返回.

>在3.6版本中有所改变. 结果将以按字母顺序排列(lexicographically).

在 `Windows` 和 `macOS` 上, 即使底层文件系统是大小写敏感的, globbing也是不分大小写的
(文件名和 `globbing` 表达式在匹配前都被转换为小写).
在其他平台上, `globbing` 是区分大小写的.

>3.3版中的新功能: 默认情况下, `GLOB` 会列出目录--
>如果 `LIST_DIRECTORIES` 被设置为 `false`, 结果中会省略目录.
>
>3.12版中的新功能: 如果指定了 `CONFIGURE_DEPENDS` 标志,
>`CMake` 将在 `main build system check target` 中加入逻辑,
>在构建时重新运行标记的 `GLOB` 命令.
>如果任何输出发生变化, `CMake` 将重新生成构建系统.

+ 注意 我们不推荐使用 `GLOB` 来收集 源码树 上的源码文件列表.
如果在 添加 或 删除源文件时没有改变 `CMakeLists.txt` 文件,
那么已生成的 `build system` 就无法知道何时要求 `CMake` 重新生成. 
`CONFIGURE_DEPENDS` 标志可能不会在所有的生成器上可靠地工作,
或者如果将来有新的生成器不能支持它, 使用它的项目将被卡住.
即使 `CONFIGURE_DEPENDS` 能够可靠地工作,
在每次 `rebuild` 时执行检查仍然是有代价的.

`globbing` 表达式的例子包括:

+ `*.cxx` - 匹配所有扩展名为 `cxx` 的文件
+ `*.vt?`      - 匹配所有以`vta,...,vtz` 为扩展名的文件
+ `f[3-5].txt` - 匹配文件 `f3.txt`, `f4.txt`, `f5.txt`

`GLOB_RECURSE` 模式将遍历 `匹配目录` 的所有 `子目录` 并匹配文件.
只有在给出 `FOLLOW_SYMLINKS` 或策略 [CMP0009][] 没有设置为 `NEW` 的情况下,
才会遍历 `符号链接`(symbol links)类型 的子目录.

>3.3版中的新内容: 默认情况下, `GLOB_RECURSE` 从结果列表中省略目录--
>将 `LIST_DIRECTORIES` 设置为 `true` 会将 `目录` 加入结果列表.
>如果给出了 `FOLLOW_SYMLINKS` 或者策略 [CMP0009][] 没有设置为 `NEW`,
>那么 `LIST_DIRECTORIES` 会将 `符号链接` 视为 `目录`.

递归 globbing 的例子包括:

```cmake
/dir/*.py #匹配 `/dir` 和子目录中的所有 python 文件
```

### 目录CRUD操作

+ 根据需要创建给定的 `目录` 和它们的 父目录.

```cmake
file(MAKE_DIRECTORY [<directories>...])
```

+ 删除给定的文件.

```cmake
file(REMOVE [<files>...])
file(REMOVE_RECURSE [<files>...])
```

`REMOVE_RECURSE` 模式将删除给定的文件和目录, 也包括 `非空的目录`.
如果给定的文件不存在, 则不会产生错误.
`相对路径` 是相对于当前源目录进行计算的.

>3.15版中的变化: `空的输入路径` 被忽略, 并发出`警告`.
>先前版本的 `CMake` 将 `空字符串` 解释为相对于 `当前目录` 的相对路径, 并删除其内容.

+ 将文件系统中的文件或目录从 `<oldname>` 移动到 `<newname>`,
原子地(atomically)替换目标.

```cmake
file(RENAME <oldname> <newname>
     [RESULT <result>]
     [NO_REPLACE])
```

选项是:

`RESULT <result>`
>在3.21版本中新增.

成功时将 `<result>`变量设为 `0`, 否则为错误信息.
如果没有指定 `RESULT`, 并且操作失败, 会发出错误信息.

`NO_REPLACE`
>3.21版中的新内容.

如果 `<newname>` 路径已经存在, 不要替换它.
如果使用 `RESULT <result>`, 结果变量将被设置为 `NO_REPLACE`.
否则, 会发出一个错误.

+ 从 `<oldname>` 复制文件到 `<newname>`. 不支持目录.
忽略 `符号链接`, `<oldfile>` 的内容被读取并作为一个新文件写入 `<newname>`.

```cmake
file(COPY_FILE <oldname> <newname>
     [RESULT <result>]
     [ONLY_IF_DIFFERENT])
```

>3.21版中的新内容.
选项是:

`RESULT <result>`
成功时将 `<result>` 变量设置为 `0`, 否则设置错误信息.
如果没有指定 `RESULT`, 并且操作失败, 将发出一个错误信息.

`only_if_different`
如果 `<newname>` 路径已经存在, 如果文件的内容已经与 `<oldname>` 相同,
就不要替换它(这样可以避免更新 `<newname>` 的时间戳).

这个子命令与带有 `COPYONLY` 选项的 `configure_file()` 有一些相似之处.
一个重要的区别是 `configure_file()` 创建了对源文件的依赖性,
因此如果源文件发生变化, `CMake` 将被重新运行.
`file(COPY_FILE)` 子命令不会创建这种依赖关系.

也请看下面的 `file(COPY)` 子命令, 它提供了进一步的文件拷贝功能.

### 进阶操作

```cmake
file(<COPY|INSTALL> <files>... DESTINATION <dir>
     [NO_SOURCE_PERMISSIONS | USE_SOURCE_PERMISSIONS]
     [FILE_PERMISSIONS <permissions>...]
     [DIRECTORY_PERMISSIONS <permissions>...]
     [FOLLOW_SYMLINK_CHAIN]
     [FILES_MATCHING]
     [[PATTERN <pattern> | REGEX <regex>]
      [EXCLUDE] [PERMISSIONS <permissions>...]] [...])
```

>注意 对于简单的文件复制操作, 刚才的 `file(COPY_FILE)` 子命令可能更容易使用.

`COPY` 签名将 `文件`, `目录` 和 `符号链接` 复制到 `目标文件夹`.
`相对输入路径` 是相对于当前的 `源目录` 进行计算的,
而 `相对目标` 是相对于当前的 `构建目录` 进行计算的.
复制保留了 `输入文件` 的 `时间戳`, 如果一个文件在目的地存在相同的时间戳, 则将其优化掉.
除非给出明确的权限或 `NO_SOURCE_PERMISSIONS` (默认为 `USE_SOURCE_PERMISSIONS`),
否则复制会保留 `输入权限`.

*3.15版中的新内容*: 如果指定了 `FOLLOW_SYMLINK_CHAIN`,
`COPY` 将递归地解析所给路径上的 `符号链接`, 直到找到真正的文件,
并为遇到的每个 `符号链接` 在 `目标文件` 中安装相应的 `符号链接`.
对于每个被安装的 `符号链接`, 解析时都会剥离目录, 只留下文件名,
这意味着新的符号链接指向与符号链接相同目录下的文件.
这个特性在一些 `Unix` 系统上很有用.
在这些系统中, 库被安装成带有 `版本号` 的符号链接链, `较粗略` 的版本指向 `更详细` 的版本.
`FOLLOW_SYMLINK_CHAIN` 将安装所有这些 `符号链接` 和 `库本身` 到目标目录中.
例如, 如果你有以下的目录结构.

```bash
/opt/foo/lib/libfoo.so.1.2.3
/opt/foo/lib/libfoo.so.1.2 -> libfoo.so.1.2.3
/opt/foo/lib/libfoo.so.1-->libfoo.so.1.2
/opt/foo/lib/libfoo.so -> libfoo.so.1
```

然后令

```cmake
file(COPY /opt/foo/lib/libfoo.so DESTINATION lib FOLLOW_SYMLINK_CHAIN)
```

这将把所有的符号链接和 `libfoo.so.1.2.3` 本身安装到 `lib` 中.

关于 `权限`, `FILES_MATCHING`, `PATTERN`, `REGEX` 和 `EXCLUDE` 选项的文档,
请参见 [install(DIRECTORY)][] 命令.
复制目录会保留其 `内容的结构`, 即使是使用 `选项` 来选择 文件的 `子集`.

命令的 `INSTALL` 签名重载 与 `COPY` 略有不同:
它打印状态信息, 并且默认为 `NO_SOURCE_PERMISSIONS`.

由 [install()][] 命令生成的 `安装脚本` 使用这个签名
(有一些未归档的选项供内部使用).

*3.22版中的改变*:
环境变量 [CMAKE_INSTALL_MODE][] 可以覆盖 `file(INSTALL)` 的默认复制行为.

### 其他操作

+ 确定 `<filename>` 的文件大小并将结果放入 `<variable>` 变量.
要求 `<filename>` 是指向文件的 `有效路径`, 并且是 `可读的`.
*3.14版中新增.*

```cmake
file(SIZE <filename>    <variable>)
```

+ 该子命令查询符号链接 `<linkname>`, 并在结果 `<variable>` 中存储其指向的路径.
如果<linkname>不存在或者不是 `符号链接`, CMake 会发出 `致命的错误`.
*3.14版中新增.*

```cmake
file(READ_SYMLINK <linkname> <variable>)
```

注意, 这个命令返回 `raw符号链接路径`, 并不解析 `相对路径`.
下面是一个关于如何确保获得绝对路径的例子:

```cmake
set(linkname "/path/to/foo.sym")
file(READ_SYMLINK "${linkname}" result)
if(NOT IS_ABSOLUTE "${result}")
  get_filename_component(dir "${linkname}" DIRECTORY)
  set(result "${dir}/${result}")
endif()
```

+ 创建指向 `<original>` 的链接 `<linkname>`.
默认情况下, 它将是 `硬链接`, 但提供 `SYMBOLIC` 选项的结果是符号链接.
硬链接要求 `original` 存在并且是文件, 而不是一个目录.
如果 `<linkname>` 已经存在, 它将被覆盖.
*3.14版中的新内容*

```cmake
file(CREATE_LINK <original> <linkname>
     [RESULT <result>] [COPY_ON_ERROR] [SYMBOLIC])
```

如果指定了 `<result>` 变量, 则接收操作的状态.
成功时它被设置为0, 否则就是错误信息.
如果没有指定 `RESULT`, 并且操作失败, 将发出致命的错误.

如果 创建链接失败, 指定 `COPY_ON_ERROR` 可以将 `复制文件` 作为后备措施(fallback).
这对于处理诸如 `<original>` 和 `<linkname>` 在不同的驱动器或挂载点上的情况很有用,
这将使它们无法形成 `硬链接`.

+ 为指定的 `<files>...` 和 `<directories>...` 设置权限.
*3.19版的新内容*

```cmake
file(CHMOD <files>... <目录>...
    [PERMISSIONS <permissions>...］
    [FILE_PERMISSIONS <permissions>...］
    [DIRECTORY_PERMISSIONS <permissions>...])
```

有效的权限是:
`OWNER_READ`, `OWNER_WRITE`, `OWNER_EXECUTE`,
`GROUP_READ`, `GROUP_WRITE`, `GROUP_EXECUTE`,
`WORLD_READ`, `WORLD_WRITE`, `WORLD_EXECUTE`,
`SETUID`, `SETGID`.

关键字的有效组合是:

+ `PERMISSIONS`;    所有项目都被改变.
+ `FILE_PERMISSIONS`;   只有文件被改变.
+ `DIRECTORY_PERMISSIONS`;  只有目录被改变.
+ `PERMISSIONS` 和 `FILE_PERMISSIONS`; `FILE_PERMISSIONS` 覆盖了文件的 `PERMISSIONS`.
+ `PERMISSIONS` and `DIRECTORY_PERMISSIONS`; `DIRECTORY_PERMISSIONS` 覆盖 目录的 `PERMISSIONS`
+ `FILE_PERMISSIONS` and `DIRECTORY_PERMISSIONS`;
对文件使用 `FILE_PERMISSIONS`, 对目录使用 `DIRECTORY_PERMISSIONS`.

+ 与 `CHMOD` 相同, 但递归地改变 `<directories>...` 中存在的文件和目录的 `权限`.
*3.19版中的新功能.*

```cmake
file(CHMOD_RECURSE <files>... <directories>...
     [PERMISSIONS <permissions>...]
     [FILE_PERMISSIONS <permissions>...]
     [DIRECTORY_PERMISSIONS <permissions>...])
```

[CMP0009]: https://cmake.org/cmake/help/latest/policy/CMP0009.html#policy:CMP0009
[install(DIRECTORY)]: https://cmake.org/cmake/help/latest/command/install.html#command:install
[install()]: https://cmake.org/cmake/help/latest/command/install.html#command:install
[CMAKE_INSTALL_MODE]: https://cmake.org/cmake/help/latest/envvar/CMAKE_INSTALL_MODE.html#envvar:CMAKE_INSTALL_MODE
