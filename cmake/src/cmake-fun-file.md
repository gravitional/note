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
那么已生成的 `build system` 就无法知道何时要求 `CMake` 重新生成. `CONFIGURE_DEPENDS` 标志可能不会在所有的生成器上可靠地工作,
或者如果将来有新的生成器不能支持它, 使用它的项目将被卡住.
即使 `CONFIGURE_DEPENDS` 能够可靠地工作,
在每次 `rebuild` 时执行检查仍然是有代价的.

globbing表达式的例子包括.

*.cxx - 匹配所有扩展名为 cxx 的文件
*.vt?      - 匹配所有以vta,...,vtz为扩展名的文件
f[3-5].txt - 匹配文件f3.txt, f4.txt, f5.txt
GLOB_RECURSE模式将遍历匹配目录的所有子目录并匹配文件.
只有在给出FOLLOW_SYMLINKS或策略CMP0009没有设置为NEW的情况下, 才会遍历属于符号链接的子目录.

3.3版中的新内容: 默认情况下, GLOB_RECURSE从结果列表中省略目录--将LIST_DIRECTORIES设置为 "true "会将目录加入结果列表.
如果给出了FOLLOW_SYMLINKS或者策略CMP0009没有设置为NEW, 那么LIST_DIRECTORIES会将符号链接视为目录.

递归球化的例子包括.

/dir/*.py - 匹配/dir和子目录中的所有python文件
file(MAKE_DIRECTORY [<directories>...])
根据需要创建给定的目录和它们的父目录.

file(REMOVE [<files>...])
file(REMOVE_RECURSE [<files>...])
删除给定的文件. REMOVE_RECURSE模式将删除给定的文件和目录, 也包括非空的目录. 如果给定的文件不存在, 则不会产生错误. 相对的输入路径是相对于当前源目录进行计算的.

Changed in version 3.15: Empty input paths are ignored with a warning. Previous versions of CMake interpreted empty strings as a relative path with respect to the current directory and removed its contents.

file(RENAME <oldname> <newname>
     [RESULT <result>]
     [NO_REPLACE])
Move a file or directory within a filesystem from <oldname> to <newname>, replacing the destination atomically.

The options are:

RESULT <result>
New in version 3.21.

Set <result> variable to 0 on success or an error message otherwise. If RESULT is not specified and the operation fails, an error is emitted.

NO_REPLACE
New in version 3.21.

If the <newname> path already exists, do not replace it. If RESULT <result> is used, the result variable will be set to NO_REPLACE. Otherwise, an error is emitted.

file(COPY_FILE <oldname> <newname>
     [RESULT <result>]
     [ONLY_IF_DIFFERENT])
New in version 3.21.

Copy a file from <oldname> to <newname>. Directories are not supported. Symlinks are ignored and <oldfile>'s content is read and written to <newname> as a new file.

The options are:

RESULT <result>
Set <result> variable to 0 on success or an error message otherwise. If RESULT is not specified and the operation fails, an error is emitted.

ONLY_IF_DIFFERENT
If the <newname> path already exists, do not replace it if the file's contents are already the same as <oldname> (this avoids updating <newname>'s timestamp).

This sub-command has some similarities to configure_file() with the COPYONLY option. An important difference is that configure_file() creates a dependency on the source file, so CMake will be re-run if it changes. The file(COPY_FILE) sub-command does not create such a dependency.

See also the file(COPY) sub-command just below which provides further file-copying capabilities.

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

Note For a simple file copying operation, the file(COPY_FILE) sub-command just above may be easier to use.
The COPY signature copies files, directories, and symlinks to a destination folder. Relative input paths are evaluated with respect to the current source directory, and a relative destination is evaluated with respect to the current build directory. Copying preserves input file timestamps, and optimizes out a file if it exists at the destination with the same timestamp. Copying preserves input permissions unless explicit permissions or NO_SOURCE_PERMISSIONS are given (default is USE_SOURCE_PERMISSIONS).

New in version 3.15: If FOLLOW_SYMLINK_CHAIN is specified, COPY will recursively resolve the symlinks at the paths given until a real file is found, and install a corresponding symlink in the destination for each symlink encountered. 
For each symlink that is installed, the resolution is stripped of the directory, leaving only the filename, meaning that the new symlink points to a file in the same directory as the symlink. This feature is useful on some Unix systems, 
where libraries are installed as a chain of symlinks with version numbers, with less specific versions pointing to more specific versions. FOLLOW_SYMLINK_CHAIN will install all of these symlinks and the library itself into the destination directory. For example, if you have the following directory structure:

/opt/foo/lib/libfoo.so.1.2.3

/opt/foo/lib/libfoo.so.1.2 -> libfoo.so.1.2.3

/opt/foo/lib/libfoo.so.1 -> libfoo.so.1.2

/opt/foo/lib/libfoo.so -> libfoo.so.1

and you do:

file(COPY /opt/foo/lib/libfoo.so DESTINATION lib FOLLOW_SYMLINK_CHAIN)
This will install all of the symlinks and libfoo.so.1.2.3 itself into lib.

See the install(DIRECTORY) command for documentation of permissions, FILES_MATCHING, PATTERN, REGEX, and EXCLUDE options. Copying directories preserves the structure of their content even if options are used to select a subset of files.

The INSTALL signature differs slightly from COPY: it prints status messages, and NO_SOURCE_PERMISSIONS is default.

Installation scripts generated by the install() command use this signature (with some undocumented options for internal use).

Changed in version 3.22: The environment variable CMAKE_INSTALL_MODE can override the default copying behavior of file(INSTALL).

file(SIZE <filename> <variable>)
New in version 3.14.

Determine the file size of the <filename> and put the result in <variable> variable. Requires that <filename> is a valid path pointing to a file and is readable.

file(READ_SYMLINK <linkname> <variable>)
New in version 3.14.

This subcommand queries the symlink <linkname> and stores the path it points to in the result <variable>. If <linkname> does not exist or is not a symlink, CMake issues a fatal error.

Note that this command returns the raw symlink path and does not resolve a relative path. The following is an example of how to ensure that an absolute path is obtained:

set(linkname "/path/to/foo.sym")
file(READ_SYMLINK "${linkname}" result)
if(NOT IS_ABSOLUTE "${result}")
  get_filename_component(dir "${linkname}" DIRECTORY)
  set(result "${dir}/${result}")
endif()
file(CREATE_LINK <original> <linkname>
     [RESULT <result>] [COPY_ON_ERROR] [SYMBOLIC])
New in version 3.14.

Create a link <linkname> that points to <original>. It will be a hard link by default, but providing the SYMBOLIC option results in a symbolic link instead. Hard links require that original exists and is a file, not a directory. If <linkname> already exists, it will be overwritten.

The <result> variable, if specified, receives the status of the operation. It is set to 0 upon success or an error message otherwise. If RESULT is not specified and the operation fails, a fatal error is emitted.

Specifying COPY_ON_ERROR enables copying the file as a fallback if creating the link fails. It can be useful for handling situations such as <original> and <linkname> being on different drives or mount points, which would make them unable to support a hard link.

file(CHMOD <files>... <directories>...
    [PERMISSIONS <permissions>...]
    [FILE_PERMISSIONS <permissions>...]
    [DIRECTORY_PERMISSIONS <permissions>...])
New in version 3.19.

Set the permissions for the <files>... and <directories>... specified. Valid permissions are OWNER_READ, OWNER_WRITE, OWNER_EXECUTE, GROUP_READ, GROUP_WRITE, GROUP_EXECUTE, WORLD_READ, WORLD_WRITE, WORLD_EXECUTE, SETUID, SETGID.

Valid combination of keywords are:

PERMISSIONS
All items are changed.

FILE_PERMISSIONS
Only files are changed.

DIRECTORY_PERMISSIONS
Only directories are changed.

PERMISSIONS and FILE_PERMISSIONS
FILE_PERMISSIONS overrides PERMISSIONS for files.

PERMISSIONS and DIRECTORY_PERMISSIONS
DIRECTORY_PERMISSIONS overrides PERMISSIONS for directories.

FILE_PERMISSIONS and DIRECTORY_PERMISSIONS
Use FILE_PERMISSIONS for files and DIRECTORY_PERMISSIONS for directories.

file(CHMOD_RECURSE <files>... <directories>...
     [PERMISSIONS <permissions>...]
     [FILE_PERMISSIONS <permissions>...]
     [DIRECTORY_PERMISSIONS <permissions>...])
New in version 3.19.

Same as CHMOD, but change the permissions of files and directories present in the <directories>... recursively.
