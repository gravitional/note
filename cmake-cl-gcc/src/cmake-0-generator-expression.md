# cmake generator 表达式

[cmake-generator-expressions(7)](https://cmake.org/cmake/help/latest/manual/cmake-generator-expressions.7.html)

## Target Artifacts

These expressions look up information about artifacts associated with a given target tgt. Unless otherwise stated, this can be any runtime artifact, namely:

An executable target created by add_executable().

A shared library target (.so, .dll but not their .lib import library) created by add_library().

A static library target created by add_library().

In the following, the phrase "the tgt filename" means the name of the tgt binary file. This has to be distinguished from the phrase "the target name", which is just the string tgt.

`$<TARGET_FILE:tgt>`
Full path to the tgt binary file.

Note that tgt is not added as a dependency of the target this expression is evaluated on, unless the expression is being used in add_custom_command() or add_custom_target().

`$<TARGET_FILE_BASE_NAME:tgt>`
Added in version 3.15.

Base name of tgt, i.e. `$<TARGET_FILE_NAME:tgt>` without prefix and suffix. For example, if the tgt filename is libbase.so, the base name is base.

See also the OUTPUT_NAME, ARCHIVE_OUTPUT_NAME, LIBRARY_OUTPUT_NAME and RUNTIME_OUTPUT_NAME target properties and their configuration specific variants OUTPUT_NAME_<CONFIG>, ARCHIVE_OUTPUT_NAME_<CONFIG>, LIBRARY_OUTPUT_NAME_<CONFIG> and RUNTIME_OUTPUT_NAME_<CONFIG>.

The <CONFIG>_POSTFIX and DEBUG_POSTFIX target properties can also be considered.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_FILE_PREFIX:tgt>`
Added in version 3.15.

Prefix of the tgt filename (such as lib).

See also the PREFIX target property.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_FILE_SUFFIX:tgt>`
Added in version 3.15.

Suffix of the tgt filename (extension such as .so or .exe).

See also the SUFFIX target property.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_FILE_NAME:tgt>`
The tgt filename.

Note that tgt is not added as a dependency of the target this expression is evaluated on (see policy CMP0112).

`$<TARGET_FILE_DIR:tgt>`
Directory of the tgt binary file.

Note that tgt is not added as a dependency of the target this expression is evaluated on (see policy CMP0112).

`$<TARGET_IMPORT_FILE:tgt>`
Added in version 3.27.

Full path to the linker import file. On DLL platforms, it would be the .lib file. For executables on AIX, and for shared libraries on macOS, it could be, respectively, the .imp or .tbd import file, depending on the value of the ENABLE_EXPORTS property.

This expands to an empty string when there is no import file associated with the target.

`$<TARGET_IMPORT_FILE_BASE_NAME:tgt>`
Added in version 3.27.

Base name of the linker import file of the target tgt without prefix or suffix. For example, if the target file name is libbase.tbd, the base name is base.

See also the OUTPUT_NAME and ARCHIVE_OUTPUT_NAME target properties and their configuration specific variants OUTPUT_NAME_<CONFIG> and ARCHIVE_OUTPUT_NAME_<CONFIG>.

The <CONFIG>_POSTFIX and DEBUG_POSTFIX target properties can also be considered.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_IMPORT_FILE_PREFIX:tgt>`
Added in version 3.27.

Prefix of the import file of the target tgt.

See also the IMPORT_PREFIX target property.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_IMPORT_FILE_SUFFIX:tgt>`
Added in version 3.27.

Suffix of the import file of the target tgt.

The suffix corresponds to the file extension (such as .lib or .tbd).

See also the IMPORT_SUFFIX target property.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_IMPORT_FILE_NAME:tgt>`
Added in version 3.27.

Name of the import file of the target tgt.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_IMPORT_FILE_DIR:tgt>`
Added in version 3.27.

Directory of the import file of the target tgt.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_FILE:tgt>`
File used when linking to the tgt target. This will usually be the library that tgt represents (.a, .lib, .so), but for a shared library on DLL platforms, it would be the .lib import library associated with the DLL.

Added in version 3.27: On macOS, it could be the .tbd import file associated with the shared library, depending on the value of the ENABLE_EXPORTS property.

This generator expression is equivalent to `$<TARGET_LINKER_LIBRARY_FILE>` or `$<TARGET_LINKER_IMPORT_FILE>` generator expressions, depending on the characteristics of the target and the platform.

`$<TARGET_LINKER_FILE_BASE_NAME:tgt>`
Added in version 3.15.

Base name of file used to link the target tgt, i.e. `$<TARGET_LINKER_FILE_NAME:tgt>` without prefix and suffix. For example, if target file name is libbase.a, the base name is base.

See also the OUTPUT_NAME, ARCHIVE_OUTPUT_NAME, and LIBRARY_OUTPUT_NAME target properties and their configuration specific variants OUTPUT_NAME_<CONFIG>, ARCHIVE_OUTPUT_NAME_<CONFIG> and LIBRARY_OUTPUT_NAME_<CONFIG>.

The <CONFIG>_POSTFIX and DEBUG_POSTFIX target properties can also be considered.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_FILE_PREFIX:tgt>`
Added in version 3.15.

Prefix of file used to link target tgt.

See also the PREFIX and IMPORT_PREFIX target properties.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_FILE_SUFFIX:tgt>`
Added in version 3.15.

Suffix of file used to link where tgt is the name of a target.

The suffix corresponds to the file extension (such as ".so" or ".lib").

See also the SUFFIX and IMPORT_SUFFIX target properties.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_FILE_NAME:tgt>`
Name of file used to link target tgt.

Note that tgt is not added as a dependency of the target this expression is evaluated on (see policy CMP0112).

`$<TARGET_LINKER_FILE_DIR:tgt>`
Directory of file used to link target tgt.

Note that tgt is not added as a dependency of the target this expression is evaluated on (see policy CMP0112).

`$<TARGET_LINKER_LIBRARY_FILE:tgt>`
Added in version 3.27.

File used when linking o the tgt target is done using directly the library, and not an import file. This will usually be the library that tgt represents (.a, .so, .dylib). So, on DLL platforms, it will be an empty string.

`$<TARGET_LINKER_LIBRARY_FILE_BASE_NAME:tgt>`
Added in version 3.27.

Base name of library file used to link the target tgt, i.e. `$<TARGET_LINKER_LIBRARY_FILE_NAME:tgt>` without prefix and suffix. For example, if target file name is libbase.a, the base name is base.

See also the OUTPUT_NAME, ARCHIVE_OUTPUT_NAME, and LIBRARY_OUTPUT_NAME target properties and their configuration specific variants OUTPUT_NAME_<CONFIG>, ARCHIVE_OUTPUT_NAME_<CONFIG> and LIBRARY_OUTPUT_NAME_<CONFIG>.

The <CONFIG>_POSTFIX and DEBUG_POSTFIX target properties can also be considered.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_LIBRARY_FILE_PREFIX:tgt>`
Added in version 3.27.

Prefix of the library file used to link target tgt.

See also the PREFIX target property.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_LIBRARY_FILE_SUFFIX:tgt>`
Added in version 3.27.

Suffix of the library file used to link target tgt.

The suffix corresponds to the file extension (such as ".a" or ".dylib").

See also the SUFFIX target property.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_LIBRARY_FILE_NAME:tgt>`
Added in version 3.27.

Name of the library file used to link target tgt.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_LIBRARY_FILE_DIR:tgt>`
Added in version 3.27.

Directory of the library file used to link target tgt.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_IMPORT_FILE:tgt>`
Added in version 3.27.

File used when linking to the tgt target is done using an import file. This will usually be the import file that tgt represents (.lib, .tbd). So, when no import file is involved in the link step, an empty string is returned.

`$<TARGET_LINKER_IMPORT_FILE_BASE_NAME:tgt>`
Added in version 3.27.

Base name of the import file used to link the target tgt, i.e. `$<TARGET_LINKER_IMPORT_FILE_NAME:tgt>` without prefix and suffix. For example, if target file name is libbase.tbd, the base name is base.

See also the OUTPUT_NAME and ARCHIVE_OUTPUT_NAME, target properties and their configuration specific variants OUTPUT_NAME_<CONFIG> and ARCHIVE_OUTPUT_NAME_<CONFIG>.

The `<CONFIG>_POSTFIX` and DEBUG_POSTFIX target properties can also be considered.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_IMPORT_FILE_PREFIX:tgt>`
Added in version 3.27.

Prefix of the import file used to link target tgt.

See also the IMPORT_PREFIX target property.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_IMPORT_FILE_SUFFIX:tgt>`
Added in version 3.27.

Suffix of the import file used to link target tgt.

The suffix corresponds to the file extension (such as ".lib" or ".tbd").

See also the IMPORT_SUFFIX target property.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_IMPORT_FILE_NAME:tgt>`
Added in version 3.27.

Name of the import file used to link target tgt.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_LINKER_IMPORT_FILE_DIR:tgt>`
Added in version 3.27.

Directory of the import file used to link target tgt.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_SONAME_FILE:tgt>`
File with soname (.so.3) where tgt is the name of a target.

`$<TARGET_SONAME_FILE_NAME:tgt>`
Name of file with soname (.so.3).

Note that tgt is not added as a dependency of the target this expression is evaluated on (see policy CMP0112).

`$<TARGET_SONAME_FILE_DIR:tgt>`
Directory of file with soname (.so.3).

Note that tgt is not added as a dependency of the target this expression is evaluated on (see policy CMP0112).

`$<TARGET_SONAME_IMPORT_FILE:tgt>`
Added in version 3.27.

Import file with soname (.3.tbd) where tgt is the name of a target.

`$<TARGET_SONAME_IMPORT_FILE_NAME:tgt>`
Added in version 3.27.

Name of the import file with soname (.3.tbd).

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_SONAME_IMPORT_FILE_DIR:tgt>`
Added in version 3.27.

Directory of the import file with soname (.3.tbd).

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_PDB_FILE:tgt>`
Added in version 3.1.

Full path to the linker generated program database file (.pdb) where tgt is the name of a target.

See also the PDB_NAME and PDB_OUTPUT_DIRECTORY target properties and their configuration specific variants PDB_NAME_<CONFIG> and PDB_OUTPUT_DIRECTORY_<CONFIG>.

`$<TARGET_PDB_FILE_BASE_NAME:tgt>`
Added in version 3.15.

Base name of the linker generated program database file (.pdb) where tgt is the name of a target.

The base name corresponds to the target PDB file name (see `$<TARGET_PDB_FILE_NAME:tgt>`) without prefix and suffix. For example, if target file name is base.pdb, the base name is base.

See also the PDB_NAME target property and its configuration specific variant `PDB_NAME_<CONFIG>`.

The <CONFIG>_POSTFIX and DEBUG_POSTFIX target properties can also be considered.

Note that tgt is not added as a dependency of the target this expression is evaluated on.

`$<TARGET_PDB_FILE_NAME:tgt>`
Added in version 3.1.

Name of the linker generated program database file (.pdb).

Note that tgt is not added as a dependency of the target this expression is evaluated on (see policy CMP0112).

`$<TARGET_PDB_FILE_DIR:tgt>`
Added in version 3.1.

Directory of the linker generated program database file (.pdb).

Note that tgt is not added as a dependency of the target this expression is evaluated on (see policy CMP0112).

`$<TARGET_BUNDLE_DIR:tgt>`
Added in version 3.9.

Full path to the bundle directory (/path/to/my.app, /path/to/my.framework, or /path/to/my.bundle), where tgt is the name of a target.

Note that tgt is not added as a dependency of the target this expression is evaluated on (see policy CMP0112).

`$<TARGET_BUNDLE_DIR_NAME:tgt>`
Added in version 3.24.

Name of the bundle directory (my.app, my.framework, or my.bundle), where tgt is the name of a target.

Note that tgt is not added as a dependency of the target this expression is evaluated on (see policy CMP0112).

`$<TARGET_BUNDLE_CONTENT_DIR:tgt>`
Added in version 3.9.

Full path to the bundle content directory where tgt is the name of a target. For the macOS SDK it leads to /path/to/my.app/Contents, /path/to/my.framework, or /path/to/my.bundle/Contents. For all other SDKs (e.g. iOS) it leads to /path/to/my.app, /path/to/my.framework, or /path/to/my.bundle due to the flat bundle structure.

Note that tgt is not added as a dependency of the target this expression is evaluated on (see policy CMP0112).

+ `$<TARGET_OBJECTS:tgt>`
在 3.1 版中添加.

生成 tgt 后的对象列表, 通常用于 [object library目标](https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#object-libraries).

+ `$<TARGET_RUNTIME_DLLS:tgt>`
在 3.21 版中添加.

目标在运行时依赖的 DLL 列表.
这取决于目标的传递依赖(transitive dependencies)关系中所有 `SHARED` 目标的位置.
如果只需要 DLL 的目录, 请参阅 `TARGET_RUNTIME_DLL_DIRS` 生成器表达式.
在 可执行文件, `SHARED`库 和 `MODULE`库 以外的目标上使用此生成器表达式会导致错误.
**在非 DLL 平台上, 此表达式的值总是空字符串**.

可以使用 `cmake -E copy -t` 命令,
在 `POST_BUILD` 自定义命令中将目标依赖的所有 DLL 复制到其输出目录. 例如

```cmake
find_package(foo CONFIG REQUIRED) # package generated by install(EXPORT)

add_executable(exe main.c)
target_link_libraries(exe PRIVATE foo::foo foo::bar)
add_custom_command(TARGET exe POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy -t $<TARGET_FILE_DIR:exe> $<TARGET_RUNTIME_DLLS:exe>
  # 或者使用
  # COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_RUNTIME_DLLS:exe> $<TARGET_FILE_DIR:exe>
  COMMAND_EXPAND_LISTS
)
```

>注意 只有当目标知道其 `.dll` 文件的位置时, 才支持 [Imported Targets][].
>导入的 `SHARED`库 必须将 `IMPORTED_LOCATION` 设置为其 `.dll` 文件的位置. 有关详情, 请参阅 [add_library 导入库][] 部分.
>许多[Find模块][] 会生成 UNKNOWN 类型的导入目标, 因此会被忽略.
>在支持运行时路径 (RPATH) 的平台上, 请参考 INSTALL_RPATH 目标属性.
>在 Apple 平台上, 请参考 INSTALL_NAME_DIR 目标属性.

[Imported Targets]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#imported-targets
[add_library 导入库]: https://cmake.org/cmake/help/latest/command/add_library.html#add-library-imported-libraries
[Find模块]: https://cmake.org/cmake/help/latest/manual/cmake-developer.7.html#find-modules

+ `$<TARGET_RUNTIME_DLL_DIRS:tgt>`
在 3.27 版中添加.

包含目标在运行时依赖的 DLL 所在的 **目录列表**(请参阅 `TARGET_RUNTIME_DLLS`).
这是由目标的 传递依赖(transitive dependencies) 关系中所有 SHARED 目标的位置决定的.
在 可执行文件, SHARED库 和 MODULE库 以外的目标上使用此生成器表达式是错误的.
**在非 DLL 平台上, 该表达式的结果总是空字符串**.

该生成器表达式可用于使用 [file(GENERATE)][] 创建批处理文件, 并相应设置 PATH 环境变量.

[file(GENERATE)]: https://cmake.org/cmake/help/latest/command/file.html#generate
