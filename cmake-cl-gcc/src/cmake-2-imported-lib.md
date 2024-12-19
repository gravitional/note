# cmake 导入库, imported lib, 导入第三方库, windows "导入库" import lib

[imported-libraries](https://cmake.org/cmake/help/latest/command/add_library.html#imported-libraries)

```bash
add_library(<name> <type> IMPORTED [GLOBAL])
```

添加名为 `<name>` 的 [IMPORTED library target][].
该 target name 可以像项目中构建的任何目标一样被引用,
但默认情况下只能在创建该目标的目录及其下级目录中可见.
添加 Global 以使全局可见.

`<type>` 必须是以下类型之一

`STATIC`, `SHARED`, `MODULE`, `UNKNOWN`
引用位于 项目外部 的库文件.
`IMPORTED_LOCATION` 目标属性
(或其 per-configuration 的变体 IMPORTED_LOCATION_<CONFIG>)
指定了主库文件在磁盘上的位置:

+ 对于大多数非 `Windows` 平台上的 `SHARED`,
主要库文件是链接器和动态加载器使用的 `.so` 或 `.dylib` 文件.
可以是用`;`分隔的字符串(cmake 列表), 表示多个文件.

    如果引用的库文件有 `SONAME`(或在 macOS 上有以 `@rpath/` 开头的 `LC_ID_DYLIB`),
    则应在 `IMPORTED_SONAME` 目标属性中设置该字段的值.

    如果引用的库文件没有 `SONAME`, 但平台支持 `SONAME`, 则应设置 `IMPORTED_NO_SONAME` 目标属性.

+ 对于 Windows 上的共享库, `IMPORTED_IMPLIB` 目标属性 (或其按配置的变体 `IMPORTED_IMPLIB_<CONFIG>`)

    指定 DLL 导入库文件(`.lib` 或 `.dll.a`)在磁盘上的位置,
    而 `IMPORTED_LOCATION` 是 `.dll` 运行时库的位置
    (可选, 但 `TARGET_RUNTIME_DLLS` 生成器表达式需要).

其他使用要求可在 `INTERFACE_*` 属性中指定.

`UNKNOWN` 库类型通常只用于 `Find Modules` 的实现.
它允许使用导入库的路径(通常使用 `find_library()`命令查找), 而无需知道它是什么类型的库.
在 Windows 系统中, `static library` 和 DLL 的 `import library ` 具有相同的文件扩展名, 这一点尤其有用.

`OBJECT`
引用位于项目外部的一组 `object files`.
`IMPORTED_OBJECTS` 目标属性(或其按配置的变体 `IMPORTED_OBJECTS_<CONFIG>`)指定了磁盘上对象文件的位置.
其他使用要求可在 `INTERFACE_*` 属性中指定.

`INTERFACE`
不引用磁盘上的任何库或对象文件, 但可在 `INTERFACE_*` 属性中指定使用要求.

选项包括

`GLOBAL` 使目标名称全局可见.

不生成构建 `imported targets` 的规则, 且 `IMPORTED` 目标属性为 `True`.
导入的库非常有用, 可以方便地从 `target_link_libraries()` 等命令中引用.

有关导入库的详细信息, 可通过设置 名称以 `IMPORTED_` 和 `INTERFACE_` 开头的属性来指定.
有关详细信息, 请参阅此类属性的文档.

[IMPORTED library target]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#imported-targets

## [Imported Targets](https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#imported-targets)

An IMPORTED target represents a pre-existing dependency. Usually such targets are defined by an upstream package and should be treated as immutable. After declaring an IMPORTED target one can adjust its target properties by using the customary commands such as target_compile_definitions(), target_include_directories(), target_compile_options() or target_link_libraries() just like with any other regular target.

IMPORTED targets may have the same usage requirement properties populated as binary targets, such as INTERFACE_INCLUDE_DIRECTORIES, INTERFACE_COMPILE_DEFINITIONS, INTERFACE_COMPILE_OPTIONS, INTERFACE_LINK_LIBRARIES, and INTERFACE_POSITION_INDEPENDENT_CODE.

The LOCATION may also be read from an IMPORTED target, though there is rarely reason to do so. Commands such as add_custom_command() can transparently use an IMPORTED EXECUTABLE target as a COMMAND executable.

The scope of the definition of an IMPORTED target is the directory where it was defined. It may be accessed and used from subdirectories, but not from parent directories or sibling directories. The scope is similar to the scope of a cmake variable.

It is also possible to define a GLOBAL IMPORTED target which is accessible globally in the buildsystem.

See the cmake-packages(7) manual for more on creating packages with IMPORTED targets.

## [IMPORTED_LOCATION](https://cmake.org/cmake/help/latest/prop_tgt/IMPORTED_LOCATION.html#prop_tgt:IMPORTED_LOCATION)

Full path to the main file on disk for an IMPORTED target.

Set this to the location of an IMPORTED target file on disk.
For executables this is the location of the executable file.
For STATIC libraries and modules this is the location of the library or module. For SHARED libraries on non-DLL platforms this is the location of the shared library.
For application bundles on macOS this is the location of the executable file inside Contents/MacOS within the bundle folder.
For frameworks on macOS this is the location of the library file symlink just inside the framework folder.
For DLLs this is the location of the .dll part of the library.
For UNKNOWN libraries this is the location of the file to be linked. Ignored for non-imported targets.

Added in version 3.28: For ordinary frameworks on Apple platforms, this may be the location of the .framework folder itself. For XCFrameworks, it may be the location of the .xcframework folder, in which case any target that links against it will get the selected library's Headers directory as a usage requirement.

The IMPORTED_LOCATION target property may be overridden for a given configuration <CONFIG> by the configuration-specific IMPORTED_LOCATION_<CONFIG> target property. Furthermore, the MAP_IMPORTED_CONFIG_<CONFIG> target property may be used to map between a project's configurations and those of an imported target. If none of these is set then the name of any other configuration listed in the IMPORTED_CONFIGURATIONS target property may be selected and its IMPORTED_LOCATION_<CONFIG> value used.

To get the location of an imported target read one of the LOCATION or LOCATION_<CONFIG> properties.

For platforms with import libraries (e.g. Windows, AIX or Apple) see also IMPORTED_IMPLIB.

## `$<TARGET_RUNTIME_DLLS:tgt>`

Added in version 3.21.

List of DLLs that the target depends on at runtime. This is determined by the locations of all the SHARED targets in the target's transitive dependencies. If only the directories of the DLLs are needed, see the TARGET_RUNTIME_DLL_DIRS generator expression. Using this generator expression on targets other than executables, SHARED libraries, and MODULE libraries is an error. On non-DLL platforms, this expression always evaluates to an empty string.

This generator expression can be used to copy all of the DLLs that a target depends on into its output directory in a POST_BUILD custom command using the cmake -E copy -t command. For example:

```bash
find_package(foo CONFIG REQUIRED) # package generated by install(EXPORT)

add_executable(exe main.c)
target_link_libraries(exe PRIVATE foo::foo foo::bar)
add_custom_command(TARGET exe POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy -t $<TARGET_FILE_DIR:exe> $<TARGET_RUNTIME_DLLS:exe>
  COMMAND_EXPAND_LISTS
)
```

Note Imported Targets are supported only if they know the location of their .dll files. An imported SHARED library must have IMPORTED_LOCATION set to its .dll file. See the add_library imported libraries section for details. Many Find Modules produce imported targets with the UNKNOWN type and therefore will be ignored.
On platforms that support runtime paths (RPATH), refer to the INSTALL_RPATH target property. On Apple platforms, refer to the INSTALL_NAME_DIR target property.

## [IMPORTED](https://cmake.org/cmake/help/latest/prop_tgt/IMPORTED.html#prop_tgt:IMPORTED)

Read-only indication of whether a target is IMPORTED.

The boolean value of this property is True for targets created with the IMPORTED option to add_executable() or add_library(). It is False for targets built within the project.

## [IMPORTED_IMPLIB](https://cmake.org/cmake/help/latest/prop_tgt/IMPORTED_IMPLIB.html#prop_tgt:IMPORTED_IMPLIB)

Full path to the import library for an IMPORTED target.

This property may be set:

On DLL platforms, to the location of the .lib part of the DLL.

Added in version 3.16: On AIX, to an import file (e.g. .imp) created for executables that export symbols (see the ENABLE_EXPORTS target property).

Added in version 3.27: On Apple platforms, to an import file (e.g. .tbd) created for shared libraries or frameworks (see the ENABLE_EXPORTS target property). For frameworks, this is the location of the .tbd file symlink just inside the framework folder.

Added in version 3.28: On non-DLL platforms, to the location of a shared library. When set without also specifying an IMPORTED_LOCATION, the library is considered to be a stub, and its location will not be added as a runtime search path to dependents that link it.

Changed in version 3.28: If an imported target is an Apple framework or XCFramework, the preferred arrangement is to set IMPORTED_LOCATION to the .framework or .xcframework directory. CMake will then find the relevant .tbd file inside that framework or XCFramework automatically without requiring IMPORTED_IMPLIB to be set.

The IMPORTED_IMPLIB target property may be overridden for a given configuration <CONFIG> by the configuration-specific IMPORTED_IMPLIB_<CONFIG> target property. Furthermore, the MAP_IMPORTED_CONFIG_<CONFIG> target property may be used to map between a project's configurations and those of an imported target. If none of these is set then the name of any other configuration listed in the IMPORTED_CONFIGURATIONS target property may be selected and its IMPORTED_IMPLIB_<CONFIG> value used.

This property is ignored for non-imported targets.