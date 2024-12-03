# cmake install

[cmake install](https://cmake.org/cmake/help/latest/command/install.html)

### FILE_SET <set-name>

Added in version 3.23.

File sets are defined by the target_sources(FILE_SET) command. If the file set <set-name> exists and is PUBLIC or INTERFACE, any files in the set are installed under the destination (see below). The directory structure relative to the file set's base directories is preserved. For example, a file added to the file set as /blah/include/myproj/here.h with a base directory /blah/include would be installed to myproj/here.h below the destination.

### CXX_MODULES_BMI

Added in version 3.28.

Any module files from C++ modules from PUBLIC sources in a file set of type CXX_MODULES will be installed to the given DESTINATION. All modules are placed directly in the destination as no directory structure is derived from the names of the modules. An empty DESTINATION may be used to suppress installing these files (for use in generic code).

The install(TARGETS) command can also accept the following options at the top level:

### EXPORT

该选项将已安装的 target files 与名为 `<export-name>` 的 `export` 关联起来.
它必须出现在任何 target 选项之前.
要实际安装 export file(xxx.cmake)本身,
请调用 `install(EXPORT)`, 如下所述.
要更改导出目标的名称, 请参阅 `EXPORT_NAME` 目标属性的文档.

如果使用了 `EXPORT` 选项,
且目标包含 `PUBLIC` 或 `INTERFACE` file sets, 则必须使用 `FILE_SET` 参数指定所有文件集.
与目标相关的所有 PUBLIC 或 INTERFACE sets 都会包含在导出中.
