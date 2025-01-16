# cmake 预定义变量

## [CMAKE_CURRENT_LIST_FILE][def]

Full path to the listfile currently being processed.

As CMake processes the listfiles in your project this variable will always be set to the one currently being processed. The value has dynamic scope. When CMake starts processing commands in a source file it sets this variable to the location of the file. When CMake finishes processing commands from the file it restores the previous value. Therefore the value of the variable inside a macro or function is the file invoking the bottom-most entry on the call stack, not the file containing the macro or function definition.

See also CMAKE_PARENT_LIST_FILE.

## [INCLUDE_DIRECTORIES][def2]

List of preprocessor include file search directories.

This property specifies the list of directories given so far to the target_include_directories() command. In addition to accepting values from that command, values may be set directly on any target using the set_property() command. A target gets its initial value for this property from the value of the INCLUDE_DIRECTORIES directory property. Both directory and target property values are adjusted by calls to the include_directories() command.

The value of this property is used by the generators to set the include paths for the compiler.

Relative paths should not be added to this property directly. Use one of the commands above instead to handle relative paths.

Contents of INCLUDE_DIRECTORIES may use cmake-generator-expressions(7) with the syntax $<...>. See the cmake-generator-expressions(7) manual for available expressions. See the cmake-buildsystem(7) manual for more on defining buildsystem properties.

## [INTERFACE_INCLUDE_DIRECTORIES][def3]

List of public include directories requirements for a library.

Targets may populate this property to publish the include directories required to compile against the headers for the target. The target_include_directories() command populates this property with values given to the PUBLIC and INTERFACE keywords. Projects may also get and set the property directly.

When target dependencies are specified using target_link_libraries(), CMake will read this property from all target dependencies to determine the build properties of the consumer.

Contents of INTERFACE_INCLUDE_DIRECTORIES may use "generator expressions" with the syntax $<...>. See the cmake-generator-expressions(7) manual for available expressions. See the cmake-buildsystem(7) -manual for more on defining buildsystem properties.

Include directories usage requirements commonly differ between the build-tree and the install-tree. The BUILD_INTERFACE and INSTALL_INTERFACE generator expressions can be used to describe separate usage requirements based on the usage location. Relative paths are allowed within the INSTALL_INTERFACE expression and are interpreted relative to the installation prefix. For example:

```cmake
target_include_directories(mylib INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/mylib>
  $<INSTALL_INTERFACE:include/mylib>  # <prefix>/include/mylib
)
```

## ref

[def]: https://cmake.org/cmake/help/latest/variable/CMAKE_CURRENT_LIST_FILE.html
[def2]: https://cmake.org/cmake/help/latest/prop_tgt/INCLUDE_DIRECTORIES.html
[def3]: https://cmake.org/cmake/help/latest/prop_tgt/INTERFACE_INCLUDE_DIRECTORIES.html