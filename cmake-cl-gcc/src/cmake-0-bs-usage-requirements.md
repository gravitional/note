# [Target Usage Requirements][def]

The usage requirements of a target are settings that propagate to consumers, which link to the target via target_link_libraries(), in order to correctly compile and link with it. They are represented by transitive compile and link properties.

Note that usage requirements are not designed as a way to make downstreams use particular COMPILE_OPTIONS, COMPILE_DEFINITIONS, etc. for convenience only. The contents of the properties must be requirements, not merely recommendations.

See the Creating Relocatable Packages section of the cmake-packages(7) manual for discussion of additional care that must be taken when specifying usage requirements while creating packages for redistribution.

The usage requirements of a target can transitively propagate to the dependents. The target_link_libraries() command has PRIVATE, INTERFACE and PUBLIC keywords to control the propagation.

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

Because the archive is a PUBLIC dependency of archiveExtras, the usage requirements of it are propagated to consumer too.

Because serialization is a PRIVATE dependency of archiveExtras, the usage requirements of it are not propagated to consumer.

Generally, a dependency should be specified in a use of target_link_libraries() with the PRIVATE keyword if it is used by only the implementation of a library, and not in the header files. If a dependency is additionally used in the header files of a library (e.g. for class inheritance), then it should be specified as a PUBLIC dependency. A dependency which is not used by the implementation of a library, but only by its headers should be specified as an INTERFACE dependency. The target_link_libraries() command may be invoked with multiple uses of each keyword:

```c
target_link_libraries(archiveExtras
  PUBLIC archive
  PRIVATE serialization
)
```

Usage requirements are propagated by reading the INTERFACE_ variants of target properties from dependencies and appending the values to the non-INTERFACE_ variants of the operand. For example, the INTERFACE_INCLUDE_DIRECTORIES of dependencies is read and appended to the INCLUDE_DIRECTORIES of the operand. In cases where order is relevant and maintained, and the order resulting from the target_link_libraries() calls does not allow correct compilation, use of an appropriate command to set the property directly may update the order.

For example, if the linked libraries for a target must be specified in the order lib1 lib2 lib3 , but the include directories must be specified in the order lib3 lib1 lib2:

```c
target_link_libraries(myExe lib1 lib2 lib3)
target_include_directories(myExe
  PRIVATE $<TARGET_PROPERTY:lib3,INTERFACE_INCLUDE_DIRECTORIES>)
```

Note that care must be taken when specifying usage requirements for targets which will be exported for installation using the install(EXPORT) command. See Creating Packages for more.

## ref

[def]: https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#target-usage-requirements