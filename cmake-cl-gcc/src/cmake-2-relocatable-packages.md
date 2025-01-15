# [创建可重定位软件包][def], Creating Relocatable Packages

Relocatable软件包不得引用 构建软件包的机器上文件的绝对路径,
而这些文件在安装软件包的机器上是不存在的.

通过 `install(EXPORT)` 创建的软件包是可重定向的,
使用的路径 相对于 软件包所在的目录.
在定义 target 的 用于 `EXPORT` 的 interface时,
请注意应将包含目录指定为相对于 `CMAKE_INSTALL_PREFIX` 的相对路径:

```cmake
target_include_directories(tgt INTERFACE
  # Wrong, not relocatable:
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include/TgtName>
)

target_include_directories(tgt INTERFACE
  # Ok, relocatable:
  $<INSTALL_INTERFACE:include/TgtName>
)
```

`$<INSTALL_PREFIX>` 生成器表达式可用作安装前缀的占位符,
而不会导致软件包不可重定位.
如果使用复杂的生成器表达式, 这一点很有必要:

```cmake
target_include_directories(tgt INTERFACE
  # Ok, relocatable:
  $<INSTALL_INTERFACE:$<$<CONFIG:Debug>:$<INSTALL_PREFIX>/include/TgtName>>
)
```

这也适用于引用外部dependencies的路径.
建议不要在 `INTERFACE_INCLUDE_DIRECTORIES` 和 `INTERFACE_LINK_LIBRARIES` 等
可能包含 paths 的属性中填入与 dependencies 相关的路径.
例如, 对于可重置软件包, 这段代码可能无法正常工作:

```cmake
target_link_libraries(ClimbingStats INTERFACE
  ${Foo_LIBRARIES} ${Bar_LIBRARIES}
  )
target_include_directories(ClimbingStats INTERFACE
  "$<INSTALL_INTERFACE:${Foo_INCLUDE_DIRS};${Bar_INCLUDE_DIRS}>"
  )
```

引用的变量可能包含, 在制作软件包的机器上找到的, 库和包含目录的绝对路径.
这样创建的软件包将包含硬编码的依赖路径, 不适合重新定位.

理想情况下, 这些 dependencies 应通过它们自己的 IMPORTED targets 来使用,
这些目标有自己的 `IMPORTED_LOCATION` 和 usage requirement 属性,
如 `INTERFACE_INCLUDE_DIRECTORIES`.
然后, 这些导入的目标可与 ClimbingStats 的 `target_link_libraries()` 命令一起使用:

```cmake
target_link_libraries(ClimbingStats INTERFACE Foo::Foo Bar::Bar)
```

通过这种方法, 软件包仅通过 IMPORTED 目标的名称来引用其外部 dependencies.
当 用户方 使用已安装的软件包时, 用户方 将运行相应的 `find_package()` 命令
(通过上述 `find_dependency` 宏)来查找依赖关系, 并在自己的机器上用相应的路径填充 imported targets.

遗憾的是, 许多随 CMake 一起发布的模块尚未提供 IMPORTED targets, 因为它们的开发早于这种方法.
随着时间的推移, 这种情况可能会逐步改善. 使用此类模块创建可重置软件包的变通方法包括

+ 在构建软件包时, 将每个 `Foo_LIBRARY` 缓存条目指定为 library name, 例如 `-DFoo_LIBRARY=foo`.
这样, 相应的 find模块就会在 `Foo_LIBRARIES` 中只填入 `foo`, 以要求链接器搜索库, 而不是硬编码路径.
+ 或者, 在安装软件包内容之后, 但在创建 软件包安装二进制文件 用于再分发之前,
用占位符手动替换绝对路径, 以便安装工具在安装软件包时进行替换.

## reference

[def]: https://cmake.org/cmake/help/latest/manual/cmake-packages.7.html#creating-relocatable-packages