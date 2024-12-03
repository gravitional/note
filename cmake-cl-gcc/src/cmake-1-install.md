# cmake install

[CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT](https://cmake.org/cmake/help/latest/variable/CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT.html)

当 `CMAKE_INSTALL_PREFIX` 刚刚被初始化为默认值时, `CMake` 会将此变量设置为 `TRUE`,
这通常发生在 CMake 在新的 build tree 中首次运行时,
并且在此时的 `CMake` 首次运行中, 未设置 `CMAKE_INSTALL_PREFIX` 环境变量.
项目代码可以使用它来更改默认值, 而无需覆盖用户提供的值:

```cmake
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set_property(CACHE CMAKE_INSTALL_PREFIX PROPERTY VALUE "/my/default")
endif()
```
