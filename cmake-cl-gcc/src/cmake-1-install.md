# cmake install

[CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT](https://cmake.org/cmake/help/latest/variable/CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT.html)

CMake sets this variable to a TRUE value when the CMAKE_INSTALL_PREFIX has just been initialized to its default value, typically on the first run of CMake within a new build tree and the CMAKE_INSTALL_PREFIX environment variable is not set on the first run of CMake. This can be used by project code to change the default without overriding a user-provided value:

```cmake
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set_property(CACHE CMAKE_INSTALL_PREFIX PROPERTY VALUE "/my/default")
endif()
```
