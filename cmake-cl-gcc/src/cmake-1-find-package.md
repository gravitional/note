# cmake find_package

[如何为cmake提供package以便于find_package, 以及用VCPKG补充CMake实现快速下载集成](https://www.jianshu.com/p/3d90d05ed7cd)
[导入导出指南](https://cmake-doc.readthedocs.io/zh_CN/latest/guide/importing-exporting/index.html)

## CMake带来的改变

### 依赖关系的思维转变: 用仓库的概念代替目录层级依赖

层级依赖:

```bash
├── TaihuApp
│       └── Qt::Quick
│       └── Qt5::Core
│       └── Qt5::Widgets
│       └── opencv ─────────────────────────────────┐
│       └── logger ---------------------┐           |
│       └── gtest ────────────────┐ [duplicate]     |
│       └── camera                |     |       [duplicate]
│               └── opencv     ───|─────|───────────┘
│               └── baumer        |     |
│               └── tucsen  [duplicate] |
│               └── protocol      |     |
│               └── logger -------|─────┘
│               └── gtest ────────┘
```

扁平依赖:

```bash
├── Repository ────>──>──>──────│
│       └── Qt::Quick           |
│       └── Qt5::Core           |
│       └── Qt5::Widgets        |────────── First Project
│       └── camera              |
│       └── logger              |
│       └── gtest               |────────── Second Project
│       └── opencv              |
│       └── baumer              |
│       └── tucsen              |────────── Other project
│       └── protocol            |
│       └── any other libs      |
```

图一: 在每个项目里都存放一套自身需要的依赖库, 类似离线式依赖包含关系;
图二: camera依赖了opencv, baumer等别的库, 但不存在包含关系,
仓库里所有库的依赖关系都是通过配置进行关联的,
本质所有的库都在项目之外的仓库里存放的.

## 简洁优雅的库依赖集成方式

```cmake
project(camera VERSION 1.0.0)

find_package(protocol REQUIRED)
find_package(logger REQUIRED)
find_package(timer REQUIRED)
find_package(opencv REQUIRED)
find_package(baumer REQUIRED)
find_package(tucsen REQUIRED)

aux_source_directory(. SRC_LIST)
add_library(${PROJECT_NAME} STATIC ${SRC_LIST})

target_link_libraries(${PROJECT_NAME} PRIVATE
    protocol
    smt-logger
    smt-timer
    baumer
    tucsen
    opencv)

target_include_directories(${PROJECT_NAME} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>)
```

建议用 `target_include_directory()` 代替 `include_directory()`,
如果当前也是一个对外提供api的库

### 依赖库版本控制(vcpkg赋能)

```json
{
  "name": "project",
  "version-string": "1.0.0",
  "supports": "(x64 | arm64) & (linux | osx | windows)",
  "dependencies": [
    { "name": "zlib", "version>=": "1.2.11#9" },
    { "name": "fmt", "version>=": "7.1.3#1" }
  ]
}
```

允许指定当前库的对外名字, 版本, 适用于哪些平台系统,
以及依赖哪些别的库甚至那些库的指定版本

## 自己的库如何能被find_pakcage(xxx)

`cmake` 有两种方式让 `find_package(xxx)` 能找到库, 如果没有找到会报错, 如下:

`find_package(OpenCV)` 出现错误如下:

```bash
CMake Warning at CMakeLists.txt:37 (find_package):
  By not providing "FindOpenCV.cmake" in CMAKE_MODULE_PATH this project has
  asked CMake to find a package configuration file provided by "OpenCV", but
  CMake did not find one.

  Could not find a package configuration file provided by "OpenCV" with any of
  the following names:

    OpenCVConfig.cmake
    OpenCV-config.cmake

  Add the installation prefix of "OpenCV" to CMAKE_PREFIX_PATH or set "OpenCV_DIR"
  to a directory containing one of the above files.  If "OpenCV" provides a
  separate development package or SDK, be sure it has been installed.
```

简单翻译下:

`cmake` 优先会以 `Moudule模式` 寻找,
即: 搜索 `CMAKE_MODULE_PATH` 指定路径下的 `FindXXX.cmake` 文件, 默认路径按系统平台区分如下:

```bash
windows:  C:/Program Files/CMake/share/cmake-3.xx/Modules
linux: /usr/share/cmake-3.xx/Modules
```

一旦找到了 `FindXXX.cmake`, 则此库一般会提供以下变量, 目的是方便调用者快速集成它:

```cmake
<NAME>_FOUND
<NAME>_INCLUDE_DIRS or <NAME>_INCLUDES
<NAME>_LIBRARIES or <NAME>_LIBS
```

如果没能找到 `FindXXX.cmake`, 则尝试以 `Config模式`:
搜索指定路径下的 `XXXConfig.cmake` 或者 `XXX-config.cmake` 文件, 搜索路径优先是 `cmake install` 的路径:

```bash
windows: C:/Program Files
linux: /usr/local
```

当然也支持在项目里通过 `CMAKE_PREFIX_PATH` 指定了寻找路径,
或者直接通过设置 `XXX_DIR` 告知准确的查找路径.
其实, 还有一种做法是通过指定 `toolchain` 让 cmake 统一从toolchain里寻找.

### Config方式

这是一种基于有项目源码的方式, 需要为cmake组织的项目提供完整的install脚本,
当执行install时候会在install目的地的lib目录下创建share目录,
并在share目录里自动生成 `XXXConfig.cmake` 或者 `xxx-config.cmake` 等配置文件

`cmake install`的脚本相对比较通用, 一般只要加在cmake项目的实现模块的 `CMakeList.txt` 最下面即可, 如下:

```cmake
# ============================== 安装脚本 ==============================
set(HEADERS ${CMAKE_SOURCE_DIR}/include/swc_camera.h)
set_target_properties(${PROJECT_NAME} PROPERTIES PUBLIC_HEADER "${HEADERS}")

# Install the target and create export-set
install(TARGETS ${PROJECT_NAME}
    EXPORT ${PROJECT_NAME}Targets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    PUBLIC_HEADER DESTINATION include)

# Generate the version file for the config file
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
    VERSION ${PACKAGE_VERSION}
    COMPATIBILITY SameMajorVersion)

# Exporting Targets from the Build Tree
install(EXPORT ${PROJECT_NAME}Targets
    DESTINATION "lib/cmake/${PROJECT_NAME}")

# Create config file
configure_package_config_file(
    ${CMAKE_SOURCE_DIR}/Config.cmake.in ${PROJECT_NAME}Config.cmake
    INSTALL_DESTINATION "lib/cmake/${PROJECT_NAME}")

# Install config files
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
    DESTINATION "lib/cmake/${PROJECT_NAME}")
```

可以通过设置 `CMAKE_INSTALL_PATH` 指定库安装的位置,
`cmake install library` 的命令是 `cmake --build ./ --target install`,
在linux下配合 make 可以简化为 `make install`, 这是makefile支持的:

`cmake --build ./ --target install` 执行后如下:

```bash
PS E:\swc-camera\build> cmake --build ./ --target install
Microsoft (R) Build Engine version 16.11.1+3e40a09f8 for .NET Framework
Copyright (C) Microsoft Corporation. All rights reserved.

  swc-camera.vcxproj -> E:\swc-camera\build\src\Debug\swc-camera.lib
  example.vcxproj -> E:\swc-camera\build\example\Debug\example.exe
  tests.vcxproj -> E:\swc-camera\build\tests\Debug\tests.exe
  -- Install configuration: "Debug"
  -- Up-to-date: E:/LOCAL_REPOSITORY/lib/swc-camera.lib
  -- Up-to-date: E:/LOCAL_REPOSITORY/include/swc_camera.h
  -- Up-to-date: E:/LOCAL_REPOSITORY/lib/cmake/swc-camera/swc-cameraTargets.cmake
  -- Up-to-date: E:/LOCAL_REPOSITORY/lib/cmake/swc-camera/swc-cameraTargets-debug.cmake
  -- Up-to-date: E:/LOCAL_REPOSITORY/lib/cmake/swc-camera/swc-cameraConfig.cmake
  -- Up-to-date: E:/LOCAL_REPOSITORY/lib/cmake/swc-camera/swc-cameraConfigVersion.cmake
```

### Module方式

这是一种当第三方库仅仅提供了编译好的binary库时候,
有时候有些库编译过程非常复杂且依赖多而且非常耗时,
我们也可以用这种方式, 为了让 `find_package(xxx)` 找到它的方式.
我们需要写一个对应的 `FindXXX.cmake`,
在 `FindXXX.cmake` 里会指定尝试寻找库所在的路径,
一般非常主流的库cmake的modules目录会提供,
但以下三种情况需要自己编写 `FindXXX.cmake`:

`cmake` 的 `modules` 目录里提供的 `FindXXX.cmake` 描述的版本号和要用的不一致
非大众库, 如 `baumer` 或者tucsen, cmake是不可能提供FindXXX.cmake的

在linux/mac系统里, 大众库的 FindXXX.cmake一般存在 `/usr/share/cmake-3.xx/Modules`
在windows系统里, 大众库的 FindXXX.cmake存在 `C:\Program Files\CMake\share\cmake-3.xx\Modules`

### 自己编写的FindXXX.cmake放哪里

默认 `find_package(xxx)` 会优先从cmake的Modules目录查找,
意味着我们可以把自己的 `FindXXX.cmake` 放到cmake的Modules目录,
但更优雅的方式是跟着项目走.

在没有集成vcpkg的情况下, 我们可以在项目根目录创建一个cmake目录,
并将各种编写的`FindXXX.cmake`放于此处, 随后需要在项目的 `CMakeList.txt` 里告知 `FindXXX.cmake` 所在目录,
即: `list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")`,
当然有了vcpkg就简单多了, 只要为此库创建一个独立的仓库,
并将 `FindXXX.cmake` 直接放于其中, 后续通过 `vcpkg` 将其 `install` 即可.

### 如何编写FindXXX.cmake

其实, `FindXXX.cmake` 本质不一定要写,
因为 `FindXXX.cmake` 的主要目的是通过
`find_library` 和 `find_path` 指定库的 `头文件` 和 `binary` 所在路径,
但因为很多时候第三方库往往有很多头文件很多库文件而且还分debug/release,
不能像下面这种方式简单描述,
因此有必要提供一个独立的文件来描述库是怎么寻找和定义的,
这样能让库寻找和库使用完全分离解耦.

```cmake
find_path(TIFF_INCLUDE_DIR tiff.h
    /usr/local/include
    /usr/include)

find_library(TIFF_LIBRARY
    NAMES tiff tiff2
    PATHS /usr/local/lib /usr/lib)

include_directories(${TIFF_INCLUDE_DIRs})
add_executable(mytiff mytiff.c)
target_link_libraries(myprogram ${TIFF_LIBRARY})
```

因为实际的编写过程会很复杂, 取决于不同形式的库,
因此下面单独章节描述如何编写 `FindXXX.cmake`.

## 如何编写FindXXX.cmake

一个合格完整的 `FindXXX.cmake` 包含以下3个部分:

定义 `XXX_INCLUDE_DIRS` 和 `XXX_LIBRARIES`:
`find_path()` 每次只能获得一个头文件所在路径,
对于有很多头文件的库, 需要通过多次 `find_path` 找到各自路径, 并将它们合并为 `XXX_INCLUDE_DIRS`,
如果一个库有很多库文件, 那么也需要多次 `find_library()` 找到各个库对应的路径, 并将其合并为 `XXX_LIBRARIES`;

定义 `XXX_FOUND` 和 `XXX_VERSION`:
确认 `XXX_INCLUDE_DIRS` 和 `XXX_LIBRARIES` 都不为空, 再定义 `XXX_FOUND` 和 `XXX_VERSION`.

至此, library 已经可以被大幅简化集成,
只是集成时候需要导入 `XXX_INCLUDE_DIRS` 作为库头文件, 链接 `XXX_LIBRARIES` 作为库文件,
如果库区分Debug和Release, 那么cmake还要以optimize和debug方式依赖对应的库;

创建 `Target`: 确认 `XXX_FOUND` 不为空后再创建 `Target`,
通过 `add_library()` 定义库类型(`SHARED|STATIC|INTERFACE`),
通过 `set_target_properties()` 设置 `LIB` 的头文件路径,
静态库地址, 动态库地址, 共享库的地址以及 `DLL` 路径.
至此, 库的集成简易程度已和源码库完全一样.

在写 `FindXXX.cmake` 前需要分析提供的第三方库的特性,
根据不同的特性将会采取不同的方式编写FindXXX.cmake:

是否单个头文件或者单个库文件:
相对来说, 单个头文件和库文件的库写 `FindXXX.cmake` 会简洁很多,
一个 `find_path和find_library` 就能描述所有的依赖关系;

库文件是否区分debug和release:
只有windows库才有可能区分debug和release,
如果区分意味着需要让cmake能动态找到对应版本的库文件;

windows库除了静态库是否还有动态库:
在定义Target时候, 需要在property里设置静态库和动态库的文件路径

### 单头文件&单库文件&单dll的情况

[IMPORTED_LOCATION](https://cmake.org/cmake/help/latest/prop_tgt/IMPORTED_LOCATION.html)

其中 `IMPORTED_LOCATION` 是导入库的位置
Full path to the main file on disk for an `IMPORTED` target.
Set this to the location of an `IMPORTED` target file on disk.

```cmake
# FindOpenCV
# --------
#
# Find the opencv libraries
# Result Variables
# ^^^^^^^^^^^^^^^^
# The following variables will be defined:
#
# ``opencv_FOUND`` True if opencv found on the local system
# ``opencv_VERSION`` Version of opencv found
# ``opencv_INCLUDE_DIRS`` Location of opencv header files
# ``opencv_LIBRARIES`` List of the opencv libraries found

find_package(PkgConfig)

# ======================= define XXX_ROOT_DIR =======================
if (DEFINED ENV{LOCAL_REPOSITORY})
    set(opencv_ROOT_DIR $ENV{LOCAL_REPOSITORY})
endif()

if (DEFINED ENV{VCPKG_ROOT} AND DEFINED ENV{VCPKG_DEFAULT_TRIPLET})
    set(opencv_ROOT_DIR $ENV{VCPKG_ROOT}/installed/$ENV{VCPKG_DEFAULT_TRIPLET})
endif()

# ======================= find header files =======================
find_path(opencv_INCLUDE_DIR
    NAMES opencv2/opencv.hpp
    PATHS ${opencv_ROOT_DIR}/include /usr/local/include)

# ======================= find library files =======================
# define macro func to find libs
macro(opencv_FIND_LIBRARY libname)
    if(NOT opencv_${libname}_LIBRARY)
        find_library(opencv_${libname}_LIBRARY
            NAMES ${libname}
            PATHS ${opencv_ROOT_DIR}/lib /usr/local/lib)

        list(APPEND opencv_LIBRARY ${opencv_${libname}_LIBRARY})
    endif()
endmacro(opencv_FIND_LIBRARY)

if(WIN32)
    find_library(opencv_LIBRARY_DEBUG
        NAMES opencv_world412d.lib
        PATHS ${opencv_ROOT_DIR}/debug/lib /usr/local/lib)

    find_library(opencv_LIBRARY_RELEASE
        NAMES opencv_world412.lib
        PATHS ${opencv_ROOT_DIR}/lib /usr/local/lib)

    include(SelectLibraryConfigurations)
    select_library_configurations(opencv)
elseif(UNIX)
    # call macro func to find libs
    opencv_FIND_LIBRARY(libopencv_core.so)
    opencv_FIND_LIBRARY(libopencv_cudaarithm.so)
    opencv_FIND_LIBRARY(libopencv_cudafilters.so)
    opencv_FIND_LIBRARY(libopencv_cudaimgproc.so)
    opencv_FIND_LIBRARY(libopencv_highgui.so)
    opencv_FIND_LIBRARY(libopencv_imgcodecs.so)
    opencv_FIND_LIBRARY(libopencv_imgproc.so)
endif()

# ======================= find bin files =======================
if(WIN32)
    find_file(opencv_LIBRARY_DLL_DEBUG
        NAMES opencv_world412d.dll
        PATHS ${opencv_ROOT_DIR}/debug/bin)

    find_file(opencv_LIBRARY_DLL_RELEASE
        NAMES opencv_world412.dll
        PATHS ${opencv_ROOT_DIR}/bin)
endif()

# ======================= verify dependencies =======================
if (opencv_INCLUDE_DIR AND opencv_LIBRARY)
    set(opencv_FOUND TRUR CACHE BOOL "")
    set(opencv_VERSION "4.1.2" CACHE STRING "")

    set(opencv_INCLUDE_DIRS ${opencv_INCLUDE_DIR} CACHE STRING "")
    set(opencv_LIBRARIES ${opencv_LIBRARY} CACHE STRING "")

    find_package_handle_standard_args(opencv
        REQUIRED_VARS opencv_INCLUDE_DIRS opencv_LIBRARIES
        VERSION_VAR opencv_VERSION)
    mark_as_advanced(opencv_INCLUDE_DIRS opencv_LIBRARIES)
endif()

# ======================= create target =======================
if (opencv_FOUND)
    include(CMakePushCheckState)
    cmake_push_check_state()

    # set required properties
    set(CMAKE_REQUIRED_QUIET ${opencv_FIND_QUIETLY})
    set(CMAKE_REQUIRED_INCLUDES ${opencv_INCLUDE_DIRS})
    set(CMAKE_REQUIRED_LIBRARIES ${opencv_LIBRARIES})

    cmake_pop_check_state()

    if(NOT TARGET opencv)
        add_library(opencv SHARED IMPORTED)
        set_target_properties(opencv PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "${opencv_INCLUDE_DIRS}")

        if(opencv_LIBRARY_DEBUG)
            set_property(TARGET opencv APPEND PROPERTY IMPORTED_CONFIGURATIONS DEBUG)
            set_target_properties(opencv PROPERTIES
                IMPORTED_LOCATION_DEBUG "${opencv_LIBRARY_DLL_DEBUG}"
                IMPORTED_IMPLIB_DEBUG "${opencv_LIBRARY_DEBUG}")
        endif()

        if(opencv_LIBRARY_RELEASE)
            set_property(TARGET opencv APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
            set_target_properties(opencv PROPERTIES
                IMPORTED_LOCATION_RELEASE "${opencv_LIBRARY_DLL_RELEASE}"
                IMPORTED_IMPLIB_RELEASE "${opencv_LIBRARY_RELEASE}")
        endif()

        if(NOT opencv_LIBRARY_RELEASE AND NOT opencv_LIBRARY_DEBUG)
            set_property(TARGET opencv APPEND PROPERTY IMPORTED_LOCATION "${opencv_LIBRARY}")
        endif()
    endif()
endif()
```

备注1: `${opencv_ROOT_DIR}` 指向的库目录是动态的,
如果定义了 `VCPKG_ROOT`, 那么 `vcpkg` 就是库的寻找源;
如果未定义 `VCPKG_ROOT` 但定义了 `LOCAL_REPOSITORY`, 那么本地目录即为库寻找源;
若都没有定义, 那么头文件和库文件就只能从系统路径寻找了.
不管当前是哪个平台, 如: x64-windows, x86-windows, arm64-linux, x64-linux等等,
`${opencv_ROOT_DIR}` 下一般目录结构都是: `include`, `lib` 以及 `bin`.

### 批量寻找头文件: 模板化find_path()

因为 `find_path` 每次只能寻找一个头文件, 需要多次调用将最终结果合并为 `XXX_INCLUDE_DIRS`.
其实, 也可以如下通过定义宏或者函数批量寻找头文件:

```cmake
# ======================= find header files =======================
# define macro func to find headers
macro(baumer_FIND_INCLUDE varname foldername headername)
    if(NOT baumer_${foldername}_INCLUDE_DIR)
        find_path(baumer_${foldername}_INCLUDE_DIR
            NAMES ${foldername}/${headername}
            PATHS ${baumer_ROOT_DIR}/include /usr/local/include)

        list(APPEND baumer_INCLUDE_DIRS ${baumer_${foldername}_INCLUDE_DIR})
        list(REMOVE_DUPLICATES baumer_INCLUDE_DIRS)
    endif()
endmacro(baumer_FIND_INCLUDE)

# call macro func to find headers
baumer_FIND_INCLUDE(bgapi2_ext            bgapi2_ext      bgapi2_ext.h)
baumer_FIND_INCLUDE(bgapi2_ext_addons     bgapi2_ext      bgapi2_ext_addons.h)
baumer_FIND_INCLUDE(bgapi2_ext_sc         bgapi2_ext_sc   bgapi2_ext_sc.h)
baumer_FIND_INCLUDE(bgapi2_def            bgapi2_genicam  bgapi2_def.h)
baumer_FIND_INCLUDE(bgapi2_featurenames   bgapi2_genicam  bgapi2_featurenames.h)
baumer_FIND_INCLUDE(bgapi2_genicam        bgapi2_genicam  bgapi2_genicam.hpp)
```

最终, `baumer_INCLUDE_DIRS=E:\vcpkg\installed\x64-windows\include`,
你可能会疑惑那么多次的find_path定位到的include路径都是一样的, 不是浪费么.
No, 其实目的是确保每个头文件都能被 `find_path` 到.

以上脚本演示了将每个public的头文件被寻找到并成为 `baumer_INCLUDES` 的一部分,
库调用者 `include` 头文件则为 `#include <foldername/headername>`, 如: `#include <bgapi2_ext/bgapi2_ext.h>`

### 批量寻找库文件: 模板化find_library()

和find_path一样find_library每次只能寻找一个库文件,
需要多次调用将最终结果合并为 `XXX_LIBRARIES`.
同样也可以如下通过定义宏或者函数批量寻找库文件:

```cmake
# ---------------- find library files----------------
# define macro func to find libs
macro(baumer_FIND_LIBRARY libname)
  if(NOT baumer_${libname}_LIBRARY)
     find_library(baumer_${libname}_LIBRARY
      NAMES ${libname}
      PATHS ${baumer_ROOT_DIR}/lib /usr/local/lib)

     list(APPEND baumer_LIBRARIES ${baumer_${libname}_LIBRARY})
  endif()
endmacro(baumer_FIND_LIBRARY)

# call macro func to find libs
Baumer_FIND_LIBRARY(bgapi2_ext.lib)
Baumer_FIND_LIBRARY(bgapi2_ext_sc.lib)
Baumer_FIND_LIBRARY(bgapi2_genicam.lib)
```

最终,

```bash
baumer_LIBRARIES=E:\vcpkg\...\bgapi2_genicam.lib
```

### 寻找库文件, 但区分DEBUG和RELEASE

对于一些区分Debug和Release的windows库,
我们不能一昧用 `xxx_LIBRARIES` 描述所有库文件:

```cmake
find_library(serial_LIBRARY_DEBUG
    NAMES seriald.lib
    PATHS ${serial_ROOT_DIR}/debug/lib /usr/local/lib)

find_library(serial_LIBRARY_RELEASE
    NAMES serial.lib
    PATHS ${serial_ROOT_DIR}/lib /usr/local/lib)

include(SelectLibraryConfigurations)
select_library_configurations(serial)
```

`select_library_configurations(xxx)`的引入
使得 `cmake configure` 会自动选取对应版本的库赋值与 `xxx_LIBRARY`,
意味着 `target_link_library` 时候不用区分optimize和debug的库

至此, 库的集成将大幅简化,
因为可以做到了屏蔽不同路径的头文件不同路径, 不同名字的库文件, 甚至不用区分系统平台:

```cmake
find_package(XXX)
target_include_directory(app ${XXX_INCLUDE_DIRS})
target_link_library(app PRIVATE
     optimize ${XXX_LIBRARIES_RELEASE}
     debug ${XXX_LIBRARIES_DEBUG})
```

### 创建Target

为了彻底, 完全做到跟源码库集成一样简洁:
省略头文件导入和不区分debug/release, 我们可以手动创建 `Target`.

#### 只有单个static或者单个so的Target

```cmake
if(NOT TARGET xxx)
    add_library(xxx SHARED|STATIC|UNKNOWN IMPORTED)
    set_target_properties(xxx PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${xxx_INCLUDE_DIRS}"
        IMPORTED_LOCATION "${xxx_LIBRARY}")
endif()
```

`add_library()` 时候可以指定library是SHARED库, STATIC库,
如果不确定那种也可以填UNKNOWN, 因为 `SHARED` 库和 `STATIC` 库的必要设置的property是一样的

#### 只有单个dll和单个lib的Target

```cmake
if(NOT TARGET xxx)
    add_library(xxx SHARED IMPORTED)
    set_target_properties(xxx PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${xxx_INCLUDE_DIRS}"
        IMPORTED_LOCATION "${xxx_LIBRARY_DLL}"
        IMPORTED_IMPLIB "${xxx_LIBRARY}")
endif()
```

#### 有多个static和多个dll的Target

```cmake
if(NOT TARGET xxx)
    add_library(xxx INTERFACE IMPORTED)
    set_target_properties(xxx PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${xxx_INCLUDE_DIRS}"
        INTERFACE_LINK_LIBRARIES "${xxx_LIBRARIES}"
        IMPORTED_LOCATION "${xxx_LIBRARY_DLLS}")
endif()
```

#### 区分Debug/Release的Target

```cmake
if(NOT TARGET xxx)
    add_library(xxx STATIC IMPORTED)
    set_target_properties(xxx PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "${xxx_INCLUDE_DIRS}")

    if(xxx_LIBRARY_DEBUG)
        set_property(TARGET xxx APPEND PROPERTY IMPORTED_CONFIGURATIONS DEBUG)
        set_target_properties(xxx PROPERTIES IMPORTED_LOCATION_DEBUG "${xxx_LIBRARY_DEBUG}")
    endif()

    if(xxx_LIBRARY_RELEASE)
        set_property(TARGET xxx APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
        set_target_properties(xxx PROPERTIES IMPORTED_LOCATION_RELEASE "${xxx_LIBRARY_RELEASE}")
    endif()

    if(NOT xxx_LIBRARY_RELEASE AND NOT xxx_LIBRARY_DEBUG)
        set_property(TARGET xxx APPEND PROPERTY IMPORTED_LOCATION "${xxx_LIBRARY}")
    endif()
endif()
```

这是一个典型的兼容Windows和Linux的Target创建案例,
因为很多时候Windows库区分Debug和Release, 而Linux的SO库是不区分的.

至此, 库的集成将变为如下方式:

```cmake
find_package(xxx REQUIRED)
target_link_library(app xxx)
```

## vcpkg

CMake提供了依赖库寻找的功能, 当找不到依赖库时只会报错,
并不会自动下载依赖库, 因为CMake不是真正的包管理器, 顶多是一个包寻找器,
所以得配合vcpkg, vcpkg是宗旨是即时编译当前或者指定平台的库,
vcpkg内部提供了众多下载依赖途径选项, 如: http, ftp, gitlab, github, bitbucket, svn, cvs等等.

### 如何让vcpkg和cmake结合让库依赖自动化

vcpkg的详细功能丰富, 可自行查看官网, 这里只指导如何快速上手创建私有port,
并让vcpkg和cmake集成达到编译即下载依赖库的作用, 我们分别以2种案例来介绍:

基于源码构建的库: 相对更简单
基于只有头文件和二进制库文件的第三方库: 需要区分多平台独立, 复杂一些

### 创建私有port

所谓port就是一个以库名为名字的目录, 里面放此库的获取来源配置,
版本及依赖关系配置, 使用说明等, 创建一个私有port的步骤如下:

在你的C++项目根目录创建一个文件夹叫vcpkg-ports,
如果你的库名叫xxx, 那么在vcpkg-ports里创建一个目录叫xxx,
那么这个xxx即是一个port.

在port目录里创建一个名为portfile.cmake的文本文件,
在此配置文件里定义库代码或者二进制获取方式, 以及相关配置.

`portfile.cmake` (以源码方式将vcpkg和cmake集成):

```cmake
set(SOURCE_PATH ${CURRENT_BUILDTREES_DIR}/src/${PORT})

vcpkg_from_git(
    OUT_SOURCE_PATH SOURCE_PATH
    URL ssh://git@lmsman-bitbucket01.europe.leicams.com:7999/lmswet-bitbucket01/~fei.zhang/smt-timer.git
    REF 9e950a8a52b7304e7da2ab59fd485f39095dca9b
    HEAD_REF master
)

# configure project and try to enable ninja
vcpkg_configure_cmake(
    SOURCE_PATH ${SOURCE_PATH}
    PREFER_NINJA)

vcpkg_install_cmake()
vcpkg_copy_pdbs()

# relocate target to vcpkg
vcpkg_fixup_cmake_targets(
    CONFIG_PATH lib/cmake/${PORT}
    TARGET_PATH /share/${PORT})

# remove headers in debug mode
file(REMOVE_RECURSE ${CURRENT_PACKAGES_DIR}/debug/include)

# install license and copyright
file(
    INSTALL ${SOURCE_PATH}/COPYING
    DESTINATION ${CURRENT_PACKAGES_DIR}/share/${PORT}
    RENAME copyright)
用已经编译好的二进制库的方式将vcpkg和cmake集成:

set(SOURCE_PATH "${CURRENT_BUILDTREES_DIR}/src/${PORT}")

vcpkg_from_git(
    OUT_SOURCE_PATH SOURCE_PATH
    URL git@10.10.231.59:fei.zhang/baumer.git
    REF 2d2d0a217da810f1ea71ca807c5337aed3e84e12
    HEAD_REF master
)

if (WIN32)
    # install headers
    file(GLOB_RECURSE HEADERS "${SOURCE_PATH}/windows/include/*")
    foreach(HEADER ${HEADERS})
         get_filename_component(ABSOLUTE_DIR ${HEADER} DIRECTORY)
         string(REGEX MATCH "include.+" NODE_DIR ${ABSOLUTE_DIR})
         file(INSTALL ${HEADER} DESTINATION "${CURRENT_PACKAGES_DIR}/${NODE_DIR}")
    endforeach()

    # install libs
    file(GLOB_RECURSE LIBRARIES "${SOURCE_PATH}/windows/lib/*")
    file(INSTALL ${LIBRARIES} DESTINATION "${CURRENT_PACKAGES_DIR}/lib")

    # install dlls
    file(GLOB_RECURSE BINS "${SOURCE_PATH}/windows/bin/*")
    file(INSTALL ${BINS} DESTINATION "${CURRENT_PACKAGES_DIR}/bin/${PORT}")
elseif (UNIX)
    # install headers
    file(GLOB_RECURSE HEADERS "${SOURCE_PATH}/linux/include/*")
    foreach(HEADER ${HEADERS})
         get_filename_component(ABSOLUTE_DIR ${HEADER} DIRECTORY )
         string(REGEX MATCH "include.+" NODE_DIR ${ABSOLUTE_DIR})
         file(INSTALL ${HEADER} DESTINATION "${CURRENT_PACKAGES_DIR}/${NODE_DIR}")
    endforeach()

    # install libs
    file(GLOB_RECURSE LIBRARIES "${SOURCE_PATH}/linux/lib/*")
    file(INSTALL ${LIBRARIES} DESTINATION "${CURRENT_PACKAGES_DIR}/lib")
endif()

# install FindXXX.cmake allow App can find pakcage via find_package(xxx)
file(INSTALL ${SOURCE_PATH}/FindBaumer.cmake DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")

# relocate the directory for cmake to find FindXXX.cmake
file(INSTALL ${CMAKE_CURRENT_LIST_DIR}/vcpkg-cmake-wrapper.cmake DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")

# install cmake integration usage
file(INSTALL ${CMAKE_CURRENT_LIST_DIR}/usage DESTINATION "${CURRENT_PACKAGES_DIR}/share/${PORT}")

# allow incomplete packages to pass validation
set(VCPKG_POLICY_EMPTY_PACKAGE enabled)
```

在此port目录里创建一个名为 `vcpkg.json` 的文本文件,
在此配置里定义当前库的基础信息以及依赖关系等,
如下以swc-camera的vcpkg.json为例
(如果此库没有其他任何依赖且没有版本, 可以不用提供):

```json
{
    "name": "swc-camera",
    "version-string": "v1.0.0",
    "description": "swc-camera is an integrated camera feature sdk",
    "dependencies": [
        "protocol",
        "smt-logger",
        "smt-timer",
        "opencv",
        "baumer",
        "tucsen",
        "gtest"
    ]
}
```

### 如何通过vcpkg自动下载库和依赖库

以swc-camera为例, 安装命令为: `vcpkg install swc-camera --overlay-ports=vcpkg-ports`
当看到如下过程log, 意味着下载 `swc-camera` 以及其依赖库已经开始工作了

`--port-overlay=vcpkg-ports` 作用是指定vcpkg安装库,
库的port配置来自 `vcpkg-ports` 目录,
如果项目不依赖任何私有托管的仓库, 则不用指定 `--port-overlay`

```bash
PS E:\vcpkg> .\vcpkg.exe install swc-camera --overlay-ports=vcpkg-ports
Computing installation plan...
The following packages will be built and installed:
  * baumer[core]:x64-windows -> v1.0.0 -- E:\vcpkg\vcpkg-ports\baumer
  * opencv[core]:x64-windows -> v4.1.2 -- E:\vcpkg\vcpkg-ports\opencv
  * smt-logger[core]:x64-windows -> v1.0.0 -- E:\vcpkg\vcpkg-ports\smt-logger
  * smt-timer[core]:x64-windows -> v1.0.0 -- E:\vcpkg\vcpkg-ports\smt-timer
    swc-camera[core]:x64-windows -> v1.0.0 -- E:\vcpkg\vcpkg-ports\swc-camera
  * tucsen[core]:x64-windows -> v1.0.0 -- E:\vcpkg\vcpkg-ports\tucsen
Additional packages (*) will be modified to complete this operation.
Detecting compiler hash for triplet x64-windows...
```

当最后出现如下信息, 意味着swc-camera安装成功:

```bash
Total elapsed time: 16.2 s

The package swc-camera:x64-windows provides CMake targets:

    find_package(swc-camera CONFIG REQUIRED)
    target_link_libraries(main PRIVATE swc-camera)
```

### 如何通过vcpkg卸载库

以swc-camera为例, 卸载命令为: `vcpkg remove swc-camera --port-overlay=vcpkg-ports`

需要注意的是remove不会将依赖库一同卸载, 因为cmake库依赖都是引用依赖,
不是包含依赖, 你不用的库可能别的项目在用.

### 如何将vcpkg集成到cmake项目中

```cmake
# use local repository if defined
if (DEFINED ENV{LOCAL_REPOSITORY})
  set(CMAKE_PREFIX_PATH $ENV{LOCAL_REPOSITORY})
endif()

# preferred to use vcpkg if defined
if(DEFINED ENV{VCPKG_ROOT})
    set(CMAKE_TOOLCHAIN_FILE "$ENV{VCPKG_ROOT}/scripts/buildsystems/vcpkg.cmake")
endif()

---------------------------------------------------------------------

project(testApp)
find_package(smt-logger)
add_executable(${PROJECT_NAME} main.cpp)
target_link_libraries(${PROJECT_NAME} smt-logger)
```

如上, 演示了如何集成vcpkg到cmake项目,
同时也指定了本地统一库寻找目录, 且有寻找优先级.

首先, find_package()默认优先会尝试从vcpkg里寻找,
假设系统环境变量定义了vcpkg根目录 --- `VCPKG_ROOT`
随后, 若VCPKG_ROOT 未定义(假设你不喜欢vcpkg, 想自己折腾),
则尝试从本地统一库寻找目录里寻找, 假设系统环境变量定义了统一库寻找目录 --- `LOCAL_REPOSITORY`
如果以上环境变量都没有, 那么则尝试找默认的路径,
linux从/usr/local里找, windows从C:/Program File里找
