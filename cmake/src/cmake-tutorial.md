# Modern CMake全攻略

[Modern CMake全攻略](https://zhuanlan.zhihu.com/p/129367287)

## 前言

接触`Cmake`是一个偶然事件, 平时我都是手撸`Make`或者人工添加指定`cpp文件`到`VS`的工程目录.
然而在后续工作效率提升的思考中, 我不断发现, 想要快速发布版本构建, 使用命令行无疑是最快捷
的, 并不需要频繁挪动鼠标去做一系列重复而低效的`劳动.

在一顿常规资料搜索后, 才了解到`Modern Cmake`运动(让CMAKE更加规范, 控制各级构建的变量泄
露), 实际上相关的资料乏善可陈, 众多模板又参差不齐;最终我想总结下相关的经验来做一个最合适
的入门级`Modern Cmake`模板.

我的工作虽然是`Win/Linux`混合C++开发, 但是当前主要是在`Win`下发布版本, 所以描述上会优先考
虑`Win`的操作细节, 后续再补充`Linux`部分的.

## Cmake是啥

Cmake是一个构建生成器, 它可以跨平台地生成各种构建需要的构建工程配置(MSVC, MAKE,
NINJIA), 然后各个平台的编译器就可以使用对应的构建文件来编译源码, 最终生成可执行文件.

经过多年的发展, CMAKE 已经形成了一套工具集, 使得开发人员可以完全控制项目的生命周期:

- `CMake`; 描述如何在主要硬件和操作系统上配置, 构建和安装项目.
- `CTest`; 定义测试, 测试套件, 并设置如何执行.
- `CPack`; 满足打包需求.
- `CDash`; 将项目的测试结果发布为 `DashBoard`.

## 安装与执行

### 安装

在[官网发布地址](https://cmake.org/download/)下载对应操作系统的`CMAKE`版本并安装就行,
如果是在`Windows`下, 个人建议安装`.msi`版本, 简单易操作.

关于运行`Cmake`的方法,官网也有一些[操作指南](https://cmake.org/runningcmake/)

如果是Win系统, 可以在本地cmake安装目录下doc找到一些帮助手册,在命令行下输入`cmake --help`也
可以查看帮助手册.

### 执行

这里我准备了一个CMAKE模板, 建议下载 代码 后在本地运行, 再次强调这是win版本的.

#### build

Cmake分为内部构建和外部构建两种方式, 其中内部构建就是在当前文件夹下寻找CMakelists.txt,同时
生成一堆中间文件来干扰项目文件目录, 所以大家推荐使用一个文件夹(build)来统一安放这些中间
文件.

所以一般操作如下

``` shell
cd build
cmake ..

#实际上, 对于最新版本的VS2019,cmake需要提前设定执行文件的操作位数以及DEBUG/RELEASE模式
#絮絮叨叨: 操作位数涉及到源代码中变量的大小, 32位操作系统下long有4字节, 64位下则为8字节.
# RELEASE下的可执行文件经过了编译器优化, 底层机器码与源码不一定对应.
#最终VS2019的版本如下
cd build
cmake .. -A win32
```

#### install

CMAKE生成MSVC平台配置时, 解决方案会默认包含ALL_BUILD, ZERO_CHECK两个工程, 当你在
CMakeLists.txt使用了`install()`命令后, 生成的VS工程里会添加一个INSTALL项目.

- ZERO_CHECK, 基本工程, 跟踪CmakeLists.txt的改动情况, 如果出现了修改, 就重新执行CMAKE刷新
  工程文件
- ALL_BUILD,全编译, 除了编译工程文件外, 对生成的工程文件会启动编译生成执行文件
- INSTALL, 手动启动, 根据指示把编译工程的产物(执行文件, 头文件等)输出到指定目录

#### Ctest && CPack && CDash

##### Ctest

在项目根目录的CMakeLists.txt文件中调用一系列的add_test 命令.

``` cmake
#启用测试
enable_testing()
#测试程序是否成功运行
add_test(test_run demo 5 2)
#测试帮助信息是否可以正常提示
add_test(test_usage demo)
set_tests_properties(test_usage
PROPERTIES PASS_REGULAR_EXPRESSION "Usage: .* base exponent")
#测试5的平方
add_test(test_5_2 demo 5 2)
set_tests_properties(test_5_2
PROPERTIES PASS_REGULAR_EXPRESSION "is 25")
#测试10的5次方
add_test(test_10_5 demo 10 5)
set_tests_properties(test_10_5
PROPERTIES PASS_REGULAR_EXPRESSION "is 100000")
#测试2的10次方
add_test(test_2_10 demo 2 10)
set_tests_properties(test_2_10
PROPERTIES PASS_REGULAR_EXPRESSION "is 1024")

# 定义一个宏, 用来简化测试工作
macro (do_test arg1 arg2 result)
add_test (test_${arg1}_${arg2} demo ${arg1} ${arg2})
set_tests_properties (test_${arg1}_${arg2}
PROPERTIES PASS_REGULAR_EXPRESSION ${result})
endmacro (do_test)

# 利用 do_test 宏, 测试一系列数据
do_test (5 2 "is 25")
do_test (10 5 "is 100000")
do_test (2 10 "is 1024")

```

##### CPack

用于配置生成各种平台上的安装包, 包括二进制安装包和源码安装包. 首先在顶层的CMakeLists.txt文
件尾部添加下面几行:

``` cmake
# 构建一个 CPack 安装包
include (InstallRequiredSystemLibraries)
set (CPACK_RESOURCE_FILE_LICENSE
"${CMAKE_CURRENT_SOURCE_DIR}/License.txt")
set (CPACK_PACKAGE_VERSION_MAJOR "${Demo_VERSION_MAJOR}")
set (CPACK_PACKAGE_VERSION_MINOR "${Demo_VERSION_MINOR}")
include (CPack)
```

License.txt

``` txt
The MIT License (MIT)

Copyright (c) 2018 Scorpio Studio

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

```

生成二进制安装包:
> cpack -C CPackConfig.cmake生成源码安装包: cpack -C CPackSourceConfig.cmake

##### CDash

为了添加Dashboard 的支持, 我们需要在顶层CMakeLists.txt文件中添加CTest 模块

``` cmake
# enable dashboard scripting
include (CTest)
```

CTestConfig.cmake文件, 在其中我们可以为dashboard指定项目的名称

```cmake
#===================================================================================

# Replace the value for CTEST_PROJECT_NAME with your project name at <http://cdash.nersc.gov>

#===================================================================================

set(CTEST_PROJECT_NAME "TiMemory") set(CTEST_NIGHTLY_START_TIME "01:00:00 UTC")
set(CTEST_DROP_METHOD "https") set(CTEST_DROP_SITE "http://cdash.nersc.gov")
set(CTEST_DROP_LOCATION "/submit.php?project=${CTEST_PROJECT_NAME}")
set(CTEST_DROP_SITE_CDASH TRUE) set(CTEST_CDASH_VERSION "1.6")
set(CTEST_CDASH_QUERY_VERSION TRUE)
```

CTestCustom.cmake

``` cmake
set(CTEST_CUSTOM_MAXIMUM_NUMBER_OF_ERRORS "200" )
set(CTEST_CUSTOM_MAXIMUM_NUMBER_OF_WARNINGS "500" )
set(CTEST_CUSTOM_MAXIMUM_PASSED_TEST_OUTPUT_SIZE "104857600") # 100 MB
set(CTEST_CUSTOM_COVERAGE_EXCLUDE "")

# either customize these directly or write as CMake Template
# and use configure_file(... @ONLY) with CMake
set(CTEST_SOURCE_DIRECTORY "/path/to/source")
set(CTEST_BINARY_DIRECTORY "/path/to/source")
# build options
set(OPTION_BUILD "-j8")
# define generator (optional), e.g. default to 'Unix Makefiles' on UNIX, Visual Studio on Windows
set(CTEST_GENERATOR "...")
# submit under Continuous, Nightly (default), Experimental
set(CTEST_MODEL "Continuous")
# define how to checkout code, e.g. copy a directory, git pull, svn co, etc.
set(CTEST_CHECKOUT_COMMAND "...")
# define how to update (optional), e.g. git checkout <git-branch>
set(CTEST_UPDATE_COMMAND "...")
# define how to configure (e.g. cmake -DCMAKE_INSTALL_PREFIX=...)
set(CTEST_CONFIGURE_COMMAND "...")
# the name of the build
set(CTEST_BUILD_NAME "...")
# how to configure
set(CTEST_CONFIGURE_COMMAND "...")
# how to build
set(CTEST_BUILD_COMMAND "...")
# default max time each tests can run (in seconds)
set(CTEST_TIMEOUT "7200")
# locale to English
set(ENV{LC_MESSAGES} "en_EN")

```

Dashboard.cmake

```cmake
ctest_read_custom_files(${CTEST_BINARY_DIRECTORY})

ctest_start (${CTEST_MODEL} TRACK ${CTEST_MODEL})
ctest_configure (BUILD ${CTEST_BINARY_DIRECTORY} RETURN_VALUE ret_con)
ctest_build (BUILD ${CTEST_BINARY_DIRECTORY} RETURN_VALUE ret_bld)

if(ret_bld)
# add as desired
ctest_test (BUILD ${CTEST_BINARY_DIRECTORY} RETURN_VALUE ret_tst)
ctest_memcheck (BUILD ${CTEST_BINARY_DIRECTORY} RETURN_VALUE ret_mem)
ctest_coverage (BUILD ${CTEST_BINARY_DIRECTORY} RETURN_VALUE ret_cov)

# attach build notes if desired, e.g. performance info, output files from tests
list(APPEND CTEST_NOTES_FILES "/file/to/attach/as/build-note")
endif()

# standard submit
ctest_submit(RETURN_VALUE ret_sub)
# if dashboard requires a token that restricts who can submit to dashboard
ctest_submit(RETURN_VALUE ret_sub HTTPHEADER "Authorization: Bearer ${CTEST_TOKEN}")

```

ctest -S Dashboard.cmake

运行ctest –D Experimental. 结果的dashboard 将会上传到[public
dashboard](https://open.cdash.org/index.php?project=PublicDashboard)

## 初识CMAKE

### 项目结构

良好的项目结构是后续项目之间完美依赖的基础. 我参考了很多知名的开源项目, 为了方便自己编译生
成打包, 也为了第三方可以方便使用, 总结出了一个不错的方案:

``` txt
.
\-- .git
\-- README.md #说明文件
\---+(PROJECT)
\-- CMakeLists.txt #根目录的CMake文件, CMAKE命令是被指定从这个文件开始执行.
\-- License.txt #权限介绍
\-- README.md #说明文件
\-- assets #资源文件
\-- build #CMAKE外部构建的文件夹, 用来存储CMAKE产生的中间文件
\-- config #配置文件
\-- log #log配置
\-- pack #打包文件夹
\-- include #代码引用的文件夹, 使用一些库时需要的头文件
\-- src #当前工程所需的源码文件
\-- bin #二进制发布文件夹
\-- lib #动态库或者静态库文件夹
\-- dfx #测试或者性能维护
+ bench
+ tests
\-- extern #第三方源码依赖的库源码
\-- cmake #安放cmake相关的模块
\-- scripts #执行脚本
\-...
```

### modern 规范

modern cmake是围绕着target这一概念而成, 不再是过程式配置, 更多地像一个类似于对象的概念, 针
对一个构建对象配置不同的源文件, 引用目录和编译选项.

### 生成构建对象

- add_executable() #生成可执行文件
- add_library() #生成库

### 配置构建对象

target_sources() #设置源码文件target_include_directories() #设置引用目录
target_compile_definitions() #设置预定义target_compile_features() #设置编译功能
target_compile_options() #设置编译选项target_link_libraries() #设置链接库
target_link_directories() #设置链接目录target_link_options() #设置链接选项

### 构建对象固有属性

get_target_property() #获取构建对象的属性set_target_properties() #设置构建对象的属性

### 辅助添加函数

find_package() #查找CMAKE_FRAMEWORK_PATH对应的库aux_source_directory() #用于自动生成目录源
文件集合add_subdirectory() #跳转到子目录

### 构建对象依赖

- Build-Requirements:  包含了所有构建Target必须的材料. 如源代码, include路径, 预编译命令,
  链接依赖, 编译/链接选项, 编译/链接特性等.
- Usage-Requirements: 包含了所有使用Target必须的材料. 如源代码, include路径, 预编译命令,
  链接依赖, 编译/链接选项, 编译/链接特性等. 这些往往是当另一个Target需要使用当前target时,
  必须包含的依赖.

属性依赖传递

- INTERFACE : 表示Target的属性不适用于自身, 只使用于依赖其的target. 自己只用到头文件, 被依
  赖时可能使用部分功能.
- PRIVATE : 表示Target的属性只定义在当前Target中, 任何依赖当前Target的Target不共享PRIVATE
  关键字下定义的属性. 只给自己生成时使用, 依赖自己时不知道自己的依赖.
- PUBLIC : 表示Target的属性既是build-requirements也是usage-requirements. 凡是依赖于当前
  Target的Target都会共享本属性. 自己生成时要用, 依赖自己时也要用.

## 高级功能

### find_package模式

find_package()有Module模式(基本用法)和Config模式(完全用法), 其中Module模式是基础, Config
模式则提供复杂高级功能.  find_package是否使用Config模式可以通过下列条件判断:
(1)find_package()中指定CONFIG关键字(2)find_package()中指定NO_MODULE关键字
(3)find_package()中使用了不再Module模式下所有支持配置的关键字

### find_package的Module模式

>find_package(<PackageName> [version] [EXACT] [QUIET] [MODULE] [REQUIRED] [[COMPONENTS]
> [components...]] [OPTIONAL_COMPONENTS components...] [NO_POLICY_SCOPE])

version和EXACT, 可选, version指定版本, 如果指定就必须检查找到的包的版本是否和version兼容.
如果指定EXACT则表示必须完全匹配的版本而不是兼容版本就可以.  QUIET, 可选字段, 表示如果查找
失败, 不会在屏幕进行输出(但如果指定了REQUIRED字段, 则QUIET无效, 仍然会输出查找失败提示
语).  MODULE, 可选字段. 前面提到说"如果Module模式查找失败则回退到Config模式进行查找", 但
是假如设定了MODULE选项, 那么就只在Module模式查找, 如果Module模式下查找失败并不回落到Config
模式查找.  REQUIRED可选字段. 表示一定要找到包, 找不到的话就立即停掉整个cmake. 而如果不指定
REQUIRED则cmake会继续执行.  COMPONENTS, components:可选字段, 表示查找的包中必须要找到的组
件(components), 如果有任何一个找不到就算失败, 类似于REQUIRED, 导致cmake停止执行.  Module
模式下需要查找到名为FindPackageName.cmake的文件. 先在CMAKE_MODULE_PATH变量对应的路径中查
找. 如果路径为空, 或者路径中查找失败, 则在cmake module directory(cmake安装时的Modules目
录, 比如/usr/local/share/cmake/Modules)查找.

### find_package的Config模式

>find_package(<PackageName> [version] [EXACT] [QUIET] [CONFIG|NO_MODULE] [NO_POLICY_SCOPE]
>[NAMES name1 [name2 ...]] [CONFIGS config1 [config2 ...]] [HINTS path1 [path2 ... ]]
>[PATHS path1 [path2 ... ]] [PATH_SUFFIXES suffix1 [suffix2 ...]] [NO_DEFAULT_PATH]
>[NO_PACKAGE_ROOT_PATH] [NO_CMAKE_PATH] [NO_CMAKE_ENVIRONMENT_PATH]
>[NO_SYSTEM_ENVIRONMENT_PATH] [NO_CMAKE_PACKAGE_REGISTRY] [NO_CMAKE_BUILDS_PATH] #
>Deprecated; does nothing. [REQUIRED] [[COMPONENTS] [components...]]
>[NO_CMAKE_SYSTEM_PATH] [NO_CMAKE_SYSTEM_PACKAGE_REGISTRY] [CMAKE_FIND_ROOT_PATH_BOTH |
>ONLY_CMAKE_FIND_ROOT_PATH | NO_CMAKE_FIND_ROOT_PATH])

Config模式下的查找顺序, 比Module模式下要多得多, 新版本的CMake比老版本的有更多的查找顺序
(新增的在最优先的查找顺序). Config模式下需要查找到名为
lower-case-package-name-config.cmake或PackageNameConfig.cmake文件.

- PackageName_ROOT的cmake变量或环境变量. CMake3.12新增. 设定CMP0074 Policy来关闭. 如果定义
  了PackageName_DIR cmake变量, 那么PackageName_ROOT 不起作用.
- cmake特定的缓存变量 CMAKE_PREFIX_PATH CMAKE_FRAMEWORK_PATH CMAKE_APPBUNDLE_PATH
- CMake特定的环境变量 PackageName_DIR CMAKE_PREFIX_PATH CMAKE_FRAMEWORK_PATH
  CMAKE_APPBUNDLE_PATH
- HINT字段指定的路径
- 搜索标准的系统环境变量PATH.
- 存储在CMake的"User Package Registry"(用户包注册表)中的路径. 通过设定
NO_CMAKE_PACKAGE_REGISTRY, 或设定CMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY为true, 来避开.
- 设定为当前系统定义的cmake变量: CMAKE_SYSTEM_PREFIX_PATH CMAKE_SYSTEM_FRAMEWORK_PATH
  CMAKE_SYSTEM_APPBUNDLE_PATH
- 在cmake的"System Package Registry"(系统包注册表)中查找.
- 从PATHS字段指定的路径中查找.

### 自定义模块

FIND_FILE(VAR name path1 path2 …) VAR变量代表找到的文件全路径, 包含文件名.
FIND_LIBRARY(VAR name path1 path2 …) VAR变量代表找到的库全路径, 包含库文件名.
FIND_PATH(VAR name path1 path2 …) VAR变量代表包含文件的路径FIND_PROGRAM(VAR name path1
path2 …) VAR变量代表包含程序的全路径

``` cmake
FIND_LIBRARY(libX X11 /usr/lib)
IF (NOT libx)
MESSAGE(FATAL_ERROR "libX not found")
ENDIF(NOT libX)

FIND_PACKAGE(NAME)
IF(NAME_FOUND)
INCLUDE_DIRECTORIES(${NAME_INCLUDE_DIR})
TARGET_LINK_LIBRARIES(targetname ${NAME_LIBRARY})
ELSE(NAME_FOUND)
MESSAGE(FATAL_ERROR "NAME library not found")
ENDIF(NAME_FOUND)

```

### 自定义模块实现

``` cmake
#Findhello.cmake
#查找hello库头文件的安装路径
FIND_PATH(HELLO_INCLUDE_DIR Hello.h /usr/local/include/hello)
#查找hello库的安装路径
FIND_LIBRARY(HELLO_LIBRARY NAMES hello /usr/local/lib/libhello.so)

IF (HELLO_INCLUDE_DIR AND HELLO_LIBRARY)
SET(HELLO_FOUND TRUE)
ENDIF (HELLO_INCLUDE_DIR AND HELLO_LIBRARY)

IF (HELLO_FOUND)
IF (NOT HELLO_FIND_QUIETLY)
MESSAGE(STATUS "Found Hello: ${HELLO_LIBRARY}")
ENDIF (NOT HELLO_FIND_QUIETLY)
ELSE (HELLO_FOUND)
IF (HELLO_FIND_REQUIRED)
MESSAGE(FATAL_ERROR "Could not find hello library")
ENDIF (HELLO_FIND_REQUIRED)
ENDIF (HELLO_FOUND)

#CMakeLists.txt
cmake_minimum_required(VERSION 2.8)
PROJECT(FindHello)
# 设置自定义模块路径
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/Hello/")
MESSAGE(STATUS "CMAKE_MODULE_PATH " ${PROJECT_SOURCE_DIR}/Hello)
#查找自定义Hello模块
FIND_PACKAGE(Hello)
IF(HELLO_FOUND)
ADD_EXECUTABLE(hello main.cpp)
INCLUDE_DIRECTORIES(${HELLO_INCLUDE_DIR})
MESSAGE(STATUS "LINK " ${HELLO_LIBRARY})
TARGET_LINK_LIBRARIES(hello ${HELLO_LIBRARY})
ELSE(HELLO_FOUND)
MESSAGE(STATUS "Could not find hello library")
ENDIF(HELLO_FOUND)
```

## 参考

[](https://chenxiaowei.gitbook.io/cmake-cookbook/preface-chinese)
[](https://cmake.org/cmake/help/latest/guide/tutorial/index.html)
