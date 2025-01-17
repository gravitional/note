# cmake 构建例子

## [quickjs](https://github.com/quickjs-ng/quickjs)

+ 打开 CMake GUI, 修改安装位置,

```cmake
CMAKE_INSTALL_PREFIX -> C:/Users/qingz/Downloads/install-demo
```

然后分两遍构建:

+ 首先勾选

```cmake
BUILD_EXAMPLES
BUILD_QJS_LIBC
BUILD_STATIC_QJS_EXE
```

不勾选 `BUILD_SHARED_LIBS`,
这样的到 `qjs.exe`, `qjs.lib`,

+ 第二遍再勾选 `BUILD_SHARED_LIBS`
这样得到动态库 `qjs.dll`

安装使用命令:

```bash
cmake --install . --config release
```

## MSVC 具体编译命令示例, `quickjs_test.vcxproj`

比如有项目 `quickjs_test.vcxproj`,
依赖于 quickjs.dll 以及相关的头文件目录.
使用 CMake 的构建过程如下:

```bash
# 构建生成系统
cmake -G 'Visual Studio 17 2022' -B . -S ..
# 编译; 目标为 quickjs_test 项目; -j 1 单线程编译; -v 输出详细信息
cmake --build . --config Release --target quickjs_test -j 1 -v
```

输出类似于下面, 其中 `xxxx` 表示一些具体路径.

说明:

+ `ClCompile:` 部分;
可以看到 `/external:I "xxxx/bin_repos/quickjs/include"`
指定了 dependecies 的头文件包含目录

+ `Link:` 部分;
选项`/IMPLIB:"xxxx/quickjs_test/Release/quickjs_test.lib"`
指定了要链接的 impl lib, 对于的dll 是 `quickjs_test.dll`.

```bat
Change Dir: 'xxxx/cppTest/build-msvc'

Run Build Command(s): "xxxx/MSBuild.exe" quickjs_test/quickjs_test.vcxproj
/p:Configuration=Release /p:Platform=x64 /p:VisualStudioVersion=17.0 /m:1 /v:n
...

生成"xxxx\build-msvc\ZERO_CHECK.vcxproj"(2) (默认目标).
PrepareForBuild:
    ...
InitializeBuildStatus:
  正在创建"x64\Release\ZERO_CHECK\ZERO_CHECK.tlog\unsuccessfulbuild", 因为已指定"AlwaysCreate".
  正在对"x64\Release\ZERO_CHECK\ZERO_CHECK.tlog\unsuccessfulbuild"执行 Touch 任务.
CustomBuild:
  所有输出均为最新.
FinalizeBuildStatus:
  正在删除文件"x64\Release\ZERO_CHECK\ZERO_CHECK.tlog\unsuccessfulbuild"...

PrepareForBuild:
  正在创建目录"quickjs_test.dir\Release\".
  ...
  正在创建目录"xxxx\build-msvc\quickjs_test\Release\".
  正在创建目录"quickjs_test.dir\Release\quickjs_test.tlog\".
InitializeBuildStatus:
  正在创建"quickjs_test.dir\Release\quickjs_test.tlog\unsuccessfulbuild", ...
CustomBuild:
  Building Custom Rule xxxx/quickjs_test/CMakeLists.txt
ClCompile:
  xxxx\x64\CL.exe /c /nologo /W1 /WX- /diagnostics:column /O2 /Ob2
  /D _MBCS /D WIN32 /D _WINDOWS /D NDEBUG /D "CMAKE_INTDIR=\"Release\"" /EHsc /MD /GS
  /Zc:wchar_t /Zc:forScope /Zc:inline /std:c++20
  /Fo"quickjs_test.dir\Release\\" /Fd"quickjs_test.dir\Release\vc143.pdb"
  /external:W0 /Gd /TP /errorReport:queue
  /external:I "xxxx/bin_repos/quickjs/include"
  /utf-8 xxxx\cppTest\quickjs_test\main.cpp
  main.cpp
Link:
  xxxx\x64\link.exe /ERRORREPORT:QUEUE /OUT:"xxxx\quickjs_test\Release\quickjs_test.exe"
  /INCREMENTAL:NO /NOLOGO xxxx\bin_repos\quickjs\lib\qjs.lib
  kernel32.lib user32.lib gdi32.lib winspool.lib shell32.lib
  ole32.lib oleaut32.lib uuid.lib comdlg32.lib advapi32.lib
  /MANIFEST /MANIFESTUAC:"level='asInvoker' uiAccess='false'" /manifest:embed
  /PDB:"xxxx/quickjs_test/Release/quickjs_test.pdb"
  /SUBSYSTEM:CONSOLE /TLBID:1 /DYNAMICBASE /NXCOMPAT
  /IMPLIB:"xxxx/quickjs_test/Release/quickjs_test.lib"
  /MACHINE:X64  /machine:x64 quickjs_test.dir\Release\main.obj
  quickjs_test.vcxproj -> xxxx\build-msvc\quickjs_test\Release\quickjs_test.exe
PostBuildEvent:
  setlocal
  "C:\Program Files\CMake\bin\cmake.exe" -E copy_if_different
  xxxx/bin_repos/quickjs/bin/qjs.dll xxxx/quickjs_test/Release
  if %errorlevel% neq 0 goto :cmEnd
  :cmEnd
  endlocal & call :cmErrorLevel %errorlevel% & goto :cmDone
  :cmErrorLevel
  exit /b %1
  :cmDone
  if %errorlevel% neq 0 goto :VCEnd
  :VCEnd
FinalizeBuildStatus:
  正在删除文件"quickjs_test.dir\Release\quickjs_test.tlog\unsuccessfulbuild"...

已完成生成项目"xxxx\build-msvc\quickjs_test\quickjs_test.vcxproj"(默认目标)的操作.
```
