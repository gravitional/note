# cpp insights

## Build & install Clang from sources

Needed to have Clang libraries and `llvm-config.exe` to setup CMake.

Installs Clang/LLVM libraries to (for example) `C:\cppLibs\LLVM_local2`.

Note:

```cmd
mkdir build
cd build

cmake -DLLVM_ENABLE_PROJECTS=clang ^
      -DCMAKE_INSTALL_PREFIX=C:\cppLibs\LLVM_local2 ^
      -G "Visual Studio 17 2022" ^
      -A x64 ^
      -Thost=x64 ^
      ..\llvm
```

使用 `nushell`

```nu
mkdir build
cd build

(cmake -DLLVM_ENABLE_PROJECTS=clang
      -DCMAKE_INSTALL_PREFIX='C:/cppLibs/LLVM_local2'
      -G "Visual Studio 17 2022"
      -A x64
      -Thost=x64
      ..\llvm)

cmake --build . --config Release --target install
```

You can also open build/LLVM.sln solution in Visual Studio and build everything from there instead of using `cmake --build xxx` command.

### note

`llvm-project-main\clang\lib\Lex\UnicodeCharSets.h(1,1)`
line395 遇到非 unicode 字符注释导致的编译错误,
删除这些注释之后编译正常.

## Build insights

原文使用 `-T LLVM_v142`

Assume:

`cppinsights` sources are in `~\myprojs\cppinsights-main`
and
`LLVM/Clang` built and installed into
`C:\cppLibs\LLVM_local2` (see step above)

```cmd
cd C:\dev\cppinsights\
mkdir build
cd build
set path=%path%;C:\cppLibs\LLVM_local2\bin
cmake -G "Visual Studio 16 2019" -A x64 -T ClangCL ..
cmake --build . --config Release --target insights
```

+ 使用 nushell

```nu
cd ~\myprojs\cppinsights-main
mkdir build
cd build
$env.Path = ($env.Path ++ 'C:/cppLibs/llvm/bin')
(
cmake -G "Visual Studio 17 2022" -A x64 -T ClangCL -DCMAKE_MT=mt -B . -S ..
)
```

+

```bash
cmake -B . -S ..
```

Instead of "Visual Studio 16 2019" generator with Clang,
you can choose whatever works for you.
See "Tested with (supported compilers)", CMake command column above.

Also, instead of building from command line,
you can open build/cpp-insights.sln and have fun with VS.