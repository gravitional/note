# classic flang

[Building-Flang](https://github.com/flang-compiler/flang/wiki/Building-Flang)
[Ubuntu 22.04 clang++ /usr/bin/ld: 找不到 -lstdc++: 没有那个文件或目录](https://blog.csdn.net/hknaruto/article/details/133738834)

## Prerequisites

编译 LLVM 需要相当先进的编译器工具链和 CMake(至少 3.3);
有关编译 Classic Flang 所需的全部工具列表, 请查看 [LLVM 入门](http://llvm.org/docs/GettingStarted.html#host-c-toolchain-both-compiler-and-standard-library) 和 [使用CMake编译 LLVM](http://llvm.org/docs/CMake.html).

在典型的 Ubuntu 系统中, 可以使用以下命令安装编译依赖项:

```bash
sudo apt-get install build-essential cmake ccache git libffi-dev libtinfo-dev ninja-build zlib1g-dev zstd libstdc++-12-dev vim python
```

如果使用 pacman

```bash
pacman -S base-devel cmake ccache git libffi-devel ncurses ninja zlib-devel zstd vim python
pacman -Syu mingw-w64-x86_64-gcc mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-gcc-libgfortran mingw-w64-x86_64-gcc-libs  mingw-w64-x86_64-lcov mingw-w64-x86_64-perl
```

## Dependencies

Classic Flang 依赖于 [LLVM 项目的一个分支](https://github.com/flang-compiler/classic-flang-llvm-project/).
该分叉已被修改, 以支持使用 Classic Flang 工具链编译 Fortran 文件,
以及特定于 Fortran 的命令行选项和调试元数据,
并可能包含 Classic Flang 已暴露但上游 LLVM 项目尚未修复的错误修正.
编译该分叉时应使用 `-DLLVM_ENABLE_CLASSIC_FLANG=ON` 选项.

Classic Flang 的 `master`主分支与 `classic-flang-llvm-project` 的 `release_15x` 分支(以及更新的分支)兼容.
如果出于某种原因需要使用 classic-flang-llvm-project 的 release_14x 分支(或更早的分支),
请 check out Classic Flang 的 `legacy` 分支.

## Building Classic Flang

Classic Flang 是在 LLVM 源代码树之外构建的.

大多数人认为最简单的方法是使用 `gcc` 和 `g++` 构建 Clang, LLVM 和 OpenMP,
然后使用刚刚构建的 `clang` 和 `clang++` 构建 `libpgmath` 和 `flang`.
在编译过程中, `flang` 将作为 `clang` 的符号链接被安装, 并用于编译经典 Flang 运行时库.
不需要预先存在的 Fortran 编译器.

下面的 Linux 命令行示例将把所有内容安装到自定义位置.
要安装到标准系统位置, 请移除下面 cmake 命令中
对 `INSTALL_PREFIX` 和 `-DCMAKE_INSTALL_PREFIX` 的引用.

### 编译 Classic Flang 时使用的 CMake 变量

#### `CMAKE_INSTALL_PREFIX`

要指定自定义的安装位置, 请在以下每个步骤中的每条 CMake 命令中添加 `-DCMAKE_INSTALL_PREFIX=<INSTALL_PREFIX> `.
如果在任何步骤中使用了 `CMAKE_INSTALL_PREFIX`, 则必须在每个步骤中使用相同的 `CMAKE_INSTALL_PREFIX`.
使用自定义安装位置时, 必须确保 `bin` 目录位于编译和运行 `Classic Flang` 时的搜索路径上.

#### `LLVM_MAIN_SRC_DIR`

如果要运行 Classic Flang 回归测试, 则必须通过指定 `-DLLVM_MAIN_SRC_DIR=<PATH_TO_CLASSIC_FLANG_LLVM_PROJECT>/llvm/`
来指定 Classic Flang build 的 LLVM 源代码目录.

#### `CMAKE_C_COMPILER`, `CMAKE_CXX_COMPILER` and `CMAKE_Fortran_COMPILER`

如果您使用了自定义的安装位置, 而该位置的 bin 目录不在您的搜索路径上,
那么在build Classic  Flang 时, 您必须使用
`-DCMAKE_CXX_COMPILER=<INSTALL_PREFIX>/bin/clang++`,
`-DCMAKE_C_COMPILER=<INSTALL_PREFIX>/bin/clang`,  和
`-DCMAKE_Fortran_COMPILER=<INSTALL_PREFIX>/bin/flang`
指定 `clang`, `clang++` 和 `flang` 的位置.

#### `LLVM_TARGETS_TO_BUILD`

指定构建 LLVM 和 Classic Flang 的特定 targets  可以加快构建速度.
例如, 要只为 X86 处理器编译, 可添加 `-DLLVM_TARGETS_TO_BUILD=X86` CMake 选项.
Classic Flang 支持 X86, PowerPC 和 AArch64.

请注意, 虽然 LLVM 可同时构建以支持多个目标,
但 Classic Flang 前端和运行库一次只能支持一个目标.

## 逐步编译, on linux zsh

1. 创建 build 目录并定义所需的 CMake 变量.
在下面的示例中, 我们将假定你想安装在你将进行构建的安装目录中.

```powershell
cd /where/you/want/to/build/flang
mkdir -force install
```

下面是一个 `setup.sh` 示例, 其他build脚本可以用它来定义常用变量.
我们指定了一个自定义安装位置, 并说明要使用 clang 为 X86 构建.

```zsh
INSTALL_PREFIX='/home/tom/llvm-flang'
# Targets to build should be one of: X86 PowerPC AArch64
CMAKE_OPTIONS="-DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX \
    -DCMAKE_CXX_COMPILER=$INSTALL_PREFIX/bin/clang++ \
    -DCMAKE_C_COMPILER=$INSTALL_PREFIX/bin/clang \
    -DCMAKE_Fortran_COMPILER=$INSTALL_PREFIX/bin/flang \
    -DCMAKE_Fortran_COMPILER_ID=Flang \
    -DLLVM_TARGETS_TO_BUILD=X86"
```

在build Classic Flang 时, 要使用试验性 和 unsupported OpenMP target offload functionality,
请在 `CMAKE_OPTIONS` 中添加 `-DFLANG_OPENMP_GPU_NVIDIA=ON`.
并非所有变量都会在每次build中使用, 因此您可能会看到一些关于未使用定义的警告.

2. 克隆 llvm-project fork, build并安装它(包括 Clang 和 OpenMP).
下面是 build-llvm-project.ps1 脚本 (使用 gcc 和 g++ 引导工具链):

使用 `zsh` 需要小心考虑 field splitting, 即使用 `$=CMAKE_OPTIONS` 而不是 `$CMAKE_OPTIONS`
**如果是 `bash` 的话, 可以直接使用 `$CMAKE_OPTIONS`**

```zsh
. setup.sh

if [[ ! -d classic-flang-llvm-project ]]; then
    git clone -b release_16x https://github.com/flang-compiler/classic-flang-llvm-project.git
fi

cd classic-flang-llvm-project
mkdir -p build && cd build
cmake $=CMAKE_OPTIONS -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=/usr/bin/gcc -DCMAKE_CXX_COMPILER=/usr/bin/g++  -DLLVM_ENABLE_CLASSIC_FLANG=ON -DLLVM_ENABLE_PROJECTS="clang;openmp" ../llvm
make
sudo make install
```

3. 克隆 `flang` 仓库, 并build `libpgmath` 和 `flang`.
下面是一个 `build-flang.ps1` 脚本示例(使用 clang 构建).
脚本首先编译 `libpgmath`, 然后编译经典 `flang` 前端和运行时库.

请注意, x86 上的 libpgmath 需要能理解 AVX-512 指令的工具链, 如 gcc 7.2 或 clang.

```bash
. setup.sh

if [[ ! -d flang ]]; then
    git clone https://github.com/flang-compiler/flang.git
fi

(cd flang/runtime/libpgmath
 mkdir -p build && cd build
 cmake $=CMAKE_OPTIONS .. -DCMAKE_BUILD_TYPE=Release -DLLVM_MAIN_SRC_DIR=/home/tom/classic-flang/classic-flang-llvm-project/llvm
 make
 sudo make install)

cd flang
mkdir -p build && cd build
cmake $=CMAKE_OPTIONS -DCMAKE_BUILD_TYPE=Release -DFLANG_LLVM_EXTENSIONS=ON -DLLVM_MAIN_SRC_DIR=/home/tom/classic-flang/classic-flang-llvm-project/llvm  ..
make
sudo make install
```

要使用 `Sphinx` 构建 HTML 文档, 请在构建经典 `Flang` 时
在 cmake 命令中添加 `-DLLVM_INCLUDE_DOCS=ON` 或 `-DFLANG_INCLUDE_DOCS=ON` .
要使用 Doxygen 构建源代码的注释索引, 还需添加 `-DLLVM_ENABLE_DOXYGEN=ON`.

4. 尝试使用新安装的 Classic Flang 编译器构建 hello-world 程序. 将此文件保存为 `hello.f90`:

```fortran
program hello
  print *, "hello world"
end program
```

要编译程序, 请执行以下命令:

```bash
$INSTALL_PREFIX/bin/flang hello.f90 -o hello.exe
```

运行程序应该会成功, 例如

```bash
$ ./hello.exe
 hello world
```

如果运行程序时加载共享库: `libflang.so` 时出错,
则可能是编译器安装到了自定义位置, 而不是系统目录(如 `/usr/local`).
在这种情况下, 导出 `LD_LIBRARY_PATH` 指向您的工具链, 然后再试一次, 例如

```bash
$ export LD_LIBRARY_PATH=$INSTALL_PREFIX/lib
$ ./hello.exe
hello world
```

## 使用构建脚本

flang 和 classic-flang-llvm-project 软件源包含我们的 GitHub 工作流,
用来构建和测试编译器的构建脚本(分别为 `scripts/build_flang.py` 和 `scripts/build_llvm_project.py`).

您可以使用这些脚本来代替上述 逐步说明 或 编写自己的脚本.
首先应在 classic-flang-llvm-project 克隆中运行 `build_llvm_project.py`,
在成功安装 Clang/LLVM 后, 再在 flang 克隆中运行 `build_flang.py`.

切记在两种情况下使用相同的 `install prefix`(`-p` 选项),
并为 `build_flang.py` 添加 `-l` 选项以指向 `LLVM` 源代码目录.
使用 --help 运行脚本将列出可用选项.
有关如何使用脚本的示例, 请参见 [.github/workflows/flang-test.yml](https://github.com/flang-compiler/classic-flang-llvm-project/blob/release_16x/.github/workflows/flang-tests.yml).

## windows 平台

#### windows 平台

setup.ps1

```powershell
$env:INSTALL_PREFIX="C:/cppLibs/flang-llvm/install"
# Targets to build should be one of: X86 PowerPC AArch64
$env:CMAKE_OPTIONS="-DCMAKE_INSTALL_PREFIX=${env:INSTALL_PREFIX} -DCMAKE_CXX_COMPILER=${env:INSTALL_PREFIX}/bin/clang++.exe -DCMAKE_C_COMPILER=${env:INSTALL_PREFIX}/bin/clang.exe -DCMAKE_Fortran_COMPILER=${env:INSTALL_PREFIX}/bin/flang.exe -DCMAKE_Fortran_COMPILER_ID=Flang -DLLVM_TARGETS_TO_BUILD=X86"
```

#### classic-flang-llvm-project

```powershell
. setup.ps1

if (! (Test-Path classic-flang-llvm-project -PathType Container)){
git clone -b release_16x git@github.com:flang-compiler/classic-flang-llvm-project.git
}

Set-Location classic-flang-llvm-project
mkdir -force build && cd build

cmake $env:CMAKE_OPTIONS -DLLVM_ENABLE_CLASSIC_FLANG=ON -DLLVM_ENABLE_PROJECTS="clang;openmp" ../llvm

cmake --build .
cmake --build . --target install
```

#### flang

```powershell
. setup.ps1

if (! (Test-Path flang )){
    git clone git@github.com:flang-compiler/flang.git
}

# 编译 libpgmath
cd flang/runtime/libpgmath
mkdir -force build && cd build
cmake $env:CMAKE_OPTIONS -DCMAKE_BUILD_TYPE=Release -DLLVM_MAIN_SRC_DIR=C:\Users\qingz\Downloads\classic-flang\classic-flang-llvm-project-release_17x\llvm .. --fresh
cmake --build .
cmake --build . --target install

# 编译 flang
cd flang
mkdir -force build && cd build
cmake $env:CMAKE_OPTIONS -DCMAKE_BUILD_TYPE=Release -DFLANG_LLVM_EXTENSIONS=ON -DLLVM_MAIN_SRC_DIR=C:\Users\qingz\Downloads\classic-flang\classic-flang-llvm-project-release_17x\llvm  -D ..
cmake --build .
cmake --build . --target install
```

## ucrt64, bash

[Cannot specify a non default C/C++ compiler in CMake under MinGW64/MSYS](https://stackoverflow.com/questions/66353055/cannot-specify-a-non-default-c-c-compiler-in-cmake-under-mingw64-msys)

使用 msys2, `ucrt64` 子环境进行编译.
注意指定编译器的时候, 应该写完全路径, 即 `-DCMAKE_C_COMPILER=/mingw64/bin/gcc.exe`,
这是 `windows` 的坑
编译的时候会缺少x64汇编语言的assembler, 称为 MASM for x64 (ml64.exe)
`ml.exe`, 使用下面的命令从 `VS` 安装中复制一份:

```bash
cp /c/Program\ Files/Microsoft\ Visual\ Studio/2022/Community/vc/Tools/MSVC/14.38.33130/bin/Hostx64/x64/ml64.exe  /usr/bin/ml.exe

```bash
# setup.sh
INSTALL_PREFIX='/c/cppLibs/llvm-flang' # on ucrt64 bash
# Targets to build should be one of: X86 PowerPC AArch64
CMAKE_OPTIONS="-DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX \
    -DCMAKE_CXX_COMPILER=$INSTALL_PREFIX/bin/clang++ \
    -DCMAKE_C_COMPILER=$INSTALL_PREFIX/bin/clang \
    -DCMAKE_Fortran_COMPILER=$INSTALL_PREFIX/bin/flang \
    -DCMAKE_Fortran_COMPILER_ID=Flang \
    -DLLVM_TARGETS_TO_BUILD=X86"
```

使用 ninja 作为后端编译

```bash
. setup.sh

if [[ ! -d classic-flang-llvm-project ]]; then
    git clone -b release_17x https://github.com/flang-compiler/classic-flang-llvm-project.git
fi

cd classic-flang-llvm-project
mkdir -p build && cd build
cmake $CMAKE_OPTIONS -G 'MSYS Makefiles' -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=gcc.exe -DCMAKE_CXX_COMPILER=g++.exe -DLLVM_ENABLE_CLASSIC_FLANG=ON -DLLVM_ENABLE_PROJECTS="clang;openmp" ../llvm --fresh
make
sudo make install
```

## new flang

```powershell
rm -Force -Recurse build
mkdir build
rm -Force -Recurse install
mkdir install
$env:ROOTDIR='C:/Users/qingz/Downloads/llvm-src'
$env:INSTALLDIR="${env:ROOTDIR}/install"
cd build

cmake -G 'Visual Studio 17 2022' -DCMAKE_BUILD_TYPE='Release' -DCMAKE_INSTALL_PREFIX="${env:INSTALLDIR}" -DCMAKE_CXX_STANDARD='17' -DCMAKE_EXPORT_COMPILE_COMMANDS='ON' -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,${env:LD_LIBRARY_PATH}" -DFLANG_ENABLE_WERROR='ON' -DLLVM_ENABLE_ASSERTIONS='ON' -DLLVM_TARGETS_TO_BUILD='host' -DLLVM_LIT_ARGS='-v' -DLLVM_ENABLE_PROJECTS="clang;mlir;flang;openmp" -DLLVM_ENABLE_RUNTIMES="compiler-rt" ../llvm --fresh

MSBuild C:\Users\qingz\Downloads\llvm-src\build\LLVM.sln /m /p:Platform=x64 /v:m /p:Configuration=Release '-t:build'

#To create the installed files:
cmake --install .
echo "latest" > "${env:INSTALLDIR}/bin/versionrc"
```

```powershell
# llvm source 目录
$env:ROOTDIR='C:/Users/qingz/Downloads/llvm-src/'
cmake -G 'Visual Studio 17 2022' -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_STANDARD=17 -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,${env:LD_LIBRARY_PATH}" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DFLANG_ENABLE_WERROR=ON -DLLVM_TARGETS_TO_BUILD=host   -DLLVM_ENABLE_ASSERTIONS=ON -DLLVM_BUILD_MAIN_SRC_DIR="${env:ROOTDIR}/build/lib/cmake/llvm" -DLLVM_EXTERNAL_LIT="${env:ROOTDIR}/build/bin/llvm-lit" -DLLVM_LIT_ARGS=-v -DLLVM_DIR="${env:ROOTDIR}/build/lib/cmake/llvm" -DCLANG_DIR="${env:ROOTDIR}/build/lib/cmake/clang" -DMLIR_DIR="${env:ROOTDIR}/build/lib/cmake/mlir"

MSBuild  C:\Users\qingz\Downloads\llvm-src\build\LLVM.sln /m /p:Platform=x64 /v:m  /p:Configuration=Release '-t:build'
```
