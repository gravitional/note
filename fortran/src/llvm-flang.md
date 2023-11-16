# llvm flang

## 构建 flang

有两种构建 flang 的方法.
第一种方法是在构建 `flang` 依赖的所有项目 的同时构建它.
这就是所谓的 building in tree.

第二种方法是首先进行 tree build, 创建 flang 依赖的所有项目.
然后, 在创建 base build 后, 只构建 `flang` 代码本身.
这就是所谓的 building standalone.
独立构建的优点是更小更快.
创建 base build 和 base install areas 后,
就可以使用它们创建多个独立构建区.

请注意, 有关构建 LLVM 的说明, 请访问 [llvm](https://llvm.org/docs/GettingStarted.html).

以下所有示例均使用 GCC 作为 C/C++ 编译器, 并使用 ninja 作为build工具.

### 在树中编译 flang

在树中编译 flang 意味着在编译 flang 的同时, 还要编译它所依赖的所有项目.
这些项目包括 `mlir`, `clang`, `flang`, `openmp` 和 `compiler-rt`.
请注意, 只有在访问 支持16位浮点数的库 时才需要使用 `compiler-rt`.
运行自动测试时不需要它.

你可以使用几种不同的 C++ 编译器来完成大部分编译工作, 包括 `GNU` 和 `clang`.
但 `compiler-rt` 需要使用 build初始过程中生成的 `clang` 编译器.

下面是一个可行的目录结构.
为 cloned 和 built 的文件创建一个根目录.
在该根目录下, 将源代码克隆到名为 `llvm-project` 的目录中.
build还会在根目录下创建名为 `build`(保存大部分built文件),
`install`(保存已安装文件)和 `compiler-rt`(保存rt结果)的子目录.

下面是克隆所有必要源代码并进行编译的一整套命令.
首先, 创建根目录并 `cd` 进入.

现在克隆源代码

```bash
git clone https://github.com/llvm/llvm-project.git
```

克隆完成后, 执行以下命令

```bash
rm -rf build
mkdir build
rm -rf install
mkdir install
ROOTDIR=`pwd`
INSTALLDIR=$ROOTDIR/install

cd build

cmake \
  -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=$INSTALLDIR \
  -DCMAKE_CXX_STANDARD=17 \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
  -DFLANG_ENABLE_WERROR=ON \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DLLVM_TARGETS_TO_BUILD=host \
  -DLLVM_LIT_ARGS=-v \
  -DLLVM_ENABLE_PROJECTS="clang;mlir;flang;openmp" \
  -DLLVM_ENABLE_RUNTIMES="compiler-rt" \
  ../llvm-project/llvm

ninja
```

默认情况下, 没有明确指定 `--target` 标志的 `flang` 测试
使用 `LLVM` 的默认target triple.
对于这些测试, 如果需要通过覆盖默认值, 在不同的triple上进行测试,
则需要在上面的 cmake 命令中添加以下内容:

```bash
-DLLVM_TARGET_TRIPLE_ENV="<some string>" \
-DFLANG_TEST_TARGET_TRIPLE="<your triple>"
```

要在此build版本上运行 flang 测试, 请在 `build` 目录下执行命令:

```bash
ninja check-flang
```

创建安装文件:

```bash
ninja install
echo "latest" > $INSTALLDIR/bin/versionrc
```

构建 `compiler-rt`:

```bash
cd $ROOTDIR
rm -rf compiler-rt
mkdir compiler-rt
cd compiler-rt
CC=$INSTALLDIR/bin/clang \
CXX=$INSTALLDIR/bin/clang++ \
cmake \
  -G Ninja \
  ../llvm-project/compiler-rt \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=$INSTALLDIR \
  -DCMAKE_CXX_STANDARD=11 \
  -DCMAKE_C_CFLAGS=-mlong-double-128 \
  -DCMAKE_CXX_CFLAGS=-mlong-double-128 \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCOMPILER_RT_BUILD_ORC=OFF \
  -DCOMPILER_RT_BUILD_XRAY=OFF \
  -DCOMPILER_RT_BUILD_MEMPROF=OFF \
  -DCOMPILER_RT_BUILD_LIBFUZZER=OFF \
  -DCOMPILER_RT_BUILD_SANITIZERS=OFF \
  -DLLVM_CONFIG_PATH=$INSTALLDIR/bin/llvm-config

ninja
ninja install
```

请注意, 这些说明将 flang 指定为在 树内编译 时要编译的项目之一.
严格来说, 这对于后续的 独立编译 并不是必须的,
但这样做可以让你运行 flang 测试来验证源代码是否完好.

## 独立构建 flang

要进行 独立build, 首先要按上文所述 `build flang in tree`.
此build可作为后续多次 独立build 的基础build.
将环境变量 `ROOT_DIR` 设置为 之前创建的 `build`子目录的 父目录, 例如

```bash
export ROOTDIR=/home/user/root
```

通过克隆 `llvm-project` 的源代码, 以同样的方式启动每次 独立构建:

```bash
mkdir standalone
cd standalone
git clone https://github.com/llvm/llvm-project.git
```

克隆完成后, 执行以下命令:

```bash
cd llvm-project/flang
rm -rf build
mkdir build
cd build

cmake \
  -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_CXX_STANDARD=17 \
  -DCMAKE_CXX_LINK_FLAGS="-Wl,-rpath,$LD_LIBRARY_PATH" \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DFLANG_ENABLE_WERROR=ON \
  -DLLVM_TARGETS_TO_BUILD=host \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DLLVM_BUILD_MAIN_SRC_DIR=$ROOTDIR/build/lib/cmake/llvm \
  -DLLVM_EXTERNAL_LIT=$ROOTDIR/build/bin/llvm-lit \
  -DLLVM_LIT_ARGS=-v \
  -DLLVM_DIR=$ROOTDIR/build/lib/cmake/llvm \
  -DCLANG_DIR=$ROOTDIR/build/lib/cmake/clang \
  -DMLIR_DIR=$ROOTDIR/build/lib/cmake/mlir \
  ..

ninja
```

要在此构建上运行 `flang` 测试, 请在 `flang/build` 目录下执行命令:

```bash
ninja check-flang
```

### 为加速器构建 flang运行时

Flang运行时 可在 experimental mode 下为加速器构建,
即 complete enabling is `WIP`.
目前支持 `CUDA` 和 `OpenMP` target offload builds.

#### 树外构建

##### CUDA 构建

支持带有 `NVPTX` 后端的 Clang 和 NVCC 编译器.

```bash
cd llvm-project/flang
rm -rf build_flang_runtime
mkdir build_flang_runtime
cd build_flang_runtime

cmake \
  -DFLANG_EXPERIMENTAL_CUDA_RUNTIME=ON \
  -DCMAKE_CUDA_ARCHITECTURES=80 \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_CUDA_COMPILER=clang \
  -DCMAKE_CUDA_HOST_COMPILER=clang++ \
  ../runtime/
make -j FortranRuntime
```

请注意, 使用的 `clang` 版本必须支持构建机器上安装的 CUDA工具包版本.
如果安装了多个 CUDA 工具包,
请使用 `-DCUDAToolkit_ROOT=/some/path` 指定兼容版本.

```bash
cd llvm-project/flang
rm -rf build_flang_runtime
mkdir build_flang_runtime
cd build_flang_runtime

cmake \
  -DFLANG_EXPERIMENTAL_CUDA_RUNTIME=ON \
  -DCMAKE_CUDA_ARCHITECTURES=80 \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_CUDA_COMPILER=nvcc \
  -DCMAKE_CUDA_HOST_COMPILER=clang++ \
  ../runtime/

make -j FortranRuntime
```

注意 `nvcc` 可能会限制对某些版本的 `CMAKE_CUDA_HOST_COMPILER` 的支持,
因此请使用兼容的版本.

编译的结果是一个包含 host and device code 的 "fat" 库.
请注意, Clang 和 NVCC 对库的打包方式不同,
因此必须使用兼容的编译器驱动程序链接库.

#### 树内构建

通过在 Flang 的树内构建配置之上提供这些额外的 CMake 变量,
可以在构建 Flang 本身的同时构建 Flang 运行时库:

例如

```bash
-DFLANG_EXPERIMENTAL_CUDA_RUNTIME=ON \
-DCMAKE_CUDA_ARCHITECTURES=80 \
-DCMAKE_C_COMPILER=clang \
-DCMAKE_CXX_COMPILER=clang++ \
-DCMAKE_CUDA_COMPILER=clang \
-DCMAKE_CUDA_HOST_COMPILER=clang++ \
```

或者

```bash
-DFLANG_EXPERIMENTAL_CUDA_RUNTIME=ON \
-DCMAKE_CUDA_ARCHITECTURES=80 \
-DCMAKE_C_COMPILER=gcc \
-DCMAKE_CXX_COMPILER=g++ \
-DCMAKE_CUDA_COMPILER=nvcc \
-DCMAKE_CUDA_HOST_COMPILER=g++ \
```

正常的 `make -j check-flang` 将在这样的 CMake 配置下工作.

#### OpenMP目标 offload build

目前仅支持 Clang 编译器.

```bash
cd llvm-project/flang
rm -rf build_flang_runtime
mkdir build_flang_runtime
cd build_flang_runtime

cmake \
  -DFLANG_EXPERIMENTAL_OMP_OFFLOAD_BUILD="host_device" \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DFLANG_OMP_DEVICE_ARCHITECTURES="all" \
  ../runtime/

make -j FortranRuntime
```

编译的结果是一个 `device-only` 库, 即库的host部分 只是 device code 的容器.
生成的库可以使用类似 `Clang` 的设备链接管道链接到用户程序.

同样的 CMake 变量集也适用于 Flang 树内编译.

## 支持的 C++ 编译器

Flang 采用 C++17 编写.

代码已使用 GCC 7.2.0 至 9.3.0 版本编译和测试.

代码已使用 GNU 的 libstdc++ 或 LLVM 的 libc++
在 clang 7.0, 8.0, 9.0 和 10.0 版本下编译和测试.

代码已在使用 CentOS7, Ubuntu18.04,
Rhel, MacOs, Mojave, XCode 和 Apple Clang 10.0.1 版本的 AArch64, x86_64 和 ppc64le 服务器上编译.

请注意, 32 位 CPU 不支持 flang.

使用 GCC 构建 flang
默认情况下, cmake 会在 PATH 中搜索 g++.
要编译 flang, g++ 版本必须是受支持的版本之一.

或者, cmake 会使用变量 CXX 来查找 C++ 编译器.
CXX 应包括编译器的完整路径或在 PATH 中可以找到的名称,
例如 g++-8.3, 假设 g++-8.3 在 PATH 中.

```
export CXX=g++-8.3
```

或

```bash
CXX=/opt/gcc-8.3/bin/g++-8.3 cmake ...
```

### 使用 clang 构建 flang

要使用 clang 联编 flang,
cmake 需要知道如何找到 clang++ 以及用于联编 clang++ 的 GCC 库和工具.

CXX 应包含 clang++ 的完整路径, 或者 clang++ 应在您的 PATH 中找到.

```bash
export CXX=clang++
```

### 安装目录

要指定自定义的安装位置, 请在 `cmake` 命令中添加 `-DCMAKE_INSTALL_PREFIX=<INSTALL_PREFIX>` ,
其中 `<INSTALL_PREFIX>` 是安装 `flang` 的路径.

#### 编译类型

要创建调试联编, 请在 cmake 命令中添加 `-DCMAKE_BUILD_TYPE=Debug`.
调试构建执行速度较慢.

要创建发行版联编, 请在 cmake 命令中添加 `-DCMAKE_BUILD_TYPE=Release`.
发行版编译会快速执行.

### 如何运行测试

Flang 支持两类不同的测试

回归测试 (https://www.llvm.org/docs/TestingGuide.html#regression-tests)
单元测试 (https://www.llvm.org/docs/TestingGuide.html#unit-tests)
对于独立构建
运行所有测试

```bash
cd ~/flang/build
cmake -DLLVM_DIR=$LLVM -DMLIR_DIR=$MLIR ~/flang/src
ninja check-all
```

要运行单个回归测试, llvm-lit 需要知道 flang 的 lit 配置.
负责这一工作的参数是: flang_site_config 和 flang_config.
它们的设置如下所示:

```bash
<path-to-llvm-lit>/llvm-lit \
 --param flang_site_config=<path-to-flang-build>/test-lit/lit.site.cfg.py \
 --param flang_config=<path-to-flang-build>/test-lit/lit.cfg.py \
  <path-to-fortran-test>
```

单元测试:

如果在联编 flang 时使用了 `-DFLANG_INCLUDE_TESTS=ON`(默认为 ON), 则可以生成单元测试.
注意: 由于 LLVM install 不包含与 googletest 相关的头文件和库, 因此在独立编译时将跳过单元测试.

运行单元测试有多种方法.

```bash

1. ninja check-flang-unit
2. ninja check-all or ninja check-flang
3. <path-to-llvm-lit>/llvm-lit \
        test/Unit
4. Invoking tests from <standalone flang build>/unittests/<respective unit test folder>
```

### 用于树形联编

如果在联编 flang 时使用了 -DFLANG_INCLUDE_TESTS=ON(默认为 ON), 则可以生成 unittests.

要运行所有 flang 单元测试, 请使用 check-flang-unit 目标:

```bash
ninja check-flang-unit
```

要运行所有 flang 回归测试, 请使用 check-flang 目标:

```bash
ninja check-flang
```
