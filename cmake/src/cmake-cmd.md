# cmake 命令行

[cmake: 命令行工具cmake](https://blog.csdn.net/zhizhengguan/article/details/118339062)
[cmake.1.html](https://cmake.org/cmake/help/latest/manual/cmake.1.html)

## 构建项目

CMake提供了一个命令行形式来构建 已经生成的项目 binary tree

```bash
cmake --build <dir>  [<options>] [-- <build-tool-options>]
cmake --build --preset <preset> [<options>] [-- <build-tool-options>]
```

这将使用以下选项抽象出 `本地构建工具` 的命令行界面:

+ `--build <dir>`
要编译的 Project binary 目录.  一般就是 `./build` 目录.
这是必填项(除非指定了预设), 并且必须放在前面.

+ `--preset <preset>, --preset=<preset>`
使用 build preset 来指定 build选项.
项目二进制目录由 `configurePreset` key推断.
当前工作目录必须包含 CMake 预设文件. 详见[预设](https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html#manual:cmake-presets(7)).

+ `--list-presets`
列出可用的编译预置. 当前工作目录必须包含 CMake 预设文件.

+ `-j [<jobs>], --parallel [<jobs>]`
3.12 版新增.
编译时使用的最大并发进程数. 如果省略 `<jobs>`, 则使用本地build工具的默认进程数.
如果设置了 `CMAKE_BUILD_PARALLEL_LEVEL` 环境变量, 则会当作此选项默认的并行级别.
有些本地编译工具总是并行编译. `<jobs>` 值为 `1` 时, 可用于限制为单个作业.

+ `-t <tgt>..., --target <tgt>...`
构建 `<tgt>` 而不是默认目标. 可以给出多个目标, 以空格分隔.

+ `--config <cfg>`
对于多配置工具, 请选择配置 `<cfg>`.
例如 Visual Studio 的 `Debug`, `Release`.

+ `--clean-first`
先构建`clean`目标, 然后再构建.
(要只进行清理, 请使用 `--target clean`).

+ `--resolve-package-references=<value>`
3.23 版新增.

在build前解析来自外部软件包管理器(如 `NuGet`)的远程软件包引用.
当 `<value>` 设置为 `on`(默认)时, 将在构建目标之前 restore软件包.
当 `<value>` 设置为 `only` 时, 将还原软件包, 但不执行构建.
当 `<value>` 设置为 `off` 时, 将不会还原软件包.

如果目标没有定义任何软件包引用, 该选项将不起任何作用.

可以在 `build preset`中指定此设置
(使用 `resolvePackageReferences`). 如果指定了此命令行选项, 预设设置将被忽略.

如果没有提供命令行参数或预设选项,
则将计算 环境特定 的缓存变量, 以决定是否应执行软件包还原.
使用 Visual Studio 生成器时, 软件包引用是通过 `VS_PACKAGE_REFERENCES` 属性定义的.
软件包引用是通过 `NuGet` 还原的.
可以通过将 `CMAKE_VS_NUGET_PACKAGE_RESTORE ` 变量设置为 `OFF` 来禁用它.

+ `--use-stderr`
Ignored. 在 CMake >= 3.0 中为默认行为.

+ `-v, --verbose`
启用详细输出(如果支持), 包括要执行的build命令.
如果设置了 `VERBOSE` 环境变量或 `CMAKE_VERBOSE_MAKEFILE` 缓存变量, 则可省略此选项.

+ `--`
将其余选项传递给 native tool.

运行 `cmake --build` 且不带任何选项, 以获得快速帮助.

## install 项目

CMake 提供了安装 已生成的 项目二进制树 的命令行形式:

```bash
cmake --install <dir> [<options>]
```

可以在构建项目后使用该命令行签名,
以便在不使用生成的build系统或本地build工具的情况下运行安装. 选项如下

+ `--install <dir>`
要安装的 项目二进制目录. 一般就是 `./build` 目录
这是必需的, 必须放在前面.

+ `--config <cfg>`
对于多配置生成器, 选择配置 `<cfg>`.
例如 `Release`

+ `--component <comp>`
基于 组件 的安装. 只安装组件 `<comp>`.

+ `--default-directory-permissions <permissions>`
默认目录安装权限. 权限格式为 `<u=rwx,g=rx,o=rx>`.

`--prefix <prefix>`
覆盖安装前缀 [CMAKE_INSTALL_PREFIX](https://cmake.org/cmake/help/latest/variable/CMAKE_INSTALL_PREFIX.html#variable:CMAKE_INSTALL_PREFIX).

+ `--strip`
安装前 strip.

+ `-v, --verbose`
启用详细输出.
如果设置了 VERBOSE 环境变量, 则可省略此选项.

不带任何选项运行 `cmake --install` 可获得快速帮助.
