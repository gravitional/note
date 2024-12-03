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
列出可用的 编译预置. 当前工作目录必须包含 CMake 预设文件.

+ `-j [<jobs>], --parallel [<jobs>]`
3.12 版新增.
编译时使用的最大并发进程数. 如果省略 `<jobs>`, 则使用本地build工具的默认进程数.
如果设置了 `CMAKE_BUILD_PARALLEL_LEVEL` 环境变量, 则会当作此选项默认的并行级别.
有些本地编译工具总是并行编译. `<jobs>` 值为 `1` 时, 可用于限制为单个作业.

+ `-t <tgt>..., --target <tgt>...`
构建 `<tgt>` 而不是默认目标. 可以给出多个目标, 以空格分隔.

+ `--config <cfg>`
对于 `多配置工具`, 请选择配置 `<cfg>`.
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

## 运行命令行工具

CMake 通过以下签名提供内置命令行工具

```bash
cmake -E <command> [<options>]
```

+ `-E [help］`
运行 cmake -E 或 cmake -E help 可获取命令摘要.

可用的命令有

### capabilities

在 3.7 版中添加.
以 JSON 格式报告 cmake 功能. 输出是一个 JSON 对象, 包含以下键:

+ `version`
包含版本信息的 JSON 对象. 键是
  + `string`; cmake --version 显示的完整版本字符串.
  + `major`; 整数形式的主版本号.
  + `minor`; 以整数形式表示的次版本号.
  + `patch`;补丁级别(整数形式).
  + `suffix`;cmake 版本后缀字符串.
  + `isDirty`; 一个 bool, 如果 cmake 编译来自一个 dirty tree, 则设置该 bool.

### `generators`

可用生成器列表. 每个生成器都是一个 JSON 对象, 包含以下关键字:
    name
    包含生成器名称的字符串.

    toolsetSupport
    如果生成器支持工具集, 则为 true;否则为 false.

    platformSupport
    如果生成器支持平台, 则为 true;否则为 false.

    supportedPlatforms
    在 3.21 版中添加.

    可选成员, 当通过 CMAKE_GENERATOR_PLATFORM (-A ...)生成器支持平台规范时可能出现. 其值是已知支持的平台列表.

    extraGenerators
    包含与生成器兼容的所有额外生成器的字符串列表.

### `fileApi`

当 cmake-file-api(7) 可用时出现的可选成员.
其值是一个包含一个成员的 JSON 对象:

    requests
    一个 JSON 数组, 包含零个或多个受支持的文件-api 请求. 每个请求都是一个 JSON 对象, 其成员包括
        kind
        指定支持的对象类型之一.

        version
        一个 JSON 数组, 其中每个元素都是一个 JSON 对象, 包含 major 和 minor 成员, 指定非负整数版本组件.
    serverMode
    如果 cmake 支持服务器模式则为 true, 否则为 false. 自 CMake 3.20 起始终为 false.

    tls
    在 3.25 版中添加.
    如果启用了 TLS 支持则为 true, 否则为 false.

    debugger
    在 3.27 版中添加.

如果支持 --debugger 模式, 则为 true;否则为 false.

### `cat [--] <files>...`

在 3.18 版中添加.
连接文件并打印到标准输出上.
    `--`
    在 3.24 版中添加.

    添加了对双破折号参数 -- 的支持. cat 的基本实现不支持任何选项, 因此使用以 - 开头的选项会导致错误. 如果文件以"-"开头, 则使用"--"表示选项结束.

    3.29 版新增: 通过传递 - 参数, cat 现在可以打印标准输入.

### `chdir <dir> <cmd> [<arg>...] `

更改当前工作目录并运行一个命令.
更改当前工作目录并运行命令.

### `compare_files [--ignore-eol] <file1> <file2>`

检查 <file1> 是否与 <file2> 相同. 如果文件相同, 则返回 0, 否则返回 1. 如果参数无效, 则返回 2.

`--忽略-eol`
在 3.14 版中添加.
该选项表示按行比较, 忽略 LF/CRLF 的差异.

### `copy <file>... <destination>, copy -t <destination> <file>...`

复制文件到 <destination>(文件或目录). 如果指定了多个文件, 或指定了 -t, <destination> 必须是目录且必须存在. 如果未指定 -t, 则假定最后一个参数为 <destination>. copy 不支持通配符. 这意味着它复制的不是符号链接, 而是指向的文件或目录.

3.5 版新增:  支持多个输入文件.

3.26 版新增:  支持 -t 参数.

### `copy_directory <dir>... <destination>`

将 `<dir>...` 目录的内容复制到 <destination> 目录. 如果 <destination> 目录不存在, 则会创建该目录. copy_directory 不会跟踪符号链接.

第 3.5 版新增:  支持多个输入目录.

第 3.15 版新增:  源目录不存在时, 命令会失败. 在此之前, 它会通过创建一个空的目标目录而成功.

### `copy_directory_if_different <dir>... <destination>`

在 3.26 版中添加.

将 `<dir>...` 目录中已更改的内容复制到 <destination> 目录. 如果 <destination> 目录不存在, 则将创建该目录.

copy_directory_if_different 遵循符号链接. 如果源目录不存在, 命令将失败.

### `copy_if_different <file>... <destination>`

将已更改的文件复制到 <destination>(文件或目录). 如果指定了多个文件, <destination> 必须是目录, 而且必须存在. copy_if_different 不会跟踪符号链接.

3.5 版新增: 支持多个输入文件.

### `create_symlink <old> <new>`

创建命名为 <old> 的符号链接 <new>.

已在 3.13 版中添加:  支持在 Windows 上创建符号链接.

注意 创建 <new> 符号链接的路径必须事先存在.

### `create_hardlink <old> <new>`

在 3.19 版中添加.

创建命名为 <old> 的 <new> 硬链接.
注意 创建 <new> 硬链接的路径必须事先存在. <old> 必须事先存在.

### `echo [<string>...]`

以文本形式显示参数.

### `echo_append [<string>...]`

以文本形式显示参数, 但不换行.

### `env [<options>] [--] <command> [<arg>...]`

在 3.1 版中添加.

在修改后的环境中运行命令. 选项如下

`NAME=VALUE`
用 VALUE 替换 NAME 的当前值.

`--unset=NAME`
取消设置 NAME 的当前值.

`--modify ENVIRONMENT_MODIFICATION`
在 3.25 版中添加.

对修改后的环境执行一次 ENVIRONMENT_MODIFICATION 操作.

`NAME=VALUE` 和 `--unset=NAME` 选项分别等同于
 `--modify NAME=set:VALUE` 和 `--modify NAME=unset:`.
请注意, --modify NAME=reset: 将 NAME 重置为 cmake 启动时的值 (或取消设置), 而不是最新的 NAME=VALUE 选项.

--
已在 3.24 版中添加.

新增对双破折号参数 -- 的支持. 使用 -- 停止解释选项/环境变量, 并将下一个参数视为命令, 即使它以 - 开头或包含 =.

### `environment`

显示当前环境变量.

### `false`

在 3.16 版中添加.

不执行任何操作, 退出代码为 1.

### `make_directory <dir>...`

创建 `<dir>` 目录. 如有必要, 也会创建父目录. 如果目录已经存在, 则会被静默忽略.

3.5 版新增: 支持多个输入目录.

### `md5sum <file>...`

以 md5sum 兼容格式创建文件的 MD5 校验和:

```bash
351abe79cd3800b38cdfb25d45015a15 file1.txt
052f86c15bbde68af55c7f7b340ab639 file2.txt
```

### `sha1sum <file>...`

已在 3.10 版中添加.

以与 sha1sum 兼容的格式创建文件的 SHA1 校验和:

4bb7932a29e6f73c97bb9272f2bdc393122f86e0 file1.txt
1df4c8f318665f9a5f2ed38f55adadb7ef9f559c file2.txt

### `sha224sum <file>...`

已在 3.10 版中添加.

以与 sha224sum 兼容的格式创建文件的 SHA224 校验和:

b9b9346bc8437bbda630b0b7ddfc5ea9ca157546dbbf4c613192f930 file1.txt
6dfbe55f4d2edc5fe5c9197bca51ceaaf824e48eba0cc453088aee24 file2.txt

### `sha256sum <file>...`

已在 3.10 版中添加.

以与 sha256sum 兼容的格式创建文件的 SHA256 校验和:

76713b23615d31680afeb0e9efe94d47d3d4229191198bb46d7485f9cb191acc file1.txt
15b682ead6c12dedb1baf91231e1e89cfc7974b3787c1e2e01b986bffadae0ea file2.txt

### `sha384sum <file>...`

在 3.10 版中添加.
在 sha384sum com 中创建文件的 SHA384 校验和.

acc049fedc091a22f5f2ce39a43b9057fd93c910e9afd76a6411a28a8f2b8a12c73d7129e292f94fc0329c309df49434  file1.txt
668ddeb108710d271ee21c0f3acbd6a7517e2b78f9181c6a2ff3b8943af92b0195dcb7cce48aa3e17893173c0a39e23d  file2.txt

### `sha512sum <file>...`

已在 3.10 版中添加.

以与 sha512sum 兼容的格式创建文件的 SHA512 校验和:

2a78d7a6c5328cfb1467c63beac8ff21794213901eaadafd48e7800289afbc08e5fb3e86aa31116c945ee3d7bf2a6194489ec6101051083d1108defc8e1dba89 file1.txt
7a0b54896fe5e70cca6dd643ad6f672614b189bf26f8153061c4d219474b05dad08c4e729af9f4b009f1a1a280cb625454bf587c690f4617c27e3aebdf3b7a2d file2.txt

### `remove [-f] <file>...`

自 3.17 版起已弃用.

删除文件. 原计划的行为是, 如果列出的任何文件已经不存在, 命令会返回一个非零的退出代码, 但不会记录任何信息. 在这种情况下, -f 选项将改变行为, 返回零退出代码(即成功). 这意味着它只会删除符号链接, 而不会删除指向的文件.

在不破坏向后兼容性的情况下无法修复. 请使用 rm 代替.

### `remove_directory <dir>...`

自 3.17 版起已被弃用.

删除 `<dir>` 目录及其内容. 如果目录不存在, 将被静默忽略. 请使用 rm 代替.

3.15 版新增:  支持多目录.

3.16 版新增:  如果 `<dir>` 是指向某个目录的符号链接, 则只移除符号链接.

### `rename <oldname> <newname>`

重命名文件或目录(在一个卷上). 如果名称为 <newname> 的文件已经存在, 则会被静默替换.

### `rm [-rRf] [--] <file|dir>...`

在 3.17 版中添加.

删除文件 `<file>` 或目录 `<dir>`. 使用 -r 或 -R 可以递归删除目录及其内容. 如果列出的任何文件/目录不存在, 命令会返回一个非零的退出代码, 但不会记录任何信息. 在这种情况下, -f 选项会改变行为, 返回零退出代码(即成功). 使用 -- 停止解释选项, 并将所有剩余参数视为路径, 即使它们以 - 开头.

### `sleep <number>`

在 3.0 版中添加.
睡眠 `<number>` 秒.
`<number>` 可以是浮点数. 由于启动/停止 CMake 可执行文件的开销, 实际最小值约为 0.1 秒.
这可以在 CMake 脚本中插入延迟:

```bash
# 休眠约 0.5 秒
execute_process(COMMAND ${CMAKE_COMMAND} -E sleep 0.5)
```

+ `tar [cxt][vf][zjJ] file.tar [<options>] [--] [<pathname>...]`
创建或解压 tar 或 zip 压缩包. 选项有

c
创建包含指定文件的新压缩包. 如果使用, <pathname>... 参数是必须的.

x
从归档文件中提取到磁盘.

在 3.15 版中添加: <pathname>... 参数可用于只提取选定的文件或目录. 提取选定的文件或目录时, 必须提供包括路径在内的准确名称, 如 list (-t) 所打印的那样.

t
列出压缩包内容.

在 3.15 版中添加: <pathname>... 参数可用于只列出选定的文件或目录.

v
产生冗长输出.

z
使用 gzip 压缩生成的压缩包.

j
使用 bzip2 压缩生成的压缩包.

J
在 3.1 版中添加.

使用 XZ 压缩生成的压缩包.

--zstd
已在 3.15 版中添加.

使用 Zstandard 压缩生成的压缩包.

--files-from=<file> 文件
已在 3.1 版中添加.

从给定文件中读取文件名, 每行一个. 空行将被忽略. 除了--add-file=<name>用于添加名称以-开头的文件外, 其他行不得以-开头.

--format=<格式
在 3.3 版中添加.

指定要创建的压缩包格式. 支持的格式有 7zip, gnutar, pax, paxr(受限 pax, 默认)和 zip.

`--mtime=<date>`
在 3.1 版中添加.

指定记录在压缩包条目中的修改时间.

--touch
在 3.24 版中添加.

使用当前本地时间戳, 而不是从压缩包中提取文件时间戳.

--
在 3.1 版中添加.

停止解释选项, 并将所有剩余参数视为文件名, 即使它们以 - 开头.

已在 3.1 版中添加:  支持 LZMA (7zip).

在 3.15 版中新增: 即使某些文件无法读取, 命令也会继续向压缩包中添加文件.
这种行为与传统的 tar 工具更为一致. 命令现在还会解析所有标记, 如果提供了无效标记, 则会发出警告.

### `time <command> [<args>...]`

运行 <command> 并显示已耗费的时间(包括 CMake 前端的开销).

3.5 版新增内容: 该命令现在能正确地将带有空格或特殊字符的参数传递给子进程.
这可能会破坏那些通过额外引号或转义来解决 bug 的脚本.

### `touch <file>...`

如果文件不存在, 则创建 <file>. 如果 <file> 已存在, 则会更改 <file> 的访问和修改次数.

### `touch_nocreate <file>...`

如果文件存在, 则触摸文件, 但不创建文件. 如果文件不存在, 它将被静默忽略.

### `true`

在 3.16 版中添加.

不执行任何操作, 退出代码为 0.

## 特定于 Windows 的命令行工具

以下 cmake -E 命令仅适用于 Windows:

+ `delete_regv <key>`
删除 Windows 注册表

+ `env_vs8_wince <sdkname>`
在 3.2 版中添加.

显示为 VS2005 中安装的 Windows CE SDK 设置环境的批处理文件.

+ `env_vs9_wince <sdkname>`
已在 3.2 版中添加.

显示为 VS2008 中安装的 Windows CE SDK 设置环境的批处理文件.

+ `write_regv <key> <value>`
写入 Windows 注册表值.
