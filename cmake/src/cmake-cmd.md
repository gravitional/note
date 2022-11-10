# cmake 命令行

[cmake: 命令行工具cmake](https://blog.csdn.net/zhizhengguan/article/details/118339062)

## 构建项目

CMake提供了一个命令行签名来 build 已经生成的项目二叉树

```bash
cmake --build <dir> [<options>] [-- <build-tool-options>]
```

这将使用以下选项抽象本机构建工具的命令行界面:

+ `--install <dir>`
要安装的项目二进制目录. 这是必须的, 也必须是第一.
+ `--config <cfg>`
对于多配置生成器, 选择配置<cfg>
+ `--component <comp>`
基于组件的安装. 仅安装组件<comp> .
+ `--prefix <prefix>`
覆盖安装前缀CMAKE_INSTALL_PREFIX.
+ `--strip`
Strip before installing
+ `-v, --verbose`
启用详细输出.
如果设置了 [VERBOSE环境变量](https://cmake.org/cmake/help/v3.15/envvar/VERBOSE.html#envvar:VERBOSE), 则可以省略此选项.

运行cmake–install, 没有快速帮助选项.
