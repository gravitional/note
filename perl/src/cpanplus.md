# cpan, perl 包管理

## CPANPLUS

cpanp - CPANPLUS 启动器

### 说明

```bash
cpanp

cpanp [-]a [ --[no-]*option*... ]  *author*...
cpanp [-]mfitulrcz [ --[no-]*option*... ]  *module*...
cpanp [-]d [ --[no-]*option*... ] [ --fetchdir=... ]  *module*...
cpanp [-]xb [ --[no-]*option*... ]
cpanp [-]o [ --[no-]*option*... ] [ *module*... ]
```

### 描述

此脚本启动 CPANPLUS 实用程序, 以从命令行执行各种操作
如果调用时没有参数, 则默认执行交互式 shell.

作为选项, 它可以接受一个单字母开关和一个或多个参数, 对每个参数执行相关操作.
下面列出了可用命令的摘要; `cpanp -h` 提供了详细列表.

+ `h` # 帮助信息
+ `v` # 版本信息

+ `a 作者 ...`  # 按作者搜索
+ `m 模块 ...`  # 按模块搜索
+ `f MODULE ...` # 列出模块的所有版本

+ `i MODULE ...`  # 安装模块
+ `t MODULE ...`  # 测试模块
+ `u MODULE ...`  # 卸载模块
+ `d MODULE ...`  # 下载模块
+ `l MODULE ...`  # 显示模块的详细信息
+ `r MODULE ...`  # 显示模块的 README 文件
+ `c MODULE ...`  # 检查来自 cpan-testers 的模块报告
+ `z MODULE ...`  # 提取模块并在其中打开命令提示符

+ `x`; 重新加载 CPAN 索引
+ `o [ MODULE ... ]`# 列出已安装但尚未更新的模块
+ `b`; 为您的配置写一个 bundle 文件

每条命令后面都可以有一个或多个 `options`.
如果 preceded by `no`, 则相应选项将设为 `0`, 否则设为 `1`.

例如 跳过模块测试

```bash
cpanp -i --skiptest MODULE ...
```

大多数命令的有效选项是 "cpantest", "debug", "flush",
"force", "prereqs", "storable", "verbose", "md5", "signature "和
skiptest";
`d` 命令也接受 `fetchdir`.

请参考 `CPANPLUS::Configure` 对其含义的解释.

示例 下载模块的 tar 包到当前目录

```bash
cpanp -d --fetchdir=. MODULE ...
```
