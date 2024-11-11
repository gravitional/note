# cpan 使用

## [windows msys2 perl](https://packages.msys2.org/groups/perl-modules)

windows 在 msys2 下使用 perl, 可以使用 pacman 安装 perl 常用发行包,
使用自带的 cpan 安装大概率会失败, 因为缺少 glibc 的很多头文件.

```bash
pacman -S perl-modules
```

## 自带 CPAN 使用说明

[How to install CPAN modules](https://www.cpan.org/modules/INSTALL.html)

安装 `cpanm`, 以便更轻松地安装其他模块.
你需要在终端模拟器(macOS, Win32, Linux)中键入以下命令

```bash
cpan App::cpanminus
```

现在安装你能找到的任何模块. `-n` 表示 `-no-test`

```bash
cpanm -n Module::Name
```

### 安装建议

```bash
cpanm --mirror http://mirrors.aliyun.com/CPAN -n  File::Utils CPAN ExtUtils::MakeMaker Module::Build Bundle::CPAN CPANPLUS App::pmuninstall
```

### cpan -T 跳过测试

如果需要安装 [Devel::REPL](https://metacpan.org/pod/Devel::REPL) 模块, 使用

```bash
cpan -i -T Devel::REPL
```

说明:

+ 网页上端 `Karen Etheridge/1.003029Devel-REPL-1.003029/Devel::REPL`,
最后的 `Devel::REPL` 即模块名称,
+ 左侧 `TOOLS` 下面有下载 tar.gz安装包 选项 `download`,

+ 可以使用 cpanm 从下载的本地包安装,

    ```bash
    cpanm ./MooseX-Getopt-0.76.tar.tz
    ```

但是会缺少依赖.

使用 `cpan` 安装的包, 二进制文件 和 lib 文件分别位于

```bash
C:\Strawberry\perl\site\bin
C:\Strawberry\perl\site\lib
```

## mirror 镜像

[CPAN 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/CPAN/)

CPAN (The Comprehensive Perl Archive Network)
镜像源的配置文件为 `MyConfig.pm`(一般位于 ~/.cpan/CPAN/MyConfig.pm), 可使用包管理脚本 cpan 进行修改.

### 初次使用

如果 `MyConfig.pm` 配置文件不存在, 在命令行中执行:

```bash
# 自动生成 MyConfig.pm
## 对于 Perl 5.36 (或 CPAN 2.29)及以上, 使用如下命令
PERL_MM_USE_DEFAULT=1 perl -MCPAN -e 'CPAN::HandleConfig->edit("pushy_https", 0); CPAN::HandleConfig->edit("urllist", "unshift", "https://mirrors.tuna.tsinghua.edu.cn/CPAN/"); mkmyconfig'
## 对于较久版本, 使用如下命令
PERL_MM_USE_DEFAULT=1 perl -MCPAN -e 'CPAN::HandleConfig->edit("urllist", "unshift", "https://mirrors.tuna.tsinghua.edu.cn/CPAN/"); mkmyconfig'

# 或不使用默认配置, 手动确认各个配置选项
perl -MCPAN -e 'mkmyconfig'
```

### 已有配置

在 CPAN Shell 中手动设置镜像
在命令行中执行 `cpan` 进入 cpan shell:

```bash
cpan shell -- CPAN exploration and modules installation
Enter 'h' for help.

# 列出当前的镜像设置
cpan[1]> o conf urllist

# 将本站镜像加入镜像列表首位
# 注: 若已在列表中则可跳过本步直接退出, 修改列表不会执行自动去重
cpan[2]> o conf urllist unshift https://mirrors.tuna.tsinghua.edu.cn/CPAN/

# 或将本站镜像加入镜像列表末尾
# 注: 本命令和上面的命令执行一个即可, 修改列表不会执行自动去重
cpan[3]> o conf urllist push https://mirrors.tuna.tsinghua.edu.cn/CPAN/

# 或清空镜像列表, 仅保留本站
cpan[4]> o conf urllist https://mirrors.tuna.tsinghua.edu.cn/CPAN/

# Perl 5.36 及以上用户需要关闭 pushy_https 以使用镜像站
cpan[5]> o conf pushy_https 0

# 保存修改后的配置至 MyConfig.pm
cpan[6]> o conf commit

# 退出 cpan shell
cpan[7]> quit
```

## cpanm 帮助

不支持 Locale `Chinese (Simplified)_China.936` 可能会导致解释器崩溃.
使用方法:

```bash
cpanm [options] Module [...]
```

### 选项

`-v,--verbose` 打开冗长输出
`-q,--quiet` 关闭最多输出
`--interactive` 开启交互式配置(`Task::` 模块需要)
`-f,--force` 强制安装
`-n,--notest` 不运行单元测试
`--test-only` 仅运行测试, 不安装
`-S,--sudo` 用 sudo 运行安装命令
`--installdeps` 仅安装依赖项
`--showdeps` 仅显示直接依赖项
`--reinstall` 即使已经安装了最新版本, 也要重新安装发行版
`--mirror` 指定镜像的基本 URL(如 `http://cpan.cpantesters.org/`)
`--mirror-only` 使用镜像的索引文件, 而不是 CPAN Meta DB
`-M,--from` 仅使用该镜像的基本 URL 及其索引文件
`--prompt`; 配置/构建/测试失败时提示
`-l,--local-lib` 指定安装模块的安装基础
`-L,--local-lib-contained` 指定安装所有非核心模块的安装基础
`--self-contained`; 安装所有非核心模块, 即使它们已经安装.
`--auto-cleanup`; cpanm 工作目录过期天数. 默认为 `7`

### 命令

`--self-upgrade`; 升级自身
`--info` Displays distribution info on CPAN
`--look` Opens the distribution with your SHELL
`-U,--uninstall`; 卸载模块(试验性的)
`-V,--version`; 显示软件版本

### 示例

```bash
cpanm Test::More # install Test::More
cpanm MIYAGAWA/Plack-0.99_05.tar.gz # 完整发布路径
cpanm http://example.org/LDS/CGI.pm-3.20.tar.gz # 从 URL 安装
cpanm ~/dists/MyCompany-Enterprise-1.00.tar.gz # 从本地文件安装
cpanm --interactive Task::Kensho # 交互式配置
cpanm . # 从本地目录安装
cpanm --installdeps . # 安装当前目录下的所有组件
cpanm -L extlib Plack # 将 Plack 和所有非核心依赖 安装到 `extlib` 中
cpanm --mirror http://cpan.cpantesters.org/ DBI # 使用 fast-syncing 镜像
cpanm -M https://cpan.metacpan.org App::perlbrew # 仅使用该安全镜像及其索引
```

Devel-REPL-1.003029

您也可以在 `shell` rc 的 `PERL_CPANM_OPT` 环境变量中指定默认选项:

```bash
export PERL_CPANM_OPT="--prompt --reinstall -l ~/perl --mirror http://cpan.cpantesters.org"
```

键入 `man cpanm` 或 `perldoc cpanm` 获取选项的更详细解释.
