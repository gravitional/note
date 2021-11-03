# 各种 Conda

[Conda](https://docs.conda.io/projects/conda/en/latest/#)
[anaconda-glossary](https://docs.conda.io/projects/conda/en/latest/glossary.html#anaconda-glossary)

## Conda

Conda 是 任何语言的`包`, `依赖`和`环境` 的管理工具--Python, R, Ruby, Lua, Scala, Java, JavaScript, C/ C++, FORTRAN

Conda是一个开源的软件包管理系统和环境管理系统, 可以在Windows, macOS和Linux上运行.
Conda可以快速安装, 运行和更新软件包及其依赖关系. Conda可以在你的本地计算机上轻松创建, 保存, 加载和切换环境. 它是为Python程序创建的, 但它可以为任何语言打包和分发软件.

Conda作为一个软件包管理器帮助你寻找和安装软件包.
如果你需要一个需要不同版本的Python的软件包, 你不需要切换到不同的环境管理器, 因为conda也是一个环境管理器.
只需几个命令, 你就可以建立一个完全独立的环境来运行那个不同版本的 Python, 同时继续在你的正常环境中运行你通常的 Python 版本.

在其默认配置中, conda 可以安装和管理 repo.anaconda.com 上超过 7,500 个由 Anaconda 构建, 审核和维护的软件包.

Conda可以与Travis CI和AppVeyor等持续集成系统相结合, 为你的代码提供频繁的自动化测试.

Conda软件包和环境管理器包含在所有版本的Anaconda, Miniconda和Anaconda Repository中.

Conda也包含在Anaconda Enterprise中, 它为Python, R, Node.js, Java和其他应用堆栈提供现场企业包和环境管理.
Conda也可以在conda-forge, 一个社区频道上使用. 你也可以在[PyPI](https://pypi.org/)上获得Conda, 但这种方式可能没有那么及时.

### Anaconda

一个可下载的, 免费的, 开源的, 高性能的, 优化的Python和R分布.
Anaconda包括conda, conda-build, Python和250多个自动安装的开源科学软件包及其依赖关系,
这些软件包已经过测试, 可以很好地协同工作, 包括SciPy, NumPy和许多其他软件包.

使用conda安装命令, 可以轻松地从Anaconda仓库中安装7500多个用于数据科学的流行开源包--包括高级和科学分析.
使用conda命令来安装数以千计的开源包.

由于Anaconda是一个Python发行版, 它可以使安装Python快速而简单, 即使是新用户.

可用于Windows, macOS和Linux, 所有版本的Anaconda都得到社区的支持.

### Miniconda

一个免费的conda最小安装程序.
[Miniconda](https://docs.conda.io/en/latest/miniconda.html) 是 Anaconda 的一个小型引导版本,
只包括 conda, Python, 它们所依赖的软件包, 以及一小部分其他有用的软件包, 包括 pip, zlib 和其他一些.
使用 conda install 命令可以从 Anaconda 仓库中安装 7,500 多个额外的 conda 包.

## 安装

[Installing on Linux](https://conda.io/projects/conda/en/latest/user-guide/install/linux.html)

### 在Linux上安装

1. 下载安装程序.
    + [Miniconda安装程序, 适用于Linux](https://docs.conda.io/en/latest/miniconda.html#linux-installers).
    + [用于Linux的Anaconda安装程序](https://www.anaconda.com/download/).
2. [验证你的安装程序的哈希值](https://conda.io/projects/conda/en/latest/user-guide/install/download.html#hash-verification).

`SHA-256` 校验可用于Miniconda和Anaconda. 我们不建议使用MD5验证, 因为SHA-256更安全.

```bash
#   Windows, 打开PowerShell控制台, 按以下方式验证文件.
Get-FileHash 文件名 -Algorithm SHA256
#   macOS: 在iTerm或终端窗口输入
shasum -a 256 文件名
#  Linux系统. 在终端输入
sha256sum 文件名
```

3. 在你的终端窗口中, 运行.
    + Miniconda:

        ```bash
        bash Miniconda3-latest-Linux-x86_64.sh
        ```

    + Anaconda:

        ```bash
        bash Anaconda-latest-Linux-x86_64.sh
        ```

4. 按照安装程序屏幕上的提示进行操作.
如果你对任何设置不确定, 请接受默认值. 你可以稍后改变它们.
5. 要使更改生效, 请关闭并重新打开你的终端窗口.
6. 测试你的安装. 在你的终端窗口或Anaconda Prompt中, 运行命令conda list.
如果已经正确安装, 会出现一个已安装软件包的列表.

如果你希望不在启动时激活 `conda` 的 `base` 环境, 就设置 `auto_activate_base` 参数为 `false`.

```bash
conda config --set auto_activate_base false
```

#### 与fish shell一起使用

要在fish shell中使用conda, 在你的终端中运行下面的命令.

```bash
conda init fish
```

#### 在静默模式下安装

请参阅[在macOS上以静默模式安装的说明](https://conda.io/projects/conda/en/latest/user-guide/install/macos.html#install-macos-silent).

### 更新Anaconda或Miniconda

打开一个终端窗口, 运行 `conda update conda`.

### 卸载 Anaconda 或 Miniconda

+ 打开一个终端窗口, 用以下方法删除整个 `Miniconda` 安装目录.

    ```bash
    rm -rf ~/miniconda
    ```

+ 可选: 编辑 `~/.bash_profile`, 从你的PATH环境变量中删除Miniconda目录.
+ 可选: 删除可能在主目录中创建的下列隐藏文件和文件夹.
    + `.condarc` 文件
    + `.conda` 目录
    + `.continuum` 目录

    通过运行

    ```bash
    rm -rf ~/.condarc ~/.conda ~/.continuum
    ```

## 国内镜像

[Anaconda 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/anaconda/)

Anaconda 安装包可以到 https://mirrors.tuna.tsinghua.edu.cn/anaconda/archive/ 下载.

TUNA 还提供了 Anaconda 仓库与第三方源(`conda-forge`, `msys2`, `pytorch` 等, 查看完整列表)的镜像,
各系统都可以通过修改用户目录下的 `.condarc` 文件.
Windows 用户无法直接创建名为 `.condarc` 的文件, 可先执行 `conda config --set show_channel_urls yes` 生成该文件之后再修改.

注: 由于更新过快难以同步, 我们不同步 `pytorch-nightly`, `pytorch-nightly-cpu`, `ignite-nightly` 这三个包.

```yaml
channels:
  - defaults
show_channel_urls: true
default_channels:
  - https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/main
  - https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/r
  - https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/msys2
custom_channels:
  conda-forge: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  msys2: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  bioconda: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  menpo: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  pytorch: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  simpleitk: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
```

即可添加 Anaconda Python 免费仓库.

运行 `conda clean -i` 清除索引缓存, 保证用的是镜像站提供的索引.

运行 `conda create -n 你的环境名 numpy`  测试一下吧.

### Miniconda 镜像使用帮助

`Miniconda` 是一个 Anaconda 的轻量级替代, 默认只包含了 python 和 conda, 但是可以通过 pip 和 conda 来安装所需要的包.

`Miniconda` 安装包可以到 [这里](https://mirrors.tuna.tsinghua.edu.cn/anaconda/miniconda/) 下载.

## conda

+ `conda create` ; 从指定的软件包列表中创建一个新的 `conda` 环境. 这个命令需要 `-n NAME` 或 `-p PREFIX` 选项.
要使用创建过的环境, 首先使用 `conda activate 环境名` 在该目录中查找.

Create a new conda environment from a list of specified packages. To use the created environment, use 'conda activate envname' look in that directory first.  This command requires either the -n NAME or -p PREFIX option.
