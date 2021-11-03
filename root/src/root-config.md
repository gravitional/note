# root

[ROOT: Data Analysis Framework](https://root.cern/)
[What is Cling](https://root.cern/cling/)
[C++ 参考手册](https://zh.cppreference.com/w/cpp)
[Python interface: PyROOT ](https://root.cern/manual/python/)

`root` 是一个开源的数据分析框架, 被高能物理学,和其他方面使用.

`ROOT` 使统计学上的科学分析和大量数据的可视化成为可能:
今天, 超过 `1 exabyte`(1,000,000,000 gigabyte)存储在ROOT文件中.
[希格斯粒子是用ROOT发现的!](https://root.cern/gallery/#higgs-plots)

作为高性能软件, ROOT主要用C++编写.
你可以在Linux, macOS或Windows上使用它; 它开箱即可使用.
ROOT是开源的: 自由使用它,
[修改它](https://root.cern/install/build_from_source/),
[为它做贡献](https://root.cern/contribute/)

ROOT带有一个令人难以置信的[C++解释器](https://root.cern/primer/#learn-c-at-the-root-prompt), 是快速原型设计的理想选择.
不喜欢C++? 由于其独特的[动态和强大的Python⇄C++绑定](https://root.cern/manual/python/), ROOT与Python的集成非常顺畅.
[或者在Jupyter笔记本中使用ROOT怎么样?](https://nbviewer.org/url/root.cern/doc/master/notebooks/rf301_composition.C.nbconvert.ipynb)

## Installing ROOT

`ROOT` 可以在 `Linux`, `Mac` 和(作为测试版)Windows上使用.
最新的稳定版ROOT是6.24/06, [关于ROOT的版本计划](https://root.cern/about/versioning).

有几种方法可以在你的电脑上安装ROOT: 它们都列在右边的内容表中.
哪种方式最适合你, 取决于你的操作系统和使用要求.
在任何情况下, 请确保总是使用最新的ROOT版本, 以获得最新的错误修复, 功能和快速的用户支持.

### 下载预编译的二进制发行版

我们为几个主要的 `Linux` 发行版, 以及MacOS和(作为一个测试版)Windows分发预编译的ROOT.
安装预编译的二进制文件的步骤很简单.

+ 用系统软件包管理器安装所有需要的依赖项
+ 下载所需平台和ROOT版本的发行版
+ 解压缩档案
+ 通过获取适当的 `thisroot.*` 脚本将 `ROOT` 库和可执行文件添加到你的环境中.
这些设置脚本可以在ROOT二进制版本的 `bin` 目录下找到.

例如, 在 `Ubuntu 20` 上, 用户可以在安装完[所有需要的依赖项后](https://root.cern/install/dependencies/),
执行以下 `bash` 命令来安装 `ROOT v6.24/02`.

```bash
$ wget https://root.cern/download/root_v6.24.02.Linux-ubuntu20-x86_64-gcc9.3.tar.gz
$ tar -xzvf root_v6.24.02.Linux-ubuntu20-x86_64-gcc9.3.tar.gz
$ source root/bin/thisroot.sh # also available: thisroot.{csh,fish,bat}
```

为了避免每次需要使用 `ROOT` 时都必须使用 `thisroot.sh`, 典型的做法是将该命令添加到 `.bashrc`, `.profile` 或类似的配置文件中.
然而, 请注意, sourcing  `thisroot.sh` 可能会干扰用不同方法安装的 `ROOT` 版本.

而以 `Windows` 为例, 安装完成后, 打开VS 2019的x86原生工具命令提示符,
cd到你的主目录(`cd %USERPROFILE%`)并调用 `thisroot.bat` (假设你把ROOT安装在 `C:\root`).
然后你就可以启动 `ROOT` 了.

```log
**********************************************************************
** Visual Studio 2019 Developer Command Prompt v16.11.3
** Copyright (c) 2021 Microsoft Corporation
**********************************************************************
[vcvarsall.bat] Environment initialized for: 'x86'

C:\Program Files (x86)\Microsoft Visual Studio\2019\Community>cd %USERPROFILE%

C:\Users\username>c:\root\bin\thisroot.bat

C:\Users\username>root
   ------------------------------------------------------------------
  | Welcome to ROOT 6.25/01                        https://root.cern |
  | (c) 1995-2021, The ROOT Team; conception: R. Brun, F. Rademakers |
  | Built for win32 on Sep 20 2021, 11:34:39                         |
  | From heads/master@v6-25-01-1903-g6928212418                      |
  | With MSVC 19.29.30133.0                                          |
  | Try '.help', '.demo', '.license', '.credits', '.quit'/'.q'       |
   ------------------------------------------------------------------

root [0]
```

## 通过软件包管理器安装

>由社区支持: 这些软件包不是由 `ROOT团队` 维护的, 而是由社区中热心的成员维护.
>请通过每个软件包管理器的标准渠道来报告任何相关问题.
>如果你的打包了 `ROOT` , 希望被加入到下面的列表中, 请点击页面底部的信件图标联系我们.

### Conda

对于任何 `Linux` 发行版和 `MacOS`, `ROOT` 都可以作为一个 `conda` 软件包.
要创建一个包含 `ROOT` 的新 `conda` 环境并激活它, 请执行下面的命令, 注意将 `<你的环境名>` 修改成你喜欢的名字.

```bash
$ conda config --set channel_priority strict
$ conda create -c conda-forge --name <你的环境名> root
$ conda activate <你的环境名>
```

将 `channel_priority` 设置为 `strict` 是必须的, 以避免在某些平台上发生冲突,
更多信息请参见[相关的conda文档](https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-channels.html#strict-channel-priority).

conda软件包使用 `C++17`.

更多关于使用这个包的说明可以在[这篇博文中找到](https://iscinumpy.gitlab.io/post/root-conda/).

请在这里报告任何[关于conda包的问题](https://github.com/conda-forge/root-feedstock).

### Snap

在许多Linux发行版上, ROOT 可以通过 Snap 进行安装. 例如, 在Ubuntu上.

```bash
$ sudo snap install root-framework
$ snap run root-framework
# 或者如果不担心与其他安装冲突.
$ root # and the output of `which root` should contain `/snap`
```

Snap 包使用C++17, 更多信息请看我们的专门博文, 或者访问 [ROOT Snap包的官方页面](https://snapcraft.io/root-framework).

### Linux软件包管理器

在下列Linux发行版中, ROOT可以直接从操作系统的软件包管理器中安装.

#### Fedora

Fedora的[ROOT包](https://src.fedoraproject.org/rpms/root)可以通过以下方式安装

```bash
$ yum install root
```

然而, 更典型的是, 用户想要的不仅仅是基本软件包.
完整的组件列表可以在 [https://src.fedoraproject.org/rpms/root/](https://src.fedoraproject.org/rpms/root/) 看到, 点击其中一个提供的版本即可看到.
例如, 要安装支持python和笔记本的ROOT, 请运行

```bash
$ yum install root python3-root root-notebook
```

#### CentOS

ROOT通过[EPEL](https://fedoraproject.org/wiki/EPEL)在CentOS上 使用. 要在CentOS上安装ROOT, 只需运行

```bash
$ yum install epel-release
$ yum install root
```

#### Arch Linux

`Arch` 的 [ROOT 包](https://www.archlinux.org/packages/community/x86_64/root)可以通过以下方式安装

```bash
$ pacman -Syu root
```

Arch软件包使用C++17.

#### Gentoo

Gentoo的ROOT包是[sci-physics/root](https://packages.gentoo.org/packages/sci-physics/root). 它可以通过以下方式安装

```bash
$ emerge sci-physics/root
```

#### NixOS/Nix/Nixpkgs

nixpkgs中ROOT的软件包名称是 `root`. 它可以被安装到用户环境中, 使用

```bash
$ nix-env -f '<nixpkgs>' -iA root
```

在临时环境中运行可以通过以下方式实现

```bash
$ nix-shell -p root --run root
```

为支持传统软件, 提供了一个 `root5` 软件包.

如果你遇到任何问题, 请随时向 [nixpkgs问题跟踪器报告](https://github.com/NixOS/nixpkgs/issues).

#### 基于Ubuntu和Debian的发行版

ROOT团队正在努力发布一个官方的 `.deb` 包. 很快会有更多关于这个主题的消息.

同时, `ROOT` 在 Ubuntu 上可以通过 [conda](https://root.cern/install/#conda),
或我们 [预编译的二进制文件使用](https://root.cern/install/#download-a-pre-compiled-binary-distribution).

#### MacOS软件包管理器

Homebrew

在Mac上, ROOT也可以作为一个[homebrew formula](https://formulae.brew.sh/formula/root)使用.
你可以用以下方式安装它

```bash
$ brew install root
```

Macports

[安装完macports](https://www.macports.org/install.php)后, 可以用以下方式安装[ROOT port](https://ports.macports.org/port/root6/summary/)

```bash
$ sudo port install root6
```

#### Nix/Nixpkgs

在 macOS 上运行时, 与[ Linux 的说明相同](https://root.cern/install/#nixos_nix_nixpkgs).

### 在 CVMFS 上的 LCG 版本

`CernVM` 文件系统提供了一个可扩展, 可靠和低维护的软件分发服务.
它的开发是为了帮助高能物理(HEP)合作组织在用于运行数据处理应用的全球分布式计算基础设施上部署软件.
CernVM-FS是作为用户空间的POSIX只读文件系统(FUSE模块)来实现的.
文件和目录被托管在标准的网络服务器上, 并挂载在通用命名空间 `/cvmfs`  中(universal namespace).

在内部, CernVM-FS 使用内容可寻址存储和 `Merkle树`, 以存储文件数据和元数据.
`CernVM-FS` 只使用出站的HTTP连接, 因此它避免了其他网络文件系统的大部分防火墙问题.
它按需传输数据和元数据, 并通过`加密哈希值`(cryptographic hashes)验证数据的完整性.

#### Standalone ROOT

如果你的平台安装了[CVMFS](https://cernvm.cern.ch/portal/filesystem),
ROOT 可以直接通过[LCG发布](http://lcginfo.cern.ch/) 使用.

对Fedora, Ubuntu, CentOS7和MacOS来说, 具有最小外部依赖性的ROOT安装可以在以下网站上找到.

```conf
/cvmfs/sft.cern.ch/lcg/app/releases/ROOT/<version>/<platform>
```

例如, 要在CentOS7机器上设置ROOT 6.24/02, 只需运行.

```bash
source /cvmfs/sft.cern.ch/lcg/app/releases/ROOT/6.24.02/x86_64-centos7-gcc48-opt/bin/thisroot.sh
```

确保你使用你系统的默认编译器, 就像这个ROOT构建一样.

#### 完整的环境

ROOT, Geant4和许多其他软件包及其所有的依赖项, 都可以在`LCG views` 上找到.

```bash
/cvmfs/sft.cern.ch/lcg/views/LCG_<version>/<platform>
```

`LCG views` 可用于CentOS7, CentOS8和最新的MacOS和Ubuntu版本.
例如, 在CERN LXPLUS上, 你可以通过以下方式建立一个包含ROOT 6.24/00的完整环境.

```bash
source /cvmfs/sft.cern.ch/lcg/views/LCG_100/x86_64-centos7-gcc10-opt/setup.sh
```

要检查LCG版本中包含的ROOT版本, 你可以访问[lcginfo.cern.ch](http://lcginfo.cern.ch/).

#### Gentoo Prefix on CVMFSPermalink

ROOT 也可以在`SFT CVMFS仓库`的 `contrib区域`内的 [Gentoo Prefix](https://wiki.gentoo.org/wiki/Project:Prefix) 安装中使用(实验性的) .
要从那里使用它, 请运行

```bash
$ /cvmfs/sft.cern.ch/lcg/contrib/gentoo/linux/x86_64/startprefix
```

这将使你进入一个新的shell, 在这个 `shell` 中所有的软件都可以使用.

#### 在Docker容器中运行

在[ROOT的官方DockerHub](https://hub.docker.com/r/rootproject)上, 可以找到几个 linux 风格 的ROOT Docker容器.

例如, 要尝试最新的ROOT版本, 只需运行 `docker run -it rootproject/root`.

#### 在CERN LXPLUS上运行

拥有CERN计算账户的用户, 可以简单地通过`SSH`连接到 `lxplus.cern.ch` 并启动 `root`:
最新的稳定版本会作为普通系统包安装.

请注意, 由于CentOS7上某些ROOT依赖的版本不兼容,
某些功能(如多线程功能)在 `lxplus.cern.ch`(或等价的 `lxplus7.cern.ch`)上不可用.
你可以使用 `lxplus8.cern.ch` 来访问 `CentOS8`, 那里不存在这种限制.

### 从源代码构建

如果没有其他安装方法, 或者你想完全控制 `ROOT` 的构建选项, 可以从源代码编译ROOT.
详细说明见从源码构建ROOT.

简而言之, 在安装完所有需要的依赖项后, ROOT可以在大多数类似UNIX的系统上用这些命令进行编译.

```bash
# 最新的稳定分支会在每次发布时自动更新.
# 你可以通过在`root_src/`中发布`git pull`命令来更新你的本地副本.
$ git clone --branch latest-stable https://github.com/root-project/root.git root_src
$ mkdir root_build root_install && cd root_build
$ cmake -DCMAKE_INSTALL_PREFIX=../root_install ../root_src # && check cmake configuration output for warnings or errors
$ cmake --build . -- install -j4 # if you have 4 cores available for compilation
$ source ../root_install/bin/thisroot.sh # or thisroot.{fish,csh}
```

而类似地, 在Windows上, 在VS 2019的x86原生工具命令提示符里面, 可以用这些命令编译 `ROOT`.

```bat
rem `latest-stable`分支会在每次发布时自动更新.
rem 你可以通过在`root_src`中发布`git pull`命令来更新你的本地副本.
C:\Users\username>git clone --branch latest-stable https://github.com/root-project/root.git root_src
C:\Users\username>mkdir root_build root_install && cd root_build
C:\Users\username>cmake -G"Visual Studio 16 2019" -A Win32 -Thost=x64 -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_INSTALL_PREFIX=../root_install ../root_src
C:\Users\username>cmake --build . --config Release --target install
C:\Users\username>..\root_install\bin\thisroot.bat
```

## Get Started

[Get Started](https://root.cern/get_started/)

[ROOT初学者指南](https://root.cern/primer/)(又称 "入门手册"), 无疑是掌握ROOT力量的第一份文件, 需要阅读.

[root 手册](https://root.cern/manual/)向你介绍了ROOT的主要部分.

在这个介绍之后, [教程演示](https://indico.cern.ch/event/395198/)(和视频! )提供了一个不同的, 更直接的ROOT基本概念的方法, 以及一些实践练习.
为了整合上述资料中的信息, 这个[ROOT课程集](https://root.cern/get_started/courses/)绝对是有用的.
一旦掌握了基本概念, 就可以通过[丰富的教程](https://root.cern/tutorials/)来深入学习代码.

记住, 你可以随时求助于[ROOT论坛](https://root-forum.cern.ch/), 以了解是否有人已经解决了你所面临的问题, 或者获得帮助!
