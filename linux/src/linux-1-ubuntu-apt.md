# linux-ubuntu

### apt 与 apt-get

[Linux中apt与apt-get命令的区别与解释](https://www.sysgeek.cn/apt-vs-apt-get/)

如果你已阅读过我们的 `apt-get` 命令指南,可能已经遇到过许多类似的命令,如`apt-cache`,`apt-config` 等.如你所见,这些命令都比较低级又包含众多功能,普通的 Linux 用户也许永远都不会使用到.换种说法来说,就是最常用的 Linux 包管理命令都被分散在了 `apt-get`,`apt-cache` 和 `apt-config` 这三条命令当中.

`apt` 命令的引入就是为了解决命令过于分散的问题,它包括了 `apt-get` 命令出现以来使用最广泛的功能选项,以及 `apt-cache` 和 `apt-config` 命令中很少用到的功能.
在使用 apt 命令时,用户不必再由 `apt-get` 转到 `apt-cache` 或 `apt-config`,而且 apt 更加结构化,并为用户提供了管理软件包所需的必要选项.

> 简单来说就是:`apt = apt-get`,`apt-cache` 和 `apt-config` 中最常用命令选项的集合.

***
`apt`

`install, remove, purge (apt-get(8))`
`apt list`(半成品)
`apt list`类似于`dpkg-query --list`, 它可以显示满足某些条件的软件包列表.

它支持用`glob(7)`匹配软件包名称, 以及列出已安装(`--installed`), 可升级(`--upgradeable`)或所有可用(`--all-versions`)版本的选项.

另外也可以用`whereis`

`whereis` - 找到命令的二进制文件, 源文件和 man 文件

***
`apt-get --install-suggests`

将建议的软件包视为安装的依赖项. 配置项:`APT::Install-Suggests`.

```bash
apt-get -f install pkg
```

### dpkg 应用管理

+ `ldd /bin/ls` : `ldd`查看依赖信息
+ `dpkg -i pkg`: 安装`pkg.deb`
+ `dpkg -r pkg`: 删除已安装的程序包
+ `dpkg -P pkg`: 彻底清除已安装的程序包
+ `dpkg -l, --list package-name-pattern...`: 列出与给定模式匹配的软件包.
+ `dpkg -s, --status package-name...`: 报告指定软件包的状态.
+ `dpkg -L, --listfiles package-name...`: 从软件包名称列出安装到系统的文件.
+ `dpkg -S, --search filename-search-pattern...` 从已安装的软件包中搜索文件名.
+ `dpkg -p, --print-avail package-name...` 显示有关软件包名称的详细信息, 存放在`/var/lib/dpkg/available`,基于`APT`的前端的用户使用`apt-cache`

### ubunut 安装 typora

[typora for linux](https://www.typora.io/#linux)

```bash
# or run:
# sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys BA300B7755AFCFAE
wget -qO - https://typora.io/linux/public-key.asc | sudo apt-key add -
# add Typora's repository
sudo add-apt-repository 'deb https://typora.io/linux ./'
sudo apt-get update
# install typora
sudo apt-get install typora
```

### ubuntu 安装 microsoft-edge

[Edge Insider Channels](https://www.microsoftedgeinsider.com/en-us/download)

如果你是一个 Linux 爱好者, 你可能想自己设置我们的PPA.
下面我们提供了命令行说明, 可以粘贴到终端窗口来安装 `签名密钥`(signing key), `sources.lst` 文件, 以及安装 edge.

```bash
## 设置密钥
curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
sudo install -o root -g root -m 644 microsoft.gpg /etc/apt/trusted.gpg.d/
sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/edge stable main" > /etc/apt/sources.list.d/microsoft-edge-beta.list'
sudo rm microsoft.gpg
## 安装
sudo apt update
sudo apt install microsoft-edge-beta
```

## Ubuntu 镜像使用帮助

清华大学的源

域名选择

```bash
https://mirrors.tuna.tsinghua.edu.cn 自动选择
https://mirrors6.tuna.tsinghua.edu.cn 只解析 IPv6
https://mirrors4.tuna.tsinghua.edu.cn 只解析 IPv4
```

Ubuntu 的软件源配置文件是 `/etc/apt/sources.list`.将系统自带的该文件做个备份,将该文件替换为下面内容,即可使用 `TUNA` 的软件源镜像.

```bash
# 默认注释了源码镜像以提高 apt update 速度,如有需要可自行取消注释
deb https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic main restricted universe multiverse
# deb-src https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic main restricted universe multiverse
deb https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-updates main restricted universe multiverse
# deb-src https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-updates main restricted universe multiverse
deb https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-backports main restricted universe multiverse
# deb-src https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-backports main restricted universe multiverse
deb https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-security main restricted universe multiverse
# deb-src https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-security main restricted universe multiverse

# 预发布软件源,不建议启用
# deb https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-proposed main restricted universe multiverse
# deb-src https://mirrors6.tuna.tsinghua.edu.cn/ubuntu/ bionic-proposed main restricted universe multiverse
```

## dpkg创建包,dpkg-buildpackage

`dget` -- Download Debian source and binary packages

SYNOPSIS

```bash
dget [options] URL ...
dget [options] [--all] package[=version] ...
```

DESCRIPTION

dget downloads Debian packages.

In the first form, dget fetches the requested `URLs`.  If this is a `.dsc` or `.changes` file,
then dget acts as a source-package aware form of wget: it also fetches any files referenced in the `.dsc/.changes` file.
The downloaded source is then checked with `dscverify` and, if successful, unpacked by `dpkg-source`.

### 完整的(重)构建

为保证完整的软件包(重)构建能顺利进行,你必须保证系统中已经安装

    build-essential 软件包;

    列于 Build-Depends 域的软件包(参看 第 4.1 节 `control`);

    列于 Build-Depends-indep 域的软件包(参看 第 4.1 节 `control`).

然后在源代码目录中执行以下命令:

```bash
$ dpkg-buildpackage -us -uc
```

这样会自动完成所有从源代码包构建二进制包的工作,包括:

    清理源代码树(debian/rules clean)

    构建源代码包(dpkg-source -b)

    构建程序(debian/rules build)

    构建二进制包(fakeroot debian/rules binary)

    制作 .dsc 文件

    用 dpkg-genchanges 命令制作 .changes 文件.

如果构建结果令人满意,那就用 debsign 命令以你的私有 GPG 密钥签署 .dsc 文件和 .changes 文件.你需要输入密码两次. [63]

对于非本地 Debian 软件包,比如 gentoo, 构建软件包之后,你将会在上一级目录(~/gentoo) 中看到下列文件:

    gentoo_0.9.12.orig.tar.gz

    这是原始的源代码 tarball,最初由 dh_make -f ../gentoo-0.9.12.tar.gz 命令创建,它的内容与上游 tarball 相同,仅被重命名以符合 Debian 的标准.

    gentoo_0.9.12-1.dsc

    这是一个从 control 文件生成的源代码概要,可被 dpkg-source(1) 程序解包.

    gentoo_0.9.12-1.debian.tar.gz

    这个压缩的 Tar 归档包含你的 debian 目录内容.其他所有对于源代码的修改都由 quilt 补丁存储于 debian/patches 中.

    如果其他人想要重新构建你的软件包,他们可以使用以上三个文件很容易地完成.只需复制三个文件,再运行 dpkg-source -x gentoo_0.9.12-1.dsc. [64]

    gentoo_0.9.12-1_i386.deb

    这是你的二进制包,可以使用 dpkg 程序安装或卸载它,就像其他软件包一样.

    gentoo_0.9.12-1_i386.changes

    这个文件描述了当前修订版本软件包中的全部变更,它被 Debian FTP 仓库维护程序用于安装二进制和源代码包.它是部分从 changelog 和 .dsc 文件生成的.

    随着你不断完善这个软件包,程序的行为会发生变化,也会有更多新特性添加进来.下载你软件包的人可以查看这个文件来快速找到有哪些变化,Debian 仓库维护程序还会把它的内容发表至 debian-devel-changes@lists.debian.org 邮件列表.

在上传到 Debian FTP 仓库中前,gentoo_0.9.12-1.dsc 文件和 gentoo_0.9.12-1_i386.changes 文件必须用 debsign 命令签署,其中使用你自己存放在 ~/.gnupg/ 目录中的 GPG 私钥. 用你的公钥,可以令 GPG 签名证明这些文件真的是你的.

debsign 命令可以用来以指定 ID 的 GPG 密钥进行签署 (这方便了赞助(sponsor)软件包), 只要照着下边在 ~/.devscripts 中的内容:

DEBSIGN_KEYID=Your_GPG_keyID

.dsc 和 .changes 文件中很长的数字串是其中提及文件的 SHA1/SHA256 校验和.下载你软件包的人可以使用 sha1sum(1) 或 sha256sum(1) 来进行核对.如果校验和不符,则说明文件已被损坏或偷换
