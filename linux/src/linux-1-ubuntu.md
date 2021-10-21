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
