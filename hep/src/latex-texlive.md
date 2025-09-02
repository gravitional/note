# texlive

## texlive安装与卸载

[Linux环境下LaTex的安装与卸载](https://blog.csdn.net/l2563898960/article/details/86774599)
[Ubuntu Texlive 2019 安装与环境配置](https://blog.csdn.net/williamyi96/java/article/details/90732304)
[TexLive 2019 安装指南](https://zhuanlan.zhihu.com/p/64530166)
[TeX Live - Quick install](https://tug.org/texlive/quickinstall.html)

### 准备工作:下载,清除

注意:安装 `lyx`, `apt` 会默认安装 `tex2017`版本,覆盖掉新版的`texlive2020`
注意:如果重新安装,请务必完全删除之前的失败安装,默认情况下,这将在这两个目录中:

#### windows下使用卸载脚本

[Texlive2023与Texstudio2023卸载与安装](https://blog.csdn.net/m0_37738114/article/details/132598628)

texlive 自带卸载脚本, 位置在例如

```bash
C:\texlive\2021\tlpkg\installer\uninst.bat
```

#### 手动删除

```bash
rm -rf /usr/local/texlive/2020
rm -rf ~/.texlive2020
```

或者参考下面的命令

```bash
sudo rm -rf /usr/local/texlive/2020
rm -rf ~/.texlive2020
sudo rm -rf /usr/local/texlive
sudo rm -rf /usr/local/share/texmf
sudo rm -rf /var/lib/texmf
sudo rm -rf /etc/texmf
sudo apt-get purge texlive*
sudo apt-get remove tex-common --purge
```

### 进行安装

因为下载好的是一个`iso`镜像文件,所以下载好之后,还需要挂载到`/mnt`目录下

```bash
sudo mount -o ro,loop,noauto texlive2020-20200406.iso /mnt
```

+ `ro` :     Mount the filesystem read-only.
+ `loop` : loop 文件
+ `auto` :   Can be mounted with the -a option.
+ `noauto` : Can only be mounted explicitly (i.e., the  -a  option  will  not cause the filesystem to be mounted).

接着运行`install-tl`脚本进行安装.

若要更改安装目录或其他选项,请阅读提示和说明.
一般需要更改路径到自己有读写权限的文件夹下面,按`D`,然后按`1`,输入比如`~/texlive/2020`

更改目录到

+ `TEXDIR:         /home/tome/texlive/2020`
+ `main tree:      /home/tome/texlive/2020/texmf-dist`

+ `TEXMFLOCAL:     /home/tome/texlive/texmf-local`
+ `TEXMFSYSVAR:    /home/tome/texlive/2020/texmf-var`
+ `TEXMFSYSCONFIG: /home/tome/texlive/2020/texmf-config`
+ `TEXMFVAR:       ~/.texlive2020/texmf-var`
+ `TEXMFCONFIG:    ~/.texlive2020/texmf-config`
+ `TEXMFHOME:      ~/texmf`

```bash
cd /tex_iso_directory
sudo ./install-tl --profile installation.profile
[... messages omitted ...]
Enter command: i
[... when done, see below for post-install ...]
```

安装程序的接口:文本,GUI,批处理
安装程序支持:文本,图形,和批处理接口.(Linux系统下没有图像安装,在Windows下支持图形安装)

`install-tl -gui text #`使用简单文本模式.也是输入`install-tl`默认选项.

`install-tl --profile=profile #`进行一个批处理安装,需要一个 `profile` (配置文件),为了创建一个`profile`,最简单的方式是使用`tlpkg/texlive.profile`文件,这是安装器在安装成功后生成的文件.

#### 卸载镜像文件

```bash
sudo umount /mnt
```

#### 字体配置

```bash
sudo cp /home/tom/texlive/2020/texmf-var/fonts/conf/texlive-fontconfig.conf /etc/fonts/conf.d/20-texlive.conf
sudo fc-cache -fsv
```

#### 环境变量

安装完之后有提示:

```bash
Add /home/tom/texlive/2020/texmf-dist/doc/man to MANPATH.
Add /home/tom/texlive/2020/texmf-dist/doc/info to INFOPATH.
Most importantly, add /home/tom/texlive/2020/bin/x86_64-linux
to your PATH for current and future sessions.
```

我用的是`zsh`,如果用的是`bash`则修改`~/.bashrc`,其中的`/home/tom/texlive/2020`改称你安装时的路径
直接把下面的语句添加到`.zshrc`文件末尾.

```bash
export MANPATH=${MANPATH}:/home/tom/texlive/2020/texmf-dist/doc/man
export INFOPATH=${INFOPATH}:/home/tom/texlive/2020/texmf-dist/doc/info
export PATH=${PATH}:/home/tom/texlive/2020/bin/x86_64-linux
```

#### 验证安装是否成功

```bash
tex -v
```

#### 设置默认纸张尺寸

`tlmgr paper letter`

#### ubuntu 仓库的texlive

使用`apt`命令从`ubuntu`仓库安装的`texlive`可以使用`dpkg -L texlive-full`查询

安装在 `/usr/local/`目录下,
`texmf`(TDS的根目录)在`/usr/share/texmf` and `/usr/share/texlive/texmf-dist`

## texlive常用命令

用`texlive**.iso`手动安装的 texlive 是可以正常使用下面这些命令的,而用 `debian`源`apt`安装的,可能会出问题.

`tlmgr [option]... action [option]... [operand]...`

安装好 `texlive` 后

如果使用`tlmgr option` 报错
`cannot setup TLPDB in /home/USER/texmf at /usr/bin/tlmgr line 5308.`

原因如下:

未初始化`tlmgr`时会产生此错误. 在大多数情况下,以普通用户身份启动以下命令可以解决此问题:

`$ tlmgr init-usertree`

此命令将在您的家目录内创建几个文件夹. 请参见手册页以获取解释:

>在用户模式下使用`tlmgr`之前,您必须使用`init-usertree`操作设置用户树.
>这将创建`usertree / web2c`和`usertree / tlpkg / tlpobj`,以及最小的`usertree / tlpkg / texlive.tlpdb`.
>此时,您可以通过添加`--usermode`命令行选项来告诉`tlmgr`执行(支持的)动作.

### `tlmgr`的常用命令

+ `tlmgr option repository ctan`
+ `tlmgr option repository http://mirror.ctan.org/systems/texlive/tlnet`
+ `tlmgr repository list`
+ `tlmgr update --self`
+ `tlmgr update  --all`

如果要使用清华的`mirror`:

`tlmgr option repository https://mirrors.tuna.tsinghua.edu.cn/CTAN/systems/texlive/tlnet`

[texlive home page](https://tug.org/texlive/)
[texlive installation and updates](https://tug.org/texlive/pkginstall.html) texlive 安装和更新
[archive of tlnet ](https://www.texlive.info/tlnet-archive/) : 各个年份的 tex 更新, 可以选择用来更新的 repository 的版本
[texlive.info](https://texlive.info/) 查看各种关于 texlive 的信息

告诉`tlmgr`使用附近的CTAN镜像进行将来的更新;  如果您从DVD映像安装了TeX Live,并且想要持续更新,则很有用.
这两个命令是等效的. `ctan`只是给定URL的别名.
注意: `mirror.ctan.org`解析为许多不同的主机,它们并没有完全同步. 我们建议仅(最多)每天更新一次,而不要更频繁.

+ `tlmgr update --list` 报告将要更新的内容,而无需实际更新任何内容.
+ `tlmgr update --all` 使本地TeX安装与软件包存储库中的安装相对应(从CTAN更新时通常很有用).
+ `tlmgr info pkg` 显示有关软件包内容的详细信息,例如搜索所有软件包中内容的安装状态和描述.

可能遇到的错误:

[tlmgr: unexpected return value from verify_checksum: -5](https://tex.stackexchange.com/questions/528634/tlmgr-unexpected-return-value-from-verify-checksum-5)

出现这个错误是由于某个`repository`的`signing key`过期了,
首先可以使用`tlmgr repository list`列出所有的库, 使用`tlmgr key list`列出所有的`keys`

首先把`repository`更换到对应`debian`发行版的仓库, 比如使用 `2019` 版本的 `repository` ,
`tlmgr option repository https://www.texlive.info/tlnet-archive/2019/12/31/tlnet/`

然后把[tug](https://www.tug.org/texlive/)的 `GPG` key 加入到 `tlmgr` 的key 列表中

```bash
curl -fsSL https://www.tug.org/texlive/files/texlive.asc | tlmgr key add -
```

这样就不会出现`erify_checksum: -5`错误了.

总结:

+ `tlmgr key list`列出所有的`key`
+ `tlmgr repository list`列出使用的仓库
+ `curl -fsSL https://www.preining.info/rsa.asc | tlmgr key add -`为`contrib`仓库添加新的gpg key
+ `tlmgr install  --verify-repo=none pkg` 免去验证

curl -fsSL https://www.preining.info/rsa.asc | tlmgr key add -

## tlmgr 命令

***
`install [option]... pkg...`

如果尚未安装,请安装命令行上给出的每个`pkg`.
(它不涉及现有软件包; 有关如何获取软件包的最新版本,请参见`更新`操作.)

默认情况下,这还会安装给定pkg所依赖的所有软件包. 选项:

+ `--dry-run` : 实际没有安装任何东西. 而是将要执行的动作写入终端.
+ `--file`: 不从安装库中获取软件包, 使用命令行上给出的软件包文件. 这些文件必须是标准的`TeX Live`软件包文件(包含`tlpobj`文件).
+ `--force`:如果存在对`tlmgr`本身(或基本基础结构的其他部分)的更新,
则除非给出此选项,否则`tlmgr`将退出紧急状态并且不会执行安装. 不建议.
+ `--no-depends`:不要安装依赖项. (默认情况下,安装软件包可确保满足该软件包的所有依赖关系.)
+ `--no-depends-at-all`:通常,当您安装附带二进制文件的软件包时,还将安装相应的二进制软件包.
也就是说,对于软件包`foo`,软件包`foo.i386-linux`也将安装在`i386-linux`系统上.
此选项抑制了这种行为,并且还暗示了`--no-depends`.
除非您确定自己在做什么,否则不要使用它.
+ `--reinstall`:即使似乎已经安装了软件包(即TLPDB中已存在),也要重新安装软件包(包括集合的依赖项).
这对于从意外删除层次结构中的文件中恢复非常有用.

***

+ `conf [texmf|tlmgr|updmap [--conffile file] [--delete] [key [value]]]`
+ `conf auxtrees [--conffile file] [show|add|delete] [value]`

仅使用`conf`,即可显示TeX Live的常规配置信息,包括活动配置文件,路径设置等.
这就像运行`texconfig conf`一样,但是可以在所有支持的平台上运行.

使用`conf texmf`,`conf tlmgr`或`conf updmap`之一显示`ROOT / texmf.cnf`(用户特定的`tlmgr`配置)中保存的所有键/值对(即所有设置) 文件(请参见下文)或第一个(通过`kpsewhich`找到的)`updmap.cfg`文件.

`conf`显示的`PATH`值与`tlmgr`使用的值相同. 包含`tlmgr`可执行文件的目录会自动添加到从环境继承的PATH值之前.

这是更改配置值的实际示例. 如果在安装过程中启用了通过`\ write18`执行的(部分或全部)系统命令,则可以在以后将其禁用:

```bash
tlmgr conf texmf shell_escape 0
```

子命令`auxtrees`允许完全在用户控制下添加和删除任意其他texmf树.
`auxtrees show`显示其他树的列表,`auxtrees add`树将树添加到列表中,`auxtrees remove`树从列表中删除树(如果存在).

树中不应包含`ls-R`文件(否则,如果`ls-R`过时,则可能找不到文件).
通过操作`ROOT / texmf.cnf`中的Kpathsea变量`TEXMFAUXTREES`来生效. 例:

```bash
tlmgr conf auxtrees add /quick/test/tree
tlmgr conf auxtrees remove /quick/test/tree
```

在所有情况下,如果需要,都可以通过选项`--conffile`文件显式指定配置文件.

警告: 此处是用于更改配置值的一般工具,但是强烈建议不要以这种方式修改设置.
同样,不对键或值进行错误检查,因此可能发生任何破损.

## 手动安装宏包

如果无法使用`tlmgr`自动安装宏包, 例如`ubuntu`自带的`texlive`, 默认的版本比远程仓库中的低, 无法自动升级.
可以直接从网上下载想要安装的宏包, 大部分宏包已经打包成标准格式, 例如`siunitx.tds.zip`, [siunitx](https://www.ctan.org/pkg/siunitx).
直接解压到`texlive`的安装目录即可.

如何定位安装目录呢? 可以参考[Installing TeX fonts](http://www.tug.org/fonts/fontinstall.html), 虽然这个文章主要是介绍安装 `字体` 的.
使用类似下面的命令找出 `texlive` 的安装目录.

```bash
kpsewhich --var-value TEXMF
tlmgr conf # 这个会输出texlive大部分配置的信息
```

我电脑上`texlive`安装在`/usr/share/texmf `, 这是共享目录, 如果安装在这里, 所有账户都能使用.
此外在家目录下, 即`/home/tom/texmf`还有一个用户目录树, 建议把宏包解压到这里, 不会影响`ubuntu`自带的发行版.
如果你的`home`没有这个文件夹, 可以运行`tlmgr init-usertree`产生一个.
由于路径中`/home/tom/texmf`在前面, 把宏包装在这里, 就会被优先使用.

## linux `./install-tl` 给出的信息

在 linux 上, 解压 texlive2025-20250308.iso,
使用 `sudo ./install-tl`, 打印出如下安装目录信息.
输入 `i`, 开始安装

```bash
 Detected platform: GNU/Linux on x86_64

 <B> set binary platforms: 1 out of 15

 <S> set installation scheme: scheme-full

 <C> set installation collections:
     40 collections out of 41, disk space required: 8779 MB (free: 820631 MB)

 <D> set directories:
   TEXDIR (the main TeX directory):
     /usr/local/texlive/2025
   TEXMFLOCAL (directory for site-wide local files):
     /usr/local/texlive/texmf-local
   TEXMFSYSVAR (directory for variable and automatically generated data):
     /usr/local/texlive/2025/texmf-var
   TEXMFSYSCONFIG (directory for local config):
     /usr/local/texlive/2025/texmf-config
   TEXMFVAR (personal directory for variable and automatically generated data):
     ~/.texlive2025/texmf-var
   TEXMFCONFIG (personal directory for local config):
     ~/.texlive2025/texmf-config
   TEXMFHOME (directory for user-specific files):
     ~/texmf

 <O> options:
   [ ] use letter size instead of A4 by default
   [X] allow execution of restricted list of programs via \write18
   [X] create all format files
   [X] install macro/font doc tree
   [X] install macro/font source tree
   [ ] create symlinks to standard directories
   [X] after install, set CTAN as source for package updates

 <V> set up for portable installation

Actions:
 <I> start installation to hard disk
 <P> save installation profile to 'texlive.profile' and exit
 <Q> quit

Enter command:
``