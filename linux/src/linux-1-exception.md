# linux 异常处理

## texlive安装与卸载

[Linux环境下LaTex的安装与卸载](https://blog.csdn.net/l2563898960/article/details/86774599)
[Ubuntu Texlive 2019 安装与环境配置](https://blog.csdn.net/williamyi96/java/article/details/90732304)
[TexLive 2019 安装指南](https://zhuanlan.zhihu.com/p/64530166)
[TeX Live - Quick install](https://tug.org/texlive/quickinstall.html)

### 准备工作:下载,清除

注意:安装 `lyx`, `apt` 会默认安装 `tex2017`版本,覆盖掉新版的`texlive2020`

注意:如果重新安装,请务必完全删除之前的失败安装,默认情况下,这将在这两个目录中:

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

***
卸载镜像文件

```bash
sudo umount /mnt
```

***
字体配置

```bash
sudo cp /home/tom/texlive/2020/texmf-var/fonts/conf/texlive-fontconfig.conf /etc/fonts/conf.d/20-texlive.conf
sudo fc-cache -fsv
```

***
环境变量

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

***
验证安装是否成功

```bash
tex -v
```

***
设置默认纸张尺寸

`tlmgr paper letter`

***
ubuntu 仓库的texlive

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

***
下面这些是`tlmgr`的常用命令:

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

## jabref

[JabRef中文手册](https://blog.csdn.net/zd0303/article/details/7676807)

entry 时间戳

本功能可以在`选项->偏好设置->通用设置`中关闭或配置. `JabRef`能自动的产生一个包含题录加入数据库的日期的域.

格式:

时间戳记的格式由包含指示词的字符串确定,指示词表示日期各部分的位置.
以下是一些可用的指示字母(示例在括号中给出,为:  2005年9月14日(星期三)下午5.45):

+ yy: year (05)
+ yyyy: year (2005)
+ MM: month (09)
+ dd: day in month (14)
+ HH: hour in day (17)
+ mm: minute in hour (45)

这些指示符可以与标点符号和空格一起使用. 几个例子:

+ `yyyy.MM.dd gives 2005.09.14`
+ `yy.MM.dd gives 05.09.14`
+ `yyyy.MM.dd HH:mm gives 2005.09.14 17:45`

## lyx 报错

***
如果桌面环境使用`Gnome`默认, 也就是`Wayland`协议, 默认`ibus`输入法在`lyx`下无法使用. 报错为

```bash
Warning: Ignoring XDG_SESSION_TYPE=wayland on Gnome. Use QT_QPA_PLATFORM=wayland to run on Wayland anyway.
```

按照[SDB:在 Wayland 中启用输入法](https://zh.opensuse.org/SDB:%E5%9C%A8_Wayland_%E4%B8%AD%E5%90%AF%E7%94%A8%E8%BE%93%E5%85%A5%E6%B3%95)
操作仍然不行.

进入 `KDE` 或 `GNOME` 的 `Wayland` 会话之后, 您可能会发现输入法(Fcitx 或 iBus)无法使用.
最新的稳定版 `Fcitx` 和 `iBus` 都已经了基本的 `Wayland` 支持, 通过 X 的协议转接实现.
`Wayland` 读取的环境配置文件是`/etc/environment` 而不是 `X` 所读取的环境变量文件. 因此对 `X` 有效的输入法配置在 `Wayland` 上不起效果了. 以管理员权限编辑它:

```bash
sudo vi /etc/environment
```

这个文件应该是空的, 只有几行注释. 添加下面几行, 以 `Fcitx` 为例:

```bash
INPUT_METHOD=fcitx
GTK_IM_MODULE=fcitx
QT_IM_MODULE=fcitx
XMODIFIERS=@im=fcitx
```

如果您使用 iBus 的话, 那么应该添加这几行:

```bash
INPUT_METHOD=ibus
GTK_IM_MODULE=ibus
QT_IM_MODULE=ibus
XMODIFIERS=@im=ibus
```

之后请重启您的系统.

***
有时安装好了texlive,也安装好了`lyx`,却仍然报错,这个时候一般是因为路径(`$PATH`)没有配置好,
lyx没有检测到texlive的各种文件.参考 [LYX Manuals](https://wiki.lyx.org/uploads//LyX/Manuals/1.6.4//Manuals.pdf)

`LYX`的一些功能可以从`LYX`内部进行配置,而无需重新配置文件.
首先,`LYX`能够检查您的系统,以查看可以使用哪些程序,`LATEX`文档类和`LATEX`软件包.
它使用此知识为多个`Preferences`设置提供合理的默认值.
尽管在系统上安装`LYX`时已经完成了此配置,但是您可能需要在本地安装一些项目,
新的`LATEX`类,而`LYX`看不到这种变化.
要强制LYX重新检查系统,您应该使用`Tools,Reconfigure`. 然后,您应该重新启动`LYX`以确保考虑到更改.

添加`tex`文件的路径到`$PATH`中的时候,注意尽量把新的`tex`路径添加到`$PATH`前面,
以防止之前安装的残留掩盖新的路径.也就是,

```bash
if [[ $SHELL == "/bin/zsh" ]] ;then
echo "\n# add texlive paths" >> ~/.zshrc
echo "export MANPATH=your_texlive_path/texmf-dist/doc/man:${MANPATH}" >> ~/.zshrc
echo "export INFOPATH=your_texlive_path/texmf-dist/doc/info:${INFOPATH}" >> ~/.zshrc
echo "export PATH=your_texlive_path/bin/x86_64-linux:${PATH}" >> ~/.zshrc
fi
```

直接使用`apt`仓库安装的`texlive`套装和`lyx`一般没有问题.

## lyx 默认pdf查看

在`tools-preferences-File Handling-File Formats`

在 `Format` 一栏中选中`PDF(XeTex)`  或者其他想要更改的格式,然后在 `Viewer`中更改程序,或者自定义程序位置.

## latexmk 选项

一般来说, `latexmk` 的通用`cmd`命令形式为:

`latexmk [options] [file]`

所有的选项可以用单个`-`连字符,也可以用双连字符`--`引入,e.g., "latexmk -help" or "latexmk --help".

***
注意:

除了文档里列出的选项, `latexmk`认识几乎所有the options recognized by the latex, pdflatex programs (and their relatives),
在当前的 TexLive and MikTeX 发行版中.

这些程序的一些选项还会引起 latexmk 的特殊 action or behavior,在本文档中有解释.否则,它们被直接传递给latex or pdflatex.
run `latexmk -showextraoptions`给出选项列表,这些选项被直接传递给latex or pdflatex.

***
注意:

"Further processing" 意味着需要运行其他程序,或者再次运行`latex`(etc),如果没有 `errors` 的话.
如果你不想让`latex`在遇到错误的时候停下,应该使用 latexmk's option `-interaction=nonstopmode`

`-xelatex`  使用`xelatex`编译
`-pv `   - preview document.  (Side effect turn off continuous preview)
` -pv-`   - turn off preview mode
`-pvc`   - preview document and continuously update.  (This also turns  on force mode, so errors do not cause latexmk to stop.)
(Side effect: turn off ordinary preview mode.)
`-pvc-`  - turn off -pvc

`-view=default` - viewer is default (dvi, ps, pdf)
`-view=ps`      - viewer is for ps
`-view=pdf`     - viewer is for pdf

`-bibtex`       - use bibtex when needed (default)
`-bibtex-`      - never use bibtex

`-cd`    - Change to directory of source file when processing it

`-recorder` - Use -recorder option for (pdf)latex (to give list of input and output files)
` -recorder-` - Do not use -recorder option for (pdf)latex

***
简单传递的命令

`-error-line=n` set the width of context lines on terminal error messages
`-half-error-line=n`      set the width of first lines of contexts in terminal error messages

`-file-line-error `       enable `file:line:error` style messages
`-halt-on-error`          stop processing at the first error
`-interaction=STRING`     set interaction mode (STRING=batchmode/nonstopmode/scrollmode/errorstopmode)
`-synctex=NUMBER`         generate `SyncTeX` data for previewers if nonzero

## 安装latex包

[Ubuntu/Mint下LaTeX宏包安装及更新](https://blog.csdn.net/codeforces_sphinx/article/details/7315044)

一般使用texlive的包管理工具,否则需要手动安装:

1. Get the package from [CTAN](http://www.ctan.org/CTAN) or wherever.
2. 如果其中有一个文件是`.ins` 结尾的,打开终端,执行命令`latex foiltex.ins`,就获得了安装需要的包.大多数 latex 包没有打包,所以可以跳过这一步.
3. 现在你需要决定,这个包要安装给所有用户使用,还是only for you.
4. 在*nix 系统上(OSX),给所有用户使用,安装到`local` TeX tree, 给自己使用,安装到`user`TeX tree.

查看`texmf.cnf`文件,它通常在`$TEXMF/web2c`文件夹,但是可以用`kpsewhich texmf.cnf`定位.

`local` Tree 的位置在 `TEXMFLOCAL` variable 中定义,通常像是`/usr/local/share/texmf`.
`user`  Tree 的位置在`TEXMFHOME`中定义,通常像是`$HOME/texmf` or `$HOME/.texliveXXXX`

如果这些变量没有定义,你需要手工指定.修改`local` Tree 可能需要 root 权限.建议修改 user tree, 因为在升级的时候,不会被覆盖.这样在备份系统的时候,可以一起备份.

现在,你需要告诉 Latex 有新文件.这取决于 LaTex 发行版.

1. 对于 TeXLive,运行`texhash`,可能需要 root 权限
2. 对于MikTeX,运行 `Settings (Admin)` and press the button marked `Refresh FNDB`

5. 最后,你需要告诉 LyX 有新的包可以使用.在LyX 中,运行 `Tools->Reconfigure` and then restart LyX

现在,新的文档 class 可以选择了,`Document->Settings->Document Class`.

## latex包安装方式2

首先要找到默认宏包所在目录,一般是:

```bash
/usr/share/texmf/tex/latex
/usr/share/texmf-texlive/tex/latex
```

1. 如果是安装一个新的宏包,就直接把宏包的压缩文件扔进第一个目录下,直接解压就行,注意解压后的文件里可能有安装说明,照着安装说明做就是了.
如果是更新一个宏包,一般都可以在第二个目录下找到,把原先的宏包重命名成`*-backup`,再解压新下载的宏包压缩文件,同时如果有安装说明的话,也照着做.
2. 之后要对宏包重新标记下,终端下执行

```bash
# texhash
```

`Log off/Log in`后,就完成了~

## latex pdf 裁剪

`texlive` 自带了一个叫做 `pdfcrop` 的 `perl` 脚本

使用方法如下:

`pdfcrop --margins 3 --clip input.pdf output.pdf; ` 或者

```bash
pdfcrop --clip --bbox '120 480 570 830' input.pdf output.pdf;
pdfcrop --clip --bbox '60 660 516 775' moban.pdf moban_crop.pdf && evince moban_crop.pdf  # 国科大试卷的裁减参数
```

四个数字的含义是, 以左下角为原点, 给出 `left bottom right top` 的数值(左下--右上), 单位是打印点 -- `point`:

    1 point = 0.3527 mm = 1/72 inch
    A4纸张 210mm * 297mm = 595.4 point * 842.1 point

## loop 设备

loop 设备 (循环设备)

[loop 设备 (循环设备)](https://blog.csdn.net/neiloid/article/details/8150629)

## loop 设备介绍

在类 UNIX 系统里,`loop` 设备是一种伪设备(pseudo-device),或者也可以说是仿真设备.它能使我们像块设备一样访问一个文件.

在使用之前,一个 `loop` 设备必须要和一个文件进行连接.这种结合方式给用户提供了一个替代块特殊文件的接口.因此,如果这个文件包含有一个完整的文件系统,那么这个文件就可以像一个磁盘设备一样被 `mount` 起来.

上面说的文件格式,我们经常见到的是 CD 或 DVD 的 ISO 光盘镜像文件或者是软盘(硬盘)的 `*.img` 镜像文件.通过这种 `loop mount` (回环`mount`)的方式,这些镜像文件就可以被 `mount` 到当前文件系统的一个目录下.

至此,顺便可以再理解一下 `loop` 的含义:对于第一层文件系统,它直接安装在我们计算机的物理设备之上;
而对于这种被 `mount` 起来的镜像文件(它也包含有文件系统),它是建立在第一层文件系统之上,
这样看来,它就像是在第一层文件系统之上再绕了一圈的文件系统,所以称为 `loop`.

在 Linux 里,`loop` 设备的设备名形如:

```bash
ls /dev/loop*
/dev/loop0  /dev/loop2  /dev/loop4  /dev/loop6
/dev/loop1  /dev/loop3  /dev/loop5  /dev/loop7
... ...
```

例如,要在一个目录下 mount 一个包含有磁盘镜像的文件,需要分 2 步走:

```bash
losetup /dev/loop0 disk.img           #使磁盘镜像文件与循环设备连结起来
mount /dev/loop0 /home/groad/disk_test   #将循环设备 mount 到目录 disk_test 下
```

经过上面的两个命令后,镜像文件就如同一个文件系统挂载在 `disk_test` 目录下,当然我们也可以往镜像里面添加文件.

其实上面的两个步骤可以写成一个步骤:

```bash
mount -t minix -o loop ./disk.img ./disk_test
```

## snap

[Ubuntu使用snap安装常用软件](https://www.jianshu.com/p/4049b97151a1)

什么是`snap`, `snap`是一种全新的软件包管理方式, 它类似一个容器拥有一个应用程序所有的文件和库, 各个应用程序之间完全独立.
所以使用`snap`包的好处就是它解决了应用程序之间的依赖问题, 使应用程序之间更容易管理. 但是由此带来的问题就是它占用更多的磁盘空间.

`Snap`的安装包扩展名是`.snap`, 类似于一个容器, 它包含一个应用程序需要用到的所有文件和库(`snap`包包含一个私有的`root`文件系统, 里面包含了依赖的软件包).
它们会被安装到单独的目录; 各个应用程序之间相互隔离. 使用`snap`有很多好处, 首先它解决了软件包的依赖问题; 其次, 也使应用程序更容易管理.

现在支持`snap`的应用并不多, `snap`软件包一般安装在`/snap`目录下.

## 开机报错

[System program problem detected?](https://askubuntu.com/questions/1160113/system-program-problem-detected)

查看转储到您的磁盘上的崩溃报告. 目录是`/var/crash/`, 它将包含几个文件, 这些文件将您指向它所涉及的软件包以及崩溃的原因.
该目录描述为:

>`/var/crash`: 系统崩溃转储(可选)
>该目录包含系统故障转储.
>自本标准发布之日起, Linux不支持系统故障转储, 但其他可能符合FHS的系统也可能支持系统转储.

`Ubuntu`版本使用此(可选)目录来转储崩溃和执行崩溃的软件包, 称为`apport` (and `whoopsie`).
如果您想获得关于崩溃的真正详细的报告, 请安装`GDB`: `The GNU Project Debugger` with `sudo apt-get install gdb`.

+ 如何摆脱它

取决于您所说的`摆脱`. 理想的解决方法是检查报告中包含的内容, 然后尝试找到解决方法.
如果不需要包装或良性包装, 也可以将其清除. 多数情况下, 它是一项核心功能.

您可以选择以下任意一种来删除崩溃报告, 直到实际删除该软件包为止(如果错误来自于`apport`本身, 那将非常具有讽刺意味):

+ `sudo rm /var/crash/*`将删除旧的崩溃并停止通知您, 直到某些软件包再次崩溃为止.
+ 您可以通过`sudo systemctl disable apport`停止服务(并通过`sudo systemctl enable apport`再次启用它)
+ 如果不想看到崩溃报告, 可以通过`vim /etc/default/apport`将其禁用. 并将`enabled = 1`更改为` enabled = 0`. 反向编辑将再次启用它.
+ 您可以使用`sudo apt purge apport`(使用`sudo apt install apport`再次安装)
+还有一种桌面方法(`问题报告`选项):

[如何阅读和使用崩溃报告](https://askubuntu.com/questions/346953/how-to-read-and-use-crash-reports)有一些有趣的答案.
它有一个示例崩溃报告和一种跟踪崩溃的方法.
