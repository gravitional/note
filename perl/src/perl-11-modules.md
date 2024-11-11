# [per module](https://www.cnblogs.com/f-ck-need-u/p/9685581.html)

## Perl模块管理

perl有自带的模块, 还有第三方模块.
自带的模块是随perl安装而安装好的, 第三方模块需要从CPAN(Comprehensive Perl Archive Network)上下载并安装,
可以从 [metacpan](https://metacpan.org) 中查找所需第三方模块.

可以借助 `perldoc 'MODULE_NAME'` 来间接查询一个模块是否已安装,
如果安装了就能正常输出对应的文档, 如果没有安装, 则报错.

如果想要查看所有已安装的 module, 则使用 `cpan -a`,
不过第一次使用它会要求你先进行一番配置, 不过一般来说,
选择让它自动配置即可.

例如, 筛选出已安装的包含CGI字符的模块.
第一个字段是字段名, 第二个字段是已安装版本号,
第三个字段是可获取的最新版本号,
第四个字段是在CPAN中的位置, 这个位置, 稍后解释.

```bash
[root@redisa-b ~]# cpan -a | grep CGI
CGI             3.63  4.40  LEEJO/CGI-4.40.tar.gz
CGI::Fast       1.09  2.13  LEEJO/CGI-Fast-2.13.tar.gz
FCGI            0.74  0.78  ETHER/FCGI-0.78.tar.gz
CGI::Carp       3.51  4.40  LEEJO/CGI-4.40.tar.gz
CGI::Cookie     1.30  4.40  LEEJO/CGI-4.40.tar.gz
CGI::Pretty     3.46  4.40  LEEJO/CGI-4.40.tar.gz
CGI::Push       1.05  4.40  LEEJO/CGI-4.40.tar.gz
CGI::Util       3.62  4.40  LEEJO/CGI-4.40.tar.gz
```

全面了解推荐内容:

man CPAN
http://learnperl.scratchcomputing.com/tutorials/configuration/

## 模块必知常识

模块实际上都是perl脚本.
在perl文化中, 非常注重模块的开发人员以及维护人员.
在每个模块相关的元数据属性中, 都会记录他们的信息.

以下是一个模块基本具备的信息:

```perl
Module id = File::Utils
    CPAN_USERID  PEKINGSAM (Yan Xueqing <yanxueqing10@163.com>)
    CPAN_VERSION 0.000005
    CPAN_FILE    P/PE/PEKINGSAM/File-Utils-0.0.5.tar.gz
    UPLOAD_DATE  2016-05-11
    MANPAGE      File::Utils - Provide various of file operation
    INST_FILE    /usr/local/share/perl5/File/Utils.pm
    INST_VERSION 0.0.5
```

Module ID: 模块ID, 也就是模块的名称, 即File::Utils
CPAN_USERID: 上传这个模块的人在CPAN中的ID, 后面是它的注册名和邮箱. 从邮箱和名称来看, 显然这是个中国人
CPAN_VERSION: 这个模块的版本号
CPAN_FILE: 这个模块的完整ID, 这个ID称为Distribution id. 这个ID由4部分组成:
第3部分是这个模块的上传者ID
第1部分是这个上传者ID的第一个字母
第2部分是上传者ID的前两个字母 ,
第4部分是模块的源码包名称
UPLOAD_DATA: 模块的上传时间
MANPAGE: man文档, 可以通过man File::Utils获取该模块的帮助信息
INST_FILE: 模块安装路径, 如果没有安装该模块, 则显示not installed
INST_VERSION: 已安装的该模块版本号
通过Distribution id, 可以直接推断出这个文件的URL. 例如, 以阿里云的CPAN源镜像(http://mirrors.aliyun.com/CPAN/)为例, 那么这个模块包的URL为: http://mirrors.aliyun.com/CPAN/authors/id/P/PE/PEKINGSAM/File-Utils-0.0.5.tar.gz.

有些工具需要我们指定 Distribution id, 4个部分毕竟比较复杂, 其实可以指定后两个部分, 一般来说可以自动补齐前两部分.

另外, CPAN安装模块时, 依赖于 `make` 工具, 所以必须先安装好make. 更简易安装好整个开发包.

```bash
yum -y install make
```

## 手动编译安装模块

例如, 网上搜索到了 `Data::Dumper` 模块, 想要手动安装它.
从网上下载好模块源码包, 然后解压, 进入源码包目录:

```bash
wget https://cpan.metacpan.org/authors/id/X/XS/XSAWYERX/Data-Dumper-2.172.tar.gz
tar xf Data-Dumper-2.172.tar.gz
cd Data-Dumper-2.172/
```

然后看看这个目录下, 存在的文件是Makefile.PL还是Build.PL,
这两种文件都可以用来编译安装, 如果同时存在, 则随便选择一种即可.

```bash
[root@redisa-b Data-Dumper-2.172]# ls -l
total 336
-rw-r--r-- 1 privoxy privoxy  11559 Sep 19 22:36 Changes
-rw-r--r-- 1 privoxy privoxy  45832 Sep 19 22:36 Dumper.pm
-rw-r--r-- 1 privoxy privoxy  51342 Sep 19 22:36 Dumper.xs
-rw-r--r-- 1 privoxy privoxy   1583 Sep 19 22:36 Makefile.PL
-rw-r--r-- 1 privoxy privoxy    618 Sep 19 22:36 MANIFEST
-rw-r--r-- 1 privoxy privoxy    418 Sep 19 22:36 MANIFEST.SKIP
-rw-r--r-- 1 privoxy privoxy    863 Sep 19 22:36 META.json
-rw-r--r-- 1 privoxy privoxy    548 Sep 19 22:36 META.yml
-rw-r--r-- 1 privoxy privoxy 200069 Sep 19 22:36 ppport.h
drwxr-xr-x 3 privoxy privoxy   4096 Sep 19 22:36 t
-rw-r--r-- 1 privoxy privoxy    768 Sep 19 22:36 Todo
```

上面的是Makefile.PL, 所以安装:

```bash
perl Makefile.PL
make
make install
```

如果想要指定安装路径, 则加上 `INSTALL_BASE` 即可:

```bash
perl Makefile.PL INSTALL_BASE=/home/perlapps
```

如果是 `Build.PL`, 则:

```bash
perl Build.PL
./Build
./Build install
```

如果想要指定安装路径, 则加上 `perl Build.PL −−install_base /home/perlapps`

如果是手动指定的安装路径, 还需要设置模块查找路径环境变量:

```bash
export PERL5LIB=/home/perlapps
```

或者在perl程序内部, 指定查找路径:

```bash
#!/usr/bin/perl
use lib qw(/home/perlapps);
```

手动编译安装时, 很可能会因为额外的库依赖问题而导致编译中断.
一般来说, 对于提示缺少的库文件名 `$foo` 来说,
`Debian/Ubuntu`上对应的库文件包是`lib$foo-dev或$foo-dev`,
`redhat`系列上则是`lib$foo-devel`, 使用包管理工具大致搜索一下即可.

## 模块管理工具: 交互式的cpan shell

手动安装模块毕竟不方便. perl自身的CPAN模块提供的shell可以让我们快速安装模块.

```bash
perl -MCPAN -e "shell"
```

它将会进入cpan的交互式shell模式.
在这个交互式模式下, 可以执行很多操作.
例如安装模块, 下载模块, 按照正则查找模块, 查找某个作者的模块, 升级模块, 列出最近上传到CPAN的模块等等.

cpan下能执行的命令, 可以通过h键来获取帮助:

```bash
cpan[8]> h

Display Information                                                (ver 1.9800)
 command  argument          description
 a,b,d,m  WORD or /REGEXP/  about authors, bundles, distributions, modules
 i        WORD or /REGEXP/  about any of the above
 ls       AUTHOR or GLOB    about files in the author's directory
    (with WORD being a module, bundle or author name or a distribution
    name of the form AUTHOR/DISTRIBUTION)

Download, Test, Make, Install...
 get      download                     clean    make clean
 make     make (implies get)           look     open subshell in dist directory
 test     make test (implies make)     readme   display these README files
 install  make install (implies test)  perldoc  display POD documentation

Upgrade
 r        WORDs or /REGEXP/ or NONE    report updates for some/matching/all modules
 upgrade  WORDs or /REGEXP/ or NONE    upgrade some/matching/all modules

Pragmas
 force  CMD    try hard to do command  fforce CMD    try harder
 notest CMD    skip testing

Other
 h,?           display this menu       ! perl-code   eval a perl command
 o conf [opt]  set and query options   q             quit the cpan shell
 reload cpan   load CPAN.pm again      reload index  load newer indices
 autobundle    Snapshot                recent        latest CPAN uploads
```

例如, 通过m命令或者i命令按正则表达式查找包含 `File::Util` 字符的模块:

```bash
cpan[11]> m /^File::Util*/
Module  < File::Util             (TOMMY/File-Util-4.161950.tar.gz)
Module  < File::Util::Cookbook   (TOMMY/File-Util-4.132140.tar.gz)
Module  < File::Util::Definitions (TOMMY/File-Util-4.161950.tar.gz)
Module  < File::Util::Exception  (TOMMY/File-Util-4.161950.tar.gz)
Module  < File::Util::Exception::Diagnostic (TOMMY/File-Util-4.161950.tar.gz)
Module  < File::Util::Exception::Standard (TOMMY/File-Util-4.161950.tar.gz)
Module  < File::Util::Interface::Classic (TOMMY/File-Util-4.161950.tar.gz)
Module  < File::Util::Interface::Modern (TOMMY/File-Util-4.161950.tar.gz)
Module  < File::Util::Manual     (TOMMY/File-Util-4.132140.tar.gz)
Module  < File::Util::Manual::Examples (TOMMY/File-Util-4.132140.tar.gz)
Module  < File::Util::Tempdir    (PERLANCAR/File-Util-Tempdir-0.030.tar.gz)
Module  < File::Utils            (PEKINGSAM/File-Utils-0.0.5.tar.gz)
```

这里列出来的模块版本可能会比CPAN网页上列出的版本更低一些.

使用过程中, 按键可能会非常不好用. 如果需要删除某个已键入的字符, 需要同时按住ctrl+back按键.

例如, 安装File::Util模块:

```bash
cpan[12]> install File::Utils
```

还可以安装网上下载的perl模块源码包, 甚至get命令可以直接下载源码包.
例如, 安装File::Rename模块, 它的 [源码包地址](https://cpan.metacpan.org/authors/id/R/RM/RMBARKER/File-Rename-1.09_02.tar.gz)

```bash
cpan[8]> get RMBARKER/File-Rename-1.09_02.tar.gz
```

下载成功后, 它会提示保存的路径:

```bash
Checksum for /root/.cpan/sources/authors/id/R/RM/RMBARKER/File-Rename-1.09_02.tar.gz ok
```

然后用 `install` 命令安装这个模块源码包即可:

```bash
cpan[9]> install RMBARKER/File-Rename-1.09_02.tar.gz
```

这里的distribution id既可以加上前两部分, 也可以省略前两部分.

默认安装一个模块时需要make test, 但是有些模块需要test非常常的时间,
比如安装CPANPLUS模块时的依赖模块File::Fetch,
它有可能会test非常长的时间, 这时候可以CTRL+C退出, 再次安装时跳过test阶段.

```bash
cpan[10]> notest install CPANPLUS
```

如果想要配置CPAN, 需要使用o conf, 这个命令很关键, 关于配置的信息非常多,
此处只介绍下修改CPAN源的方法. 例如修改CPAN的源为阿里云的源:

```bash
# 1.指定aliyun的CPAN
o conf urllist push https://mirrors.aliyun.com/CPAN/

# 2.提交, 写入到磁盘配置文件
o conf commit

# 3.查看当前的CPAN源
o conf urllist
```

关于cpan交互式, 以及o conf以及CPAN模块的配置文件, 可以man CPAN获取更多用法.

另外注意, 在perl程序代码中,
 可以通过 `CPAN::Shell->COMMAND("arguments");` 实现和交互式模式下一样的操作:

例如, 在perl程序代码中安装模块:

```bash
#!/usr/bin/perl

use CPAN;
CPAN::Shell->install("Acme::Meta");
CPAN::Shell->install("NWCLARK/Acme-Meta-0.02.tar.gz");
```

## CPAN客户端: cpan命令(脚本)

cpan命令是随perl一起安装的一个perl脚本, 它让我们从cpan那恶心的交互式中解脱出来.
但它内部实现的功能主要还是调用`CPAN.pm`, 和cpan交互式是一样的.

大概看几个cpan脚本的常用选项:

```bash
-a: 创建CPAN.pm的autobundle
-D module: 查看模块的详细属性信息. 例如是否安装, 安装的版本号, 最新的版本号, 对应的模块路径, 对应的源码包文件路径, 谁维护的
-g module: 下载最新版本的模块到当前目录
-i module: 安装指定的模块
-j Config.pm: 指定CPAN配置数据的文件
-J: 以CPAN.pm相同的格式dump当前的配置文件
-O: 列出过期的模块
-v: 输出cpan脚本的版本号以及CPAN.pm的版本号
```

例如, 安装File::Util::Manual模块, 很简单了.

```bash
cpan -i File::Util::Manual
```

查看模块的信息:

```bash
[root@redisa-b ~]# cpan -D File::Utils
Reading '/root/.cpan/Metadata'
  Database was generated on Wed, 19 Sep 2018 20:17:03 GMT
File::Utils
-------------------------------------------------------------------------
        (no description)
        P/PE/PEKINGSAM/File-Utils-0.0.5.tar.gz     # 模块的distribution id
        /usr/local/share/perl5/File/Utils.pm       # 模块的安装路径
        Installed: 0.0.5                           # 已安装的模块版本号
        CPAN:      0.000005  up to date            # CPAN中最新的模块版本号
        Yan Xueqing (PEKINGSAM)                    # 作者名称及CPAN中的ID
        yanxueqing10@163.com
```

`cpan -J` 对于了解和修改CPAN模块配置文件非常有帮助, 可以执行一下试试看.

### CPAN客户端: CPANPLUS模块

CPANPLUS提供了和CPAN.pm类似的功能, 但特性更丰富.

首先, 安装它:

```bash
cpan[20]> notest install CPANPLUS
```

cpanp命令可以进入交互式, 也可以使用命令行模式, 命令行模式功能也很丰富.
不过用法都很简单, 和cpan都类似, 可以cpanp -h大致看一下.

例如, 安装MongoDB模块:

```bash
cpanp -i MongoDB
```

卸载MongoDB模块:

```bash
cpanp -u MongoDB
```

### CPAN客户端: cpanm

这个是真正的完全一键安装, 无需任何配置. 而且, 它没有交互式模式.

首先, 安装这个工具所在cpanminus模块. :

```bash
install App::cpanminus
```

选项和用法很简单, 可以cpanm -h去瞧一瞧.

要安装某个模块:

```bash
cpanm File::Utils
```

默认情况下, 它搜索的CPAN源是http://www.cpan.org/, 可以指定CPAN源:

```bash
cpanm --mirrors http://mirrors.aliyun.com/CPAN File::Utils
```

### 卸载模块

要卸载一个模块, 可以安装pmuninstall模块, 它提供pm-uninstall命令, 可以快速卸载模块.

```bash
cpan App::pmuninstall
```

例如, 卸载MongoDB模块:

```bash
pm-uninstall MongoDB
```

默认会给出提示, 是否要确认卸载. 可以指定"-f"选项强制卸载, 无需交互式提示:

```bash
pm-uninstall -f MongoDB
```

当然, 除了pm-uninstall, cpan, cpanp, cpanm都带有卸载的功能.

### local::lib

默认情况下, 安装的第三方模块都会和perl放在一起, 而且对于那些非root用户,
对某些目录没有写入权限, 导致安装失败, 只能sudo.
可以使用local::lib, 自定义安装路径.

首先, 安装这个模块:

```bash
cpan[9]> install local::lib
```

然后回到bash, 可以查看这个模块导出的环境变量:

```bash
$ perl -Mlocal::lib
PATH="/home/fairy/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/home/fairy/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/fairy/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/fairy/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/fairy/perl5"; export PERL_MM_OPT;
```

`local::lib`正是借助于修改某些环境变量(注意其中的PERL5LIB)
让它们脱离perl内置路径`@INC`, 从而影响CPAN客户端安装的目录路径.

然后使用cpan客户端的`-I`开关, 就表示根据local::lib的配置来安装(注: 有些cpan客户端没有`-I`选项).

```bash
cpan -I Text::Levenshtein
```

如果使用cpanm, 它比较聪明一点, 如果你已经设置了 `local::lib`, 它会直接按照设定安装,
如果没有设定, 它会检查对默认的安装目录是否具有写权限, 如果没有,
就自动设置`local::lib`模块. 所以, cpanm无需设置`local::lib`, 但仍然可以显式指明使用local::lib来安装:

```bash
cpanm --local-lib HTML::Parser
```

如果仍然使用cpan客户端, 指定安装路径. 可以修改两个参数:

```bash
cpan[1]> o conf makepl_arg INSTALL_BASE=/home/fairy/perl5
cpan[2]> o conf mbuild_arg "--install_base /home/fairy/perl5"
cpan[3]> o conf commit
```

可以从man CPAN中获取这两个参数的解释: 分别是传递给 `perl Makefile.PL` 和 `./Build` 的参数.
也就是说, 通过设置这两个参数, 无论是makefile格式的安装, 还是build格式的安装, 都会安装到给定目录下.

## CPAN模块的配置文件和配置建议

当我们第一次进入`CPAN shell`的时候, 会让我们配置CPAN, 我们可以选择让它自动配置.
自动配置后, 它会提示配置文件保存到了哪里.

我们也可以在 `cpan` 交互式中使用 `mkmyconfig` 重新生成属于当前用户的配置文件,
它会保存到`~/.cpan/CPAN/MyConfig.pm`,
同时还会将 `local::lib` 的一些相关环境变量追加到 `~/.bashrc` 中.

进入cpan交互式shell, 键入`o conf`即可输出当前的配置.
内容项很多, 有几项是建议修改的:

### 1.修改CPAN的源为阿里云的源

```bash
# 1.指定aliyun的CPAN
o conf urllist push https://mirrors.aliyun.com/CPAN/

# 2.提交, 写入到磁盘配置文件
o conf commit

# 3.查看当前的CPAN源
o conf urllist
```

上面的push命令是将aliyun的CPAN源推到urllist的数组最后一个位置(key是urllist, value是一个由url组成的数组),
还可以使用pop移除数组最后一个元素, 使用shift移除数组第一个元素,
使用unshift推到数组的第一个元素. 如果不给这几个命令, 直接给url, 则替换整个数组.

如果想清空urllist, 可以使用o conf init urllist.

### 2.修改make和build的执行方式, 加上sudo. 这主要是针对非root用户的

```bash
o conf make_install_make_command 'sudo /usr/bin/make'
o conf mbuild_install_build_command 'sudo ./Build'
```

### 3.指定安装路径. 这主要是针对非root用户的

```bash
cpan[1]> o conf makepl_arg INSTALL_BASE=/home/fairy/perl5
cpan[2]> o conf mbuild_arg "--install_base /home/fairy/perl5"
```

### 4.配置自动提交

```bash
o conf auto_commit 1
```

### 5.配置依赖策略

```bash
o conf prerequisites_policy follow
```

除了上面几项, 使用cpan时, 还建议更新和安装以下几个模块:

```bash
install CPAN ExtUtils::MakeMaker Module::Build Bundle::CPAN
```

安装这几个模块可以解决很多不必要的麻烦.
