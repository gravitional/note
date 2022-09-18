# Introduction

## Perl 是啥意思?

### Perl到底有什么用?

Perl 适合于那些在三分钟内完成的快速和肮脏的程序.
Perl也适用于那些需要十几个程序员花三年时间才能完成的长篇大论的程序.
当然, 你可能会发现自己写的许多程序只需要不到一个小时就能完成, 从最初的计划到完全测试过的代码,

Perl是针对90%的文本工作, 和10%的其他问题而优化的.
这种描述似乎适合于现在突然出现的大多数编程任务.
在一个完美的世界里, 每个程序员都会知道每种语言; 你总是能够为每个项目选择最好的语言.
大多数时候, 你会选择Perl. 尽管在拉里创造Perl的时候, 网络在蒂姆-伯纳斯-李的眼里还不是很重要, 但这是一场网络上的婚姻.
有些人声称, 在20世纪90年代初, Perl的部署使人们能够非常迅速地将大量的内容转移到HTML格式中, 而网络没有内容就无法存在.

当然, Perl也是小型CGI脚本(由 web server 运行的程序)的宠儿--
以至于许多不了解情况的人还会说: "CGI不就是Perl吗? 你为什么要用Perl来做CGI以外的事情? "
我们觉得这些说法很有趣.

### Perl不适合做什么?

那么, 既然 Perl 适合做这么多事情, 那么它不适合做什么呢? 如果你想制作一个不透明的二进制文件, 你就不应该选择 Perl.

那是一个你可以送人或卖给别人的程序, 而后者又不能看到你在源代码中的秘密算法, 因此也不能帮助你维护或调试你的代码.
当你把你的Perl程序给别人时, 你通常会给他们源代码, 而不是一个不透明的二进制文件.
如果你希望得到一个不透明的二进制文件, 那么我们必须告诉你, 它们并不存在.

如果有人能安装和运行你的程序, 他们就能把它变成源代码.
当然, 这不一定是你开始时的那个源代码, 但它将是某种源代码.

真正能让你的秘密算法保持秘密的方法是, 唉, 应用适当数量的律师.
他们可以写一份许可证, 说 "你可以用这些代码做这个, 但你不能做那个. 如果你违反了我们的规则, 我们有适当数量的律师来确保你会后悔".

## 如何安装CPAN模块

`CPAN` 是 Perl 综合档案网(Comprehensive Perl Archive Network), 是Perl的一站式购物网站.
它有Perl本身的源代码, 可随时安装到各种非 Unix系统, 实例, 文档, Perl的扩展, 以及关于Perl的信息档案.
简而言之, CPAN是全面的.

CPAN被复制在世界各地的数百台镜像机器上;
从 http://search.cpan.org/ 来浏览或搜索档案.

如果你不能上网, 你可能会找到一张CD-ROM或其他的文件. 上面有CPAN的所有有用部分.
请向当地的技术书店查询. 不过, 要找一个最近制作的档案.
因为CPAN每天都在变化, 两年前的存档是个老古董.
更好的办法是找个能上网的好心朋友给你刻录一个今天的CPAN.

这里有一些推荐的方法来安装CPAN中的模块, 就像Perl 的其他方面一样, 有多种选择.

### 一些基础知识

[XS]: https://perldoc.perl.org/perlxs
[C编写]: https://en.wikipedia.org/wiki/C_(programming_language)
[CPAN]: https://www.cpan.org/

大多数 `Perl模块` 是用Perl编写的, 有些使用 [XS][] (它们是用 [C编写][] 的), 所以需要一个C编译器(这很容易设置--不要惊慌),
请看下面你选择的操作系统以了解如何获得正确的编译器.
`模块`可能依赖于其他模块(几乎总是在 [CPAN][] 上), 没有这些依赖模块就不能安装(或者没有它们的特定版本). 值得通读下面这些选项的文档.
CPAN上的许多模块都需要一个比较新的Perl版本(5.8版或以上).

### 快速启动

安装 `cpanm` 以使安装其他模块更容易(你以后会感谢我们).
你需要在一个终端模拟器(Mac OS X, Win32, Linux)中输入这些命令

```bash
cpan App::cpanminus
```

现在安装任何[你能找到的模块](https://www.cpan.org/modules/index.html).

```bash
cpanm Module::Name
```

### 工具

[App::cpanminus]: https://metacpan.org/release/App-cpanminus
[安装说明]: https://metacpan.org/pod/App::cpanminus#INSTALLATION
[cpan-outdated]: https://metacpan.org/dist/cpan-outdated/view/script/cpan-outdated
[pm-uninstall]: https://metacpan.org/dist/App-pmuninstall/view/bin/pm-uninstall
[cpan-listchanges]: https://metacpan.org/dist/cpan-listchanges/view/script/cpan-listchanges
[App::perlbrew]: https://metacpan.org/dist/App-perlbrew
[cpanp]: https://metacpan.org/dist/CPANPLUS/view/bin/cpanp
[CPANPLUS]: https://metacpan.org/dist/CPANPLUS
[cpanm]: https://metacpan.org/dist/App-cpanminus/view/bin/cpanm
[CPAN]: https://metacpan.org/dist/CPAN
[cpan]: https://metacpan.org/dist/CPAN/view/scripts/cpan

帮助你安装和管理你的模块.

`local::lib` 使你能够将模块安装到指定的目录中, 而不需要 `root` 或管理员权限.
关于如何开始, 请看[引导技术](https://metacpan.org/pod/local::lib#The-bootstrapping-technique).
你可以为每个用户/项目/公司创建一个目录, 并通过`复制`该目录部署到其他服务器(只要你在同一个操作系统和 `perl版本`).

来自 [App::cpanminus][] 的 [cpanm][] 是从 CPAN `获取`, `解包`, `构建` 和 `安装` 模块的脚本.
它是无依赖的(可以自己启动, bootstrap), 并且不需要配置([安装说明][]).

它可以自动完成 [CPAN][] 上大多数模块的整个构建过程, 并且与 `local::lib` 和 `perlbrew` 配合得很好.
许多有经验的Perl开发者把它作为他们的首选工具.
相关工具: [cpan-outdated][], [pm-uninstall][], [cpan-listchanges][].

来自 [App::perlbrew][] 的 `perlbrew` 是很有用的,
如果你的系统 `perl` 太老了, 不能支持现代的 `CPAN` 模块, 或者它在其他方面很麻烦(RedHat/CentOS也在此列).
`perlbrew` 使在任何目录下安装 `Perl` 的过程变得更容易, 这样你就可以完全独立于任何系统 `Perl` 工作而不需要 `root` 或管理员的权限.
你可以在不同的项目中使用多个版本的 `Perl`(也许随着你的升级).
与系统 `Perl` 的分离使得服务器的维护更加容易, 而且你对你的项目的设置更加有信心. 目前不支持Windows.

[CPAN][] 中的 [cpan][] 从1997年(5.004)开始与Perl一起发布.
它比 `cpanm` 有更多的选项, 它也更 verbose.

[CPANPLUS][] 的 [cpanp][] 从5.10(2007)到5.20(2014)一直随Perl发布.
它提供了比 `cpanm` 或 `cpan` 更多的选项, 并且可以像 `cpanminus` 一样安装.

### Windows上的Perl(Win32和Win64)

[Strawberry Perl]: https://strawberryperl.com/
[ActiveState]: https://www.activestate.com/products/perl/

[Strawberry Perl][] 是一个用于Windows操作系统的Perl开源二进制发行版.
它包括一个编译器和预装模块, 提供了直接从 `CPAN` 安装 `XS CPAN` 模块的能力. 它还预装了很多模块, 包括 `cpanm`.

[ActiveState][] 提供Perl的二进制发行版(适用于许多平台), 以及他们自己的 `Perl` 包管理器(`ppm`).
有些模块不能以 `ppm` 的形式提供, 或者在 `ppm` 构建系统中报告了错误, 这并不意味着它们不能工作.
你可以使用 `cpan` 脚本从 `CPAN` 中构建模块来补充 `ActiveState Perl`.

### Mac OSX上的Perl

为了建立和安装你自己的模块, 你需要安装 `Command Line Tools for XCode` 或  `XCode` 软件包
--详情见我们的[ports page](https://www.cpan.org/ports/binaries.html#mac_osx).
一旦你完成了这些, 你就可以使用上面提到的所有工具.

### 在其他类似Unix的操作系统上使用Perl

通过你的软件包管理器安装 `make`. 然后你就可以使用上面提到的所有工具了.

### 其他工具

[CPAN::Mini]: https://metacpan.org/dist/CPAN-Mini
[CPAN::Mini::Inject]: https://metacpan.org/release/CPAN-Mini-Inject/

[CPAN::Mini][] 可以为你提供一个 [CPAN][] 的最小镜像(只是所有模块的最新版本). 这使得离线工作变得很容易.

[CPAN::Mini::Inject][] 允许你将你自己的模块添加到你的 `CPAN::Mini` 的本地镜像中.
所以你可以通过你用于 `CPAN` 模块的相同工具来安装和部署你自己的模块.

### 我应该使用哪些模块?

[Task::Kensho]: https://metacpan.org/release/Task-Kensho
[https://metacpan.org/]: https://metacpan.org//
[社区]: https://www.perl.org/community.html
[邮件列表]: https://lists.perl.org/
[Perl Mongers]: https://www.pm.org/

[Task::Kensho][] 为各种任务列出了建议的最佳实践模块.
[https://metacpan.org/][] 可以帮你搜索CPAN.
你也可以参与到 [社区][] 中来, 在 [邮件列表][] 中询问, 或者找到离你最近的 [Perl Mongers][] 小组.

## Perl::LanguageServer, vscode

[Perl::LanguageServer](https://marketplace.visualstudio.com/items?itemName=richterger.perl&ssr=false#overview)

[ Gerald Richter/Perl-LanguageServer](https://metacpan.org/pod/Perl::LanguageServer)

若要在 VS Code 中使用 `Perl::LanguageServer`, 也就是 VS 商店中 perl 语言的拓展,
首先安装拓展, 然后安装它的依赖, 按照 CPAN 官网的教程 [install CPAN modules](https://www.cpan.org/modules/INSTALL.html),
先安装 `cpanm` 以使安装其他模块更容易

```bash
cpan App::cpanminus
```

然后使用 `cpanm`  安装 Perl LanguageServer:

```bash
cpanm Perl::LanguageServer
```

这一步可能会报错, 由于 LanguageServer 的依赖安装失败, 例如

    ! Configure failed for IO-AIO-4.76. See /home/你的名字/.cpanm/work/1638862570.72724/build.log for details.

但是报错信息会给出安装日志的路径, 比如上文的 `/home/你的名字/.cpanm/work/1638862570.72724`.
`cd` 到这个目录, 可以查看失败的详细原因. 对于我的情况, 在这个文件夹的 `config.log` 文件中查找 `error`, 可以看到:

```log
configure:2721: x86_64-linux-gnu-gcc -D_REENTRANT ...   RE -L/usr/share/perl/5.30 -lperl -ldl -lm -lpthread -lc -lcrypt >&5
/usr/bin/ld: cannot find -lperl
collect2: error: ld returned 1 exit status
```

关键错误是 `/usr/bin/ld: cannot find -lperl`, bing `lperl` 可知, 是缺少一个依赖库 `libperl-dev`,
参考: [can't find -lperl](https://blog.csdn.net/jianzhibeihang/article/details/4042702), 一行命令安装

```bash
sudo apt-get install libperl-dev
```

再次运行 `cpanm Perl::LanguageServer` 解决, 如果不行的话, 可以加 `sudo` 试试.

## ActiveState Perl, Strawberry Perl

[ActiveState Perl, Strawberry Perl 与 DWIM Perl 有什么区别](https://www.zhihu.com/question/31408723/answer/59819082)
