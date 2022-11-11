# ripgrep: 更快捷的搜索(简介与原理)

[BurntSushi/ripgrep](https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md#recursive-search)

使用 Linux 时想从许多文件中搜索关键词怎么办?
Linux 本身有着 grep 命令可以完成这一任务.
但 grep 较为古老, 使用不便且效率较低, 本文介绍一个更为现代的搜索工具: `ripgrep`.

ripgrep 是一个替代 grep (或者说 GNU grep)命令的搜索工具.
其主要特点是命令的使用更为方便实用,
以及搜索性能极高, 在庞大的项目中有着出色的表现.
并且默认可以忽略 `.gitignore` 文件中的内容, 非常实用.

虽然 ripgrep 是用 Rust 写的, 但早已可以在多种系统下直接安装,
不需要安装 Rust. 为了新手友好以下简略介绍一下基础使用.

所需背景知识:
本文前半部分(安装, 常用用法)会基础的 Linux 操作即可.
后半部分(深入细节)是原理分析, 需要一点计算机体系结构, 操作系统, 算法等基础知识.

## 安装

详见 [GitHub repo 安装说明](https://github.com/BurntSushi/ripgrep#installation).
对于大部分系统可以直接使用包管理工具安装(例如 macOS 可用 Homebrew ).
除非你使用 Rust, 否则不必使用 cargo 进行安装.

特别的, 如果你使用 Debian 10 以下或 Ubuntu 18.10 以下,
需要手动下载安装包 deb 文件进行安装.
注意其中的版本号可以自行修改, 可以去 [Release 页面](https://github.com/BurntSushi/ripgrep/releases) 找到最新版本进行下载安装.

```bash
$ curl -LO https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep_13.0.0_amd64.deb
$ sudo dpkg -i ripgrep_13.0.0_amd64.deb
```

+ 检查版本:

```bash
$ rg --version
ripgrep 13.0.0
-SIMD -AVX (compiled)
+SIMD +AVX (runtime)
```

## 常用用法

我的日常基本只用到这些, 比使用 grep 简短许多!

1. 默认用法: 在当前目录下的所有文件内容中, 递归搜索字符串. 例如, 搜索 `hello`:

```bash
$ rg hello
```

`ripgrep` 的输入认为是正则表达式, 因此一些字符需要转义
(并由于 bash/zsh 也识别通配符的原因需加单引号), 例如, 如果要搜索 `main()`:

```bash
$ rg 'main\(\)'
    test.cpp
    13:int main() {
```

2. 查看上下文,  `-A` 表示 `after` 的行数,  -B 表示 `before` 的行数:

```bash
$ rg somebody -A 2 -B 2
    test.txt
    4-They'd banish us, you know.
    5-
    6:How dreary to be somebody!
    7-How public, like a frog
    8-To tell your name the livelong day
```

3. 在特定文件名模式中搜索, 使用 `-g` 参数(全称 `--glob`):

```bash
$ rg main -g '*.cpp'
```

4. 在 `当前目录` 及其所有 `子目录` 中搜索文件名:

```bash
$ rg --files | rg test
test.txt
test.cpp
```

test.txt test.cpp更多用法和细节可以参考文档或官网或是网络上其它帖子,
但我个人使用过程中上述几种就足够了.

## 深入细节

`ripgrep` 另一个重要优势是其非常高的性能.
在其作者 [Andrew Gallant 的这篇博客](https://blog.burntsushi.net/)
中说明了ripgrep 性能高的主要原因及一些性能计算方法.

`ripgrep` 使用 `Rust` 语言编写, 得益于许多 Rust 原生库的优势.
在设计的时候需要重点考虑以下几个角度:

+ 使用风格
+ 文件的遍历
+ 并发
+ 搜索算法的设计
+ 搜索粒度

## 使用风格

+ 不递归: grep 是默认不进行任何递归搜索, 输入什么就搜索什么,
因此 grep 会更侧重于考虑对于单一大文件如何进行更快速的搜索.
+ 递归: `ack` 是后来出现的搜索工具, 默认就对当前目录进行递归搜索,
于是需要更多考虑文件系统的递归遍历效率.  ripgrep 也是沿用了这一风格.

## 文件的遍历

文件的遍历看似简单, 但一个没有精心设计的迭代过程会使用过多不必要的系统调用,
导致性能退化. 并且, 由于相关逻辑十分底层, 追踪困难,
连 Python 也是 2014 年才实现了 [scandir() 的优化](https://benhoyt.com/writings/scandir/).

Rust 库中的递归迭代器已经实现好了这一部分, 尽可能减少系统调用的次数,
ripgrep 借此获得了遍历上的高效.
此外, 还要处理文件名筛选, 忽略 `.gitignore` 中的文件等情况,
博客中没有详细说明算法细节, 如有可能之后补充.

## 并发

+ 同步问题: `ripgrep` 会将文本划分到多个线程进行并行搜索.
线程之间需要进行同步, 相比有锁数据结构,
`ripgrep` 使用了更为快速的无锁队列
[Chase-Lev work-stealing queue](https://github.com/kinghajj/deque) 进行通信.

+ 打印输出: 打印是另一个需要考虑的问题.
多线程如果直接输打印出会使得对两个文件的搜索结果交织在一起.
而如果使用互斥锁进行控制, 会使得性能降低, 最坏情况甚至接近串行.
大部分相似工具的解决方案都是将每个线程的结果分别写到各自的缓冲区,
再用一个线程将完成了的结果进行输出.
但你可能会问, 如果输出结果非常多, 例如一个 2GB 大小的文件每一行都匹配成功了,
岂不是需要 2GB 大小的缓冲区才能正确输出?没错, 就是会有这个问题!
但这种情况极其少见, 并且在有些情况下可以想办法缓解,
因此在空间和时间的权衡中,  `ripgrep` 做了这样的决定.

## 搜索算法的设计

`ripgrep` 需要支持 `正则搜索`.
正则搜索引擎主要有两个类型: `回溯法` 和 `有穷自动机`.
回溯法可以很快, 支持的语法更全面, 但在最坏情况下会十分缓慢.
有穷自动机相对不全面, 但可以保证在 `线性时间` 内求解.

考虑到, 如果进行普通的字符串搜索匹配
如 [Boyer-Moore 算法](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string_search_algorithm).
其效率是要高于正则匹配的,
一方面是其算法本身已经设计的很好, 一方面是还存在一些硬件优化
如 C 语言中使用了 SIMD 指令的函数 [memchr](https://man7.org/linux/man-pages/man3/memchr.3.html). 那么能否利用这些优势加快我们的搜索?

由于 `ripgrep` 的使用场景下, 通常是文本空间很大, 但能成功匹配的很少.
于是 `ripgrep` 使用了一个技巧: 提取正则表达式中的一些常量关键词,
如 `\w+foo\d+` 中的 `foo`. 先使用字符串搜索匹配算法查找 `foo`,
在能够匹配的字符串上, 再进行较慢的 `正则匹配` 来确认.
更多细节请自行参考[博客](https://blog.burntsushi.net/ripgrep/#literal-optimizations).

## 搜索粒度

虽然 `ripgrep` 的输出是按行输出, 但不等于搜索时候就要按行搜索.
理由很简单, 因为按行分割文本本身就是一个比较耗时的事情,
而成功匹配又是一个低概率事件.
因此所有搜索工具都是直接对一个较大的文本块进行搜索, 匹配成功后再将相应行打印输出.

`ripgrep` 使用的方案就是一块一块地读入文本进行匹配的增量搜索 (incrementally searching).
但这其中涉及非常多复杂的情况需要考虑, 例如需要打印行号的时候,
两块的分界在一行文字的中间, 一行文字过长一块缓冲区都放不下,
显示上下文等等, 程序的逻辑十分复杂.

其它工具有的解决方案是如将文件全部读入或是利用交换分区等,
但这时就无法对标准输入流进行搜索了.
ripgrep 为了应对各种需求, 选择了更为复杂的方式.

此外, 这篇博客中有一半的篇幅在描述性能的计算, 角度非常全面, 值得学习.
