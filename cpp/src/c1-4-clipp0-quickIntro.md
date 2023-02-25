# c++ 命令行参数解析

[优雅的C++命令行参数解析](https://zhuanlan.zhihu.com/p/342388341)
[muellan/clipp](https://github.com/muellan/clipp)

为C++11/14/17编写的易于使用, 功能强大,
表现力强的命令行参数处理, 包含在单个头文件中.

`options`(选项), `options+value(s)`(选项->值),
`positional values`(位置参数), `positional commands`(位置命令),
`nested alternatives` (嵌套可选),
`decision trees`(决策树),
`joinable flags`(可串联标志),
自定义 `value filters`(自定义值过滤器), ...

文档生成(用法概述, 手册页); 错误处理
大量的例子; 大量的测试

## 快速介绍

### 简单的用例 - 简单的设置!

考虑一下这个命令行界面.

```man
SYNOPSIS
    convert <input file> [-r] [-o <output format>] [-utf16]

OPTIONS
    -r, --recursive  convert files recursively
    -utf16           use UTF-16 encoding
```

下面是定义 位置参数 `input file`, 和三个选项 `-r`, `-o`和 `-utf16` 的代码.
如果解析失败, 上述默认的类似于 `man page` 的片段将被打印到 `stdout`.

```cpp
#include <iostream>
#include "clipp.h"
using namespace clipp; using std::cout; using std::string;

int main(int argc, char* argv[]) {
    bool rec = false, utf16 = false;
    string infile = "", fmt = "csv";

    auto cli = (
        value("input file", infile),
        option("-r", "--recursive").set(rec).doc("convert files recursively"),
        option("-o") & value("output format", fmt),
        option("-utf16").set(utf16).doc("use UTF-16 encoding")
    );

    if(!parse(argc, argv, cli)) cout << make_man_page(cli, argv[0]);
    // ...
}
```

### 稍复杂的例子

```man
SYNOPSIS
    finder make <wordfile> -dict <dictionary> [--progress] [-v]
    finder find <infile>... -dict <dictionary> [-o <outfile>] [-split|-nosplit] [-v]
    finder help [-v]

OPTIONS
    --progress, -p           show progress
    -o, --output <outfile>   write to file instead of stdout
    -split, -nosplit         (do not) split output
    -v, --version            show version
```

这个 `CLI` 有三个可供选择的命令(`make`, `find`, `help`),

一些位置值参数(`<wordfile>`, `<infile>`), 其中 `<infile>`是可重复的;
一个带有值参数的 `required flag`(必要项),  `-dict <dictionary>`;
一个带有值参数的 `option`(可选项), `-o <outfile>`;
一个带有两个`alternatives `(方案)的可选项, `-split`, `-nosplit`;
和两个 `conventional options`(常用选项), `-v`, `-progress`.

下面是定义接口的代码, 生成上面的 `man page snippet `, 并处理解析结果.

```cpp
using namespace clipp; using std::cout; using std::string;

//variables storing the parsing result; initialized with their default values
enum class mode {make, find, help};
mode selected = mode::help;
std::vector<string> input;
string dict, out;
bool split = false, progr = false;

auto dictionary = required("-dict") & value("dictionary", dict);

auto makeMode = (
    command("make").set(selected,mode::make),
    values("wordfile", input),
    dictionary,
    option("--progress", "-p").set(progr) % "show progress" );

auto findMode = (
    command("find").set(selected,mode::find),
    values("infile", input),
    dictionary,
    (option("-o", "--output") & value("outfile", out)) % "write to file instead of stdout",
    ( option("-split"  ).set(split,true) |
      option("-nosplit").set(split,false) ) % "(do not) split output" );

auto cli = (
    (makeMode | findMode | command("help").set(selected,mode::help) ),
    option("-v", "--version").call([]{cout << "version 1.0\n\n";}).doc("show version")  );

if(parse(argc, argv, cli)) {
    switch(selected) {
        case mode::make: /* ... */ break;
        case mode::find: /* ... */ break;
        case mode::help: cout << make_man_page(cli, "finder"); break;
    }
} else {
     cout << usage_lines(cli, "finder") << '\n';
}
```

## 快速参考

下面是几个例子, 应该让你对 `clipp` 的工作原理有一个概念.
考虑这个基本配置, 有几个我们想用命令行参数来设置的变量.

```cpp
int main(int argc, char* argv[]) {
    using namespace clipp;

    // 定义一些变量
    bool a = false, b = false;
    int n = 0, k = 0;
    double x = 0.0, y = 0.0;
    std::vector<int> ids;

    auto cli = ( /* 在这里定义命令行界面*/ );

    parse(argc, argv, cli);    // 这里会自动排出 argv[0], 它总是当前exe的名称

    std::cout << usage_lines(cli, "exe") << '\n';
}
```

命令行界面(usage_lines)  Code ( cli 括号中的内容)

+ `exe [-a]`    ;    `option("-a", "--all").set(a)`
+ `exe [--all]` ;    `option("--all", "-a", "--ALL").set(a)`
+ `exe [-a] [-b]`   ;    `option("-a").set(a), option("-b").set(b)`
+ `exe -a`  ;    `required("-a").set(a)`
+ `exe [-a] -b` ;    `option("-a").set(a), required("-b").set(b)`
+ `exe [-n <times>]`    ;      `option("-n", "--iter") & value("times", n)`
+ `exe [-n [<times>]]`  ;      `option("-n", "--iter") & opt_value("times", n)`
+ `exe -n <times>`  ;      `required("-n", "--iter") & value("times", n)`
+ `exe -n [<times>]`    ;      `required("-n", "--iter") & opt_value("times", n)`
+ `exe [-c <x> <y>]`    ;      `option("-c") & value("x", x) & value("y", y)`
+ `exe -c <x> <y> ` ;      `required("-c") & value("x", x) & value("y", y)`
+ `exe -c <x> [<y>]`    ;      `required("-c") & value("x", x) & opt_value("y", y)`
+ `exe [-l <lines>...]` ;      `option("-l") & values("lines", ids)`
+ `exe [-l [<lines>...]]`       ;       `option("-l") & opt_values("lines", ids)`
+ `exe [-l <lines>]...`     ;       `repeatable( option("-l") & value("lines", ids) )`
+ `exe -l <lines>...`       ;       `required("-l") & values("lines", ids)`
+ `exe -l [<lines>...]`     ;       `required("-l") & opt_values("lines", ids)`
+ `exe (-l <lines>)...`     ;       `repeatable( required("-l") & value("lines", ids) )`
+ `exe fetch [-a]`      ;       `command("fetch").set(k,1), option("-a").set(a)`
+ `exe init | fetch [-a]`   ;  `command("init").set(k,0) | (command("fetch").set(k,1), option("-a").set(a))`

+ `exe [-a|-b]` ;   `option("-a").set(a) | option("-b").set(b)`
+ `exe [-m a|b]`    ;   `option("-m") & (required("a").set(a) | required("b").set(b))`

## 动机

好吧, 我没有找到一个能让构建简单的命令行界面变得简单的库, 也没有找到用来构建复杂的CLI 的.
我真的不想为了3个简单的命令行选项而写20行的模板.
我也不想为此而拖累像boost或Qt这样的怪物.
随着时间的推移, clipp演变成了一种特定领域的语言(纯C++),
我希望它至少可以在某种程度上接近 [docopt](http://docopt.org/) 的可读性,
但可以利用C++类型系统的优势(工具性等).

### 其他库(Boost, POCO, Qt, adishavit/Argh, Taywee/args......或'getopt')

+ 往往涉及大量的 `boilerplate`( 模板,对于非常简单的用例).
+ 有些库在可用性方面可能还不错, 但不能让你建立复杂的界面,
包括 nested alternatives, 混合的 `commands ` 和 `options`,
位置值, 每个 `option` 超过两个 `option` 等.

+ 我非常喜欢像 [Argh](https://github.com/adishavit/argh) 这样的  ad-hoc parsers(专用分析器), 因为它们很简单,
但它们不会生成用法/手册页, 也不允许有错误检查的复杂界面.
+ 大多数库使人很难通过看代码来弄清所产生的命令行界面
(好吧, 你来判断我在这方面是否做得更好...).
+ 有些库有大量的依赖关系(Qt, Boost). 我只想要一个头文件.

+ 有些库要求: `命令/选项` 与相关的 `代码` 分离.
我发现这比把所有与一个选项相关的东西放在一个地方更难维护.
+ 有时, `命令/选项` 的 `标志字符串` 必须重复多次.
+ 许多库都有关于 `标志名称`, `格式化`, `可连接性`(joinability) 等方面的限制.

那 [docopt](http://docopt.org/) 呢?

我喜欢这个基本想法, 但我不喜欢代码中的冗余, 尤其是涉及到重复的 字符串字面值.
Docopt从一个 "man page docstring "生成一个命令行参数解析器.
在解析之后, 你必须查询 `字典` 以获得 `values` 或检查 `option` 是否存在.

这意味着你要么必须两次提到相同的 `标志名称`(在 docstring 中和在查询中),
要么你必须使用 `字符串变量`, 这使得 `docstring` 难以阅读, 有点违背了 `docopt` 的目的.
此外, 代码中往往充斥着 `字符串到XXX` 的转换.

我还希望, 能够在代码中把所有与 某个`选项/命令/值` 有关的东西放在一起,
我发现这对有很多命令行选项的程序非常有用.
Docopt 不允许你这样做,  因为 `界面的定义` 和 `执行任何动作的代码` 总是分开的
(除非你放弃 docopt 核心的 "美丽的文档页 "的想法, 并将你的输入字符串逐个建立给 parser generator).

## 设计目标

+ 尽量减少 `boilerplate`
+ 为简单的用例编写简单的代码
+ 良好的代码可读性(在 C++ 允许的范围内).
+ 避免歧义
+ 消除重复敲代码
+ 能够将与一个选项/命令/值有关的代码放在一起
+ 支持许多不同的命令行界面惯例
+ 支持不同的编码风格, 惯例和品味
+ 尽可能地 `值语义`(value semantics )
+ 在未来不要破坏 `clipp` 的一般界面和惯例

## 需求和依赖

需要一个大部分符合C++11的编译器

### 编译器, clipp的编译器有

+ g++ 5.3.0, g++ 5.4.1, g++ 6, g++ 7
+ clang++ 3.8, clang++ 3.9, clang++ 4.0, clang++ 5.0
+ MSVC 14.11.25503 (编译器 19.11.25547)

## 反馈

我很高兴听到任何在他们的项目中使用clipp的人的意见.
如果你发现错误或设计缺陷, 或想建议改进, 请 open an issue  或提交 pull request.

## 开发

`clipp` 还很年轻, 所以我可能不会在短时间内增加新的功能, 而是.

+ 修复bug
+ 改进测试案例和覆盖率
+ 增加更多的代码文档
+ 清理代码/使其更容易理解
+ 增加使用其他字符串类的能力
