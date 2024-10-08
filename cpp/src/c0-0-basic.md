# cpp 基本概念

[runoob-cpp](https://www.runoob.com/cplusplus/cpp-environment-setup.html).

## g++ 应用说明

程序 `g++` 是将 `gcc` 默认语言设为 `C++` 的一个特殊的版本, 链接时它自动使用 `C++` 标准库而不用 `C` 标准库.
若遵循源码的命名规范并指定对应库的名字, 即可以用 `gcc` 来编译链接 `C++` 程序:

```bash
gcc main.cpp -lstdc++ -o main
gcc helloworld.cpp -lstdc++ -o helloworld
```

下面是一个保存在文件 `helloworld.cpp` 中一个简单的 `C++` 程序的代码:

```cpp
#include <iostream>
using namespace std;
int main()
{
    cout << "Hello, world!" << endl;
    return 0;
}
```

最简单的编译方式:

```bash
g++ helloworld.cpp
```

由于命令行中未指定可执行程序的文件名, 编译器采用默认的 `a.out`. 程序可以这样来运行:

```bash
$ ./a.out
Hello, world!
```

通常我们使用 `-o` 选项指定可执行程序的文件名, 以下实例生成一个 `helloworld` 的可执行文件:

```bash
g++ helloworld.cpp -o helloworld
```

执行 `helloworld`:

```bash
$ ./helloworld
Hello, world!
```

如果是多个`C++`代码文件, 如 `runoob1.cpp, runoob2.cpp`, 编译命令如下:

```bash
g++ runoob1.cpp runoob2.cpp -o runoob
```

生成一个`runoob`可执行文件.

`g++`有些系统默认是使用`C++98`, 我们可以指定使用`C++11`来编译`main.cpp`文件:

```bash
g++ -g -Wall -std=c++11 main.cpp
```

`g++`常用命令选项

| 选项           | 解释                                                                                       |
| -------------- | ------------------------------------------------------------------------------------------ |
| `-ansi`        | `只支持 ANSI 标准的 C 语法. 这一选项将禁止 GNU C 的某些特色,  例如 asm 或 typeof 关键词. ` |
| `-c`           | `只编译并生成目标文件. `                                                                   |
| `-DMACRO`      | `以字符串"1"定义 MACRO 宏. `                                                               |
| `-DMACRO=DEFN` | `以字符串"DEFN"定义 MACRO 宏. `                                                            |
| `-E`           | `只运行 C 预编译器. `                                                                      |
| `-g`           | `生成调试信息. GNU 调试器可利用该信息. `                                                   |
| `-IDIRECTORY`  | `指定额外的头文件搜索路径DIRECTORY. `                                                      |
| `-LDIRECTORY`  | `指定额外的函数库搜索路径DIRECTORY. `                                                      |
| `-lLIBRARY`    | `连接时搜索指定的函数库LIBRARY. `                                                          |
| `-m486`        | `针对 486 进行代码优化. `                                                                  |
| `-o`           | `FILE 生成指定的输出文件. 用在生成可执行文件时. `                                          |
| `-O0`          | `不进行优化处理. `                                                                         |
| `-O`           | `或 -O1 优化生成代码. `                                                                    |
| `-O2`          | `进一步优化. `                                                                             |
| `-O3`          | `比 -O2 更进一步优化, 包括 inline 函数. `                                                  |
| `-shared`      | `生成共享目标文件. 通常用在建立共享库时. `                                                 |
| `-static`      | `禁止使用共享连接. `                                                                       |
| `-UMACRO`      | `取消对 MACRO 宏的定义. `                                                                  |
| `-w`           | `不生成任何警告信息`                                                                       |
| `-Wall`        | `生成所有警告信息`                                                                         |

## C++ 基本语法

`C++`程序可以定义为对象的集合, 这些对象通过调用彼此的方法进行交互.
现在让我们简要地看一下什么是类, 对象, 方法, 即时变量.

+ **对象** - 对象具有状态和行为. 例如: 一只狗的状态 - 颜色, 名称, 品种, 行为 - 摇动, 叫唤, 吃. 对象是类的实例.
+ **类** - 类可以定义为描述对象行为/状态的模板/蓝图.
+ **方法** - 从基本上说, 一个方法表示一种行为. 一个类可以包含多个方法. 可以在方法中写入逻辑, 操作数据以及执行所有的动作.
+ **即时变量** - 每个对象都有其独特的即时变量. 对象的状态是由这些即时变量的值创建的.

### C++ 程序结构

让我们看一段简单的代码, 可以输出单词 `Hello World`.

实例

```cpp
#include <iostream>
using namespace std;

// main() 是程序开始执行的地方

int main()
{
   cout << "Hello World"; // 输出 Hello World
   return 0;
}
```

接下来我们讲解一下上面这段程序:

+ C++ 语言定义了一些头文件, 这些头文件包含了程序中必需的或有用的信息. 上面这段程序中, 包含了头文件`<iostream>`.
+ 下一行 `using namespace std`; 告诉编译器使用`std`命名空间. 命名空间是 `C++` 中一个相对新的概念.
+ 下一行 `// main()` 是程序开始执行的地方 是一个单行注释. 单行注释以 `//` 开头, 在行末结束.
+ 下一行 `int main()` 是主函数, 程序从这里开始执行.
+ 下一行 `cout << "Hello World"`; 会在屏幕上显示消息 "`Hello World`".
+ 下一行 `return 0`; 终止 `main( )`函数, 并向调用进程返回值 `0`.

### 编译 & 执行 C++ 程序

接下来让我们看看如何把源代码保存在一个文件中, 以及如何编译并运行它. 下面是简单的步骤:

+ 打开一个文本编辑器, 添加上述代码.
+ 保存文件为 `hello.cpp`.
+ 打开命令提示符, 进入到保存文件所在的目录.
+ 键入 '`g++ hello.cpp`', 输入回车, 编译代码. 如果代码中没有错误, 命令提示符会跳到下一行, 并生成 `a.out` 可执行文件.
+ 现在, 键入 `a.out` 来运行程序.
+ 您可以看到屏幕上显示 '`Hello World`'.

```bash
$ g++ hello.cpp
"nothing"
$ ./a.out
Hello World
```

请确保您的路径中已包含 `g++` 编译器, 并确保在包含源文件 `hello.cpp` 的目录中运行它. 您也可以使用 `makefile` 来编译 `C/C++` 程序.

### C++ 中的分号 & 语句块

在 `C++` 中, 分号是语句结束符. 也就是说, 每个语句必须以分号结束. 它表明一个逻辑实体的结束.

例如, 下面是三个不同的语句:

```cpp
x = y;
y = y+1;
add(x, y);
```

语句块是一组使用大括号括起来的按逻辑连接的语句. 例如:

```cpp
{
   cout << "Hello World"; // 输出 Hello World
   return 0;
}
```

`C++` 不以行末作为结束符的标识, 因此, 您可以在一行上放置多个语句. 例如:

```cpp
x = y;
y = y+1;
add(x, y);
```

等同于

```cpp
x = y; y = y+1; add(x, y);
```

### C++ 标识符

`C++` 标识符是用来标识变量, 函数, 类, 模块, 或任何其他用户自定义项目的名称.
一个标识符以字母 `A-Z` 或 `a-z` 或下划线 `_` 开始, 后跟零个或多个字母, 下划线和数字(`0-9`).

`C++` 标识符内不允许出现标点字符, 比如 `@`, `&` 和 `%`. `C++`是区分大小写的编程语言.
因此, 在 `C++` 中, `Manpower` 和 `manpower` 是两个不同的标识符.

下面列出几个有效的标识符:

```cpp
mohd       zara    abc   move_name  a_123
myname50   _temp   j     a23b9      retVal
```

### C++ 关键字

下表列出了 `C++` 中的保留字. 这些保留字不能作为常量名, 变量名或其他标识符名称.

|              |           |                  |          |
| ------------ | --------- | ---------------- | -------- |
| asm          | else      | new              | this     |
| auto         | enum      | operator         | throw    |
| bool         | explicit  | private          | true     |
| break        | export    | protected        | try      |
| case         | extern    | public           | typedef  |
| catch        | false     | register         | typeid   |
| char         | float     | reinterpret_cast | typename |
| class        | for       | return           | union    |
| const        | friend    | short            | unsigned |
| const_cast   | goto      | signed           | using    |
| continue     | if        | sizeof           | virtual  |
| default      | inline    | static           | void     |
| delete       | int       | static_cast      | volatile |
| do           | long      | struct           | wchar_t  |
| double       | mutable   | switch           | while    |
| dynamic_cast | namespace | templat          |          |

完整关键字介绍可查阅: [C++ 的关键字(保留字)完整介绍](https://www.runoob.com/w3cnote/cpp-keyword-intro.html)

### 运算符

逗号`,`可以用来连接两个表达式, 两个表达式依次计算. 例如

```c++
a=15,a*4;
```

`<`,`<=`,`>`, `>=`: 优先级高.
`==`,`!=`: 优先级低.

#### 逻辑运算符

`!`,`&&`,`||`

条件运算符:`表达式1 ? 表达式2 : 表达式3`, 优先级高于赋值运算符, 低于逻辑运算符.

```c++
x = a>b?a:b
```

### 三字符组

**三字符组**就是用于表示**另一个字符**的**三个字符序列**, 又称为**三字符序列**.
三字符序列总是以两个问号开头.
三字符序列不太常见, 但 `C++` 标准允许把某些字符指定为三字符序列.
以前为了表示键盘上没有的字符, 这是必不可少的一种方法.

三字符序列**可以出现在任何地方**, 包括`字符串`, `字符序列`, `注释`和`预处理指令`.
下面列出了最常用的三字符序列:

| 三字符组 | 替换 |
| -------- | ---- |
| `??=`    | `#`  |
| `??/`    | `\`  |
| `??'`    | `^`  |
| `??(`    | `[`  |
| `??)`    | `]`  |
| `??!`    | `\|`  |
| `??<`    | `{`  |
| `??>`    | `}`  |
| `??-`    | `~`  |

如果希望在源程序中有两个连续的问号, 且不希望被预处理器替换,
这种情况出现在字符常量, 字符串字面值或者是程序注释中,
可选办法是用字符串的自动连接: `"...?""?..."`
或者转义序列: `"...?\?..."`.

从`Microsoft Visual C++ 2010`版开始, 该编译器默认不再自动替换三字符组.
如果需要使用三字符组替换(如为了兼容古老的软件代码), 需要设置编译器命令行选项`/Zc:trigraphs`
`g++` 仍默认支持三字符组, 但会给出编译警告.

### C++ 中的空格

只包含空格的行, 被称为空白行, 可能带有注释, `C++`编译器会完全忽略它.

在 `C++` 中, 空格用于描述`空白符`, `制表符`, `换行符`和`注释`.
空格分隔语句的各个部分, 让编译器能识别语句中的某个元素(比如 `int`)在哪里结束,
下一个元素在哪里开始. 因此, 在下面的语句中:

```cpp
int age;
```

在这里, `int` 和 `age` 之间必须至少有一个`空格字符`(通常是一个`空白符`),
这样编译器才能够区分它们. 另一方面, 在下面的语句中:

```cpp
fruit = apples + oranges;   // 获取水果的总数
```

`fruit` 和 `=`, 或者 `=` 和 `apples` 之间的空格字符不是必需的,
但是为了增强可读性, 您可以根据需要适当增加一些空格.

## C++ 注释

程序的注释是解释性语句, 您可以在 `C++` 代码中包含注释, 这将提高源代码的可读性.
所有的编程语言都允许某种形式的注释.

`C++` 支持单行注释和多行注释. 注释中的所有字符会被 `C++` 编译器忽略.

`C++` 注释以 `/*` 开始, 以 `*/` 终止. 例如:

```cpp
/* 这是注释 */

/* C++ 注释也可以
 * 跨行
 */
```

注释也能以 `//` 开始, 直到行末为止. 例如:

实例

```cpp
#include <iostream>
using namespace std;

int main()
{
   cout << "Hello World"; // 输出 Hello World

   return 0;
}
```

当上面的代码被编译时, 编译器会忽略 `//` 输出 `Hello World`, 最后会产生以下结果:

```cpp
Hello World
```

在 `/*` 和 `*/` 注释内部, `//` 字符没有特殊的含义.
在 `//` 注释内, `/*` 和 `*/` 字符也没有特殊的含义.
因此, 您可以在一种注释内嵌套另一种注释. 例如:

```cpp
/* 用于输出 Hello World 的注释

cout << "Hello World"; // 输出 Hello World

*/
```

## 编译

如果是多个 `C++` 代码文件, 如 `runoob1.cpp, runoob2.cpp`, 编译命令如下:

```bash
g++ runoob1.cpp runoob2.cpp -o runoob
```

`g++` 有些系统默认是使用 `C++98`, 我们可以指定使用 `C++11` 来编译 `main.cpp` 文件:

```bash
g++ -g -Wall -std=c++11 main.cpp
```

## 二进制数字和数字分隔符, 引号

[ISO C++ 14 重点介绍](https://www.cnblogs.com/harlanc/p/6596016.html)

C++ 程序员现在可以创建一个二进制数字, 向已经包含十进制, 十六进制以及很少使用的八进制的标准中又添加了一员.
二进制数字使用前缀 `0b` 后面紧接数字.
在美国和英国, 我们使用逗号来作为数字分隔符, 如: `$1,000,000`.
这种写法真正方便了读者, 使得我们的大脑处理很长的数字时更加容易.
因为同样的原因C++标准委员会添加了数字分隔符.
它们不影响数值, 只是通过分块让数字的读写更加容易.

数字分隔符使用什么字符?
在C++中基本上每个标点符号都被特定的特性使用了, 因此没有很明显的选择.
最后的选择是使用单引号字符, 使得C++的百万数表示如下: `1'000'000.00`.
注意 `分隔符` 对数值没有任何影响, 因此百万数也可表示如下:  `1'0'00'0'00.00`.

下面的例子使用了两个新特性:

```cpp
#include <iostream>

int main()
{
    int val = 0b11110000;
    std::cout << "Output mask: " << 0b1000'0001'1000'0000 << "\n";
    std::cout << "Proposed salary: $" << 300'000.00 << "\n";
    return 0;
}
```

结果也是你所意料的:

```bash
Output mask: 33152
Proposed salary: $300000
```

## 变量模板

`变量模板` 在变量上对模板的扩展. 总会使用到的例子是变量 `pi<T>` 的一个实现.
当实现为 `double` 的时候, 变量会返回 `3.14`, 当实现为 `int` 时, 它可能返回 `3`,
当实现为 `string` 时, 可能返回"3.14"或者"pi", 这是个很棒的特性, 以前是在 `<limits>` 中实现的.

变量模板的语法和语义和类模板是基本相同的——你无需额外的学习就能使用它们.

对constexpr函数的限制放松了, 例如, 可以有多个返回值, 可以在内部使用case和if语句, 可以用循环以及其它.
这就对能在编译器做的事进行了扩展, 为模板的引入插上了翅膀.
其他小的特性包括为内存分配指定大小(sized deallocations)和一些语法的整理(tidying).

## c 语言 dollar sign, 美元符号

[does-c11-allow-dollar-signs-in-identifiers](https://stackoverflow.com/questions/26301737/does-c11-allow-dollar-signs-in-identifiers)

这是实现定义的行为, `$` 不包含在标识符语法中.
C++11 中标识符名称的规则是

+ 不能以数字开头
+ 可以由字母, 数字, 下划线, 通用字符名和 `实现定义的字符` 组成
+ 不能是[关键字](http://en.cppreference.com/w/cpp/keyword)

允许使用实现定义的字符, 许多编译器
(包括 gcc, clang, Visual Studio 和 DEC C++ 编译器)都支持这种扩展.

[C++ 标准草案](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2012/n3485.pdf)
第 2.11 节 `标识符` 涵盖了该语法, 我添加了以 <- 开头的附加说明:

```c
identifier:
  identifier-nondigit            <- Can only start with a non-digit
  identifier identifier-nondigit <- Next two rules allows for subsequent
  identifier digit               <-  characters to be those outlined in 2 above
identifier-nondigit:
  nondigit                       <- a-z, A-Z and _
  universal-character-name
  other implementation-defined characters
[...]
```

如果我们使用带有 `-pedantic-errors` 标志的 clang 来编译这段代码,
它将无法编译:

```c
int $ = 0
```

并产生以下错误:

```bash
error: '$' in identifier [-Werror,-Wdollar-in-identifier-extension]
int $ = 0;
    ^
```

## c++ 函数修饰符总结

[C++函数修饰符总结](https://blog.csdn.net/qq_24649627/article/details/110953124)

按修饰符的位置分为函数名前与函数名后两种, 以下分别做介绍.

### 函数名前

+ `返回值类型`, 是C++中定义函数的必备部分,
这些修饰符包括 `void`, `(unsigned) int`, `bool` 等内置基本数据类型和自定义类型,
也包括修饰返回值 `const` 关键字 (如 `const int*`), 还包括C++11中新增的类型自动推导 `auto` 关键字.
+ `template` 此关键字 声明函数是模板函数.
+ `virtual` 此关键字 声明函数是虚函数, 可被子类覆盖.
+ `inline` 此关键字 提示编译器应将函数内联.
+ `static`, static修饰类的成员函数时指示函数是静态成员函数, 不从属于具体对象;
修饰单独的函数时, 限定函数的可见范围为本文件内.

+ `extern` 此关键字声明一个定义在外部的函数
+ `explicit` 此关键字在C++11新增, 只用于构造函数, 指定构造函数要显式定义, 不能隐式转换.
+ `friend` 此关键字声明类的友元函数, 在函数内可直接访问对象的私有或受保护成员及成员函数.
+ `constexpr` C++11中新增, 指示函数返回常量表达式(可以简单理解为返回字面量).

### 函数名后

置于函数名后的修饰符有两种, 第一种使用等号(`=`), 第二种直接修饰.

#### 使用=

此类较为简单, 总共有三种:

+ `=0`: 只能用于虚函数, 表示函数为纯虚函数.
+ `=default`: C++11新增, 只能用于编译器提供默认实现的特殊成员函数, 指示使用默认实现.
+ `=delete`: C++11新增, 只能用于编译器提供默认实现的特殊成员函数, 指示编译器应该删除该函数的默认实现.

#### 直接修饰

种类繁多, 本人已知的修饰符包括:

+ `const`; 表示函数不会修改对象(或者说调用期间对象不变), 注意不包括 `mutable` 修饰的成员变量.

+ `volatile`; 类似于const修饰的函数, 表示对象状态可能随时会改变;
const修饰的函数内只能调用自身的const成员方法, 同理volatile函数内也只能调用自身volatile成员函数.

+ `&`; C++11引入的功能, 左值引用限定符, 指示函数只能被左值对象调用.
+ `&&`; C++11引入, 右值引用限定符, 指示函数只能被右值调用.
如果函数没有引用限定符修饰, 左值和右值均可调用. 一个引用限定例子:

```cpp
 #include <iostream>
  struct S {
    void f() & { std::cout << "lvalue\n"; }
    void f() &&{ std::cout << "rvalue\n"; }
  };

  int main(){
    S s;
    s.f();            // 打印" lvalue "
    std::move(s).f(); // 打印" rvalue "
    S().f();          // 打印" rvalue "
  }
```

+ `override`; C++11引入的功能, 声明成员函数覆盖父类的虚函数.
声明为override后, 子类声明时可不写virtual.
+ `final`; C++11引入, 指示函数是最终实现, 子类不应当再定义或覆盖, 可与override同时使用.
+ `noexcept`; C++11引入, 修饰函数是否会抛出异常.
+ `throw`; 指示函数抛出异常及类型, C++11起被废弃.

>总结
>需要注意的是, 严格来说上述内容中并不全是修饰符, 部分仅是语法结构.
