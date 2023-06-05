# c, c++ 字符串

## C, C++中的单引号和双引号

[C, C++中的单引号和双引号](https://blog.csdn.net/SlowIsFastLemon/article/details/103664581)

C, C++中的单引号和双引号作用如下:

+ `单引号` 用来表示 `字符字面量`,  `单引号` 括起来的 `单个字符` 代表 `整数`.
+ `双引号` 用来表示 `字符串字面量`, `双引号` 括起来的 `若干字符` 代表 `字符指针`.

比如:

`'a'` 表示字符字面量, 在内存中占1个字节
`'a'+1` 表示 'a' 的 `ASCII 码+1`, 结果为 'b'

`"a"` 表示字符串字面量, 在内存中占 `2` 个字节.
`"a"+1` 表示指针运算, 结果指向 "a" 结束符 '\0'.

### 单引号和双引号的错误使用举例

#### 举例一: 单引号和双引号的本质

如下程序片段合法吗?

```cpp
#include <stdio.h>

int main()
{

    char* p1 =  1 ;
    char* p2 = '1';
    char* p3 = "1";

    printf("%s, %s, %s", p1, p2, p3); // 段错误

    printf('\n');      // 段错误
    printf("\n");

    return 0;
}
```

为了解释为何会出现段错误, 我们需要知道如下知识:

+ `字符字面量` 被编译为对应的 ASCII码.
+ `字符串字面量` 被编译为对应的 内存地址.
+ `printf` 的第一个参数被当成 `字符串内存地址`.
+ 内存的低地址空间 不能在程序中随意访问.

我们可以得到如下的内存图, 段错误的原因也就显而易见了:

![img1](https://img-blog.csdnimg.cn/20191223142139771.png)
![img2](https://img-blog.csdnimg.cn/2019122314220066.png)

#### 举例二: 混淆概念的代码

```cpp
# include <stdio.h>

int main()
{

    char c = " ";

    while( (c == "\t") || (c == " ") || (c == "\n") )
    {
        scanf("%c", &c);
    }

    return 0;
}
// 运行结果: 程序直接执行结束, 不会接收用户的输入.
```

为了理解上述运行结果, 我们需要运行结果,
我们需要知道 `char c = "string"` 发生了什么. 分析:

+ 编译后字符串 `"string"` 的内存地址被赋值给变量c.
+ 内存地址占用4个字节, 而变量 `c` 只占用1个字节.
+ 由于类型不同, 赋值后产生截断.

注意:

+ C编译器接受字符和字符串的比较, 无任何意义.
+ C编译器允许字符串对字符变量赋值, 只能得到错误.

## C++ 字符串比较

[C++string字符串比较方法详解](http://c.biancheng.net/view/1447.html)

`字符串`可以和类型相同的 `字符串` 相比较, 也可以和具有同样字符类型的 `数组` 比较.

`Basic_string` 类模板既提供了  `>`, `<`, `==,` `>=,` `<=,` `!=` 等比较运算符,
还提供了 `compare()` 函数, 其中 `compare()` 函数支持多参数处理,
支持用 `索引值` 和 `长度` 定位子串进行比较.
该函数返回一个整数来表示比较结果. 如果相比较的两个子串相同,
`compare()` 函数返回 `0`, 否则返回 `非零值`.

## compare()函数

类 `basic_string` 的成员函数 `compare()` 的原型如下:

```cpp
int compare (const basic_string& s) const;
int compare (const Ch* p) const;
int compare (size_type pos, size_type n, const basic_string& s) const;
int compare (size_type pos, size_type n, const basic_string& s,size_type pos2, size_type n2) const;
int compare (size_type pos, size_type n, const Ch* p, size_type = npos) const;
```

如果在使用 `compare()` 函数时, 参数中出现了位置和大小, 比较时只能用指定的子串. 例如:

```cpp
s.compare {pos,n, s2);
```

若参与比较的两个串值`相同`, 则函数返回 `0`;
若 `字符串 S`  按字典顺序要先于 `S2`, 则返回负值;反之, 则返回 `正值`.
下面举例说明如何使用 `string类` 的 `compare()` 函数.

[string-compare1](../exa2-string-compare/compare1.cpp)

程序的执行结果为:

```bash
m = 1, n = -1, p = -1, q = 0
```

`string类`的比较 `compare()` 函数使用非常方便, 而且能区分字母的大小写.
建议读者多使用此函数.

## 比较运算符

`String类` 的常见运算符包括`>`, `<`, `==`,  `>=`, `<=`, `!=`.
其意义分别为"大于", "小于", "等于", "大于等于", "小于等于", "不等于".

比较运算符使用起来非常方便, 此处不再介绍其函数原型, 读者直接使用即可.
下面以例 2 进行说明.

[string-compare2](../exa2-string-compare/compare2.cpp)

程序运行结果为:

```cpp
S1= DEF
CP1 = ABC
CP2 = DEF
CP3 = DEFG
CP4 = def
S1 <= CP1 returned False
S1 <= CP2 returned True
S1 <= CP3 returned True
CP1 <= S1 returned True
CP2 <= S1 returned True
CP4 <= S1 returned False
```

由上述内容可知, 使用 `比较运算符` 可以非常容易地实现字符串的大小比较.
在使用时比较运算符时, 读者应注意, 对于参加比较的两个字符串,
任一个字符串均不能为 `NULL`, 否则程序会异常退出.

## std::string 初始化

[::basic_string](https://zh.cppreference.com/w/cpp/string/basic_string/basic_string)

重载(5)

```cpp
basic_string( const CharT* s, const Allocator& alloc = Allocator() ); //(C++20 前)
constexpr basic_string( const CharT* s, const Allocator& alloc = Allocator() ); //(C++20 起)
```

注意
以含内嵌 `\0` 字符的字符串字面量初始化 string 会使用重载 (5),
并在首个空字符停止.
这可通过指定不同的构造函数, 或通过使用 `operator""s` 避免:

```cpp
std::string s1 = "ab\0\0cd";   // s1 含 "ab"
std::string s2{"ab\0\0cd", 6}; // s2 含 "ab\0\0cd"
std::string s3 = "ab\0\0cd"s;  // s3 含 "ab\0\0cd"
```
