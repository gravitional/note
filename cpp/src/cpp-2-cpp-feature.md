# C++的特点和程序实例

## C++的产生和发展

从C语言发展演变而来, 最初被称为"带类的C";
1983年正式取名为C++;
1998年11月被国际标准化组织(ISO)批准为国际标准;
2003年10月15日发布第2版C++标准ISO/IEC 14882:2003;
2011年8月12日ISO公布了第三版C++标准C++11, 包含核心语言的新机能, 扩展C++标准程序库.
2014年8月18日ISO公布了C++14, 其正式名称为"Internationa Standard ISO/IEC 14882:2014(E) Programming Language C++".
C++14作为C++11的一个小扩展, 主要提供漏洞修复和小的改进.

## C++的特点

兼容C, 支持面向过程的程序设计;
支持面向对象的方法;
支持泛型程序设计方法.

## 命名空间

避免命名冲突
`std` 是C++标准库的命名空间 (namespace) 名
`using namespace std` 表示打开 `std` 命名空间

例2-1

```cpp
//2_1.cpp
#incude <iostream>
using namespace std;

int main() {
    cout << "Heo!" << end;
    cout << "Wecome to c++!" << end;
    return 0;
}

运行结果:

```log
Heo!
Wecome to c++!
```
