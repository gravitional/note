# 内联函数

声明时使用关键字 inline.

编译时在调用处用函数体进行替换, 节省了参数传递, 控制转移等开销.

注意:

内联函数体内不能有循环语句和switch语句;
内联函数的定义必须出现在内联函数第一次被调用之前;
对内联函数不能进行异常接口声明.

例3-14  内联函数应用举例

```cpp
#include <iostream>

using namespace std;

const double PI = 3.14159265358979;

inline double calArea(double radius) {

          return PI * radius * radius;

}

int main() {

          double r = 3.0;

          double area = calArea(r);

          cout << area << endl;

          return 0;

}
```

## constexpr函数

constexpr函数语法规定

constexpr修饰的函数在其所有参数都是constexpr时, 一定返回constexpr;

函数体中必须有且仅有一条return语句.
constexpr函数举例

```cpp
constexpr int get_size() { return 20; }
constexpr int foo = get_size();  //正确: foo是一个常量表达式
```
