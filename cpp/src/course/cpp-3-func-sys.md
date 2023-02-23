# C++系统函数

## 系统函数

C++的系统库中提供了几百个函数可供程序员使用, 例如:

+ 求平方根函数(sqrt)
+ 求绝对值函数(abs)
+ 使用系统函数时要包含相应的头文件, 例如: `cmath`

## 例3-17 系统函数应用举例

题目: 从键盘输入一个角度值, 求出该角度的正弦值, 余弦值和正切值.

分析: 系统函数中提供了求正弦值, 余弦值和正切值的函数: sin(), cos(), tan(), 函数的说明在头文件 `cmath` 中.
源代码

```cpp
#include <iostream>
#include <cmath>
using namespace std;

const double PI = 3.14159265358979;

int main() {
      double angle;
      cout << "Please enter an angle: ";
      cin >> angle;  //输入角度值
      double radian = angle * PI / 180; //转为弧度
      cout << "sin(" << angle << ") = " << sin(radian) <<endl;
      cout << "cos(" << angle << ") = " << cos(radian) <<endl;
      cout << "tan(" << angle << ") = " << tan(radian) <<endl;
      return 0;
}
```

运行结果

```log
30
sin(30)=0.5
cos(30)=0.866025
tan(30)=0.57735
```
