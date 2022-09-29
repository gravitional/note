# 函数重载的概念

[函数重载](https://www.xuetangx.com/learn/THU08091000247/THU08091000247/10322314/video/17397882)

C++允许功能相近的函数在相同的作用域内以相同函数名声明, 从而形成重载.
方便使用, 便于记忆. 例:

注意事项

+ 重载函数的形参必须不同:个数不同或类型不同.
+ 编译程序将根据实参和形参的类型及个数的最佳匹配来选择调用哪一个函数.
+ 不要将不同功能的函数声明为重载函数, 以免出现调用结果的误解, 混淆. 这样不好:

## 例3-16重载函数应用举例

编写两个名为sumOfSquare的重载函数, 分别求两整数的平方和及两实数的平方和.

源代码:

```cpp
#include <iostream>
using namespace std;
int sumOfSquare(int a, int b) {
    return a * a + b * b;
}
double sumOfSquare(double a, double b) {
    return a * a + b * b;
}
int main() {
    int m, n;
    cout << "Enter two integer: ";
    cin >> m >> n;
    cout<<"Their sum of square: "<<sumOfSquare(m, n)<<endl;
    double x, y;
    cout << "Enter two real number: ";
    cin >> x >> y;
    cout<<"Their sum of square: "<<sumOfSquare(x, y)<<endl;
    return 0;
}
```

运行结果:

```log
Enter two integer: 3 5
Their sum of square: 34
Enter two real number: 2.3 5.8
Their sum of square: 38.93
```
