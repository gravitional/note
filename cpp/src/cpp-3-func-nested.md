# 函数的嵌套调用

嵌套调用

![nested](http://studio-tsinghua.xuetangx.com/asset-v1:TsinghuaX+00740043-91-20202+2020_T2+type@asset+block@image001qiantao.jpg)

例3-7 输入两个整数, 求平方和

```cpp
#include <iostream>
using namespace std;
int fun2(int m) {
    return m * m;
}
int fun1(int x,int y) {
    return fun2(x) + fun2(y);
}
int main() {
    int a, b;
    cout<<"Please enter two integers (a and b): ";
    cin >> a >> b;
    cout << "The sum of square of a and  b: "
         << fun1(a, b) << endl;
    return 0;
}
```
