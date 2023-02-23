# 引用的概念

[引用类型](https://www.xuetangx.com/learn/THU08091000247/THU08091000247/10322314/video/17397831)

引用(&)是标识符的别名;

    定义一个引用时, 必须同时对它进行初始化, 使它指向一个已存在的对象.

    例如:

    int i, j;

    int &ri = i; //定义int引用ri, 并初始化为变量i的引用

    j = 10;

    ri = j;  //相当于 i = j;

    一旦一个引用被初始化后, 就不能改为指向其它对象.

    引用可以作为形参

例3-11 输入两个整数并交换(值传递)

```cpp
#include<iostream>
using namespace std;
void swap(int a, int b) {
    int t = a;
    a = b;
    b = t;
}

int main() {
    int x = 5, y = 10;
    cout<<"x = "<<x<<"  y = "<<y<<endl;
    swap(x, y);
    cout<<"x = "<<x<<"  y = "<<y<<endl;
    return 0;
}

运行结果:
x = 5      y = 10
x = 5      y = 10
```

## 例3-11 参数传递示意图

例3-12 输入两个整数并交换(引用传递)

```cpp
#include<iostream>
using namespace std;

void swap(int& a, int& b) {
    int t = a;
    a = b;
    b = t;
}

int main() {
    int x = 5, y = 10;
    cout<<"x = "<<x<<"  y = "<<y<<endl;
    swap(x, y);
    cout<<"x = "<<x<<"  y = "<<y<< endl;
    return 0;
}
```
