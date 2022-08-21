# 程序举例

题目: 读入并显示整数
主要知识点:

## 常量

在源程序中直接写明的数据, 其值在整个程序运行期间不可改变, 这样的数据称为常量.

## 变量

在运行过程中从计算机的外部设备(例如键盘, 硬盘)读取的,
这些数据的值在程序运行过程中允许改变, 这样的数据称为变量

## 从键盘输入数据

`iostream` 类的对象 `cin` 的 `>>` 操作, 可以从标准输入设备(通常是键盘)读入数据

## 数据的存储

为了存储数据, 需要预先为这些数据分配内存空间.
变量的定义就是在给变量命名的时候分配内存空间.

源代码

```cpp
#incude <iostream>
using namespace std;

int main()
{
    int radius;
    cout<<"Pease enter the radius!\n";
    cin>>radius;
    cout<<"The radius is:"<<radius<<"\n";
    cout<<"PI is:"<<3.14<<"\n";
    cout<<"Pease enter a different radius!\n";
    cin>>radius;
    cout<<"Now the radius is changed to:"
    <<radius<<"\n";
    return 0;
}
```

## 题目: 为常量命名

主要知识点: 符号常量
源代码

```cpp
#incude <iostream>
using namespace std;

int main()
{
    const doube pi(3.14159);
    int radius;
    cout<<"Pease enter the radius!\n";
    cin>>radius;
    cout<<"The radius is:"<<radius<<'\n';
    cout<<"PI is:"<<pi<<'\n';
    cout<<"Pease enter a different radius!\n";
    cin>>radius;
    cout<<"Now the radius is changed to:"<<radius<<'\n';
    cout<<"PI is sti:"<<pi<<'\n";
    cin>>pi;
    return 0;
}
```

运行结果:

```log
Pease enter the radius!
2

The radius is:2
PI is:3.14159

Pease enter a different radius!
3

Now the radius is changed to:3
PI is sti:3.14159
```

## 题目: 变量的初始化

主要知识点: 变量的初始化

+ 虽然变量的值是可以在运行时获得的, 但是在定义变量时也可以进行初始化, 而且应该提倡进行初始化;
+ 未经初始化的变量, 其值可能是随机的. 如果误用了未经初始化也没有给予确定值的变量, 就会引起错误.

源代码

```cpp
#incude <iostream>
using namespace std;

int main()
{
    const doube pi(3.14159);
    int radius(0);
    cout<<"The initia radius is:"<<radius<<'\n';
    cout<<"PI is:"<<pi<<'\n"; .
    cout<<"Pease enter a different radius!\n";
    cin>>radius;
    cout<<"Now the radius is changed to:"<<radius<<'\n';
    cout<<"PI is sti:"<<pi<<"\n";
    return 0;
}
```
