# 带默认参数值的函数

默认参数值

可以预先设置默认的参数值, 调用时如给出实参, 则采用实参值, 否则采用预先设置的默认参数值.

例:

int add(int x = 5,int y = 6) {

     return x + y;

}

int main() {

     add(10,20);  //10+20

     add(10);     //10+6

     add();       //5+6

}

默认参数值的说明次序

有默认参数的形参必须列在形参列表的最右, 即默认参数值的右面不能有无默认值的参数;

调用时实参与形参的结合次序是从左向右.

例:

int add(int x, int y = 5, int z = 6);//正确

int add(int x = 1, int y = 5, int z);//错误

int add(int x = 1, int y, int z = 6);//错误
默认参数值与函数的调用位置

如果一个函数有原型声明, 且原型声明在定义之前, 则默认参数值应在函数原型声明中给出; 如果只有函数的定义, 或函数定义在前, 则默认参数值可以函数定义中给出.

例:

## 例3-15计算长方体的体积

    函数getVolume计算体积

        有三个形参: length(长), width(宽), height(高), 其中width和height带有默认值2和3.

    主函数中以不同形式调用getVolume函数.

源代码

//3_15.cpp

#include <iostream>

#include <iomanip>

using namespace std;

int getVolume(int length, int width = 2, int height = 3);

int main() {

const int X = 10, Y = 12, Z = 15;

cout << "Some box data is " ;

cout << getVolume(X, Y, Z) << endl;

cout << "Some box data is " ;

cout << getVolume(X, Y) << endl;

cout << "Some box data is " ;

cout << getVolume(X) << endl;

return 0;

}

int getVolume(int length, int width, int height) {

cout << setw(5) << length << setw(5) << width << setw(5)

<< height << '\t';

return length * width * height;

}