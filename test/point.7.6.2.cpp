#include <iostream>
using namespace std;

int main()
{
    int i;                             // 定义 int 型数 i，非静态局部变量。
    int *ptr = &i;                     // 取 i 的地址赋给 ptr
    i = 10;                            // int 型数 赋初值
    cout << "i=" << i << endl;         //输出int 型 数的内容
    cout << "*ptr = " << *ptr << endl; //输出int型指针所指地址的内容
    return 0;
}