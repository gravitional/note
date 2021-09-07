#include <iostream>
using namespace std;

int i; //全局变量，文件作用域

int main()
{
    i = 5; //为全局变量赋值
    {
        int i; //局域变量，局部作用域。
        i = 7;
        cout << " i= " << i << endl; //输出7
    }
    cout << " i= " << i << endl; //输出5
    return 0;
}
