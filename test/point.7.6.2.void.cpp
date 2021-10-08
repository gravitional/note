#include <iostream>
using namespace std;

int main()
{
    //!void voidObject ; 错，不能声明 void 类型的变量，编译器无法处理.
    void *pv; // 可以声明 void 类型的指针
    int i = 5;
    pv = &i;                            // void 类型指针指向整型变量.
    int *pint = static_cast<int *>(pv); // void 指针转换为 int 指针
    cout << "*pint= " << *pint << endl;
    return 0;
}