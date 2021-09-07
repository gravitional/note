#include <iostream>
using namespace std;

class R
{
public:
    R(int r1, int r2) : r1(r1), r2(r2){};
    void print();
    void print() const; // 是合法的重载形式

private:
    int r1, r2;
};

void R::print()
{
    cout << r1 << " : " << r2 << endl;
}

void R::print() const //承诺不改变对象状态，编译器会进行检查
{
    cout << " const " << r1 << " : " << r2 << endl;
}

int main()
{
    R a(5, 4);
    a.print();         // 调用 void print()
    const R b(20, 52); //常量对象，只能调用常函数
    b.print();         //调用 void print() const
    return 0;
}