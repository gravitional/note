#include <iostream>
using namespace std;

class IntNum
{
public:
    IntNum(int x = 0) : xptr(new int(x))
    { //构造函数
        cout << "Calling constructor..." << endl;
    }
    IntNum(const IntNum &n) : xptr(new int(*n.xptr)) // * n. xptr 即 n 的指针 对应的值
    {                                                //复制构造函数
        cout << "Calling copy constructor..." << endl;
    }
    ~IntNum()
    { //析构函数
        delete xptr;
        cout << "Destrcuting... " << endl;
    }
    int getInt() { return *xptr; }

private:
    int *xptr;
};

// 返回值为 IntNum 类对象
IntNum getNum()
{
    IntNum a;
    return a;
}

int main()
{
    cout << getNum().getInt() << endl;
    return 0;
}