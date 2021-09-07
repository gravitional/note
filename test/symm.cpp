#include <iostream>
#include <math.h>

using namespace std;

bool symm(unsigned n);

int main()
{
    for (unsigned m = 11; m < 1000; m++)
        if (symm(m) && symm(m ^ 2) && symm(m ^ 3))
        {
            cout << "m=" << m << endl;
            cout << "m^2=" << m * m << endl;
            cout << "m^3=" << m * m * m << endl;
        };
        cout << sin(3.1415926/2.0)<<endl;
}

bool symm(unsigned n) // 判断是否为回文数
{
    unsigned i = n;
    unsigned m = 0;
    while (i > 0)
    {
        m = m * 10 + i % 10;
        i /= 10;
    }
    return m == n;
}