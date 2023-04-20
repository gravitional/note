#include <iostream>

#define prt(V) std::cout << #V << " = " << V << "\n";

int main()
{
    prt(94500);
    prt(94'500);
    prt(9'4500);
    prt(0123);
    prt(0x1f3a);
    prt(0x1FFF'536B);
    prt(0b1100'1101'0001'1011);
}