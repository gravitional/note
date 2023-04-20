
#include <iostream>

#define prt(VALUE) std::cout << #VALUE " is :" << VALUE << "\n";

int main()
{
    bool b = true;
    prt(true);
    prt(false);
    prt(sizeof(bool));
}