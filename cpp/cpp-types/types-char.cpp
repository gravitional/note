#include <iostream>
#include <climits>

#define prt(V) std::cout << #V " = " << V << "\n";
#define prt2(M, V) std::cout << M " = " << V << "\n";
#define line std::cout << "------------------\n";

int main()
{
    // char 类型的 bit 数目
    prt(CHAR_BIT);
    prt(CHAR_MIN);
    prt(CHAR_MAX);
    line;
    prt(sizeof(wchar_t));
    // prt(sizeof(char8_t)); // c++ 20标准 utf8
    prt(sizeof(char16_t));
    prt(sizeof(char32_t));
}