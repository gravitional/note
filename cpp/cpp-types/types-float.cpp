#include <iostream>
#include <cfloat>

#define prt(V) std::cout << #V " = " << V << "\n";
#define prt2(M, V) std::cout << M " = " << V << "\n";
#define line std::cout << "------------------\n";

int main()
{
    prt(sizeof(float));
    prt(sizeof(double));
    prt(sizeof(long double));
    line;
    prt2("float min", FLT_MIN);
    prt2("double min", DBL_MIN);
    prt2("long double min", LDBL_MIN);
    line;
    prt2("float max", FLT_MAX);
    prt2("double max", DBL_MAX);
    prt2("long double max", LDBL_MAX);
}