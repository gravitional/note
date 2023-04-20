#include <iostream>
#include <limits>

#define prt(V) std::cout << #V " = " << V << "\n";
#define prt2(M, V) std::cout << M " = " << V << "\n";

int main()
{
    prt(sizeof(short));
    prt(sizeof(int));
    prt(sizeof(long));
    prt(sizeof(int *));
    prt(sizeof(long long));

    prt2("lowest of short int", std::numeric_limits<short>::lowest());
    prt2("lowest of int", std::numeric_limits<int>::lowest());
    prt2("lowest of long int", std::numeric_limits<long>::lowest());
    prt2("lowest of long long int", std::numeric_limits<long long>::lowest());

    prt2("lowest of unsigned short int", std::numeric_limits<unsigned short>::lowest());
    prt2("lowest of unsigned int", std::numeric_limits<unsigned int>::lowest());
    prt2("lowest of unsigned long int", std::numeric_limits<unsigned long>::lowest());
    prt2("lowest of unsigned long long int", std::numeric_limits<unsigned long long>::lowest());

    prt2("max of short int", std::numeric_limits<short>::max());
    prt2("max of int", std::numeric_limits<int>::max());
    prt2("max of long int", std::numeric_limits<long>::max());
    prt2("max of long long int", std::numeric_limits<long long>::max());

    prt2("max of unsigned short int", std::numeric_limits<unsigned short>::max());
    prt2("max of unsigned int", std::numeric_limits<unsigned int>::max());
    prt2("max of unsigned long int", std::numeric_limits<unsigned long>::max());
    prt2("max of unsigned long long int", std::numeric_limits<unsigned long long>::max());
}