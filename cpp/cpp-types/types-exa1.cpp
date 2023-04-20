#include <iostream>
#include <type_traits>

#define prt(TYPE) std::cout << #TYPE " is: " << ((std::is_fundamental<TYPE>::value) ? " true.\n " : " false.\n");

class A
{
};

int main()
{
    prt(int);
    prt(int *);
    prt(int &);
    prt(float);
    prt(float *);
    prt(float &);
    prt(A);
}