#include <iostream>
#include <string_view>

template <class Derived>
struct Base1
{
    void name()
    {
        (static_cast<Derived *>(this))->impl();
    }
};
struct D1 : public Base1<D1>
{
    void impl()
    {
        std::cout << "D1::impl()\n";
    }
};
struct D2 : public Base1<D2>
{
    void impl()
    {
        std::cout << "D2::impl()\n";
    }
};

//--------------------------- example2
template <typename Derived>
class Base2
{
public:
    void PrintType() const
    {
        std::string_view name = typeid(Derived).name();
        std::cout << name.substr(1, name.size() - 1) << "\n";
    }
};

class Derived1 : public Base2<Derived1>
{
};
class Derived2 : public Base2<Derived2>
{
};

int main()
{
    //---------- example 1
    Base1<D1> b1;
    b1.name();
    Base1<D2> b2;
    b2.name();

    D1 d1;
    d1.name();
    D2 d2;
    d2.name();
    std::cout << "===================================" << std::endl;
    //---------- example 2
    Derived1 derived1;
    Derived2 derived2;

    derived1.PrintType();
    derived2.PrintType();
    return 0;
}