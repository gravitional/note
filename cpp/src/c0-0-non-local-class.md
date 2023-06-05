# local & nonlocal class

[Friend declaration](https://en.cppreference.com/w/cpp/language/friend)
[non-local class definitions](https://stackoverflow.com/questions/38280711/friend-function-in-class-definition-only-allowed-in-non-local-class-definitions)

[C++中外部函数如何访问私有变量?](https://www.zhihu.com/question/521898260)
[WhoTFAmI的回答](https://www.zhihu.com/question/521898260/answer/2394522797)

```cpp
#include <iostream>

class t80_Bank
{
private:
    int money = 999'999'999;

public:
    void check() { std::cout << "check:" << money << "\n"; }
};

template <typename Bank, typename Money, Money Bank::*p>
class t80_Thief
{
public:
    friend Money& t80_steal(Bank& bank) { return bank.*p; }
};

// 显式实例化类模板 Thief, 编译器生成模板类 Thief
// https://cppinsights.io/s/0f7828b7
template class t80_Thief<t80_Bank, int, &t80_Bank::money>;
int& t80_steal(t80_Bank&);

int test80()
{
    t80_Bank bank;
    bank.check();  // 999999999
    t80_steal(bank) -= 123456789;
    bank.check();  // 876543210
    return 0;
}
```
