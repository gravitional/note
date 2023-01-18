#include <iostream>
#include <string>
using namespace std;
int main()
{
    string A("aBcdef");
    string B("AbcdEf");
    string C("123456");
    string D("123dfg");
    // 下面是各种比较方法
    int m = A.compare(B);             // 完整的A和B的比较
    int n = A.compare(1, 5, B, 4, 2); //"Bcdef"和"AbcdEf"比较
    int p = A.compare(1, 5, B, 4, 2); //"Bcdef"和"Ef"比较
    int q = C.compare(0, 3, D, 0, 3); //"123"和"123"比较
    cout << "m = " << m << ", n = " << n << ", p = " << p << ", q = " << q << endl;
    cin.get();
    return 0;
}