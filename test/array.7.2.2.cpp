#include <iostream>
using namespace std;

int main()
{
    int f[20] = {1, 1};          // 初始化第0，1个数
    for (int i = 0; i < 20; i++) // 求第 2～19个数
    {
        f[i] = f[i - 2] + f[i - 1];
    }
    for (int i = 0; i < 20; i++) //输出，每行5个数
    {
        if (i % 5 == 0)
        {
            cout << endl;
        }
        cout.width(12); //设置输出宽度为 12
        cout << f[i];
    }
    return 0;
}
