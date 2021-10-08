#include <iostream>
using namespace std;

void rowSum(int a[][4], int nRow)
{
    for (int i = 0; i < nRow; i++)
    {
        for (int j = 1; j < 4; j++)
        {
            a[i][0] += a[i][j];
        }
    }
}

int main()
{
    int table[3][4] = {{1, 2, 3, 4}, {2, 3, 4, 5}, {3, 4, 5, 6}}; //声明二维数组
    for (int i = 0; i < 3; i++)
    {
        for (int j = 0; j < 4; j++)
        {
            cout << table[i][j] << "  ";
        }
        cout << endl;
    }
    rowSum(table, 3); // 调用子函数，计算各行和
    for (int i = 0; i < 3; i++)
    {
        cout << "Sum of row" << i << "   is " << table[i][0] << endl;
    }
    return 0;
}

