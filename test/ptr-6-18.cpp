#include <iostream>
using namespace std;

int main()
{
    int(*cp)[9][8] = new int[7][9][8];
    for (int i = 0; i < 7; i++)
        for (int j = 0; j < 7; j++)
            for (int k = 0; k < 7; k++)
                *(*(*(cp + i) + j) + k) = (i * 100 + j * 10 * k);

    for (int i = 0; i < 7; i++)
    {
        for (int j = 0; j < 7; j++)
        {
            for (int k = 0; k < 7; k++)
            {
                cout << cp[i][j][k] << " ";
            }
            cout << endl;
        }
        cout << endl;
    }
    delete[] cp;
    return 0;
}