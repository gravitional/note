#include <iostream>
#include <cmath>
using namespace std;

int amx1(int x, int y);
int amx1(int x, int y, int z);

int amx1(double x, double y);
int amx1(double x, double y, double z);

int max1(int x, int y) //两个数的最大值
{
    if (x == y)
        return x;
    else if (x >= y)
        return x;
    else
        return y;
}

int max1(int x, int y, int z) //三个数的最大值
{
    return max1(max1(x, y), z);
}

double max1(double x, double y)
{
    if (abs(x - y) < 1e-10)
        return x;
    else if (x >= y)
        return x;
    else
        return y;
}

double max1(double x, double y, double z)
{
    return max1(max1(x, y), z);
}

int main()
{
    int a, b, c;
    double x, y, z;

    cout << "Enter int a,b,c: ";
    cin >> a >> b >> c;
    cout << "max of them: \n"
         << max1(a, b, c)<<endl;

    cout << "Enter flaot x,y,z: ";
    cin >> x >> y >> z;
    cout << "max of them: \n"
         << max1(x, y, z)<<endl;
    return 0;
}