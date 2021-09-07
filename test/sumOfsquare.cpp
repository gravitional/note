#include <iostream>
#include <iomanip>
using namespace std;

int sumOfsquare(int a, int b)
{
    return a * a + b * b;
}
double sumOfsquare(double a, double b)
{
    return a * a + b * b;
}

int main()
{
    int m, n;
    cout << "Enter two integers: ";
    cin >> m >> n;
    cout << "Their sum of square: " << sumOfsquare(m, n) << endl;
    double x, y;
    cout << "Enter two real number: ";
    cin >> x >> y;
    cout << "Their sum of square: " << sumOfsquare(x, y) << endl;
    return 0;
}