#include <iostream>
#include <string>
using namespace std;
void TrueOrFalse(int x)
{
    cout << (x ? "True" : "False") << endl;
}
int main()
{
    string S1 = "DEF";
    string CP1 = "ABC";
    string CP2 = "DEF";
    string CP3 = "DEFG";
    string CP4 = "def";
    cout << "S1= " << S1 << endl;
    cout << "CP1 = " << CP1 << endl;
    cout << "CP2 = " << CP2 << endl;
    cout << "CP3 = " << CP3 << endl;
    cout << "CP4 = " << CP4 << endl;
    cout << "S1 <= CP1 returned ";
    TrueOrFalse(S1 <= CP1);
    cout << "S1 <= CP2 returned ";
    TrueOrFalse(S1 <= CP2);
    cout << "S1 <= CP3 returned ";
    TrueOrFalse(S1 <= CP3);
    cout << "CP1 <= S1 returned ";
    TrueOrFalse(CP1 <= S1);
    cout << "CP2 <= S1 returned ";
    TrueOrFalse(CP2 <= S1);
    cout << "CP4 <= S1 returned ";
    TrueOrFalse(CP4 <= S1);
    cin.get();
    return 0;
}