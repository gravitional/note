#include <iostream>
using namespace std;

int main(){
    enum days{one, two, three}day;
    day = one;
    switch(day){
        case one:
            cout << "wo cccc" << endl;
            break;
        case two:
            cout << "two" << endl;
            break;
        default:
            cout << "three" << endl;
            break;
    }
    return 0;
}