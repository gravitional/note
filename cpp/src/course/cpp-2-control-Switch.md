# switch语句

语法形式

```cpp
switch  (表达式){
    case    常量表达式 1: 语句1
    case   常量表达式 2: 语句2
    ┆
    case   常量表达式 n: 语句n
    default :             语句n+1
}
```

执行顺序;
以case中的常量表达式值为入口标号, 由此开始顺序执行.
因此, 每个case分支最后应该加break语句.

+ 注意

+ `case分支` 可包含多个语句, 且不用 `{ }`.
+ 表达式, 判断值都是 `int型` 或 `char型`.
+ 如果若干分支执行内容相同, 可共用一组语句.

例2-4: 输入一个 `0~6` 的整数, 转换成星期输出

```cpp
#include <iostream>
using namespace std;

int main() {
    int day;
    cin >> day;
    switch (day) {
        case 0: cout << "Sunday" << endl; break;
        case 1: cout << "Monday" << endl; break;
        case 2: cout << "Tuesday" << endl; break;
        case 3: cout << "Wednesday" << endl; break;
        case 4: cout << "Thursday" << endl; break;
        case 5: cout << "Friday" << endl; break;
        case 6: cout << "Saturday" << endl; break;
        default:
        cout<<"Day out of range Sunday .. Saturday"<< endl;   break;
    }
    return 0;
}
```
