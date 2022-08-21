# while语句

语法形式

```cpp
while  (表达式)  
    语句: 可以是复合语句, 其中必须含有改变条件表达式的语句.
```

执行顺序; 先判断表达式的值, 若为 true 时, 执行语句.

例2-5 求自然数 1~10 之和

```cpp
#include <iostream>
using namespace std;

int main() {
    int i = 1, sum = 0;
    while (i <= 10) {
        sum += i;  //相当于sum = sum + i;
        i++;
        }
    cout << "sum = " << sum << endl;
    return 0;
}
```
