# for语句

+ for语句语法形式:

```cpp
for (size_t i=0;i<n; i++){
    ...
}
```

+ for语句的另一种形式: `范围for` 语句:

```cpp
for  (声明: 表达式)
    语句
```

例2-8: 输入一个整数, 求出它的所有因子

```cpp
#include <iostream>
using namespace std;

int main() {
    int n;
    cout << "Enter a positive integer: ";
    cin >> n;
    cout << "Number  " << n << "   Factors  ";
    for (int k = 1; k <= n; k++)
      if (n % k == 0)
        cout << k << "  ";
    cout << endl;
    return 0;
}
```

运行结果1:

```log
Enter a positive integer: 36
Number  36  Factors  1  2  3  4  6  9  12  18  36
```

运行结果2:

```log
Enter a positive integer: 7
Number  7   Factors  1  7
```
