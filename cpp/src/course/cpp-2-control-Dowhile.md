# do-while语句

## `do-while` 语句的语法形式

```cpp
do   语句     // 可以是复合语句, 其中必须含有改变条件表达式值的语句.

while (表达式)
```

执行顺序

+ 先执行循环体语句, 后判断条件.
+ 表达式为 `true` 时, 继续执行循环体.

例2-6: 输入一个数, 将各位数字翻转后输出

```cpp
#include <iostream>

using namespace std;
int main() {

    int n, right_digit, newnum = 0;
    cout << "Enter the number: ";
    cin >> n;
    cout << "The number in reverse order is  ";
    do {
         right_digit = n % 10;
         cout << right_digit;
         n /= 10;  /*相当于n=n/10*/
    } while (n != 0);
    cout << endl;
    return 0;
}
```

## 例2-7 用 `do-while` 语句编程, 求自然数1~10之和

```cpp
#include <iostream>
using namespace std;

int main() {
      int i = 1, sum = 0;
      do {
           sum += i;
           i++;
      } while (i <= 10);
      cout << "sum = " << sum << endl;
      return 0;
}
```

对比下面的程序, 程序1:

```cpp
#include <iostream>
using namespace std;

int main() {
  int i, sum = 0;
  cin >> i;
  while (i <= 10) {
    sum += i;
    i++;
  }
  cout<< "sum= " << sum << endl;
  return 0;
}
```

程序2:

```cpp
#include <iostream>
using namespace std;

int main() {
  int i, sum = 0;
  cin >> i;
  do {
    sum += i;
    i++;
  } while (i <= 10);
  cout << "sum=" << sum<< endl;
  return 0;
}
```
