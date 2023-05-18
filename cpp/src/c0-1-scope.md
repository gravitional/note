# c++ 作用域

[c++作用域解析运算符(::)](https://blog.csdn.net/nicai_xiaoqinxi/article/details/88081069)

`::函数`或 类型调用, 表示调用全局的函数或类型;
`::value` 引用全局变量; 例:

```cpp
#include <iostream>
using namespace std;

int value = 1;
int main(int argc, char *argv[])
{
    int value = 10;

    cout<<::value<<endl; /* output: 1 */
    cout<<value<<endl;   /* output: 10 */
    return 0;
}
```
