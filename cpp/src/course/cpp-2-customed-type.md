# 自定义类型

## 类型别名: 为已有类型另外命名

```cpp
typedef  已有类型名  新类型名表
```

例:

```cpp
typedef double Area, Volume;
typedef int Natural;
Natural i1,i2;
Area a;
Volume v;
```

```cpp
using  新类型名 = 已有类型名;
```

例:

```cpp
using Area = double;
using Volume = double;
```

### 枚举类型

定义方式: 将全部可取值一一列举出来.
语法形式:

```cpp
enum  枚举类型名  {变量值列表};
```

例:

```cpp
enum Weekday {SUN, MON, TUE, WED, THU, FRI, SAT};
```

默认情况下

`SUN=0, MON=1, TUE=2, ......, SAT=6`

### C++包含两种枚举类型

不限定作用域枚举类型:

```cppp
enum  枚举类型名  {变量值列表};
```

限定作用域的enum类将在第4章介绍.

不限定作用域枚举类型说明:
枚举元素是常量, 不能对它们赋值, 例如有如下定义

```cpp
enum Weekday {SUN, MON, TUE, WED, THU, FRI, SAT};
```

+ 不能写赋值表达式: `SUN = 0`
枚举元素具有默认值, 它们依次为:  `0`, `1`, `2`, ....
+ 也可以在声明时另行指定枚举元素的值, 如:

```cpp
enum Weekday{SUN=7,MON=1,TUE,WED, THU,FRI,SAT};
```

+ 枚举值可以进行关系运算.

+ 整数值不能直接赋给枚举变量,
如需要将整数赋值给枚举变量, 应进行强制类型转换.

### 枚举值可以赋给整型变量

例2-11
设某次体育比赛的结果有四种可能:
胜(WIN), 负(LOSE), 平局(TIE), 比赛取消(CANCEL), 编写程序顺序输出这四种情况.

分析: 比赛结果只有四种可能, 可以声明一个枚举类型.

```cpp
#include <iostream>
using namespace std;
enum GameResult {WIN, LOSE, TIE, CANCEL};

int main() {
    GameResult result;
    enum GameResult omit = CANCEL;
    for (int count = WIN; count <= CANCEL; count++) {
        result = GameResult(count);
        if (result == omit)
        cout << "The game was cancelled" << endl;
        else {
            cout << "The game was played ";
            if (result == WIN)      cout << "and we won!";
            if (result == LOSE)       cout << "and we lost.";
            cout << endl;
            }
    }
    return 0;
}
```

### `auto` 类型与 `decltype` 类型

`auto`: 编译器通过初始值自动推断变量的类型
例如: auto val = val1 + val2;

+ 如果 val1+val2 是int类型, 则val是int类型;
+ 如果 val1+val2 是double类型, 则val是double类型.

decltype: 定义一个变量与某一表达式的类型相同, 但并不用该表达式初始化变量
例如:

```cpp
decltype(i) j = 2;
```
