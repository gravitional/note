# 前置单目运算符重载规则

    如果要重载 U 为类成员函数, 使之能够实现表达式 U oprd, 其中 oprd 为A类对象, 则 U 应被重载为 A 类的成员函数, 无形参.

    经重载后, 表达式 U oprd 相当于 oprd.operator U()

## 后置单目运算符 ++ 和 --重载规则

如果要重载 `++` 或 `--`为类成员函数, 使之能够实现表达式 `oprd++` 或 `oprd--`, 
其中 `oprd` 为 `A` 类对象, 则 `++` 或 `--` 应被重载为 A 类的成员函数, 且具有一个 `int` 类型形参.

经重载后, 表达式 `oprd++` 相当于 `oprd.operator ++(0)`

例8-2重载 `前置++` 和 `后置++` 为时钟类成员函数

+ 前置单目运算符, 重载函数没有形参
+ 后置++运算符, 重载函数需要有一个 `int形参`

操作数是时钟类的对象. 实现时间增加1秒钟.

源代码:

```cpp
#include <iostream>
 using namespace std;
 class Clock {//时钟类定义
 public:
     Clock(int hour = 0, int minute = 0, int second = 0);
     void showTime() const;
   //前置单目运算符重载
     Clock& operator ++ ();
   //后置单目运算符重载
     Clock operator ++ (int);
 private:
     int hour, minute, second;
 };

Clock::Clock(int hour, int minute, int second) {
    if (0 <= hour && hour < 24 && 0 <= minute && minute < 60
        && 0 <= second && second < 60) {
        this->hour = hour;
        this->minute = minute;
        this->second = second;
    } else
        cout << "Time error!" << endl;
}
void Clock::showTime() const {  //显示时间
    cout << hour << ":" << minute << ":" << second << endl;
}
```

例8-2重载前置++和后置++为时钟类成员函数

```cpp
Clock & Clock::operator ++ () {
    second++;
    if (second >= 60) {
        second -= 60;  minute++;
        if (minute >= 60) {
          minute -= 60; hour = (hour + 1) % 24;
        }
    }
    return *this;
}

Clock Clock::operator ++ (int) {
    //注意形参表中的整型参数
    Clock old = *this;
    ++(*this);  //调用前置"++"运算符
    return old;
}
```

例8-2重载前置++和后置++为时钟类成员函数

```cpp
int main() {
    Clock myClock(23, 59, 59);
    cout << "First time output: ";
    myClock.showTime();
    cout << "Show myClock++:    ";
    (myClock++).showTime();
    cout << "Show ++myClock:    ";
    (++myClock).showTime();
    return 0;
}
```
