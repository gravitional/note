#include <iostream>
using namespace std;

class Clock
{
public:
    Clock(int newH, int newM, int newS); //构造函数
    Clock(int newH);
    Clock(); // 默认构造函数
    void setTime(int newH = 0, int newM = 0, int newS = 0);
    void showTime();

private:
    int hour, minute, second;
};

void Clock::setTime(int newH, int newM, int newS)
{
    hour = newH;
    minute = newM;
    second = newS;
}

void Clock::showTime()
{
    cout << hour << ":" << minute << ":" << second << endl;
}

Clock::Clock(int newH, int newM, int newS) : hour(newH), minute(newM), second(newS) {} //初始化列表
Clock::Clock(int newH) : hour(newH), minute(0), second(0) {}
Clock::Clock() : hour(0), minute(0), second(0) {}

int main()
{
    Clock c1(8, 10, 0); //自动调用构造函数
    Clock c2;
    Clock c3(3);
    c1.showTime();
    c2.showTime();
    c3.showTime();
    return 0;
}