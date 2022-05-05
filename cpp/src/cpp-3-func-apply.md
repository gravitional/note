# 函数调用

调用函数需要先声明函数原型

若函数定义在调用点之前, 可以不另外声明;

若函数定义在调用点之后, 必须要在调用函数前声明函数原型:

函数原型: 类型标识符 被调用函数名(含类型说明的形参表)

函数调用形式

函数名(实参列表)

![嵌套调用17.png](http://sc0.ykt.io/ue_i/20191116/1195549444330885120.png)

嵌套调用: 在一个函数的函数体中, 调用另一函数.

例3-1编写一个求x的n次方的函数

#include <iostream>

using namespace std;

//计算x的n次方

double power(double x, int n) {

double val = 1.0;

while (n--) val *= x;

return val;

}

int main() {

cout << "5 to the power 2 is "
<< power(5, 2) << endl;

return 0;

}

## 例3-2  数制转换

输入一个8位二进制数, 将其转换为十进制数输出.

例如: 从键盘输入1101

11012=1×23+1×22+0×21+1×20=1310

所以, 程序应输出13

源代码:

#include <iostream>
using namespace std;

double power (double x, int n); //计算x的n次方

int main() {
    int  value = 0;
    cout << "Enter an 8 bit binary number  ";
    for (int i = 7; i >= 0; i--) {
      char ch;
      cin >> ch;
      if (ch == '1')
        value += static_cast<int>(power(2, i));
    }
    cout << "Decimal value is  " << value << endl;
    return 0;
}
double power (double x, int n) {
    double val = 1.0;
    while (n--)
      val *= x;
    return val;
}

## 例3-3 编写程序求π的值

π的计算公式如下:

![arctan1.gif](http://sc0.ykt.io/ue_i/20200304/1235038756407480320.gif)

其中arctan用如下形式的级数计算:

![arctan2.gif](http://sc0.ykt.io/ue_i/20200304/1235038812019757056.gif)

直到级数某项绝对值不大于10-15为止; π和x均为double型.

arctan函数

#include <iostream>

using namespace std;

double arctan(double x) {

          double sqr = x * x;

          double e = x;

          double r = 0;

          int i = 1;

          while (e / i > 1e-15) {

                   double f = e / i;

                   r = (i % 4 == 1) ? r + f : r - f;

                   e = e * sqr;

                   i += 2;

          }

          return r;

}
主程序

int main() {

          double a = 16.0 * arctan(1/5.0);

          double b = 4.0 * arctan(1/239.0);

          //注意: 因为整数相除结果取整, 如果参数写1/5, 1/239, 结果就都是0

          cout << "PI = " << a - b << endl;

          return 0;

}

## 例3-4 寻找并输出11~999之间的数M, 它满足M, M2和M3均为回文数.

回文: 各位数字左右对称的整数.

例如: 11满足上述条件

n  112=121, 113=1331.

分析:

用除以10取余的方法, 从最低位开始, 依次取出该数的各位数字. 按反序重新构成新的数, 比较与原数是否相等, 若相等, 则原数为回文.
源代码:

#include <iostream>

using namespace std;

//判断n是否为回文数

bool symm(unsigned n) {

      unsigned i = n;

      unsigned m = 0;

      while (i > 0) {

        m = m * 10 + i % 10;

        i /= 10;

  }

  return m == n;

}

int main() {

      for(unsigned m = 11; m < 1000; m++)

        if (symm(m) && symm(m * m) && symm(m * m * m)) {

          cout << "m = " << m;

          cout << "  m * m = " << m * m;

          cout << "  m * m * m = "

               << m * m * m << endl;

        }

      return 0;

}

运行结果:

m=11  m*m=121  m*m*m=1331

m=101  m*m=10201  m*m*m=1030301

m=111  m*m=12321  m*m*m=1367631

## 例3-5 计算分段函数, 并输出结果

![piecewise](http://studio-tsinghua.xuetangx.com/asset-v1:TsinghuaX+00740043-91-20202+2020_T2+type@asset+block@k.jpg)

其中r, s的值由键盘输入. sin x的近似值按如下公式计算, 计算精度为10-10:

![sin x](http://studio-tsinghua.xuetangx.com/asset-v1:TsinghuaX+00740043-91-20202+2020_T2+type@asset+block@sinx.jpg)

[程序](https://www.xuetangx.com/learn/THU08091000247/THU08091000247/10322314/video/17397771)

## 函数调用例3-6

例3-6投骰子的随机游戏

每个骰子有六面, 点数分别为1, 2, 3, 4, 5, 6. 游戏者在程序开始时输入一个无符号整数, 作为产生随机数的种子.

每轮投两次骰子, 第一轮如果和数为7或11则为胜, 游戏结束; 和数为2, 3或12则为负, 游戏结束; 和数为其它值则将此值作为自己的点数, 继续第二轮, 第三轮...直到某轮的和数等于点数则取胜, 若在此前出现和数为7则为负.

rand函数

 函数原型: int rand(void);

 所需头文件: <cstdlib>

 功能和返回值: 求出并返回一个伪随机数

srand函数

 void srand(unsigned int seed);

 参数: seed产生随机数的种子

 所需头文件: <cstdlib>

 功能: 为使rand()产生一序列伪随机整数而设置起始点. 使用1作为seed参数, 可以重新初化rand().
