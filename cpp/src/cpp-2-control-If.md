# if语句

If语句的语法形式

if (表达式) 语句

例: if (x > y) cout << x;

if (表达式) 语句1 else 语句2

例: if (x > y) cout << x;

else cout << y;

if (表达式1)语句1
else if (表达式2) 语句2
else if (表达式3) 语句3
          ...
else 语句 n

例2-2输入一个年份, 判断是否闰年

#include <iostream>

using namespace std;

int main() {

          int year;

          bool isLeapYear;

          cout << "Enter the year: ";

          cin >> year;

          isLeapYear = ((year % 4 == 0 && year % 100 != 0) || (year % 400 == 0));

          if (isLeapYear)

                   cout << year << " is a leap year" << endl;

          else

                   cout << year << " is not a leap year" << endl;

          return 0;

}

嵌套的if结构

语法形式

if(   )

if(   ) 语句 1

else 语句 2

else

if(   ) 语句 3

else 语句 4

注意

n  语句 1, 2, 3, 4 可以是复合语句;

n  每层的if 与 else 配对, 或用 { } 来确定层次关系.

例2-3: 输入两个整数, 比较两个数的大小

#include<iostream>

using namespace std;

int main() {

          int x, y;

          cout << "Enter x and y:";

          cin >> x >> y;

          if (x != y)

                   if (x > y)

                             cout << "x > y" << endl;

                   else

                             cout << "x < y" << endl;

          else

                   cout << "x = y" << endl;

          return 0;

}
