# 嵌套的控制结构, 其他控制语句

例2-10 输入一系列整数, 统计出正整数个数i和负整数个数j,读入0则结束.

#include <iostream>

using namespace std;

int main() {

      int i = 0, j = 0, n;

      cout <<"Enter some integers please (enter 0 to quit):" << endl;

      cin >> n;

      while (n != 0) {

        if (n > 0) i += 1;

        if (n < 0) j += 1;

        cin >> n;

      }

      cout << "Count of positive integers: " << i << endl;

      cout << "Count of negative integers: " << j << endl;

      return 0;

}
其他控制语句

break语句

使程序从循环体和switch语句内跳出, 继续执行逻辑上的下一条语句. 不宜用在别处.

continue 语句

结束本次循环, 接着判断是否执行下一次循环.

goto 语句

使程序的执行流程跳转到语句标号所指定的语句. 不提倡使用.
