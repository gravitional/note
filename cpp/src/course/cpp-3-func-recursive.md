# 函数的递归调用

## 定义

函数直接或间接地调用自身, 称为递归调用.

## 例3-8 求n!

使用递归函数实现. 源代码:

```cpp
#include <iostream>
using namespace std;
unsigned fac(int n){
    unsigned f;
    if (n == 0)
      f = 1;
    else
      f = fac(n - 1) * n;
    return f;
}
int main() {
    unsigned n;
    cout << "Enter a positive integer:";
    cin >> n;
    unsigned y = fac(n);
    cout << n << "! = " << y << endl;
    return 0;
}
```

运行结果:

Enter a positive integer:8
8! = 40320

## 例3-10

有三根针A, B, C. A针上有N个盘子, 大的在下, 小的在上, 要求把这N个盘子从A针移到C针,
在移动过程中可以借助B针, 每次只允许移动一个盘, 且在移动过程中在三根针上都保持大盘在下, 小盘在上.

![hanno tower](http://studio-tsinghua.xuetangx.com/asset-v1:TsinghuaX+00740043-91-20202+2020_T2+type@asset+block@hanoi.png)

将 n 个盘子从A针移到C针可以分解为三个步骤:

+ 将A 上n-1个盘子移到 B针上(借助C针);
+ 把A针上剩下的一个盘子移到C针上;
+ 将n-1个盘子从B针移到C针上(借助A针).

源代码:

```cpp
#include <iostream>
using namespace std;

//将src针的最上面一个盘子移动到dest针上
void move(char src, char dest) {
    cout << src << " --> " << dest << endl;
}

//将n个盘子从src针移动到dest针, 以medium针作为中转
void hanoi(int n, char src, char medium, char dest)
{
  if (n == 1)
    move(src, dest);
  else {
    hanoi(n - 1, src, dest, medium);
    move(src, dest);
    hanoi(n - 1, medium, src, dest);
  }
}
int main() {
  int m;
  cout << "Enter the number of diskes: ";
  cin >> m;
  cout << "the steps to moving " << m << " diskes:" << endl;
  hanoi(m,'A','B','C');
  return 0;
}
```

运行结果:

Enter the number of diskes:3
the steps to moving 3 diskes:
A --> C
A --> B
C --> B
A --> C
B --> A
B --> C
A --> C
