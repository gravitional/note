# Sizeof运算, 位运算

## sizeof运算

语法形式

```cpp
sizeof (类型名)
或 sizeof 表达式
```

结果值:
`类型名` 所指定的类型, 或 `表达式 的结果类型` 所占的字节数.

例:

```cpp
sizeof(short)
sizeof  x
```

## 位运算 -- 按位与(&)

运算规则
将两个运算量的每一个位进行逻辑与操作

举例: 计算 `3 & 5`

![10.png](http://sc0.ykt.io/ue_i/20191116/1195547304468287488.png)

### 用途:

+ 将某一位置0, 其他位不变.
例如: 将char型变量a的最低位置0:

```cpp
a = a & 0xfe; ;(0xfe:1111 1110)
```

### 取指定位

例如: 有 `char c; int a;`
取出a的低字节, 置于c中:

```cpp
c=a & 0xff; (0xff:1111 1111)
```

### 位运算 -- 按位或(|)

运算规则; 将两个运算量的每一个位进行逻辑或操作
举例: 计算 `3 | 5`

![11.png](http://sc0.ykt.io/ue_i/20191116/1195547446013464576.png)

用途: 将某些位置1, 其他位不变.
例如: 将 int 型变量 a 的低字节置 1 :

```cpp
a = a | 0xff;
```

### 位运算 -- 按位异或(^)

运算规则; 两个操作数进行异或:

+ 若对应位相同, 则结果该位为 0,
+ 若对应位不同, 则结果该位为 1,

举例: 计算071^052

![12.png](http://sc0.ykt.io/ue_i/20191116/1195547581837611008.png)

+ 用途举例: 使特定位翻转(与0异或保持原值, 与1异或取反)

例如: 要使 01111010 低四位翻转:

![13.png](http://sc0.ykt.io/ue_i/20191116/1195547727174438912.png)

## 位运算 -- 取反(~)

运算规则;  单目运算符, 对一个二进制数按位取反.
例:

```cpp
  025: 0000000000010101
~025: 1111111111101010
```

## 位运算 -- 移位(<<, >>)

+ 左移运算(<<)
+ 左移后, 低位补0, 高位舍弃.
+ 右移运算(>>)

+ 右移后:
    低位: 舍弃
    高位:
        无符号数: 补0
        有符号数: 补"符号位"