# solver debug

## 矩阵组装失败, 读取位置为神秘常量

[烫烫烫烫烫烫](https://zhuanlan.zhihu.com/p/92004659)

Visual Studio的编译器分配完空间后, 会顺手往里面装进去一点内容.
默认装进去的东西也有几种差别:

+ 未分配或静态分配但未赋初值的内存空间, 初值用 `0xCCCC` 填充;
+ 动态分配但未赋初值的内存空间, 初值用 `0xCDCD` 填充;
+ 动态分配后又被回收了的内存空间, 用 `0xDDDD` 填充;

正常情况下, 这些内容是不会被访问到的. 当然, 这是正常情况.
当发生一些bug, 如非常常见的数组越界的时候, 事情就发生了.
因为bug, 刚好以字符输出了情况①的填充内容, 我们就会看到华丽而壮观的……

    烫烫烫烫烫烫烫烫烫烫烫烫烫烫烫烫

是的, `0xCCCC` 就是 `烫` 的GBK编码!

顺带一提, `0xCDCD` 对应的是汉字 `屯`, 而 `0xDDDD` 对应的是汉字 `葺`.
只有在按字符输出的情况下才能看到这些.

按int输出的话, 看到的, 只有索然无味的

+ `0xCCCC CCCC`; `-858993460`, `烫`
+ `0xCDCD CDCD`; `-842150451`, `屯`
+ `0xDDDD DDDD`; `-572662307`, `葺`

### OpenMP Critical

原因是组装矩阵时,
将 local 矩阵插入全局矩阵的操作, 不在同一个 Critical 块中.
导致 矩阵组装 过程那边变量被 race condition.

## GetDofValue, HasField 报错, STPBEG_ACCUM

原因, 没有初始化 `STPBEG_ACCUM` 对应的解.

## 非线性迭代不收敛, Release 收敛, debug 不收敛.

可能是 `自定义函数` 设置错误,
Create 函数, 参数读取错误, Release 版本会将读取失败的参数 初始化为零.

## 控制台乱码, 控制台中文

在 main.cpp 中添加

```cpp
//... 在这个位置添加
#ifdef _WIN32
// ...

system("chcp 65001")
#endif
```

### 电化学, 反应动力学 自定义函数 计算失败, y too large

改变 电场初始值, 可能会收敛.

### warning C26451

[Warning C26451: Arithmetic overflow](https://stackoverflow.com/questions/55995817/warning-c26451-arithmetic-overflow)
[Warning C26451](https://learn.microsoft.com/en-us/cpp/code-quality/c26451?view=msvc-170)
[C++ 中的 #pragma warning(push) 和 #pragma warning(pop)有什么用](https://blog.csdn.net/zgaoq/article/details/109123906)

我认为这是 VS2019 中的一个bug. 在 VS2022 中它不再被标记.
例如, 下列程序会产生警告

```cpp
double test2(int n)
{
     return 4.0 * (n - 1);
}
```

但是这个反而不会

```cpp
int test2a(int n)
{
    return 4 * (n - 1);
}
```

然而, 后者出现未定义行为的风险要大得多.
`乘以4` 会大大增加出现未定义行为的风险, 因为大量的 `n` 会产生未定义行为.
有多大?在第一个例子中, 在大约 `40亿` 个可能的 `n` 值中, 只有一个可能值会溢出.
在第二个例子中, 大约有 `30亿个n` 会溢出/溢出.
为什么反而只有第一个被警告呢?
这是因为如果每一个比加 0 或乘 1 更复杂的表达式, 都会因为可能溢出而被标记, 那么整数运算就无法进行了.

可以说, 如果要将警告设置得如此之高, 几乎所有对 ints 的算术运算都会受到警告.
不过, 从 VS2022 开始, 微软不再对此发出 C26451 警告.
在 `-Wall` 下也不会显示. 他们显然懂得都懂.

`Code Analysis` 觉得会 overflow, 可以这样解决:

+ Disable the warning in code:

```cpp
#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable : 26454)    # 禁止compiler警告
#endif

//  your code  here

#if defined(_MSC_VER)
#pragma warning(pop)  # 恢复compiler警告
#endif
```

+ 本答案展示了在 VS 2019 代码分析规则集编辑器中禁用此警告的方法.
在`Code Analysis rule` 中禁用此 warning
[Use rule sets to group code analysis rules](https://learn.microsoft.com/en-us/visualstudio/code-quality/using-rule-sets-to-group-code-analysis-rules)

![img](https://i.stack.imgur.com/GRon3.png)

在 project 上右键, 通过 `properties` 页面寻找 code analysis.

## solver gcc 编译问题

+ 不要使用 `std::unordered_map<Enum,xxx>`, gcc 不会处理 Enum 作 key 的情形.
+ 不要使用 `std::unordered_map` 的 `insert_or_assign` 接口, gcc 不认识.
