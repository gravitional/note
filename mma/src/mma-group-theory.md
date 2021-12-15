# 群论

![群环域](https://img-blog.csdnimg.cn/20210429150145188.jpeg?x-oss-process=image/watermark,type_ZmFuZ3poZW5naGVpdGk,shadow_10,text_aHR0cHM6Ly9ibG9nLmNzZG4ubmV0L2N3Mzk3MjY1MzYy,size_16,color_FFFFFF,t_70)

[群, 环, 域的概念总结](https://blog.csdn.net/xq723310/article/details/90382479)

半群:

+ 加法封闭性
+ 加法结合律

群: 如整数加法群.

+ 加法封闭性
+ 加法结合律
+ 加法单位元
+ 加法逆元

***
交换群: 如整数加法群.

+ 加法交换律

***
环: 如 整数环 (`+`, `*`)

+ 乘法封闭性
+ 乘法结合律
+ 分配律

***
交换环: 如 整数交换环 (`+`, `*`)

+ 乘法交换律

***
整环: 如 整数整环 (`+`, `*`)

+ 乘法单位元:
+ 无零因子:

***
域: 如有理数域 (`+`, `*`)

+ 乘法逆元

域是 `交换性` 除环: `(F,  +,  *)`, 其中加法单位元(`0`)不等于乘法单位元(`1`), 所有 `非零元素` 都有乘法 `逆元`.
其中 $0 \neq 1$ 的要求是为了排除没有什么意义的, 只有一个元素组成的域.

如果一个含`幺环` R 的每一个非零元有乘法逆, 则 R 称为 `除环`(division ring), 常记为 `D`.
`除环` 和 `域` 只相差 `乘法交换律`, 所以 `交换除环` 就是域.
有时我们也称 `除环` 为 skew field, skew 正是指(二元乘法的)不对称.
最经典的除环要数哈密顿发现的四元数环, 有意思的是, 这样的非退化的除环并没有有限的形式. 也就是说, 我们有如下著名的定理:

>定理(Wedderburn):每个有限除环 `D` 都是域.

## 置换群

### Permute,置换操作

置换可以表示成置换列表的形式, 即对`{1,2,3,..}`的重排结果.
也可以表示成不相连循环的形式, 即类似`{123}{45}{78}`, 每一组内部独立进行轮换.

+ `Permute[expr,perm]` ; 根据置换 `perm` 对 `expr`元素的位置进行排列.
+ `Permute[expr,gr]` ; 返回 `expr` 在置换群`gr`的元素作用下形式.

+ `Permute`适用于任何非原子表达式, 对表达式的第一层进行操作.
+ `permute`对表达式的元素进行重新排序, 但不改变其长度.
+ 置换`perm`可以用不相连的轮换形式给出, 或者以置换列表的形式给出.
+ 当`perm`以循环形式给出时 `Cycles[cyc1,cyc2,...]}]`, 轮换 `{p1,p2}`以循环的方式移动`expr`的元素, 使`expr[[pi]`被移动到位置`p(i+1)`.
+ 当`perm` 以置换列表的形式给出, 其结果等同于使用 `Permute[expr, PermutationCycles[perm] ]`.
+ 置换群 `gr` 可以由生成元指定, 头部为 `PermutationGroup`, 或者用名称指定, 头部为 `SymmetricGroup`, `AlternatingGroup`, ...

```mathematica
Permute[{a, b, c, d, e}, Cycles[{{1, 3, 2}}]]
{b, c, a, d, e}
```

### Cycles,轮换的表示

+ `Cycles[ {cyc1, cyc2, ... }]` ; 代表一个具有不相连轮换`cyci`的置换.
+ 置换中的循环`cyc1]`是用正整数列表的形式给出的, 代表置换作用的位置.
+ 循环`{p1, p2, ..., pn}` 代表把`pi`映射到`p(i+1)`. 最后一个点`pn`被映射到`p1`.
+ 不包括在任何循环中的点被认为映射到它们自己.
+ 循环必须是不相交的, 也就是说, 它们必须没有公共点, 参数必须都是正整数.
+ 循环对象被自动正规化, 通过丢弃空循环和单一循环, 旋转每个循环使最小的点先出现. 并按第一个点对多个循环排序.
+ `Cycles[{}]`代表恒等置换.

### 置换形式的转换

+ `PermutationCycles[perm]`; 给出置换 `perm` 的不相交的轮换表示.

+ 输入的 `perm` 可以是一个置换列表, 也可以是不相连的循环.
+ 置换列表是连续正整数`{1,2,..,n}`的重新排序.
+ `PermutationCycles[perm]`返回一个表达式, 头部是`Cycles`, 并包含循环的列表, 每个循环的形式为`{p1, p2, ..., pn}`, 它表示`pi`到`p(i+1)`的映射.
最后一点`pn`被映射到`p1`.
+ `PermutationCycles[perm,h]`返回一个头部为`h`的表达式.
+ `PermutationCycles`的结果自动正规化, 通过旋转每个循环使最小的点出现在前面, 并通过首位的点对循环进行排序.

```mathematica
PermutationCycles[{2, 5, 3, 6, 1, 8, 7, 9, 4, 10}]
Cycles[{{1, 2, 5}, {4, 6, 8, 9}}]
```

+ `PermutationList[perm]` ; 给出置换`perm` 的置换列表形式.

+ `PermutationList[perm,len]`; 返回一个长度为 `len` 的置换列表.
+ 置换`perm`的输入形式可以是置换列表, 也可以是不相连的循环.
+ `PermutationList` 也可以作用到 `SparseArray` 对象上.

```mathematica
PermutationList[Cycles[{{3, 2}, {1, 6, 7}}]]
{6, 3, 2, 4, 5, 7, 1}
```

### 生成置换组合

`Permutations[list]`; 生成 `list` 中元素的所有可能的排列组合的列表.
`Permutations[list,n]` ; 给出所有最多包含 `n` 个元素的排列组合.
`Permutations[list,{n}]` ; 给出所有正好包含 `n` 个元素的排列组合.
`Signature[list]`; 给出置换的`signature`, 这个置换能把列表中的元素恢复成标准顺序.

## 置换的运算

`PermutationProduct `
`InversePermutation`
`PermutationPower`
`FindPermutation`
