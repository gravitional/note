# 群论

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