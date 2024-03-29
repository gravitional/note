# 数组

## 数组重排

### Flatten

如果 $m_{ij}$ 为矩阵, `Flatten[{{ $m_{11}$, $m_{12}$}, {$m_{21}$, $m_{22}$ }},{{1,3},{2,4}}]`
实际上建立了由块 $m_{ij}$ 组成的单个矩阵.

本来的矩阵, 实际上有`4`个指标`ijkl`
`{{1,3},{2,4}}` 的意思是, 把第一个和第三个指标放在一起, 把第二个和第四个指标放在一起.

也就是说 Flatten 实际上是 矩阵索引 重新划分函数.
对于一个矩阵, 有`n`个索引(指标)(`a1,a2,...,an`).
总的元素个数是 `a1* a2*...*an`
可以重新组合这些指标, 例如把`a1, a2` 并入一个指标中,
指标的取值范围变成`1,2,...,a_1*a_2`

类似的, 矩阵的各种指标可以随意交换,
`a1* a2* a3* a4` to `a3* a2* a1* a4`, 这就是广义转置的过程.
广义转置再加上重新划分(指标的重新组合), 这就是Flatten的作用.

类似的操作还有

### ArrayFlatten

`ArrayFlatten[a,r]`: 数组展平, 通常等价于`Flatten[a,{{1,r+1},{2,r+2},...,{r,2r}}]`, 相当于`Flatten`的简化版

```
ArrayFlatten[ {
  {m11,m12,...},
  {m21,m22,...},
  ...
  }]
```

从矩阵`mij`组成的嵌套矩阵出发, 创建一个单独的平铺矩阵. 也就是去掉内部多余的括号.

### ArrayReshape

把数组重塑成指定维数的数组: `ArrayReshape[list,dims] `

```mathematica
In[1]:= ArrayReshape[{a,b,c,d,e,f},{2,3}]
Out[1]= {{a,b,c},{d,e,f}}
```

## Splice

```mathematica
Splice[{ e1, e2, ...}]; 代表`表达式`, 该表达式将被自动 `拼接` 到它出现的任何列表中, 作为元素`e_i` 的序列
```

### 性质和关系

在列表中时, `Splice[{ e1, e2, ...}] ` 的行为类似于 `Sequence[e1, e2, ...]`:

```mathematica
{a, b, c, Splice[{1, 2, 3}], d, e}
Out[1]= {a, b, c, 1, 2, 3, d, e}

{a, b, c, Sequence[1, 2, 3], d, e}
Out[2]= {a, b, c, 1, 2, 3, d, e}
```

`Sequence` 对象在其他 `heads` 里面也可以拼接, 但 `Splice` 对象不会:

```mathematica
head[a, b, c, Sequence[1, 2, 3], d, e]
Out[3]= head[a, b, c, 1, 2, 3, d, e]

head[a, b, c, Splice[{1, 2, 3}], d, e]
Out[4]= head[a, b, c, Splice[{1, 2, 3}], d, e]
```
