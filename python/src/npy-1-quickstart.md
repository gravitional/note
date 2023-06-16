# quick start

[NumPy quickstart](https://numpy.org/doc/stable/user/quickstart.html#advanced-indexing-and-index-tricks)

## The Basics

ndarray.ndim
ndarray.shape
ndarray.size
ndarray.dtype
ndarray.itemsize
ndarray.data

## Array Creation

```python
import numpy as np
a = np.array([2, 3, 4])

np.zeros((3, 4))
np.ones((2, 3, 4), dtype=np.int16)
np.empty((2, 3))

np.arange(10, 30, 5)
np.linspace(0, 2, 9)

b = np.zeros_like(a)
```

## Printing Arrays

```python
a = np.arange(6)                    # 1d array
b = np.arange(12).reshape(4, 3)     # 2d array
c = np.arange(24).reshape(2, 3, 4)  # 3d array

np.set_printoptions(threshold=sys.maxsize)  # sys module should be imported
```

## Basic Operations

Arithmetic operators on arrays apply `elementwise`.
A new array is created and filled with the result.

```
a = np.array([20, 30, 40, 50])
b = np.arange(4)
c = a - b

10 * np.sin(a)
```

+ the product operator * operates elementwise in NumPy arrays.
+ The matrix product can be performed using the @ operator (in python >=3.5) or the dot function or method:

```python
A = np.array([[1, 1],
              [0, 1]])
B = np.array([[2, 0],
              [3, 4]])
A * B     # elementwise product
A @ B     # matrix product
A.dot(B)  # another matrix product
```

in-place 运算

```python
rg = np.random.default_rng(1)  # create instance of default random number generator
a = np.ones((2, 3), dtype=int)
b = rg.random((2, 3))
a *= 3
```

method 风格 与 函数风格

```python
a = rg.random((2, 3))

a.sum()
a.min()
a.max()
```

显式指定 方法生效的 axis

```python
b = np.arange(12).reshape(3, 4)

b.sum(axis=0)     # sum of each column

b.min(axis=1)     # min of each row

b.cumsum(axis=1)  # cumulative sum along each row
```

## 通用函数, Universal Functions

```python
B = np.arange(3)

np.exp(B)
np.sqrt(B)
C = np.array([2., -1., 4.])
np.add(B, C)
```

```python
all, any, apply_along_axis,
argmax, argmin, argsort, average, bincount,
ceil, clip, conj, corrcoef, cov, cross,
cumprod, cumsum, diff, dot, floor, inner, invert, lexsort,
max, maximum, mean, median, min, minimum,
nonzero, outer, prod, re, round, sort,
std, sum, trace, transpose, var, vdot, vectorize, where
```

## Indexing, Slicing and Iterating

```python
a = np.arange(10)**3

a[2]
a[2:5]

a[:6:2] = 1000
a[::-1]  # reversed a

for i in a:
    print(i**(1 / 3.))
```

多维数组

```python
def f(x, y):
    return 10 * x + y

b = np.fromfunction(f, (5, 4), dtype=int)

b[2, 3]

b[0:5, 1]  # each row in the second column of b
b[:, 1]    # equivalent to the previous example
b[1:3, :]  # each column in the second and third row of b
```

省略指标

```python
b[-1]   # the last row. Equivalent to b[-1, :]
out: array([40, 41, 42, 43])

b[-1, ...]
```

例如, `x` is an array with `5` axes

`x[1, 2, ...]` is equivalent to `x[1, 2, :, :, :]`,
`x[..., 3]` to `x[:, :, :, :, 3]` and
`x[4, ..., 5, :]` to `x[4, :, :, 5, :]`.

遍历

```python
# 遍历行, 1st 指标
for row in b:
    print(row)

# 遍历元素
for element in b.flat:
    print(element)
```

[Indexing on ndarrays](https://numpy.org/doc/stable/user/basics.indexing.html#basics-indexing)
[Indexing routines (reference)](https://numpy.org/doc/stable/reference/arrays.indexing.html#arrays-indexing)
newaxis, ndenumerate, indices

## Shape Manipulation

Note that the following three commands all return a modified array,
but do not change the original array:

```cpp
a.ravel()  # returns the array, flattened

a.reshape(6, 2)  # returns the array with a modified shape

a.T  # returns the array, transposed
a.T.shape
```

The order of the elements in the array resulting from ravel is normally "C-style",
so the element after a[0, 0] is a[0, 1].

the `ndarray.resize` method modifies the array itself:

```python
a.resize((2, 6))

a.reshape(3, -1) # -1表示自动计算维度
```

### Stacking together different arrays

顺序为 v,h

```python
a = np.floor(10 * rg.random((2, 2)))

b = np.floor(10 * rg.random((2, 2)))

np.vstack((a, b)) # 1st 指标连接
np.hstack((a, b)) # 2nd 指标连接
```

`column_stack` 将1D数组 按列组装成 2D 数组

```python
from numpy import newaxis
np.column_stack((a, b))  # with 2D arrays

a = np.array([4., 2.])
b = np.array([3., 8.])
np.column_stack((a, b))  # returns a 2D array

np.hstack((a, b))        # the result is different

a[:, newaxis]  # view `a` as a 2D column vector

np.column_stack((a[:, newaxis], b[:, newaxis]))
np.hstack((a[:, newaxis], b[:, newaxis]))  # the result is the same
```

`row_stack` is an alias for `vstack`

```python
np.column_stack is np.hstack
False
np.row_stack is np.vstack
True
```

>Note

In complex cases, `r_` and `c_` are useful for creating arrays by stacking numbers along one axis. They allow the use of range literals `:`

```python
np.r_[1:4, 0, 4]
array([1, 2, 3, 0, 4])
```

### Splitting one array into several smaller ones

```python
a = np.floor(10 * rg.random((2, 12)))

# Split `a` into 3
np.hsplit(a, 3)

# Split `a` after the third and the fourth column
np.hsplit(a, (3, 4))
```

`vsplit` splits along the vertical axis,
and `array_split` allows one to specify along which axis to split.

## Copies and Views

### 不复制

Simple assignments make no copy of objects or their data.

```python
a = np.array([[ 0,  1,  2,  3],
              [ 4,  5,  6,  7],
              [ 8,  9, 10, 11]])
b = a            # no new object is created
b is a
```

### View or Shallow Copy

```python
c = a.view()
c is a #False

c.base is a         # c is a view of the data owned by a; True
c.flags.owndata     #False
c = c.reshape((2, 6))  # a's shape doesn't change
c[0, 4] = 1234         # a's data changes
```

`Slicing` an array returns a `view` of it:

```python
s = a[:, 1:3]
# s[:] is a view of s. Note the difference between s = 10 and s[:] = 10
s[:] = 10
print(a)
```

### Deep Copy

The `copy` method makes a complete copy of the `array` and its `data`.

```python
d = a.copy()  # a new array object with new data is created

d is a # False

d.base is a  # d doesn't share anything with a; False

d[0, 0] = 9999 # a doesn't change
```

通过copy, 显式释放源数组内存,

```python
a = np.arange(int(1e8))
b = a[:100].copy()
del a  # the memory of ``a`` can be released.
```

## Less Basic

### 广播规则

[广播]: https://numpy.org/doc/stable/user/basics.broadcasting.html#basics-broadcasting

广播允许 通用函数(universal functions) 以有意义的方式处理那些 形状不完全相同的输入.

+ 广播的第一条规则是,
如果 的输入数组所有维数不相同, `1` 将被重复 `prepended` 到较小数组的 `shape`上, 直到所有的数组 具有相同的维数.

+ 第二条广播规则确保, 沿某一特定维度的尺寸为 `1` 的数组,
表现的像具有 该维度的最大形状的数组. (自动复制)
也就是对于 `broadcast` 数组, 沿着该维度的数组元素的值被认为是相同的.
即 `a = [a,a,a,a,...]`

在应用了广播规则后, 所有数组的大小必须匹配.
更多的细节可以在 [广播][] 中找到.

## Advanced indexing and index tricks

以2D数组 `a` 为例, 提供 `行索引数组`, `列索引数组`,
例如 `[1,2,3]`, `[3,2,1]` 去提取矩阵 `a` 中的元素
对于给定索引, 有两种理解方式,

经纬线确定了一个个交点, 可以只选取对角线上的元素,
对应 `a[1,3]`, `a[2,2]`, `a[3,1]`

也可以把所有交点的元素取出来, 对应
`a[1,3]`, `a[1,2]`, `a[1,1]`,
`a[2,3]`, `a[2,2]`, `a[2,1]`,
`a[3,3]`, `a[3,2]`, `a[3,1]`,

图像展示

```wolfram
lenx = 4; leny = 4;
line$x = Line /@ Array[{{1, #}, {lenx, #}} &, 3];
line$y = Line /@ Array[{{#, 1}, {#, lenx}} &, 4];
pts = {PointSize[Large], Red, Point[{
     {1, 3}, {2, 2}, {3, 1}
     }]};
Graphics[{line$x, line$y, pts}]
```
