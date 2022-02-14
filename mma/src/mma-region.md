# Region

## Element(属于)

```mathematica
Element[x,dom]; 或 `x\[Element] dom` 断言 `x` 是域 `dom` 的元素.

Element[x,reg]; 或 `x\[Element] reg` 断言 `x` 是区域 `reg` 中的元素.

Element[x1, x2, ..., dom]; 断言所有 `x_i` 都是 `dom` 的元素.

Element[patt,dom]; 断言任何匹配模式 `patt` 的表达式, 是 `dom` 中的元素.
```

## 详细

+ $x \in dom$ 可以被输入为 `x esc el esc dom` 或 `x \[Element] dom`.
+ `Element` 可用于, 在 `Simplify` 和相关函数中, 设置 `assumptions`.
+ `dom` 可以是 `数域`(`domain`), 或 $R^n$ 中的 `区域`(`region`).
+ 可能的域 `dom`是:
    + `Algebraics`;     代数数字
    + `Booleans`;       `True` or `False`
    + `Complexes`;      复数
    + `Integers`;       整数
    + `Primes`;     质数
    + `Rationals`;      有理数
    + `Reals`;      实数

+ 可能的 regions  `reg` 是由 `RegionQ` 定义的.
+ 如果可能的话, 当 `x` 是数字时, `x [Element] dom` 会立即计算.
+ 对于 domain `dom`, `{x1,x2, ...} \[Element] dom` 等同于 `(x1 | x2 | ...) \[Element] dom`.
+ 对于 region `reg`, `{x1, x2, ...} \[Element] reg` 断言, 坐标为 `x1, x2, ...` 的点属于 `reg`.
+ 如果不能立即确定真假, `{x1, x2, ...} \[Element] dom` 将被转换成 `(x1 | x2 | ...) \[Element] dom`.

### 例子

#### 基础例子

检验 `\[Pi]` 是否属于 `reals`:

```mathematica
In[1]:= Pi \[Element] Reals

Out[1]= True
```

测试 点 `{1/2,1/3}` 是否属于 `单位盘`(unit disk):

```mathematica
{1/2, 1/3} \[Element] Disk[]
Out[1]= True
```

表示`表达式`的所属域(domain membership):

```mathematica
Element[x + y, Reals]
Out[1]= x + y \[Element] Reals
```

断言点 `{x,y,z}` 属于单位球:

```mathematica
Element[{x, y, z}, Ball[]]
Out[1]= {x, y, z} \[Element] Ball[{0, 0, 0}]
```

使用 `element` 断言来对区域积分:

```mathematica
Integrate[1, {x, y, z} \[Element] Ball[]]
Out[2]= (4 \[Pi])/3
```

或者在 `区域上` 进行优化(optimize):

```mathematica
MinValue[x + y, {x, y, z} \[Element] Ball[]]
Out[3]= -Sqrt[2]
```

使用 `esc elem esc` 输入:

```mathematica
x \[Element] Reals
Out[1]= x \[Element] Reals
```
