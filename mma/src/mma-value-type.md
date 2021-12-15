# mma 的数据类型 值类型

一个变量的值具有许多类型, 见: ref/Set

OwnValues
DownValues
SubValues;
UpValues
DefaultValues
NValues
FormatValues
Definition

`上值`:`UpValue`

`^:=` 定义上值(`upvalue`), 它的方式和使用一个标签的相同:

```mathematica
In[1]:= g /: f[g[x_]] := f1[x]

In[2]:= f[h[x_]] ^:= f2[x]

In[3]:= {UpValues[g], UpValues[h]}

Out[3]= {{HoldPattern[f[g[x_]]] :> f1[x]}, {HoldPattern[f[h[x_]]] :> f2[x]}}
```

一个标签仅定义一个上值(upvalue), `^:=` 执行所有符号的定义:

```mathematica
In[1]:= g /: f1[g[x_], h[y_]] := gh[x y]

In[2]:= f2[g[x_], h[y_]] ^:= gh[x y]

In[3]:= {UpValues[g], UpValues[h]}

Out[3]= {{HoldPattern[f1[g[x_], h[y_]]] :> gh[x y],
HoldPattern[f2[g[x_], h[y_]]] :> gh[x y]}, {HoldPattern[f2[g[x_], h[y_]]] :>
  gh[x y]}}
```

进行定义时, 计算立即赋值的右边:

```mathematica
In[1]:= rand[int] ^= Random[Integer];

In[2]:= {rand[int], rand[int]}

Out[2]= {0, 0}
```

每次使用定义时, 每次计算延迟定义的右边:

```mathematica
In[3]:= rand[real] ^:= Random[Real]

In[4]:= {rand[real], rand[real]}

Out[4]= {0.409393, 0.730688}
```

## SubValue

`SubValues[f]`; 给出符号`f` 中定义的所有和 `subvalues` 相关的变换规则,
其中 `subvalues` 是形如 `f[x, ...][...]` 的值.
