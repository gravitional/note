# 积分

tutorial/DefiniteIntegrals

即使能求出函数的不定积分, 如果只管将积分上下限处的值相减, 还是往往会导致错误. 原因在于积分区域中可能有奇点.

这里是 $1/x^2$ 的不定积分:

```mathematica
In[10]:= Integrate[1/x^2, x]
Out[10]= -(1/x)
```

实际上, $x=0$ 是双重极点, 此定积分是发散的:

这里是一个更妙的例子, 其中包括分支线:

```mathematica
In[13]:= Integrate[1/(1 + a Sin[x]), x]
Out[13]= (2 ArcTan[(a + Tan[x/2])/Sqrt[1 - a^2]])/Sqrt[1 - a^2]
```

端点处的极限值相减得$0$:

$$In[14]:= \lim[Out[13], x \to 2 \pi] - \lim[Out[13], x \to 0] \\
Out[14]= 0 $$

然而该定积分的正确结果依赖于$a$. 假定保证了函数的收敛:

$$ In[15]:= \int[1/(1 + a \sin[x]), \{x, 0, 2 \pi\}, Assumptions \to -1 < a < 1] \\
Out[15]= (2 \pi)/\sqrt{1 - a^2} $$

$$ \text{Integrate}[\text{function}(x),\{x,-1,1\},\text{PrincipalValue}\to \text{True}]$$

定积分的柯西主值

这是 $1/x$ 的不定积分:

$$\int \frac{1}{x} \, dx
\log (x)  $$

$-1$和$+2$处的极限值相减产生一个包含$i\,\pi$的奇怪结果:

Riemann 意义下的定积分是发散的:
Out[38]= `Integrate::idiv: 1/x 的积分在 {-1,2} 上不收敛`.

$$\int_{-1}^2 \frac{1}{x} \, dx$$

而柯西主值意义下该积分是有限的:

$$\text{Integrate}\left[\frac{1}{x},\{x,-1,2\},\text{PrincipalValue}\to \text{True}\right] \\
\log (2) $$

即使定积分收敛, 积分路径上奇点的存在也会导致结果随参数变化而不连续.
有时能使用包含如`Sign`函数的单个公式归纳结果.

此处 `If` 给出积分收敛的条件:

$$ \int_0^{\infty } \frac{\sin (a x)}{x} \, dx \\
\text{ConditionalExpression}\left[\frac{\pi  a}{2 \sqrt{a^2}},\Im(a)\leq 0\right]  $$

结果关于 $a$ 是不连续的. 不连续的原因是 $x=\infty$ 是 $sin(x)$ 的本性奇点
