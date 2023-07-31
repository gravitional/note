# numpy

## 矩阵索引约定

[numpy中 C order与F order的区别是什么?](https://www.zhihu.com/question/23798415)
[NumPy: C order & F order, data buffer & strides](https://zhuanlan.zhihu.com/p/454916593)

下面的 `优先`, 指的是被优先 `访问`, `索引`, `填充`,

### C order

`C order` 是 Row-major Order, 行优先顺序:

![row major](https://pic3.zhimg.com/80/v2-486354b994cc21e4baa1ffa7241cfada_720w.webp)

`C order` 优先访问 `指标2`, 列指标, 视觉上是 `行填充`.
对于多维数组, 最右边的指标变化的最快.

行填充 是 列变动
列填充 是 行变动

### F order

`F order` 是 Column-major Order, 列优先格式:

![column-major](https://pic4.zhimg.com/80/v2-32581c35be18305f810bbfbe853d376b_720w.webp)

`F order` 优先访问指标1, 行指标, 视觉上是 列填充.
对于多维数组, 最左边的指标变化的最快.

## others

[<Cython系列>1. Cython 是什么?为什么要有 Cython?为什么我们要用 Cython? ](https://www.cnblogs.com/traditional/p/13196509.html)

## numpy FFT 细节

[实施细节](https://numpy.org/devdocs/reference/routines.fft.html#module-numpy.fft)

定义 DFT 的方法有很多, 在 指数符号, 归一化 等方面各不相同.
在本实现中, DFT 定义为
$$A_k=\sum_{m=0}^{n-1} a_m\exp\left\{-2\pi i\frac{mk}{n}\right\},\quad k=0,\cdots,n-1$$

一般来说, DFT 是为 复数 输入和输出定义的, 线性频率下的单频分量用复数指数表示.
$a_m=\exp\{2\pi ifm\Delta t\}$, 其中 $\Delta t$ 是采样间隔

结果中的值遵循所谓的 `标准` 次序:
如果 `A = fft(a,n)`, 则 `A[0]` 包含零频项(信号之和),
对于实数输入, 零频项总是纯实数.
然后, `A[1:n/2]` 包含 `正频项`, `A[n/2+1:]` 包含 `负频项`,
按负频递减的顺序排列.

+ 对于偶数输入点, `A[n/2]` 表示正负 奈奎斯特频率(Nyquist),
对于实数输入 `A[n/2]` 也是纯实数.
+ 对于奇数输入点, `A[(n-1)/2]` 包含最大的正频率,
而 `A[(n+1)/2]` 包含最大的负频率.

例程 `np.fft.fftfreq(n)` 返回一个数组, 给出输出中相应元素的 `频率`.
例程 `np.fft.fftshift(A)` 平移变换及其 `频率`,
将零频率成分放在中间, 而 `np.fft.ifftshift(A)` 则撤销移动.

当输入 `a` 是时域信号且 `A = fft(a)` 时,
`np.abs(A)` 是其 `幅值谱`, `np.abs(A)**2` 是其功率谱.
相位频谱由 `np.angle(A)` 得出.

逆 DFT 的定义为
$$a_m=\frac{1}{n}\sum_{k=0}^{n-1} A_k\exp\left\{2\pi i\frac{mk}{n} \right\},\quad m=0,\cdots,n-1$$

它与正向变换的区别在于 指数参数的符号 和 默认的归一化, 即除以 `1/n`.

### np.fft.rfft()

输出: 复数数组

如果输入有 `n` 个, 输出也有 `n` 个, 指标是 `0`<~>`n-1`:
`截断` 或 `零填充` 的输入值, 沿轴线指示的 axis 变换, 
如果未指定 axis, 则沿最后一个 axis 变换.

+ 如果 `n` 为偶数, 变换后的轴长度为 `(n/2)+1`: `0, 1, ..., n/2`. 
指标 `0` 为求和项, 共 `1` 项
指标 `1` <~> `n/2-1` 为正频解, 共 `n/2-1` 项
指标 `n/2` 为中心频率, 共 `1` 项
指标 `n/2+1` <~> `n-1` 为负频解, 共 `n/2-1` 项

+ 如果 `n` 为奇数, 则长度为 `(n+1)/2`: `0, 1, ..., (n+1)/2`
指标 `0` 为求和项, 
指标 `1` <~> `(n-1)/2` 为正频解, 共 `(n-1)/2` 项
没有中心频率
指标 `(n+1)/2` <~> `n-1` 为负频解, 共 `(n-1)/2` 项

numpy 的 rfft 接口

```python
import numpy as np
x = np.array([0, 1, 2, 3, 4])
a = np.fft.rfft(x, norm='backward')
```

对应如下 mma 调用

```mathematica
Fourier[{0, 1, 2, 3, 4}, FourierParameters -> {1, -1}]
{10. + 0. I, -2.5 + 3.44095 I, -2.5 + 0.812299 I, -2.5 - 
  0.812299 I, -2.5 - 3.44095 I}
```
