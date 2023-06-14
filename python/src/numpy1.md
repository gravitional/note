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
