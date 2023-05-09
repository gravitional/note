# 严格弱序, strict weak order

[[STL]为什么sort的自定义cmp函数中必须使用严格弱序(strict weak order)](https://blog.csdn.net/Strengthennn/article/details/107738011)

STL官方文档中对 `sort` 函数的 `cmp` 参数(文档中为comp)的描述如下:[1]

`comp`
Binary function that accepts two elements in the range as arguments, and returns a value convertible to bool.
The value returned indicates whether the element passed as `first` argument is considered to `go before` the `second` in the specific `strict weak ordering` it defines.
The function shall not modify any of its arguments.
This can either be a `function pointer` or a `function object`.

翻译如下:

接受两个元素参数的二元函数, 返回一个可以转为bool类型的值.
`返回值` 表明了   `comp` 在两个 输入参数间定义的 `严格弱序`, 即第一个参数是否要排在第二个参数后面.
该函数不可以修改传入的参数.
输入可以是 `函数指针` 或 `函数对象`.

那么什么是严格弱序呢?
[维基百科](https://en.wikipedia.org/wiki/Partially_ordered_set)中对严格弱序的介绍如下:

A `strict weak ordering` is a binary relation `<` on a `set S` that
is a `strict partial order` (a `transitive` relation that is `irreflexive`, or equivalently, that is `asymmetric`)
in which the relation "neither a < b nor b < a" is transitive.

Therefore, a strict weak ordering has the following properties:

+ For all `x` in `S`, it is not the case that `x < x` (irreflexivity, 非自反).
+ For all `x, y` in `S`, if `x < y` then it is not the case that `y < x` (asymmetry, 非对称).
+ For all `x, y, z` in `S`, if `x < y` and `y < z` then `x < z` (transitivity, 传递).
+ For all `x, y, z` in `S`, if `x` is incomparable with `y` (neither `x < y` nor `y < x` hold),
and `y` is incomparable with `z`, then `x` is incomparable with `z` (transitivity of incomparability).

翻译如下:

严格弱序是一个在集合 `S` 上的 严格偏序关系 `>`
(是一种 `非自反` 的传递关系, 即 `非对称关系`).
该关系在"a!<b"并且"b!<a"时具有传递性. 因此, 一个严格弱序满足如下几条性质:

+ 对于集合S中的任意元素 `x`, 不存在 `x<x` (非自反性).
+ 对于集合S中的任意元素 `x`, `y`, 如果 `x<y`, 则不存在 `y<x`(非对称性).
+ 对于集合S中的任意元素 `x`, `y` 和 `z`, 如果 `x<y`, 并且 `y<z`, 那么 `x<z` (传递性).
+ 对于集合S中的任意元素 `x`, `y` 和 `z`, 如果 `x` 与 `y` 不可比(即不存在 `x<y` 和 `y<x`),
`y` 与`z` 不具备可比性, 那么 `x` 与 `z` 也不可比(`不可比` 的传递性).

从严格弱序的定义中可以看出, `<=` 和 `>=` 关系并不满足严格弱序.
因为对于任意的 `x`, 有 `x<=x`(`x>=x`), 不满足性质1(非自反性).

而 `<` 和 `>` 满足所有的四条性质, 所以 `<` 和 `>` 关系是一种严格弱序关系.
因此, 在 `sort` 的自定义cmp函数中, 不可以使用 `<=`  或者 `>=` 关系确定返回值, 可以使用 `<` 或 `>` 关系确定返回值.
至于为什么必须使用严格弱序的详细原因可以从sort的源代码中找到.
