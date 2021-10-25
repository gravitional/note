# 计算时间测试

## Timing

`Timing[expr]`; 对 `expr` 进行计算, 并返回一个列表, 首元素是所使用的时间(秒), 末元素是`expr`计算的结果.

### Details

+ `Timing`有属性 `HoldAll`.
+ `Timing[expr;]` 将给出 `{timing,Null}`.
+ `Timing` 只精确到至少 `$TimeUnit` 秒的程度.
+ `Timing`只包括在 `Wolfram Language` 内核中花费的 `CPU` 时间.
它不包括通过 `WSTP` , 或以其他方式连接外部`进程`所花费的时间.
它也不包括在 Wolfram System 前端花费的时间.
+ `Timing[expr]` 只包括 `expr` 的计算时间, 而不包括, 例如`格式化`结果或`打印`的时间.
+ 在某些具有多个 `CPU` 的计算机系统上, Wolfram Language 内核有时可能会在不同的 `CPU` 上产生额外的线程(threads).
在某些操作系统上, `Timing` 可能会忽略这些额外的线程.
在其他操作系统上, 它可能会给出所有线程花费的总时间, 这可能会超过 `AbsoluteTiming` 的结果.
+ `Timing[expr]` 可能在同一会话的不同地方给出不同的结果.
一个典型的原因是 `内部系统缓存` 的使用, 这些缓存可以用 `ClearSystemCache` 来清除.

### 实例

`computation`的时间花费:

```mathematica
Timing[Total[Range[123456]]]
Out[1]= {0.07, 7620753696}.
```

比较以不同方式进行计算的时间:

```mathematica
Timing[Module[{x = 1/Pi}, Do[x = 3.5 x (1 - x), {10^6}]; x]]
输出[1]={3.054, 0.500884}

Timing[Nest[3.5 # (1 - #) &, 1./Pi, 10^6]]
输出[2]={0.16, 0.500884}
```

抑制输出, 以便在输出结果很庞大时查看`Timing`:

```mathematica
Timing[f = Fourier[RandomReal[1, 2^16]];]
Out[1]={0.03, Null}.
```

做一个图, 比较计算长度为`n`的`FFT`的时间:

```
ListPlot[Table[{n, First[Timing[Fourier[RandomReal[1, n]]]]}, {n, 2^16, 2^16 + 100}]]
```

## AbsoluteTiming

`AbsoluteTiming[expr]` ; 对 `expr` 进行计算, 返回一个列表.
首元素是已经过去的实时绝对秒数, 末元素是计算`expr`得到的结果.

+ `AbsoluteTiming` 具有属性 `HoldAll`.
+ `AbsoluteTiming[expr;]` 将给出 `{timing,Null}`.
+ `AbsoluteTiming` 总是精确到 `$TimeUnit` 秒的粒度(granularity), 但在许多系统上要精确得多.
+ `AbsoluteTiming[expr]` 只测量实际计算 ` expr` 的时间, 而不包括`格式化`结果的时间.

### 示例

获取做一次计算的总时间:

```mathematica
AbsoluteTiming[x = 1 + 2; Pause[x]; x + 3]
Out[1]= {3.0002112,6}
```

`Timing`只是报告使用的 `CPU 时间`.

```mathematica
Timing[x = 1 + 2; Pause[x]; x + 3]
Out[2]= {0.,6}
```

`AbsoluteTiming` 考虑所有的时间, 包括在需要时, 通过互联网获取数据用的时间:

```mathematica
AbsoluteTiming[CountryData["France", "Population"]]
Out[1]= {6.262998,6.3783*10^7people}
```

第二次运行时速度更快, 因为数据已经被保存下来, 可以立即重复使用:

```mathematica
AbsoluteTiming[CountryData["France", "Population"]]
```
