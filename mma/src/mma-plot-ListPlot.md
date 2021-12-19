# 散点图,ListPlot

```mathematica
ListPlot[{y1,... yn}]; plots 点 {1, y1}, {2, y2},....

ListPlot[{{x1,y1}, ..., {xn,yn}}]; 绘制具有指定 `x` 和 `y` 坐标的一列点.

ListPlot[{data1, data2,...}]; 绘制所有 data_i 中的数据.

ListPlot[{...,w[data_i,...],...}]; 绘制 data_i, 以及 `符号包装器` w 的特性.
```

+ 支持对自定义 `点` 的注释:

    ```mathematica
    {y1->"lbl1", y2->"lbl2", ...} ,  {y1, y2, ...}->{"lbl1", "lbl2", ...}; 
    值 `{y1, y2, ...}` 对应 标签 {"lbl1", "lbl2", ...}
    ```

    例如:

    ```mathematica
    ListPlot[{{0, 1} -> "a", {1, 2} -> "b"}]
    ```

    等价于:

    ```mathematica
    ListPlot[<|"a" -> {0, 1}, "b" -> {1, 2}, "c" -> {2, 3}|>]
    ```

+ 也支持 `嵌套关联` 的写法:

    ```mathematica
    ListPlot[<|
      "a" -> <|0 -> 1|>, "b" -> <|1 -> 2|>, "c" -> <|2 -> 3|>
      |>]
    ```

    输入数据将被解包为 

    ```mathematica
    {"a" -> <|0 -> 1|>, "b" -> <|1 -> 2|>, "c" -> <|2 -> 3|>}
    (*进一步被解包为*)
    {"a" -> {{0,1}}, "b" ->{{1,2}}, "c" ->{{2,3}} }
    ```

    最终相当于输入了三个注释的 `data_i`. 更深层的嵌套是无效的.
