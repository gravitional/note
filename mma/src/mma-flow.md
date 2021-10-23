# mma 控制结构 flow

## Switch

    Switch[expr, form1, value1, form2, value2, ... ]

计算 `expr`, 然后依次与每个 `form_i`进行比较, 
对于第一个匹配成功的 `form`, 计算并`返回`对应的 `value`

### Details

+ 只有与`第一个`匹配 `expr` 的 `form_i`, 对应的 `value_i`被计算.
每个`form_i`, 只有在轮到它尝试匹配`expr`时, 才会被计算.
+ 若最后一个 `form_i` 是模式 `_`, 那么如果达到这个`case`, 
相应的 `value_i`总是被返回, 也就是默认结果.
+ 如果没有`form_i` 与 `expr` 匹配, 那么 `Switch` 将`不被计算`(unevaluated), 返回原式.
+ `Switch` 有属性 `HoldRest`(即 `form,value` 对在传入时保持不计算).
+ 你可以在 `Switch` 中使用 `Break`, `Return` 和 `Throw`.

### 实例

### 基本例子

将布尔值转换为 `1` 或 `0`, 给出一个 `message`, 当不是 `布尔值` 时默认为`0`.

```mathematica
f::boole = "The value `1` is not True or False.";
f[b_] := Switch[b, True, 1, False, 0, _, Message[f::boole, b]; 0]
{f[True], f[False], f[x]}
f::boole: The value x is not True or False.
Out[3]= {1, 0, 0}
```

给出一个表达式的 `符号变换` 建议

```mathematica
t[e_] := Switch[e, _Plus, Together, _Times, Apart, _, Identity]
e = (1 + x)/(1 - x) + x/(1 + x);
t[e]
Out[2]= Together

(*试着进行转换:*)
e1 = t[e]
Out[3]= (-1 - 3 x)/((-1 + x) (1 + x))
```
