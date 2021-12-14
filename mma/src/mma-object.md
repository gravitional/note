# mma 类对象语法

共享函数, `SetSharedVariable` 的帮助文档给出了 `队列类型`(`先进先出`) 的构造函数(constructor)的示例:

```mathematica
newList[list_Symbol] := Module[(*局部变量 data 存储列表的元素, 初始化为空列表*)
  {data = {}},
  (*push 方法,在队列末尾压入数据*)
  list[push, e_] := (AppendTo[data, e];);
  (*pop 方法弹出数据,如果到达队列末尾,则返回 $Failed*)
  list[pop] :=   If[Length[data] == 0, $Failed, With[{e = First[data]}, data = Rest[data]; e]];
  (*相当于 get 方法,返回整个队列的数据*)
  list[] := data;
  (*最后返回构造的队列实例*)
  list ]
```

注意这里必须用 `Module`, 它创建 lexical scope 变量;
而不能用 `Block`, 因为它创建 dynamical 变量, 在 `Block` 块运行结束之后, `index` 将被恢复原来的值.
如果原先的上下文没有 `index`, 那么 `index` 仍为 `undefined`, 也就不能就行算术运算.

类似的, 我们可以定义 `迭代器`, 调用 `next` 方法, 会使迭代器返回的值 `++`:

```mathematica
newIter[iter_Symbol] := Module[(*局部变量 index 存储 当前的数字, 初始化为 0*)
  {index = 0},
  (* next 方法,将 index 的值 加加 *)
  iter["next"] := (index++;);
  (*相当于 get 方法,返回 当前 index *)
  iter[] := index;
  (*最后返回构造的队列实例*)
  iter
  ]
```
