# 消息,Messages

tutorial/Messages

消息系统可以用来输出错误和警告: `tutorial/Messages`
常用函数有: `Message`, `Messages`, `Information`

+ `$MessageList` : 当前输入行计算产生的消息列表.
+ `Check[expr,failexpr]`:  如果计算成功, 返回 `expr`, 如果计算期间产生消息(一般是因为错误), 就返回`failexpr`

+ `全局信息` tutorial/GlobalSystemInformation

可以使用一些特殊的全局变量, 来查看运行环境, 如:

```mathematica
$Notebooks   记录是否正在使用笔记本前端
$BatchInput   是否以批处理方式给出输入
$BatchOutput   是否应以批处理方式给出输出, 从而不带标签等.
$CommandLine   用于调用Wolfram语言内核的原始命令行
$ParentLink   WSTP LinkObject, 指定调用内核的程序(如果直接调用内核, 则为Null)
$ProcessID   操作系统分配给Wolfram语言内核进程的ID
$ParentProcessID   调用Wolfram语言内核的进程的ID
$Username   运行Wolfram语言内核的用户的登录名
Environment["var"]   操作系统定义的变量的值
```

## Message

```mathematica
Message[symbol::tag]; 打印 `symbol::tag`, 除非它被关闭.
Message[symbol::tag, e1, e2, ...]; 打印一条消息, 根据需要插入 e_i 的值.
```

### 详细

+ `Message` 在 `通道`(channel) `$Messages` 上产生输出.
+ 你可以使用 `Off[symbol::tag]` 关闭消息, 用 `On[symbol::tag]` 打开消息.
+ 在任何两个连续的输入行之间, Wolfram Language 最多打印三次带有特定名称的消息. 在最后一次出现时, 它将打印出 `General::stop` 消息.
+ `Off[General::stop]` 使 Wolfram Language 不暂停信息重复.

+ 在计算特定的 `输入行` 时, 与该输入行相关的消息名称被附加到列表 `$MessageList` 中, 并以 `HoldForm` 包装.
在对第 `n` 个输入行的计算结束时, `$MessageList` 的值被分配给 `MessageList[n]`.

+ `Message[mname, e1, e2, ...]` 被打印成 `StringForm[mess, e1, e2, ...]`, 其中 `mess` 是消息 `mname` 的值.
字符串 `mess` 中形式为 `` `i` `` 的条目被相应的 `e_i` 替换.

+ 通过 `symbol::tag`  指定的消息, `Message` 首先搜索消息 `symbol::tag::lang_i`, 对于列表 `$Language` 中的每种语言.
如果它没有找到, 再搜索实际的 `symbol::tag`.
如果它没有找到, 它就对 `General::tag` 执行同样的搜索步骤.
如果仍然没有找到消息, 它会将全局变量 `$NewMessage` 的任何值应用于 `symbol` 和 `"tag"`.

+ 如果你指定消息为 `symbol::tag::lang`, 那么 `Message` 将只搜索具有特定语言 `lang` 的消息.

### 例子

+ 发出消息:

    ```mathematica
    Message[f::argx, 1, 2]
    ```

+ 定义 `函数` 来发出消息, 并在出错时返回不计算的形式:

    ```mathematica
    rsqrt[x_] /; If[TrueQ[x >= 0], True, Message[rsqrt::nnarg, x]; False] := Sqrt[x]
    ```

    定义消息:

    ```mathematica
    rsqrt::nnarg = "The argument `1` is not greater than or equal to zero.";

    rsqrt[2.25]
    Out[3]= 1.5

    rsqrt[-2.25]
    rsqrt::nnarg: The argument -2.25 is not greater than or equal to zero.
    Out[4]= rsqrt[-2.25]
    ```
