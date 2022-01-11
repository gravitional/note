# Flow Control

guide/FlowControl
guide/RobustnessAndErrorHandling

传统的 `过程化(procedural)编程语言` 通常要求程序员在其程序的每个阶段都明确定义 `控制流`(flow of control).
Wolfram 语言提供了标准的流程控制原语(primitives), 以及各种符号扩展(symbolic extensions) --
尽管其更高层次的编程范式(paradigm), 通常使程序员不必指定流程控制的细节.

The Wolfram Language provides a variety of mechanisms for detecting and managing errors and for helping ensure that programs are robust and operate as intended. The Confirm family of functions allows various forms of error conditions to be checked during the execution of a program, with program execution immediately terminated when errors are detected. Enclose defines the scope in a program in which errors will be caught.

Wolfram语言提供了各种机制来检测和管理错误(detecting and managing), 并帮助确保程序的稳健性(robust), 和按预期运行.
`Confirm` 系列函数允许在程序执行过程中检查各种形式的 `error conditions`,
在检测到 `errors` 时立即终止程序执行. `Enclose` 定义了程序中 `错误` 被捕获的 `作用域`.

+ Throw "抛出" 任何表达式, 然后被 `0层` 的 `Catch` "捕获".
+ `Confirm, Enclose` 处理 `错误` 和 `异常` 传递.

>`0层` 即 Throw 的外层列表中, 离它最近的 `Catch` 所处的层

## Confirm 系列

`True` 返回 `原表达式`, `False` 返回错误

```mathematica
Confirm[expr]; 给出 `expr`, 如果 `expr` 不被认为是 `failure`, 否则向 0层 Enclose 抛出 error

ConfirmBy[expr,f];  确认f[expr]返回True, 则给出 `expr`, 否则向周围最近的Enclose抛出 error.

ConfirmMatch[expr,form]; 如果 `expr` 匹配 `form`, 则给出 `expr`.
若不匹配, ConfirmMatch 停止计算并向 0层 Enclose 抛出错误.

ConfirmQuiet[expr]; 如果计算expr时没有产生任何信息, 则给出expr. 如果产生 messages, 则抛出 error.
```

只判断是否出错, 返回 `Null`

```mathematica
ConfirmAssert[test]; 确认 test  为真, 否则向周围最近的 Enclose 抛出错误.
`ConfirmAssert[...]]` 总是返回 `Null`.
```

`Enclose` 负责捕获 `error`:

```mathematica
Enclose[expr, "prop"]
给出生成的任何 failure 对象的属性 prop.
```
