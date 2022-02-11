# Flow Control

guide/FlowControl

传统的 `过程化(procedural)编程语言` 通常要求程序员在其程序的每个阶段都明确定义 `控制流`(flow of control).
Wolfram 语言提供了标准的流程控制原语(primitives), 以及各种符号扩展(symbolic extensions) --
尽管其更高层次的编程范式(paradigm), 通常使程序员不必指定流程控制的细节.

+ Throw "抛出" 任何表达式, 然后被 `0层` 的 `Catch` "捕获".
+ `Confirm, Enclose` 处理 `错误` 和 `异常` 传递.

>`0层` 即 Throw 的外层列表中, 离它最近的 `Catch` 所处的层

## 错误处理

guide/RobustnessAndErrorHandling

Wolfram语言提供了各种机制来检测和管理错误(detecting and managing), 并帮助确保程序的稳健性(robust), 和按预期运行.
`Confirm` 系列函数允许在程序执行过程中检查各种形式的 `error conditions`,在检测到 `errors` 时立即终止程序执行.
`Enclose` 定义了程序中 `错误` 被捕获的 `作用域`.

### 函数参数检查

+ `CheckArguments`; 检查函数参数的结构
+ `Condition (/;)` ; 需要`模式`或`定义` 满足`条件`
+ `ArgumentsOptions`, `OptionsPattern`

### 运行时错误处理

+ `Confirm`; 计算表达式, 确认没有产生`failure`
+ `Enclose`; 计算表达式, 如果发生`failure`, 则`停止`并返回`失败对象`

### 确认的类型

+ `ConfirmBy`; 计算表达式, 确认其结果`满足条件`
+ `ConfirmMatch`; 计算表达式, 确认结果与`模式`相匹配
+ `ConfirmQuiet`; 计算表达式, 确认`没有消息`产生
+ `ConfirmAssert`; 确认`断言`得到满足

### 失败表示

+ `Failure `
+ `Missing `
+ `$Failed`

### 代码监控

+ `Assert` ; 如果条件不满足, 则中止
+ `Interrupt`, `Abort`(中断,终止)

+ `Echo`; 打印中间结果
+ `EchoEvaluation`, `EchoTiming`, `EchoFunction`, `EchoLabel`

### 底层错误处理

+ `Check`, `CheckAbort`
+ `Throw`, `Catch`

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
