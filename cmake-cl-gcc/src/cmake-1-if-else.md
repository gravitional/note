# [cmake if](https://cmake.org/cmake/help/latest/command/if.html)

## Condition Syntax

## Basic Expressions

+ `if(<constant>)`

如果 常量 为 `1, ON, YES, TRUE, Y` 或非零数字(包括浮点数), 则为 True.
如果常量为 `0, OFF, NO, FALSE, N, IGNORE, NOTFOUND`, 
空字符串 或 以后缀`-NOTFOUND` 结尾, 则为 False.
已命名的布尔常数 **不区分大小写**.
如果参数不属于这些特定常量, 
则会被视为 变量 或 字符串(请参阅下面的 变量展开), 并采用以下两种形式之一.

+ `if(<variable>)`
如果给定变量的定义值不是 false constant, 则为 `True`.
否则为 False, 包括变量未定义的情况.
请注意, macro arguments 不是变量.
环境变量也不能用这种方法测试, 例如 `if(ENV{some_var})` 的结果总是 `false`.

+ `if(<string>)`
一个带引号的字符串总是求值为 `false`, 除非:
字符串的值是 true constants 之一, |
或策略 CMP0054 未设置为 `NEW`, 且 字符串的值 恰好是受 CMP0054 行为影响的 变量名.

## Logic Operators

+ `if(NOT <condition>)`
True if the condition is not true.

+ `if(<cond1> AND <cond2>)`
True if both conditions would be considered true individually.

+ `if(<cond1> OR <cond2>)`
True if either condition would be considered true individually.

+ `if((condition) AND (condition OR (condition)))`
The conditions inside the parenthesis are evaluated first and then the remaining condition is evaluated as in the other examples. Where there are nested parenthesis the innermost are evaluated as part of evaluating the condition that contains them.

## Existence Checks

+ `if(COMMAND <command-name>)`
True if the given name is a command, macro or function that can be invoked.

+ `if(POLICY <policy-id>)`
True if the given name is an existing policy (of the form CMP<NNNN>).

+ `if(TARGET <target-name>)`
True if the given name is an existing logical target name created by a call to the add_executable(), add_library(), or add_custom_target() command that has already been invoked (in any directory).

+ `if(TEST <test-name>)`
New in version 3.3.

True if the given name is an existing test name created by the add_test() command.

+ `if(DEFINED <name>|CACHE{<name>}|ENV{<name>})`
如果定义了 变量, 缓存变量 或 环境变量 `<name>`, 则判断为 `True`.
变量的值并不重要. 请注意以下注意事项:

Macro arguments 不是变量.
无法直接测试 `<name>` 是否为非缓存变量.
如果存在缓存变量或非缓存变量 `someName`, 表达式 `if(DEFINED someName)` 的值将为 `true`.

相比之下, 表达式 `if(DEFINED CACHE{someName})` 只有在缓存变量 `someName` 存在时才会返回 `true`.
如果需要知道是否存在 非缓存变量, 则需要测试这两个表达式:
`if(DEFINED someName AND NOT DEFINED CACHE{someName})`.

3.14 版新增功能:  新增对 `CACHE{<name>}` 变量的支持.
