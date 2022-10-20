# Cmake 宏, macro()

开始记录 `macro`, 以便以后作为命令调用

```cmake
macro(<name> [<arg1> ...])
  <commands>
endmacro()
```

定义名为 `<name>` 的宏, 它的参数名为 `<arg1>, ...`
在 `macro`关键字后面, 在匹配的 `endmacro()` 之前列出的命令,
在宏被调用之前不会被执行.

根据传统, `endmacro()` 命令允许一个可选的 `<name>` 参数.
如果使用的话, 它必须是逐字重复的开头宏命令的参数.

参见 [cmake_policy()][] 命令的文档, 了解宏中的策略行为.

关于 `CMake宏` 和 `functions` 的区别, 见下面的 [Macro vs Function][] 部分.

[cmake_policy()]: https://cmake.org/cmake/help/latest/command/cmake_policy.html#command:cmake_policy
[Macro vs Function]: https://cmake.org/cmake/help/latest/command/macro.html?highlight=macro#macro-vs-function

## 调用

宏的调用是不分大小写的. 定义为:

```macro
macro(foo)
  <commands>
endmacro()
```

的宏可以通过以下任何一种方式调用:

```cmake
foo()
Foo()
FOO()
cmake_language(CALL foo)
```

等方式调用. 然而, 我们强烈建议保持在宏定义中选择的大小写.
典型的宏使用全小写的名字.

*3.18版的新内容*: `cmake_language(CALL ...)` 命令也可以用来调用宏.

## 参数

当宏被调用时, 记录在宏中的命令首先被修改,
用传递的参数替换 `形参`( `${arg1}`, ...), 然后作为普通命令被调用.

除了引用形式参数外, 还可以引用 `${ARGC}`, 它将被设置为传入函数的 `arg数目`,
以及 `${ARGV0}`, `${ARGV1}`, `${ARGV2}`..., 它们将是传入参数的实际值.
这有利于创建带有 可选参数 的宏.

此外, `${ARGV}` 保存了给宏的所有参数的列表,
`${ARGN}` 保存了 `末尾预期参数` 之后的 `参数列表`(即多余参数).
引用形如 `${ARGV#}`, 即次序超过 `${ARGC}` 的参数会产生未定义的行为.
检查 `${ARGC}` 是否大于 `#`, 是确保 `${ARGV#}` 被作为额外参数传递给函数的唯一方法.

## 宏与函数

`macro` 命令与 `function()` 命令非常相似. 尽管如此, 还是有一些重要的区别.

在函数中, `ARGN`, `ARGC`, `ARGV` 和 `ARGV0`, `ARGV1`, ...
是通常意义上的 CMake 的真实变量.
在宏中, 它们不是, 它们是字符串替换, 就像C预处理器(preprocessor )对宏的处理一样.
这有一些后果, 在下面的参数注意事项部分有解释.

`宏` 和 `函数` 的另一个区别是控制流.
`函数` 的执行是通过将控制从 `调用语句` 转移到 `函数主体`.
`宏` 的执行方式是将 `宏的主体` 粘贴在 `调用语句` 的位置上.

这样做的结果是, 宏主体中的 `return()` 并不只是终止宏的执行,
而是将控制权从 宏被调用 的scope中返回.
为了避免混淆, 我们建议完全避免在宏中使用 `return()`.

与函数不同, `CMAKE_CURRENT_FUNCTION`, `CMAKE_CURRENT_FUNCTION_LIST_DIR`, `CMAKE_CURRENT_FUNCTION_LIST_FILE`, `CMAKE_CURRENT_FUNCTION_LIST_LINE`
等变量不会为宏设置.

## 参数注意事项

由于 `ARGN`, `ARGC`, `ARGV`, `ARGV0` 等不是变量, 你将不能使用类似以下的命令

```cmake
if(ARGV1) # ARGV1不是变量.
if(DEFINED ARGV2) # ARGV2不是变量
if(ARGC GREATER 2) # ARGC不是变量
foreach(loop_var IN LISTS ARGN) # ARGN不是变量
```

+ 在第一种情况下, 你可以使用 `if(${ARGV1})`.
+ 在第二种和第三种情况下,
检查是否有可选变量传递给宏的正确方法是使用 `if(${ARGC} GREATER 2`
+ 在最后一种情况下, 你可以使用 `foreach(loop_var ${ARGN}`,
但这将跳过空参数. 如果你需要包括它们, 你可以使用

```cmake
set(list_var "${ARGN}")
foreach(loop_var IN LISTS list_var)
```

注意, 如果在你调用宏的作用域中有 `同名的变量`,
使用` 非引用名` 将使用 `现有的变量` 而不是参数.
`macro` 没有自己的 scope, 将使用上层 scope 的参数.
比如说:

```cmake
macro(bar)
  # 此处的 ARGN 还是 abc, 而不是 x y z
  foreach(arg IN LISTS ARGN)
    <commands>
  endforeach()
endmacro()

function(foo)
  bar(x y z)
endfunction()

foo(a b c) # ARGN 对应 a b c
```

将在 `a;b;c` 上循环, 而不是像人们所期望的那样在 `x;y;z` 上循环.
如果你想要真正的 `CMake变量` 和/或更好的CMake范围控制, 你应该看一下函数命令.
