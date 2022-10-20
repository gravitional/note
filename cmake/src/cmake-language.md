# cmake-language

[cmake-language(7)](https://cmake.org/cmake/help/latest/manual/cmake-language.7.html?highlight=variable#variable-references)

## 变量

变量是CMake语言中的基本存储单位.
它们的值总是 `字符串类型` 的, 尽管一些命令可能将字符串解释为其他类型的值.
`set()` 和 `unset()` 命令明确地设置或取消变量, 但其他命令也有修改变量的语义.
变量名称是区分大小写的, 几乎可以由任何文本组成,
但我们建议坚持使用仅由字母数字字符加上 `_` 和 `-` 组成的名称.

变量具有动态范围. 每个变量的 `set` 或 `unset` 都会在当前范围内创建绑定.

### 函数范围

`function()` 命令创建的 `Command Definitions`,
在调用时, 在新的 `变量绑定范围内` 处理记录的命令.
变量的 `set()` 或 `unset()` 在这个作用域中绑定,
对 `当前函数` 和其中的任何嵌套调用都是可见的, 但在函数返回后就不可见了.

### 目录范围

源树中的每个目录都有自己的变量绑定.
在处理一个目录的 `CMakeLists.txt` 文件之前,
CMake 会复制当前在父目录中定义的所有变量绑定,
如果有的话, 以初始化新的目录 scope.
当用 `cmake -P` 执行CMake 脚本时, 在单个 `目录` scope内绑定变量.

不在函数调用中的变量 `set` 或 `unset` 会绑定到当前目录范围.

### 持久缓存

CMake存储了一组单独的 `cache` 变量, 或 `cache entries`,
其值在 `项目构建树` 中的多次运行中都会持续存在.
缓存项目有一个独立的 `绑定scope`, 只有通过明确的请求才能修改,
比如通过 `set()` 和 `unset()` 命令的 `CACHE` 选项.

当计算 `变量引用` 时, CMake 首先在 `函数调用堆栈`(如果有的话)中搜索绑定,
然后返回到 `当前目录` 范围内的绑定(如果有的话).
如果找到了 `set` 绑定, 则使用其值.
如果找到了 `unset` 的绑定, 或者没有找到绑定, CMake 会搜索 cache entry..
如果找到了 缓存条目, 则使用其值. 否则, `变量引用` 会被计算为 `空字符串`.
可以用 `$CACHE{VAR}` 语法来直接进行缓存条目的查找.

[cmake-variables(7)][] 手册记录了许多由 CMake 提供的变量,
或者由项目代码设置后对CMake有意义的变量.

注意 CMake 保留了以下标识符:

+ 以 `CMAKE_` 开头(大写, 小写或混合大小写), 或
+ 以 `_CMAKE_` 开头(大写, 小写或混合大小写), 或
+ 以 `_` 开头, 后面是任何 [CMake命令][] 的名称.

[cmake-variables(7)]: https://cmake.org/cmake/help/latest/manual/cmake-variables.7.html#manual:cmake-variables(7)
[CMake命令]: https://cmake.org/cmake/help/latest/manual/cmake-commands.7.html#manual:cmake-commands(7)

## 变量引用

`变量引用`的形式是 `${<variable>}`, 在 带引号的参数 或 未带引号的参数 中被计算.
`变量引用` 会被指定的 `变量` 或 `缓存项`(cache entry) 的值所替代,
如果两者都没有设置, 则被空字符串替代. 变量引用可以嵌套, 并且从内向外计算,
例如: `${outer_${inner_variable}_variable}`.

`Literal变量引用` 可以由字母数字字符, 字符 `/_.+-` 和 `Escape序列` 组成.
`嵌套引用` 可用于计算 `任何名称` 的变量.
也请参见政策 [CMP0053][] 文件, 以了解历史上的考虑
和在技术上 `$` 也是允许的但不鼓励的原因.

变量部分记录了变量名称的 `范围`, 以及如何设置它们的值.

`环境变量` 的引用形式为 `$ENV{<variable>}`.
更多信息请参见环境变量部分.

`缓存变量` 引用的形式是 `$CACHE{<variable>}`,
它被指定的缓存条目的值所取代, 而不需要检查是否有同名的 `普通变量`(normal variable)).
如果缓存条目不存在, 它将被空字符串替换. 更多信息见 [CACHE][].

`if()` 命令有一个特殊的条件语法,
允许用 `<variable>` 而不是 `${<variable>}` 的简短形式来引用变量.
然而, 环境变量总是需要以 `$ENV{<variable>}` 来引用.

[CMP0053]: https://cmake.org/cmake/help/latest/policy/CMP0053.html#policy:CMP0053
[CACHE]: https://cmake.org/cmake/help/latest/variable/CACHE.html#variable:CACHE
